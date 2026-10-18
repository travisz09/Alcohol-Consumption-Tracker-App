#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##### Travis Zalesky
##### Objective: Create a custom app for tracking and visualizing my alcohol consumption habits.

##### To Do: ####
##### Create figures page
##### Summary data 
##### Publish updated script to github.
##### Password protect
##### Bug: Gender select info button dosn't work well on mobile

##### Begin app #####
#install.packages("shiny")
#install.packages("googlesheets4")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("hms")
#install.packages("shinyWidgets")
#install.packages("clock")

library(shiny)
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(hms)
library(shinyWidgets)
library(clock)

# R functions source file
source("R/data.R")

# Required auth. token for shinyapp.io to access gsheets.
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  # Below script will write ".sectrets" folder to your directory, or other directory as specified below.
  # Do not share your ".secrets" to prevent unauthorized use.
  gargle_oauth_cache = ".secrets"
)

# User Input Required!!
# Put your gsheet url in .secrets/url.txt.
sheetURL <- readLines(".secrets/url.txt")

1 #google sheets option, grant permissions. Will require user browser input on first usage.

# Default values for calculating BAC (update as desired)
defaultWeight <- 170
defaultGender <- "M"

# Timezone
app_tz <- "America/Phoenix"
today <- as.Date(Sys.time(), tz = app_tz)

# Define UI for application
ui <- fluidPage(
  lang='en',
  theme = bslib::bs_theme(bootswatch = 'sandstone'),
  
  # Application title
  titlePanel(
    windowTitle = "Alcohol Consumption Data Tracker App",
    column(
      width = 4,
      "Alcohol Consumption\nData Logger App"
    )
  ),
  tabsetPanel(
    type = 'tabs',
    tabPanel(
      'input',
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "drinkSelect",
            "Drink",
            choices = NULL,
            multiple = FALSE,
            options = list(create = TRUE)
          ),
          selectizeInput(
            "brandSelect",
            "Brand",
            choices = NULL,
            multiple = FALSE,
            options = list(create = TRUE)
          ),
          selectizeInput(
            "styleSelect", 
            "Style", 
            choices = NULL,
            multiple = FALSE,
            options = list(create = TRUE)
          ),
          numericInput(
            "abv", 
            "Alcohol by volume (abv)",
            min = 0, 
            max = 100,
            value = NULL
          ),
          numericInput(
            "volume", 
            "Volume (oz)",
            min = 0,
            value = NULL
          ),
          textInput('notes', "Notes"),
          actionButton(
            'submit', 
            "Drink!",
            class ='btn-lg btn-success'
          ),
          column(
            width = 4,
            tableOutput('table') # static table output
          ) 
        ) 
      ) 
    ), 
    tabPanel(
      "BAC",
      fluidRow(
        column(
          width = 4,
          selectInput(
            "genderSelect",
            label = tags$span(
              "Gender",
              tags$i(
                class = "glyphicon glyphicon-info-sign",
                style = "color:#0072B2;",
                title = "Formulas used to calculate BAC use sex assigned at birth. This is a limitation of the available science, and validated equations to calculate estimated BAC for people who are trans or non-binary have not been published."
              ),
            ),
            choices = c("M", "F"),
            selected = defaultGender, # Change default value as desired
            multiple = FALSE
          ),
          numericInput(
            "weight", 
            "Weight (lbs)", 
            value = defaultWeight, # Change default value as desired
            min = 1
          ),
        )
      ), 
      fluidRow(
        column(
          width = 4,
          htmlOutput("BAC"),
        ), 
      ), 
      fluidRow(
        column(
          width = 4,
          textOutput("soberEst")
        )
      )
    ) 
  ), 
  
  # Begin footer
  fluidRow(
    column(
      width = 4, 
      align = 'center',
      style = 'border: 1px solid black; margin: 5px',
      div(
        class = 'footer',
        includeHTML('footer.html')
      ) 
    ) 
  ) 

) 

# Define server logic
server <- function(input, output, session) {

  # Read the raw data (reactive)
  rawData <- reactiveVal(
    read_drinks(sheetURL)
  )

  # Summarize data for UI selectors
  summaryTable <- reactive({
    rawData() %>%
      group_by(drink, brand, style, abv) %>%
      summarise(
        lastDate = max(date),
        lastVolume = last(volume),
        .groups = "drop"
      ) %>%
      arrange(desc(lastDate))
  })

  # Initiate UI input selectors
  # drinkSelect -> brandSelect -> styleSelect -> abv & volume
  # Bug: On change, the old selection gets ported to the new input choices. E.g. If going from San Tan Pumpkin Porter to PBR, Pumpkin Porter follows the change and becomes available in the PBR style selector. etc. etc.
  observe({
    updateSelectizeInput(
      session,
      "drinkSelect",
      choices = unique(summaryTable()$drink),
      server = TRUE
    )
  })
  observeEvent(input$drinkSelect, {
    choices <- summaryTable() %>%
      filter(drink == input$drinkSelect) %>%
      pull(brand) %>%
      unique()

    updateSelectizeInput(
      session,
      "brandSelect",
      choices = choices,
      server = TRUE
    )
  })
  observeEvent(input$brandSelect, {
    choices <- summaryTable() %>%
      filter(
        drink == input$drinkSelect,
        brand == input$brandSelect
      ) %>%
      pull(style) %>%
      unique()

    updateSelectizeInput(
      session,
      "styleSelect",
      choices = choices,
      server = TRUE
    )
  })
  observeEvent(input$styleSelect, {
    selection <- summaryTable() %>%
      filter(
        drink == input$drinkSelect,
        brand == input$brandSelect,
        style == input$styleSelect
      ) %>%
      slice(1)

    updateNumericInput(
      session,
      'abv',
      value = selection$abv
    )
    updateNumericInput(
      session,
      'volume',
      value = selection$lastVolume
    )
  })

  # Render UI summary table
  output$table <- renderTable({
    rawData() %>%
      mutate(
        date = as.character(date),
        time = as.character(time)
      ) %>%
      arrange(
        desc(date),
        desc(time)
      ) %>%
      head()
  })
  
  # Begin BAC tab functions
  # Establish reactive value r for gender-based BAC calculations
  r <- reactive({
    req(input$genderSelect)

    if (input$genderSelect == "F") {
      0.55
    } else { 0.68 }
  })
    

  weightInG <- reactive({
    req(input$weight)
    input$weight * 453.6
  })

  BAC <- reactive({
    req(weightInG())

    bac <- (
      alcoholConsumed() / (weightInG() * r()) * 100
    ) - (elapsedTime() * 0.015)

    max(bac, 0)  # Enforces positive values >= 0
  })

  alcoholConsumed <- reactive({
    sum(
      rawData() %>%
        filter(date == today)%>%
        reframe(
          totalAlcohol = (abv / 100) * (volume * 29.574)
        )
    )
  })

  lastDrink <- reactive({
    drinks <- rawData() %>%
      filter(!is.na(time))

    as.POSIXct(
      paste(
        last(drinks$date),
        last(drinks$time)
      ),
      tz = app_tz
    )
  })

  lastDrinkDate <- reactive({
    as.Date(lastDrink(), tz = app_tz)
  })

  firstDrink <- reactive({
    first_time <- rawData() %>%
      filter(
        date == lastDrinkDate(),
        !is.na(time)
      ) %>%
      pull(time) %>%
      first()

    as.POSIXct(
      paste(lastDrinkDate(), first_time),
      tz = app_tz
    )
  })

  elapsedTime <- reactive({
    as.numeric(
      difftime(
        Sys.time(),
        firstDrink(),
        units = 'hours'
      )
    )
  })

  tToSober <- reactiveVal(0)

  tSober <- reactiveVal(0)

  soberMessage <- reactiveVal("You'll be sober in")
  
  observeEvent(
    list(input$weight, input$genderSelect), 
    {
      req(input$weight, input$genderSelect)

      # weightInG()
      
      # g <- input$genderSelect
      # if (g == "F"){
      #   r(0.55)
      # } else { r(0.68) }
      
      # BAC(
      #   (alcoholConsumed() / (weightInG() * r()) * 100) -
      #     (elapsedTime() * 0.015)
      # )
    
      if (BAC() == 0){
        # Sobriety block
        firstDrink()

        lastTotalAlc <- sum(
          rawData() %>%
            filter(date == as.Date(lastDrink(), tz = app_tz))%>%
            reframe(
              totalAlcohol = (abv / 100) * (volume * 29.574)
            )
          )
        elapsedTime()

        peakBAC <- (lastTotalAlc / (weightInG() * r()) * 100) -
          (elapsedTime() * 0.015)
        
        t0 <- lastDrink() + ((peakBAC / 0.015) * 3600)
        
        tSober(difftime(today, t0, units = "days"))
        
        soberMessage("Congratulations! You've been sober for")
        if (tSober() > 0) {
          output$soberEst <- renderText({ 
            paste(
              soberMessage(), 
              sub("\\.\\d+$", "", tSober()), 
              "days",
              round(
                round(
                  as.numeric(
                    sub("^\\d+\\.", "0.", tSober())
                  ), 
                  2
                ) * 24, 
                0
              ),
              "hours", sep = " "
            ) 
          })
        } else { 
          # if tSober() < 0 
          tSober(0)
          output$soberEst <- renderText({ 
            paste(
              soberMessage(),
              sub("\\.\\d+$", "", tSober()), 
              "days",
              round(
                as.numeric(
                  sub("^\\d+\\.", "0.", tSober())
                ) * 24,
                0
              ),
              "hours", 
              sep = " "
            ) 
          })
        }
      } else { 
        # if BAC >= 0
        if (round(BAC(), 3) >= 0.08) {
          # Intoxicated
          output$BAC <- renderText({ 
            paste(
              tags$b(
                "Approximate BAC =",
                tags$span(
                  style = "color: red",
                  round(BAC(), 3)
                ),
                sep = " "
              )
            ) 
          })
        } else {
          # Impaired
          output$BAC <- renderText({ 
            paste(
              tags$b(
                "Approximate BAC =", 
                round(BAC(), 3)
              ),
              sep = " "
            ) 
          })
        }
        
        tToSober((BAC()/0.015))
        if (tToSober() >= 1) {
          output$soberEst <- renderText({ 
            paste(
              soberMessage(),
              sub("\\.\\d+$", "", tToSober()), 
              "hours",
              round(
                as.numeric(
                  sub("^(.*?)\\.", "0.", tToSober())
                ) * 60, 
                0
              ),
              "minutes", 
              sep = " "
            ) 
          })
        }
        
        if (tToSober() < 1) {
          output$soberEst <- renderText({ 
            paste(
              soberMessage(),
              round((tToSober() * 60), 0), 
              "minutes",
              sep = " "
            ) 
          })
        }
      } 
  })
  
  output$BAC <- renderText({ 
    paste(
      tags$b(
        "Approximate BAC =", 
        round(BAC(), 3), 
        sep = " "
      )
    ) 
  })
  
  output$soberEst <- renderText({ 
    paste(
      soberMessage(),
      round(tToSober(), 0), 
      "hours",
      round(
        (tToSober() - round(tToSober(), 0)) * 60, 
        0
      ), 
      "minutes",
      sep = " "
    ) 
  })
  
  # On user submit, "Drink!"
  observeEvent(input$submit, {
    # Generate time stamp
    now <- Sys.time()

    # Construct newEntry df for append_drink()
    # TODO: Data currently uses separate Date and Time columns (without tz information). Update to canonical timestamp structure
    newEntry <- data.frame(
      date = today,
      time = format(now, tz = app_tz, format = "%H:%M:%S"),
      drink = input$drinkSelect,
      brand = input$brandSelect,
      style = input$styleSelect,
      abv = input$abv,
      volume = input$volume,
      notes = input$notes
    )
    
    # Update gsheet data
    append_drink(
      sheet_url = sheetURL,
      drink = newEntry
    )
    
    # Read in updated data
    rawData(
      read_drinks(sheetURL)
    )
    
    #Update BAC
    alcoholConsumed()
    
    # BAC(
    #   (alcoholConsumed() / (weightInG() * r()) * 100) -
    #     (elapsedTime() * 0.015)
    # )
    
    if (BAC() <= 0){ # only applicable with non-alcoholic beverages.
      BAC(0)
      
      firstDrink(
        as.POSIXct(
          paste(
            lastDrinkDate(),
            first(
              subset(
                rawData(),
                date == lastDrinkDate()
              )$time
            )
          ),
          tz = app_tz
        )
      )
      lastTotalAlc <- sum(
        rawData() %>%
          filter(date == as.Date(lastDrink(), tz = app_tz))%>%
          reframe(
            totalAlcohol = (abv / 100) * (volume * 29.574)
          )
        )
      elapsedTime(
        as.numeric(
          difftime(
            lastDrink(), 
            firstDrink(), 
            units = 'hours'
          )
        )
      )
      peakBAC <- (lastTotalAlc / (weightInG() * r()) * 100) -
        (elapsedTime() * 0.015)
      
      t0 <- lastDrink() + ((peakBAC/0.015)*3600)
      
      tSober(difftime(dateTime, t0, units = "days"))
      
      soberMessage("Congratulations! You've been sober for")
      if (tSober() > 0) {
        output$soberEst <- renderText({ 
          paste(
            soberMessage(),
            sub("\\.\\d+$", "", tSober()), 
            "days",
            round(
              round(
                as.numeric(
                  sub("^\\d+\\.", "0.", tSober())
                ), 
                2
              ) * 24,
              0
            ),
            "hours", 
            sep = " "
          ) 
        })
      } else { # if tSober() < 0 
        tSober(0)
        output$soberEst <- renderText({ 
          paste(soberMessage(),
          sub("\\.\\d+$", "", tSober()), 
          "days",
          round(
            as.numeric(
              sub("^\\d+\\.", "0.", tSober())
            ) * 24, 
            0
          ),
          "hours", 
          sep = " "
          ) 
        })
      }
    } else { #if BAC >= 0
      soberMessage("You'll be sober in")
      tToSober((BAC()/0.015))
      if (tToSober() >= 1) {
        output$soberEst <- renderText({ 
          paste(soberMessage(),
          sub("\\.\\d+$", "", tToSober()), 
          "hours",
          round(
            as.numeric(
              sub("^(.*?)\\.", "0.", tToSober())
            ) * 60, 
            0
          ),
          "minutes", 
          sep = " "
          )
        })
      }
      
      if (tToSober() < 1) {
        output$soberEst <- renderText({ 
          paste(
            soberMessage(),
            round((tToSober() *60), 0), 
            "minutes",
            sep = " "
          ) 
        })
      }
    
      if (round(BAC(), 3) >= 0.08) {
        output$BAC <- renderText({ 
          paste(
            tags$b(
              "Approximate BAC =",
              tags$span(
                style = "color: red",
                round(BAC(), 3)
              ), 
              sep = " "
            )
          ) 
        })
      } else {
        output$BAC <- renderText({ 
          paste(
            tags$b(
              "Approximate BAC =", 
              round(BAC(), 3)
            ),
            sep = " "
          ) 
        })
      }
    }
  }) 
} 

# Run the application 
shinyApp(ui = ui, server = server)
