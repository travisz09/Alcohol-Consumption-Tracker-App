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
source("R/utils.R")

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
          uiOutput("BAC"),
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

  # On user submit, "Drink!"
  observeEvent(input$submit, {
    # Validate inputs
    if (
      is.null(input$drinkSelect) ||
        input$drinkSelect == "" ||
        is.null(input$abv)  ||
        is.null(input$volume)
    ) {
      showNotification(
        'Drink type, ABV, and volume are required.',
        type = 'error'
      )
      return()
    }

    # Validate ABV
    if(input$abv < 0 || input$abv > 100) {
      showNotification(
        'ABV must be between 0 and 100.',
        type = 'error'
      )
      return()
    }

    # Validate volume
    if(input$volume <=0) {
      showNotification(
        "Volume must be greater than 0.",
        type = 'error'
      )
      return()
    }

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
    
    # Submission validation and error handling
    write_success <- tryCatch(
      {
        # Update gsheet data
        append_drink(
          sheet_url = sheetURL,
          drink = newEntry
        )
        TRUE
      },
      error = function(e) {
        showNotification(
          'Unable to save drink. Please try again.',
          type = 'error',
          duration = 6
        )

        message('Drink submission failed: ', conditionMessage(e))
        FALSE
      }
    )
    if(!write_success) {
      return()
    }

    # Submission success, check data refresh
    tryCatch(
      {
        # Read in updated data
        rawData(
          read_drinks(sheetURL)
        )
        showNotification(
          'Drink logged.',
          type = 'message',
          duration = 3
        )
      },
      error = function(e) {
        showNotification(
          'The drink was saved, but the app could not refresh the data.',
          type = 'warning',
          duration = 6
        )

        message("Data refresh failed: ", conditionMessage(e))
      }
    )
  })
  
  # BAC tab functions
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

    calculate_bac(
      alcohol = alcoholConsumed(),
      weight_g = weightInG(),
      r = r(),
      elapsed_hours = elapsedTime()
    )
  })

  alcoholConsumed <- reactive({
    # TODO: Refactor drinking sessions to use timestamps rather than calendar date.
    df <- rawData() %>%
      filter(date == today)

    sum(
      calculate_alcohol(
        abv = df$abv, 
        volume = df$volume 
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

  tToSober <- reactive({
    BAC() / 0.015
  })

  # Sobriety calculations
  # Alcohol consumed on the most recent drinking date
  lastTotalAlcohol <- reactive({
    df <- rawData() %>%
      filter(date == lastDrinkDate())

    sum(
       calculate_alcohol(
        abv = df$abv,
        volume = df$volume
       )
    )
  })

  peakBAC <- reactive({
    req(weightInG(), r())

    drinkingDuration <- as.numeric(
      difftime(
        lastDrink(),
        firstDrink(),
        units = "hours"
      )
    )

    bac <- calculate_bac(
      alcohol = lastTotalAlcohol(),
      weight_g = weightInG(),
      r = r(),
      elapsed_hours = drinkingDuration
    )
  })

  # Timestamp of when BAC last reached 0
  soberTime <- reactive({
    lastDrink() + (peakBAC() / 0.015) * 3600
  })

  # Sobriety durration
  tSober <- reactive({
    as.numeric(
      difftime(
        Sys.time(),
        soberTime(),
        units = "hours"
      )
    )
  })
  
  # User BAC estimate
  output$BAC <- renderUI({ 
    bac <- round(BAC(), 3)

    if (bac >= 0.08) {
      # Intoxicated
      tags$b(
        'Approximate BAC = ',
        tags$span(
          style = 'color: red;',
          bac
        )
      )
    } else {
      # Impaired
      tags$b(
        'Approximate BAC = ',
        bac
      )
    }
  })
  
  # User sobriety message
  output$soberEst <- renderText({ 
    if(BAC() <= 0 && tSober() > 0) {
      paste(
        "Congratulations! You've been sober for",
        format_duration(tSober())
      )
    } else {
      paste(
        "You'll be sober in",
        format_duration(tToSober())
      )
    }
  })
} 

# Run the application 
shinyApp(ui = ui, server = server)
