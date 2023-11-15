#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##### Travis Zalesky
##### Last Update: 9/3/23
##### Objective: Create a custom app for tracking and visualizing my alcohol consumption habits.
##### Version History ####
##### V0.0.0 6/14/23: Draft
##### V1.0.0 6/14/23: First working commit. Data logger only.
##### V1.1.0 9/2/23: Create BAC calculator.
##### V1.1.1 9/3/23: Bug fix; default elapsedTime difftime function to units = 'hours'.
#####                 Bug fix; remove today() function, replace with universal function with specified timezone.
##### V1.2.0 9/3/23: Feat: Real time update BAC.

##### Begin app #####
#install.packages("shiny")
#install.packages("googlesheets4")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("hms")

library(shiny)
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(hms)



#Required auth. token for shinyapp.io to access gsheets.
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  # Below script will write ".sectrets" folder to your directory, or other directory as specified below.
  # Do not share your ".secrets" to prevent unauthorized use.
  gargle_oauth_cache = ".secrets"
)

 
#Connect to your google docs by inserting web address below.
rawData <- read_sheet("https://docs.google.com/Your_Sheet_Here")%>%
#fix pesky NULL values
  #notes column being read in as list, creating downstream errors.
  #Don't know how to fix right now, so just dropping notes column.
  select(-notes)

1 #google sheets option, grant permissions. Will require browser input on first use.

summaryTable <- rawData%>%
  mutate(abv = as.numeric(abv),
         volume = as.numeric(volume))%>%
  group_by(drink, brand, style, abv)%>%
  summarise(lastDate = max(date),
            lastVolume = last(volume))%>%
  arrange(desc(lastDate))

userGender <- "M"
#r(male)=. 68 r(female)=. 55
r = 0.68 #used for calculating BAC
userWeight <- 170

# Define UI for application
ui <- fluidPage(lang='en',
                theme = bslib::bs_theme(bootswatch = 'sandstone'),
                

    # Application title
    titlePanel(windowTitle = "Alcohol Consumption Data Tracker App",
               column(4,
                      "Alcohol Consumption\nData Logger App"
                      ) #end column
               ), #end titlePanel
    tabsetPanel(type = 'tabs',
                tabPanel('input',
    fluidRow(
      column(4,
        selectizeInput("drinkSelect", "Drink", summaryTable$drink,
                       selected = TRUE,
                       multiple = FALSE,
                       options = list(create = TRUE)),
        selectizeInput("brandSelect", "Brand", summaryTable$brand,
                       selected = TRUE,
                       multiple = FALSE,
                    options = list(create = TRUE)),
        selectizeInput("styleSelect", "Style", summaryTable$style,
                       selected = TRUE,
                       multiple = FALSE,
                    options = list(create = TRUE)),
        numericInput("abv", "Alcohol by volume (abv)", 
                     min = 0, max = 100, 
                     value = first(summaryTable$abv)),
        numericInput("volume", "Volume (oz)", 
                     min = 0, 
                     value = first(summaryTable$lastVolume)),
        textInput('notes', "Notes"),
        actionButton('submit', "Drink!", 
                     class ='btn-lg btn-success'),
        column(4,
               tableOutput('table') #static table output
        ) #end column
      ) #end column
      ) #end fluidRow
    ), #end tabPanel
                tabPanel("BAC",
             fluidRow(
               column(4, 
                      selectInput("genderSelect", "Gender",
                                  choices = c("M", "F"),
                                        selected = userGender,
                                        multiple = FALSE),
                      numericInput("weight", "Weight (lbs)", value = userWeight,
                                   min = 0),
                      #textOutput("gender"),
                      #textOutput("rText"),
                      h5("Approximate BAC = "),
                      textOutput("BAC")
               ) #end column
             ) #end row
             ) #end tabPanel
    ), #end tabsetPanel
    
    # Begin footer
    fluidRow(
      column(4, align = 'center', 
             style = 'border: 1px solid black;
                      margin: 5px',
             div(
               class = 'footer',
               includeHTML('footer.html')
             ) #end div
             ) #end column
    ) #end fluidRow, footer
    
    
    
    


        

) #end fluidPage

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Convert all data to character to avoid known issue with renderTable dates.
  characterTable <- rawData%>%
    mutate(date = as.character(date),
           time = as.character(time))%>%
    arrange(desc(date), desc(time))
  output$table <-renderTable(head(characterTable))
  
  #on user select, Drink
  observeEvent(input$drinkSelect, {
    #Filter brands
    d <- summaryTable%>%filter(drink == input$drinkSelect)
    
    #Update brandSelect
    updateSelectInput(session, 'brandSelect', 
                      choices = d$brand)
  }) #end observeEvent
  
  #on user select, Brand
  observeEvent(input$brandSelect, {
    #Filter brands
    b <- summaryTable%>%filter(brand == input$brandSelect)
    
    #Update brandSelect
    updateSelectInput(session, 'styleSelect', 
                      choices = b$style)
  }) #end observeEvent
  
  #on user select, Style
  observeEvent(input$styleSelect, {
    #Filter style
    s <- summaryTable%>%filter(brand == input$brandSelect,
                               style == input$styleSelect)
    
    #Update brandSelect
    updateNumericInput(session, 'abv', 
                      value = s$abv)
  }) #end observeEvent

  #Begin BAC tab functions
  #output$gender <- renderText({ userGender })
  #output$rText <- renderText({ r })
  
  alcoholConsumed <- sum(rawData%>%
                           filter(date == format(as.Date(.POSIXct(Sys.time(), tz='UTC')-25200)))%>%
                           summarise(totalAlcohol = (as.numeric(abv)/100)*(as.numeric(volume)*29.574)))
  firstDrink <- as_datetime(paste(format(as.Date(.POSIXct(Sys.time(), tz='UTC')-25200)),first(subset(rawData, date == format(as.Date(.POSIXct(Sys.time(), tz='UTC')-25200)))$time)),
                            tz = "US/Pacific")
  elapsedTime <- as.numeric(difftime(Sys.time(), firstDrink, units = 'hours'))
  weightInG <- userWeight*453.6
  BAC <- (alcoholConsumed/(weightInG*r)*100)-(elapsedTime*0.015)
  BAC <- if_else(BAC <= 0, 0, BAC)
  output$BAC <- renderText({ round(BAC, 3) })
  
  #on user submit, "Drink!"
  observeEvent(input$submit, {
    
    #set variables
    #Minus 25200 seconds (7 hours) from UTC for PST time zone (i.e. US West Coast).
    #Users in other time zones will need to modify.
    date <- format(as.Date(.POSIXct(Sys.time(), tz='UTC')-25200))
    time <- format(.POSIXct(Sys.time(), tz='UTC')-25200, format = "%H:%M:%S")
    drink <- as.character(input$drinkSelect)
    brand <- as.character(input$brandSelect)
    style <- as.character(input$styleSelect)
    abv <- as.numeric(input$abv)
    volume <- as.numeric(input$volume)
    notes <- as.character(input$notes)
    
    #create row
    row <- c(date, time, drink, brand, style,
                         abv, volume, notes)
    #create df
    df <- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(df) <- c('date', 'time', 'drink', 'brand', 'style',
                  'abv', 'volume', 'notes')
    #bind row to df
    newData <- df%>%
      rbind(row)
    
    #print(newData)
    
    #Append raw data file
    #Connect to your google docs by inserting web address below.
    sheet_append("https://docs.google.com/Your_Sheet_Here",
                 newData)
    
    #Read in updated data
    #Connect to your google docs by inserting web address below.
    rawData <- as.data.frame(read_sheet("https://docs.google.com/Your_Sheet_Here"))%>%
                    #fix pesky NULL values
                    #notes column being read in as list, creating downstream errors.
                    #Don't know how to fix right now, so just dropping notes column.
                               select(-notes)
    #print(rawData)
    #Convert all data to character to avoid known issue with renderTable dates.
    characterTable <- rawData%>%
      mutate(date = as.character(date),
             time = as.character(time),
             abv = as.character(abv),
             volume = as.character(volume))%>%
      arrange(desc(date), desc(time))
    
    #Output updated table
    output$table <-renderTable(head(characterTable))
    #Update BAC
    alcoholConsumed <- sum(rawData%>%
                             filter(date == format(as.Date(.POSIXct(Sys.time(), tz='UTC')-25200)))%>%
                             summarise(totalAlcohol = (as.numeric(abv)/100)*(as.numeric(volume)*29.574)))
    firstDrink <- as_datetime(paste(format(as.Date(.POSIXct(Sys.time(), tz='UTC')-25200)),first(subset(rawData, date == format(as.Date(.POSIXct(Sys.time(), tz='UTC')-25200)))$time)),
                              tz = "US/Pacific")
    elapsedTime <- as.numeric(difftime(Sys.time(), firstDrink, units = 'hours'))
    weightInG <- userWeight*453.6
    BAC <- (alcoholConsumed/(weightInG*r)*100)-(elapsedTime*0.015)
    BAC <- if_else(BAC <= 0, 0, BAC)
    output$BAC <- renderText({ round(BAC, 3) })
  }) # End Input tab functions

} #end server function

# Run the application 
shinyApp(ui = ui, server = server)
