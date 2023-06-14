#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##### Travis Zalesky
##### Last Update: 6/14/23
##### Objective: Create a custom app for tracking and visualizing my alcohol consumption habits.
##### Version History ####
##### V0.0.0 6/14/23: Draft
##### V1.0.0 6/14/23: First working commit. Data logger only.


##### Begin app #####
##Install required packages if not already insalled.
#install.packages(shiny)
#install.packages(googlesheets4)
#install.packages(tidyverse)
#install.packages(lubridate)
library(shiny)
library(googlesheets4)
library(tidyverse)
library(lubridate)

#Required auth. token for shinyapp.io to access gsheets.
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
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


# Define UI for application
ui <- fluidPage(lang='en',
                theme = bslib::bs_theme(bootswatch = 'sandstone'),
                

    # Application title
    titlePanel(windowTitle = "Alcohol Consumption Data Tracker App",
               column(4,
                      "Alcohol Consumption\nData Logger App"
                      ) #end column
               ), #end titlePanel
    
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
                     class ='btn-lg btn-success')
      ), #end column
      column(4,
        tableOutput('table') #static table output
      ) #end column
    ), #end fluidRow
    
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
    sheet_append('https://docs.google.com/spreadsheets/d/1hJzB6tUMgvErbkr_r0_x_7E1TPJD-Dr-9Aaa-KtZcsQ/edit#gid=0',
                 newData)
    
    #Read in updated data
    rawData <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1hJzB6tUMgvErbkr_r0_x_7E1TPJD-Dr-9Aaa-KtZcsQ/edit#gid=0"))%>%
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
  })

} #end server function

# Run the application 
shinyApp(ui = ui, server = server)