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
##### V1.1.0 9/2/23: Feat: Create BAC calculator.
##### V1.1.1 9/3/23: Bug fix: default elapsedTime difftime function to units = 'hours'.
#####                 Bug fix: remove today() function, replace with universal function with specified timezone.
##### V1.2.0 9/3/23: Feat: Real time update BAC.
##### V1.2.1 1/2/23: Bug fix: Fix gender and weight settings for BAC calculator.
##### V1.3.0 1/5/23: Feat: Estimate time till sober.
#####                 Bug fix: Daylight Savings Time fix.
##### V1.3.1 1/8/23: Bug fix: Fix bad regex expression in tSober() calculation.
#####                 Bug fix: Fix lastDrink variable to account for non-alcoholic beverages.

##### To Do: ####
##### Create figures page
##### Summary data 
##### Publish updated script to github.
##### Sanitize inputs. Crashes if weight = NULL.
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

#Required auth. token for shinyapp.io to access gsheets.
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  # Below script will write ".sectrets" folder to your directory, or other directory as specified below.
  # Do not share your ".secrets" to prevent unauthorized use.
  gargle_oauth_cache = ".secrets"
)

##User Input Required!!
##Update myGsheetUrl to link to your google data sheet.
myGsheetUrl <- "https://docs.google.com/spreadsheets/Your_URL_Here"

rawData <- as.data.frame(read_sheet(myGsheetUrl))%>%
  #fix pesky NULL values
  #notes column being read in as list, creating downstream errors.
  #Don't know how to fix right now, so just dropping notes column.
  select(-notes)
#Connect to your google docs by inserting web address below.
#rawData <- read_sheet("https://docs.google.com/Your_Sheet_Here")

1 #google sheets option, grant permissions. Will require user browser input on first usage.

summaryTable <- rawData%>%
  mutate(abv = as.numeric(abv),
         volume = as.numeric(volume))%>%
  group_by(drink, brand, style, abv)%>%
  summarise(lastDate = max(date),
            lastVolume = last(volume))%>%
  arrange(desc(lastDate))

#r used for calculating BAC
#r(male)= .68; r(female)= .55

defaltWeight <- 170
tz <- "US/Pacific"
tzAdjust <- 28800 #seconds, i.e. 8 hours
dateTime <- .POSIXct(Sys.time(), tz='US/Pacific')#-tzAdjust

#Determine DST boundary dates in current calender year.
dst_boundaries <- {
  # [start, end)
  # example: [2021-01-01, 2022-01-01)
  start <- date_time_build(year(Sys.time()), zone = tz)
  end <- date_time_build(year(Sys.time()) + 1L, zone = tz)
  
  start <- as_sys_time(start)
  end <- as_sys_time(end)
  
  # An empty vector of date-times that we will add boundaries to
  boundaries <- .POSIXct(double(), tz = tz)
  
  repeat {
    # Look up DST info based on the current `start`.
    # It'll find the previous and next DST boundary.
    info <- sys_time_info(start, tz)
    boundary <- info$end
    
    # Is the DST boundary outside this year? If so, we are done.
    if (boundary >= end) {
      break
    }
    
    # Otherwise add it to our growing list of boundaries
    boundaries <- c(boundaries, as.POSIXct(boundary, tz = tz))
    start <- boundary
  }
  
  boundaries
} # end dst_boundaries

#Modify tzAdjust for DST.
if (dateTime >= dst_boundaries[1] & dateTime <= dst_boundaries[2]) {
  tzAdjust <- 25200 #seconds, i.e. 7 hours
  #dateTime <- .POSIXct(Sys.time(), tz='UTC')-tzAdjust
} # end if

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
                      selectInput("genderSelect", 
                                  label = tags$span("Gender",
                                                    tags$i(
                                                      class = "glyphicon glyphicon-info-sign", 
                                                      style = "color:#0072B2;",
                                                      title = "Formulas used to calculate BAC use sex assigned at birth. This is a limitation of the available science, and validated equations to calculate estimated BAC for people who are trans or non-binary have not been published."
                                                    ),
                                                
                                        ),
                                  choices = c("M", "F"),
                                        selected = "M", #Change default value as desired
                                        multiple = FALSE),
                      numericInput("weight", "Weight (lbs)", value = defaltWeight, #Change default value as desired
                                   min = 0),
               ) #end column
             ), #end row
             fluidRow(
                      column(4,
                        htmlOutput("BAC"),
                      ), #end column
             ), #end row
             fluidRow(
                      column(4,
                        textOutput("soberEst")
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

# Define server logic
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
  r <- reactiveVal(0.68) #Defalut r for M.
  BAC <- reactiveVal(0)
  userWeight <- reactiveVal(defaltWeight) #match value to default input$weight
  alcoholConsumed <- reactiveVal(sum(rawData%>%
                           filter(date == format(as.Date(.POSIXct(Sys.time(), tz='UTC')-tzAdjust)))%>%
                           reframe(totalAlcohol = (as.numeric(abv)/100)*(as.numeric(volume)*29.574))))
  lastDrink <<- as_datetime(paste(last(subset(rawData, abv > 0)$date), last(rawData$time), sep = " "),
                            tz = tz)
  firstDrink <- as_datetime(paste(format(as.Date(.POSIXct(lastDrink, tz='UTC')-tzAdjust)),
                                  first(subset(rawData, date == format(as.Date(.POSIXct(lastDrink, tz='UTC')-tzAdjust)))$time)),
                            tz = tz)
  elapsedTime <- as.numeric(difftime(Sys.time(), firstDrink, units = 'hours'))
  tToSober <- reactiveVal(0)
  tSober <- reactiveVal(0)
  soberMessage <- reactiveVal("You'll be sober in")
  
  observeEvent(input$weight, {
    w <- input$weight
    userWeight(w)
    
    g <- input$genderSelect
    if (g == "F"){
      r(0.55)
    } else { r(0.68) }
    
    weightInG <- userWeight()*453.6
    
    BAC((alcoholConsumed()/(weightInG*r())*100)-(elapsedTime*0.015))
    
    if (BAC() <= 0){
      BAC(0)
      
      firstDrink <- as_datetime(paste(first(subset(rawData, date == as.Date(lastDrink, tz = tz))$date), 
                                      first(subset(rawData, date == as.Date(lastDrink, tz = tz))$time), 
                                      sep = " "),
                                tz = tz)
      lastTotalAlc <- sum(rawData%>%
                            filter(date == as.Date(lastDrink, tz = tz))%>%
                            reframe(totalAlcohol = (as.numeric(abv)/100)*(as.numeric(volume)*29.574)))
      elapsedTime <- as.numeric(difftime(lastDrink, firstDrink, units = 'hours'))
      peakBAC <- (lastTotalAlc/(weightInG*r())*100)-(elapsedTime*0.015)
      t0 <- lastDrink + ((peakBAC/0.015)*3600)
      
      tSober(difftime(dateTime, t0, units = "days"))
      
      soberMessage("Congratulations! You've been sober for")
      if (tSober() > 0) {
      output$soberEst <- renderText({ paste(soberMessage(), 
                                            sub("\\.\\d+$", "", tSober()), "days",
                                            round(round(as.numeric(sub("^\\d+\\.", "0.", tSober())), 2)*24, 0),
                                            "hours", sep = " ") })
      } else { #if tSober() < 0 (bug fix if weight is extremely small value, typically a transitory state)
        tSober(0)
        output$soberEst <- renderText({ paste(soberMessage(), 
                                              sub("\\.\\d+$", "", tSober()), "days",
                                              round(as.numeric(sub("^\\d+\\.", "0.", tSober()))*24, 0),
                                              "hours", sep = " ") })
      }
    } else { #if BAC >= 0
      
      if (round(BAC(), 3) >= 0.08) {
        output$BAC <- renderText({ paste(tags$b("Approximate BAC =", 
                                                tags$span(style = "color: red",
                                                          round(BAC(), 3)), 
                                                sep = " ")) })
      } else {
        output$BAC <- renderText({ paste(tags$b("Approximate BAC =", round(BAC(), 3)), 
                                         sep = " ") })
      }
      
      tToSober((BAC()/0.015))
      if (tToSober() >= 1) {
        output$soberEst <- renderText({ paste(soberMessage(), 
                                              sub("\\.\\d+$", "", tToSober()), "hours",
                                              round(as.numeric(sub("^(.*?)\\.", "0.", tToSober()))*60, 0),
                                              "minutes", sep = " ") })
      }
      
      if (tToSober() < 1) {
        output$soberEst <- renderText({ paste(soberMessage(), 
                                              round((tToSober() *60), 0), "minutes",
                                              sep = " ") })
      }
    } #end else
  }) # end observeEvent weight
  
  observeEvent(input$genderSelect, {
    g <- input$genderSelect
    if (g == "F"){
      r(0.55)
    } else { r(0.68) }
    
    w <- input$weight
    userWeight(w)
    weightInG <- userWeight()*453.6
    
    BAC((alcoholConsumed()/(weightInG*r())*100)-(elapsedTime*0.015))
    
    if (BAC() <= 0){
      BAC(0)
      
      firstDrink <- as_datetime(paste(first(subset(rawData, date == as.Date(lastDrink, tz = tz))$date), 
                                      first(subset(rawData, date == as.Date(lastDrink, tz = tz))$time), 
                                      sep = " "),
                                tz = tz)
      lastTotalAlc <- sum(rawData%>%
                            filter(date == as.Date(lastDrink, tz = tz))%>%
                            reframe(totalAlcohol = (as.numeric(abv)/100)*(as.numeric(volume)*29.574)))
      elapsedTime <- as.numeric(difftime(lastDrink, firstDrink, units = 'hours'))
      peakBAC <- (lastTotalAlc/(weightInG*r())*100)-(elapsedTime*0.015)
      #temp <- (peakBAC/0.015)*3600
      t0 <- lastDrink + ((peakBAC/0.015)*3600)
      
      tSober(difftime(dateTime, t0, units = "days"))
      
      soberMessage("Congratulations! You've been sober for")
      if (tSober() > 0) {
        output$soberEst <- renderText({ paste(soberMessage(), 
                                              sub("\\.\\d+$", "", tSober()), "days",
                                              round(round(as.numeric(sub("^\\d+\\.", "0.", tSober())), 2)*24, 0),
                                              "hours", sep = " ") })
      } else { #if tSober() < 0 (bug fix if weight is extremely small value, typically a transitory state)
        tSober(0)
        output$soberEst <- renderText({ paste(soberMessage(), 
                                              sub("\\.\\d+$", "", tSober()), "days",
                                              round(as.numeric(sub("^\\d+\\.", "0.", tSober()))*24, 0),
                                              "hours", sep = " ") })
        }
      } else { #if BAC >= 0
      soberMessage("You'll be sober in")
      
    if (round(BAC(), 3) >= 0.08) {
      output$BAC <- renderText({ paste(tags$b("Approximate BAC =", 
                                              tags$span(style = "color: red",
                                                        round(BAC(), 3)), 
                                              sep = " ")) })
    } else {
      output$BAC <- renderText({ paste(tags$b("Approximate BAC =", round(BAC(), 3)), 
                                       sep = " ") })
    }
    
    tToSober((BAC()/0.015))
    if (tToSober() >= 1) {
      output$soberEst <- renderText({ paste(soberMessage(), 
                                            sub("\\.\\d+$", "", tToSober()), "hours",
                                            round(as.numeric(sub("^(.*?)\\.", "0.", tToSober()))*60, 0),
                                            "minutes", sep = " ") })
    }
    
    if (tToSober() < 1) {
      output$soberEst <- renderText({ paste(soberMessage(), 
                                            round((tToSober() *60), 0), "minutes",
                                            sep = " ") })
    }
    }
  }) #end observeEvent gender select
  
  output$BAC <- renderText({ paste(tags$b("Approximate BAC =", round(BAC(), 3), sep = " ")) })
  #if statement must be in reactive context
  ##Save snipet for later
  #if (BAC() >= 0.08) {
   # output$BAC <- renderText({ paste(tags$b("Approximate BAC =", "<font color=\"#FF0000\">", round(BAC(), 3), "</font>", sep = " ")) })
  #} #end if
  
  output$soberEst <- renderText({ paste(soberMessage(), 
                                        round(tToSober(), 0), "hours",
                                        round((tToSober() - round(tToSober(), 0)) *60, 0), "minutes",
                                        sep = " ") })
  
  
  #on user submit, "Drink!"
  observeEvent(input$submit, {
    
    #set variables
    #Minus 25200 seconds (7 hours) from UTC for PST time zone (i.e. US West Coast).
    #Users in other time zones will need to modify.
    date <- format(as.Date(.POSIXct(Sys.time(), tz='UTC')-tzAdjust))
    time <- format(.POSIXct(Sys.time(), tz='UTC')-tzAdjust, format = "%H:%M:%S")
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
    sheet_append(myGsheetUrl,
                 newData)
    
    #Read in updated data
    rawData <<- as.data.frame(read_sheet(myGsheetUrl))%>%
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
    output$table <- renderTable(head(characterTable))
    
    #Update BAC
    alcoholConsumed(sum(rawData%>%
                             filter(date == format(as.Date(.POSIXct(Sys.time(), tz='UTC')-tzAdjust)))%>%
                             reframe(totalAlcohol = (as.numeric(abv)/100)*(as.numeric(volume)*29.574))))
    lastDrink <<- as_datetime(paste(last(subset(rawData, abv > 0)$date), last(rawData$time), sep = " "),
                              tz = tz)
    firstDrink <<- as_datetime(paste(format(as.Date(.POSIXct(lastDrink, tz='UTC')-tzAdjust)),
                                    first(subset(rawData, date == format(as.Date(.POSIXct(lastDrink, tz='UTC')-tzAdjust)))$time)),
                              tz = tz)
    elapsedTime <<- as.numeric(difftime(Sys.time(), firstDrink, units = 'hours'))
    weightInG <<- userWeight()*453.6
    BAC((alcoholConsumed()/(weightInG*r())*100)-(elapsedTime*0.015))
    
    #print(BAC())
    #print(firstDrink)
    #print(lastDrink)
    
    if (BAC() <= 0){ #only applicable with non-alcoholic beverages.
      BAC(0)
      
      firstDrink <- as_datetime(paste(first(subset(rawData, date == as.Date(lastDrink, tz = tz))$date), 
                                      first(subset(rawData, date == as.Date(lastDrink, tz = tz))$time), 
                                      sep = " "),
                                tz = tz)
      lastTotalAlc <- sum(rawData%>%
                            filter(date == as.Date(lastDrink, tz = tz))%>%
                            reframe(totalAlcohol = (as.numeric(abv)/100)*(as.numeric(volume)*29.574)))
      elapsedTime <- as.numeric(difftime(lastDrink, firstDrink, units = 'hours'))
      peakBAC <- (lastTotalAlc/(weightInG*r())*100)-(elapsedTime*0.015)
      
      t0 <- lastDrink + ((peakBAC/0.015)*3600)
      
      tSober(difftime(dateTime, t0, units = "days"))
      
      soberMessage("Congratulations! You've been sober for")
      if (tSober() > 0) {
        output$soberEst <- renderText({ paste(soberMessage(), 
                                              sub("\\.\\d+$", "", tSober()), "days",
                                              round(round(as.numeric(sub("^\\d+\\.", "0.", tSober())), 2)*24, 0),
                                              "hours", sep = " ") })
      } else { #if tSober() < 0 (bug fix if weight is extremely small value, typically a transitory state)
        tSober(0)
        output$soberEst <- renderText({ paste(soberMessage(), 
                                              sub("\\.\\d+$", "", tSober()), "days",
                                              round(as.numeric(sub("^\\d+\\.", "0.", tSober()))*24, 0),
                                              "hours", sep = " ") })
        }
      } else { #if BAC >= 0
      soberMessage("You'll be sober in")
      tToSober((BAC()/0.015))
      if (tToSober() >= 1) {
        output$soberEst <- renderText({ paste(soberMessage(), 
                                              sub("\\.\\d+$", "", tToSober()), "hours",
                                              round(as.numeric(sub("^(.*?)\\.", "0.", tToSober()))*60, 0),
                                              "minutes", sep = " ") })
      }
      
      if (tToSober() < 1) {
        output$soberEst <- renderText({ paste(soberMessage(), 
                                              round((tToSober() *60), 0), "minutes",
                                              sep = " ") })
      }
    
    if (round(BAC(), 3) >= 0.08) {
      output$BAC <- renderText({ paste(tags$b("Approximate BAC =", 
                                              tags$span(style = "color: red",
                                                        round(BAC(), 3)), 
                                              sep = " ")) })
    } else {
      output$BAC <- renderText({ paste(tags$b("Approximate BAC =", round(BAC(), 3)), 
                                       sep = " ") })
    }
    
    #print(tzAdjust)
    }
  }) # End observeEvent Drink
  
  
  
  

} #end server function

# Run the application 
shinyApp(ui = ui, server = server)
