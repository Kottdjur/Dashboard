library(shiny)
library(httr)
library(jsonlite)
library(timevis)
library(lubridate)

shinyApp(
  
  # UI
  ui = fluidPage(
    tableOutput("test"),
    tableOutput("combined"),
    timevisOutput("schema")
  ),
  
  # Server
  server = function(input, output, session) {
    
    groups <- data.frame(id = c("Week","Olof","Lina","Mahmoud","Ale","Maria","Anton","Tomas","Olle","Lisa"),
                         content = c("Vecka","Olof","Lina","Mahmoud","Ale","Maria","Anton","Tomas","Olle","Lisa"))
    
    # Week stuff
    weekStart = floor_date(Sys.Date() + weeks(-20:20), unit="week")+days(1)
    weeks <- data.frame(
      weekStart = weekStart,
      weekEnd = weekStart + days(7),
      weekNr = week(weekStart)
    )
    
    weekData <- data.frame(
      id = seq(1, length(weeks[,1]), 1),
      start = as.character(weeks$weekStart),
      end = as.character(weeks$weekEnd),
      content = weeks$weekNr,
      style = rep(NA, length(weeks[,1])),
      group = rep("Week", length(weeks[,1]))
    )
    
    startID <- tail(weekData$id, 1) + 1
    
    calendarRV <- reactiveFileReader(1000, session, "C:/Users/kottd/Documents/Dashboard/Test/calendar_backup.csv", read.csv, stringsAsFactors = FALSE)
    output$test <- renderTable({calendarRV()})
    
    rv <- reactiveValues()
    
    combinedData <- eventReactive(calendarRV, {
      print("calendar updated")
      
      calendar <- calendarRV()
      
      calendarData <- data.frame(
        id = seq(startID, startID + length(calendar[,1]) -1, 1),
        start = as.character(calendar$start),
        end = as.character(calendar$end),
        content = calendar$content,
        style = rep(NA, length(calendar[,1])),
        group = calendar$group
      )
      
      combinedData = rbind(weekData, calendarData)
      
      return(combinedData)
    })
    
    output$combined <- renderTable({combinedData()})
    
    # Schedule plot
    output$schema <- renderTimevis({
      combinedData <- combinedData()
      
      timevis(combinedData,
              showZoom = FALSE, fit = FALSE,
              groups = groups,
              options = list(editable = c(remove = TRUE, updateGroup = TRUE),
                             zoomable = FALSE,
                             timeAxis = c(scale = "weekday", step = 1),
                             start = floor_date(Sys.Date(), unit="week")+days(1),
                             end = ceiling_date(Sys.Date(), unit="week")+days(1),
                             #min = floor_date(Sys.Date(), unit="week")+days(1),
                             stack = FALSE
              )
      )
    })
  }
  
)