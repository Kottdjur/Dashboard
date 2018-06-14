# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  temp <- reactiveVal(fromJSON(urlWeather)$main$temp)
  
  # Refresh day and week every hour
  today <- reactive({
    invalidateLater(3600000)
    Sys.Date()
  })
  weekNr <- reactive({
    invalidateLater(3600000)
    week(Sys.Date())
  })
  
  # Where to put this? One file reader per session or one shared with all sessions.
  #backup_file = "H:/Dokument/Dashboard/Dashboard/calendar_backup.csv"
  backup_file = "C:/Users/kottd/Documents/Dashboard/Dashboard/calendar_backup.csv"
  calendarRV <- reactiveFileReader(1000, session, backup_file, read.csv, stringsAsFactors = FALSE)
  
  # Update calendar when the csv is changed
  combinedData <- reactive({
    calendar <- calendarRV()
    
    calendarData <- data.frame(
      id = seq(startID, startID + length(calendar[, 1]) - 1, 1),
      start = as.character(calendar$start),
      end = as.character(calendar$end),
      content = calendar$content,
      style = rep(NA, length(calendar[, 1])),
      group = calendar$group
    )
    
    combinedData = rbind(weekData, calendarData)
    
    return(combinedData)
  })
  
  # Schedule plot
  output$schema <- renderTimevis({
    timevis(
      combinedData(),
      showZoom = FALSE,
      fit = FALSE,
      groups = groups,
      options = list(
        editable = c(remove = TRUE),
        zoomable = FALSE,
        timeAxis = c(scale = "weekday", step = 1),
        start = floor_date(today(), unit = "week") + days(1),
        end = ceiling_date(today(), unit = "week") + days(1),
        stack = FALSE
      )
    )
  })
  
  # Add events directly to csv file
  observeEvent(input$addEvent, {
    appendVec <-
      c(
        as.character(input$startDate),
        as.character(input$endDate),
        input$eventName,
        input$person
      )
    appendLine <- paste(appendVec, collapse = ",")
    write(appendLine, file = backup_file, append = TRUE)
  })
  
  # Save csv backup of calendar when something is removed via the UI
  observeEvent(input$schema_data, {
    newData <- input$schema_data
    newData <- newData[!(newData$group == "Week"), ]
    oldData <- calendarRV()
    
    if (length(newData[, 1]) < length(oldData[, 1])) {
      backupTable <- newData
      backupTable$id = NULL
      write.csv(backupTable, file = backup_file, row.names = FALSE)
    }
  })
  
  # Focus on selected item
  observeEvent(input$schema_selected, {
    centerItem("schema", input$schema_selected)
  })
  
  fika <- reactiveVal(1)
  
  observeEvent(weekNr(), {
    fika(fikaStep(1))
  })
  
  observeEvent(input$fikaNext, {
    fika(fikaStep(1))
  })
  
  observeEvent(input$fikaBack, {
    fika(fikaStep(-1))
  })
  
  output$weekNr <- renderValueBox({
    valueBox(
      paste("Vecka", weekNr()),
      today(),
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$fika <- renderValueBox({
    valueBox(Names[fika()],
             "Fika Ansvarig",
             icon = icon("coffee"),
             color = "green")
  })
  
  output$fikaNext <- renderValueBox({
    valueBox(
      Names[fikaStep(1)],
      "Nästa Fika Ansvarig",
      icon = icon("angle-double-right"),
      color = "yellow"
    )
  })
  
  output$temp <- renderValueBox({
    valueBox(
      paste(temp(), "°C"),
      ifelse(temp() <= 0, "Brrrr...", "Nice..."),
      icon = icon("thermometer-half"),
      color = ifelse(temp() <= 0, "aqua", ifelse(temp() <= 10, "green", "yellow"))
    )
  })
  
  # Get the index of the next person in the fika list
  fikaStep <- function(step) {
    if (fika() >= length(Names) & step > 0) {
      return(1)
    }
    else if (fika() == 1 & step < 0) {
      return(length(Names))
    }
    else {
      return(fika() + step)
    }
  }
  
}