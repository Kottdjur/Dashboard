# Define server logic required to draw a histogram
server <- function(input, output) {
  
  today <- reactiveVal(as.Date(Sys.Date()))
  weekNr <- reactiveVal(week(Sys.Date()))
  temp <- reactiveVal(fromJSON(urlWeather)$main$temp)
  
  rv <- reactiveValues()
  rv$calendar <- read.csv("calendar_backup.csv", stringsAsFactors = FALSE)
  
  # Schedule plot
  output$schema <- renderTimevis(timevis(combinedData,
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
  )
  
  # Add events to schedule
  observeEvent(input$addEvent, {
    addItem("schema",
            data = list(id = randomID(),
                        content = input$eventName,
                        start = input$startDate,
                        end = input$endDate,
                        group = input$person
                        ))
  })
  
  # Focus on selected item
  observeEvent(input$schema_selected, {
    centerItem("schema", input$schema_selected)
  })
  
  # Save csv backup of calendar every time something changes
  observeEvent(input$schema_data, {
    backupTable <- input$schema_data
    # Don't save week numbers or IDs.
    backupTable <- backupTable[!(backupTable$group == "Week"),]
    backupTable$id = NULL
    write.csv(backupTable, file = "calendar_backup.csv", row.names = FALSE)
  })
  
  output$weekNr <- renderValueBox({
    valueBox(paste("Vecka",weekNr()), today(), icon = icon("calendar"), color = "blue")
  })
  # output$weekNr <- renderInfoBox({
  #   infoBox(title=h3(paste("Vecka",weekNr())), today(), icon = icon("calendar"), color = "aqua", fill = TRUE)
  # })
  output$fika <- renderValueBox({
    valueBox(fikaWeek[fikaWeek$week == weekNr,][["name"]], "Fika Ansvarig", icon = icon("coffee"), color = "green")
  })
  output$fikaNext <- renderValueBox({
    valueBox(fikaWeek[fikaWeek$week == weekNr+1,][["name"]], "Nästa Fika Ansvarig", icon = icon("angle-double-right"), color = "yellow")
  })
  output$temp <- renderValueBox({
    valueBox(paste(temp(),"°C"), ifelse(temp()<=0, "Brrrr...", "Nice..."), icon = icon("thermometer-half"), color = ifelse(temp()<=0, "aqua", ifelse(temp()<=10, "green","yellow")))
  })
  
  # output$choices<-renderUI({
  #   checkboxGroupButtons(
  #     inputId = "OlofS", label = "Olof", choices = weeks, 
  #     justified = TRUE, status = "success", size = "lg",
  #     selected = weeks[which(semester[,"Olof"]==1)],
  #     checkIcon = list(yes = icon("ok", lib = "glyphicon")) #, no = icon("remove", lib = "glyphicon")
  #   ) 
  # })
  
  # Temp data table output
  output$table <- renderTable({
    data <- input$schema_data
    data
  })
}