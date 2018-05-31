# Define server logic required to draw a histogram
server <- function(input, output) {
  
  today<-reactiveVal(as.Date(Sys.Date()))
  weekNr<-reactiveVal(as.numeric(strftime(as.Date(Sys.Date()), format = "%V")))
  temp<-reactiveVal(fromJSON(urlWeather)$main$temp)
  
  # Schedule plot
  output$hm <- renderPlot({
    par(mar=c(3,5,2,1), las=1, cex=1.5)
    image(semester01, col=brewer.pal(3,"Set2"), xaxt="n", yaxt="n", ylim=c(1.05,-0.05))
    
    abline(v=weeksAxis01, lty=3, col=colsYear[weeksMonth], lwd=1)
    mtext(text = "Vecka", side= 1, line = 2, cex = 1.5)
    axis(1, at=seq(0,1, length.out=length(weeks)), weeks)
    axis(2, at=seq(0,1, length.out=length(Names)), Names)
    # axis(3, at=seq(0,1, length.out = 11), labels = "")
    axis(3, at=weeksAxis01[monthID], month.abb[c(2:12,1)], las=1, hadj = -.7)
    polygon(x=c(weeksAxis01[weekNr()-5], weeksAxis01[weekNr()-5], weeksAxis01[weekNr()-4], weeksAxis01[weekNr()-4]), 
            y= c(-.5,1.5,1.5,-.5), border=0, col="#605CA830", lwd=3)
    graphics::box()
  })
  
  output$weekNr <- renderValueBox({
    valueBox(paste("Vecka",weekNr()), today(), icon = icon("calendar"), color = "blue")
  })
  # output$weekNr <- renderInfoBox({
  #   infoBox(title=h3(paste("Vecka",weekNr())), today(), icon = icon("calendar"), color = "aqua", fill = TRUE)
  # })
  output$fika <- renderValueBox({
    valueBox(fikaweek[which(weeks==weekNr()),2], "Fika Ansvarig", icon = icon("coffee"), color = "green")
  })
  output$fikaNext <- renderValueBox({
    valueBox(fikaweek[which(weeks==weekNr())+1,2], "Nästa Fika Ansvarig", icon = icon("angle-double-right"), color = "yellow")
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
}