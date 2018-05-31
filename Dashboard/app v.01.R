library(shiny)
library(shinydashboard)
# library(shinyjs)
# library(shinyWidgets)
library(RColorBrewer)
library(httr)
library(jsonlite)

library("googlesheets")
suppressPackageStartupMessages(library("dplyr"))
# http://127.0.0.1:13930/library/googlesheets/html/gs_auth.html
# https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/
  

# url<-"https://greenswayab-my.sharepoint.com/:x:/r/personal/alejandro_greensway_se/_layouts/15/WopiFrame.aspx?sourcedoc=%7B04178A94-EF2B-4C6B-91F2-C2C1E2FB3BF8%7D&file=Semester%202018.csv"
# 
# GET(url, write_disk("semester.csv", overwrite=TRUE))  
# frmData <- read.csv("semester.csv", header=TRUE, sep=",", encoding="UTF-8") 
# 


# setwd("/home/pi/Documents/GSdashboard")
colsYear<-c(rev(brewer.pal(6, "RdYlBu")), brewer.pal(6, "RdYlBu"))


semester<-read.csv("Semester 2018.csv", encoding = "UTF-8", sep=",")
semester01<-ifelse(semester[,4:length(semester)]=="" | is.na(semester[,4:length(semester)]), 0, 1)
semester[,4:length(semester)]<-0
semester[,4:length(semester)]<-lapply(semester[,4:length(semester)], function(x) {
  if(is.factor(x)) as.numeric(0) else x
  if(is.logical(x)) as.numeric(x) else x
})
semester[,4:length(semester)]<-semester01

weeks<-strftime(as.Date(as.character(semester[,2]), "%d/%m/%Y"), "%V")
weeksMonth<-as.numeric(strftime(as.Date(as.character(semester[,2]), "%d/%m/%Y"), "%m"))
monthID<-cumsum(rle(weeksMonth)$lengths)
WeekStart<-as.Date(as.character(semester[,2]), "%d/%m/%Y")
weeksAxis01<-seq(0,1, length.out = length(weeks))+0.0106383
Names<-c("Olof","Lina","Mahmoud","Ale","Maria","Anton","Tomas","Olle","Lisa")

# weeks<-seq(5,52)
fikaweek<-matrix(cbind(weeks, rep(Names,length.out=length(weeks))), ncol=2)
 
urlWeather<-"https://api.openweathermap.org/data/2.5/weather?q=Uppsala,SE&units=metric&appid=97ff4d6525b004787e9152705e0db608"


header<-  dashboardHeader(title=HTML('<div><img src="./Greensway-Logo-180307-3.png" alt="Greensway" height="50" style="vertical-align:middle;"></div>'), 
                          titleWidth = 230)

body<-dashboardBody(
  # useShinyjs(),
  tags$head(
    tags$link(rel="shortcut icon", href="./Greensway-Micro-logo-green.png"),
    # Include our custom CSS
    # includeCSS("styles.css"),
    # tags$style(HTML('.info-box {min-height: 55px;} .info-box-icon {height: 55px; line-height: 55px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}')),
    tags$script('!function(d,s,id){
                                var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';
                                if(!d.getElementById(id)){js=d.createElement(s);
                                  js.id=id;
                                  js.src=p+"://platform.twitter.com/widgets.js";
                                  fjs.parentNode.insertBefore(js,fjs);
                                }}(document,"script","twitter-wjs");')
  ),
  column(9,
    fluidRow(
      valueBoxOutput("weekNr", width = 3),
      # infoBoxOutput("weekNr", width = 3),
      valueBoxOutput("temp", width = 3),
      valueBoxOutput("fika", width = 3),
      valueBoxOutput("fikaNext", width = 3)
    ),
    fluidRow(  
     box(title = "Semester", width = 12, status = "success",  solidHeader = TRUE,
        # h4("Semester Planner"),
        # uiOutput("choices")
        plotOutput("hm", width = "97%")
     )
    )
  ),
  column(3,
    box(width=NULL, height="auto",
              a("Tweets by @GreenswayEco", class="twitter-timeline",
                #href = "https://twitter.com/search?q=GreenswayEco"
                href = "https://twitter.com/GreenswayEco"
              )
    )
  )
  
 
)

# Define UI for application that draws a histogram
ui <- dashboardPage(title="Greensway Week",
  header,
  sidebar = dashboardSidebar( disable = TRUE),
  body,
  skin = "green")


# Define server logic required to draw a histogram
server <- function(input, output) {

  today<-reactiveVal(as.Date(Sys.Date()))
  weekNr<-reactiveVal(as.numeric(strftime(as.Date(Sys.Date()), format = "%V")))
  temp<-reactiveVal(fromJSON(urlWeather)$main$temp)
  
  output$hm <- renderPlot({
    par(mar=c(3,5,2,1), las=1, cex=1.5)
    image(semester01, col=c("white", "#00A65A"), xaxt="n", yaxt="n", ylim=c(1.05,-0.05))
    
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

# Run the application 
shinyApp(ui = ui, server = server)

