library(shiny)
library(shinydashboard)
# library(shinyjs)
# library(shinyWidgets)
library(RColorBrewer)
library(httr)
library(jsonlite)
library(timevis)
library(lubridate)

library("googlesheets")
suppressPackageStartupMessages(library("dplyr"))
# http://127.0.0.1:13930/library/googlesheets/html/gs_auth.html
# https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/


# url<-"https://greenswayab-my.sharepoint.com/:x:/r/personal/alejandro_greensway_se/_layouts/15/WopiFrame.aspx?sourcedoc=%7B04178A94-EF2B-4C6B-91F2-C2C1E2FB3BF8%7D&file=Semester%202018.csv"
# 
# GET(url, write_disk("semester.csv", overwrite=TRUE))  
# frmData <- read.csv("semester.csv", header=TRUE, sep=",", encoding="UTF-8") 
# 

setwd("C:/Users/kottd/Documents/Dashboard/Dashboard")
#setwd("H:/Dokument/Dashboard/Dashboard")
colsYear<-c(rev(brewer.pal(6, "RdYlBu")), brewer.pal(6, "RdYlBu"))

# In order of the fika list
Names<-c("Olof","Lina","Mahmoud","Ale","Maria","Anton","Tomas","Olle","Lisa")

groups <- data.frame(id = c("Week", Names),
                     content = c("Vecka", Names),
                     className = c("Week", rep("Names", length(Names)))
                     )

urlWeather<-"https://api.openweathermap.org/data/2.5/weather?q=Uppsala,SE&units=metric&appid=97ff4d6525b004787e9152705e0db608"

# Start and end of weeks + week nrs
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
  content = paste("Vecka", weeks$weekNr),
  className = rep("Week", length(weeks[,1])),
  group = rep("Week", length(weeks[,1]))
)

startID <- tail(weekData$id, 1) + 1

# Random ID generator for adding events interactively
randomID <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
}
