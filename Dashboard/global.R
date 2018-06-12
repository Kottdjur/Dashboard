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
colsYear<-c(rev(brewer.pal(6, "RdYlBu")), brewer.pal(6, "RdYlBu"))

Names<-c("Olof","Lina","Mahmoud","Ale","Maria","Anton","Tomas","Olle","Lisa")
groups <- data.frame(id = c("Week","Olof","Lina","Mahmoud","Ale","Maria","Anton","Tomas","Olle","Lisa"),
                     content = c("Vecka","Olof","Lina","Mahmoud","Ale","Maria","Anton","Tomas","Olle","Lisa"))

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
  content = weeks$weekNr,
  style = rep(NA, length(weeks[,1])),
  group = rep("Week", length(weeks[,1]))
)

# Load data from backup file on startup
# calendar <- read.csv("calendar_backup.csv", stringsAsFactors = FALSE)
startID <- tail(weekData$id, 1) + 1

# calendarData <- data.frame(
#   id = seq(startID, startID + length(calendar[,1]) -1, 1),
#   start = as.character(calendar$start),
#   end = as.character(calendar$end),
#   content = calendar$content,
#   style = rep(NA, length(calendar[,1])),
#   group = calendar$group
# )

# Data to load into calendar
# combinedData = rbind(weekData, calendarData)

fikaweek<-matrix(cbind(weeks$weekNr, rep(Names,length.out=length(weeks[,1]))), ncol=2)
fikaWeek <- data.frame(
  week = weeks$weekNr,
  name = rep(Names,length.out=length(weeks[,1]))
)

# Random ID generator for adding events interactively
randomID <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
}
