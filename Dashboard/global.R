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


semester<-read.csv("Semester 2018.csv", encoding = "UTF-8", sep=",", stringsAsFactors = FALSE)
#semester01<-ifelse(semester[,4:length(semester)]=="" | is.na(semester[,4:length(semester)]), 0, 1)
semester[semester == ""] <- as.numeric(0)
semester[is.na(semester)] <- as.numeric(0)
semester[semester == "SEMESTER"] <- as.numeric(1)
semester[semester == "FIELD"] <- as.numeric(2)
semester01<-as.matrix(semester[,4:length(semester)])

#semester[,4:length(semester)]<-lapply(semester[,4:length(semester)], function(x) {
#  if(is.factor(x)) as.numeric(0) else x
#  if(is.logical(x)) as.numeric(x) else x
#})
#semester[,4:length(semester)]<-semester01

#weeks<-strftime(as.Date(as.character(semester[,2]), "%d/%m/%Y"), "%V")  # Week nrs from the csv
weeksMonth<-as.numeric(strftime(as.Date(as.character(semester[,2]), "%d/%m/%Y"), "%m"))   # Which month every week belongs to
monthID<-cumsum(rle(weeksMonth)$lengths)  # Which week every month starts at
#WeekStart<-as.Date(as.character(semester[,2]), "%d/%m/%Y")    # Which date every week starts at
#WeekEnd<-as.Date(WeekStart + days(7))
weeksAxis01<-seq(0,1, length.out = length(weeks))+0.0106383
Names<-c("Olof","Lina","Mahmoud","Ale","Maria","Anton","Tomas","Olle","Lisa")
groups <- data.frame(id = c("Week","Olof","Lina","Mahmoud","Ale","Maria","Anton","Tomas","Olle","Lisa"),
                     content = c("Vecka","Olof","Lina","Mahmoud","Ale","Maria","Anton","Tomas","Olle","Lisa"))

# weeks<-seq(5,52)
fikaweek<-matrix(cbind(weeks, rep(Names,length.out=length(weeks))), ncol=2)

urlWeather<-"https://api.openweathermap.org/data/2.5/weather?q=Uppsala,SE&units=metric&appid=97ff4d6525b004787e9152705e0db608"

#data <- data.frame(
#  id = 51:53,
#  start = c("2018-06-05", "2018-06-05", "2018-06-06"),
#  end = c("2018-06-15", "2018-06-15", "2018-06-15"),
#  content = c("Semester", "Fält", "Kontor"),
#  style = c(NA, NA, NA),
#  group = c("Olof", "Lina", "Mahmoud"),
#  stringsAsFactors = FALSE
#)

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
calendar <- read.csv("calendar_backup.csv", stringsAsFactors = FALSE)
startID <- tail(weekData$id, 1) + 1

calendarData <- data.frame(
  id = seq(startID, startID + length(calendar[,1]) -1, 1),
  start = as.character(calendar$start),
  end = as.character(calendar$end),
  content = calendar$content,
  style = rep(NA, length(calendar[,1])),
  group = calendar$group
)

# Data to load into calendar
combinedData = rbind(weekData, calendarData)

fikaweek<-matrix(cbind(weeks$weekNr, rep(Names,length.out=length(weeks[,1]))), ncol=2)
fikaWeek <- data.frame(
  week = weeks$weekNr,
  name = rep(Names,length.out=length(weeks[,1]))
)

# Random ID generator for adding events interactively
randomID <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
}
