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

weeks<-strftime(as.Date(as.character(semester[,2]), "%d/%m/%Y"), "%V")  # Week nrs from the csv
weeksMonth<-as.numeric(strftime(as.Date(as.character(semester[,2]), "%d/%m/%Y"), "%m"))   # Which month every week belongs to
monthID<-cumsum(rle(weeksMonth)$lengths)  # Which week every month starts at
WeekStart<-as.Date(as.character(semester[,2]), "%d/%m/%Y")    # Which date every week starts at
weeksAxis01<-seq(0,1, length.out = length(weeks))+0.0106383
Names<-c("Olof","Lina","Mahmoud","Ale","Maria","Anton","Tomas","Olle","Lisa")

# weeks<-seq(5,52)
fikaweek<-matrix(cbind(weeks, rep(Names,length.out=length(weeks))), ncol=2)

urlWeather<-"https://api.openweathermap.org/data/2.5/weather?q=Uppsala,SE&units=metric&appid=97ff4d6525b004787e9152705e0db608"
