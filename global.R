####################################
# ui.R
#
# Executive Producer: Leslie McIntosh
# Developer: Connie Zabarovskaya
# Project Manager: Mary Uhlmansiek
# Center for Biomedical Informatics
# Washington University in St. Louis
####################################

#this code is visible to both ui.R and server.R. It's needed for ui.R pre-populated dropdown menus
#read in Ministry of Magic data
ministry <- read.csv("data/ministry.csv", stringsAsFactors = FALSE)
#convert the date column to Date format
ministry$DateUsed <- as.Date(ministry$DateUsed, format = "%m/%d/%Y")

#function to convert to POSIXct date format, specifically for chart 12
to_jsdate2 <- function(x){
  as.numeric(as.POSIXct(as.Date(x), origin="1970-01-01")) * 1000
}