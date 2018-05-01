library(ggplot2)
library(ggthemes)
library(lubridate)
library(readr)

source("NCAA/NCAAFunctions.R")
source("NCAA/Data/NCAAScheduleScraper.R")

compile.current.season <- function(){

  if (month(today()) < 7){
    season <- year(today())
  } else {
    season <- year(today()) + 1
  }
  
  # Create Directory
  directory <- paste0("NCAA/Season-To-Date/", today())
  if(!dir.exists(directory)){
    dir.create(directory)
  }

  season.schedule.raw <- read_csv(paste0("NCAA/Data/Schedules/",season-1,"-",season,"_Clean.csv"))
  
  # Clean it, and calculate record and game results
  season.schedule <- season.schedule.raw %>% 
    filter(!is.na(HomeScore))
  write_csv(season.schedule,paste0(directory,"/Schedule.csv")) # Save Schedule File to Current Folder
  print("Schedule Saved")
  
  season.results <- compile.results(season.schedule)
  write_csv(season.results,paste0(directory,"/Results.csv")) # Compile Individual Game Results to Current Folder
  print("Results Compiled and Saved")
  
  season.record <- compile.record(season.results) # Calculate Team Record
  expanded.record <- compile.rpi(season.record,season.results) # Expand Record to Include RPI Information
  write_csv(expanded.record,paste0(directory,"/Record.csv")) # Save Team Record
  print("Record Compiled and Saved")
  
}
