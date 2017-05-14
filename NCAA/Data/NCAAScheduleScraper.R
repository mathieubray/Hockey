library(plyr)
library(dplyr)
library(tidyr)
library(rvest)
library(lubridate)

source("NCAA/NCAAFunctions.R")

# Scrape raw schedule from USCHO.com

scrape.raw.schedule <- function(season){
  
  # Set URL
  url <- paste0("http://www.uscho.com/scoreboard/division-i-men/",season-1,"-",season,"/composite-schedule/")
  
  raw.data <- url %>% 
    read_html %>% 
    html_nodes('.comp') %>%
    html_table(header=F, fill=F) %>%
    data.frame(stringsAsFactors = F)
  
  # Add column names
  colnames(raw.data) <- c("Day","Blank","Date","Time","Away","AwayScore","at","Home","HomeScore","OT","Notes","GameType","Box","Recap","TV")
  
  # Add season identifier and remove redundant columns
  raw.schedule <- raw.data %>%
    mutate(Season = paste0(season-1,season)) %>%
    select(Season,Away,AwayScore,at,Home,HomeScore,OT,Notes,Date,GameType)
  
  # Assign season label
  season.label <- paste0(season-1,"-",season)
  
  # Save raw schedule
  write.csv(raw.schedule,paste0("NCAA/Data/Schedules/",season.label,"_Raw.csv"),row.names=F)
  
  print(paste0("Raw ",season.label," Schedule Scraped and Saved"))
  
}


# Apply standard cleaning procedures to raw schedule (requires NCAATeamStats.csv)

clean.schedule <- function(season){
  
  # Helper function for cleaning strings
  clean.strings <- function(strings){
    
    new.strings <- unlist(strsplit(strings,split=') ')) # Split strings with rankings in parantheses eg. (1) Michigan
    new.strings <- new.strings[substring(new.strings,1,1)!="("] # Remove rankings in parantheses
    
    new.strings[new.strings == "Army West Point"] <- "Army"
    new.strings[new.strings == "Omaha"] <- "Nebraska-Omaha"
    
    return(new.strings)
  }
  
  # Lists of tournaments
  conf.tournaments <- c("AHA Tournament","Big Ten Tournament","ECAC Tournament","HEA Tournament","NCHC Tournament","WCHA Tournament","CCHA Tournament","CHA Tournament")
  ncaa.tournaments <- c("NCAA East Regional","NCAA Midwest Regional","NCAA Northeast Regional","NCAA West Regional","NCAA Tournament")
  
  
  # Assign season label
  season.label <- paste0(season-1,"-",season)
  
  # Load raw schedule and filter team information for relevant season
  
  raw.schedule <- read.csv(paste0("NCAA/Data/Schedules/",season.label,"_Raw.csv"),header=T,stringsAsFactors = F)
  
  team.list <- read.csv("NCAA/Data/Statistics/NCAATeamStats.csv",header=T,stringsAsFactors = F) %>% 
    filter(Season == paste0(season-1,season))
  
  # List tournaments for this season
  notes <- levels(as.factor(raw.schedule$Notes))
  tournaments <- notes[!grepl(pattern="wins",x=notes) & notes!=""]
  
  # Clean strings and fix calendar date
  raw.schedule$Home <- clean.strings(raw.schedule$Home)
  raw.schedule$Away <- clean.strings(raw.schedule$Away)
  raw.schedule$Date <- mdy(raw.schedule$Date)
  
  clean.schedule <- raw.schedule %>% 
    filter(GameType != "EX", Home %in% team.list$Team, Away %in% team.list$Team) %>% # Ignore exhibition games, only consider games between D-1 teams each season
    rowwise() %>% 
    mutate(HomeWin = win(T,HomeScore,AwayScore)) %>% # Determine whether home team won, lost or tied
    mutate(AwayWin = win(F,HomeScore,AwayScore)) %>% # Determine whether away team won, lost or tied
    mutate(Tournament = ifelse(Notes %in% tournaments,"Yes","No"),
           ConfTournament = ifelse(Notes %in% conf.tournaments,"Yes","No"),
           NCAA=ifelse(Notes %in% ncaa.tournaments,"Yes","No")) %>%
    inner_join(select(team.list,Team,Conference),by=c("Home"="Team")) %>% # Include conference information
    rename(HomeConference=Conference) %>%
    inner_join(select(team.list,Team,Conference),by=c("Away"="Team")) %>%
    rename(AwayConference=Conference)
  
  
  # Save clean schedule
  write.csv(clean.schedule, paste0("NCAA/Data/Schedules/",season.label,"_Clean.csv"),row.names=F)
  
  print(paste0(season.label," Schedule Cleaned and Saved"))
  
}
