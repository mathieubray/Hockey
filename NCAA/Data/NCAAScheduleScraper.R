library(dplyr)
library(tidyr)
library(rvest)
library(lubridate)
library(magrittr)

# Scrape raw schedule from USCHO.com

scrape.raw.schedule <- function(season){
  
  command <- paste0("NCAA/Data/Scrapers/phantomjs NCAA/Data/Scrapers/scrape_schedule.js")
  
  system(command)
  
  raw.data <- read_html("NCAA/Data/Scrapers/schedule.html")
  
  cells <- raw.data %>%
    html_nodes("td") %>%
    html_text %>%
    tail(-26) %>%
    head(-118)
  
  rows <- raw.data %>%
    html_nodes("tr") %>%
    html_text %>% 
    tail(-2) %>%
    head(-59) 
  
  extract.data <- function(row.number, cells){
    
    seq <- ((row.number-1)*13 + 1):((row.number-1)*13 + 13)
    
    row <- cells[seq]
    
    schedule.data <- row %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      set_colnames(c("Day","Date","Time","Away","AwayScore","at","Home","HomeScore","OT","Notes","GameType","Recap","TV"))
    
    return(schedule.data)
    
  }
  
  raw.data <- map_df(1:length(rows), extract.data, cells=cells)
    
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
  
  # Helper function for determining winner
  win <- function(home,homescore,awayscore){
    
    if (is.na(homescore) | is.na(awayscore)){
      return(as.character(NA))
    }
    
    result  <-  "Win"
    
    if(home){
      if (homescore > awayscore){
        result  <-  "Win"
      } else if (homescore < awayscore){
        result  <-  "Loss" 
      } else {
        result  <-  "Tie"
      }
    } else {
      if (awayscore > homescore){
        result  <-  "Win"
      } else if (awayscore < homescore){
        result  <-  "Loss"
      } else {
        result  <-  "Tie"
      }
    }
    
    return(result)
  }
  
  # Helper function for cleaning strings
  clean.strings <- function(strings){
    
    new.strings <- unlist(strsplit(strings,split=') ')) # Split strings with rankings in parantheses eg. (1) Michigan
    new.strings <- new.strings[substring(new.strings,1,1)!="("] # Remove rankings in parantheses
    
    new.strings[new.strings == "Army West Point"] <- "Army"
    new.strings[new.strings == "Omaha"] <- "Nebraska-Omaha"
    new.strings[new.strings == "UMass Lowell"] <- "Massachusetts-Lowell"
    new.strings[new.strings == "Alabama Huntsville"] <- "Alabama-Huntsville"
    new.strings[new.strings == "Alaska Anchorage"] <- "Alaska-Anchorage"
    new.strings[new.strings == "Minnesota Duluth"] <- "Minnesota-Duluth"
    
    
    
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
