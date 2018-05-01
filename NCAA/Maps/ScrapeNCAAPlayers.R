library(dplyr) # Really should be default
library(rvest) # Scrape scrape scrape
library(tm) # Work with strings
library(stringi) # Need 'stri_trans_general' function to standardize scraped strings
library(stringr)


# Scrape player table for each team
collect.player.info <- function(var.list, season) {
  
  # Some players have their NHL draft team in parantheses next to their name
  clean.names <- function(name) {
    
    clean.name <- iconv(name, to='ASCII//TRANSLIT') %>%
      str_split(" ") %>%
      map(str_subset,pattern="^(?!\\()") %>%
      map(str_replace,pattern="A$",replacement="") %>%
      map(str_replace,pattern=fixed("AC"),replacement="e") %>%
      map_chr(paste,collapse = " ")
    
    return(clean.name) # Paste name string back together
    
  }
  
  # Hometown string also has previous team affiliation; get rid of that
  clean.hometown <- function(hometown){
    
    clean.hometown <- iconv(hometown, to='ASCII//TRANSLIT') %>%
      str_split("/") %>%
      map(extract,1) %>%
      str_trim %>%
      str_replace(pattern=fixed("AC"),replacement="e")
    
    return(clean.hometown)
  }
  
  # For use with "geocode" function, simplify the hometown string
  simplify.hometown <- function(hometown){
    
    simple.hometown <- hometown %>%
      tolower %>%
      str_replace_all("[:punct:]","")
    
    return(simple.hometown)
    
  }
  
  # Puts together a label for each player to assign as tooltips for maps
  assign.label <- function(team,number,name,class,position,hometown){
    
    if (number < 10){ # Force number to have 2 digits
      number.label<-paste0("0",number)
    } else {
      number.label<-as.character(number)
    }
    
    hometown.label <- str_replace_all(hometown,pattern="'", replacement="") # Remove single-quotes "'"; causes problems with leaflet
    
    return(paste0(team,": ",number.label," ",name," - ",hometown," (",class,"-", position,")")) # Paste info together in nice format
  }
  
  team <- var.list$Team
  code <- var.list$Code
  season.url <- paste0(substr(as.character(season-1),3,4),substr(as.character(season),3,4))
  
  url <- paste0("http://collegehockeystats.net/",season.url,"/rosters/",code) # For year 2016-2017
  
  player.info.raw <- url %>% 
    read_html 
  
  player.info <- player.info.raw %>% 
    html_nodes('.rostable') %>%
    html_table(header=T, fill=T) %>%
    extract2(1) %>%
    data.frame(stringsAsFactors = F) %>%
    set_colnames(c("Number","Name","Class","Position","Height","Weight","Shoots","YOB","Hometown")) %>%
    select(Number,Name,Class,Position,Hometown) %>%
    mutate(Team = team,
           Number = as.numeric(str_replace(Number,"#","")),
           Name = clean.names(Name),
           Hometown = clean.hometown(Hometown),
           Simple.Hometown = simplify.hometown(Hometown)) %>%
    rowwise %>%
    mutate(Label = assign.label(Team,Number,Name,Class,Position,Hometown)) %>%
    ungroup
  
  Sys.sleep(3) # So that College Hockey Stats doesn't get mad
  
  message(paste0("Completed Scraping for ", team)) # Track progress
  
  return(player.info)
}



##################################################################################################3


# Table of URL codes for each team
team.urls <- read_csv("NCAA/Maps/CollegeHockeyStatsURLCode.csv")
season <- 2018

team.url.list <- split(team.urls,1:nrow(team.urls))

players.scraped <- map_df(team.url.list, collect.player.info, season = season)

players <- players.scraped %>% mutate(Season = paste0(season-1,season))

write_csv(players, paste0("NCAA/Maps/PlayersScraped_",season-1,"-",season,".csv"))



