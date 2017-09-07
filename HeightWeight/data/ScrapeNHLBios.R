library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)

# Player Bios

scrape.players <- function(season){
  
  seasonID <- paste0(season-1,season)
  
  url = paste0("http://www.nhl.com/stats/rest/grouped/skaters/basic/season/bios?cayenneExp=seasonId>=",seasonID," and seasonId<=",seasonID," and gameTypeId=2")
  raw.data <- readLines(url, warn = "F")
  
  clean.data <- fromJSON(raw.data)[[1]]
  
  names(clean.data) <- c("A","GP","G","CityOfBirth","CountryOfBirth","Birthdate","StateProvince","DraftNumber",
                         "DraftRound","DraftYear","FirstName","Height","ID","HHOF","LastName","Name",
                         "Nationality","Position","ShootsCatches","Teams","Weight","P","Season")
  
  final.data <- clean.data %>%
    select(Season,Name,LastName,FirstName,Teams,Position,ShootsCatches,Birthdate,CityOfBirth,CountryOfBirth,StateProvince,
           Nationality,Height,Weight,DraftYear,DraftRound,DraftNumber,HHOF,GP,G,A,P) %>%
    arrange(LastName,FirstName,Season,Teams) %>%
    mutate(CityOfBirth = str_replace_all(CityOfBirth,"[^[:alnum:]]",""))

  print(paste0("Done ",seasonID))
  
  write.csv(final.data,paste0("HeightWeight/data/NHLPlayerBios_",seasonID,".csv"),row.names=F)
  
}



# Goalie Bios

scrape.goalies <- function(season){
  
  seasonID <- paste0(season-1,season)
  
  url = paste0("http://www.nhl.com/stats/rest/grouped/goalies/basic/season/goaliebios?cayenneExp=seasonId>=",seasonID," and seasonId<=",seasonID," and gameTypeId=2 and playerPositionCode='G'")
  raw.data <- readLines(url, warn = "F")
  
  clean.data <- fromJSON(raw.data)[[1]]
  
  names(clean.data) <- c("L","OTL","CityOfBirth","CountryOfBirth","Birthdate","StateProvince","DraftNumber",
                         "DraftRound","DraftYear","FirstName","Height","ID","HHOF","LastName","Name",
                         "Nationality","Position","ShootsCatches","Teams","Weight","Season","Ties","W")
  
  final.data <- clean.data %>%
    select(Season,Name,LastName,FirstName,Teams,Position,ShootsCatches,Birthdate,CityOfBirth,CountryOfBirth,StateProvince,
           Nationality,Height,Weight,DraftYear,DraftRound,DraftNumber,HHOF,W,L,Ties,OTL) %>%
    arrange(LastName,FirstName,Season,Teams) %>%
    mutate(CityOfBirth = str_replace_all(CityOfBirth,"[^[:alnum:]]",""))
  
  print(paste0("Done ",seasonID))
  
  write.csv(final.data,paste0("HeightWeight/data/NHLGoalieBios_",seasonID,".csv"),row.names=F)
  
}


# Collect Data

map(2010:2017,scrape.players)
map(2010:2017,scrape.goalies)
