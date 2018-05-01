library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)

# Player Bios

scrape.players <- function(season){
  
  seasonID <- paste0(season-1,season)
  
  url = paste0("http://www.nhl.com/stats/rest/skaters?isAggregate=true&reportType=basic&isGame=false&reportName=bios&cayenneExp=gameTypeId=2 and seasonId>=",seasonID," and seasonId<=",seasonID)
 
  raw.data <- readLines(url, warn = "F")
  
  clean.data <- fromJSON(raw.data)[[1]]
  
  names(clean.data) <- c("A","GP","G","CityOfBirth","CountryOfBirth","Birthdate","StateProvince","DraftNumber",
                         "DraftRound","DraftYear","FirstName","Height","ID","HHOF","Active","LastName","Name",
                         "Nationality","Position","ShootsCatches","Weight","P")
  
  final.data <- clean.data %>%
    select(Name,Height,Weight,Position) %>%
    arrange(Name) %>%
    mutate(Season = season)

  print(paste0("Done ",seasonID))
  
  write.csv(final.data,paste0("HeightWeight/data/NHLPlayerHeightWeight_",seasonID,".csv"),row.names=F)
  
}



# Goalie Bios

scrape.goalies <- function(season){
  
  seasonID <- paste0(season-1,season)
  
  url = paste0("http://www.nhl.com/stats/rest/goalies?isAggregate=true&reportType=basic&isGame=false&reportName=bios&cayenneExp=gameTypeId=2 and seasonId>=",seasonID," and seasonId<=",seasonID)
  raw.data <- readLines(url, warn = "F")
  
  clean.data <- fromJSON(raw.data)[[1]]
  
  names(clean.data) <- c("A","GP","G","CityOfBirth","CountryOfBirth","Birthdate","StateProvince","DraftNumber",
                         "DraftRound","DraftYear","FirstName","Height","ID","HHOF","Active","LastName","Name",
                         "Nationality","Position","ShootsCatches","Weight","P")
  
  final.data <- clean.data %>%
    select(Name,Height,Weight) %>%
    arrange(Name) %>%
    mutate(Season = season)
  
  print(paste0("Done ",seasonID))
  
  write.csv(final.data,paste0("HeightWeight/data/NHLGoalieHeightWeight_",seasonID,".csv"),row.names=F)
  
}


# Collect Data

map(2010:2018,scrape.players)
map(2010:2018,scrape.goalies)
