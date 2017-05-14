library(jsonlite)
library(dplyr)
library(stringr)

# Player Bios

url = "http://www.nhl.com/stats/rest/grouped/skaters/basic/season/bios?cayenneExp=seasonId>=20022003 and seasonId<=20162017 and gameTypeId=2"
raw.data <- readLines(url, warn = "F")

clean.data <- fromJSON(raw.data)[[1]]

names(clean.data) <- c("A","GP","G","CityOfBirth","CountryOfBirth","Birthdate","StateProvince","DraftNumber",
                       "DraftRound","DraftYear","FirstName","Height","ID","LastName","Name",
                       "Nationality","Position","ShootsCatches","Teams","Weight","P","Season")

final.data <- clean.data %>%
  select(Season,Name,LastName,FirstName,Teams,Position,ShootsCatches,Birthdate,CityOfBirth,CountryOfBirth,StateProvince,
         Nationality,Height,Weight,DraftYear,DraftRound,DraftNumber,GP,G,A,P) %>%
  arrange(LastName,FirstName,Season,Teams) %>%
  mutate(CityOfBirth = str_replace_all(CityOfBirth,"[^[:alnum:]]",""))

write.csv(final.data,"HeightWeight/NHLPlayerBios.csv",row.names=F)


# Goalie Bios

url = "http://www.nhl.com/stats/rest/grouped/goalies/basic/season/goaliebios?cayenneExp=seasonId>=20022003 and seasonId<=20152016 and gameTypeId=2 and playerPositionCode='G'"
raw.data <- readLines(url, warn = "F")

clean.data <- fromJSON(raw.data)[[1]]

names(clean.data) <- c("L","OTL","CityOfBirth","CountryOfBirth","Birthdate","StateProvince","DraftNumber",
                       "DraftRound","DraftYear","FirstName","Height","ID","LastName","Name",
                       "Nationality","Position","ShootsCatches","Teams","Weight","Season","Ties","W")

final.data <- clean.data %>%
  select(Season,Name,LastName,FirstName,Teams,Position,ShootsCatches,Birthdate,CityOfBirth,CountryOfBirth,StateProvince,
         Nationality,Height,Weight,DraftYear,DraftRound,DraftNumber,W,L,Ties,OTL) %>%
  arrange(LastName,FirstName,Season,Teams) %>%
  mutate(CityOfBirth = str_replace_all(CityOfBirth,"[^[:alnum:]]",""))


write.csv(final.data,"HeightWeight/NHLGoalieBios.csv",row.names=F)
