library(XML)
library(tm)
library(stringr)
library(dplyr)
library(lubridate)

# Load 2014-2015 data from NHLscrapr

load("../Data/NHLScrapr/nhlscrapr20142015.RData")

# Get player names and identifying codes 
roster <- nhlscrapr.20142015.roster %>% 
  select(firstlast,index) %>%
  rename(Player = firstlast,PlayerCode=index)

# Retrieve all goals scored against Montreal
goals <- nhlscrapr.20142015.pbp %>% 
  filter(etype=="GOAL", period <= 4) %>% # Consider only non-shootout goals
  select(ev.team,ev.player.1,awayteam,hometeam,refdate) %>%
  rename(Team=ev.team,PlayerCode=ev.player.1,Away=awayteam,Home=hometeam,Date=refdate) %>%
  inner_join(roster,by="PlayerCode") %>% # Merge in player names
  mutate(Date = ymd("2002-01-01") + days(Date)) %>% # Get correct date
  select(-PlayerCode) %>%
  rowwise() %>%
  mutate(Opponent = ifelse(Team==Away,Home,Away), # Get opponent of team that scored
         InMTL = Home=="MTL") %>% # Did the game occur in Montreal? T/F
  select(-Home,-Away) %>%
  filter(Opponent == "MTL") %>% # Consider only goals against Montreal
  ungroup() %>%
  arrange(Date)

head(goals)

write.csv(goals,"QuebecPlayers/NHL_Goals_2014-2015.csv",row.names=F)

# Scrape Quebec Players

url <- c("http://www.hockey-reference.com/friv/birthplaces.cgi?country=CA&province=QC&state=")

# Only 1 Table in list, column 12 repeats GP for goaltenders, remove it as the column name is the same as the previous and causes issues
quebec.players <- readHTMLTable(url,stringsAsFactors=F)[[1]][,-12] %>% 
  filter(Player != "Player") # Remove rows not corresponding to any player

names(quebec.players) <- c("Rk","Player","Start.Year","End.Date",
                           "Position","GP","G","A","PTS","Plus-Minus",
                           "PIM","W","L","TO","SV","GAA","Birthplace","Birth.Date","Death.Date")

write.csv(quebec.players,"QuebecPlayers/QuebecPlayers.csv",row.names=F)


# Collect Relevant Quebec Players

quebec.players <- read.csv("QuebecPlayers/QuebecPlayers.csv",header=T,stringsAsFactors=F)

relevant.quebec.players <- quebec.players %>% 
  mutate(End.Date = as.numeric(End.Date)) %>%
  filter(End.Date >= 2015,
         Position != "G") %>% # Consider only shooters who played up to to 2014-2015 season
  mutate(Player = toupper(Player)) %>%
  select(Player,Birthplace)

head(relevant.quebec.players)


# Merge With Master PBP and See Who Scored Against MTL

quebec.against.mtl <- inner_join(goals,relevant.quebec.players,by="Player") %>%
  select(Team,Date,Player,InMTL,Birthplace)

quebec.against.mtl

