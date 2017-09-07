library(tm)
library(stringr)
library(dplyr)
library(lubridate)

# Using Corsica data

load("../Data/Corsica/pbp20142015.RDa")


# Retrieve all goals scored against Montreal

goals.corsica <- pbp %>% 
  filter(Event=="GOAL",Period %in% c("1","2","3","4","OT")) %>%
  select(ev.team,p1,Away.Team,Home.Team,Date) %>%
  rename(Team=ev.team,Player=p1,Away=Away.Team,Home=Home.Team) %>%
  mutate(Date=ymd(Date),
         Opponent = if_else(Team==Away,Home,Away), # Get opponent of team that scored
         InMTL = Home=="MTL") %>% # Did the game occur in Montreal? T/F
  select(-Home,-Away) %>%
  filter(Opponent == "MTL") %>%
  arrange(Date)

head(goals)


# Collect Relevant Quebec Players

quebec.players <- read.csv("QuebecPlayers/QuebecPlayers.csv",header=T,stringsAsFactors=F)

relevant.quebec.players <- quebec.players %>% 
  mutate(End.Date = as.numeric(End.Date)) %>%
  filter(End.Date >= 2015,
         Position != "G") %>% # Consider only shooters who played up to to 2014-2015 season
  mutate(Player = gsub(toupper(Player),pattern=" ",replacement=".")) %>%
  mutate(Player = gsub(Player,pattern="..",replacement=". ",fixed=TRUE)) # Marty St-Louis...
select(Player,Birthplace)

head(relevant.quebec.players)


# Merge With Master PBP and See Who Scored Against MTL

quebec.against.mtl <- inner_join(goals.corsica,relevant.quebec.players,by="Player") %>%
  select(Team,Date,Player,InMTL,Birthplace)

quebec.against.mtl