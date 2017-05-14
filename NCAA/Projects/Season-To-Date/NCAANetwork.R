library(plyr)
library(dplyr)
library(tidyr)
library(visNetwork) 

source("NCAA/NCAAFunctions.R")

tag <- "2016-2017_PreNCAA"

directory <- paste0("NCAA/Projects/Season-To-Date/",tag)


logos <- read.csv("Logos/LogoInfo.csv",header=T,stringsAsFactors=F) %>%
  filter(League == "NCAA") %>%
  select(Team,FileName) %>%
  mutate(FileName = paste0("Logos/NCAA/",FileName,".png")) %>%
  rename(id = Team)
  

# Load USCHO values

season.schedule <- read.csv(paste0(directory,"/Schedule.csv"),header=T,stringsAsFactors=F)

nodes <- data.frame(team = unique(c(season.schedule$Home,season.schedule$Away)), stringsAsFactors=F) %>%
  arrange(team) %>%
  rename(id = team) %>%
  left_join(logos,by="id") %>%
  rename(image=FileName)

reduced.schedule <- season.schedule %>%
  select(Home,Away,HomeWin) %>%
  filter(HomeWin != "Tie")

reduced.schedule.1 <- reduced.schedule %>%
  filter(HomeWin == "Win") %>%
  select(-HomeWin) %>%
  rename(from = Home,to = Away) 

reduced.schedule.2 <- reduced.schedule %>%
  filter(HomeWin == "Loss") %>%
  select(-HomeWin) %>%
  rename(from = Home, to = Away) %>%
  select(from,to)

edges <- rbind(reduced.schedule.1,reduced.schedule.2) %>%
  group_by(from,to) %>%
  summarize(width = n())

visNetwork(nodes,edges) %>%
  visNodes(size=100,shape="image") %>%
  visEdges(smooth=F,arrows="to")
