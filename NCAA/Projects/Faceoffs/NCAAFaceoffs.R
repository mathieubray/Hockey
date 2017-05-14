library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(ggimage)
library(lubridate)
library(broom)
library(jsonlite)

source("NCAA/NCAAFunctions.R")
source("Logos/Logos.R")

ncaa.logo.list <- load.logos(0.75,"NCAA")$Logos
nhl.logo.list <- load.logos(0.75,"NHL")$Logos

######################################################################################################

# Open stats file
ncaa.faceoffs <- read.csv("NCAA/Data/Statistics/NCAATeamStats.csv",header=T,stringsAsFactors=F) %>%
  select(Team,Season,FO.) %>%
  rename(FOPct = FO.) %>%
  filter(!is.na(FOPct))


# Convert season to string
seasons <- as.numeric(substr(as.character(unique(ncaa.faceoffs$Season)),5,8))

get.win.pct <- function(season){
  
  schedule <- read.csv(paste0("NCAA/Data/Schedules/",season-1,"-",season,"_Clean.csv"),header=T,stringsAsFactors=F)
  
  season.results <- compile.results(schedule)
  season.record <- compile.record(season.results)
  
  win.pct <- season.record %>% 
    select(Team,Pct) %>%
    rename(WinPct = Pct) %>%
    mutate(Season = as.numeric(paste0(season-1,season)))
  
  return(win.pct)
  
}

ncaa.win.pct <- rbind.fill(lapply(seasons,get.win.pct))

final.ncaa.faceoff.data <- full_join(ncaa.faceoffs,ncaa.win.pct,by=c("Team","Season"))

write.csv(final.ncaa.faceoff.data,"NCAA/Projects/Faceoffs/NCAAFaceoffs.csv",row.names=F)

########################################################################################################

# Collect NHL Data

url = "http://www.nhl.com/stats/rest/grouped/team/basic/season/teamsummary?cayenneExp=seasonId>=20052006 and seasonId<=20152016 and gameTypeId=2&factCayenneExp=gamesPlayed>=1"
raw.nhl.faceoff.data <- readLines(url, warn = "F")

clean.nhl.faceoff.data <- fromJSON(raw.nhl.faceoff.data)[[1]] %>%
  select(teamAbbrev,seasonId,faceoffWinPctg,pointPctg) %>%
  rename(Team = teamAbbrev,
         Season = seasonId,
         FOPct = faceoffWinPctg,
         WinPct = pointPctg) %>%
  arrange(Team,Season) %>%
  mutate(FOPct = 100 * FOPct)

write.csv(clean.nhl.faceoff.data, "NCAA/Projects/Faceoffs/NHLFaceoffs.csv",row.names=F)

########################################################################################################

# Load Faceoff Data

ncaa.faceoffs <- read.csv("NCAA/Projects/Faceoffs/NCAAFaceoffs.csv",header=T,stringsAsFactors=F)


# Plot Faceoff Data

ggplot(data=ncaa.faceoffs,aes(x=FOPct,y=WinPct)) + 
  geom_point(size=0.001) +
  geom_smooth(method="lm",se=F,size=0.5) +
  xlab("Faceoff Win Pct (%)") +
  ylab("Win Pct") +
  xlim(40,60) +
  ylim(0,1) +
  mapply(
    function(xx,yy,tt) {
      annotation_custom(ncaa.logo.list[[tt]],xmin=xx-0.25, xmax=xx+0.25, ymin=yy-0.25, ymax=yy+0.25)
    }, ncaa.faceoffs$FOPct, ncaa.faceoffs$WinPct, ncaa.faceoffs$Team
  ) +
  geom_text(size=6,aes(x=41,y=1,label=r.function(ncaa.faceoffs$FOPct,ncaa.faceoffs$WinPct))) +
  annotate("text",x=50,y=0.5,col="red",label="@mathieubray",alpha=0.1,cex=30,fontface="bold",angle=15) +
  ggtitle(paste0("Does Faceoff Winning Percentage Matter in the NCAA?")) +
  theme_bw(18)

ggsave(paste0("NCAA/Projects/Faceoffs/NCAAFaceoffs.png"),height=8,width=16,units="in")


# Fit Model to NCAA Faceoff Data

ncaa.model <- lm(data=ncaa.faceoffs,WinPct~FOPct)

ncaa.coefs <- tidy(ncaa.model)
ncaa.resids <- augment(ncaa.model)
ncaa.tests <- glance(ncaa.model)


# Compare to NHL Data

nhl.faceoffs <- read.csv("NCAA/Projects/Faceoffs/NHLFaceoffs.csv",header=T,stringsAsFactors=F) %>%
  filter(Team != "ATL")

ggplot(data=nhl.faceoffs,aes(x=FOPct,y=WinPct)) + 
  geom_point(size=0.001) +
  geom_smooth(method="lm",se=F,size=0.5) +
  xlab("Faceoff Win Pct (%)") +
  ylab("Win Pct.") +
  xlim(40,60) +
  ylim(0,1) +
  mapply(
    function(xx,yy,tt) {
      annotation_custom(nhl.logo.list[[tt]],xmin=xx-0.25, xmax=xx+0.25, ymin=yy-0.25, ymax=yy+0.25)
    }, nhl.faceoffs$FOPct, nhl.faceoffs$WinPct, nhl.faceoffs$Team
  ) +
  geom_text(size=6,aes(x=41,y=1,label=r.function(nhl.faceoffs$FOPct,nhl.faceoffs$WinPct))) +
  annotate("text",x=50,y=0.5,col="red",label="@mathieubray",alpha=0.1,cex=30,fontface="bold",angle=15) +
  ggtitle(paste0("Does Faceoff Winning Percentage Matter in the NHL?")) +
  theme_bw(18)

ggsave(paste0("NCAA/Projects/Faceoffs/NHLFaceoffs.png"),height=8,width=16,units="in")

nhl.model <- lm(WinPct~FOPct,data=nhl.faceoffs)

nhl.coefs <- tidy(nhl.model)
nhl.resids <- augment(nhl.model)
nhl.tests <- glance(nhl.model)

