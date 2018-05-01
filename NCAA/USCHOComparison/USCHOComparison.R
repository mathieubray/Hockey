library(dplyr)
library(tidyr)
library(readr)
library(rvest)
library(lubridate)

source("NCAA/NCAAFunctions.R")

clean.schedule <- read_csv("NCAA/Data/Schedules/2017-2018_Clean.csv") %>%
  filter(NCAA == "No") # Ignore NCAA Tournament Results

results <- compile.results(clean.schedule)

record <- compile.record(results) %>%
  arrange(Team) %>%
  mutate(PctRound=round(Pct,4),
         AdjPctRound=round(AdjPct,4)) %>%
  select(Team,Wins,Losses,Ties,Pct,PctRound,AdjPct,AdjPctRound)


# Compare to the adjusted win percentage from USCHO

uscho <- read_csv("NCAA/SOS/USCHOValues_20172018.csv")

sum(uscho$Pct == record$PctRound)  # 60, if all 60 teams come out with the same value. So far so good!


# Get the OWP for each team, and paste to our record table

opp.win.pct <- map_dbl(record$Team,OWP,results=results)
record$OppWinPct <- opp.win.pct


# Isolate the OWP values for each team
owps <- record %>% 
  select(Team, OppWinPct)

  
# Get the OOWP for each team, and paste to our record table

opp.opp.win.pct <- map_dbl(record$Team,OOWP,results=results,oppwinpcts=owps)
record$OppOppWinPct <- opp.opp.win.pct
 
# Put together RPI

rpi.table  <-  record %>%
  mutate(SOS = (21*OppWinPct+54*OppOppWinPct)/75,
         RPI = 0.25*AdjPct+0.21*OppWinPct+0.54*OppOppWinPct) %>%
  select(Team,Pct,SOS,RPI) %>%
  arrange(Team) 


r.function(rpi.table$Pct,uscho$Pct) # = 1, Perfect Agreement for Adjusted Win Pct
r.function(rpi.table$SOS,uscho$SOS) # = 9986, Near-Perfect Agreement for Adjusted Win Pct, but not equal
r.function(rpi.table$RPI,uscho$RPI) # = 9998, Near-Perfect Agreement for Adjusted Win Pct, but not equal

