library(dplyr)
library(tidyr)
library(rvest)
library(lubridate)

clean.schedule <- read.csv("NCAA/Data/Schedules/2016-2017_Clean.csv",header=T,stringsAsFactors=F) %>% head(-3)


# Split each game into two lines for each team, listing their outcome and whether they were playing at home, away, or neutral

get.team.results <- function(schedule){
  
  if (schedule$at=="@"){ # The 'at' column has either an '@' symbol for a regular game or a 'vs.' for a neutral site game
    status1 <- "Home"
    status2 <- "Away"
  } else {
    status1 <- "Neutral"
    status2 <- "Neutral"
  }
  
  home.result <- data.frame(Team=schedule$Home,Win=schedule$HomeWin,Status=status1,Opponent=schedule$Away,stringsAsFactors=F)
  away.result <- data.frame(Team=schedule$Away,Win=schedule$AwayWin,Status=status2,Opponent=schedule$Home,stringsAsFactors=F)
  
  results <- rbind(home.result,away.result)
  
  return(results)
  
}

results <- clean.schedule %>% 
  rowwise() %>%
  do(get.team.results(.)) %>%
  ungroup()

head(results)


# Calculate each team's record, including their adjusted winning percentages...

record <- results %>% 
  group_by(Team) %>%                            # For each team...
  summarize(Wins=sum(Win=="Win"),               # ... count the number of wins, losses, ties, etc. 
            Losses=sum(Win=="Loss"),
            Ties=sum(Win=="Tie"),
            HomeWins=sum(Win=="Win" & Status=="Home"),
            HomeLosses=sum(Win=="Loss" & Status=="Home"),
            HomeTies=sum(Win=="Tie" & Status=="Home"),
            AwayWins=sum(Win=="Win" & Status=="Away"),
            AwayLosses=sum(Win=="Loss" & Status=="Away"),
            AwayTies=sum(Win=="Tie" & Status=="Away"),
            NeutralWins=sum(Win=="Win" & Status=="Neutral"),
            NeutralLosses=sum(Win=="Loss" & Status=="Neutral"),
            NeutralTies=sum(Win=="Tie" & Status=="Neutral")) %>%
  mutate(Pct=(Wins+0.5*Ties)/(Wins+Losses+Ties), # Simple Win Pct. Adjusted Win Pct is below, using weights for each type of game
         AdjPct=(1.2*AwayWins + 1*NeutralWins + 0.8*HomeWins + 0.6*AwayTies + 0.5*NeutralTies +0.4*HomeTies)/((1.2*AwayWins + 0.8*HomeWins + NeutralWins + 0.8*AwayLosses + NeutralLosses + 1.2*HomeLosses + Ties))) %>%
  arrange(Team) %>%
  mutate(AdjPctRound=round(AdjPct,4)) %>%
  select(Team,Wins,Losses,Ties,Pct,AdjPct,AdjPctRound)


# Compare to the adjusted win percentage from USCHO

uscho <- read.csv("NCAA/Projects/SOS/USCHOValues_20162017.csv",header=T,stringsAsFactors=F)

head(uscho)

sum(uscho$AdjPct == record$AdjPctRound)  # = 60 if all 60 teams come out with the same value. So far so good!


# Function that determines the weighted value of a game

weight <- function(win,status){
  
  if(win=="Win"){
    if (status=="Home"){
      val <- 0.8
    } else if (status=="Away"){
      val <- 1.2
    } else {
      val <- 1
    }
  } else if(win=="Loss") {
    if (status=="Home"){
      val <- 1.2
    } else if (status=="Away"){
      val <- 0.8
    } else {
      val <- 1
    }
  } else {
    val <- 1
  }
}  


# Function to calculate the Opponent's Winning Percentage (OWP) for a team

OWP <- function(team,results){
  
  # Gather all opponents of our team in question, 'team'
  team_opponents <- unique((results %>% filter(Team==team))$Opponent) 
  
  # Get opponent's record and adjusted winning percentage in games not involving 'team'
  opponents_record <- results %>% 
    filter(Team %in% team_opponents & Opponent != team) %>% # Consider all games played by 'team's opponents, except those involving 'team'
    group_by(Team) %>%
    summarize(Wins=sum(Win=="Win"),      # Again, count number of wins, losses, ties, etc. by each team
              Losses=sum(Win=="Loss"),
              Ties=sum(Win=="Tie"),
              HomeWins=sum(Win=="Win" & Status=="Home"),
              HomeLosses=sum(Win=="Loss" & Status=="Home"),
              HomeTies=sum(Win=="Tie" & Status=="Home"),
              AwayWins=sum(Win=="Win" & Status=="Away"),
              AwayLosses=sum(Win=="Loss" & Status=="Away"),
              AwayTies=sum(Win=="Tie" & Status=="Away"),
              NeutralWins=sum(Win=="Win" & Status=="Neutral"),
              NeutralLosses=sum(Win=="Loss" & Status=="Neutral"),
              NeutralTies=sum(Win=="Tie" & Status=="Neutral")) %>%
    mutate(AdjPct=(AwayWins+NeutralWins+HomeWins+0.5*AwayTies+0.5*NeutralTies+0.5*HomeTies)/((AwayWins+HomeWins+NeutralWins+AwayLosses+NeutralLosses+HomeLosses+Ties))) %>%
    select(Team,AdjPct) # Calculate regular adjusted winning percentage
  
  # Now, assign the correct weight to each game, and take the weighted average of the opponents adjusted winning percentages over each game
  opponent_winpct <- results %>% 
    filter(Team == team) %>% # Only consider games involving 'team'
    inner_join(opponents_record,by=c("Opponent"="Team")) %>% # Merge in the opponent winning percentages
    rowwise() %>%
    mutate(Weight = weight(Win,Status)) # Assign weight to each game
  
  opponent_winpct <- weighted.mean(opponent_winpct$AdjPct,opponent_winpct$Weight) # Take the weighted average as the OWP
  
  return(opponent_winpct)
}

# Function to calculate the Opponent's Opponents Winning Percentage (OOWP) for a team
OOWP <- function(team,results,oppwinpcts){
  
  # Gather all opponents of our team in question, 'team'
  team_opponents <- unique((results %>% filter(Team==team))$Opponent)
  
  # For each game, assigne the correct weight and take the weighted average of the OWP as the OOWP 
  opp_opp_winpct <- results %>% 
    filter(Team == team) %>% # Only consider games involving 'team'
    inner_join(oppwinpcts,by=c("Opponent"="Team")) %>% # Merge in the OWP values for each opponent
    rowwise() %>%
    mutate(Weight = weight(Win,Status)) # Assign weight to each game
  
  opp_opp_winpct <- weighted.mean(opp_opp_winpct$OppWinPct,opp_opp_winpct$Weight) # Take the weighted average as the OOWP
  
  return(opp_opp_winpct)
}


# Get the OWP for each team, and paste to our record table
opp.win.pct <- sapply(record$Team,OWP,results=results)
record$OppWinPct <- opp.win.pct

head(record)

# Isolate the OWP values for each team
owps <- record %>% select(Team, OppWinPct)
  
# Get the OOWP for each team, and paste to our record table
opp.opp.win.pct <- sapply(record$Team,OOWP,results=results,oppwinpcts=owps)
record$OppOppWinPct <- opp.opp.win.pct

head(record)  


rpi.table  <-  record %>%
  mutate(SOS = (21*OppWinPct+54*OppOppWinPct)/75,
         RPI = 0.25*AdjPct+0.21*OppWinPct+0.54*OppOppWinPct) %>%
  arrange(desc(RPI))

head(rpi.table)


rpi.table.reduced <- rpi.table %>%
  select(Team,AdjPct,SOS,RPI) %>%
  arrange(Team) 

r.function <- function(x,y){
  return(paste0("R^2: ",round(summary(lm(y ~ x))$r.squared,4)))
}

r.function(rpi.table.reduced$AdjPct,uscho$AdjPct) # = 1, Perfect Agreement for Adjusted Win Pct
r.function(rpi.table.reduced$SOS,uscho$SOS) # = 9986, Near-Perfect Agreement for Adjusted Win Pct, but not equal
r.function(rpi.table.reduced$RPI,uscho$RPI) # = 9998, Near-Perfect Agreement for Adjusted Win Pct, but not equal


write.csv(rpi.table,"CalculatedValues.csv",row.names=F)


