library(plyr)
library(dplyr)
library(tidyr)
library(rvest)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(doSNOW)
library(itertools)

r.function <- function(x,y){
  return(paste0("R^2: ",round(summary(lm(y ~ x))$r.squared,4)))
}

# Team-level results from individual games
compile.results <- function(schedule){
  
  rows <- 2*nrow(schedule)

  results <- data.frame(Game=numeric(rows),
                        Season=character(rows),
                        Team=character(rows),
                        Goals=numeric(rows),
                        Conference=character(rows),
                        Status=character(rows),
                        Win=character(rows),
                        Opponent=character(rows),
                        OpponentGoals=numeric(rows),
                        OpponentConference=character(rows),
                        stringsAsFactors=F)

  # There's probably a better way of doing this...
  for (i in 1:nrow(schedule)){
    if (schedule$at[i]=="@"){
      status1 <- "Home"
      status2 <- "Away"
    } else {
      status1 <- "Neutral"
      status2 <- "Neutral"
    }
    
    results[2*i-1,] <- list(Game=i,
                            Season=schedule$Season[i],
                            Team=schedule$Home[i],
                            Goals=schedule$HomeScore[i],
                            Conference=schedule$HomeConference[i],
                            Status=status1,
                            Win=schedule$HomeWin[i],
                            Opponent=schedule$Away[i],
                            OpponentGoals=schedule$AwayScore[i],
                            OpponentConference=schedule$AwayConference[i])
    results[2*i,] <- list(Game=i,
                          Season=schedule$Season[i],
                          Team=schedule$Away[i],
                          Goals=schedule$AwayScore[i],
                          Conference=schedule$AwayConference[i],
                          Status=status2,
                          Win=schedule$AwayWin[i],
                          Opponent=schedule$Home[i],
                          OpponentGoals=schedule$HomeScore[i],
                          OpponentConference=schedule$HomeConference[i])
  }
  
  return(results)
  
}

# Calculates team records based on team-level game data
compile.record <- function(results){
  
  record <- results %>% 
    group_by(Team) %>%
    summarize(Wins=sum(Win=="Win"),
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
    mutate(Pct=(Wins+0.5*Ties)/(Wins+Losses+Ties),
           AdjPct=(1.2*AwayWins+NeutralWins+0.8*HomeWins+0.6*AwayTies+0.5*NeutralTies+0.4*HomeTies)/((1.2*AwayWins+0.8*HomeWins+NeutralWins+0.8*AwayLosses+NeutralLosses+1.2*HomeLosses+Ties))) %>%
    arrange(desc(AdjPct))
  
  return(record)
}


# Calculates RPI based on team records
compile.rpi <- function(temp.record,temp.results){
  
  rpi.table <- temp.record
  
  opp.win.pct <- sapply(temp.record$Team,OWP,results=temp.results)
  rpi.table$OppWinPct <- opp.win.pct
  
  owps <- rpi.table %>% select(Team, OppWinPct)
  
  opp.opp.win.pct <- sapply(rpi.table$Team,OOWP,results=temp.results,oppwinpcts=owps)
  rpi.table$OppOppWinPct <- opp.opp.win.pct
  
  rpi.table.final  <-  rpi.table %>%
    mutate(SOS = (21*OppWinPct+54*OppOppWinPct)/75,
                  RPI = 0.25*AdjPct+0.21*OppWinPct+0.54*OppOppWinPct) %>%
    arrange(desc(RPI))
  
  return(rpi.table.final)
}


##### STATISTIC CALCULATIONS #####

# Determines whether a team has won, lost, or tied
win <- function(home,homescore,awayscore){
  
  if (is.na(homescore) | is.na(awayscore)){
    return(as.character(NA))
  }
  
  result  <-  "Win"
  
  if(home){
    if (homescore > awayscore){
      result  <-  "Win"
    } else if (homescore < awayscore){
      result  <-  "Loss" 
    } else {
      result  <-  "Tie"
    }
  } else {
    if (awayscore > homescore){
      result  <-  "Win"
    } else if (awayscore < homescore){
      result  <-  "Loss"
    } else {
      result  <-  "Tie"
    }
  }
  
  return(result)
}


# Determines the weighted value of a game
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


# Calculated the Opponent's Winning Percentage (OWP) of a team
OWP <- function(team,results){
  team_opponents <- (results %>% filter(Team==team))$Opponent
  
  opponents_record <- results %>% 
    filter(Team %in% unique(team_opponents) & Opponent != team) %>%
    group_by(Team) %>%
    summarize(Wins=sum(Win=="Win"),
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
    select(Team,AdjPct)
  
  opponent_winpct <- results %>% filter(Team == team & Opponent %in% unique(team_opponents)) %>%
    inner_join(opponents_record,by=c("Opponent"="Team")) %>%
    rowwise() %>%
    mutate(Weight = weight(Win,Status))
  
  opponent_winpct <- weighted.mean(opponent_winpct$AdjPct,opponent_winpct$Weight)
  
  return(opponent_winpct)
}

# Calculate the Opponent's Opponents Winning Percentage (OOWP) of a team
OOWP <- function(team,results,oppwinpcts){
  
  team_opponents <- (results %>% filter(Team==team))$Opponent
  
  opp_opp_winpct <- results %>% filter(Team == team & Opponent %in% unique(team_opponents)) %>%
    inner_join(oppwinpcts,by=c("Opponent"="Team"))%>%
    rowwise() %>%
    mutate(Weight = weight(Win,Status))
  
  opp_opp_winpct <- weighted.mean(opp_opp_winpct$OppWinPct,opp_opp_winpct$Weight)
  
  return(opp_opp_winpct)
}


#### PAIRWISE CALCULATOR #####

# Pairwise calculator
PW <- function(team1,team2,record,results){
  
  team1Pts <- 0
  team2Pts <- 0
  
  #H2H
  r <- results %>% filter(Team==team1 & Opponent==team2)
  if (nrow(r)>0){
    for (j in 1:nrow(r)){
      if (r$Win[j]=="Win"){
        team1Pts <- team1Pts+1
      } else if (r$Win[j]=="Loss"){
        team2Pts <- team2Pts+1
      } 
    }
  }
  
  #Common Opponents
  team1opponents <- unique((results %>% filter(Team==team1))$Opponent)
  team2opponents <- unique((results %>% filter(Team==team2))$Opponent)
  commonopponents <- intersect(team1opponents,team2opponents)
  
  q <- results %>% filter(Team %in% c(team1,team2) & Opponent %in% commonopponents) %>%
    group_by(Team,Opponent) %>%
    summarize(Wins=sum(Win=="Win"),
              Losses=sum(Win=="Loss"),
              Ties=sum(Win=="Tie")) %>%
    mutate(Pct=(Wins+0.5*Ties)/(Wins + Losses + Ties)) %>%
    group_by(Team) %>%
    summarize(AvgPct = mean(Pct))
  
  if (nrow(q) > 0){
    team1cop <- q$AvgPct[which(q$Team == team1)] 
    team2cop <- q$AvgPct[which(q$Team == team2)]
    
    if (team1cop > team2cop){
      team1Pts <- team1Pts+1
    } else if (team1cop < team2cop){
      team2Pts <- team2Pts+1
    }
  }
  
  #RPI
  
  team1RPI <- record$RPI[which(record$Team == team1)]
  team2RPI <- record$RPI[which(record$Team == team2)]
  
  if(team1RPI > team2RPI){
    team1Pts <- team1Pts+1
  } else if (team1RPI < team2RPI){
    team2Pts <- team2Pts+1
  }
  
  # Total
  
  if (team1Pts == team2Pts){
    if(team1RPI > team2RPI){
      return(1)
      #winner <- team1
    } else {
      return(0)
      #winner <- team2
    }
  } else {
    if (team1Pts > team2Pts){
      return(1)
      #winner <- team1
    } else {
      return(0)
      #winner <- team2
    }
  }
  
  return(winner)
}





