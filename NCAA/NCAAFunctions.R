library(dplyr)
library(tidyr)
library(rvest)
library(lubridate)


### SCHEDULE COMPILERS

# Team-level results from individual games
#compile.results <- function(schedule){
  
#  rows <- 2*nrow(schedule)

#  results <- data.frame(Game=numeric(rows),
#                        Season=character(rows),
#                        Team=character(rows),
 #                       Goals=numeric(rows),
  #                      Conference=character(rows),
   #                     Status=character(rows),
    #                    Win=character(rows),
     #                   Opponent=character(rows),
      #                  OpponentGoals=numeric(rows),
       #                 OpponentConference=character(rows),
        #                stringsAsFactors=F)

  # There's probably a better way of doing this...
  #for (i in 1:nrow(schedule)){
   # if (schedule$at[i]=="@"){
    #  status1 <- "Home"
     # status2 <- "Away"
    #} else {
     # status1 <- "Neutral"
    #  status2 <- "Neutral"
    #}
    
    #results[2*i-1,] <- list(Game=i,
     #                       Season=schedule$Season[i],
      #                      Team=schedule$Home[i],
       #                     Goals=schedule$HomeScore[i],
        #                    Conference=schedule$HomeConference[i],
         #                   Status=status1,
          #                  Win=schedule$HomeWin[i],
           #                 Opponent=schedule$Away[i],
            #                OpponentGoals=schedule$AwayScore[i],
             #               OpponentConference=schedule$AwayConference[i])
    #results[2*i,] <- list(Game=i,
     #                     Season=schedule$Season[i],
      #                    Team=schedule$Away[i],
       #                   Goals=schedule$AwayScore[i],
        #                  Conference=schedule$AwayConference[i],
         #                 Status=status2,
          #                Win=schedule$AwayWin[i],
           #               Opponent=schedule$Home[i],
            #              OpponentGoals=schedule$HomeScore[i],
             #             OpponentConference=schedule$HomeConference[i])
  #}
  
 # return(results)
  
#}

# Function to calculate R-squared

r.function <- function(x,y,round=4){
  return(paste0("R^2: ",round(summary(lm(y ~ x))$r.squared,round)))
}


# Split each game into two lines for each team, listing their outcome and whether they were playing at home, away, or neutral

compile.results <- function(clean.schedule){
  
  get.team.results <- function(schedule){
    
    if (schedule$at=="@"){ # The 'at' column has either an '@' symbol for a regular game or a 'vs.' for a neutral site game
      status1 <- "Home"
      status2 <- "Away"
    } else {
      status1 <- "Neutral"
      status2 <- "Neutral"
    }
    
    home.result <- tibble(Season=schedule$Season,
                          Team=schedule$Home,
                          Win=schedule$HomeWin,
                          Goals=schedule$HomeScore,
                          Status=status1,
                          Opponent=schedule$Away,
                          OpponentGoals=schedule$AwayScore)
    away.result <- tibble(Season=schedule$Season,
                          Team=schedule$Away,
                          Win=schedule$AwayWin,
                          Goals=schedule$AwayScore,
                          Status=status2,
                          Opponent=schedule$Home,
                          OpponentGoals=schedule$HomeScore)
    
    results <- bind_rows(home.result,away.result)
    
    return(results)
    
  }
  
  results <- clean.schedule %>%
    split(1:nrow(.)) %>%
    map_df(get.team.results, .id="Game")
  
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


### WIN FUNCTIONS

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


### RPI CALCULATIONS

# Calculated the Opponent's Winning Percentage (OWP) of a team
OWP <- function(team,results){
  
  team_opponents <- results %>% 
    filter(Team==team) %>%
    .$Opponent %>%
    unique
  
  opponents_record <- results %>% 
    filter(Team %in% team_opponents & Opponent != team) %>%
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
  
  opponent_winpct <- results %>% 
    filter(Team == team & Opponent %in% team_opponents) %>%
    inner_join(opponents_record,by=c("Opponent"="Team")) %>%
    rowwise() %>%
    mutate(Weight = weight(Win,Status))
  
  opponent_winpct <- weighted.mean(opponent_winpct$AdjPct,opponent_winpct$Weight)
  
  return(opponent_winpct)
}



# Calculate the Opponent's Opponents Winning Percentage (OOWP) of a team
OOWP <- function(team,results,oppwinpcts){
  
  team_opponents <- results %>% 
    filter(Team==team) %>% 
    .$Opponent %>%
    unique
  
  opp_opp_winpct <- results %>% 
    filter(Team == team & Opponent %in% team_opponents) %>%
    inner_join(oppwinpcts,by=c("Opponent"="Team"))%>%
    rowwise() %>%
    mutate(Weight = weight(Win,Status))
  
  opp_opp_winpct <- weighted.mean(opp_opp_winpct$OppWinPct,opp_opp_winpct$Weight)
  
  return(opp_opp_winpct)
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


#### PAIRWISE CALCULATOR #####

pairwise.comparison <- function(team, compare.team, record, results){
  
  team.points <- 0
  compare.team.points <- 0
  
  # Head to Head
  head.to.head <- results %>% 
    filter(Team==team, Opponent==compare.team)
  
  head.to.head.games <- nrow(head.to.head)
  head.to.head.wins <- head.to.head %>% 
    filter(Win == "Win") %>%
    nrow
  head.to.head.losses <- head.to.head %>% 
    filter(Win == "Loss") %>%
    nrow
  
  team.points <- team.points + head.to.head.wins
  compare.team.points <- compare.team.points + head.to.head.losses
  
  # Common Opponents
  team.opponents <- results %>%
    filter(Team == team) %>%
    .$Opponent %>%
    unique
  
  compare.team.opponents <- results %>%
    filter(Team == compare.team) %>%
    .$Opponent %>%
    unique
  
  common.opponents <- intersect(team.opponents, compare.team.opponents)
  
  common.opponent.results <- results %>% 
    filter(Team %in% c(team, compare.team), Opponent %in% common.opponents) %>%
    group_by(Team,Opponent) %>%
    summarize(Wins=sum(Win=="Win"),
              Losses=sum(Win=="Loss"),
              Ties=sum(Win=="Tie")) %>%
    mutate(Pct=(Wins+0.5*Ties)/(Wins + Losses + Ties)) %>%
    group_by(Team) %>%
    summarize(AvgPct = mean(Pct))
  
  if (nrow(common.opponent.results) > 0){
    
    team.record <- common.opponent.results$AvgPct[which(common.opponent.results$Team == team)] 
    
    team.record <- common.opponent.results %>%
      filter(Team == team) %>%
      .$AvgPct
    
    compare.team.record <- common.opponent.results %>%
      filter(Team == compare.team) %>%
      .$AvgPct
    
    if (team.record > compare.team.record){
      team.points <- team.points + 1
    } else if (team.record < compare.team.record){
      compare.team.points <- compare.team.points + 1
    }
  }
  
  #RPI
  team.rpi <- record$RPI[which(record$Team == team)]
  
  team.rpi <- record %>%
    filter(Team == team) %>%
    .$RPI
  
  compare.team.rpi <- record %>%
    filter(Team == compare.team) %>%
    .$RPI
  
  if(team.rpi > compare.team.rpi){
    team.points <- team.points + 1
  } else if (team.rpi < compare.team.rpi){
    compare.team.points <- compare.team.points + 1
  }
  
  # Total
  
  if (team.points == compare.team.points){
    if(team.rpi > compare.team.rpi){
      return(1)
    } else {
      return(0)
    }
  } else {
    if (team.points > compare.team.points){
      return(1)
    } else {
      return(0)
    }
  }
}






