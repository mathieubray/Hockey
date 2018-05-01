
source("NCAA/NCAAFunctions.R")
source("NCAA/Projects/Simulations/Simulation_ExpGoals.R")

library(itertools)
library(doSNOW)


# Split teams into tiers based on historical games

split.into.tiers <- function(results, record, number.of.tiers){
  
  locations <- c("Home","Away","Neutral")
  tiers <- paste("Tier",number.of.tiers:1)
  
  tiers.info <- record %>% 
    arrange(desc(AdjPct)) %>%
    mutate(Tier = cut_number(AdjPct, number.of.tiers, labels=tiers)) %>% 
    select(Team,Tier)
  
  cross.tiers <- split(expand.grid(FirstTier = tiers, SecondTier = tiers), seq(length(tiers)^2))
  
  extract.tier.win.pcts <- function(tiers.list){
    
    first.tier <- tiers.list %>%
      .$FirstTier %>%
      as.character
    
    second.tier <- tiers.list %>%
      .$SecondTier %>%
      as.character
    
    first.teams <- tiers.info %>%
      filter(Tier == first.tier) %>%
      .$Team
    
    second.teams <- tiers.info %>%
      filter(Tier == second.tier) %>%
      .$Team
      
    xG <- expected.goals.summary(results, 
                                 locations = locations, 
                                 query.team.names = first.teams, 
                                 target.team.names = second.teams) %>% 
      .$Record
    
    home.games <- sum(xG$HomeWins) + sum(xG$HomeLosses) + sum(xG$HomeTies)
    home.win.pct <- sum(xG$HomeWins)/home.games
    home.loss.pct <- sum(xG$HomeLosses)/home.games
    home.tie.pct <- sum(xG$HomeTies)/home.games
    away.games <- sum(xG$AwayWins) + sum(xG$AwayLosses) + sum(xG$AwayTies)
    away.win.pct <- sum(xG$AwayWins)/away.games
    away.loss.pct <- sum(xG$AwayLosses)/away.games
    away.tie.pct <- sum(xG$AwayTies)/away.games
    
    tier.win.pcts <- data.frame(Tier1 = first.tier,
                                      Tier2 = second.tier,
                                      HomeWinPct = home.win.pct,
                                      HomeLossPct = home.loss.pct, 
                                      HomeTiePct = home.tie.pct, 
                                      AwayWinPct = away.win.pct, 
                                      AwayLossPct = away.loss.pct, 
                                      AwayTiePct = away.tie.pct, 
                                      stringsAsFactors = F)
    
    return(tier.win.pcts)
    
  }
  
  tier.win.pcts <- map(cross.tiers, extract.tier.win.pcts) %>%
    bind_rows
  
  return(list(Tiers = tiers.info, WinPcts = historical.win.pcts))
}



simulate.season.tiers <- function(sample.schedule, teams.of.interest, tier.table, seed){
  
  set.seed(seed)
  
  tier.probs <- function(home.tier, away.tier, vals) {
    probs <- vals %>% 
      filter(Tier1 == home.tier, Tier2 == away.tier) %>%
      `[`(1,3:5)
    
    return(probs)
  }
  
  stripped.schedule <- sample.schedule %>%
    select(Season,Away,Home,at,AwayScore,HomeScore,AwayConference,HomeConference,HomeTier,AwayTier) %>%
    rowwise %>%
    mutate(NewHomeScore = ifelse(Away %in% teams.of.interest || Home %in% teams.of.interest,
                                  sample(c(2,0,1),size=1,prob=tier.probs(HomeTier,AwayTier,vals=tier.table)),
                                  HomeScore)) %>%
    mutate(NewAwayScore = ifelse(Away %in% teams.of.interest || Home %in% teams.of.interest,
                                  ifelse(NewHomeScore==2,0,ifelse(NewHomeScore==0,2,1)),
                                  AwayScore)) %>%
    mutate(HomeWin = win(T,NewHomeScore,NewAwayScore)) %>%
    mutate(AwayWin = win(F,NewHomeScore,NewAwayScore)) %>%
    ungroup %>%
    select(Season,Away,Home,at,NewAwayScore,NewHomeScore,AwayWin,HomeWin,AwayConference,HomeConference) %>%
    rename(AwayScore = NewAwayScore, HomeScore = NewHomeScore)
  
  sample.results <- compile.results(stripped.schedule)
  sample.record <- compile.record(sample.results)
  sample.record <- compile.rpi(sample.record,sample.results)
  
  return(list(Results=sample.results, Record=sample.record))
}


run.simulation.tiers <- function(iterations, sample.schedule, teams.of.interest, tier.table){
  
  required.functions <- c('win','weight','compile.results','compile.record','compile.rpi','OWP','OOWP','simulate.season.tiers')
  
  machines <- rep("localhost", each=4)
  cl <- makeCluster(machines, type="SOCK")
  
  registerDoSNOW(cl)
  
  results <- foreach(i=1:iterations, .packages='dplyr', .export=required.functions) %dopar% {
    
    rpi <- simulate.season.tiers(sample.schedule, teams.of.interest, tier.table, i) %>%
      .$Record %>%
      filter(Team == "Michigan") %>%
      .$RPI
    
    return(rpi)
  }
  
  stopCluster(cl)
  
  return(results)
}
