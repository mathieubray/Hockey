
source("NCAA/NCAAFunctions.R")

library(itertools)
library(doSNOW)
library(magrittr)

# Calculate expected goals based on historical games
expected.goals.summary <- function(games, locations,
                                   query.team.names=NULL,
                                   target.team.names=NULL,
                                   query.conference.names=NULL,
                                   target.conference.names=NULL){
  
  if(is.null(query.team.names)){
    query.team.names <- unique(games$Team)
  }
  if(is.null(target.team.names)){
    target.team.names <- unique(games$Team)
  }
  if(is.null(query.conference.names)){
    query.conference.names <- unique(games$Conference)
  }
  if(is.null(target.conference.names)){
    target.conference.names <- unique(games$Conference)
  }
  
  # Filter Game Results
  filtered.history <- games %>%
    filter(Status %in% locations,
           Conference %in% query.conference.names,
           OpponentConference %in% target.conference.names,
           Team %in% query.team.names,
           Opponent %in% target.team.names)
  
  # Summarize Record
  filtered.record <- compile.record(filtered.history)
  
  # Fit Goals as Poisson Model
  poisson.fit <- glm(Goals ~ 1, family="quasipoisson",data = filtered.history)
  
  lambda <- poisson.fit %>%
    summary %>%
    .$coef %>%
    extract(1) %>%
    exp
  
  disp <- poisson.fit %>%
    summary %>%
    .$dispersion
    
  
  # Summarize Actual and Predicted Goal Totals
  goal.summary <- filtered.history %>%
    group_by(Goals) %>%
    summarize(GoalCount = n())
  
  predictions <- map_dbl(0:max(goal.summary$Goals),dpois,lambda=lambda) * nrow(filtered.history)
  prediction.summary <- data.frame("Goals"=0:max(goal.summary$Goals),"Predicted"=predictions)
  
  goal.summary <- right_join(goal.summary, prediction.summary, by="Goals") %>%
    mutate(GoalCount = case_when(is.na(GoalCount) ~ 0,
                                 TRUE ~ as.numeric(GoalCount)))
  
  return(list(Games=filtered.history,
              Record=filtered.record,
              GoalSummary=goal.summary,
              ExpectedGoals=lambda,
              Dispersion=disp))
  
}


simulate.season.xG <- function(sample.schedule, teams.of.interest, xG.table, seed){
  
  set.seed(seed)
  
  new.score <- function(team,vals,home=T){
    
    if(home){
      lambda <- vals %>%
        filter(Team==team) %>%
        .$ExpGoalsHome
    } else {
      lambda <- vals %>%
        filter(Team==team) %>%
        .$ExpGoalsAway
    }
    
    score <- rpois(1,lambda = lambda)
    
    return(score)
  }
  
  stripped.schedule <- sample.schedule %>%
    select(Season,Away,Home,at,AwayScore,HomeScore,AwayConference,HomeConference) %>%
    rowwise %>%
    mutate(NewAwayScore = ifelse(Away %in% teams.of.interest || Home %in% teams.of.interest,
                                 new.score(Away,vals=xG.table,home=F),
                                 AwayScore)) %>%
    mutate(NewHomeScore = ifelse(Away %in% teams.of.interest || Home %in% teams.of.interest,
                                 ifelse(at == "@",
                                        new.score(Home,vals=xG.table,home=T),
                                        new.score(Home,vals=xG.table,home=F)),
                                 HomeScore)) %>%
    mutate(HomeWin = win(T,NewHomeScore,NewAwayScore)) %>%
    mutate(AwayWin = win(F,NewHomeScore,NewAwayScore)) %>%
    ungroup %>%
    select(Season,Away,Home,at,NewAwayScore,NewHomeScore,AwayWin,HomeWin,AwayConference,HomeConference) %>%
    rename(AwayScore = NewAwayScore, HomeScore = NewHomeScore)
  
  sample.results <- compile.results(stripped.schedule)
  sample.record <- compile.record(sample.results)
  sample.record <- compile.rpi(sample.record,sample.results)
  
  return(list(Results=sample.results,Record=sample.record))
}


run.simulation.xG <- function(iterations, sample.schedule, teams.of.interest, xG.table){
  
  required.functions <- c('win','weight','compile.results','compile.record','compile.rpi','OWP','OOWP','simulate.season.xG')
  
  machines <- rep("localhost", each=4)
  cl <- makeCluster(machines, type="SOCK")
  
  registerDoSNOW(cl)
  
  results <- foreach(i=1:iterations, .packages='tidyverse', .export=required.functions) %dopar% {
    
    rpi <- simulate.season.xG(sample.schedule, teams.of.interest, xG.table, i) %>%
      .$Record
    
    message("Finished Iteration ",i)
    
    return(rpi)
  }
  
  stopCluster(cl)
  
  return(results)
}
