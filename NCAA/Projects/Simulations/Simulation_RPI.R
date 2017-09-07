
source("NCAA/NCAAFunctions.R")

library(itertools)
library(doSNOW)

simulate.season.rpi <- function(sample.schedule, teams.of.interest, prob.tie, rpi.table, seed){
  
  set.seed(seed)
  
  rpi.probs <- function(home.team, away.team, tie, rpis){
    away.rpi <- rpis %>%
      filter(Team == away.team) %>%
      .$RPI %>%
      head(1)
    
    home.rpi <- rpis %>%
      filter(Team == home.team) %>%
      .$RPI %>%
      head(1)
    
    home.win.pct <- home.rpi/(home.rpi + away.rpi)
    
    return(c(home.win.pct, (1-tie) - home.win.pct, tie))
  }
  
  stripped.schedule <- sample.schedule %>%
    select(Season,Away,Home,at,AwayScore,HomeScore,AwayConference,HomeConference) %>%
    rowwise %>%
    mutate(NewHomeScore = ifelse(Home %in% teams.of.interest,
                                  sample(c(2,0,1),size=1,prob=rpi.probs(Home, Away, tie=prob.tie, rpis=rpi.table)),
                                  ifelse(Away %in% teams.of.interest,
                                          sample(c(0,2,1),size=1,prob=rpi.probs(Home, Away, tie=prob.tie, rpis=rpi.table)),
                                          HomeScore))) %>%
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




run.simulation.rpi <- function(iterations, sample.schedule, teams.of.interest, prob.tie, rpi.table){
  
  required.functions <- c('win','weight','compile.results','compile.record','compile.rpi','OWP','OOWP','simulate.season.rpi')
  
  machines <- rep("localhost", each=4)
  cl <- makeCluster(machines, type="SOCK")
  
  registerDoSNOW(cl)
  
  results <- foreach(i=1:iterations, .packages='dplyr', .export=required.functions) %dopar% {
    
    rpi <- simulate.season.rpi(sample.schedule, teams.of.interest, prob.tie, rpi.table, i) %>%
      .$Record %>%
      filter(Team == "Michigan") %>%
      .$RPI
    
    return(rpi)
  }
  
  stopCluster(cl)
  
  return(results)
}