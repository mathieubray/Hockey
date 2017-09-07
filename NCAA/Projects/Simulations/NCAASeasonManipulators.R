library(dplyr)

source("NCAA/NCAAFunctions.R")


sample.season <- read.csv(paste0("NCAA/Data/Schedules/2016-2017_Clean.csv"),header=T,stringsAsFactors=F) %>%
  filter(!is.na(HomeScore),!is.na(AwayScore),
         Tournament == "No")

sample.results <- compile.results(sample.season)
sample.record <- compile.record(sample.results)
sample.rpi <- compile.rpi(sample.record,sample.results)


games.to.change <- sample.season %>%
  mutate(GameNumber = row_number()) %>%
  filter(Home == "Michigan") %>%
  .$GameNumber



### Switching Up Home and Away

switch.venue <- function(schedule, games, tiers=F){
  
  new.games <- schedule %>%
    mutate(GameNumber = row_number()) %>%
    filter(GameNumber %in% games) %>%
    mutate(NewHome = Away,
           NewAway = Home,
           NewHomeConference = AwayConference,
           NewAwayConference = HomeConference,
           NewHomeScore = AwayScore,
           NewAwayScore = HomeScore,
           NewHomeWin = AwayWin,
           NewAwayWin = HomeWin) %>%
    mutate(Home = NewHome,
           Away = NewAway,
           HomeConference = NewHomeConference,
           AwayConference = NewAwayConference,
           HomeScore = NewHomeScore,
           AwayScore = NewAwayScore,
           HomeWin = NewHomeWin,
           AwayWin = NewAwayWin) %>%
    select(-starts_with("New"))
  
  if (tiers){
    
    new.games <- new.games %>%
      mutate(NewHomeTier = AwayTier,
             NewAwayTier = HomeTier) %>%
      mutate(HomeTier = NewHomeTier,
             AwayTier = NewAwayTier) %>%
      select(-NewHomeTier,-NewAwayTier)
  }
  
  new.schedule <- schedule %>%
    mutate(GameNumber = row_number()) %>%
    filter(!(GameNumber %in% games)) %>%
    rbind(new.games) %>%
    arrange(GameNumber) %>%
    select(-GameNumber)
  
  return(new.schedule)
  
}

road.michigan.season <- switch.venue(sample.season, games.to.change)

new.sample.results <- compile.results(road.michigan.season)
new.sample.record <- compile.record(new.sample.results)
new.sample.rpi <- compile.rpi(new.sample.record,new.sample.results)


rpi <- sample.rpi %>% 
  select(Team,RPI) %>% 
  filter(Team=="Michigan") %>%
  mutate(Sample = "Old")

new.rpi <- new.sample.rpi %>% 
  select(Team,RPI) %>% 
  filter(Team=="Michigan") %>%
  mutate(Sample = "New")

rbind(rpi, new.rpi)



# User Assigns Scores

change.score <- function(schedule, games, home.scores, away.scores){
  
  if (length(games) != length(home.scores) | length(games) != length(away.scores)){
    warning("Game and Score Vectors are Different Sizes; Returning Unchanged Schedule")
    
    return(schedule)
  }
  
  new.scores <- schedule %>%
    mutate(GameNumber = row_number()) %>%
    filter(GameNumber %in% games) %>%
    mutate(HomeScore = home.scores,
           AwayScore = away.scores) %>%
    rowwise %>%
    mutate(HomeWin = win(T,HomeScore,AwayScore),
           AwayWin = win(F,HomeScore,AwayScore))
  
  new.schedule <- schedule %>%
    mutate(GameNumber = row_number()) %>%
    filter(!(GameNumber %in% games)) %>%
    rbind(new.scores) %>%
    arrange(GameNumber) %>%
    select(-GameNumber)
  
  return(new.schedule)
}


home.sc <- rep(3,18)
away.sc <- rep(2,18)

weird.michigan.season <- change.score(sample.season, games.to.change, home.sc, away.sc) 



# User Assigns Teams

swap.teams <- function(schedule, games, new.teams, new.conferences, home=T, new.tiers=NULL){
  
  if (length(games) != length(new.teams) | length(games) != length(new.conferences)){
    warning("Game and Team Vectors are Different Sizes; Returning Unchanged Schedule")
    
    return(schedule)
  }
  
  if (home){
    new.teams <- schedule %>%
      mutate(GameNumber = row_number()) %>%
      filter(GameNumber %in% games) %>%
      mutate(NewHome = new.teams,
             NewHomeConference = new.conferences)
    
    if (!is.null(new.tiers)){
      
      if (length(games) != length(new.tiers)){
        
        warning("Game and Team Vectors are Different Sizes; Returning Unchanged Schedule")
        
      } else {
        
        new.teams <- new.teams %>%
          mutate(NewHomeTier = new.tiers)
      }
    }
    
    new.teams <- new.teams %>%
      rowwise %>%
      mutate(Home = ifelse(NewHome == Away, Home, NewHome),
             HomeConference = ifelse(NewHome == Away, HomeConference, NewHomeConference)) %>%
      ungroup
    
    if (!is.null(new.tiers)){
      new.teams <- new.teams %>%
        rowwise %>%
        mutate(HomeTier = ifelse(NewHome == Away, HomeTier, NewHomeTier)) %>%
        ungroup
    }
    
    
    
  } else {
    
    new.teams <- schedule %>%
      mutate(GameNumber = row_number()) %>%
      filter(GameNumber %in% games) %>%
      mutate(NewAway = new.teams,
             NewAwayConference = new.conferences)
    
    if (!is.null(new.tiers)){
      
      if (length(games) != length(new.tiers)){
        
        warning("Game and Team Vectors are Different Sizes; Returning Unchanged Schedule")
        
      } else {
        
        new.teams <- new.teams %>%
          mutate(NewAwayTier = new.tiers)
      }
    }
    
    new.teams <- new.teams %>%
      rowwise %>%
      mutate(Away = ifelse(NewAway == Home, Away, NewAway),
             AwayConference = ifelse(NewAway == Home, AwayConference, NewAwayConference)) %>%
      ungroup
    
    if (!is.null(new.tiers)){
      new.teams <- new.teams %>%
        rowwise %>%
        mutate(AwayTier = ifelse(NewAway == Home, AwayTier, NewAwayTier)) %>%
        ungroup
    }
  }
  
  new.teams <- new.teams %>%
    select(-starts_with("New"))
  
  new.schedule <- schedule %>%
    mutate(GameNumber = row_number()) %>%
    filter(!(GameNumber %in% games)) %>%
    rbind(new.teams) %>%
    arrange(GameNumber) %>%
    select(-GameNumber)
  
  return(new.schedule)
  
}


new.teams <- rep("Michigan State", 18)
new.conferences <- rep("Big Ten", 18)

away.vs.msu.michigan.season <- swap.teams(sample.season, games.to.change, new.teams, new.conferences, home=F) 

