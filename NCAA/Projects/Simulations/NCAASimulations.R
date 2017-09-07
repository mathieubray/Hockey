library(dplyr)
library(purrr)

source("NCAA/NCAAFunctions.R")


### Load Historical Data

seasons <- 2015:2016
season.tags <- paste0(seasons-1,seasons)

retrieve.season.schedule <- function(season){
  
  schedule <- read.csv(paste0("NCAA/Data/Schedules/",season-1,"-",season,"_Clean.csv"),header=T,stringsAsFactors=F) %>%
    filter(!is.na(HomeScore),!is.na(AwayScore))
  
  return(schedule)
}

historical.seasons <- map(seasons,retrieve.season.schedule) %>%
  bind_rows

historical.results <- compile.results(historical.seasons)
historical.record <- compile.record(historical.results)
historical.rpi <- compile.rpi(historical.record, historical.results)

historical.stats <- read.csv("NCAA/Data/Statistics/NCAATeamStats.csv",header=T,stringsAsFactors=F) %>%
  filter(Season %in% season.tags)


### Load Sample Season Schedule

sample.season <- read.csv(paste0("NCAA/Data/Schedules/2016-2017_Clean.csv"),header=T,stringsAsFactors=F) %>%
  filter(!is.na(HomeScore),!is.na(AwayScore),
         Tournament == "No")

sample.results <- compile.results(sample.season)
sample.record <- compile.record(sample.results)
sample.rpi <- compile.rpi(sample.record,sample.results)


### Create Folder to Save Simulation Results

tag <- as.character(today())

directory <- paste0("NCAA/Projects/Simulations/",tag)
if(!dir.exists(directory)){
  dir.create(directory)
}



### Expected Goals Simulation

source("NCAA/Projects/Simulations/Simulation_ExpGoals.R")

expected.goals <- function(team, results, location){
  
  xG <- expected.goals.summary(results, location, query.team.names = team) %>%
    .$ExpectedGoals %>%
    head(1)
  
  return(xG)
}

xG.table <- historical.record %>% 
  rowwise %>%
  mutate(ExpGoalsHome = expected.goals(Team, historical.results, location="Home"),
         ExpGoalsAway = expected.goals(Team, historical.results, location="Away")) %>%
  select(Team,ExpGoalsHome,ExpGoalsAway)


iterations <- 10

exp.goal.results <- run.simulation.xG(iterations, sample.season, "Michigan", xG.table)

results.table <- data.frame(Method = "xG", RPI = unlist(exp.goal.results))

write.csv(results.table, paste0(directory,"/xG.csv"),row.names=F)


### Tiers Simulation

source("NCAA/Projects/Simulations/Simulation_Tiers.R")

number.of.tiers <- 5
tiers <- split.into.tiers(historical.results, historical.record, number.of.tiers)

team.tiers <- tiers$Tiers
tier.winning.pcts <- tiers$WinPcts

sample.season.tiers <- sample.season %>% 
  inner_join(y=team.tiers,by=c("Away"="Team"))%>%
  rename(AwayTier=Tier) %>%
  inner_join(y=team.tiers,by=c("Home"="Team")) %>%
  rename(HomeTier=Tier) 

iterations <- 10

tier.results <- run.simulation.tiers(iterations, sample.season.tiers, "Michigan", tier.winning.pcts)

results.table <- data.frame(Method = "Tiers", RPI = unlist(tier.results))

write.csv(results.table, paste0(directory,"/Tiers.csv"),row.names=F)


### RPI Simulation

source("NCAA/Projects/Simulations/Simulation_RPI.R")

prob.tie <- 0.1
iterations <- 10

rpi.results <- run.simulation.rpi(iterations, sample.season, "Michigan", prob.tie, historical.rpi)

results.table <- data.frame(Method = "RPI", RPI = unlist(rpi.results))

write.csv(results.table, paste0(directory,"/RPI.csv"),row.names=F)


### Win Pct Simulation

source("NCAA/Projects/Simulations/Simulation_FixedWinPct.R")

iterations <- 10
probs <- seq(0.2,0.8,by=0.2)
prob.tie <- 0.1

fixed.win.pct.simulation <- function(prob, its, sample.season){
  
  fixed.win.pct.results <- run.simulation.fixed(its, sample.season, "Michigan", prob, prob.tie)
  
  results.table <- data.frame(Method = "Fixed", Probability = prob, RPI = unlist(fixed.win.pct.results))
  
  return(results.table)
}

results.table <- map(probs, fixed.win.pct.simulation, its=iterations, sample.season=sample.season) %>%
  bind_rows

write.csv(results.table, paste0(directory,"/FixedWinPct.csv"),row.names=F)

