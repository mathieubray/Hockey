### Expected Goals Simulation

historical.season.dates <- 2016:2017

retrieve.conference.info <- function(season){
  
  conferences <- read_csv(paste0("NCAA/Data/Statistics/",season-1,"-",season,"_Conferences.csv"))
  
  return(conferences)
  
}

historical.conferences <- historical.season.dates %>%
  map_df(retrieve.conference.info)

retrieve.season.schedule <- function(season){
  
  schedule <- read_csv(paste0("NCAA/Data/Schedules/",season-1,"-",season,"_Clean.csv")) %>%
    filter(!is.na(HomeScore),!is.na(AwayScore))
  
  return(schedule)
}

historical.seasons <- historical.season.dates %>%
  map_df(retrieve.season.schedule)

historical.results <- compile.results(historical.seasons)
historical.record <- compile.record(historical.results)
historical.rpi <- compile.rpi(historical.record, historical.results)

historical.results.with.conferences <- historical.results %>%
  left_join(historical.conferences, by=c("Team" = "Team", "Season"="Season")) %>%
  left_join(historical.conferences, by=c("Opponent" = "Team", "Season"="Season"), suffix=c("","Opponent")) %>%
  rename(OpponentConference = ConferenceOpponent)



source("NCAA/Simulations/Simulation_ExpGoals.R")

expected.goals <- function(team, results, location){
  
  xG <- expected.goals.summary(results, location, query.team.names = team) %>%
    .$ExpectedGoals %>%
    head(1)
  
  return(xG)
}

xG.teams <- historical.record$Team

xG.home <- map_dbl(xG.teams, expected.goals, results=historical.results.with.conferences, location="Home")
xG.away <- map_dbl(xG.teams, expected.goals, results=historical.results.with.conferences, location="Away")

xG.table <- tibble(Team=xG.teams,ExpGoalsHome=xG.home,ExpGoalsAway=xG.away)

iterations <- 10

exp.goal.results <- run.simulation.xG(iterations, sample.season, xG.teams, xG.table)

results.table <- exp.goal.results %>% bind_rows

write_csv(results.table, paste0(directory,"/xG.csv"))


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