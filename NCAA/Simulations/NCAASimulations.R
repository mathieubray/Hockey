library(tidyverse)
library(magrittr)
library(mathieuR)
library(gridExtra)

source("NCAA/NCAAFunctions.R")

### Load Sample Season Schedule

sample.season.date <- 2018

sample.conferences <- read_csv(paste0("NCAA/Data/Statistics/",sample.season.date-1,"-",sample.season.date,"_Conferences.csv")) %>%
  select(Team,Conference)

sample.season <- read_csv(paste0("NCAA/Data/Schedules/",sample.season.date-1,"-",sample.season.date,"_Clean.csv")) %>%
  filter(!is.na(HomeScore),!is.na(AwayScore)) %>%
  mutate(Game = row_number())

sample.results <- compile.results(sample.season)
sample.record <- compile.record(sample.results)
sample.rpi <- compile.rpi(sample.record,sample.results)
sample.pw <- compile.pw(sample.rpi,sample.results)

### Get Expected Goals Table

xG.table <- sample.season %>%
  filter(GameType != "NC" ) %>%
  compile.results %>%
  group_by(Team, Status) %>%
  summarize(xG = mean(Goals))

xG.table.azstate <- sample.season %>%
  filter(Home == "Arizona State" | Away == "Arizona State") %>%
  compile.results %>%
  filter(Team == "Arizona State") %>%
  group_by(Team, Status) %>%
  summarize(xG = mean(Goals))

xG.table <- bind_rows(xG.table, xG.table.azstate)


# For Michigan

games.of.interest <- sample.season %>%
  filter(Home == "Michigan" | Away == "Michigan", GameType == "NC", Tournament == "No") %>%
  .$Game

non.conference.opponents <- sample.conferences %>%
  filter(Conference != "Big Ten")

simulate.season.xG <- function(i, sample.season, games.of.interest, xG.table){
  
  new.score <- function(team,location){
    
    score <- xG.table %>%
        filter(Team==team, Status==location) %>%
        .$xG %>%
        rpois(1,lambda = .)
    
    return(score)
  }
  
  new.teams <- non.conference.opponents %>%
    mutate(NewTeam = sample(Team)) %>%
    select(-Conference) %>%
    bind_rows(tibble(Team="Michigan",NewTeam="Michigan"))
  
  new.schedule <- sample.season %>%
    filter(Game %in% games.of.interest) %>%
    left_join(new.teams,by=c("Away" = "Team")) %>%
    select(-Away) %>%
    rename(Away = NewTeam) %>%
    left_join(new.teams,by=c("Home" = "Team")) %>%
    select(-Home) %>%
    rename(Home = NewTeam) %>%
    select(Season,Away,AwayScore,at,Home,HomeScore,OT:Game)
  
  new.schedule.final <- sample.season %>%
    filter(!(Game %in% games.of.interest)) %>%
    bind_rows(new.schedule) %>%
    arrange(Game)
  
  message(paste0(i,": Generated New Schedule"))
  
  stripped.schedule <- new.schedule.final %>%
    select(Season,Game,Away,Home,at,AwayScore,HomeScore) %>%
    rowwise %>%
    mutate(NewAwayScore = case_when(Game %in% games.of.interest ~ new.score(Away,location="Away"),
                                    TRUE ~ AwayScore)) %>%
    mutate(NewHomeScore = case_when(Game %in% games.of.interest && at == "@" ~ new.score(Home,location="Home"),
                                    Game %in% games.of.interest && at != "@" ~ new.score(Home,location="Away"),
                                    TRUE ~ HomeScore)) %>%
    ungroup
  
  message(paste0(i,": Simulated New Scores"))
  
  stripped.sched <- stripped.schedule %>%
    mutate(HomeWin = case_when(NewHomeScore > NewAwayScore ~ "Win",
                               NewHomeScore < NewAwayScore ~ "Loss",
                               TRUE ~ "Tie")) %>%
    mutate(AwayWin = case_when(NewHomeScore < NewAwayScore ~ "Win",
                               NewHomeScore > NewAwayScore ~ "Loss",
                               TRUE ~ "Tie")) %>%
    select(Season,Away,Home,at,NewAwayScore,NewHomeScore,AwayWin,HomeWin) %>%
    rename(AwayScore = NewAwayScore, HomeScore = NewHomeScore)
  
  sample.results <- compile.results(stripped.sched)
  
  message(paste0(i,": Compiled New Results"))
  
  sample.record <- compile.record(sample.results)
  sample.rpi <- compile.rpi(sample.record,sample.results)
  sample.pw <- compile.pw(sample.rpi,sample.results)
  
  message(paste0(i,": Pairwise Complete"))
  
  return(sample.pw)
}

set.seed(1001)

sims <- map(1:200, simulate.season.xG, 
       sample.season=sample.season, 
       games.of.interest=games.of.interest, 
       xG.table=xG.table)

final.sim.results <- bind_rows(sims, .id="Simulation")

write_csv(final.sim.results,"NCAA/Simulations/Sim_Results.csv")


### Plot Preliminary Results

final.sim.results <- read_csv("NCAA/Simulations/Sim_Results.csv")

michigan.sim.results <- final.sim.results %>%
  filter(Team == "Michigan") %>%
  select(RPI,PWRank) %>%
  mutate(PWRankFactor = factor(PWRank, levels=1:30))

p1 <- ggplot(data=michigan.sim.results, aes(x=PWRankFactor)) +
  geom_bar(stat="count",color="#00274c",fill="#ffcb05") +
  scale_x_discrete(drop=FALSE) +
  ylim(0,45) +
  theme_mathieu(14,title.label="Pairwise Ranking Results for Michigan",
                subtitle.label="Based on 200 Resamplings of Michigan's Non-Conference Schedule",
                x.label = "Pairwise Ranking",
                y.label = "Number of Simulations",
                x.watermark = 15,
                y.watermark = 20,
                watermark.size = 12)

p2 <- ggplot(data=michigan.sim.results, aes(x=RPI)) +
  geom_histogram(binwidth=0.0025,color="#00274c",fill="#ffcb05") +
  xlim(0.45,0.6) +
  ylim(0,45) +
  theme_mathieu(14,title.label="RPI Results for Michigan",
                subtitle.label="Based on 200 Resamplings of Michigan's Non-Conference Schedule",
                x.label = "RPI",
                y.label = "Number of Simulations",
                x.watermark = 0.525,
                y.watermark = 20,
                watermark.size = 12)
  
  
grid.arrange(p1, p2, nrow = 1)
