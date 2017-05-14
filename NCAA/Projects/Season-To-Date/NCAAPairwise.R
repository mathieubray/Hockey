
source("NCAA/NCAAFunctions.R")

tag <- "2016-2017_PostNCAA"

directory <- paste0("NCAA/Projects/Season-To-Date/",tag)

# Load USCHO values

season.schedule <- read.csv(paste0(directory,"/Schedule.csv"),header=T,stringsAsFactors=F)

season.schedule <- season.schedule[1:(nrow(season.schedule)-3),]

season.results <- compile.results(season.schedule)
season.record <- compile.record(season.results)
expanded.record <- compile.rpi(season.record,season.results)


k <- expanded.record %>%
  select(Team,Wins,Losses,Ties,AdjPct,SOS)








# Pairwise calculator
pairwise.comparison <- function(team,compare.team,record,results){
  
  team.points <- 0
  compare.team.points <- 0
  
  # Head to Head
  head.to.head <- results %>% 
    filter(Team==team,Opponent==compare.team)
  
  head.to.head.games <- nrow(head.to.head)
  head.to.head.wins <- nrow(head.to.head %>% filter(Win == "Win"))
  head.to.head.losses <- nrow(head.to.head %>% filter(Win == "Loss"))
  
  team.points <- team.points + head.to.head.wins
  compare.team.points <- compare.team.points + head.to.head.losses
  
  # Common Opponents
  team.opponents <- unique((results %>% filter(Team==team))$Opponent)
  compare.team.opponents <- unique((results %>% filter(Team==compare.team))$Opponent)
  common.opponents <- intersect(team.opponents,compare.team.opponents)
  
  common.opponent.results <- results %>% 
    filter(Team %in% c(team,compare.team),Opponent %in% common.opponents) %>%
    group_by(Team,Opponent) %>%
    summarize(Wins=sum(Win=="Win"),
              Losses=sum(Win=="Loss"),
              Ties=sum(Win=="Tie")) %>%
    mutate(Pct=(Wins+0.5*Ties)/(Wins + Losses + Ties)) %>%
    group_by(Team) %>%
    summarize(AvgPct = mean(Pct))
  
  if (nrow(common.opponent.results) > 0){
    team.record <- common.opponent.results$AvgPct[which(common.opponent.results$Team == team)] 
    compare.team.record <- common.opponent.results$AvgPct[which(common.opponent.results$Team == compare.team)]
    
    if (team.record > compare.team.record){
      team.points <- team.points + 1
    } else if (team.record < compare.team.record){
      compare.team.points <- compare.team.points+1
    }
  }
  
  #RPI
  team.rpi <- record$RPI[which(record$Team == team)]
  compare.team.rpi <- record$RPI[which(record$Team == compare.team)]
  
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
  
  return(winner)
}

teams <- unique(c(schedule$Away,schedule$Home))

possibilities <- expand.grid(Team=teams,Opponent=teams,stringsAsFactors = F) %>%
  filter(Team != Opponent, Team < Opponent) %>%
  arrange(Team,Opponent) %>%
  rowwise() %>%
  mutate(PW = pairwise.comparison(Team,Opponent,record=expanded.record,results=season.results))

obtain.pairwise <- function(team,possibilities){
  
  focus.on.team <- possibilities %>%
    filter(Team == team | Opponent == team) %>%
    mutate(Value = ifelse(Team == team,PW,1-PW))
  
  return(sum(focus.on.team$Value))
}


final.pw <- data.frame(Team = teams, stringsAsFactors=F) %>%
  rowwise() %>%
  mutate(Pairwise = obtain.pairwise(Team,possibilities)) %>%
  arrange(desc(Pairwise))





uscho <- read.csv(paste0(directory,"/USCHO.csv"),header=T,stringsAsFactors=F) %>%
  select(Team,PairwiseComparisonsWon) %>%
  arrange(desc(PairwiseComparisonsWon))

k <- uscho %>%
  inner_join(final.pw,by="Team") %>%
  mutate(R = Pairwise - PairwiseComparisonsWon)
