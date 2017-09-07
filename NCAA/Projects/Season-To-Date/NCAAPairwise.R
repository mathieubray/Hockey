
source("NCAA/NCAAFunctions.R")


# Load Schedule and Compile Record and Results

tag <- "2016-2017_PostNCAA"

directory <- paste0("NCAA/Projects/Season-To-Date/",tag)

season.schedule <- read.csv(paste0(directory,"/Schedule.csv"),header=T,stringsAsFactors=F) %>% head(-3)

season.results <- compile.results(season.schedule)
season.record <- compile.record(season.results)
expanded.record <- compile.rpi(season.record,season.results)


# Generate All Possible Matchups and Perform Pairwise Comparison

teams <- c(season.schedule$Away, season.schedule$Home) %>% unique

pw.result <- expand.grid(Team=teams, Opponent=teams, stringsAsFactors = F) %>%
  filter(Team != Opponent, Team < Opponent) %>%
  arrange(Team, Opponent) %>%
  rowwise %>%
  mutate(PW = pairwise.comparison(Team, Opponent, record=expanded.record, results=season.results))


# Calculate Pairwise for Each Team

obtain.pairwise <- function(team, pw){
  
  focus.on.team <- pw %>%
    filter(Team == team | Opponent == team) %>%
    mutate(Value = ifelse(Team == team, PW, 1-PW)) %>%
    .$Value %>%
    sum
  
  return(focus.on.team)
}

pairwise <- data.frame(Team=teams, stringsAsFactors=F) %>%
  rowwise() %>%
  mutate(Pairwise = obtain.pairwise(Team, pw.result)) %>%
  arrange(desc(Pairwise))


# Combine Record

full.record <- inner_join(expanded.record, pairwise, by="Team") %>%
  arrange(desc(Pairwise), desc(RPI))
