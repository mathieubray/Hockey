
source("NCAA/NCAAFunctions.R")


# Load Schedule and Compile Record and Results

tag <- "2017-2018"

#season.schedule <- read_csv(paste0("NCAA/Season-To-Date/",tag,"/Schedule.csv")) %>%
 # filter(!is.na(HomeScore))

#season.results <- compile.results(season.schedule)
#season.record <- compile.record(season.results)
#expanded.record <- compile.rpi(season.record,season.results)

season.results <- read_csv(paste0("NCAA/Season-To-Date/",tag,"/Results.csv"))
season.expanded.record <- read_csv(paste0("NCAA/Season-To-Date/",tag,"/Record.csv"))


# Generate All Possible Matchups and Perform Pairwise Comparison

teams <- season.expanded.record$Team

pw.result <- crossing(Team=teams, Opponent=teams) %>%
  filter(Team != Opponent, Team < Opponent) %>%
  arrange(Team, Opponent) %>%
  rowwise %>%
  mutate(PW = pairwise.comparison(Team, Opponent, record=season.expanded.record, results=season.results))


# Calculate Pairwise for Each Team

obtain.pairwise <- function(team, pw){
  
  focus.on.team <- pw %>%
    filter(Team == team | Opponent == team) %>%
    mutate(Value = if_else(Team == team, PW, 1-PW)) %>%
    .$Value %>%
    sum
  
  return(focus.on.team)
}

pairwise <- data.frame(Team=teams, stringsAsFactors=F) %>%
  rowwise() %>%
  mutate(Pairwise = obtain.pairwise(Team, pw.result)) %>%
  arrange(desc(Pairwise))


# Combine Record

full.record <- season.expanded.record %>%
  left_join(pairwise, by="Team") %>%
  arrange(desc(Pairwise), desc(RPI))

write_csv(full.record, paste0("NCAA/Season-To-Date/",tag,"/Record.csv"))
