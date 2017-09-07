library(rvest)
library(plyr)
library(dplyr)
library(tm)
library(stringr)
library(purrr)

## Collect Draft Information

get.draft.table <- function(season){
  
  url <- paste0('http://www.hockeydb.com/ihdb/draft/nhl',season,'e.html')
  
  data <- url %>% 
    read_html %>% 
    html_nodes('table') %>%
    html_table(header=T, fill=T) 
  
  draft.table.raw <- data[[1]][-1,1:6]
  
  row.names(draft.table.raw) <- NULL
  colnames(draft.table.raw) <- c("Round","Pick","NHLTeam","Name","Position","League")
  
  draft <- draft.table.raw %>% filter(Round %in% as.character(1:7)) # Remove redundant rows
  
  # Extract league
  leagues <- strsplit(draft$League,"[[(]")
  
  extract.league <- function(team.string){
    team <- ""
    league <- ""
    team.league <- team.string %>% removePunctuation %>% str_trim
    
    if(length(team.league)==2){
      team <- team.league[1]
      league <- team.league[2]
    } else if (length(team.league)==1){
      team <- team.league[1]
    }
    
    return(data.frame(Team=team,League=league,stringsAsFactors=F))
  }
  
  league.table <- bind_rows(lapply(leagues,extract.league))
  
  draft.table <- cbind(draft %>% select(-League),league.table)
  
  write.csv(draft.table,paste0("Draft/",season,"_NHL-Draft.csv"), row.names=F)
  
}

map(2010:2017,get.draft.table)

######################################################################


draft.table <- read.csv("Draft/2017_NHL-Draft.csv",header=T,stringsAsFactors=F)

draft.reduced <- draft.table %>%
  select(Name,League,NHLTeam) %>%
  group_by(League,NHLTeam) %>%
  dplyr::summarize(Players=n())


library(googleVis)

plot(gvisSankey(draft.reduced, from="League", to="NHLTeam", weight="Players",
                options=list(height=800, width=850,
                             sankey="{
     link:{color:{fill: 'lightgray', fillOpacity: 0.7}},
     node:{nodePadding: 5, label:{fontSize: 12}, interactivity: true, width: 20},
   }")
  )
)

# http://thedatagame.com.au/2015/12/14/visualising-the-2015-nba-draft-in-r/?utm_content=buffer56c30&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer

ushl <- draft.table %>%
  filter(League == "USHL") %>%
  group_by(Team,NHLTeam) %>%
  summarize(Players=n()) %>%
  arrange(Team,NHLTeam)

plot(gvisSankey(ushl, from="Team", to="NHLTeam", weight="Players",
                options=list(height=800, width=850,
                             sankey="{
                             link:{color:{fill: 'lightgray', fillOpacity: 0.7}},
                             node:{nodePadding: 10, label:{fontSize: 12}, interactivity: true, width: 20},
                             }")))




