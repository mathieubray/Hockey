library(dplyr)
library(tm)
library(stringr)
library(purrr)

draft.table <- read.csv("Draft/data/2017_NHL-Draft.csv",header=T,stringsAsFactors=F)

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




