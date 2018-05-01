library(dplyr)
library(readr)
library(tidyr)
library(ggplot2) 
library(igraph)
library(ggraph)

# Load season schedule

season.schedule <- read_csv("NCAA/Data/Schedules/2017-2018_Clean.csv")
conferences <- read_csv("NCAA/Data/Statistics/2017-2018_Conferences.csv") %>% select(-Season)

# Set up network

nodes <- tibble(id = unique(c(season.schedule$Home,season.schedule$Away)))  %>%
  left_join(conferences,by=c("id" = "Team"))

games <- season.schedule %>%
  select(Home,Away)

ncaa.graph <- graph_from_data_frame(games, vertices = nodes)


# Plot network

ggraph(ncaa.graph, layout = 'kk') +
  geom_edge_link() +
  geom_node_label(aes(label = name, fill = Conference), alpha=0.75) +
  theme_graph()

