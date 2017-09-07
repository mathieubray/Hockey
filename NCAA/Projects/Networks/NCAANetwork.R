library(dplyr)
library(tidyr)
library(ggplot2) 
library(igraph)
library(ggraph)

# Load season schedule

season.schedule <- read.csv("NCAA/Data/Schedules/2016-2017_Clean.csv",header=T,stringsAsFactors=F)

# Set up network

nodes <- data.frame(id = unique(c(season.schedule$Home,season.schedule$Away)), stringsAsFactors=F) 

games <- season.schedule %>%
  select(Home,Away)

ncaa.graph <- graph_from_data_frame(games)
V(ncaa.graph)$TeamName <- V(ncaa.graph)$name

# Plot network

ggraph(ncaa.graph, layout = 'kk') +
  geom_edge_link() +
  geom_node_label(aes(label = name)) +
  theme_graph()

