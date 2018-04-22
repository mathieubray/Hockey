library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(gganimate)
library(lubridate)
library(purrr)
library(viridis)

# Load data

load.bios <- function(season, playerType){
  
  seasonID <- paste0(season-1,season)
  
  bios <- read.csv(paste0("HeightWeight/data/NHL",playerType,"HeightWeight_",seasonID,".csv"),header=T,stringsAsFactors=F)
  
  return(bios)
  
}

player.bios <- map_df(2010:2017,load.bios,playerType="Player") %>%
  mutate(Position = if_else(Position == "D", "D", "F"))

goalie.bios <- map_df(2010:2017,load.bios,playerType="Goalie") %>%
  mutate(Position="G")

bios <- bind_rows(player.bios,goalie.bios) %>%
  filter(!is.na(Height),!is.na(Weight)) %>%
  arrange(Season,Name)


### GIFs

p <- ggplot(bios, aes(x=Height,y=Weight,color=Position,shape=Position,frame=Season)) +
  geom_jitter(size=5,alpha=0.7) +
  scale_color_manual(name="Position",values=c("blue","orange","purple")) +
  scale_shape_manual(name="Position",values=c(16,15,17)) +
  xlim(60,85) +
  xlab("Height (in.)")+
  ylim(120,275) +
  ylab("Weight (lb.)") +
  ggtitle("NHL Players Height and Weight: ") +
  theme_bw(16)

gg_animate(p,"HeightWeight/HeightWeight.gif")


p2 <- ggplot(bios, aes(x=Height,y=Weight,frame=Season)) +
  facet_wrap(~Position)+
  geom_jitter(size=5,alpha=0.7,color="blue",shape=16)+
  xlim(60,85) +
  xlab("Height (in.)")+
  ylim(120,275) +
  ylab("Weight (lb.)") +
  ggtitle("NHL Players Height and Weight: ") +
  theme_bw(16)

gg_animate(p2,"HeightWeight/HeightWeightByPosition.gif")


# Ridge Plots

ggplot(goalie.bios, aes(x=Height,y=as.factor(Season))) +
  geom_density_ridges(fill="blue",scale=2,alpha=0.5,rel_min_height=0.01) +
  xlab("Height (in.)") +
  ylab("Season") +
  ggtitle("NHL Goaltender Height Distribution by Season",
          subtitle="All goaltenders who appeared in at least 1 game (not weighted by number of games played)\nData from NHL.com") +
  ggplot2::annotate("text",x=72,y=4,col="red",label=paste("@mathieubray",year(today())),alpha=0.1,cex=15,fontface="bold",angle=15) +
  theme_bw(16)

ggplot(bios, aes(x=Height,y=as.factor(Season),fill=Position)) +
  geom_density_ridges(scale=2,alpha=0.5,rel_min_height=0.01) +
  xlab("Height (in.)") +
  ylab("Season") +
  scale_fill_viridis(discrete=T) +
  ggtitle("NHLHeight Distribution by Season by Position",
          subtitle="All players who appeared in at least 1 game (not weighted by number of games played)\nData from NHL.com") +
  ggplot2::annotate("text",x=72,y=4,col="red",label=paste("@mathieubray",year(today())),alpha=0.1,cex=15,fontface="bold",angle=15) +
  theme_bw(16)



