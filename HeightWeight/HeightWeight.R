library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggjoy)
library(gganimate)
library(lubridate)
library(purrr)

# Load data

load.bios <- function(season, playerType){
  
  seasonID <- paste0(season-1,season)
  
  bios <- read.csv(paste0("HeightWeight/NHL",playerType,"Bios_",seasonID,".csv"),header=T,stringsAsFactors=F)
  
  return(bios)
  
}

player.bios <- map(2010:2017,load.bios,playerType="Player") %>%
  bind_rows %>%
  select(Name,Season,Height,Weight,Position) %>%
  rowwise %>%
  mutate(NewPosition = ifelse(Position == "D", "D", "F")) %>%
  ungroup

goalie.bios <- map(2010:2017,load.bios,playerType="Goalie") %>%
  bind_rows %>%
  select(Name,Season,Height,Weight,Position) %>%
  mutate(NewPosition="G")

bios <- rbind(player.bios,goalie.bios) %>%
  filter(!is.na(Height),!is.na(Weight)) %>%
  arrange(Season,Name) %>%
  mutate(Year = as.integer(substring(as.character(Season),5,8)))




# GIFs

p <- ggplot(bios, aes(x=Height,y=Weight,color=NewPosition,shape=NewPosition,frame=Year)) +
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


p2 <- ggplot(bios, aes(x=Height,y=Weight,frame=Year)) +
  facet_wrap(~NewPosition)+
  geom_jitter(size=5,alpha=0.7,color="blue",shape=16)+
  xlim(60,85) +
  xlab("Height (in.)")+
  ylim(120,275) +
  ylab("Weight (lb.)") +
  ggtitle("NHL Players Height and Weight: ") +
  theme_bw(16)

gg_animate(p2,"HeightWeight/HeightWeightFacet.gif")


reduced.bios <- bios %>%
  group_by(Year,NewPosition) %>%
  summarize(Players=n(),AvgHeight=mean(Height),AvgWeight=mean(Weight))

q <- ggplot(reduced.bios, aes(x=AvgHeight,y=AvgWeight,color=NewPosition,shape=NewPosition,size=Players,frame=Year)) +
  geom_jitter(alpha=0.7) +
  scale_size_continuous(name="Cohort Size",range=c(5,15)) +
  scale_color_manual(name="Position",values=c("blue","orange","purple")) +
  scale_shape_manual(name="Position",values=c(16,15,17)) +
  xlim(65,80) +
  xlab("Height (in.)")+
  ylim(150,250) +
  ylab("Weight (lb.)") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  ggtitle("NHL Players Height and Weight: ") +
  theme_bw(16)

gg_animate(q,"HeightWeight/HeightWeightAverage.gif")



# Joy Plots

p <- ggplot(goalie.bios, aes(x=Height,y=as.factor(Season),group=as.factor(Season))) +
  geom_joy(fill="blue",scale=2,alpha=0.5) +
  xlab("Height (in.)") +
  ylab("Season") +
  ggtitle("NHL Goaltender Height Distribution by Season",
          subtitle="All goaltenders who appeared in at least 1 game (not weighted by number of games played)\nData from NHL.com") +
  ggplot2::annotate("text",x=72,y=4,col="red",label=paste("@mathieubray",year(today())),alpha=0.1,cex=15,fontface="bold",angle=15) +
  theme_bw(16)
  
p

ggsave("HeightWeight/GoalieHeight.png",p)


g.bios <- bios %>% filter(NewPosition=="G")
d.bios <- bios %>% filter(NewPosition=="D")
f.bios <- bios %>% filter(NewPosition=="F")

p2 <- ggplot() +
  geom_joy(aes(y=as.factor(g.bios$Year),
               group=as.factor(g.bios$Year),
               x= g.bios$Height),
           fill="blue",scale=3,alpha=0.3) +
  geom_joy(aes(y=as.factor(d.bios$Year),
               group=as.factor(d.bios$Year),
               x= d.bios$Height),
           fill="red",scale=3,alpha=0.3) +
  geom_joy(aes(y=as.factor(f.bios$Year),
               group=as.factor(f.bios$Year),
               x= f.bios$Height),
           fill="yellow",scale=3,alpha=0.3) +
  theme_bw()


