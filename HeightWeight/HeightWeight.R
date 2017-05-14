library(dplyr)
library(ggplot2)
library(ggthemes)


player.bios <- read.csv("HeightWeight/NHLPlayerBios.csv",header=T,stringsAsFactors=F) %>%
  select(Name,Season,Height,Weight,Position) %>%
  rowwise() %>%
  mutate(NewPosition = ifelse(Position == "D", "D", "F")) %>%
  ungroup()

goalie.bios <- read.csv("HeightWeight/NHLGoalieBios.csv",header=T,stringsAsFactors=F) %>%
  select(Name,Season,Height,Weight,Position) %>%
  mutate(NewPosition="G")

bios <- rbind(player.bios,goalie.bios) %>%
  filter(!is.na(Height),!is.na(Weight)) %>%
  arrange(Season,Name) %>%
  mutate(Year = as.integer(substring(as.character(Season),5,8)))


library(gganimate)

animation::ani.options(interval=0.5)

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

gganimate(p,"HeightWeight/HeightWeight.gif")



p2 <- ggplot(bios, aes(x=Height,y=Weight,frame=Year)) +
  facet_wrap(~NewPosition)+
  geom_jitter(size=5,alpha=0.7,color="blue",shape=16)+
  xlim(60,85) +
  xlab("Height (in.)")+
  ylim(120,275) +
  ylab("Weight (lb.)") +
  ggtitle("NHL Players Height and Weight: ") +
  theme_bw(16)

gganimate(p2,"HeightWeight/HeightWeightFacet.gif")



reduced.table <- bios %>%
  group_by(Year,NewPosition) %>%
  summarize(Players=n(),AvgHeight=mean(Height),AvgWeight=mean(Weight))

q <- ggplot(reduced.table, aes(x=AvgHeight,y=AvgWeight,color=NewPosition,shape=NewPosition,size=Players,frame=Year)) +
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

gganimate(q,"HeightWeight/HeightWeightAverage.gif")


