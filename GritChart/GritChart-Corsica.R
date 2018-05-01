library(ggplot2)
library(ggthemes)
library(png)
library(grid)
library(dplyr)
library(lubridate)

img <- readPNG("GritChart/images/rink-cropped.png")

hits <- read.csv("GritChart/data/hits-corsica.csv",header=T,stringsAsFactors=F)

player <- "RYAN.GETZLAF"

player.hits <- hits %>% 
  filter(Player == player,
         !is.na(X),
         !is.na(Y))

ggplot(data=player.hits,aes(x=X,y=Y,fill=Zone)) + 
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")),-100, 100, -42, 42) +
  geom_point(size=6,alpha=0.8,pch=21,color="black") + 
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_few() +
  scale_fill_manual(name="Zone",values=c("blue","green","purple")) +
  theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+
  ggplot2::annotate("text",x=0,y=0,col="red",label=paste0("@mathieubray ",year(today())),alpha=0.15,cex=20,fontface="bold",angle=30)

