library(ggplot2)
library(ggthemes)
library(png)
library(grid)
library(dplyr)
library(lubridate)

# Load Corsica data

load("../Data/Corsica/pbp20152016.RDa")

img <- readPNG("GritChart/images/rink-cropped.png")


# Collect Player Names

a1 <- pbp %>%
  select(a1.num,a1.pos) %>%
  filter(a1.pos != "G") %>%
  .$a1.num %>%
  unique

a2 <- pbp %>%
  select(a2.num,a2.pos) %>%
  filter(a2.pos != "G") %>%
  .$a2.num %>%
  unique

a3 <- pbp %>%
  select(a3.num,a3.pos) %>%
  filter(a3.pos != "G") %>%
  .$a3.num %>%
  unique

a4 <- pbp %>%
  select(a4.num,a4.pos) %>%
  filter(a4.pos != "G") %>%
  .$a4.num %>%
  unique

a5 <- pbp %>%
  select(a5.num,a5.pos) %>%
  filter(a5.pos != "G") %>%
  .$a5.num %>%
  unique

a6 <- pbp %>%
  select(a6.num,a6.pos) %>%
  filter(a6.pos != "G") %>%
  .$a6.num %>%
  unique

h1 <- pbp %>%
  select(h1.num,h1.pos) %>%
  filter(h1.pos != "G") %>%
  .$h1.num %>%
  unique

h2 <- pbp %>%
  select(h2.num,h2.pos) %>%
  filter(h2.pos != "G") %>%
  .$h2.num %>%
  unique

h3 <- pbp %>%
  select(h3.num,h3.pos) %>%
  filter(h3.pos != "G") %>%
  .$h3.num %>%
  unique

h4 <- pbp %>%
  select(h4.num,h4.pos) %>%
  filter(h4.pos != "G") %>%
  .$h4.num %>%
  unique

h5 <- pbp %>%
  select(h5.num,h5.pos) %>%
  filter(h5.pos != "G") %>%
  .$h5.num %>%
  unique

h6 <- pbp %>%
  select(h6.num,h6.pos) %>%
  filter(h6.pos != "G") %>%
  .$h6.num %>%
  unique

players <- c(a1,a2,a3,a4,a5,a6,h1,h2,h3,h4,h5,h6) %>% 
  unique %>%
  sort

data.frame(Players = players) %>%
  write.csv("GritChart/data/players.csv",row.names=F)


# Collect Player Hit Data

zone.function <- function(event.team,event.zone,home.team){
  
  correct.zone <- event.zone
  
  if (home.team != event.team){
    if (event.zone == "Off"){
      correct.zone <- "Def"
    } else if (event.zone == "Def"){
      correct.zone <- "Off"
    }
  }
  
  return(correct.zone)
}

multiplier.function <- function(event.team,home.team){
  
  if (as.character(event.team) != as.character(home.team)){
    return(-1)
  } else {
    return(1)
  }
}

nhl.hits <- pbp %>%
  filter(Event=="HIT",!is.na(Home.Zone)) %>%
  select(Period,Date,ev.team,p1,p2,Home.Team,Away.Team,Home.Zone,XC,YC) %>%
  rename(Team=ev.team,Player=p1,Victim=p2,HomeTeam=Home.Team,AwayTeam=Away.Team,Zone=Home.Zone,X=XC,Y=YC) %>%
  mutate(Date=ymd(Date),
         OpposingTeam=if_else(Team==HomeTeam,AwayTeam,HomeTeam)) %>%
  select(Date,Period,Team,Player,OpposingTeam,Victim,Zone,X,Y,HomeTeam) %>%
  rowwise %>%
  mutate(Zone=zone.function(Team,Zone,HomeTeam),
         Multiplier=multiplier.function(Team,HomeTeam)) %>%
  ungroup %>%
  mutate(X = Multiplier * as.numeric(X),
         Y = Multiplier * as.numeric(Y))

write.csv(nhl.hits,"GritChart/data/hits-corsica.csv",row.names=F)


# Plot Hits

nhl.hits <- read.csv("GritChart/data/hits-corsica.csv",header=T,stringsAsFactors=F)

player <- "RYAN.GETZLAF"

player.hits <- nhl.hits %>% 
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
  ggplot2::annotate("text",x=0,y=0,col="red",label=paste0("MATHIEU BRAY ",year(today())),alpha=0.15,cex=20,fontface="bold",angle=30)

