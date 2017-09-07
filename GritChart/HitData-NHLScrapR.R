library(dplyr)
library(ggplot2)
library(ggthemes)
library(png)
library(grid)
library(lubridate)

img <- readPNG("GritChart/images/rink-cropped.png")

# Load NHLScrapr data

load("Data/Scraped/nhlscrapr20142015.RData")


# Collect Player Information

nhl.players <- nhlscrapr.20142015.roster %>%
  filter(pG==0) %>% # Remove Goalies
  select(player.id,firstlast)  

nhl.players <- nhl.players[-1,] # Remove first row


# Collect Hit Information

zone.function <- function(ev.team,hometeam,homezone){
  
  eventzone<-"Neu"
  
  if (as.character(ev.team)==as.character(hometeam)){
    if (homezone == "Off"){
      eventzone<-"Off"
    } else {
      if (homezone == "Def"){
        eventzone<-"Def"
      }
    }
  } else {
    if (homezone == "Off"){
      eventzone<-"Def"
    } else {
      if (homezone == "Def"){
        eventzone<-"Off"
      }
    }
  }
  
  return(eventzone)
}

opposing.team.function <- function(ev.team,hometeam,awayteam){
  
  return(ifelse(ev.team==hometeam,awayteam,hometeam))
  
}

multiplier.function <- function(hometeam,ev.team,refdate){
  
  multiplier<-1
  
  switchTeams<-c("CAR","CGY","CHI","COL","DAL","DET","EDM","FLA","L.A","MIN","MTL","NYI","PHI","TOR","WPG","WSH")

  if (hometeam %in% switchTeams){
    if (refdate <= 4738){
      multiplier<- -1
    } 
  } else {
    if (hometeam == "CBJ"){
      if (refdate>=4743 & refdate<=4747){
        multiplier <- -1
      } 
    } else {
      if (hometeam == "VAN"){
        if (refdate<=4753 | refdate == 4757){
          multiplier <- -1
        } 
      }
    }
  }
  
  if (as.character(hometeam)!=as.character(ev.team)){
    multiplier <- multiplier * (-1)
  }
  
  return (multiplier)
}
 

nhl.hits <- nhlscrapr.20142015.pbp %>%
  filter(etype=="HIT") %>%
  select(period,refdate,ev.team,ev.player.1,ev.player.2,
         homezone,awayteam,hometeam,xcoord,ycoord)
  
  
  
nhl.hits$multiplier <- mapply(multiplier.function,hometeam=nhl.hits$hometeam,ev.team=nhl.hits$ev.team,refdate=nhl.hits$refdate)
nhl.hits$eventzone <- mapply(zone.function,ev.team=nhl.hits$ev.team,hometeam=nhl.hits$hometeam,homezone=nhl.hits$homezone)
nhl.hits$opposingteam <- mapply(opposing.team.function,ev.team=nhl.hits$ev.team,hometeam=nhl.hits$hometeam,awayteam=nhl.hits$awayteam)  
nhl.hits$x = ifelse(nhl.hits$period==2 | nhl.hits$period ==4,
                          (-1)*nhl.hits$xcoord*nhl.hits$multiplier,
                          nhl.hits$xcoord*nhl.hits$multiplier)
nhl.hits$y = ifelse(nhl.hits$period==2 | nhl.hits$period ==4,
                          (-1)*nhl.hits$ycoord*nhl.hits$multiplier,
                          nhl.hits$ycoord*nhl.hits$multiplier)

nhl.hits$refdate <- as.Date("2002-01-01") + as.numeric(nhl.hits$refdate)

nhl.hits.final <- nhl.hits %>%
  left_join(nhl.players,by=c("ev.player.1" = "player.id")) %>%
  dplyr::rename(Hitter = firstlast) %>%
  left_join(nhl.players,by=c("ev.player.2" = "player.id")) %>%
  dplyr::rename(Hittee=firstlast) %>%
  select(-xcoord,-ycoord,-ev.player.1,-ev.player.2,-hometeam,-awayteam,-multiplier,-homezone) %>%
  dplyr::rename(Period=period,Date=refdate,Team=ev.team,Zone=eventzone,X=x,Y=y,OpposingTeam=opposingteam,Player=Hitter,OpposingPlayer=Hittee) %>%
  select(Date,Period,Team,Player,OpposingTeam,OpposingPlayer,Zone,X,Y)
  

write.csv(nhl.hits.final,"GritChart/data/hits.csv",row.names=F)


# Plot Hits

hit.table <- read.csv("GritChart/data/hits.csv",header=T,stringsAsFactors=F)

hit.subtable <- hit.table %>% filter(Player == "AARON EKBLAD")

ggplot(data=hit.subtable,aes(x=X,y=Y,fill=Zone)) + 
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")),-100, 100, -42, 42) +
  geom_point(size=6,alpha=0.8,pch=21,color="black") + 
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_few() +
  scale_fill_manual(name="Zone",values=c("blue","green","purple")) +
  theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+
  ggplot2::annotate("text",x=0,y=0,col="red",label="MATHIEU BRAY 2015",alpha=0.15,cex=20,fontface="bold",angle=30)

