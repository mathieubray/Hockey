library(ggplot2)
library(viridis)


tag <- as.character(today())
directory <- paste0("NCAA/Projects/Simulations/",tag)

historical.stats <- read.csv("NCAA/Data/Statistics/NCAATeamStats.csv",header=T,stringsAsFactors=F)


### Compare Methods

xG.results <- read.csv(paste0("NCAA/Projects/Simulations/",tag,"/xG.csv"), header=T, stringsAsFactors = F)
tiers.results <- read.csv(paste0("NCAA/Projects/Simulations/",tag,"/Tiers.csv"), header=T, stringsAsFactors = F)
rpi.results <- read.csv(paste0("NCAA/Projects/Simulations/",tag,"/RPI.csv"), header=T, stringsAsFactors = F)


simulation.data <- rbind(xG.results,tiers.results,rpi.results)

simulation.averages <- simulation.data %>% 
  group_by(Method) %>%
  summarize(Mean = mean(RPI), SD = sd(RPI))

simulation.data <- inner_join(simulation.data, simulation.averages)

target <- historical.stats %>%
  filter(Team=="Michigan",Season=="20162017") %>%
  .$RPI %>%
  head(1)

ggplot(data=simulation.data,aes(x=RPI)) + 
  geom_histogram(fill="blue",alpha=0.3,color="black",binwidth=0.005,position="identity") +
  facet_wrap(~Method)+
  xlab("RPI") +
  ylab("Number of Simulated Seasons") +
  ggtitle("What would Michigan's 2016-2017 season look like if repeated over and over?") +
  scale_x_continuous(limits=c(0.4,0.7),breaks=seq(0.4,0.7, by=0.025)) +
  geom_segment(aes(x=target,xend=target,y=0,yend=Inf), color= "red",size=1.2, linetype="longdash")+
  geom_segment(aes(x=Mean,xend=Mean,y=0,yend=Inf),color="blue",linetype="longdash",size=1.2) +
  geom_text(aes(x=0.675,y=30,label=paste0("Mean: ", round(Mean,4))),size=6,color="blue") +
  geom_text(aes(x=0.425,y=30,label=paste0("Target: ", round(target,4))), size=6,color="red") +
  theme_bw(16)

ggsave(paste0(directory,"/Plot.png"),height=8,width=20,units="in")


### Compare Fixed Win Probabilities

fixed.results <- read.csv(paste0("NCAA/Projects/Simulations/",tag,"/Fixed.csv"), header=T, stringsAsFactors = F)

season.rpi  <-  historical.stats %>%
  filter(Season=="20162017") %>% 
  arrange(desc(RPI))

max.rpi <- season.rpi$RPI[1]
top10.rpi <- season.rpi$RPI[10]
top16.rpi <- season.rpi$RPI[16]

ggplot(data=sim.data,aes(x=RPI,fill=as.factor(Probability))) + 
  geom_histogram(alpha=0.5,color="black",binwidth=0.005,position="identity") +
  scale_x_continuous(limits=c(0.35,0.7),breaks=seq(0.3,0.8, by=0.025)) +
  scale_fill_viridis(name="Win Probability",discrete=T) +
  ylab("Number of Simulated Seasons") +
  geom_segment(aes(x=max.rpi,xend=max.rpi,y=0,yend=Inf), color= "red",size=1.2, linetype="longdash") +
  geom_text(aes(x=max.rpi,y=-2,label=paste0("Max RPI: ", round(max.rpi,4))), size=4,color="red") +
  geom_segment(aes(x=top10.rpi,xend=top10.rpi,y=0,yend=Inf), color= "red",size=1.2, linetype="longdash") +
  geom_text(aes(x=top10.rpi,y=-2,label=paste0("Top 10 RPI: ", round(top10.rpi,4))), size=4,color="red") +
  geom_segment(aes(x=top16.rpi,xend=top16.rpi,y=0,yend=Inf), color= "red",size=1.2, linetype="longdash") +
  geom_text(aes(x=top16.rpi,y=-4,label=paste0("Top 16 RPI: ", round(top16.rpi,4))), size=4,color="red") +
  xlab("RPI") +
  ggtitle("How Last Season Looks Under Different Win Probabilities") +
  theme_bw(16)

ggplot(data=sim.data,aes(x=RPI,fill=as.factor(Probability))) + 
  geom_density(alpha=0.5,color="black",position="identity",adjust=1.2) +
  scale_x_continuous(limits=c(0.35,0.7),breaks=seq(0.3,0.8, by=0.025)) +
  scale_y_continuous(breaks=NULL) +
  scale_fill_viridis(name="Win Probability",discrete=T) +
  geom_segment(aes(x=max.rpi,xend=max.rpi,y=0,yend=Inf), color= "red",size=1.2, linetype="longdash") +
  geom_text(aes(x=max.rpi,y=-2,label=paste0("Max RPI: ", round(max.rpi,4))), size=4,color="red") +
  geom_segment(aes(x=top10.rpi,xend=top10.rpi,y=0,yend=Inf), color= "red",size=1.2, linetype="longdash") +
  geom_text(aes(x=top10.rpi,y=-2,label=paste0("Top 10 RPI: ", round(top10.rpi,4))), size=4,color="red") +
  geom_segment(aes(x=top16.rpi,xend=top16.rpi,y=0,yend=Inf), color= "red",size=1.2, linetype="longdash") +
  geom_text(aes(x=top16.rpi,y=-4,label=paste0("Top 16 RPI: ", round(top16.rpi,4))), size=4,color="red") +
  xlab("RPI") +
  ylab("Density") +
  ggtitle("How Last Season Looks Under Different Win Probabilities") +
  theme_bw(16)
