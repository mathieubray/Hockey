library(ggplot2)
library(ggthemes)

source("Logos/Logos.R")

logos <- load.logos(0.5,"NCAA")

logo.list <- logos$Logos
logo.color <- logos$LogoColors

# Open historical RPI file, filter out empty fields and independent teams
team.rpi <- read.csv("NCAA/Data/Statistics/NCAATeamStats.csv",header=T,stringsAsFactors=F) %>%
  filter(!is.na(Conference),!is.na(RPI), Conference != "Independent")

# Extract season year
team.rpi$Season <- as.numeric(substring(as.character(team.rpi$Season),5))


# Extract conferences
conferences <- as.list(unique(team.rpi$Conference))

# This takes longer than a loop...
plot.conference.rpi <- function(rpi.table, conference) {
  
   # Focus on specific conference
   conference.rpi <- rpi.table %>% filter(Conference == conference)
    
   ggplot(data = conference.rpi, aes(x=Season,y=RPI,color=Team)) + 
     geom_line(aes(group=Team),size=1.2,alpha=0.5) +
     scale_color_manual(values=logo.color) +
     theme_bw(18) +
     xlim(2007,2016) +
     ylim(0.35,0.65) +
     ggtitle(conference) +
     mapply(
       function(xx,yy,tt) {
         annotation_custom(logo.list[[tt]],xmin=xx-0.22, xmax=xx+0.22, ymin=yy-0.0075, ymax=yy+0.0075)
       }, conference.rpi$Season, conference.rpi$RPI, conference.rpi$Team
     ) +
     annotate("text",x=2011.5,y=0.5,col="red",label="@mathieubray",alpha=0.1,cex=30,fontface="bold",angle=15) +
     guides(color=guide_legend(override.aes = list(alpha=1,size=5)))
   
   # Change conference label to have no spaces
   conference.label <- gsub(conference,pattern=" ",replacement="_")
   
   # Save
   ggsave(paste0("NCAA/Projects/RPI/",conference.label,".png"),height=8,width=12,units = "in")
}

# Draw plots
lapply(conferences, plot.conference.rpi, rpi.table = team.rpi)

