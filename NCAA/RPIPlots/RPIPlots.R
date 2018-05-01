library(ggplot2)
library(ggthemes)
library(purrr)
library(stringr)
library(readr)
library(mathieuR)

source("Logos/Logos.R")

logos <- load.logos(0.5,"NCAA")

logo.list <- logos$Logos
logo.color <- logos$LogoColors

conference.table <- list.files("NCAA/Data/Statistics",full.names=T) %>%
  str_subset("Conference") %>%
  map_df(read_csv)

rpi <- list.files("NCAA/Data/Statistics",full.names=T) %>%
  str_subset("RPI") %>%
  map_df(read_csv) %>%
  left_join(conference.table, by=c("Team","Season")) %>%
  filter(!is.na(Conference),!is.na(RPI), Conference != "Independent") %>%
  mutate(Season = as.numeric(substring(as.character(Season),5)))


# Extract conferences
conferences <- rpi$Conference %>% unique

# This takes longer than a loop...
plot.conference.rpi <- function(conference) {
  
   # Focus on specific conference
   conference.rpi <- rpi %>% 
     filter(Conference == conference)
    
   ggplot(data = conference.rpi, aes(x=Season,y=RPI,color=Team)) + 
     geom_line(aes(group=Team),size=1.2,alpha=0.5) +
     scale_color_manual(values=logo.color) +
     #theme_bw(18) +
     scale_x_continuous(breaks=2006:2018,limits=c(2006,2018)) +
     ylim(0.35,0.65) +
     #ggtitle(conference) +
     mapply(
       function(xx,yy,tt) {
         annotation_custom(logo.list[[tt]],xmin=xx-0.22, xmax=xx+0.22, ymin=yy-0.0075, ymax=yy+0.0075)
       }, conference.rpi$Season, conference.rpi$RPI, conference.rpi$Team
     ) +
     #annotate("text",x=2011.5,y=0.5,col="red",label="@mathieubray",alpha=0.1,cex=30,fontface="bold",angle=15) +
     guides(color=guide_legend(override.aes = list(alpha=1,size=5))) +
     theme_mathieu(base_size=18,
                   title.label = conference,
                   subtitle.label = "",
                   x.label = "Season",
                   y.label = "RPI",
                   x.watermark = 2011.5,
                   y.watermark = 0.5,
                   watermark.size = 20)
   
   # Change conference label to have no spaces
   conference.label <- gsub(conference,pattern=" ",replacement="_")
   
   print(paste0("Plotted ",conference))
   
   # Save
   ggsave(paste0("NCAA/RPI/",conference.label,".png"),height=8,width=12,units = "in")
}

# Draw plots
map(conferences,plot.conference.rpi)

