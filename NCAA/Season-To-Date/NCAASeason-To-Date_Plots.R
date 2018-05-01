library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(lubridate)
library(mathieuR)

source("Logos/Logos.R")
source("NCAA/NCAAFunctions.R")

logos <- load.logos(0.7,"NCAA")
logo.list <- logos$Logos
logo.colors <- logos$LogoColors

# Fix up data to plot

tag <- "2017-2018"

record <- read_csv(paste0("NCAA/Season-To-Date/",tag,"/Record.csv"))

plot.data <- record %>% 
  select(Team,Pct:Pairwise) %>%
  arrange(desc(Pairwise),desc(RPI)) %>%
  mutate(PairwiseRank = 1:60,
         Top10 = PairwiseRank <= 10)

top10 <- plot.data %>%
  filter(Top10)

plot.parameters <- list(
  
  Pct = list(val = min(top10$Pct) - 0.005,
                label = "Unadjusted Win Pct.",
                column = plot.data$Pct,
                stretch = 0.015),
  
  AdjPct = list(val = min(top10$AdjPct) - 0.005,
                   label = "Adjusted Win Pct.",
                   column = plot.data$AdjPct,
                   stretch = 0.015),
  
  OppWinPct = list(val = min(top10$OppWinPct) - 0.001,
                   label = "Opponent's Win Pct.",
                   column = plot.data$OppWinPct,
                   stretch = 0.005),
  
  OppOppWinPct = list(val = min(top10$OppOppWinPct) - 0.001,
                      label = "Opponent's Opponent's Win Pct.",
                      column = plot.data$OppOppWinPct,
                      stretch = 0.005),
  
  SOS = list(val = min(top10$SOS) - 0.001,
             label = "Strength of Schedule",
             column = plot.data$SOS,
             stretch = 0.005),
  
  RPI = list(val = min(top10$RPI) - 0.001,
             label = "Unadjusted RPI",
             column = plot.data$RPI,
             stretch = 0.005),
  
  PairwiseRank = list(val = 10.1,
                label = "Pairwise Rank",
                column = plot.data$PairwiseRank,
                stretch = 1)
)


ncaa.plot <- function(x.var,y.var,plot.params = plot.parameters){
  
  if (x.var == "PairwiseRank"){
    temp <- x.var
    x.var <- y.var
    y.var <- temp
  }
  
  x.params <- plot.params[[x.var]]
  y.params <- plot.params[[y.var]]
  
  x.val <- x.params$val
  x.label <- x.params$label
  x.column <- x.params$column
  x.stretch <- x.params$stretch
  
  y.val <- y.params$val
  y.label <- y.params$label
  y.column <- y.params$column
  y.stretch <- y.params$stretch
  
  if (x.var == "PairwiseRank" | y.var == "PairwiseRank"){
    
    p <- ggplot(data=plot.data,aes_string(x=x.var,y=y.var)) + 
      geom_point(size=0.01) +
      geom_vline(xintercept=x.val,color="red",linetype="dotted") +
      geom_hline(yintercept=y.val,color="red",linetype="dotted") +
      annotate("rect",xmin=x.val,xmax=Inf,ymin=y.val,ymax=Inf,fill="yellow",alpha=0.2) +
      annotate("rect",xmin=x.val,xmax=-Inf,ymin=y.val,ymax=Inf,fill="red",alpha=0.1) +
      annotate("rect",xmin=x.val,xmax=Inf,ymin=y.val,ymax=-Inf,fill="green",alpha=0.1) +
      annotate("rect",xmax=x.val,xmin=-Inf,ymax=y.val,ymin=-Inf,fill="yellow",alpha=0.2) +
      scale_y_continuous(trans = "reverse", breaks=1:60) +
      mapply(
        function(xx,yy,tt) {
          annotation_custom(logo.list[[tt]],xmin=xx-x.stretch, xmax=xx+x.stretch, ymin=yy-y.stretch, ymax=yy+y.stretch)
        }, x.column,(-1) * y.column, plot.data$Team
      ) +
      geom_label_repel(aes(label=Team),size=3,alpha=0.5) +
      theme_mathieu(base_size = 15,
                    title.label = paste0(tag,": ",x.label," vs. ",y.label),
                    subtitle.label = "",
                    x.label = x.label,
                    y.label = y.label,
                    x.watermark = (max(x.column) + min(x.column)) / 2,
                    y.watermark = 30,
                    watermark.size = 30)
    
  } else {
    
    x.stretch <- x.params$stretch
    y.stretch <- y.params$stretch
    
    p <- ggplot(data=plot.data,aes_string(x=x.var,y=y.var)) + 
      geom_point(size=0.01) +
      geom_smooth(method="lm",se=F,size=0.5) +
      geom_vline(xintercept=x.val,color="red",linetype="dotted") +
      geom_hline(yintercept=y.val,color="red",linetype="dotted") +
      annotate("rect",xmin=x.val,xmax=Inf,ymin=y.val,ymax=Inf,fill="green",alpha=0.1) +
      annotate("rect",xmin=x.val,xmax=-Inf,ymin=y.val,ymax=Inf,fill="yellow",alpha=0.2) +
      annotate("rect",xmin=x.val,xmax=Inf,ymin=y.val,ymax=-Inf,fill="yellow",alpha=0.2) +
      annotate("rect",xmax=x.val,xmin=-Inf,ymax=y.val,ymin=-Inf,fill="red",alpha=0.1) +
      mapply(
        function(xx,yy,tt) {
          annotation_custom(logo.list[[tt]],xmin=xx-x.stretch, xmax=xx+x.stretch, ymin=yy-y.stretch, ymax=yy+y.stretch)
        }, x.column, y.column, plot.data$Team
      ) +
      geom_label_repel(aes(label=Team),size=3,alpha=0.5) +
      theme_mathieu(base_size = 15,
                    title.label = paste0(tag,": ",y.label," vs. ",x.label," (",r.function(x.column, y.column),")"),
                    subtitle.label = "",
                    x.label = x.label,
                    y.label = y.label,
                    x.watermark = (max(x.column) + min(x.column)) / 2,
                    y.watermark = (max(y.column) + min(y.column)) / 2,
                    watermark.size = 30)
    
  } 
  
  return(p)
}
