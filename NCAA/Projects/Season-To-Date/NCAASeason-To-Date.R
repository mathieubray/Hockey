library(ggplot2)
library(ggthemes)

source("NCAA/NCAAFunctions.R")
source("NCAA/Data/NCAAScheduleScraper.R")

compile.current.season <- function(season.to.date=today(), tag=today(), scrape=T){

  if (month(season.to.date) < 7){
    season <- year(season.to.date)
  } else {
    season <- year(season.to.date) + 1
  }
  
  # Create Directory
  directory <- paste0("NCAA/Projects/Season-To-Date/",tag)
  if(!dir.exists(directory)){
    dir.create(directory)
  }

  raw.schedule.file <- paste0("NCAA/Data/Schedules/",season-1,"-",season,"_Clean.csv")
  
  # Scrape schedule from USCHO.com
  if(scrape | !file.exists(raw.schedule.file)){
    scrape.raw.schedule(season)
    clean.schedule(season)
  }
  
  season.schedule.raw <- read.csv(raw.schedule.file,header=T,stringsAsFactors=F)
  
  # Clean it, and calculate record and game results
  season.schedule <- season.schedule.raw %>% filter(!is.na(HomeScore))
  write.csv(season.schedule,paste0(directory,"/Schedule.csv"),row.names=F) # Save Schedule File to Current Folder
  print("Schedule Saved")
  
  season.results <- compile.results(season.schedule)
  write.csv(season.results,paste0(directory,"/Results.csv"),row.names=F) # Compile Individual Game Results to Current Folder
  print("Results Compiled and Saved")
  
  season.record <- compile.record(season.results) # Calculate Team Record
  expanded.record <- compile.rpi(season.record,season.results) # Expand Record to Include RPI Information
  write.csv(expanded.record,paste0(directory,"/Record.csv"),row.names=F) # Save Team Record
  print("Record Compiled and Saved")
  
}

scrape.uscho <- function(tag=today()){
  
  # Create Directory
  directory <- paste0("NCAA/Projects/Season-To-Date/",tag)
  if(!dir.exists(directory)){
    dir.create(directory)
  }
  
  url <- 'http://www.uscho.com/rankings/rpi/d-i-men'
  
  rpi.raw <- url %>% 
    read_html%>% 
    html_nodes('table') %>%
    html_table(header=T, fill=F) %>%
    data.frame(stringsAsFactors = F) %>%
    rename(AdjustedWinPct = Win.., QWBAdjustedRPI = QWB.Adj.RPI, AdjustedRPI = Adj.RPI) %>%
    select(Team,RPI,AdjustedRPI,QWB,QWBAdjustedRPI,AdjustedWinPct,SOS) %>%
    arrange(Team)
  
  url <- 'http://www.uscho.com/rankings/pairwise-rankings/d-i-men/'
  
  pw.raw <- url %>% 
    read_html %>% 
    html_nodes('table') %>%
    html_table(header=T, fill=F) %>%
    data.frame(stringsAsFactors = F) %>%
    rename(PairwiseComparisonsWon = PWR, UnadjustedWinPct = Win..) %>%
    select(Team,PairwiseComparisonsWon,UnadjustedWinPct)
  
  rpi.info <- inner_join(rpi.raw,pw.raw,by="Team") %>%
    mutate(GamesDropped = grepl(pattern="\\*",AdjustedRPI)) %>%
    mutate(QWBAdjRPIRank = dense_rank(desc(QWBAdjustedRPI))) %>%
    mutate(RPIRank = dense_rank(desc(RPI))) %>%
    mutate(SOSRank = dense_rank(desc(SOS))) %>%
    mutate(WinRank = dense_rank(desc(AdjustedWinPct)))
  
  rpi.info$Team <- ifelse(rpi.info$Team =="Omaha","Nebraska-Omaha",rpi.info$Team)
  
  rpi.info.final <- rpi.info %>% arrange(Team)
  
  write.csv(rpi.info.final,paste0(directory,"/USCHO.csv"),row.names=F)
  print(paste("USCHO Information Scraped for ",today()))

}
  
check.rpi.calculations <- function(tag=today()){

  directory <- paste0("NCAA/Projects/Season-To-Date/",tag)
  
  if(!file.exists(paste0(directory,"/Record.csv")) | !file.exists(paste0(directory,"/USCHO.csv"))){
    print(paste0("Files Missing for ",tag))
    
    return()
  }

  record <- read.csv(paste0(directory,"/Record.csv"),header=T,stringsAsFactors=F) %>% arrange(Team)
  uscho <- read.csv(paste0(directory,"/USCHO.csv"),header=T,stringsAsFactors=F) %>% arrange(Team)
  
  # Check Adjusted Winning Pcts
  paste0("Successful Checks: ", length(round(record$AdjPct,4)==uscho$AdjustedWinPct),"/",nrow(record))

  # Check Comparison Values
  record.values <- record %>% 
    select(Team,AdjPct,SOS,RPI) %>%
    rename(AdjustedWinPct = AdjPct) %>%
    gather(key=Variable,value=Calculated,-Team)

  uscho.values <- uscho %>% 
    select(Team,AdjustedWinPct,SOS,QWBAdjustedRPI) %>%
    rename(RPI = QWBAdjustedRPI) %>%
    gather(key=Variable,value=Benchmark,-Team)

  comparison.data.set <- inner_join(record.values,uscho.values,by=c("Team","Variable"))

  comparison.grouped <- comparison.data.set %>%
    group_by(Variable) %>%
    summarize(Label = r.function(Benchmark,Calculated))

  comparison.data.merged <- inner_join(comparison.data.set,comparison.grouped,by="Variable")
  
  ggplot(data=comparison.data.merged,aes(y=Benchmark,x=Calculated,label=Label)) + 
    facet_wrap(~Variable) +
    geom_point(size=3,alpha=0.5) +
    geom_smooth(method="lm",se=F,size=0.5) +
    xlab("Calculated") +
    ylab("Benchmark") +
    theme_bw(18) +
    geom_text(x=0.45,y=0.6,parse=T)

  ggsave(paste0(directory,"/Tests.png"),height=8,width=16,units="in")
  
  print("Test Diagnostics Printed")

}
