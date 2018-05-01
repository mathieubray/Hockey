library(dplyr)
library(readr)
library(rvest)
library(magrittr)


command <- paste0("NCAA/Data/Scrapers/phantomjs NCAA/Data/Scrapers/scrape_rpi.js")

system(command)

raw.data <- read_html("NCAA/Data/Scrapers/rpi.html")

cells <- raw.data %>%
  html_nodes("td") %>%
  html_text 

rows <- raw.data %>%
  html_nodes("tr") %>%
  html_text %>% 
  tail(-2)

offset <- length(cells)/length(rows)

extract.data <- function(row.number, cells, offset){
  
  seq <- ((row.number-1)*offset + 1):((row.number-1)*offset + offset)
  
  row <- cells[seq]
  
  schedule.data <- row %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    set_colnames(c("QWBAdjustedRPIRank","Team","RPI","AdjustedRPI","QWB","QWBAdjustedRPI","WLT","Pct","PctRank","SOS","SOSRank"))
  
  return(schedule.data)
  
}

rpi.data <- map_df(1:length(rows), extract.data, cells=cells, offset=offset)

uscho <- rpi.data %>%
  mutate(Team = case_when(Team == "Alabama Huntsville" ~ "Alabama-Huntsville",
                          Team == "Alaska Anchorage" ~ "Alaska-Anchorage",
                          Team == "Minnesota Duluth" ~ "Minnesota-Duluth",
                          Team == "Omaha" ~ "Nebraska-Omaha",
                          Team == "UMass Lowell" ~ "Massachusetts-Lowell",
                          TRUE ~ Team)) %>%
  select(Team,Pct,SOS,RPI) %>%
  arrange(Team)

write_csv(uscho,"NCAA/SOS/USCHOValues_20172018.csv")








###########################################################


scrape.uscho <- function(tag=today()){
  
  # Create Directory
  directory <- paste0("NCAA/Projects/Season-To-Date/",tag)
  if(!dir.exists(directory)){
    dir.create(directory)
  }
  
  #url <- 'http://www.uscho.com/rankings/rpi/d-i-men'
  
  #rpi.raw <- url %>% 
  #read_html%>% 
  #html_nodes('table') %>%
  #html_table(header=T, fill=F) %>%
  #data.frame(stringsAsFactors = F) %>%
  #rename(AdjustedWinPct = Win.., QWBAdjustedRPI = QWB.Adj.RPI, AdjustedRPI = Adj.RPI) %>%
  #select(Team,RPI,AdjustedRPI,QWB,QWBAdjustedRPI,AdjustedWinPct,SOS) %>%
  #arrange(Team)
  
  command <- paste0("NCAA/Data/phantomjs NCAA/Data/scrape_rpi.js")
  
  system(command)
  
  raw.data <- read_html("NCAA/Data/RPI.html")
  
  cells <- raw.data %>%
    html_nodes("td") %>%
    html_text %>%
    head(660)
  
  extract.data <- function(row.number, cells){
    
    seq <- ((row.number-1)*11 + 1):((row.number-1)*11 + 11)
    
    row <- cells[seq]
    
    schedule.data <- row %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      set_colnames(c("Rank","Team","RPI","AdjustedRPI","QWB","QWBAdjustedRPI","WLT","AdjustedWinPct","WinPctRank","SOS","SOSRank"))
    
    return(schedule.data)
    
  }
  
  rpi.raw <- map_df(1:60, extract.data, cells=cells) %>%
    select(Team,RPI,AdjustedRPI,QWB,QWBAdjustedRPI,AdjustedWinPct,SOS) %>%
    arrange(Team)
  
  #url <- 'http://www.uscho.com/rankings/pairwise-rankings/d-i-men/'
  
  #pw.raw <- url %>% 
  # read_html %>% 
  #html_nodes('table') %>%
  #html_table(header=T, fill=F) %>%
  #data.frame(stringsAsFactors = F) %>%
  #rename(PairwiseComparisonsWon = PWR, UnadjustedWinPct = Win..) %>%
  #select(Team,PairwiseComparisonsWon,UnadjustedWinPct)
  
  command <- paste0("NCAA/Data/phantomjs NCAA/Data/scrape_pw.js")
  
  system(command)
  
  raw.data <- read_html("NCAA/Data/PW.html")
  
  cells <- raw.data %>%
    html_nodes("td") %>%
    html_text %>%
    head(480)
  
  extract.data <- function(row.number, cells){
    
    seq <- ((row.number-1)*8 + 1):((row.number-1)*8 + 8)
    
    row <- cells[seq]
    
    schedule.data <- row %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      set_colnames(c("Rank","Team","PairwiseComparisonsWon","WLT","UnadjustedWinPct","WinPctRank","RPI","RPIRank"))
    
    return(schedule.data)
    
  }
  
  pw.raw <- map_df(1:60, extract.data, cells=cells) %>%
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
