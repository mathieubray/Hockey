library(plyr)
library(dplyr)
library(rvest)
library(tm)
library(stringr)

# Collect historical team statistics from College Hockey News

scrape.team.statistics <- function(season){
  
  # Helper function to convert height to total inches
  convert.height <- function(x){
    return(12*as.numeric(x[1])+as.numeric(x[2]))
  }

  # Only have statistics from 2005-2006 onward
  if (season >= 2006){
  
    # Set URL
    url <- paste0("http://www.collegehockeynews.com/stats/?season=",season-1,season)
    
    # Scrape raw team data
    raw.team.data <- url %>% 
      read_html %>% 
      html_nodes('.sortable') %>%
      html_table(header=F, fill=T)
    
    # Advanced stats only available from 2014-2015 season onward
    if (season < 2015){
      
      team.stats <- raw.team.data %>% 
        data.frame(stringsAsFactors=F)
      
      names(team.stats) <- team.stats[2,]
      
      team.stats <- team.stats[c(-1,-2),]
      team.stats$Ht <- sapply(strsplit(team.stats$Ht,"-"),convert.height)
      
    } else {
      standard.stats <- raw.team.data[[1]] %>% 
        data.frame(stringsAsFactors=F)
      
      names(standard.stats)<-standard.stats[2,]
      
      standard.stats <- standard.stats[c(-1,-2),]
      standard.stats$Ht <- sapply(strsplit(standard.stats$Ht,"-"),convert.height)
      
      advanced.stats <- raw.team.data[[2]] %>% 
        data.frame(stringsAsFactors=F)
      
      names(advanced.stats) <- advanced.stats[3,]
      names(advanced.stats)[9:14] <- paste0("ES",names(advanced.stats)[9:14])
      names(advanced.stats)[15:20] <- paste0("PP",names(advanced.stats)[15:20])
      names(advanced.stats)[21:26] <- paste0("Close",names(advanced.stats)[21:26])
      
      advanced.stats <- advanced.stats[c(-1,-2,-3),]
      
      team.stats <- inner_join(standard.stats,advanced.stats,by=c("Team","GP"))
    }
    
    team.stats$Team[which(team.stats$Team == "Alaska-Fairbanks")] <- "Alaska"
    team.stats$Team[which(team.stats$Team == "Mass.-Lowell")] <- "Massachusetts-Lowell"
    team.stats$Team[which(team.stats$Team == "American Int'l")] <- "American International"
    
    team.stats$Season <- paste0(season-1,season)
    
    team.stats <- team.stats %>% 
      arrange(Team)
    
    write.csv(team.stats,paste0("NCAA/Data/Statistics/",season-1,"-",season,"_Statistics.csv"),row.names=F)
    
  }
}
  
  
# Collect historical RPI information from College Hockey News

scrape.team.rpi <- function(season){
  
  # RPI only exists from season 2004-2005 onward
  if (season >= 2005){
  
    url <- paste0("http://www.collegehockeynews.com/ratings/rpi.php?s=",season-1,season)
    
    rpi.data <- url %>% 
      read_html %>% 
      html_nodes('.pw') %>%
      html_table(header=T, fill=F) %>%
      data.frame(stringsAsFactors = F) %>% 
      select(-W.L.T,-Rk)
    
    rpi.data$Team[which(rpi.data$Team=="Mass.-Lowell")] <- "Massachusetts-Lowell"
    rpi.data$Team[which(rpi.data$Team=="American Int'l")] <- "American International"
    
    rpi.data$Season <- paste0(season-1,season)
    
    rpi.data <- rpi.data %>% arrange(Team)
    
    write.csv(rpi.data,paste0("NCAA/Data/Statistics/",season-1,"-",season,"_RPI.csv"),row.names=F)
  }
}


# Collect historical conference information from Wikipedia (not working for 2017!)
# Should probably find a better source for these...

scrape.team.conferences <- function(season){
  
  keywords <- c("Atlantic","Big","independents","ECAC","East","National","Western","Central","America")
  conferences <- c("Atlantic Hockey","Big Ten","Independent","ECAC","Hockey East","NCHC","WCHA","CCHA","College Hockey America")
  
  if (season >= 2007){
    
    conference.table <- data.frame(Team=character(0),Conference=character(0),Season=character(0),stringsAsFactors=F)
     
    suffix <- paste0(season-2000)
    if (nchar(suffix) < 2){
       suffix<-paste0("0",suffix)
    }
    
    # Search wikipedia pages for tables, matching keywords to conferences
    url <- paste0("https://en.wikipedia.org/wiki/",season-1,"-",suffix,"_NCAA_Division_I_men's_ice_hockey_season")
    
    # Read table
    tables <- url %>% 
      read_html %>% 
      html_nodes('.wikitable')
    
    # Inspect first several tables (later tables include keywords in header and screw things up)
    for (i in 1:min(length(tables),9)){
      
      raw.table <- tables[[i]] %>%
        html_table(header=F, fill=T) %>%
        data.frame(stringsAsFactors = F)
      
      # Split table header into tokens, look for tokens in keyword list
      string.tokens <- scan_tokenizer(raw.table[1,1])
      token <- intersect(string.tokens,keywords)
      
      # If found...
      if (length(token) == 1){
        
        # Find matching conference
        conference <- conferences[match(token,keywords)]
        
        # Clean team names
        n <- nrow(raw.table)
        team.names <- raw.table[4:(n-1),1] %>%
          gsub(pattern="\\*",replacement="") %>%
          gsub(pattern="\\#",replacement="") %>%
          gsub(pattern="\\^",replacement="") %>%
          gsub(pattern="â€“",replacement="-") %>%
          gsub(pattern="â€",replacement="") %>%
          removeNumbers() %>%
          str_trim()
        
        # Append to master table
        conference.table <- rbind(conference.table,data.frame(Team=team.names,Conference=conference,Season=paste0(season-1,season),stringsAsFactors=F),stringsAsFactors=F)
      }
    }
    
    # Replace team names with standard team names
    conference.table$Team[which(conference.table$Team=="UMass")] <- "Massachusetts"
    conference.table$Team[which(conference.table$Team=="UMass Lowell")] <- "Massachusetts-Lowell"
    conference.table$Team[which(conference.table$Team=="Miami (OH)")] <- "Miami"
    conference.table$Team[which(conference.table$Team=="Omaha")] <- "Nebraska-Omaha"
    conference.table$Team[which(conference.table$Team=="Alaska Anchorage")] <- "Alaska-Anchorage"
    conference.table$Team[which(conference.table$Team=="Lake Superior State")] <- "Lake Superior"
    
    # Sort table
    conference.table <- conference.table %>% arrange(Team)
    
    write.csv(conference.table,paste0("Data/Statistics/",season-1,"-",season,"_Conferences.csv"),row.names=F)
    
  }
}


# Merge all files into one table

merge.team.information <- function(){
  
  files <- list.files("NCAA/Data/Statistics")
  
  statistics.files <- files[grepl("Statistics",files)]
  rpi.files <- files[grepl("RPI",files)]
  conference.files <- files[grepl("Conferences",files)]
  
  load.table<-function(file){
    return(read.csv(paste0("NCAA/Data/Statistics/",file),header=T,stringsAsFactors=F))
  }
  
  statistics.table <- rbind.fill(lapply(statistics.files,load.table))
  rpi.table <- rbind.fill(lapply(rpi.files,load.table))
  conference.table <- rbind.fill(lapply(conference.files,load.table))
  
  master.team.table <- full_join(full_join(conference.table,rpi.table,by=c("Team","Season")),statistics.table,by=c("Team","Season")) %>%
    arrange(Season,Team)
  
  write.csv(master.team.table,"NCAA/Data/Statistics/NCAATeamStats.csv",row.names=F)
  
}
  
  

