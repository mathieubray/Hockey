library(dplyr)
library(rvest)
library(tm)
library(stringr)
library(purrr)

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
    rpi.data$Team[which(rpi.data$Team=="")] <- "Wayne State"
    
    rpi.data$Season <- paste0(season-1,season)
    
    rpi.data <- rpi.data %>% arrange(Team)
    
    write.csv(rpi.data,paste0("NCAA/Data/Statistics/",season-1,"-",season,"_RPI.csv"),row.names=F)
  }
}


# Collect historical conference information from Wikipedia (not working for 2017!)
# Should probably find a better source for these...

scrape.team.conferences <- function(season){
  
  tag <- paste0(substr(as.character(season-1),3,4),substr(as.character(season),3,4))
  
  season.tag <- paste0(season-1,season)
  
  if (season >= 2015){
    url <- paste0("http://collegehockeystats.net/",tag,"/textstats/d1m")
    
    tables <- url %>% 
      read_html %>%
      html_nodes("table") %>%
      `[`(2) %>%
      html_table(header=F,fill=T) %>%
      `[[`(1)
    
  } else {
    url <- paste0("http://collegehockeystats.net/",tag,"/teamstats")
    
    tables <- url %>% 
      read_html %>%
      html_nodes("table") %>%
      `[`(2) %>%
      html_table(header=F,fill=T) %>%
      `[[`(1)
  }
  
  if (season >= 2015){
    subtables <- list(tables$X1[1:2],tables$X1[3:4],tables$X1[5:6],
                      tables$X2[1:2],tables$X2[3:4],tables$X2[5:6],
                      tables$X3[1:2],tables$X3[3:4],tables$X3[5:6])
  } else if (season %in% 2007:2014){
    subtables <- list(tables$X1[1:2],tables$X1[3:4],tables$X1[5:6],
                      tables$X2[1:2],tables$X2[3:4],
                      tables$X3[1:2],tables$X3[3:4])
  } else  {
    subtables <- list(tables$X1[1:2],tables$X1[3:4],tables$X1[5:6],
                      tables$X2[1:2],tables$X2[3:4],tables$X2[5:6],
                      tables$X3[1:2],tables$X3[3:4])
  }
  
  clean.subtable <- function(subtable){
    
    subtable[1] -> conference
    subtable[2] -> teams
    
    clean.conference <- conference %>%
      gsub(pattern = " Men", replacement = "", fixed=T)
    
    clean.teams <- teams %>%
      gsub(pattern = "-",replacement = "@") %>%
      gsub(pattern = "RIT", replacement = "Rit",fixed=T) %>%
      gsub(pattern = "UMass", replacement = "Umass", fixed=T) %>%
      gsub(pattern = " ", replacement = "@") %>%
      gsub(pattern = '([[:upper:]])', replacement=' \\1') %>%
      gsub(pattern = "@ ",replacement="@",fixed=T) %>%
      strsplit(split= " ")%>%
      unlist %>%
      gsub(pattern = "@", replacement = " ") %>% 
      gsub(pattern = "Rit", replacement = "RIT",fixed=T) %>%
      gsub(pattern = "Umass", replacement = "UMass", fixed=T)
    
    conference.table <- data.frame(Conference = clean.conference, Team = clean.teams, stringsAsFactors=F) %>%
      filter(Team != "")
    
    return(conference.table)
  }
  
  
  conference.table <- map(subtables,clean.subtable) %>% 
    bind_rows() %>%
    filter(!is.na(Team), Conference != "Ivy League") %>%
    mutate(Season = season.tag) %>%
    select(Team, Conference, Season) %>%
    mutate(Conference = case_when(Conference == "Men's Independent" ~ "Independent",
                                  Conference == "Men's Division I Independent" ~ "Independent",
                                  Conference == "Men's Division I Independents" ~ "Independent",
                                  Conference == "ECAC Hockey" ~ "ECAC",
                                  Conference == "ECAC Hockey League" ~ "ECAC",
                                  Conference == "National Collegiate Hockey Conference" ~ "NCHC",
                                  T ~ Conference),
           Team = case_when(Team == "Alabama Huntsville" ~ "Alabama-Huntsville",
                            Team == "Alaska Anchorage" ~ "Alaska-Anchorage",
                            Team == "Alaska Fairbanks" ~ "Alaska",
                            Team == "Army West Point" ~ "Army",
                            Team == "Lake Superior State" ~ "Lake Superior",
                            Team == "Minnesota Duluth" ~ "Minnesota-Duluth",
                            Team == "Omaha" ~ "Nebraska-Omaha",
                            Team == "Nebraska Omaha" ~ "Nebraska-Omaha",
                            Team == "UMass Lowell" ~ "Massachusetts-Lowell",
                            T ~ Team)) %>%
    arrange(Team)
  
  
  write.csv(conference.table,paste0("NCAA/Data/Statistics/",season-1,"-",season,"_Conferences.csv"),row.names=F)
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
  
  statistics.table <- map(statistics.files, load.table) %>%
    bind_rows
  rpi.table <- map(rpi.files, load.table) %>%
    bind_rows
  conference.table <- map(conference.files, load.table) %>%
    bind_rows
  
  master.team.table <- full_join(full_join(conference.table,rpi.table,by=c("Team","Season")),statistics.table,by=c("Team","Season")) %>%
    filter(Team != "") %>%
    arrange(Season,Team)
  
  write.csv(master.team.table,"NCAA/Data/Statistics/NCAATeamStats.csv",row.names=F)
  
}
  






j <- read.csv("NCAA/Data/Statistics/NCAATeamStats.csv",header=T,stringsAsFactors=F)

k <- j %>% filter(is.na(Conference))

