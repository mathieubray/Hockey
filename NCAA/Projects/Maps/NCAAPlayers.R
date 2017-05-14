library(dplyr) # Really should be default
library(rvest) # Scrape scrape scrape
library(tm) # Work with strings
library(stringi) # Need 'stri_trans_general' function to standardize scraped strings

# Some players have their NHL draft team in parantheses next to their name
remove.drafted.team <- function(name) {
  
  name.tokens <- unlist(strsplit(name," ")) # Split name into tokens
  
  k <- length(name.tokens) # Get number of tokens
  
  if (grepl(x=name.tokens[k],pattern="\\(")){ # If the last token begins with a parantheses...
    name.tokens <- name.tokens[-k] # ...toss it out
  }
  
  return(paste(name.tokens,collapse= " ")) # Paste name string back together
  
}

# Hometown string also has previous team affiliation; get rid of that
extract.hometown <- function(hometown.string){
  
  tokens <- unlist(strsplit(hometown.string,"/")) # Split string into tokens
  
  hometown <- trimws(tokens[1]) # Extract the first token and trim the whitespace on each end
  
  return(hometown)
  
}

# For use with "geocode" function, simplify the hometown string
simplify.hometown <- function(hometown.string){
  
  hometown <- removePunctuation(tolower(hometown.string)) # Remove punctuation and put into all lower case
  
  return(hometown)
  
}

# Puts together a label for each player to assign as tooltips for maps
assign.label <- function(team,number,name,class,position,hometown){
  
  if (number < 10){ # Force number to have 2 digits
    number.label<-paste0("0",number)
  } else {
    number.label<-as.character(number)
  }
  
  hometown.label <- gsub(hometown,pattern="'",replacement="") # Remove single-quotes "'"; causes problems with leaflet
  
  return(paste0(team,": ",number.label," ",name," - ",hometown," (",class,"-", position,")")) # Paste info together in nice format
}

# Scrape player table for each team
collect.player.info <- function(var.list) {
  
  team <- var.list$Team[1]
  code <- var.list$Code[1]
  
  url <- paste0("http://collegehockeystats.net/1617/rosters/",code) # For year 2016-2017
  
  player.info <-  url %>% read_html %>% 
    html_nodes('.rostable') %>%
    html_table(header=T, fill=T) %>%
    data.frame(stringsAsFactors = F)
  
  player.info <- player.info[,1:9]
  
  # Add column names
  names(player.info) <- c("Number","Name","Class","Position","Height","Weight","Shoots","YOB","Hometown")
  
  player.info$Name <- stri_trans_general(player.info$Name,"Latin-ASCII") # Convert strings to all ASCII; random characters screw with ggmap and leaflet
  player.info$Hometown <- stri_trans_general(player.info$Hometown,"Latin-ASCII")
  
  # Assemble clean table
  player.info.clean <- player.info %>%
    rowwise() %>%
    select(Number,Name,Class,Position,Hometown) %>%
    mutate(Team = team,
           Number = as.numeric(gsub(Number,pattern="#",replacement = "")),
           Name = remove.drafted.team(gsub("[^\\x{00}-\\x{7f}]", " ", Name, perl = TRUE)), # Gets rid of weird spaces (may not be needed after using 'stri_trans_general'?)
           Class = gsub("[^\\x{00}-\\x{7f}]", " ", Class, perl = TRUE),
           Hometown = extract.hometown(Hometown),
           Simple.Hometown = simplify.hometown(Hometown),
           Label = assign.label(Team,Number,Name,Class,Position,Hometown))
  
  print(paste0("Completed Scraping for ", team)) # Track progress
  
  return(player.info.clean)
}


# Table of URL codes for each team
team.urls <- read.csv("NCAA/Projects/Maps/TeamCodes.csv",header=T,stringsAsFactors=F)

# Will collect tables sequentially as a list
opponent.info.list <- list()

for (i in 1:nrow(team.urls)){
  
  var.list <- team.urls[i,]
  opponent.info.list[[i]] <- collect.player.info(var.list)
  
  Sys.sleep(3) # So that collegehockeystats doesn't get angry...
  
}

players.complete <- bind_rows(opponent.info.list) # Merge all tables together into one

write.csv(players.complete,"NCAA/Projects/Maps/NCAAPlayersRaw.csv",row.names=F)


### Get latitude and longitude for each player hometown

library(ggmap)

players <- read.csv("NCAA/Projects/Maps/NCAAPlayersRaw.csv",header=T,stringsAsFactors=F)

# Still a few weird hometowns in the data set. Let's see which characters are causing problems
troublesome.hometowns <- players %>%
  rowwise() %>%
  filter(grepl('[^[:alnum:] ]',Simple.Hometown)) %>%
  .$Simple.Hometown

troublesome.hometowns

# In this case it might just be less of a pain to manually fix the trouble spots

players.fixed <- players

players.fixed[grepl('[^[:alnum:] ]',players.fixed$Simple.Hometown),]$Simple.Hometown <- c("montreal quebec", "malmo sweden", 
                                                                            "gothenburg sweden", "vlasim czech republic", 
                                                                            "jonkoping sweden", "kirkland quebec", 
                                                                            "montreal quebec", "pointeclaire quebec", 
                                                                            "quebec city quebec", "montreal quebec", 
                                                                            "saintlambert quebec", "montreal quebec", 
                                                                            "longueuil quebec", "montreal quebec", 
                                                                            "montreal quebec", "pointeclaire quebec", 
                                                                            "breakeyville quebec", "gothenburg sweden", 
                                                                            "saintcolomban quebec", "pointeclaire quebec", 
                                                                            "gothenburg sweden", "frystak czech republic", 
                                                                            "sainteannedebellevue quebec", "lidingo sweden", 
                                                                            "laval quebec", "sherbrooke quebec", 
                                                                            "longueuil quebec", "montreal quebec", 
                                                                            "montreal quebec", "laval quebec", 
                                                                            "saintlazare quebec", "cotesaintluc quebec"
) 

# There are a few players with no hometown, so let's toss them out
players.fixed <- players.fixed %>% filter(!is.na(Simple.Hometown))

# Append sequentially the results of the 'geocode' function to obtain the latitude and longitude of their hometown
for(i in 1:nrow(players.fixed)) {
  
  result <- geocode(players.fixed$Simple.Hometown[i], output = "latlona", source = "google") # Get coordinates
  
  # Extract coordinates
  players.fixed$Lon[i] <- as.numeric(result[1])
  players.fixed$Lat[i] <- as.numeric(result[2])
}

# Toss out players that 'geocode' couldn't find
players.fixed <- players.fixed %>%
  filter(!is.na(Lat),!is.na(Lon))

write.csv(players.fixed,"NCAA/Projects/Maps/PlayersHometown.csv",row.names=F)

