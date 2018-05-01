library(tidyverse)
library(ggmap)

### Get latitude and longitude for each player hometown

season <- 2018

players <- read_csv(paste0("NCAA/Maps/PlayersScraped_",season-1,"-",season,".csv"))

# Still a few weird hometowns in the data set. Let's see which characters are causing problems
#troublesome.hometowns <- players %>%
#  rowwise() %>%
#  filter(grepl('[^[:alnum:] ]',Simple.Hometown)) %>%
#  .$Simple.Hometown

#troublesome.hometowns

# In this case it might just be less of a pain to manually fix the trouble spots

#players.fixed <- players

#players.fixed[grepl('[^[:alnum:] ]',players.fixed$Simple.Hometown),]$Simple.Hometown <- c("montreal quebec", "malmo sweden", 
#                                                                                          "gothenburg sweden", "vlasim czech republic", 
#                                                                                          "jonkoping sweden", "kirkland quebec", 
  #                                                                                        "montreal quebec", "pointeclaire quebec", 
   #                                                                                       "quebec city quebec", "montreal quebec", 
    #                                                                                      "saintlambert quebec", "montreal quebec", 
     #                                                                                     "longueuil quebec", "montreal quebec", 
      #                                                                                    "montreal quebec", "pointeclaire quebec", 
       #                                                                                   "breakeyville quebec", "gothenburg sweden", 
        #                                                                                  "saintcolomban quebec", "pointeclaire quebec", 
         #                                                                                 "gothenburg sweden", "frystak czech republic", 
          #                                                                                "sainteannedebellevue quebec", "lidingo sweden", 
           #                                                                               "laval quebec", "sherbrooke quebec", 
            #                                                                              "longueuil quebec", "montreal quebec", 
             #                                                                             "montreal quebec", "laval quebec", 
              #                                                                            "saintlazare quebec", "cotesaintluc quebec") 

# There are a few players with no hometown, so let's toss them out
players.fixed <- players %>% 
  filter(!is.na(Simple.Hometown))

collect.geocode <- function(hometown){
  
  geocode.result <- geocode(hometown, output = "latlona", source = "google") # Get coordinates
  
  longitude <- as.numeric(geocode.result[1])
  latitude <- as.numeric(geocode.result[2])
  
  return(tibble(Hometown = hometown, Lon = longitude, Lat = latitude))
  
}

geocodes <- map_df(players.fixed$Simple.Hometown, collect.geocode)


# Append sequentially the results of the 'geocode' function to obtain the latitude and longitude of their hometown
#for(i in 1:nrow(players.fixed)) {
  
#  result <- geocode(players.fixed$Simple.Hometown[i], output = "latlona", source = "google") # Get coordinates
  
  # Extract coordinates
##  players.fixed$Lon[i] <- as.numeric(result[1])
#  players.fixed$Lat[i] <- as.numeric(result[2])
#}

# Toss out players that 'geocode' couldn't find
players.final <- players.fixed %>%
  cbind(geocodes %>% select(-Hometown)) %>%
  filter(!is.na(Lat),!is.na(Lon))

write_csv(players.final, paste0("NCAA/Maps/PlayersHometown_",season-1,"-",season,".csv"))
          