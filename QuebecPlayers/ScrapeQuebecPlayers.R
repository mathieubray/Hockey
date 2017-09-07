library(XML)
library(dplyr)

# Scrape Quebec Players

url <- c("http://www.hockey-reference.com/friv/birthplaces.cgi?country=CA&province=QC&state=")

# Only 1 Table in list, column 12 repeats GP for goaltenders, remove it as the column name is the same as the previous and causes issues
quebec.players <- readHTMLTable(url,stringsAsFactors=F)[[1]][,-12] %>% 
  filter(Player != "Player") # Remove rows not corresponding to any player

names(quebec.players) <- c("Rk","Player","Start.Year","End.Date",
                           "Position","GP","G","A","PTS","Plus-Minus",
                           "PIM","W","L","TO","SV","GAA","Birthplace","Birth.Date","Death.Date")

write.csv(quebec.players,"QuebecPlayers/QuebecPlayers.csv",row.names=F)