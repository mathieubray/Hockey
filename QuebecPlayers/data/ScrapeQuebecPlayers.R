library(rvest)
library(dplyr)
library(magrittr)

# Scrape Quebec Players

url <- c("https://www.hockey-reference.com/friv/birthplaces.cgi?country=CA&province=QC&state=")

raw.data <- read_html(url) %>% 
  html_nodes("#stats") %>%
  `[[`(1) %>%
  html_table()


quebec.players <- raw.data %>%
  set_colnames(c("Rk","Player","Start.Year","End.Date",
                           "Position","GP","G","A","PTS","Plus-Minus",
                           "PIM","Goalie.GP","W","L","TO","SV","GAA","Birthplace","Birth.Date","Death.Date")) %>%
  filter(Rk != "Rk")

write.csv(quebec.players,"QuebecPlayers/data/QuebecPlayers.csv",row.names=F)
