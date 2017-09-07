library(dplyr)
library(rvest)

url <- 'http://www.uscho.com/rankings/rpi/d-i-men'

uscho <- url %>% # Scrape USCHO data
  read_html%>% 
  html_nodes('table') %>%
  html_table(header=T, fill=F) %>%
  data.frame(stringsAsFactors = F) %>%
  rename(AdjPct = Win.., QWBAdjustedRPI = QWB.Adj.RPI, AdjustedRPI = Adj.RPI) %>%
  rowwise() %>%
  mutate(Team = ifelse(Team == "Army","Army West Point",Team)) %>%
  ungroup() %>%
  select(Team,AdjPct,SOS,RPI) %>%
  arrange(Team)

write.csv(uscho,"USCHOValues_20162017.csv",row.names=F)