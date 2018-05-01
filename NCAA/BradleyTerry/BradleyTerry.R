library(dplyr)
library(readr)
library(BradleyTerry2)

# Bradley-Terry Simulations

sched <- read_csv("NCAA/Data/Schedules/2017-2018_Clean.csv") %>%
  select(Home,at,Away,HomeScore,AwayScore,Date) %>%
  mutate(HomePts = case_when(HomeScore > AwayScore ~ 2, 
                            HomeScore == AwayScore ~ 1,
                            HomeScore < AwayScore ~ 0,
                            TRUE ~ NA_real_),
         OppPts = 2 - HomePts)

teams <- c(sched$Home,sched$Away) %>%
  unique %>%
  sort


x <- matrix(0, nrow = nrow(sched), ncol = length(teams)) # Games x Teams + 1

for (i in 1:nrow(sched)) {
  x[i, which(as.character(sched[i,"Home"]) == teams)] <- 1
  x[i, which(as.character(sched[i,"Away"]) == teams)] <- -1
} 


model.fit <- BTm(outcome = cbind(HomePts,OppPts),
                 data.frame(team = Home, home.adv = 1),
                 data.frame(team = Away, home.adv = 0),
                 ~ team + home.adv,
                 id = "team",
                 data = sched)

betas <- model.fit %>%
  tidy %>%
  .$estimate %>%
  head(-1) %>%
  c(0,.)
  
bt.abilities <- tibble(Team=teams,Beta=betas) %>%
  arrange(desc(Beta))
