library(dplyr)
library(BradleyTerry2)

# Bradley-Terry Simulations

sched <- historical.seasons %>%
  dplyr::select(Home,Away,HomeWin) %>%
  mutate(HomePts = ifelse(HomeWin == "Win",2,ifelse(HomeWin == "Tie",1,0)),
         OppPts = 2 - HomePts)

teams <- sort(unique(c(sched$Home,sched$Away)))


x <- matrix(0, nrow = nrow(sched), ncol = length(teams)) # Games x Teams + 1

for (i in 1:nrow(sched)) {
  x[i, which(as.character(sched[i,"Home"]) == teams)] <- 1
  x[i, which(as.character(sched[i,"Away"]) == teams)] <- -1
} 

library(BradleyTerry2)

model.fit <- BTm(outcome = cbind(HomePts,OppPts),
                 data.frame(team = Home, home.adv = 1),
                 data.frame(team = Away, home.adv = 0),
                 ~ team + home.adv,
                 id = "team",
                 data = sched)

betas <- matrix(c(0,msummary(model.fit)$coeff[,1]))[-61,]

j <- data.frame(Team=teams,Beta=betas, stringsAsFactors=F) %>%
  arrange(desc(Beta))