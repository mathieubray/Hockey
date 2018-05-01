library(tidyverse)
library(lubridate)
library(purrr)
library(broom)
library(qvcalc)

### Retrieve data ###

# Read in indivdual game data
nba <- read.table("NCAA/BradleyTerry/OriginalExamples/NBA_20092010.txt", header=T, stringsAsFactors=F) %>%
  mutate(Date = ymd(Date),
         Result = ifelse(ScoreHome > ScoreAway,1,0)) %>%
  arrange(Date)

# Count number of games
n.games <- nrow(nba)

# Collect all team names
teams <- c(nba$HomeTeam,nba$AwayTeam) %>%
  unique %>%
  sort

# Count number of teams
n.teams <- length(teams)

# Convert to indivudal team result data
home.results <- data.frame(Date=nba$Date,HomeAway="Home",Team=nba$HomeTeam,Result=nba$Result,stringsAsFactors=F)
away.results <- data.frame(Date=nba$Date,HomeAway="Away",Team=nba$AwayTeam,Result=1-nba$Result,stringsAsFactors=F)
team.results <- rbind(home.results,away.results)


### Static Bradley-Terry model analysis ###

get.bt.estimates <- function(Z){
  
  y <- cbind(nba$Result, 1-nba$Result)
  
  bt.formula <- c("y~1", make.names(colnames(Z)[-1])) %>%
    paste(collapse="+") %>%
    as.formula
  
  bt.fit <- glm(bt.formula, family=binomial(link="logit"), data=as.data.frame(Z))
  
  bt.coefs <- bt.fit %>%
    tidy %>%
    .$estimate
  
  return(list(coefs=bt.coefs,fit=bt.fit))
  
}

# Create design matrix

bt.design.matrix <- matrix(0, nrow=n.games, ncol=n.teams)

for (i in 1:n.games) {
  bt.design.matrix[i, which(nba[i,"HomeTeam"] == teams)] <- 1
  bt.design.matrix[i, which(nba[i,"AwayTeam"] == teams)] <- -1
} 

colnames(bt.design.matrix) <- teams


# Run Bradley-Terry model

bt <- get.bt.estimates(bt.design.matrix)

bt.coefs<- bt$coefs # Get coefficients

bt.ability <- rep(0,30) # Convert to abilities
reference <- (-sum(bt.coefs[-1]))/n.teams
bt.ability[1] <- reference
bt.ability[2:30] <- bt.coefs[-1] + reference

# Compute quasi-standard errors

var.matrix <- vcov(bt$fit)
var.improved <- matrix(0, nrow=30, ncol=30)
var.improved[2:30,2:30] <- var.matrix[2:30, 2:30]

qse <- qvcalc(var.improved, estimates=bt.ability, labels=teams)


### Dyanmic Bradley-Terry model analysis ###

# Set the starting values of the covariates as the percentage of wins and losses at home during the *previous* season
# Set the remaining vectors to help speed up (considerably) computation...

prev.year.pct <- 0.608

home <- nba$HomeTeam
away <- nba$AwayTeam
game.results <- nba$Result

# Function for computing the covariates x of the dynamic model given lambda_1 and lambda_2 (see formula (6))

build.previous <- function(lambda1, lambda2){
  
  prev <- matrix(c(rep(prev.year.pct,n.games),rep(1-prev.year.pct,n.games)),nrow=n.games,ncol=2)
  
  for (i in 2:nrow(prev)){
    
    home.game.ids <- which(home[1:(i-1)]==home[i]) # This is faster than tidy functions...
    away.game.ids <- which(away[1:(i-1)]==away[i])
    
    if (length(home.game.ids)>0){
      
      ewma.home <- lambda1*(1-lambda1)^(length(home.game.ids):1-1)
      pts.home <- game.results[home.game.ids]
      prev[i,1] <- sum(ewma.home*pts.home,(1-lambda1)^(length(home.game.ids))*prev.year.pct)
    }
    
    if (length(away.game.ids)>0){
      
      ewma.away <- lambda2*(1-lambda2)^(length(away.game.ids):1-1)
      pts.away <- 1-game.results[away.game.ids]
      prev[i,2] <- sum(ewma.away*pts.away,(1-lambda2)^(length(away.game.ids))*(1-prev.year.pct))
    }
  }
  
  prev
}

# Profile likelihood
profile.likelihood <- function(lambdas){
    
    lambdas <- plogis(lambdas)
    previous <- build.previous(lambdas[1], lambdas[2])
    fit <- glm.fit(x=cbind(previous[,1], -previous[,2]), y=cbind(game.results, 1-game.results), family=binomial(link="logit"))
    
    print(paste0("Profile Likelihood fit for (",lambdas[1],",",lambdas[2],")"))
    
    return(-fit$deviance/2)
}

# Find the values of lambda_1 and lambda_2 that maximize the profile likelihood 

op <- optim(qlogis(rep(0.5, 2)), profile.likelihood, control=list(fnscale=-1))
best.lambda <- plogis(op$par)
best.lambda


# Plot of the profile likelihood (Figure 1)

seq.lambda <- seq(0,0.2,by=0.01)

lambda.combinations <- expand.grid(lambda1=seq.lambda,lambda2=seq.lambda)

lambda.combo.lists <- split(lambda.combinations,1:nrow(lambda.combinations))

profile.likelihoods <- map_dbl(lambda.combo.lists, function(lambda.list){ profile.likelihood(qlogis(c(as.numeric(lambda.list[1]), as.numeric(lambda.list[2])))) } )

lambda.frame <- cbind(lambda.combinations,profile.likelihoods)

ggplot(data=lambda.frame,aes(x=lambda1,y=lambda2,z=profile.likelihoods)) +
  geom_point(x=best.lambda[1],y=best.lambda[2],size=5,color="red") +
  geom_contour(bins=30) +
  theme_bw()


## Re-fit best model

get.dynamic.bt.estimates <- function(lambda){

  y <- cbind(nba$Result, 1-nba$Result)

  prev <- build.previous(lambda[1], lambda[2])

  dynamic.fit <- glm(y~(-1)+prev[,1]+I(-prev[,2]), family=binomial(link="logit"))

  dynamic.coefs <- dynamic.fit %>%
  tidy %>%
  .$estimate

  return(list(prev=prev,coefs=dynamic.coefs,fit=dynamic.fit))
}

dynamic.bt <- get.dynamic.bt.estimates(best.lambda)

dynamic.bt.coefs <- dynamic.bt$coefs # Get coefficients


## Smoothed abilities plots (Figure 3)

nba.augmented <- data.frame(nba,dynamic.bt$prev)

range.ability <- c(nba.augmented$X1*dynamic.bt.coefs[1], nba.augmented$X2*dynamic.bt.coefs[2]) %>% range

team <- "Cleveland"

team.final.home <- nba.augmented %>%
  filter(HomeTeam == team)

team.final.away <- nba.augmented %>%
  filter(AwayTeam == team)

home.spline <- data.frame(spline(1:41, team.final.home$X1*dynamic.bt.coefs[1]),place="Home")
away.spline <- data.frame(spline(1:41, team.final.away$X2*dynamic.bt.coefs[2]),place="Away")

splines <- rbind(home.spline,away.spline)

ggplot(data=splines, aes(x=x,y=y,linetype=place)) +
  geom_line() +
  theme_bw() +
  xlab("Game #") +
  ylab("Ability") +
  labs(linetype="") +
  ggtitle(paste0("Ability: ",team)) +
  expand_limits(y=0)

# Get abilities

calculate.dynamic.ability <- function(team,coefs){

  home.ability <- nba.augmented %>%
    filter(HomeTeam == team) %>%
    mutate(X1 = X1*coefs[1]) %>%
    .$X1
  
  away.ability <- nba.augmented %>%
    filter(AwayTeam == team) %>%
    mutate(X2 = X2*coefs[2]) %>%
    .$X2

  return(mean(c(home.ability,away.ability)))
  
}

dynamic.ability.values <- teams %>% map_dbl(calculate.dynamic.ability,coefs=dynamic.bt.coefs)

mean.dynamic.ability <- mean(dynamic.ability.values)


# Build up Table 1

wins <- team.results %>%
  group_by(Team) %>%
  summarize(Wins = sum(Result))

home.win.pct <- team.results %>%
  group_by(Team,Result) %>%
  summarize(Home.Win.Pct = sum(HomeAway=="Home")/n()) %>%
  filter(Result == 1) %>%
  select(-Result)

abilities <- data.frame(Team=teams, Ability=bt.ability, stringsAsFactors=F) %>%
  arrange(desc(Ability)) %>%
  mutate(Rank=1:30)

quasi.st.errs <- data.frame(Team=row.names(qse$qvframe), QSE = qse$qvframe %>% select(quasiSE) %>% .$quasiSE, stringsAsFactors=F)
row.names(quasi.st.errs) <- NULL

dynamic.abilities <- data.frame(Team = teams, Dynamic.Ability = dynamic.ability.values, stringsAsFactors=F) %>%
  mutate(Dynamic.Ability = round(Dynamic.Ability - mean.dynamic.ability, 3)) %>%
  arrange(desc(Dynamic.Ability)) %>%
  mutate(Dynamic.Rank = 1:30)

results.table <- wins %>%
  inner_join(home.win.pct) %>%
  inner_join(abilities) %>%
  inner_join(quasi.st.errs) %>%
  inner_join(dynamic.abilities) %>%
  arrange(desc(Wins))

results.table

## Goodness-of-fit by Brier score: model-based and "unstructured" (means reported in Section 4.1)

dynamic.fitted <- dynamic.bt$fit %>%
  augment %>%
  .$.fitted %>%
  plogis

bt.fitted <- bt$fit %>%
  augment %>%
  .$.fitted %>%
  plogis


BS <- function(y, p){
  return(2*(y*(1-p)^2+(1-y)*p^2))
}

result <- nba$Result

BS.dynamic <- BS(result,dynamic.fitted)
BS.bt <- BS(result,bt.fitted)

mean(BS.bt)
mean(BS.dynamic)

cor(BS.bt,BS.dynamic)


### Prediction ###

last <- function(x){
  return(x[length(x)])
}

## Generic function for predicting results of matches taking place in day "day", given observations from 1 to (day-1)

predict.DBT <- function(game.date){
  
  prev.year.pct <- 0.608
  
  # Retrieve all games prior to the given date
  observed.games <- nba %>% filter(Date < game.date)
  n.observed.games <- nrow(observed.games)
  
  # Retrieve all games on the given date
  games.to.predict <- nba %>% filter(Date == game.date)
  n.games.to.predict <- nrow(games.to.predict)
  
  home.observed <- observed.games$HomeTeam
  away.observed <- observed.games$AwayTeam
  game.results.observed <- observed.games$Result

  build.previous <- function(lambda1, lambda2){
    
    prev <- matrix(c(rep(prev.year.pct,n.observed.games),rep(1-prev.year.pct,n.observed.games)),nrow=n.observed.games,ncol=2)
    
    for (i in 2:nrow(prev)){
      
      home.game.ids <- which(home.observed[1:(i-1)]==home.observed[i])
      away.game.ids <- which(away.observed[1:(i-1)]==away.observed[i])
      
      if (length(home.game.ids)>0){
        #ewma.home <- (1-lambda1)*(lambda1)^(length(home.game.ids):1-1)
        ewma.home <- lambda1*(1-lambda1)^(length(home.game.ids):1-1)
        pts.home <- game.results.observed[home.game.ids]
        prev[i,1] <- sum(ewma.home*pts.home,(1-lambda1)^(length(home.game.ids))*prev.year.pct)
        #prev[i,1] <- sum(ewma.home*pts.home,(lambda1)^(length(home.game.ids))*prev.year.pct)
      }
      
      if (length(away.game.ids)>0){
        #ewma.away <- (1-lambda2)*(lambda2)^(length(away.game.ids):1-1)
        ewma.away <- lambda2*(1-lambda2)^(length(away.game.ids):1-1)
        pts.away <- 1-game.results.observed[away.game.ids]
        prev[i,2] <- sum(ewma.away*pts.away,(1-lambda2)^(length(away.game.ids))*(1-prev.year.pct))
        #prev[i,2] <- sum(ewma.away*pts.away,(lambda2)^(length(away.game.ids))*(1-prev.year.pct))
      }
    }
    
    prev
  }
  
  profile.likelihood <- function(lambdas){
    
    lambdas <- plogis(lambdas)
    previous <- build.previous(lambdas[1], lambdas[2])
    fit <- glm.fit(x=cbind(previous[,1], -previous[,2]), y=cbind(game.results.observed, 1-game.results.observed), family=binomial(link="logit"))
    
    return(-fit$deviance/2)
  }
  
  ## Find the values of lambda_1 and lambda_2 that maximize the profile likelihood 
  op <- optim(qlogis(rep(0.5, 2)), profile.likelihood, control=list(fnscale=-1))
  best.lambda <- plogis(op$par)
  
  ## Fit dynamic model    
  prev <- build.previous(best.lambda[1], best.lambda[2])
  
  get.dynamic.bt.estimates <- function(lambda){
    
    y <- cbind(game.results.observed, 1-game.results.observed)
    
    dynamic.fit <- glm(y~(-1)+prev[,1]+I(-prev[,2]), family=binomial(link="logit"))
    
    dynamic.coefs <- dynamic.fit %>%
      tidy %>%
      .$estimate
    
    return(list(prev=prev,coefs=dynamic.coefs,fit=dynamic.fit))
  }
  
  dynamic.bt <- get.dynamic.bt.estimates(best.lambda)
  
  dynamic.bt.coefs <- dynamic.bt$coefs
  
  
  ## Fit static model
  
  bt.design.reduced <- bt.design.matrix[1:n.observed.games,]
  
  get.bt.estimates <- function(Z){
    
    y <- cbind(game.results.observed, 1-game.results.observed)
    
    bt.formula <- c("y~1", make.names(colnames(Z)[-1])) %>%
      paste(collapse="+") %>%
      as.formula
    
    bt.fit <- glm(bt.formula, family=binomial(link="logit"), data=as.data.frame(Z))
    
    bt.coefs <- bt.fit %>%
      tidy %>%
      .$estimate
    
    return(list(coefs=bt.coefs,fit=bt.fit))
    
  }
  
  bt <- get.bt.estimates(bt.design.reduced)
  
  bt.coefs <- bt$coefs
  
  home.bt <- bt.coefs[1]
  teams.bt <- c(0,bt.coefs[-1])
  
  ## Predictions
  
  dynamic.predict <- rep(NA, n.games.to.predict)
  bt.predict <- rep(NA, n.games.to.predict)
  
  observed.games.final <- cbind(observed.games,dynamic.bt$prev)
  names(observed.games.final)[7:8]<-c("X1","X2")
  
  for(i in 1:n.games.to.predict){
    
    home.team <- games.to.predict$HomeTeam[i]
    
    prev.games.team.home <- observed.games.final %>%
      filter(HomeTeam == home.team)
    
    prev.old.home <- prev.games.team.home %>%
      .$X1 %>%
      last
    
    last.result.home <- prev.games.team.home %>%
      .$Result %>%
      last
    
    home.ability <- ((best.lambda[1])*last.result.home+(1-best.lambda[1])*prev.old.home)*dynamic.bt.coefs[1]
    #home.ability <- ((1-best.lambda[1])*last.result.home+best.lambda[1]*prev.old.home)*dynamic.bt.coefs[1]
    #home.ability <- best.lambda[1]*dynamic.bt.coefs[1]*last.result.home + (1-best.lambda[1])*prev.old.home
    
    
    away.team <- games.to.predict$AwayTeam[i]
    
    prev.games.team.away <- observed.games.final %>%
      filter(AwayTeam == away.team)
    
    prev.old.away <- prev.games.team.away %>%
      .$X2 %>%
      last
    
    last.result.away <- prev.games.team.away %>%
      .$Result %>%
      last #???
    
    away.ability <- ((best.lambda[2])*(1-last.result.away)+(1-best.lambda[2])*prev.old.away)*dynamic.bt.coefs[2]
    #away.ability <- ((1-best.lambda[2])*(1-last.result.away)+best.lambda[2]*prev.old.away)*dynamic.bt.coefs[2]
    #away.ability <- best.lambda[2]*dynamic.bt.coefs[2]*(1-last.result.away) + (1-best.lambda[2])*prev.old.away
    
    diff.ability <- home.ability-away.ability
    
    dynamic.predict[i] <- plogis(diff.ability)
    bt.predict[i] <- plogis(home.bt+teams.bt[which(teams==home.team)]-teams.bt[which(teams==away.team)])
  }
  
  BS.dynamic <- BS(games.to.predict$Result, dynamic.predict)
  BS.bt <- BS(games.to.predict$Result, bt.predict)
  
  
  return(data.frame(BS.dynamic=BS.dynamic, BS.bt=BS.bt))
}

## All predictions since mid-season

dates <- nba$Date %>%
  unique %>% 
  sort

n.dates <- length(dates)
end.date <- n.dates/2


run.prediction.alg <- function(day.to.predict){
  
  date.to.predict <- dates[day.to.predict]
  
  p <- predict.DBT(date.to.predict)
  
  print(paste0("Done: ",date.to.predict))
  
  return(p)
  
}

days.to.predict <- (end.date+1):n.dates

sim.results <- map(days.to.predict,run.prediction.alg)


## Box plots of the Brier scores (Figure 2)

library(magrittr)

dynamic.results <- sim.results %>% map(extract,"BS.dynamic") %>% unlist
names(dynamic.results) <- NULL

bt.results <- sim.results %>% map(extract,"BS.bt") %>% unlist
names(bt.results) <- NULL

end.day <- dates[end.date]
observed.games <- nba %>% filter(Date <= end.day)
n.observed.games <- nrow(observed.games)

wins <- observed.games %>%
  filter(Result==1) %>%
  nrow

win.pct <- wins/n.observed.games

games.to.predict <- nba %>% filter(Date > end.day)

game.results <- games.to.predict$Result

empirical.results <- 2*(game.results - win.pct)^2

sim.data <- data.frame(Dynamic=dynamic.results,BT=bt.results,Empirical=empirical.results) %>%
  gather(key="Simulation",value="Result")

ggplot(data=sim.data,aes(x=Simulation,y=Result)) +
  geom_boxplot() +
  theme_bw()
