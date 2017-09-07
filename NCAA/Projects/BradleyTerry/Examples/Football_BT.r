library(tidyverse)
library(lubridate)
library(purrr)
library(broom)
library(qvcalc)

### Retrieve data ###

# Read in indivdual game data
serieA <- read.table("InProgress/BradleyTerry/football08_09.txt", header=T, stringsAsFactors=F) %>%
  mutate(Date = ymd(Date),
         Result = ifelse(ScoreHome > ScoreAway, 2, ifelse(ScoreHome == ScoreAway,1,0)),
         Points = ifelse(Result==2,3,Result)) %>%
  arrange(Date)

# Count number of games
n.games <- nrow(serieA)

# Collect all team names
teams <- c(serieA$HomeTeam,serieA$AwayTeam) %>%
  unique %>%
  sort

# Count number of teams
n.teams <- length(teams)

# Convert to indivudal team result data
home.results <- data.frame(Date=serieA$Date,HomeAway="Home",Team=serieA$HomeTeam,Result=serieA$Result,Points=serieA$Points,stringsAsFactors=F)
away.results <- data.frame(Date=serieA$Date,HomeAway="Away",Team=serieA$AwayTeam,Result=2-serieA$Result,Points=ifelse(serieA$Points==3,0,ifelse(serieA$Points==0,3,1)),stringsAsFactors=F)
team.results <- rbind(home.results,away.results)



### Static Bradley-Terry model analysis ###

# Create design matrix

bt.design.matrix <- matrix(0, nrow=n.games, ncol=n.teams)

for (i in 1:n.games) {
  bt.design.matrix[i, which(serieA[i,"HomeTeam"] == teams)] <- 1
  bt.design.matrix[i, which(serieA[i,"AwayTeam"] == teams)] <- -1
} 

colnames(bt.design.matrix) <- teams

# Likelihood function for the Bradley-Terry model with three-categorical results
likelihood.f <- function(param, x, y){
  num.parameters <- length(param)
  delta <- exp(param[num.parameters])
  
  bt.formula <- x %*% param[1:(num.parameters-1)]
  
  prob.loss <- plogis(-delta-bt.formula)
  prob.loss.tie <- plogis(delta-bt.formula)
  prob.tie <- prob.loss.tie-prob.loss
  prob.win <- 1-prob.loss.tie
  
  prob <- sum(log(c(prob.loss[which(y==0)], prob.tie[which(y==1)], prob.win[which(y==2)])))
  
  return(prob)
}

# Run Bradley-Terry model
BT.fit <- optim(c(rep(0, n.teams), log(0.01)), 
                fn=likelihood.f, 
                y=serieA$Result,
                x=cbind(1, bt.design.matrix[,-1]), 
                method="BFGS",
                control=list(fnscale=-1, maxit=1500000), 
                hessian=TRUE)

bt.coefs <- BT.fit$par[2:n.teams]
bt.ability <- rep(0, n.teams)
reference <- (-sum(bt.coefs))/n.teams
bt.ability[1] <- reference
bt.ability[2:20] <- reference + bt.coefs

# Compute the estimate of the threshold parameter
delta <- exp(BT.fit$par[21])
delta

# Compute quasi-standard errors
var.matrix <- solve(-BT.fit$hessian)
var.improved <- matrix(0, nrow=20, ncol=20)
var.improved[2:20,2:20] <- var.matrix[2:20,2:20]

qse <- qvcalc(var.improved, estimates=bt.ability, labels=teams)


### Dyanmic Bradley-Terry model analysis ###

# Set the starting values of the covariates as the percentage of wins and losses at home during the *previous* season
# Set the remaining vectors to help speed up (considerably) computation...

prev.year.mean.pts <- c(1.676, 1.029)

home <- serieA$HomeTeam
away <- serieA$AwayTeam
game.results <- serieA$Result
game.points <- serieA$Points

# Function for computing the covariates x of the dynamic model given lambda_1 and lambda_2 (see formula (6))

build.previous <- function(lambda1, lambda2){
  
  prev <- matrix(rep(prev.year.mean.pts, n.games), nrow=n.games, ncol=2, byrow=TRUE)
  
  for (i in 2:nrow(prev)){
    
    home.game.ids <- which(home[1:(i-1)]==home[i])
    away.game.ids <- which(away[1:(i-1)]==away[i])
    
    if (length(home.game.ids)>0){
      
      ewma.home <- lambda1*(1-lambda1)^(length(home.game.ids):1-1)
      pts.home <- game.points[home.game.ids] # Use the number of points (i.e. 3) in case of win
      prev[i,1] <- sum(ewma.home*pts.home,(1-lambda1)^(length(home.game.ids))*prev.year.mean.pts[1])
    }
    
    if (length(away.game.ids)>0){
      
      ewma.away <- lambda2*(1-lambda2)^(length(away.game.ids):1-1)
      pts.away <- ifelse(game.points[away.game.ids]==3,0,ifelse(game.points[away.game.ids]==0,3,1))
      prev[i,2] <- sum(ewma.away*pts.away,(1-lambda2)^(length(away.game.ids))*prev.year.mean.pts[2])
    }
  }
  
  prev
}

# Profile likelihood
profile.likelihood <- function(par, y){
  
  lambda <- plogis(par)
  previous <- build.previous(lambda[1], lambda[2])
  fit <- optim(c(0,0,log(0.01)), 
              fn=likelihood.f, 
              y=y,
              x=cbind(previous[,1], -previous[,2]),
              method="Nelder-Mead", 
              control=list(fnscale=-1, maxit=150000))
  
  return(fit$value)
             
}

# Find the values of lambda_1 and lambda_2 that maximize the profile likelihood  
op <- optim(qlogis(rep(0.5,2)), 
            fn=profile.likelihood, 
            y=game.results, 
            method="Nelder-Mead",
            control=list(fnscale=-1, maxit=150000))


best.lambda <- plogis(op$par)
best.lambda


# Plot of the profile likelihood 

seq.lambda <- seq(0, 0.2, by=0.01)

lambda.combinations <- expand.grid(lambda1=seq.lambda,lambda2=seq.lambda)

lambda.combo.lists <- split(lambda.combinations,1:nrow(lambda.combinations))

profile.likelihoods <- map_dbl(lambda.combo.lists, function(lambda.list){ profile.likelihood(qlogis(c(as.numeric(lambda.list[1]), as.numeric(lambda.list[2]))),y=game.results) } )

lambda.frame <- cbind(lambda.combinations,profile.likelihoods)

ggplot(data=lambda.frame,aes(x=lambda1,y=lambda2,z=profile.likelihoods)) +
  geom_point(x=best.lambda[1],y=best.lambda[2],size=5,color="red") +
  geom_contour(bins=30) +
  theme_bw()


## Re-fit best model
prev <- build.previous(best.lambda[1], best.lambda[2])
dynamic.bt <- optim(c(0,0, log(0.01)), 
              likelihood.f, 
              y=game.results,
              x=cbind(prev[,1], -prev[,2]),
              method="Nelder-Mead", 
              control=list(fnscale=-1, maxit=1000000),
              hessian=TRUE)

dynamic.bt.coefs <- dynamic.bt$par
dynamic.bt.st.err <- -dynamic.bt$hessian  %>% solve %>% diag %>% sqrt


## Smoothed abilities plots (Figure 4)

serieA.augmented <- data.frame(serieA,prev)

range.ability <- c(serieA.augmented$X1*dynamic.bt.coefs[1], serieA.augmented$X2*dynamic.bt.coefs[2]) %>% range

team <- "Inter"

team.final.home <- serieA.augmented %>%
  filter(HomeTeam == team)

team.final.away <- serieA.augmented %>%
  filter(AwayTeam == team)

home.spline <- data.frame(spline(1:19,team.final.home$X1*dynamic.bt.coefs[1]),place="Home")
away.spline <- data.frame(spline(1:19,team.final.away$X2*dynamic.bt.coefs[2]),place="Away")

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
  
  home.ability <- serieA.augmented %>%
    filter(HomeTeam == team) %>%
    mutate(X1 = X1*coefs[1]) %>%
    .$X1
  
  away.ability <- serieA.augmented %>%
    filter(AwayTeam == team) %>%
    mutate(X2 = X2*coefs[2]) %>%
    .$X2
  
  return(mean(c(home.ability,away.ability)))
  
}

dynamic.ability.values <- teams %>% map_dbl(calculate.dynamic.ability,coefs=dynamic.bt.coefs)

mean.dynamic.ability <- mean(dynamic.ability.values)


# Build up Table 1

points <- team.results %>%
  group_by(Team) %>%
  summarize(Total.Points = sum(Points))

home.points.pct <- team.results %>%
  group_by(Team,HomeAway) %>%
  summarize(Points = sum(Points)) %>%
  ungroup() %>%
  group_by(Team) %>%
  summarize(Home.Points.Pct = sum(Points*ifelse(HomeAway=="Home",1,0))/sum(Points)) %>%
  mutate(Home.Points.Pct = round(Home.Points.Pct,2))

abilities <- data.frame(Team=teams, Ability=bt.ability, stringsAsFactors=F) %>%
  arrange(desc(Ability)) %>%
  mutate(Ability = round(Ability,3),
         Rank=1:n.teams)

quasi.st.errs <- data.frame(Team=row.names(qse$qvframe), QSE = qse$qvframe %>% select(quasiSE) %>% .$quasiSE, stringsAsFactors=F) %>%
  mutate(QSE = round(QSE,3))
row.names(quasi.st.errs) <- NULL

dynamic.abilities <- data.frame(Team = teams, Dynamic.Ability = dynamic.ability.values, stringsAsFactors=F) %>%
  mutate(Dynamic.Ability = round(Dynamic.Ability - mean.dynamic.ability, 3)) %>%
  arrange(desc(Dynamic.Ability)) %>%
  mutate(Dynamic.Rank = 1:n.teams)

results.table <- points %>%
  inner_join(home.points.pct) %>%
  inner_join(abilities) %>%
  inner_join(quasi.st.errs) %>%
  inner_join(dynamic.abilities) %>%
  arrange(desc(Total.Points),Team)

results.table




## Goodness-of-fit by rank probability score

## Cumulative observed results
cumulative.results <- matrix(0, nrow=n.games, ncol=3)
cumulative.results[which(game.results == 0),1] <- 1
cumulative.results[which(game.results <= 1),2] <- 1
cumulative.results[,3] <- 1

delta.dynamic <- exp(dynamic.bt$par[3])
delta.bt <- exp(BT.fit$par[21])

means.dynamic <- cbind(prev[,1], -prev[,2]) %*% dynamic.bt$par[1:2]
prob.loss.dynamic <- plogis(-delta.dynamic-means.dynamic)
prob.loss.tie.dynamic <- plogis(delta.dynamic-means.dynamic)

means.bt <- cbind(1, bt.design.matrix[,-1]) %*% BT.fit$par[1:20]
prob.loss.bt <- plogis(-delta.bt-means.bt)
prob.loss.tie.bt <- plogis(delta.bt-means.bt)

rank.prob.score.dynamic <- (cumulative.results-cbind(prob.loss.dynamic, prob.loss.tie.dynamic, 1))^2 %>% rowSums
rank.prob.score.bt <- (cumulative.results-cbind(prob.loss.bt, prob.loss.tie.bt, 1))^2 %>% rowSums

mean(rank.prob.score.dynamic)
mean(rank.prob.score.bt)

cor(rank.prob.score.dynamic,rank.prob.score.bt)



### Prediction ###

last <- function(x){
  return(x[length(x)])
}

## Computes the rank probability score
RPS <- function(y, p){
  return(((y==0)-p[,1])^2+((y<=1)-(p[,1]+p[,2]))^2)
}

## Generic function for predicting results of matches taking place in day "day", given observations from 1 to (day-1)

predict.DBT <- function(game.date){
  
  prev.year.mean.pts <- c(1.676, 1.029)
  
  # Retrieve all games prior to the given date
  observed.games <- serieA %>% filter(Date < game.date)
  n.observed.games <- nrow(observed.games)
  
  # Retrieve all games on the given date
  games.to.predict <- serieA %>% filter(Date == game.date)
  n.games.to.predict <- nrow(games.to.predict)
  
  home.observed <- observed.games$HomeTeam
  away.observed <- observed.games$AwayTeam
  game.results.observed <- observed.games$Result
  game.points.observed <- observed.games$Points
  
  
  build.previous <- function(lambda1, lambda2){
    
    prev <- matrix(rep(prev.year.mean.pts, n.observed.games), nrow=n.observed.games, ncol=2, byrow=TRUE)
    
    for (i in 2:nrow(prev)){
      
      home.game.ids <- which(home.observed[1:(i-1)]==home.observed[i])
      away.game.ids <- which(away.observed[1:(i-1)]==away.observed[i])
      
      if (length(home.game.ids)>0){
        
        ewma.home <- lambda1*(1-lambda1)^(length(home.game.ids):1-1)
        pts.home <- game.points.observed[home.game.ids] # Use the number of points (i.e. 3) in case of win
        prev[i,1] <- sum(ewma.home*pts.home,(1-lambda1)^(length(home.game.ids))*prev.year.mean.pts[1])
      }
      
      if (length(away.game.ids)>0){
        
        ewma.away <- lambda2*(1-lambda2)^(length(away.game.ids):1-1)
        pts.away <- ifelse(game.points.observed[away.game.ids]==3,0,ifelse(game.points.observed[away.game.ids]==0,3,1))
        prev[i,2] <- sum(ewma.away*pts.away,(1-lambda2)^(length(away.game.ids))*prev.year.mean.pts[2])
      }
    }
    
    prev
  }
  
  profile.likelihood <- function(par, y){
    
    lambda <- plogis(par)
    previous <- build.previous(lambda[1], lambda[2])
    fit <- optim(c(0,0,log(0.01)), 
                 fn=likelihood.f, 
                 y=y,
                 x=cbind(previous[,1], -previous[,2]),
                 method="Nelder-Mead", 
                 control=list(fnscale=-1, maxit=150000))
    
    return(fit$value)
    
  }
  
  likelihood.f <- function(param, x, y){
    num.parameters <- length(param)
    delta <- exp(param[num.parameters])
    
    bt.formula <- x %*% param[1:(num.parameters-1)]
    
    prob.loss <- plogis(-delta-bt.formula)
    prob.loss.tie <- plogis(delta-bt.formula)
    prob.tie <- prob.loss.tie-prob.loss
    prob.win <- 1-prob.loss.tie
    
    prob <- sum(log(c(prob.loss[which(y==0)], prob.tie[which(y==1)], prob.win[which(y==2)])))
    
    return(prob)
  }
  
  ## Find the values of lambda_1 and lambda_2 that maximize the profile likelihood 
  op <- optim(qlogis(rep(0.5,2)), 
              fn=profile.likelihood, 
              y=game.results.observed, 
              method="Nelder-Mead",
              control=list(fnscale=-1, maxit=150000))
  
  
  best.lambda <- plogis(op$par)
  
  ## Fit dynamic model
  prev <- build.previous(best.lambda[1], best.lambda[2])
  dynamic.bt <- optim(c(0,0, log(0.01)), 
                      likelihood.f, 
                      y=game.results.observed, 
                      x=cbind(prev[,1], -prev[,2]),
                      method="Nelder-Mead", 
                      control=list(fnscale=-1, maxit=150000))
  
  dynamic.bt.coefs <- dynamic.bt$par[1:2]
  delta.dynamic <- exp(dynamic.bt$par[3])
  
  
  ## Fit static model
  
  bt.design.reduced <- bt.design.matrix[1:n.observed.games,]
  
  BT.fit <- optim(c(rep(0, n.teams), log(0.01)), 
                  fn=likelihood.f, 
                  y=game.results.observed,
                  x=cbind(1, bt.design.reduced[,-1]), 
                  method="BFGS",
                  control=list(fnscale=-1, maxit=1500000))
  
  home.bt <- BT.fit$par[1]
  teams.bt <- c(0,BT.fit$par[2:n.teams])
  
  delta.bt <- exp(BT.fit$par[n.teams+1])
  
  
  ## Predictions
  
  dynamic.predict <- matrix(NA, nrow=n.games.to.predict,ncol=3)
  bt.predict <- matrix(NA, nrow=n.games.to.predict,ncol=3)
  
  observed.games.final <- cbind(observed.games,prev)
  names(observed.games.final)[8:9]<-c("X1","X2")
  
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
    
    last.result.home <- ifelse(last.result.home==2,3,last.result.home)
    
    home.ability <- ((best.lambda[1])*last.result.home+(1-best.lambda[1])*prev.old.home)*dynamic.bt.coefs[1]
    
    
    away.team <- games.to.predict$AwayTeam[i]
    
    prev.games.team.away <- observed.games.final %>%
      filter(AwayTeam == away.team)
    
    prev.old.away <- prev.games.team.away %>%
      .$X2 %>%
      last
    
    last.result.away <- prev.games.team.away %>%
      .$Result %>%
      last #???
    
    last.result.away <- ifelse(last.result.away==2,0,ifelse(last.result.away==0,3,1))
    
    away.ability <- ((best.lambda[2])*(last.result.away)+(1-best.lambda[2])*prev.old.away)*dynamic.bt.coefs[2]
    
    diff.ability <- home.ability-away.ability
    
    dynamic.predict[i,1] <- plogis(-delta.dynamic - diff.ability) # loss
    dynamic.loss.tie <- plogis(delta.dynamic - diff.ability) # loss or tie
    dynamic.predict[i,2] <- dynamic.loss.tie - dynamic.predict[i,1] # tie
    dynamic.predict[i,3] <- 1 - dynamic.loss.tie # win
    
    bt.predict[i,1] <- plogis(-delta.bt - home.bt-(teams.bt[which(teams==home.team)]-teams.bt[which(teams==away.team)]))
    bt.loss.tie <- plogis(delta.bt - home.bt - (teams.bt[which(teams==home.team)]-teams.bt[which(teams==away.team)]))
    bt.predict[i,2] <- bt.loss.tie - bt.predict[i,1]
    bt.predict[i,3] <- 1 - bt.loss.tie
  }
  
  RPS.dynamic <- RPS(games.to.predict$Result, dynamic.predict)
  RPS.bt <- RPS(games.to.predict$Result, bt.predict)
  
  return(data.frame(RPS.dynamic=RPS.dynamic, RPS.bt=RPS.bt))
}


## All predictions since mid-season 38

dates <- serieA$Date %>%
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

dynamic.results <- sim.results %>% map(extract,"RPS.dynamic") %>% unlist
names(dynamic.results) <- NULL

bt.results <- sim.results %>% map(extract,"RPS.bt") %>% unlist
names(bt.results) <- NULL

end.day <- dates[end.date]
observed.games <- serieA %>% filter(Date <= end.day)
n.observed.games <- nrow(observed.games)

probs.observed <- observed.games %>%
  group_by(Result) %>%
  summarize(Prob = n()) %>%
  mutate(Prob = Prob/n.observed.games) %>%
  .$Prob


games.to.predict <- serieA %>% filter(Date > end.day)
n.games.to.predict <- nrow(games.to.predict)

probs.observed.matrix <- matrix(rep(probs.observed,n.games.to.predict),nrow=n.games.to.predict,byrow = T)

game.results <- games.to.predict$Result

empirical.results <- RPS(game.results,probs.observed.matrix)

sim.data <- data.frame(Dynamic=dynamic.results,BT=bt.results,Empirical=empirical.results) %>%
  gather(key="Simulation",value="Result")

ggplot(data=sim.data,aes(x=Simulation,y=Result)) +
  geom_boxplot() +
  theme_bw()

