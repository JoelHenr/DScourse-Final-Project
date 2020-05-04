library(mlogit)
library(mlr)
library(tidyverse)
library(magrittr)
library(clubSandwich)
library(plm)
library(wooldridge)
library(broom)
library(lmtest)
library(estimatr)
library(stargazer)
library(car)
library(dplyr)
library(survival)
library(mfx)
library(bife)
library(glmmML)
library(margins)

# Loading in Data
#nba <-read.csv("nba3.csv")
nba<-read.csv("https://raw.githubusercontent.com/JoelHenr/DScourse-Final-Project/master/nba3.csv")

# Declaring Data as Panel data 
nba.p <- pdata.frame(nba, index=c("team_id","gameorder"))

# Logit Model for home versus away
est.l<-glm(game_result ~ game_location, family = binomial(link='logit'),nba.p)
print(summary(est.l))
stargazer(coeftest(est.l, vcovHC),type = "text")
coeftest(est.l, vcovHC(est.l, type = "HC0", cluster = "group"))

# Fixed Effects (FIGURE 2 - IN THE STUDY)
fe <- clogit(wins~home + strata(gameorder), data =nba.p, method = "exact")
stargazer(fe, type="text")

# Marginal Effects (TABLE 3 - USED IN PAPER)
logitmfx(est.l, atmean=TRUE, data = nba.p, robust = TRUE)
logitmfx(est.l, atmean=FALSE, data = nba.p, robust = TRUE)

# Average Marginal Effects 
margins(est.l)

# Probit Model for home versus away
est.p<-glm(game_result ~ game_location, family = binomial(link='probit'),data=nba.p)
print(summary(est.p))
stargazer(coeftest(est.p, vcovHC), type = "text")
coeftest(est.p, vcovHC(est.p, type = "HC0", cluster = "group"))


# Marginal Effects probit (TABLE 3 - USE IN PAPER)
probitmfx(est.p, atmean=TRUE, data = nba.p, robust = TRUE)
probitmfx(est.p, atmean=FALSE, data = nba.p, robust = TRUE)

# Average Marginal Effects 
margins(est.p)

# Adding more variables logit
est.l2 <- glm(game_result ~ game_location + forecast + pts + elo_i + is_playoffs, family = binomial(link='logit'),data=nba.p)
print(summary(est.l2))
stargazer(coeftest(est.l2, vcovHC), type = "text")

# FE (FIGURE 1 - IN THE STUDY)
fe1 <- clogit(wins~home + forecast + pts + elo_i + is_playoffs + strata(gameorder), data =nba.p, method = "exact")
stargazer(fe1, type="text")

# Marginal Effects logit (TABLE 2 - USE IN PAPER)
logitmfx(est.l2, atmean=TRUE, data = nba.p, robust = TRUE)
logitmfx(est.l2, atmean=FALSE, data = nba.p, robust = TRUE)

# AME
margins(est.l2)

# Adding more variables probit
est.p2 <- glm(game_result ~ game_location + forecast + pts + elo_i + is_playoffs, family = binomial(link='probit'),data=nba.p)
print(summary(est.p2))
stargazer(coeftest(est.p2, vcovHC), type = "text")


# Marginal Effects probit (TABLE 2 - USE IN PAPER)
logitmfx(est.p2, atmean=TRUE, data = nba.p, robust = TRUE)
logitmfx(est.p2, atmean=FALSE, data = nba.p, robust = TRUE)

# AME
margins(est.p2)






