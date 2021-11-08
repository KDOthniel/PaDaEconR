#* Example 1.1 Individual heterogeneity..

library(AER)
library(tidyverse)

data("Fatalities")
summary(Fatalities)
dim(Fatalities)
attach(Fatalities)
table(state)
length(table(state))


Fatalities$frate<-with(Fatalities, (fatal/pop)*1000)

Fatalities %>% 
  select(state,fatal,pop,frate)

fm<-frate~beertax

mod82<-lm(fm, data = Fatalities, subset = (year==1982)) 
summary(mod82)

mod88<-update(mod82, subset= (year==1988))
summary(mod88)

library(lmtest)
coeftest(mod88)

library(plm)
poolmod<-plm(fm, data = Fatalities, model = "pooling")
summary(poolmod)
coeftest(poolmod)


head(diff(Fatalities$frate,5))
head(Fatalities$frate)


dmod<-plm(diff(frate,5)~diff(beertax,5), Fatalities, 
          model = "pooling")
summary(dmod)


lsdv.fm<-update(fm, .~.+state-1)
lsdvmod<-lm(lsdv.fm, Fatalities)
summary(lsdvmod)


femod<-plm(fm, Fatalities, model = "within")
coeftest(femod)





















































































































































