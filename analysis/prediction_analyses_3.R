rm(list=ls())
library(langcog)
library(ggplot2)
library(dplyr)
library(boot)

d <- read.csv("../data/for_prediction_2.csv")

## now average for each sentence, our unit of prediction. 

bys <- d %>% 
  filter(type=="target") %>%
  group_by(animacy, number, definiteness, subject, tense.modal, 
           aspect, be.have, sentence.length, sentence) %>%
  summarise(generic = mean(response == 1)) %>%
  filter(generic == 1 | generic == 0) # get rid of ambigous or now

# models
summary(glm(generic ~ number * definiteness + tense.modal + aspect + sentence.length + be.have, data = bys))

mean.fun <- function(dat, idx) mean(dat[idx], na.rm = TRUE)

upper = function(x){
  boot.out = boot(data=x,statistic=mean.fun,R=1000)
  c(mean(boot.out$t) + 1.96 * sd(boot.out$t))
}

lower = function(x){
  boot.out = boot(data=x,statistic=mean.fun,R=1000)
  c(mean(boot.out$t) - 1.96* sd(boot.out$t))
}

bys <- d %>% 
  filter(type=="target") %>%
  group_by(animacy, number, definiteness, subject, tense.aspect.modal, 
          be.have, sentence.length, sentence) %>%
  summarise(generic = mean(response == 1)) %>%
  filter(generic == 1 | generic == 0) # get rid of ambigous or now

summary1 = summarise(group_by(bys,number,definiteness,tense.aspect.modal), genericity = mean(generic),upper = upper(generic),lower=lower(generic))

qplot(x=tense.aspect.modal,y=genericity,geom='pointrange',ymin=lower,ymax=upper,facets = definiteness~number,data=summary1)

summary2 = summarise(group_by(bys,number,definiteness,be.have), genericity = mean(generic),upper = upper(generic),lower=lower(generic))

qplot(x=be.have,y=genericity,geom='pointrange',ymin=lower,ymax=upper,facets = definiteness~number,data=summary2)
