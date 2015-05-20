rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
source("~/Projects/R/Ranalysis/useful_dplyr.R") # from github.com/langcog/Ranalysis
library(directlabels)
require(gridExtra)
library(plyr)

d <- read.csv("data/Results 5-19-parsed.csv")
d2 <- read.csv("data/Results 5-20-parsed.csv")
d3 <- read.csv("data/Results 5-20 2-parsed.csv")

colnames(d)[3] = 'language'
colnames(d2)[3] = 'language'
colnames(d3)[3] = 'language'

colnames(d)[4] = 'feedback'
colnames(d2)[4] = 'feedback'
colnames(d3)[4] = 'feedback'

d <- rbind(d,d2,d3)
rm(d2)
rm(d3)

levels(d$language)

d = subset(d, language != '"Hindi"' & language != '"Bengali"' & language != '"Korean"' & language != '"laotian"' & language != '"Spanish"')
levels(d$language) = droplevels(d$language)

d = subset(d, abs(rt - mean(d$rt)) < 2 * sd(d$rt))

d$response.factor = as.factor(d$response)
levels(d$response.factor) = c('Generic','Non-generic')

targets <- d %>% filter(type=="target")

mss <- ddply(targets, .(number,definiteness,WorkerId), summarise,
                  percent.generic = mean((-1*response)+2))

ms <- ddply(mss, .(number,definiteness), summarise,
                 percent.generic = mean(percent.generic))

qplot(definiteness, percent.generic, fill=number,
      group=number, stat="identity",
      position=position_dodge(width=.9),
      geom="bar",data=ms)

mod1 = glmer(response.factor ~ definiteness * number * animacy * (definiteness + number + animacy | WorkerId), family=binomial,data = targets)

summary(mod1)

mod = glmer(response.factor ~ definiteness * number * animacy * (definiteness * number * animacy | WorkerId), family=binomial,data = targets)

summary(mod2)