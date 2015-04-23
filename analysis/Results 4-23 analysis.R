rm(list=ls())
source("~/Documents/Programming/R/useful.R") # from github.com/langcog/Ranalysis
source("~/Documents/Programming/R/useful_dplyr.R") # from github.com/langcog/Ranalysis
library(directlabels)
require(gridExtra)
library(plyr)

d <- read.csv("data/Results 4-23-parsed.csv")

colnames(d)[3] = 'language'
colnames(d)[4] = 'experiment.type'

targets <- d %>% filter(type=="target")

mss <- ddply(targets, .(number,definiteness,experiment.type,WorkerId), summarise,
                  percent.generic = mean((-1*response)+2))

ms <- ddply(mss, .(number,definiteness,experiment.type), summarise,
                 percent.generic = mean(percent.generic))

ms$experiment.type = as.factor(ms$experiment.type)
levels(ms$experiment.type) = c('Zebras','Zebras in general')

qplot(definiteness, percent.generic, fill=number,facets=.~experiment.type,
      group=number, stat="identity",
      position=position_dodge(width=.9),
      geom=c("bar"),data=ms)