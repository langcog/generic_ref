rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
source("~/Projects/R/Ranalysis/useful_dplyr.R") # from github.com/langcog/Ranalysis
library(directlabels)
require(gridExtra)
library(plyr)

d <- read.csv("data/Results 4-25-parsed.csv")

colnames(d)[3] = 'language'
colnames(d)[4] = 'experiment.type'

levels(d$language)

d = d[d$language != '"bosnians"' & d$language != '"Spanish"',]
levels(d$language) = droplevels(d$language)

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
      geom="bar",data=ms)

mss_items <- ddply(targets, .(sentence_id,experiment.type,WorkerId), summarise,
                   percent.generic = mean((-1*response)+2))

ms_items <- ddply(mss_items, .(sentence_id,experiment.type), summarise,
                  percent.generic = mean(percent.generic))
ms_items = melt(ms_items,id=c('sentence_id','experiment.type'))
ms_items = dcast(ms_items,sentence_id ~ variable+experiment.type,mean)
ms_items$diff = ms_items$percent.generic_1 - ms_items$percent.generic_2
ms_items = merge(ms_items,unique(targets[,9:10]),by='sentence_id')

# Scatter plot
qplot(x=percent.generic_1,y=percent.generic_2,data=ms_items)

# Add labels
qplot(x=percent.generic_1,y=percent.generic_2,data=ms_items,label=sentence_id,geom='text')

# Correlation = 0.993
cor(ms_items$percent.generic_1,ms_items$percent.generic_2)

attach(ms_items)

# Sentences that were more often labelled generic in  'zebras vs. specific zebras' condition
ms_items[order(diff,decreasing=T),][1:10,4:5]

# Sentences that were more often labelled generic in  'zebras in general vs. specific zebras' condition
ms_items[order(diff,decreasing=F),][1:10,4:5]