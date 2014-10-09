rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
d = read.csv("data/Results 10-3-parsed.csv")

### DATA CLEANUP
colnames(d)[3] = 'language'

d = d[d$language != '"Hindi"' & d$language != '"spanish"' & d$language != '"Spanish"',]

levels(d$language) = droplevels(d$language)

targets = subset(d,type=='target')
judgments = subset(d, type=='judgment')

judgment.mean.rt= mean(judgments$rt)
judgment.sd.rt = sd(judgments$rt)
judgments = subset(judgments, (judgments$rt - judgment.mean.rt) < abs(2 * judgment.sd.rt))
judgments$ratings = as.numeric(judgments$response) - 11 # ????

### HISTOGRAMS
qplot(ratings, facets = definiteness ~ number, data=judgments)

### PLOT WITHOUT ANIMACY
mss <- ddply(judgments, .(definiteness,number,WorkerId), summarise,
            ratings = mean(ratings))

ms <- ddply(mss, .(definiteness,number), summarise,
      mean = mean(ratings),
      cil = ci.low(ratings),
      cih = ci.high(ratings))

qplot(definiteness, mean, fill=number, 
      group=number, stat="identity",
      position=position_dodge(width=.9),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("bar","linerange"),data=ms)

### NOW WITH ANIMACY
mssa <- ddply(judgments, .(definiteness,number,animacy,WorkerId), summarise,
             ratings = mean(ratings))

msa <- ddply(mssa, .(definiteness,number,animacy), summarise,
            mean = mean(ratings),
            cil = ci.low(ratings),
            cih = ci.high(ratings))

qplot(definiteness, mean, fill=number, 
      facets=.~animacy,
      group=number, stat="identity",
      position=position_dodge(width=.9),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("bar","linerange"),data=msa)


### MODEL WE SHOULD BE FITTING
mod <- lmer(ratings ~ animacy * definiteness * number + 
              (animacy * definiteness * number | WorkerId) + 
              (definiteness * number | subject), data=judgments)

### FASTER MODEL
mod <- lmer(ratings ~ animacy * definiteness * number + 
              (animacy + definiteness + number | WorkerId) + 
              (definiteness + number | subject), data=judgments)




