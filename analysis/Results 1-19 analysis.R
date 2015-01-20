rm(list=ls())
source("~/Projects/R/Ranalysis/useful_dplyr.R") # from github.com/langcog/Ranalysis
d = read.csv("data/Results 1-19-parsed.csv")

colnames(d)[3] = 'language'
colnames(d)[4] = 'feedback'

### DATA CLEANUP
d = d[d$language != '\"Bosnian\"' & d$language != '\"english/dutch\"' & d$language != '\"hindi\"' & d$language != '\"spanish\"' & d$language != '\"Tagalog\"',]
levels(d$language) = droplevels(d$language)

targets <- d %>% filter(type=="target")

mean.rt= mean(targets$rt)
sd.rt = sd(targets$rt)
targets = subset(targets, (targets$rt - mean.rt) < abs(2 * sd.rt))

### HISTOGRAMS
qplot(response, facets = definiteness ~ number, data=targets)
qplot(response, facets = definiteness ~ animacy, data=targets)

### PLOT WITHOUT ANIMACY
mss <- ddply(targets, .(definiteness,number,WorkerId), summarise,
             response = mean(response))

ms <- ddply(mss, .(definiteness,number), summarise,
            mean = mean(response),
            cil = ci.low(response),
            cih = ci.high(response))

qplot(definiteness, mean, fill=number, 
      group=number, stat="identity",
      position=position_dodge(width=.9),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("bar","linerange"),data=ms)

### NOW WITH ANIMACY

mssa <- ddply(targets, .(definiteness,number,animacy,WorkerId), summarise,
              response = mean(response))

msa <- ddply(mssa, .(definiteness,number,animacy), summarise,
             mean = mean(response),
             cil = ci.low(response),
             cih = ci.high(response))

qplot(definiteness, mean, fill=number, 
      facets=.~animacy,
      group=number, stat="identity",
      position=position_dodge(width=.9),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("bar","linerange"),data=msa)

### NOW WITH IMAGE AND WITHOUT ANIMACY
mssa <- ddply(targets, .(definiteness,number,image,WorkerId), summarise,
              response = mean(response))

msa <- ddply(mssa, .(definiteness,number,image), summarise,
             mean = mean(response),
             cil = ci.low(response),
             cih = ci.high(response))

qplot(definiteness, mean, fill=number, 
      facets=.~image,
      group=number, stat="identity",
      position=position_dodge(width=.9),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("bar","linerange"),data=msa)

### NOW WITH IMAGE AND ANIMACY
mssa <- ddply(targets, .(definiteness,number,image,animacy,WorkerId), summarise,
              response = mean(response))

msa <- ddply(mssa, .(definiteness,number,image,animacy), summarise,
             mean = mean(response),
             cil = ci.low(response),
             cih = ci.high(response))

qplot(definiteness, mean, fill=number, 
      facets=animacy~image,
      group=number, stat="identity",
      position=position_dodge(width=.9),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("bar","linerange"),data=msa)

### MODEL WE SHOULD BE FITTING
mod <- lmer(response ~ animacy * definiteness * number + 
              (animacy * definiteness * number | WorkerId) + 
              (definiteness * number | subject), data=targets)

### FASTER MODEL
mod <- lmer(response ~ animacy * definiteness * number * image + 
              (animacy + definiteness + number  | WorkerId) + 
              (definiteness + number | subject), data=targets)

### FASTER MODEL WITHOUT ANIMACY
mod <- lmer(response ~ definiteness * number * image + 
              (definiteness + number  | WorkerId) + 
              (definiteness + number | subject), data=targets)

mssa <- ddply(targets, .(definiteness,number,image,WorkerId), summarise,
              response = mean(response))

msa <- ddply(mssa, .(definiteness,number,image), summarise,
             mean = mean(response),
             cil = ci.low(response),
             cih = ci.high(response))

qplot(image, mean, color=number, 
      facets=.~definiteness,
      group=number, stat="identity",
      ymin=mean - cil, ymax=mean + cih,
      geom=c("line","linerange"),data=msa)