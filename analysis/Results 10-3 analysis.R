d = read.csv("~/Desktop/generics/Results 10-3-parsed.csv")

colnames(d)[3] = 'language'

d = d[d$language != '"Hindi"' & d$language != '"spanish"' & d$language != '"Spanish"',]

levels(d$language) = droplevels(d$language)

targets = subset(d,type=='target')
judgments = subset(d, type=='judgment')

judgment.mean.rt= mean(judgments$rt)
judgment.sd.rt = sd(judgments$rt)

judgments = subset(judgments, (judgments$rt - judgment.mean.rt) < abs(2 * judgment.sd.rt))

judgments$ratings = as.numeric(judgments$response) - 11

source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis

qplot(ratings, facets = definiteness ~ number, data=judgments)

ms <- ddply(judgments, .(definiteness,number), summarise,
      mean = mean(ratings),
      cil = ci.low(ratings),
      cih = ci.high(ratings))

qplot(definiteness, mean, fill=number, 
      group=number, stat="identity",
      position=position_dodge(width=.9),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("bar","linerange"),data=ms)

msa <- ddply(judgments, .(definiteness,number,animacy), summarise,
            mean = mean(ratings),
            cil = ci.low(ratings),
            cih = ci.high(ratings))
qplot(definiteness, mean, fill=number, 
      facets=.~animacy,
      group=number, stat="identity",
      position=position_dodge(width=.9),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("bar","linerange"),data=msa)

mod <- lmer(ratings ~ animacy * definiteness * number + 
       (animacy * definiteness * number | WorkerId), data=judgments)



