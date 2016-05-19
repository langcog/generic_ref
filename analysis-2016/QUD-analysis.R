rm(list=ls())
source("~/Documents/Programming/R/useful.R") # from github.com/langcog/Ranalysis
source("~/Documents/Programming/R/useful_dplyr.R") # from github.com/langcog/Ranalysis
library(directlabels)

d = read.csv("~/Documents/Linguistics/Generics/Experiments/QUD/Results/QUD-result-parsed.csv")

colnames(d)[3] = 'language'
levels(d$language)
d = subset(d,language != "\"Croatian\"" & language != "\"English and Korean\"" & language != "\"English, Armenian\"" & language != "\"English, Greek\"")
levels(d$language) = droplevels(d$language)

mean.rt = mean(log(d$rt))
sd.rt = sd(log(d$rt))
d = subset(d, abs(log(rt) - mean.rt) < 3 * sd.rt)

targets = subset(d, type == 'target')

targets$tense = factor(targets$tense,levels(targets$tense)[c(2,1,3)])

mss <- ddply(targets, .(definiteness,number,tense,context,WorkerId), summarise,
             ratings = mean(genericity))

ms <- ddply(mss, .(definiteness,number,tense,context), summarise,
            mean = mean(ratings),
            cil = ci.low(ratings),
            cih = ci.high(ratings))

qplot(context, mean, col=number, lty=definiteness,
      group=interaction(number,definiteness),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("line","pointrange"), data=ms) +
  geom_dl(aes(label=interaction(number, definiteness)), 
          method=list("last.qp", cex=.8, hjust=-.15)) + 
  facet_grid(~tense) +
  ylim(c(0,1)) + 
  ylab("Percent Rated Generic") + 
  xlab("Context") + 
  theme_classic() + 
  scale_colour_manual(values=c("deepskyblue","deeppink"), guide=FALSE) + 
  scale_linetype_discrete(guide=FALSE)

mod1 = glm(genericity ~ number * definiteness * tense * context,
          family='binomial',
          data=targets)

mod2 = glmer(genericity ~ number * definiteness * tense * context
            + (number * definiteness * tense * context | WorkerId) 
            + (number * definiteness * tense * context |subject), 
            data=targets,
            family='binomial',
            control = glmerControl(optimizer = 'bobyqa'))