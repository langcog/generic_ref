rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
source("~/Projects/R/Ranalysis/useful_dplyr.R") # from github.com/langcog/Ranalysis
library(directlabels)

d = read.csv("../data-2016/Tense-Experiment-Parsed.csv")

### DATA CLEANUP
colnames(d)[3] = 'language'

targets = subset(d,type=='target')

targets.mean.rt = mean(targets$rt)
targets.sd.rt = sd(targets$rt)
targets = subset(targets, (targets$rt - targets.mean.rt) < abs(2 * targets.sd.rt))

targets$response = ifelse(targets$response == 1, 1,0)

### PLOT WITHOUT ANIMACY
mss <- ddply(targets, .(definiteness,number,tense,WorkerId), summarise,
            ratings = mean(response))

ms <- ddply(mss, .(definiteness,number,tense), summarise,
      mean = mean(ratings),
      cil = ci.low(ratings),
      cih = ci.high(ratings))

ms$definiteness <- factor(ms$definiteness, 
                          levels = c("definite","indefinite"),
                          labels = c("Definite","Indefinite"))
ms$number <- factor(ms$number, 
                          levels = c("plural","singular"),
                          labels = c("Plural","Singular"))
ms$tense <- factor(ms$tense, 
                    levels = c("past","present"),
                    labels = c("Past","present"))

pdf("../cogsci-2016/figures/tense.pdf", width=4, height=3)
qplot(tense, mean, col=number, lty=definiteness,
      group=interaction(number,definiteness),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("line","pointrange"), data=ms) +
  geom_dl(aes(label=interaction(number, definiteness)), 
          method=list("last.qp", cex=.8, hjust=-.15)) + 
  ylim(c(0,1)) + 
  ylab("Percent of Sentences Judged Generic") + 
  xlab("Tense") + 
  theme_classic() + 
  scale_colour_manual(values=c("darkgray","black"), guide=FALSE) + 
  scale_linetype_discrete(guide=FALSE)
dev.off()

mod = glmer(response ~ animacy * definiteness * number * tense
            + (tense + definiteness + number | WorkerId) + (tense|subject), 
            data=targets,family='binomial',control = glmerControl(optimizer = 'bobyqa'))

summary(mod)

