rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
source("~/Projects/R/Ranalysis/useful_dplyr.R") # from github.com/langcog/Ranalysis
library(directlabels)

d = read.csv("../data-2016/Experiment-1-Independent-Judgments.csv")

levels(d$response.factor) = factor(c(1,0))

mod = glmer(response.factor ~ animacy * definiteness * number + 
             (animacy + definiteness + number | WorkerId) + 
             (definiteness + number | subject), data=d,family='binomial',
            control = glmerControl(optimizer = "bobyqa"))

d$response = ifelse(d$response == 1, 1,0)

### PLOT WITHOUT ANIMACY
mss <- ddply(d, .(definiteness,number,WorkerId), summarise,
             ratings = mean(response))

ms <- ddply(mss, .(definiteness,number), summarise,
            mean = mean(ratings),
            cil = ci.low(ratings),
            cih = ci.high(ratings))

ms$definiteness <- factor(ms$definiteness, 
                          levels = c("definite","indefinite"),
                          labels = c("Definite","Indefinite"))
ms$number <- factor(ms$number, 
                    levels = c("singular","plural"),
                    labels = c("Singular","Plural"))

pdf("../cogsci-2016/figures/e1.pdf", width=4, height=3)
qplot(definiteness, mean, col=number, 
      group=number, 
      ymin=mean - cil, ymax=mean + cih,
      geom=c("line","linerange"),data=ms) + 
  ylim(0,1) + 
  ylab("Percentage Rated Generic") +
  xlab("Definiteness") +
  theme_classic() +
  scale_colour_manual(values=c("darkgray","black"), guide=FALSE) + 
  geom_dl(aes(label=number), method=list("last.qp",hjust=-.15, cex=.8))
dev.off()
