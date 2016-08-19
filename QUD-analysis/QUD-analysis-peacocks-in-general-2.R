rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
source("~/Projects/R/Ranalysis/useful_dplyr.R") # from github.com/langcog/Ranalysis
library(directlabels)
library(langcog)

d = read.csv("../QUD-data/QUD-results-peacocks-in-general-2.csv")

exp1 = read.csv("../QUD-data/QUD-results-peacocks.csv")
exp2 = read.csv("../QUD-data/QUD-results-peacocks-in-general.csv")
exp3 = read.csv("../QUD-data/QUD-results-felicity.csv")

ids = c(levels(exp1$WorkerId),levels(exp2$WorkerId),levels(exp3$WorkerId))

colnames(d)[3] = 'language'
d = subset(d,language != "\"Dutch\"" & language != "\"English/Spanish billingual\"" & language != "\"Romanian\"" & language != "\"Spanish\"")
levels(d$language) = droplevels(d$language)

d = subset(d,!(WorkerId  %in% ids))

rm(exp1,exp2,exp3)

mean.rt = mean(log(d$rt))
sd.rt = sd(log(d$rt))
d = subset(d, abs(log(rt) - mean.rt) < 3 * sd.rt)

targets = subset(d, type == 'target')

targets$tense = factor(targets$tense,levels(targets$tense)[c(2,1,3)])

mss <- ddply(targets, .(definiteness,number,tense,context,WorkerId), summarise,
             ratings = mean(genericity), rts= mean(rt))

ms <- ddply(mss, .(definiteness,number,tense,context), summarise,
            mean.ratings = mean(ratings),
            cil.ratings = ci.low(ratings),
            cih.ratings = ci.high(ratings))

y.label = 'Percent Rated Generic'
y.limits = c(0,1)
x.label = 'Context'
plot1 = qplot(context, mean.ratings, col=number, lty=definiteness,
             group=interaction(number,definiteness),
             ymin=mean.ratings - cil.ratings, ymax=mean.ratings + cih.ratings,
             geom=c("line","pointrange"), data=ms) + 
  facet_grid(~tense) + 
  geom_dl(aes(label=interaction(number, definiteness)), 
                               method=list("last.qp", cex=.8, hjust=-.15)) + 
  ylim(y.limits) + 
  ylab(y.label) + 
  xlab(x.label) + 
  theme_classic() +
  scale_color_solarized(guide=FALSE) +
  scale_linetype_manual(values=c("solid", "dashed"),guide=FALSE)

main.effect.mod = glm(genericity ~ number + definiteness + tense + context,
                      family = 'binomial',
                      data = targets)

ms$predictions = predict(main.effect.mod,type='response',newdata=ms)
ms$resid = ms$mean.ratings - ms$predictions

plot2 = qplot(context, resid, col=number, lty=definiteness,
              group=interaction(number,definiteness),
              geom=c('point','line'), data=ms) + 
  facet_grid(~tense) + 
  geom_dl(aes(label=interaction(number, definiteness)), 
          method=list("last.qp", cex=.8, hjust=-.15)) + 
  theme_classic() +
  scale_color_solarized(guide=FALSE) +
  scale_linetype_manual(values=c("solid", "dashed"),guide=FALSE)
plot2

mod1 = glm(genericity ~ animacy * number * definiteness * tense * context,
          family='binomial',
          data=targets)

mod2 = glm(genericity ~ number * definiteness * tense * context,
           family='binomial',
           data=targets)

AIC(mod1,mod2)
BIC(mod1,mod2)

mod2 = glmer(genericity ~ animacy * number * definiteness * tense * context
             + (1 | WorkerId) 
             + (1 |subject), 
             data=targets,
             family='binomial',
             control = glmerControl(optimizer = 'bobyqa'))

mod3 = glmer(genericity ~ animacy * number * definiteness * tense * context
            + (animacy + number + definiteness + tense + context | WorkerId) 
            + (animacy + number + definiteness + tense + context |subject), 
            data=targets,
            family='binomial',
            control = glmerControl(optimizer = 'bobyqa'))