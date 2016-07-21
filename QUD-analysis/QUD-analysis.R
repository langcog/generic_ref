rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
source("~/Projects/R/Ranalysis/useful_dplyr.R") # from github.com/langcog/Ranalysis
library(directlabels)
library(gridExtra)
library(glmnet)
library(langcog)

d1 = read.csv("../QUD-data/QUD-results-peacocks.csv")
d2 = read.csv("../QUD-data/QUD-results-peacocks-in-general.csv")
d3 = read.csv("../QUD-data/QUD-results-felicity.csv")

d1$experiment.type = 'peacocks'
d2$experiment.type = 'peacocks in general'
d3$experiment.type = 'felicity'

colnames(d1)[which(colnames(d1) == 'genericity')] = 'response'
colnames(d1)[which(colnames(d1) == 'Answer.40')] = 'language'

colnames(d2)[which(colnames(d2) == 'genericity')] = 'response'
colnames(d2)[which(colnames(d2) == 'Answer.40')] = 'language'

colnames(d3)[which(colnames(d3) == 'naturalness')] = 'response'
colnames(d3)[which(colnames(d3) == 'Answer.41')] = 'language'

d = rbind(d1,d2,d3)

d = subset(d,language != "\"Croatian\"" & language != "\"English and Korean\"" & language != "\"English, Armenian\"" & language != "\"English, Greek\"" & language != "\"spanish\"")
levels(d$language) = droplevels(d$language)

mean.rt = mean(log(d$rt))
sd.rt = sd(log(d$rt))
d = subset(d, abs(log(rt) - mean.rt) < 3 * sd.rt)

d = subset(d,!grepl('pigeons bobs',sentence))

# Remove sentences with typos.

d = subset(d,WorkerId != 'A1FF5C5BXODWSY' & WorkerId != 'A1KISJJJMHEFVP')

# Remove workers who participated in two experiments

targets = subset(d, type=='target')

targets$tense = factor(targets$tense,levels(targets$tense)[c(2,1,3)])

mss = ddply(targets, 
            .(definiteness,number,tense,context,experiment.type,WorkerId),
            summarise,
            ratings = mean(response))

ms = ddply(mss, 
           .(definiteness,number,tense,context,experiment.type), 
           summarise,
           mean.ratings = mean(ratings),
           cil.ratings = ci.low(ratings),
           cih.ratings = ci.high(ratings))

plot = qplot(context, 
             mean.ratings, 
             col=number, 
             lty=definiteness,
             group=interaction(number,definiteness),
             ymin=mean.ratings - cil.ratings, 
             ymax=mean.ratings + cih.ratings,
             geom=c("line","pointrange"),
             data=ms) + 
  facet_wrap(tense ~ experiment.type,scales='free_y') + 
  geom_dl(aes(label=interaction(number, definiteness)), 
          method=list("last.qp", cex=1, hjust=-.05)) + 
  labs(title = 'QUD Experiments') +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text.x=element_text(size=12)) + 
  geom_line(size=1.2) +
  scale_color_solarized(guide=FALSE) +
  scale_linetype_manual(values=c("solid", "dashed"),guide=FALSE)

pdf('qud-plot.pdf',24,12)
plot
dev.off()

targets = subset(d,type=='target')

mod1 = glm(response ~ number * definiteness * tense * context * animacy,
           data=subset(targets,experiment.type=='peacocks'),
           family='binomial')
summary(mod1)
# Model for experiment 1 with no random effects. 
# Significant predictors: Definiteness, Tense, Definiteness:Tense:Animacy interaction

mod2 = glmer(response ~ number * definiteness * tense * context * animacy 
             + (number + definiteness + tense + context + animacy | subject) 
             + (number + definiteness + tense + context + animacy | WorkerId),
             data=subset(targets,experiment.type=='peacocks'),
             family='binomial')
# Does not converge.

mod3 = glmer(response ~ number * definiteness * tense * context * animacy 
             + (1 | subject) 
             + (1 | WorkerId),
             data=subset(targets,experiment.type=='peacocks'),
             family='binomial',
             control = glmerControl('bobyqa'))
# Does not converge.

mod4 = glm(response ~ number * definiteness * tense * context * animacy,
           data=subset(targets,experiment.type=='peacocks in general'),
           family='binomial')
summary(mod4)
# Model for experiment 2 with no random effects. 
# Significant predictors: Tense, Animacy, Number:Animacy interaction, Context:Animacy interaction, Number:Tense:Animacy interaction, Number:Context:Animacy interaction, Tense:Context:Animacy interaction

mod5 = glmer(response ~ number * definiteness * tense * context * animacy 
             + (number + definiteness + tense + context + animacy | subject) 
             + (number + definiteness + tense + context + animacy | WorkerId),
             data=subset(targets,experiment.type=='peacocks in general'),
             family='binomial')
# Does not converge.

mod6 = glmer(response ~ number * definiteness * tense * context * animacy 
             + (1 | subject) 
             + (1 | WorkerId),
             data=subset(targets,experiment.type=='peacocks in general'),
             family='binomial',
             control = glmerControl('bobyqa'))
# Does not converge.

mod7 = lm(response ~ number * definiteness * tense * context * animacy,
           data=subset(targets,experiment.type=='felicity'))
summary(mod7)
# Model for experiment 3 with no random effects. 
# Significant predictors: Definiteness, Animacy, Definiteness:Tense interaction, Definiteness:Context interaction, Tense:Context interaction, Context:Animacy interaction, Definiteness:Context:Animacy interaction, Tense:Context:Animacy interaction

mod8 = lmer(response ~ number * definiteness * tense * context * animacy 
             + (number + definiteness + tense + context + animacy | subject) 
             + (number + definiteness + tense + context + animacy | WorkerId),
             data=subset(targets,experiment.type=='felicity'))
# Takes very long time to converge (if it converges at all)

mod9 = lmer(response ~ number * definiteness * tense * context * animacy 
            + (1 | subject) 
            + (1 | WorkerId),
            data=subset(targets,experiment.type=='felicity'))
# Model for experiment 3 with random intercepts.
# Significant predictors (|t| > 2): Definiteness, Definiteness:Tense interaction, Definiteness:Context interaction, Tense:Context interaction, Context:Animacy interaction, Tense:Context:Animacy interaction

# Does felicity predict genericity ratings?

# First calculate mean felicity ratings for each sentence in each experimental context
item.felicity = ddply(subset(d,experiment.type=='felicity'), .(sentence,context), summarise, item.naturalness = mean(response))

targets = targets %>% merge(item.felicity)

# Now see if felicity is a significant predictor of genericity ratings.

mod1.with.felicity = glm(response ~ number * definiteness * tense * context * animacy * item.naturalness,data=subset(targets,experiment.type=='peacocks'),family='binomial')
# Error: fitted probabilities 0 and 1

mod1.with.felicity = glm(response ~ number * definiteness * tense * context * animacy + item.naturalness,data=subset(targets,experiment.type=='peacocks'),family='binomial')
summary(mod1.with.felicity)
# Felicity not a significant predictor

mod4.with.felicity = glm(response ~ number * definiteness * tense * context * animacy * item.naturalness,data=subset(targets,experiment.type=='peacocks in general'),family='binomial')
# Error: fitted probabilities 0 and 1

mod4.with.felicity = glm(response ~ number * definiteness * tense * context * animacy + item.naturalness,data=subset(targets,experiment.type=='peacocks in general'),family='binomial')
summary(mod4.with.felicity)
# Felicity not a significant predictor