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

### NOW WITH IMAGE AND WITHOUT ANIMACY
mssa <- ddply(targets, .(definiteness,number,image,WorkerId), summarise,
              response = mean(response))

ms <- ddply(mssa, .(definiteness,number,image), summarise,
             mean = mean(response),
             cil = ci.low(response),
             cih = ci.high(response))

pdf("cogsci/figures/e3.pdf", width=4, height=3)
qplot(image, mean, col=number, lty=definiteness,
      group=interaction(number,definiteness),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("line","pointrange"), data=ms) +
  geom_dl(aes(label=interaction(number, definiteness)), 
          method=list("last.qp", cex=.8, hjust=-.15)) + 
  ylim(c(1,5)) + 
  ylab("Mean Genericity Rating") + 
  xlab("Picture/Plurality Relationship") + 
  theme_classic() + 
  scale_colour_manual(values=c("darkgray","black"), guide=FALSE) + 
  scale_linetype_discrete(guide=FALSE)
dev.off()








qplot(image, mean, col=definiteness, lty=number, 
      group=interaction(definiteness,number),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("line", "linerange"),     
      position=position_dodge(width=.05),
      data=msa) + 
  ylim(c(1,5)) + 
  ylab("Mean Genericity Rating")

qplot(interaction(definiteness,number), mean, fill=image, 
      position="dodge", stat="identity",
      geom="bar",     
      data=msa) + 
  geom_linerange(aes(ymin=mean - cil, ymax=mean + cih),
                 position=position_dodge(width=.9)) +
  ylab("Mean Genericity Rating") + 
  ylim(c(0,5))

qplot(image, mean, fill=interaction(definiteness,number), 
      position="dodge", stat="identity",
      geom="bar",     
      data=msa) + 
  geom_linerange(aes(ymin=mean - cil, ymax=mean + cih),
                 position=position_dodge(width=.9)) +
  ylab("Mean Genericity Rating") + 
  ylim(c(0,5))
### MODEL WE SHOULD BE FITTING
mod <- lmer(response ~ image * definiteness * number + 
              (image | WorkerId), 
            data=targets)


mod2 <- lmer(response ~ image + definiteness + number + 
              (image | WorkerId), 
            data=targets)

anova(mod,mod2)

mod3 <- lmer(response ~ image + (image | WorkerId), 
             data=targets)

anova(mod,mod3)

summary(mod3)

### sentence by sentence
library(dplyr)

sms <- targets %>% 
  group_by(sentence, definiteness, number, image) %>%
  summarise(response = mean(response)) %>%
  spread(image, response) %>%
  mutate(mismatch_diff = mismatch - match) 
  
qplot(mismatch_diff, facets= number ~ definiteness, 
      binwidth=.25,
      data=sms)

as.data.frame(sms %>% top_n(n = 10, wt = mismatch_diff))
sms %>% top_n(n = 10, wt = -mismatch_diff)
