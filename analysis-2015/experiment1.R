rm(list=ls())
source("~/Documents/Programming/R/useful.R") # from github.com/langcog/Ranalysis
source("~/Documents/Programming/R/useful_dplyr.R") # from github.com/langcog/Ranalysis
library(directlabels)

d = read.csv("~/Documents/Linguistics/Generics/Experiments/generic_ref/data/Results 10-3-parsed.csv")

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
### 'Respone' column of original CSV includes both strings for part 1 of experiment and integers for part 2. `Ratings' column is inteded to only include integers from part 2. When converting these to numeric, R added 11 to each number for unclear reasons. 

### HISTOGRAMS
qplot(ratings, facets = definiteness ~ number, data=judgments)

### PLOT WITHOUT ANIMACY
mss <- ddply(judgments, .(definiteness,number,WorkerId), summarise,
            ratings = mean(ratings))

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

pdf("cogsci/figures/e1.pdf", width=4, height=3)
qplot(definiteness, mean, col=number, 
      group=number, 
      ymin=mean - cil, ymax=mean + cih,
      geom=c("line","linerange"),data=ms) + 
  ylim(1,5) + 
  ylab("Mean Genericity Rating") +
  xlab("Definiteness") +
  theme_classic() +
  scale_colour_manual(values=c("darkgray","black"), guide=FALSE) + 
  geom_dl(aes(label=number), method=list("last.qp",hjust=-.15, cex=.8))
dev.off()

mssa <- ddply(judgments, .(animacy,WorkerId), summarise,
             ratings = mean(ratings))

ms <- ddply(mssa, .(animacy), summarise,
            mean = mean(ratings),
            cil = ci.low(ratings),
            cih = ci.high(ratings))

ms$animacy <- factor(ms$animacy, 
                    levels = c("animate","inanimate"),
                    labels = c("Animate","Inanimate"))

ms$animacy = relevel(ms$animacy,"Inanimate")

pdf("~/e1_anim.pdf", width=4, height=3)
qplot(animacy, mean, group=1,
      ymin=mean - cil, ymax=mean + cih,
      geom=c("line","linerange"),data=ms) + 
  ylim(1,5) + 
  ylab("Mean Genericity Rating") +
  xlab("Animacy") +
  theme_classic() +
  scale_colour_manual(values=c("black"), guide=FALSE)
dev.off()



qplot(definiteness, mean, fill=number, 
      group=number, stat="identity",
      position=position_dodge(width=.9),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("bar","linerange"),data=ms) + 
  ylim(0,5) + 
  ylab("Mean Genericity Rating") +
  xlab("Definiteness") +
  scale_fill_manual(values=c("darkgray","black"))

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


sents <- judgments %>% 
  group_by(full_sentence, animacy, definiteness, number) %>%
  summarise(n = n(),
            response = mean(as.numeric(response) - 11))

quartz()

pdf("cogsci/figures/e1_norming.pdf", width=4, height=3)
qplot(response, facets = definiteness ~ number, 
      data=sents) +
  ylab("Count") + 
  xlab("Genericity Rating") + 
  theme_classic() 
dev.off()
