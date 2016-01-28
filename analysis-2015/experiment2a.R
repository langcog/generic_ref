rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
# source("~/Documents/Programming/R/useful.R")
d = read.csv("data/Results-11-17-parsed.csv")
# d = read.csv("~/Documents/Linguistics/Generics/Experiments/generic_ref/data/Results-11-17-parsed.csv")
library(directlabels)

### DATA CLEANUP
colnames(d)[3] = 'language'

d = d[d$language != '"Arabic"' & d$language != '"mandarin"' & d$language != '"Spanish"',]

levels(d$language) = droplevels(d$language)

targets = subset(d,type=='target')
judgments = subset(d, type=='judgment')

judgment.mean.rt= mean(judgments$rt)
judgment.sd.rt = sd(judgments$rt)
judgments = subset(judgments, (judgments$rt - judgment.mean.rt) < abs(2 * judgment.sd.rt))
judgments$ratings = as.numeric(judgments$response) - 36 # ????
### 'Respone' column of original CSV includes both strings for part 1 of experiment and integers for part 2. `Ratings' column is inteded to only include integers from part 2. When converting these to numeric, R added 11 to each number for unclear reasons. 

### HISTOGRAMS
qplot(ratings, facets = definiteness ~ number, data=judgments)
qplot(ratings, facets = definiteness ~ animacy, data=judgments)

### NOW WITH IMAGE
mssa <- ddply(judgments, .(definiteness,number,image,WorkerId), summarise,
              ratings = mean(ratings))

ms <- ddply(mssa, .(definiteness,number,image), summarise,
             mean = mean(ratings),
             cil = ci.low(ratings),
             cih = ci.high(ratings))

ms$definiteness <- factor(ms$definiteness, 
                          levels = c("definite","indefinite"),
                          labels = c("Definite","Indefinite"))
ms$number <- factor(ms$number, 
                    levels = c("singular","plural"),
                    labels = c("Singular","Plural"))
ms$image <- factor(ms$image, 
                    levels = c("match","mismatch"),
                    labels = c("Match","Mismatch"))

pdf("cogsci/figures/e2a.pdf", width=4, height=3)
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


### NOW WITH IMAGE and Animacy
mssa <- ddply(judgments, .(definiteness,number,image,animacy,WorkerId), summarise,
              ratings = mean(ratings))

msa <- ddply(mssa, .(definiteness,number,image,animacy), summarise,
             mean = mean(ratings),
             cil = ci.low(ratings),
             cih = ci.high(ratings))

qplot(definiteness, mean, fill=number, 
      facets=animacy~image,
      group=number, stat="identity",
      position=position_dodge(width=.9),
      ymin=mean - cil, ymax=mean + cih,
      geom=c("bar","linerange"),data=msa)


### MODEL WE SHOULD BE FITTING
mod <- lmer(ratings ~ animacy * definiteness * number * image + 
              (animacy * definiteness * number * image| WorkerId) + 
              (definiteness * number * image | subject), data=judgments)

### FASTER MODEL
mod <- lmer(ratings ~ animacy * definiteness * number * image + 
              (animacy + definiteness + number + image | WorkerId) + 
              (definiteness + number + image | subject), data=judgments)

