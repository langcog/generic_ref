rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
d = read.csv("data/Results 1-6-parsed.csv")

colnames(d)[3] = 'language'
colnames(d)[4] = 'feedback'

### DATA CLEANUP
d = d[d$language != '\"Italian\"' & d$language != '\"korean\"' & d$language != '\"Punjabi\"' & d$language != '\"Russian\"' & d$language != '\"Spanish\"',]
levels(d$language) = droplevels(d$language)

d = d[d$sentence_id != 1177 & d$sentence_id != 2238 & d$sentence_id != 641,] # Three Turker-generated sentences were removed because they were possibly offensive. They were replaced with dummy sentences using the same subject. 

targets = subset(d,type=='target')

mean.rt= mean(targets$rt)
sd.rt = sd(targets$rt)
targets = subset(targets, (targets$rt - mean.rt) < abs(2 * sd.rt))

### HISTOGRAMS
qplot(response, facets = definiteness ~ number, data=targets)
qplot(response, facets = definiteness ~ animacy, data=targets)


mssa <- ddply(targets, .(definiteness,number,image,WorkerId), summarise,
              response = mean(response))

ms <- ddply(mssa, .(definiteness,number,image), summarise,
             mean = mean(response),
             cil = ci.low(response),
             cih = ci.high(response))


ms$definiteness <- factor(ms$definiteness, 
                          levels = c("definite","indefinite"),
                          labels = c("Definite","Indefinite"))
ms$number <- factor(ms$number, 
                    levels = c("singular","plural"),
                    labels = c("Singular","Plural"))
ms$image <- factor(ms$image, 
                   levels = c("match","mismatch"),
                   labels = c("Match","Mismatch"))

pdf("cogsci/figures/e2b.pdf", width=4, height=3)
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