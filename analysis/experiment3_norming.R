rm(list=ls())
source("~/Projects/R/Ranalysis/useful_dplyr.R") # from github.com/langcog/Ranalysis
d = read.csv("data/Results 1-13-parsed.csv")

### DATA CLEANUP
d = d[d$language != '\"Italian\"' & d$language != '\"korean\"' & d$language != '\"Punjabi\"' & d$language != '\"Russian\"' & d$language != '\"Spanish\"',]
levels(d$language) = droplevels(d$language)

targets <- d %>% filter(type=="target")

### HISTOGRAMS
qplot(response, facets = definiteness ~ number, data=targets)

### find controversial sentences
sents <- targets %>% 
  group_by(sentence, animacy, definiteness, number) %>%
  summarise(n = n(),
    response = mean(response))

quartz()

pdf("cogsci/figures/e3_norming.pdf", width=4, height=3)
qplot(response, facets = definiteness ~ number, 
      data=sents) +
  ylab("Count") + 
  xlab("Mean Genericity Rating") + 
  theme_classic() 
dev.off()





## look at distribution
sents %>% group_by(definiteness, number, add=FALSE) %>% 
  filter(response > 2 & response < 4) %>%
  summarise(n=n())

droplevels(as.data.frame(sents %>% filter(response > 2 & response < 4))$sent)
