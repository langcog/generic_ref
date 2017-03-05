rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
source("~/Projects/R/Ranalysis/useful_dplyr.R") # from github.com/langcog/Ranalysis
library(directlabels)

original = read.csv("testing.csv")
new1 = read.csv("Subject-Agreement-Cleaned-Parsed.csv",stringsAsFactors=FALSE)
new1 = new1[,!(names(new1) == 'Answer.56')]
new2 = read.csv("Subject-Agreement-Parsed-2.csv",stringsAsFactors=FALSE)

new = rbind(new1,new2)

subset(new,type=='target' & !(sentence %in% original$sentence))$sentence

# Data Cleanup
new[503,]$sentence = 'The towels were left, wet and soggy, on the bathroom floor.'
new[507,]$sentence = 'Pigs are, in fact, very clean animals. '
new[876,]$sentence = '"A cow goes ""moo."""'
new[1969,]$sentence = '"A cow goes ""moo."""'
new[2172,]$sentence = '"The foxes say ""Ring-ding-ding-ding-dingeringeding""!"'
new[2234,]$sentence = '"The foxes say ""Ring-ding-ding-ding-dingeringeding""!"'

original = subset(original,select=c('sentence','response'))
original$response = ifelse(original$response == 'generic',1,0)

d = subset(new,type=='target',select=c('sentence','response'))
d$response = ifelse(d$response == 1, 1, 0)
d$original.response = original[match(d$sentence,original$sentence),2]

d %>%
  group_by(sentence) %>%
  mutate(n = n()) %>%
  filter(n>1) %>%
  ungroup() %>%
  mutate(agree = response == original.response) %>%
  group_by(sentence) %>%
  summarise(agree = mean(agree)) %>%
  ungroup() %>%
  summarise(mean(agree))