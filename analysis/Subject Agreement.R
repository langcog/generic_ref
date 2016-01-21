rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R") # from github.com/langcog/Ranalysis
source("~/Projects/R/Ranalysis/useful_dplyr.R") # from github.com/langcog/Ranalysis
library(directlabels)

original = read.csv("../data/testing.csv")
new = read.csv("../data/Subject-Agreement-Cleaned-Parsed.csv")

original = subset(original,select=c('sentence','response'))
original$response = ifelse(original$response == 'generic',1,0)
new = subset(new,type=='target',select=c('sentence','response'))
new$response = ifelse(new$response == 1, 1, 0)

d = rbind(original,new)


d %>%
  group_by(sentence) %>%
  mutate(n = n()) %>%
  filter(n>1) %>%
  group_by(sentence) %>%
  mutate(agree = ifelse(sum(response) >= n/2,sum(response) /n, 1 - sum(response) /n)) %>%
  ungroup() %>%
  summarise(agree = mean(agree))