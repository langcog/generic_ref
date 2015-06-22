rm(list=ls())
library(langcog)
library(ggplot2)
library(dplyr)

d <- read.csv("data/Results 5-19-parsed.csv")
d2 <- read.csv("data/Results 5-20-parsed.csv")
d3 <- read.csv("data/Results 5-20 2-parsed.csv")
# To include tense and aspect as the same factor, import a different CSV file:
#d4 <- read.csv("data/tense_aspect_1.csv")
d4 <- read.csv("data/tense_aspect_2.csv")

colnames(d)[3] = 'language'
colnames(d2)[3] = 'language'
colnames(d3)[3] = 'language'

colnames(d)[4] = 'feedback'
colnames(d2)[4] = 'feedback'
colnames(d3)[4] = 'feedback'

d <- rbind(d,d2,d3)
rm(d2)
rm(d3)

d4[2] = NULL
# If using tense_aspect_1.csv, use the following:
# colnames(d4) = c('sentence_id','tense.aspect')
colnames(d4) = c('sentence_id','tense','aspect')
d = merge(d,d4,by='sentence_id')
rm(d4)

d$sentence.length = 0

for (i in 1:nrow(d)){
  if (d[i,]$definiteness == 'definite' | d[i,]$number == 'singular'){
    d[i,]$sentence.length = length(strsplit(toString(d[i,]$sentence),' ')[[1]]) - length(strsplit(toString(d[i,]$subject),' ')[[1]]) - 1
  } else {
    d[i,]$sentence.length = length(strsplit(toString(d[i,]$sentence),' ')[[1]]) - length(strsplit(toString(d[i,]$subject),' ')[[1]])
  }
}

levels(d$language)

# If using tense_aspect_1.csv, it is necessary to filter out sentences with the tense.aspect values "other" and "multiple":
# d = subset(d, language != '"Hindi"' & language != '"Bengali"' & language != '"Korean"' & language != '"laotian"' & language != '"Spanish"' & tense.aspect != 'other' & tense.aspect != 'multiple')
d = subset(d, language != '"Hindi"' & language != '"Bengali"' & language != '"Korean"' & language != '"laotian"' & language != '"Spanish"')
d$language = factor(d$language)
# If using tense_aspect_1.csv, use the following line:
# d$tense.aspect = factor(d$tense.aspect)
d$tense = factor(d$tense)
d$aspect = factor(d$aspect)

d = subset(d, abs(rt - mean(d$rt)) < 2 * sd(d$rt))

d$response.factor = as.factor(d$response)
levels(d$response.factor) = c('Generic','Non-generic')

ms <- d %>% filter(type=="target") %>%
  group_by(number, definiteness, WorkerId) %>%
  summarise(percent.generic = mean(response == 1)) %>%
  group_by(number, definiteness) %>%
  summarise(percent.generic = mean(percent.generic))

qplot(definiteness, percent.generic, fill=number,
      group=number, stat="identity",
      position=position_dodge(width=.9),
      geom="bar",data=ms)

write.csv(d, "data/for_prediction.csv", row.names = FALSE)

## models
bys <- d %>% 
  filter(type=="target") %>%
  group_by(animacy, number, definiteness, subject, tense, aspect, sentence.length, sentence) %>%
  summarise(percent.generic = mean(response == 1))

qplot(