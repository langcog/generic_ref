rm(list=ls())
library(langcog)
library(ggplot2)
library(dplyr)

d <- read.csv("../data/for_prediction.csv")

## now average for each sentence, our unit of prediction. 

bys <- d %>% 
  filter(type=="target") %>%
  group_by(animacy, number, definiteness, subject, tense, 
           aspect, sentence.length, sentence) %>%
  summarise(generic = mean(response == 1)) %>%
  filter(generic == 1 | generic == 0) # get rid of ambigous or now

# models
summary(glm(generic ~ number * definiteness + tense + aspect + sentence.length, data = bys))

ms <- bys %>%
  group_by(number, definiteness, tense, aspect) %>%
  multi_boot.data.frame(column = "generic", 
                        statistics_functions = c("ci_lower", "ci_upper", "mean"))

qplot(tense:aspect, mean, 
      geom = "pointrange",
      facets = number ~ definiteness, 
      ymin = ci_lower, 
      ymax = ci_upper,
      data= ms)