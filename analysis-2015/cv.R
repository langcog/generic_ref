rm(list=ls())
library(langcog)
library(ggplot2)
library(dplyr)
library(HapEstXXR)

d <- read.csv("for_prediction_2.csv")

bys <- d %>% 
  filter(type=="target") %>%
  group_by(animacy, number, definiteness, subject, tense.modal, be.have,
           aspect, sentence.length, sentence) %>%
  summarise(generic = mean(response == 1)) %>%
  filter(generic == 1 | generic == 0) # get rid of ambigous or now

bys$fold = sample(1:10,nrow(bys),replace=TRUE)

feature.sets = powerset(c(colnames(bys)[1:3],colnames(bys)[5:8]))
error.df = data.frame(index = integer(0),error =numeric(0))

for (i in 0:length(feature.sets)){
  bys.subset = subset(bys,select=c('generic','fold',unlist(feature.sets[i])))
  total.error = 0
  for (j in 1:10){
  mod = glm(generic ~ . - fold,data=subset(bys.subset,fold!=j),family='binomial')
  predictions = predict(mod,subset(bys,fold==j)) > 0 
  error = sum(predictions != subset(bys,fold==j)$generic) / nrow(subset(bys,fold==j))
  total.error = total.error + error
  }
  error = total.error / 10
  error.df = rbind(error.df,data.frame(index=i,error=error))
  if ('number' %in% unlist(feature.sets[i]) && 'definiteness' %in% unlist(feature.sets[i])){
    bys.subset = subset(bys,select=c('generic','fold',unlist(feature.sets[i])))
    total.error = 0
    for (j in 1:10){
      mod = glm(generic ~ number*definiteness + . - fold,data=subset(bys.subset,fold!=j),family='binomial')
      predictions = predict(mod,subset(bys,fold==j)) > 0 
      error = sum(predictions != subset(bys,fold==j)$generic) / nrow(subset(bys,fold==j))
      total.error = total.error + error
    }
    error = total.error / 10
    error.df = rbind(error.df,data.frame(index=paste(i,' interaction'),error=error))
  }
}

ggplot(error.df, aes(y=error, x=reorder(index,-error),group=1)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_point() + ylab('CV Error') + xlab('Index') + geom_line()

misclassified.sentences = bys[0,]
  
for (j in 1:10){
  best.mod = glm(generic ~ ., data=subset(bys,fold!=j,select=c('generic',unlist(feature.sets[as.numeric(error.df[error.df$error == min(error.df$error),][1,1])]))),family='binomial')
  predictions = predict(mod,subset(bys,fold==j)) > 0 
  misclassified.sentences = rbind(misclassified.sentences,subset(bys,fold==j)[predictions != subset(bys,fold==j)$generic,])
}

xtabs(~ number + definiteness + generic,data=misclassified.sentences)
