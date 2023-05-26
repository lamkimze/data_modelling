setwd("C:/Users/Predator/Desktop/monash/y2s2/FIT 2086/ass3")
source("my.prediction.stats.R")
source("wrappers.R")
library(rpart)

heart.train = read.csv("heart.train.ass3.2022.csv", stringsAsFactors = T)
heart.test = read.csv("heart.test.ass3.2022.csv", stringsAsFactors = T)
summary(heart.train)
summary(heart.test)

#question 2.1
tree.heart = rpart(HD ~ ., heart.train)
plot(tree.heart)
text(tree.heart, pretty = 3)
summary(tree.heart)
tree.heart$variable.importance

cv = learn.tree.cv(HD~.,data=heart.train,nfolds=10,m=5000)
cv$best.tree
plot.tree.cv(cv)
text(tree.heart,pretty=12)

#question 2.2
plot(tree.heart)
text(cv$best.tree,pretty=12)

#question 2.3
plot(tree.heart)
text(cv$best.tree,pretty=12)

#question 2.5
fullmod = glm(HD~., data=heart.train, family = binomial)
KIC_prune = step(fullmod, direction="both",k=3)
print(KIC_prune)
tree.heart$variable.importance/max(tree.heart$variable.importance)
summary(KIC_prune)

#question 2.8
my.pred.stats(predict(tree.heart, heart.test)[,2], heart.test$HD)
my.pred.stats(predict(KIC_prune, heart.test, type="response"), heart.test$HD)

#question 2.9
#a
patient10 = predict(tree.heart, heart.test, type = "prob")[,2][10]
odds = patient10/(1-patient10)
print(odds)

#b
KIC_patient10 = predict(KIC_prune, heart.test, type = "response")[10]
KIC_odds = KIC_patient10/(1-KIC_patient10)
print(KIC_odds)

#question 2.10
library(boot)

#from studio week 10
boots = function(data, patient = 66)
{
  prob = predict(KIC_prune, data[patient,], type="response")
  odd2.10 = prob/(1-prob)
  return(odd2.10)
}
bs = boot(data = heart, statistic = boots, R = 5000)
boot.ci(bs, conf = 0.95, type = "bca")
