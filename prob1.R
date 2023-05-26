setwd("C:/Users/Predator/Desktop/monash/y2s2/FIT 2086/ass3")

#Question 1
fuel = read.csv("fuel.ass3.2022.csv", stringsAsFactors = T)
source("my.prediction.stats.R")

#question 1.1
model <- lm(Comb.FE ~ ., fuel)

summary(model)

#question 1.2
pvalues = coefficients(summary(model))[,4]
pvalues < 0.05/17

#question 1.4
step.fit.bic = step(model, direction="both",k = log(nrow(fuel)))
print(step.fit.bic)
summary(step.fit.bic)

#question 1.5
#a
predict(step.fit.bic, c(fuel[33,]), interval = "confidence", level = 0.95)

#Question 2
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
#patient = 65
boot65 = function(data, patient = 65)
{
  prob65 = predict(KIC_prune, data[patient,], type="response")
  odd65 = prob65/(1-prob65)
  return(odd65)
}
bs65 = boot(data = heart.test, statistic = boot65, R = 5000)
boot.ci(bs65, conf = 0.95, type = "bca")

#patient = 66
boot66 = function(data, patient = 66)
{
  prob66 = predict(KIC_prune, data[patient,], type="response")
  odd66 = prob66/(1-prob66)
  return(odd66)
}
bs66 = boot(data = heart.test, statistic = boot66, R = 5000)
boot.ci(bs66, conf = 0.95, type = "bca")



#Question 3
install.packages("kknn")
library(kknn)
library(boot)

ms.truth = read.csv("ms.truth.2022.csv")
ms.measured = read.csv("ms.measured.2022.csv")

#Question 3.1
table = array(0,25)
for(i in 1:25){
  knn = train.kknn(intensity~., data = ms.measured, kmax=i, kernal = "optimal")
  ytest.hat = fitted( kknn(intensity~ ., ms.measured, ms.truth , kernel = "optimal", k = i))
  table[i] = sqrt(mean((ytest.hat - ms.truth$intensity)^2))
}
plot(table, type = "b", xlab = "MZ values", ylab = "intensity", main = "intensity against MZ values")
knn$best.parameters$k

#Question 3.2

#graph 1 (k=2) 
knn = train.kknn(intensity~., data = ms.measured, kmax=2)
int2hat <- fitted( kknn(intensity~ .,ms.measured, ms.truth, kernel = "optimal", k = 2))
plot(ms.measured$intensity, type = "l", col = "red", xlab = "MZ values", ylab = "intensity", main = "intensity against MZ values with k = 2")
lines(ms.truth$intensity, type = "l", col = "blue")
lines(int2hat, type = "l", col = "green")
legend(380, 30, legend = c("training data", "true spectrum", "estimated spectrum"),col = c("red", "blue", "green"), lty=1:2, cex=0.8)

#graph 2 (k=6)
knn = train.kknn(intensity~., data = ms.measured, kmax=6)
int6hat <- fitted( kknn(intensity~ .,ms.measured, ms.truth, kernel = "optimal", k = 6))
plot(ms.measured$intensity, type = "l", col = "red", xlab = "MZ values", ylab = "intensity", main = "intensity against MZ values with k = 6")
lines(ms.truth$intensity, type = "l", col = "blue")
lines(int6hat, type = "l", col = "green")
legend(380, 30, legend = c("training data", "true spectrum", "estimated spectrum"),col = c("red", "blue", "green"), lty=1:2, cex=0.8)

#graph 3 (k=12)
knn = train.kknn(intensity~., data = ms.measured, kmax=12)
int12hat <- fitted( kknn(intensity~ .,ms.measured, ms.truth, kernel = "optimal", k = 12))
plot(ms.measured$intensity, type = "l", col = "red", xlab = "MZ values", ylab = "intensity", main = "intensity against MZ values with k = 12")
lines(ms.truth$intensity, type = "l", col = "blue")
lines(int12hat, type = "l", col = "green")
legend(380, 30, legend = c("training data", "true spectrum", "estimated spectrum"),col = c("red", "blue", "green"), lty=1:2, cex=0.8)

#graph 4 (k=25)
knn = train.kknn(intensity~., data = ms.measured, kmax=25)
int25hat <- fitted( kknn(intensity~ .,ms.measured, ms.truth, kernel = "optimal", k = 25))
plot(ms.measured$intensity, type = "l", col = "red", xlab = "MZ values", ylab = "intensity", main = "intensity against MZ values with k = 25")
lines(ms.truth$intensity, type = "l", col = "blue")
lines(int25hat, type = "l", col = "green")
legend(380, 30, legend = c("training data", "true spectrum", "estimated spectrum"),col = c("red", "blue", "green"), lty=1:2, cex=0.8)


#Question 3.3
k2 = sqrt(mean((int2hat - ms.truth$intensity)^2))
print(k2)

k6 = sqrt(mean((int6hat - ms.truth$intensity)^2))
print(k6)

k12 = sqrt(mean((int12hat - ms.truth$intensity)^2))
print(k12)

k25 = sqrt(mean((int25hat - ms.truth$intensity)^2))
print(k25)

#Question 3.5
knnq3.5 = train.kknn(intensity~., data = ms.measured, kmax = 25, kernel =  "optimal" )
ytest.hat = fitted(kknn(intensity~., ms.measured, ms.truth, kernel =  "optimal", k = knnq3.5$best.parameters$k))

knnq3.5$best.parameters$k 

#Question 3.6
test.hat3.6 = fitted(kknn(intensity~., ms.measured, ms.truth, kernel =  "optimal", k=5))
ave.error = mean((test.hat3.6-ms.measured$intensity)^2)
sqrt(sum(((test.hat3.6-ms.measured$intensity)-ave.error)^2)/(501-1))

#Question 3.7
ms.measured$intensity
test.hat3.7 = fitted(kknn(intensity~., ms.measured, ms.truth, k=5))
ms.measured$MZ[which.max(test.hat3.7)]

#Question 3.8
#bootstrap
booti = function(data, indices, k)
{
  d = data[indices,]
  test.hat3.8 = fitted( kknn(intensity~.,d, ms.truth, kernel = "optimal", k = k))
  estimated = test.hat3.8[160]
  return(test.hat3.8[160])
}

#Confidence interval
#Q3.5 best value (k=5) 
intensity3.5 <- boot(data=ms.measured, statistic = booti, R=5000, k = 5)
boot.ci(intensity3.5, conf = 0.95, type="bca")

#k = 3 
intensity3 <- boot(data=ms.measured, statistic = booti, R=5000, k = 3)
boot.ci(intensity3, conf = 0.95, type="bca")

#k = 20
intensity20 <- boot(data=ms.measured, statistic = booti, R=5000, k = 20)
boot.ci(intensity20, conf = 0.95, type="bca")