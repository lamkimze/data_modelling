setwd("C:/Users/Predator/Desktop/monash/y2s2/FIT 2086/ass3")
install.packages("kknn")
library(kknn)
library(boot)

ms.truth = read.csv("ms.truth.2022.csv")
ms.measured = read.csv("ms.measured.2022.csv")

#Question 3.1
table = array(0,25)
for(i in 1:25){
knn = train.kknn(intensity~., data = ms.measured, kmax=i, kernal = "optimal")
ytest.hat = fitted( kknn(intensity~ .,ms.truth, ms.measured, kernel = "optimal", k = i))
table[i] = sqrt(mean((ytest.hat - ms.truth$intensity)^2))
}
plot(table, type = "b", xlab = "MZ values", ylab = "intensity", main = "intensity against MZ values")
knn$best.parameters$k
knn$best.parameters$k

#Question 3.2
predtable = array(0,4)
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
