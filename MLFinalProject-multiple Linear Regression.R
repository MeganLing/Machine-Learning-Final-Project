library(leaps)
library(ISLR)
library(glmnet)
library(glmulti)
library(FNN)
library(reshape2)
library(DescTools)
set.seed(5072)
data <- read.csv("MLProject2.csv")
# ncol(data)
# melt <- melt(cor(data[,2:29]))
# melt
# for (i in 1:nrow(melt)){
#     if (abs(melt[i,3]) > .8 & abs(melt[i,3]) < 1){
#         print(paste(melt[i,1],melt[i,2]))
#     }
# }
colnames(data)
abs(-.3)
#data$yr_renovated <- NULL
#data$yrs_Current_Arrangement <- NULL
ncol(data)
n <- nrow(data)
trainprop <- .999
train  <-  sample(n, trainprop * n)
test <- setdiff(1:n, train)

trainset <- data[train,]
testset <- data[test,]

regfit.full <- regsubsets(price ~ ., 
                          data=trainset,
                          nvmax=29)
reg.summary <- summary(regfit.full)
reg.summary
par(mfrow=c(1, 1))
# plot(reg.summary$rss, 
#      xlab="Number of Variables", 
#      ylab="RSS", 
#      type="l")
# plot(reg.summary$adjr2, 
#      xlab="Number of Variables", 
#      ylab="Adjusted RSq", 
#      type="l")
# (max<-which.max(reg.summary$adjr2))
# points(max, reg.summary$adjr2[max],  
#        col="red", 
#        cex=2, 
#        pch=20)
plot(reg.summary$cp, 
     xlab="Number of Variables", 
     ylab="Cp", 
     type='l')
(min <- which.min(reg.summary$cp))
points(min, reg.summary$cp[min], 
       col="red", 
       cex=2, 
       pch=20)
# (min <- which.min(reg.summary$bic))
# plot(reg.summary$bic, 
#      xlab="Number of Variables", 
#      ylab="BIC", 
#      type='l')
# points(min, reg.summary$bic[min], 
#        col="red", 
#        cex=2, 
#        pch=20)
mean <- mean(reg.summary$cp[14:29])
sd <- sd(reg.summary$cp[14:29])
ub <- reg.summary$cp[min]+sd
abline(h = ub, col = "red")
ub
reg.summary$cp #20

(min <- which.min(reg.summary$bic))
plot(reg.summary$bic,
     xlab="Number of Variables",
     ylab="BIC",
     type='l')
points(min, reg.summary$bic[min],
       col="red",
       cex=2,
       pch=20)

sd <- sd(reg.summary$bic[15:29])
ub <- reg.summary$bic[min]+sd
abline(h = ub, col = "red")
ub
reg.summary$bic #both show that 20 variables is the smallest number that is within one standard deviation of the min 

options(scipen = 999)
coef(regfit.full, 20) #got 20 for both

lm.bestfit <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + yr_built + yr_renovated + sqft_lot15 + Median + Renovated + grade3 + grade4 + grade5 + grade6 + grade7 + grade8 + grade9 + grade10 + grade11 + grade12, data = trainset) #add back last grade variable
lm.bestfit
predictions <- predict(lm.bestfit, newdata = testset)
plot(predictions, testset$price)
abline(a = 0, b = 1, col = 'red')
par(mfrow=c(2, 2))
plot(lm.bestfit)
par(mfrow=c(1,1))


###Log transform
set.seed(5072)
data <- read.csv("MLProject2.csv")

data$price <- log(data$price)
trainprop <- .999
train  <-  sample(n, trainprop * n)
test <- setdiff(1:n, train)

trainset <- data[train,]
testset <- data[test,]

regfit.full <- regsubsets(price ~ ., 
                          data=trainset,
                          nvmax=31)
reg.summary <- summary(regfit.full)
reg.summary

plot(reg.summary$cp, 
     xlab="Number of Variables", 
     ylab="Cp", 
     type='l')
(min <- which.min(reg.summary$cp))
points(min, reg.summary$cp[min], 
       col="red", 
       cex=2, 
       pch=20)
mean <- mean(reg.summary$cp[14:29])
sd <- sd(reg.summary$cp[14:29])
ub <- reg.summary$cp[min]+sd
abline(h = ub, col = "red")
ub
reg.summary$cp #19

(min <- which.min(reg.summary$bic))
plot(reg.summary$bic,
     xlab="Number of Variables",
     ylab="BIC",
     type='l')
points(min, reg.summary$bic[min],
       col="red",
       cex=2,
       pch=20)

sd <- sd(reg.summary$bic[15:29])
ub <- reg.summary$bic[min]+sd
abline(h = ub, col = "red")
ub
reg.summary$bic #both show that 19 variables is the smallest number that is within one standard deviation of the min 

options(scipen = 999)
coef(regfit.full, 19) #got 19 for both

lm.bestfit <- lm(price ~ bedrooms + bathrooms + floors + waterfront + 
                    view + sqft_above+ yr_built + yr_renovated + sqft_lot15 + Renovated + grade3 + 
                     grade4 + grade5 + grade6 + grade7 + grade8 + grade9 + 
                     grade10 + grade11 + grade12, data = trainset) 
summary(lm.bestfit)
predictions <- predict(lm.bestfit, newdata = testset)
plot(predictions, testset$price)
abline(a = 0, b = 1, col = 'red')
par(mfrow=c(2, 2))
plot(lm.bestfit)
par(mfrow=c(1,1))


lm.evenbetterfit <- lm(price ~  bathrooms + floors + waterfront + view + sqft_above+ yr_built + yr_renovated + sqft_lot15 + Renovated + grade3 + grade4 + grade5 + grade6 + grade7 + grade8 + grade9 + grade10 + grade11 + grade12, data = trainset)
summary(lm.evenbetterfit)
predictions <- predict(lm.evenbetterfit, newdata = testset)
plot(predictions, testset$price)
abline(a = 0, b = 1, col = 'red')
par(mfrow=c(2, 2))
plot(lm.evenbetterfit)
par(mfrow=c(1,1))
confint(lm.evenbetterfit)

VIF(lm.evenbetterfit) #grades look problematic, will be removed. Try keeping Renovation
head(trainset)

lm.notcolinear <- lm(price ~  bathrooms + floors + waterfront + view + sqft_above+ yr_built + sqft_lot15 + Renovated, data = trainset)
summary(lm.notcolinear)


lm.notcolinear2 <- lm(price ~  bathrooms + floors + waterfront + view + sqft_above+ yr_built + sqft_lot15 + yr_renovated, data = trainset)
summary(lm.notcolinear2)

lm.notcolinear3 <- lm(price ~  bathrooms + floors + waterfront + view + sqft_above+ yr_built + sqft_lot15 + grade3 + grade4, data = trainset) #as it turns out, neither renovation variable is significant on its own
summary(lm.notcolinear3)

predictions <- predict(lm.notcolinear3, newdata = testset)
plot(predictions, testset$price)
abline(a = 0, b = 1, col = 'red')
par(mfrow=c(2, 2))
plot(lm.notcolinear3)



par(mfrow=c(1,1))
summary(lm.notcolinear3)
VIF(lm.notcolinear3)
confint(lm.notcolinear3)

confint <- predict.lm(lm.evenbetterfit, newdata = testset, interval = "confidence")
confint
predint <- predict.lm(lm.notcolinear3, newdata = testset, interval = "prediction")
exp(1)^15 #9940
j = 1
#exp(1)^data[9940,1]
data2 <- data.frame()
for (i in as.numeric(rownames(predint))){
    data2 <- rbind(data2, cbind(exp(1)^data[i,1], exp(1)^predint[j,1], exp(1)^predint[j,2], exp(1)^predint[j,3]))
    j <- j + 1
}
data2
colnames(data2) <- c("Actual", "Predicted", "Lower Bound", "Upper Bound")
as.numeric(rownames(predint))
j =1
errors <- c()
for (i in as.numeric(rownames(predint))){
    errors[j] <- ((exp(1)^data[i,1])-(exp(1)^predint[j,1]))^2
    j <- j + 1
}
errors <- c()

for (i in 1:66){
    errors[i] <- (data2[i,1]-data2[i,2])^2
}
good <- c()
for (i in 1:66){
    if (data2[i,2]<(exp(1)^15)){
        good <- c(good, i)
    }
}
exp(1)^15
good
data2[1,1]
errors
sqrt(mean(errors))
mean(errors)
length(rownames(predint))
summary(lm.notcolinear3)

#sqrt transform
#NOT AS GOOD AS LOG
set.seed(5072)
data <- read.csv("MLProject2.csv")

data$price <- sqrt(data$price)
trainprop <- .999
train  <-  sample(n, trainprop * n)
test <- setdiff(1:n, train)

trainset <- data[train,]
testset <- data[test,]

regfit.full <- regsubsets(price ~ ., 
                          data=trainset,
                          nvmax=31)
reg.summary <- summary(regfit.full)
reg.summary
par(mfrow=c(1, 1))

plot(reg.summary$cp, 
     xlab="Number of Variables", 
     ylab="Cp", 
     type='l')
(min <- which.min(reg.summary$cp))
points(min, reg.summary$cp[min], 
       col="red", 
       cex=2, 
       pch=20)
mean <- mean(reg.summary$cp[14:29])
sd <- sd(reg.summary$cp[14:29])
ub <- reg.summary$cp[min]+sd
abline(h = ub, col = "red")
ub
reg.summary$cp #19

(min <- which.min(reg.summary$bic))
plot(reg.summary$bic,
     xlab="Number of Variables",
     ylab="BIC",
     type='l')
points(min, reg.summary$bic[min],
       col="red",
       cex=2,
       pch=20)

sd <- sd(reg.summary$bic[15:29])
ub <- reg.summary$bic[min]+sd
abline(h = ub, col = "red")
ub
reg.summary$bic #both show that 19 variables is the smallest number that is within one standard deviation of the min 

options(scipen = 999)
coef(regfit.full, 19) #got 19 for both

lm.bestfit <- lm(price ~ bedrooms + bathrooms + floors + waterfront + view + sqft_above + yr_built + yr_renovated + sqft_lot15 + Renovated + grade3 + grade4 + grade5 + grade6 + grade7 + grade8 + grade9 + grade10 + grade11 + grade12, data = trainset) #add back last grade variable
lm.bestfit
predictions <- predict(lm.bestfit, newdata = testset)
plot(predictions, testset$price)
abline(a = 0, b = 1, col = 'red')
par(mfrow=c(2, 2))
plot(lm.bestfit)
par(mfrow=c(1,1))

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_191') 
