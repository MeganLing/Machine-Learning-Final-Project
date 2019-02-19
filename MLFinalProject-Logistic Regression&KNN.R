rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed <- c('class', 'boot', 'pROC', 'verification')      
installIfAbsentAndLoad(needed)

df<-read.csv("MLProject2.csv")
attach(df)
set.seed(5072)
df[2:31]<-scale(df[2:31])

trainprop<- 0.999
rows<- sample(x=nrow(df), size=trainprop*nrow(df)) 

trainset<- df[rows,]
testset<- df[-rows,]

trainset$price<-with(ifelse(trainset$price>median(trainset$price),"High","Low"), data=trainset)
trainset$price<- as.factor(trainset$price)

testset$price<-with(ifelse(testset$price>median(testset$price),"High","Low"), data=testset)
testset$price<- as.factor(testset$price)


#glm predicitons
glm.fit <- glm(price ~ bedrooms+bathrooms+sqft_living15+floors+view +sqft_above+sqft_basement+yr_built+yr_renovated+sqft_lot15+Median+Renovated+yrs_Current_Arrangement+condition2+condition3+condition4+condition5,data=trainset,family=binomial)
glm.probs<- predict(glm.fit, testset, type="response")
glm.pred<- ifelse(glm.probs>0.5, "Low", "High")

conf_matrix<-table(testset$price,glm.pred)
conf_matrix

testset$price<- as.numeric(testset$price)-1

roc.plot(testset$price,glm.probs)
print(roc(testset$price, glm.probs)$auc)


print(paste('The overall error rate: ', (conf_matrix["High", "Low"]+conf_matrix["Low", "High"])/sum(conf_matrix),sep=''))
print(paste('The Type 1 Error Rate: ', (conf_matrix["Low", "High"]/sum(conf_matrix["Low", ])),sep=''))
print(paste('The Type 2 Error rate: ', (conf_matrix["High", "Low"]/sum(conf_matrix["High", ])),sep=''))
print(paste('The Power of the model: ', (conf_matrix["High", "High"]/sum(conf_matrix["High", ])),sep=''))
print(paste('The Precision of the model: ', (conf_matrix["High", "High"]/sum(conf_matrix[, "High"])),sep=''))
####################################


##KNN prediction model 
rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed <- c('class', 'boot', 'pROC', 'verification')      
installIfAbsentAndLoad(needed)

df<-read.csv("MLProject2.csv")
attach(df)
set.seed(5072)
df[2:31]<-scale(df[2:31])

trainprop<- 0.999
rows<- sample(x=nrow(df), size=trainprop*nrow(df)) 

trainset<- df[rows,]
testset<- df[-rows,]

trainset$price<-with(ifelse(trainset$price>median(trainset$price),"High","Low"), data=trainset)
trainset$price<- as.factor(trainset$price)

testset$price<-with(ifelse(testset$price>median(testset$price),"High","Low"), data=testset)
testset$price<- as.factor(testset$price)


train.x <- cbind(trainset$sqft_living,trainset$floors,trainset$yr_built)
test.x<-cbind(testset$sqft_living,testset$floors,testset$yr_built)
train.price <-  trainset$price
test.price<- testset$price 
i<-1 
errors<- c()  
while (i<90){ 
  knn.pred <- knn(train.x, test.x, train.price, k=i)
  conf_matrix <- table(test.price,knn.pred)
  x <- mean(knn.pred != test.price)
  errors[i]<- cbind(x) 
  i <- i +1
}
bestk=which.min(errors)
knn.pred <- knn(train.x, test.x, train.price, k=bestk) 
conf_matrix<- table(test.price,knn.pred)
rownames(conf_matrix)<- c("High", "Low")
colnames(conf_matrix)<-c("High","Low")
print(conf_matrix)

print(paste('The overall error rate: ', (conf_matrix["High", "Low"]+conf_matrix["Low", "High"])/sum(conf_matrix),sep=''))
print(paste('The Type 1 Error Rate: ', (conf_matrix["Low", "High"]/sum(conf_matrix["Low", ])),sep=''))
print(paste('The Type 2 Error rate: ', (conf_matrix["High", "Low"]/sum(conf_matrix["High", ])),sep=''))
print(paste('The Power of the model: ', (conf_matrix["High", "High"]/sum(conf_matrix["High", ])),sep=''))
print(paste('The Precision of the model: ', (conf_matrix["High", "High"]/sum(conf_matrix[, "High"])),sep=''))
