# initialize
library(ISLR)
library(MASS)

wines <- read.csv("C:/nus/ca_ml/Wine-new.csv")

#------------------
# describe data
#------------------

name(wines)

# check how many rows and colums
dim(wines) 
#178 rows and 14 columns, sample data is of medium size

# clean data
str(wines)
head(wines)

# check how many types are there and numbers of wines in each type
table(wines[,1])

summary(wines) #data
sum(is.na(wines)) #there is no column where NA value found

# remove the Type column as it is not numeric
winesvalue <- wines[,-1]

# check correlation
cor(winesvalue)

#Create function to check for normality in all of the variables 
normTest <- function(x){
  y <- sapply(x, function(xx)any(shapiro.test(xx)$p.value>0.05))
  names(y[y])
}

#Test normality at the 95% level and list variables that are not normally distributed
normTest(winesvalue)
# Only Ash_Alcalinity is not normal, so LDA approximation should be reliable

set.seed(1)

#Split data into a training set and a test set
trainingIndexes <- sample(1:nrow(wines), size=0.8*nrow(wines))
train <- wines[trainingIndexes, c(1:ncol(wines))]
test <- wines[-trainingIndexes, c(1:ncol(wines))]

summary(train)
summary(test)


# LDA
lda.fit=lda(Type~., data=train)
lda.fit
#Proportion of trace:
#LD1    LD2 
#0.7058 0.2942 
plot(lda.fit)

#Use model to predict test data
lda.pred <- predict(lda.fit, test)
names(lda.pred)
#View confusion matrix
table(lda.pred$class,test$Type)
#     A  B  C
#A 13  0  0
#B  0 14  0
#C  0  0  9
#Prediction accuracy
sum(lda.pred$class==test$Type)/nrow(test)
mean(lda.pred$class==test$Type) #1


# Test LOOCV on LDA
cv.loocv = rep(0,178)

for (k in 1:178) {
  train_wine = wines[-k,]
  test_wine = wines[k,]
  
  lda.fit=lda(Type~., data=train_wine)
  lda.pred=predict(lda.fit, test_wine)
  lda.pred
  if (lda.pred$class[1] != test_wine$Type) cv.loocv[k] = 1
}
test.lda = mean(cv.loocv)
test.lda # 0.01123596

#QDA
qda.fit = qda(Type~.,data=train)
qda.fit

#Use model to predict test data
qda.pred <- predict(qda.fit, test)
names(qda.pred)
#View confusion matrix
table(qda.pred$class,test$Type)
#     A  B  C
#A 13  0  0
#B  0 14  0
#C  0  0  9
#Prediction accuracy
sum(qda.pred$class==test$Type)/nrow(test)
mean(qda.pred$class==test$Type) #1

#Test QDA LOOCV

cv.loocv = rep(0,178)

for (k in 1:178) {
  train_wine = wines[-k,]
  test_wine = wines[k,]
  
  qda.fit=qda(Type~., data=train_wine)
  qda.pred=predict(qda.fit, test_wine)
  qda.pred
  if (qda.pred$class[1] != test_wine$Type) cv.loocv[k] = 1
}
test.qda = mean(cv.loocv)
test.qda # 0.005617978

# logistic regression 
library(nnet)
cv.loocv3 = rep(0,178)
for (k in 1:178) { 
  train_wine = wines[-k,]
  test_wine = wines[k,]
  
  glm.mul <- multinom(Type~., data=train_wine)
  glm.pred=predict(glm.mul, test_wine)
  table(glm.pred, test_wine$Type)
  if (glm.pred == test_wine$Type) cv.loocv3[k] = 1
}
test.lr = mean(cv.loocv3)
test.lr # 0.9438202

# best subset selection
library(leaps)
regfit.full=regsubsets(Type~., wines)
summary(regfit.full)

# Test LOOCV on LDA without Magnesium
cv.loocv = rep(0,178)

for (k in 1:178) {
  train_wine = wines[-k,]
  test_wine = wines[k,]
  
  lda.fit=lda(Type~Alcohol+Malic_Acid+Ash+Ash_Alcalinity+Total_Phenols+Flavanoids+Color_Intensity+Hue+OD280_OD315+Proline, data=train_wine)
  lda.pred=predict(lda.fit, test_wine)
  lda.pred
  if (lda.pred$class[1] != test_wine$Type) cv.loocv[k] = 1
}
test.lda = mean(cv.loocv)
test.lda # 0.01123596