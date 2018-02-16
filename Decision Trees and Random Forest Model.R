#Question 2 - Predicting whether a listing will get a visit or not
#Reading the data in R
library(readr)
Q2_data = read.csv("~/INST 737/Q2_data.csv", header =T, na.strings=c(""))


#Including a new column that specifies whether a listing has been reviewed or not.
#Considering that as a measurement of whether a listing has been visited or not.
Q2_data$review <- ifelse(Q2_data$number_of_reviews>0, 1, 0)
View(Q2_data)

#Creating a subset with all the columns that are required to make a decision tree
review_new <- Q2_data [c(-1,-2,-3,-7,-11,-12,-13,-14,-15,-16)]
View(review_new)
review_new <- review_new[c(-3)]
review_new <- na.omit(review_new)

#Making the data random
set.seed(12345)
Q2_reviews_rand <- review_new[order(runif(25621)),]	

#Splitting the data into Training and Testing (70:30 ratio)
Q2_reviews_rand_train = Q2_reviews_rand[1:17934, ]
Q2_reviews_rand_test = Q2_reviews_rand[17934:25621, ]

#Check proportion table for both
#The values for both the training and testing data set are similar
prop.table(table(Q2_reviews_rand_test$review))
prop.table(table(Q2_reviews_rand_train$review))
prop.table(table(Q2_reviews_rand$review))

#Creating the Decision Tree
library(C50)
str(Q2_reviews_rand_train)
Q2_reviews_rand_test$review <- as.factor(Q2_reviews_rand_test$review)
Q2_reviews_rand_train$review <- as.factor(Q2_reviews_rand_train$review)
Q2_reviews_model = C50::C5.0(Q2_reviews_rand_train[-10], Q2_reviews_rand_train$review)
Q2_reviews_model
summary(Q2_reviews_model)

#Prediction values
review_pred=predict(Q2_reviews_model,Q2_reviews_rand_test)
review_pred

#Creating the confusion matrix
library(gmodels)
CrossTable(Q2_reviews_rand_test$review,review_pred,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn=c('actual review','predicted review'))

#Boosting with trails = 7
Q2_reviews_model7 = C5.0(Q2_reviews_rand_train[-10],Q2_reviews_rand_train$review,trials=7)
Q2_reviews_model7
summary(Q2_reviews_model7)
Q2_reviews_model_pred7=predict(Q2_reviews_model7,Q2_reviews_rand_test)
CrossTable(Q2_reviews_rand_test$review,Q2_reviews_model_pred7,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn=c('actual review','predicted review'))

#Boosting with trails = 5
Q2_reviews_model5 = C5.0(Q2_reviews_rand_train[-10],Q2_reviews_rand_train$review,trials=5)
Q2_reviews_model5
summary(Q2_reviews_model5)
Q2_reviews_model_pred5=predict(Q2_reviews_model5,Q2_reviews_rand_test)
CrossTable(Q2_reviews_rand_test$review,Q2_reviews_model_pred5,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn=c('actual review','predicted review'))


#Bagging
library(ISLR)	
library(randomForest)
baggin_review <- review_new[c(-5)]
attach(baggin_review)
View(baggin_review)

set.seed(1)	
train	<- sample(1:nrow(baggin_review),nrow(baggin_review)/2)

bag.review	<- randomForest(review~.,data = baggin_review, subset=train, mtry=9, importance=TRUE)	
bag.review 
pred	<- predict(bag.review, baggin_review[-train, ])
pred
mean((pred-baggin_review[-train,9])^2)	
plot(pred,baggin_review[-train,9])

#Random Forests
bag.review3	<- randomForest(review~.,data = baggin_review, subset=train, mtry=3, importance=TRUE)
bag.review3
pred3 <- predict(bag.review3, baggin_review[-train, ])
pred3
mean((pred3-baggin_review[-train,9])^2)	
plot(pred3,baggin_review[-train,9])
