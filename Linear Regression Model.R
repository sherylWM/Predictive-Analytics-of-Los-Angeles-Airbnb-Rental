#Importing dataset
listing <- read.csv(file.choose())



#Data Cleaning#
#Splitting the string in the price column, ie, removing the dollar sign from all the values in the column - $xyz => xyz
listing$price  <- substr(listing$price,2,10)

#Converting price column into integer format
listing$price <- as.integer(listing$price)

#Removing "null" and "0" from price column
listing <- subset(listing, !(listing$price == 0 | is.na(listing$price))) 

#Splitting the string in the security deposit column, ie, removing the dollar sign from all the values in the column - $xyz => xyz
class(listing$security_deposit)
listing$security_deposit  <- substr(listing$security_deposit,2,10)
listing$security_deposit <- as.integer(listing$security_deposit)

#Splitting the string in the cleaning fee column, ie, removing the dollar sign from all the values in the column - $xyz => xyz
class(listing$cleaning_fee)
head(listing$cleaning_fee)
listing$cleaning_fee  <- substr(listing$cleaning_fee,2,10)
listing$cleaning_fee <- as.integer(listing$cleaning_fee)
head(listing$cleaning_fee)

#Checking levels of categorical variable and these three columns have blank vlaues - ""
class(listing$host_is_superhost)
levels(listing$host_is_superhost) #this has a value = ""

class(listing$host_has_profile_pic)
levels(listing$host_has_profile_pic) #this has a value = ""

class(listing$host_identity_verified)
levels(listing$host_identity_verified) #this has a value = ""

#Removing blank values - "" - from the dataset for the columns host is super host, host acceptance rate, host response rate
#host_is_superhost
listing = listing[listing$host_is_superhost != "", ]

#host_has_profile_pic
listing = listing[listing$host_has_profile_pic != "", ]

#host_identity_verified
listing = listing[listing$host_identity_verified != "", ]

#Split the last caharacter - % - from each entry in the column host response rate and convert the column  into integer
listing$host_response_rate <- as.character( listing$host_response_rate)
listing$host_response_rate  <- substr(listing$host_response_rate,1,nchar(listing$host_response_rate)-1)
listing$host_response_rate <- as.integer(listing$host_response_rate)


#Split the last caharacter - % - from each entry in the column host acceptance rate and convert the column  into integer
listing$host_acceptance_rate <- as.character(listing$host_acceptance_rate)
listing$host_acceptance_rate <- substr(listing$host_acceptance_rate,1,nchar(listing$host_acceptance_rate)-1)
listing$host_acceptance_rate <- as.integer(listing$host_acceptance_rate)

#Pruning the dataset
seq(1,length(names(listing)))[names(listing) == "review_scores_rating"]
#integer
27
28
54
55
56
57
66
77
95
80
#factor
29
36
37
90

listing1 <- listing[,c(61,27,28,54,55,56,57,66,77,95,80,29,36,37,90)]



#Test and Train#
#splitting dataset into test and train datasets
set.seed(123)

## 70% of the sample size
smp_size <- floor(0.70 * nrow(listing1))


#splits into test and train dataset
train_ind <- sample(seq_len(nrow(listing1)), size = smp_size)
train <- listing1[train_ind, ]
test <- listing1[-train_ind, ]



#Regression Models#
#[a]

#Function to calculate mean square error
mse <- function(sm) 
  mean(sm$residuals^2)

#integers
model1.1 <- lm(train$price~train$host_response_rate)
summary(model1.1) #not significant (not a predictive feature)
#intercept = 145.37282
#co-efficient = 0.03250 

model1.1 <- lm(train$price~train$host_acceptance_rate)
summary(model1.1) #not significant (not a predictive feature)
#intercept = 157.6918
#co-efficient = -0.1045

model1.1 <- lm(train$price~train$accommodates)
summary(model1.1) #significant (is a predictive feature)
#intercept = 27.6658
#co-efficient = 36.4648

######
model1.1 <- lm(price~bathrooms, data = train)
summary(model1.1) #significant (is a predictive feature)
#intercept = 11.097
#co-efficient = 102.816

pred <- predict(model1.1, newdata = test)
cor(pred, test$price, use = "complete.obs") #0.5293741
mse(model1.1) #12387.1

qqnorm(resid(model1.1)) 
qqline(resid(model1.1))

######
model1.1 <- lm(price~bedrooms, data = train)
summary(model1.1) #significant (is a predictive feature)
#intercept = 22.2073
#co-efficient = 97.1243

pred <- predict(model1.1, newdata = test)
cor(pred, test$price, use = "complete.obs") #0.6317727
mse(model1.1) #10136.64

qqnorm(resid(model1.1)) 
qqline(resid(model1.1))

######
model1.1 <- lm(price~beds, data = train)
summary(model1.1) #significant (is a predictive feature)
#intercept = 59.6953
#co-efficient = 49.2761

pred <- predict(model1.1, newdata = test)
cor(pred, test$price, use = "complete.obs") #0.5040674
mse(model1.1) #12965.16

qqnorm(resid(model1.1)) 
qqline(resid(model1.1))


model1.1 <- lm(train$price~train$host_response_rate)
summary(model1.1) #not significant (not a predictive feature)
#intercept = 145.37282
#co-efficient = 0.03250

model1.1 <- lm(train$price~train$number_of_reviews)
summary(model1.1) #significant (is a predictive feature)
#intercept = 151.36489
#co-efficient = -0.22712

model1.1 <- lm(train$price~train$reviews_per_month)
summary(model1.1) #significant (is a predictive feature)
#intercept = 156.3791
#co-efficient = -6.6605

model1.1 <- lm(train$price~train$review_scores_rating)
summary(model1.1) #significant (is a predictive feature)
#intercept = 21.2161
#co-efficient = 1.3040

#factors
model1.1 <- lm(train$price~train$host_is_superhost)
summary(model1.1) #not significant (not a predictive feature)
#intercept = 146.724 
#co-efficient = 4.620

model1.1 <- lm(train$price~train$host_has_profile_pic)
summary(model1.1) #not significant (not a predictive feature)
#intercept = 160.14
#co-efficient = -12.72

model1.1 <- lm(train$price~train$host_identity_verified)
summary(model1.1) #significant (is a predictive feature)
#intercept = 139.953
#co-efficient = 10.704

model1.1 <- lm(train$price~train$instant_bookable)
summary(model1.1) #significant (is a predictive feature)
#intercept = 151.611
#co-efficient = -23.686



#[b]
#Integers and factors
model2.1 <- lm(price ~ host_response_rate + host_acceptance_rate + accommodates + bathrooms + beds 
               + bedrooms + guests_included + number_of_reviews + reviews_per_month + review_scores_rating +
                 host_is_superhost + host_has_profile_pic + host_identity_verified + instant_bookable, data = train)

summary(model2.1)

pred <- predict(model2.1, newdata = test)
cor(pred, test$price, use = "complete.obs") #0.7319368
mse(model2.1) #7086.039

#including only significant variables
model2.2 <- lm(price ~ accommodates + bathrooms + beds + bedrooms + guests_included + reviews_per_month + 
                 review_scores_rating + host_is_superhost + instant_bookable, data = train)

summary(model2.2)

pred <- predict(model2.2, newdata = test)
cor(pred, test$price, use = "complete.obs") #0.72978
mse(model2.2) #6954.915

#Only integers
model3.1 <- lm(price ~ host_response_rate + host_acceptance_rate + accommodates + bathrooms + beds 
               + bedrooms + guests_included + number_of_reviews + reviews_per_month + review_scores_rating,data = train)

summary(model3.1)

pred <- predict(model3.1, newdata = test)
cor(pred, test$price, use = "complete.obs") #0.7298308
mse(model3.1) #7127.509

#including only significant variables
model3.2 <- lm(price ~ accommodates + bathrooms + beds 
               + bedrooms + guests_included + number_of_reviews + reviews_per_month + review_scores_rating,data = train)

summary(model3.2)

pred <- predict(model3.2, newdata = test)
cor(pred, test$price, use = "complete.obs") #0.7278594
mse(model3.2) #6988.823



#Regularization#
#[c]
#Install and use glmnet package
library(glmnet) 

#Creating only a numeric subset from the train dataset 
seq(1,length(names(train)))[names(train) == "host_is_superhost"]
train2 <- train[,c(1,2,3,4,5,6,7,8,9,10,11)]

#Removing all Null values, as glmnet command does not handle Null values
train2 <- train2[complete.cases(train2),]

#Creating a regression model fitted with regularization with lasso penalty
cv.fit <- cv.glmnet(as.matrix(train2[,c(-1)]), as.vector(train2[,1]), alpha=1)

#Plotting and finding out the co-efficients of the model
plot(cv.fit) 
coef(cv.fit)

#Creating only a numeric subset from the test dataset 
test2 <- test[,c(1,2,3,4,5,6,7,8,9,10,11)]

#Removing all Null values, as glmnet command does not handle Null values
test2 <- test2[complete.cases(test2),]

#Using the model on the test dataset
pred1 <- predict(cv.fit, newx=as.matrix(test2[,c(-1)]))

#Finding correlation
cor(pred1, as.vector(test2[,1])) #0.7247442



#[d]
#Repitition 1
#splitting dataset into test and train datasets
set.seed(471)

## 70% of the sample size
smp_size <- floor(0.70 * nrow(listing1))


#splits into test and train dataset
train_ind <- sample(seq_len(nrow(listing1)), size = smp_size)
train <- listing1[train_ind, ]
test <- listing1[-train_ind, ]
#Run [a], [b], [c] and [d] again

#Repitition 2
#splitting dataset into test and train datasets
set.seed(456)

## 70% of the sample size
smp_size <- floor(0.70 * nrow(listing1))


#splits into test and train dataset
train_ind <- sample(seq_len(nrow(listing1)), size = smp_size)
train <- listing1[train_ind, ]
test <- listing1[-train_ind, ]
#Run [a], [b], [c] and [d] again


#Repitition 3
#splitting dataset into test and train datasets
set.seed(789)

## 70% of the sample size
smp_size <- floor(0.70 * nrow(listing1))


#splits into test and train dataset
train_ind <- sample(seq_len(nrow(listing1)), size = smp_size)
train <- listing1[train_ind, ]
test <- listing1[-train_ind, ]
#Run [a], [b], [c] and [d] again




