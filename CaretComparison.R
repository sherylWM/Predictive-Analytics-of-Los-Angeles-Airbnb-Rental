Listings <- read.csv(file.choose(),header=T)
names(Listings)

ListingsDataFrame <- Listings[,c(27:42)]
View(ListingsDataFrame)

ListingsDataFr <- na.omit(ListingsDataFrame)
View(ListingsDataFr)
names(ListingsDataFr)
install.packages("readr")
library(readr)
library(caret)
?createDataPartition

set.seed(7)
sample_size <- createDataPartition(ListingsDataFr$get_visits,p=0.2,list = FALSE)
NewLstingsDF <- ListingsDataFr[sample_size,]
View(NewLstingsDF)

#Preparing training scheme using repeated K-fold cross validation with 10 folds repeated thrice
control <- trainControl(method = "repeatedcv", number= 10, repeats = 3)


attach(NewLstingsDF)
#train the NB model
modelNB <- train(get_visits~host_response_rate + host_acceptance_rate + host_listings_count + accommodates + bathrooms + bedrooms + beds + price 
                 + securitydeposit + cleaning_fee + guests_included + extra_people + minimum_nights
                 + maximum_nights + availability_365, data = NewLstingsDF ,method = "nb",trControl = control)
modelNB

set.seed(7)
#train the RF model
modelRF <- train(get_visits~host_response_rate + host_acceptance_rate + host_listings_count + accommodates + bathrooms + bedrooms + beds + price 
                 + securitydeposit + cleaning_fee + guests_included + extra_people + minimum_nights
                 + maximum_nights + availability_365, data = NewLstingsDF ,method = "rf",trControl = control, verbose=FALSE)
modelRF

set.seed(7)
#train the SVM model
modelSVM <- train(get_visits~host_response_rate + host_acceptance_rate + host_listings_count + accommodates + bathrooms + bedrooms + beds + price 
                 + securitydeposit + cleaning_fee + guests_included + extra_people + minimum_nights
                 + maximum_nights + availability_365, data = NewLstingsDF ,method = "svmRadial",trControl = control)
modelSVM

#collect resamples
results <- resamples(list(NB=modelNB,RF=modelRF,SVM=modelSVM))
#summarize the distributions
summary(results)

#Preparing training scheme using K-fold cross validation with 10 folds
control <- trainControl(method = "cv", number= 10)
set.seed(7)
attach(NewLstingsDF)
#train the NB model
modelNB <- train(get_visits~host_response_rate + host_acceptance_rate + host_listings_count + 
                   accommodates + bathrooms + bedrooms + beds + price 
                 + securitydeposit + cleaning_fee + guests_included + extra_people + minimum_nights
                 + maximum_nights + availability_365, data = NewLstingsDF ,method = "nb",trControl = control)
modelNB

set.seed(7)
#train the RF model
modelRF <- train(get_visits~host_response_rate + host_acceptance_rate + host_listings_count + accommodates + 
                   bathrooms + bedrooms + beds + price 
                 + securitydeposit + cleaning_fee + guests_included + extra_people + minimum_nights
                 + maximum_nights + availability_365, data = NewLstingsDF ,method = "rf",trControl = control,verbose=FALSE)
modelRF

set.seed(7)
#train the SVM model
modelSVM <- train(get_visits~host_response_rate + host_acceptance_rate + host_listings_count + accommodates + 
                    bathrooms + bedrooms + beds + price 
                  + securitydeposit + cleaning_fee + guests_included + extra_people + minimum_nights
                  + maximum_nights + availability_365, data = NewLstingsDF ,method = "svmRadial",trControl = control)
modelSVM
#collect resamples
results <- resamples(list(NB=modelNB,RF=modelRF,SVM=modelSVM))
#summarize the distributions
summary(results)
#boxplot of results
bwplot(results)
#dotplot of results
dotplot(results)

#Preparing training scheme using repeated boostrap CV technique
control3 <- trainControl(method = "boot", number= 10)
set.seed(7)
attach(NewLstingsDF)
#train the NB model
modelNB <- train(get_visits~host_response_rate + host_acceptance_rate + host_listings_count + 
                   accommodates + bathrooms + bedrooms + beds + price 
                 + securitydeposit + cleaning_fee + guests_included + extra_people + minimum_nights
                 + maximum_nights + availability_365, data = NewLstingsDF ,method = "nb",trControl = control3)
modelNB

set.seed(7)
#train the RF model
modelRF <- train(get_visits~host_response_rate + host_acceptance_rate + host_listings_count + accommodates + 
                   bathrooms + bedrooms + beds + price 
                 + securitydeposit + cleaning_fee + guests_included + extra_people + minimum_nights
                 + maximum_nights + availability_365, data = NewLstingsDF ,method = "rf",trControl = control3,verbose=FALSE)
modelRF

set.seed(7)
#train the SVM model
modelSVM <- train(get_visits~host_response_rate + host_acceptance_rate + host_listings_count + accommodates + 
                    bathrooms + bedrooms + beds + price 
                  + securitydeposit + cleaning_fee + guests_included + extra_people + minimum_nights
                  + maximum_nights + availability_365, data = NewLstingsDF ,method = "svmRadial",trControl = control3)
modelSVM
#collect resamples
results <- resamples(list(NB=modelNB,RF=modelRF,SVM=modelSVM))
#summarize the distributions
summary(results)
#boxplot of results
bwplot(results)
#dotplot of results
dotplot(results)
