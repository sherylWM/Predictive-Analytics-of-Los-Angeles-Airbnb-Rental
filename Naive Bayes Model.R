#This R script contains code related to creating a Naive Bayes classifier using e1071 package
#to classify a listing by price as cheap, moderate 
# and expensive using only categorical and ratio independent features

#Reading the file 
listingprices <- read.csv(file.choose(),header=T)
View(listingprices)
#installing and loading the Hmisc package
install.packages("Hmisc")
library(Hmisc)
#splitting the data frame based on a listing's price into 3 groups
df <- split(listingprices,cut2(listingprices$price, g = 3))

#write.csv(df[1], "NBPrices.csv")
#write.csv(df[2], "NBPrices1.csv")
#write.csv(df[3], "NBPrices2.csv")

#adding a new column that assigns a label to the data frame called price_category
listingprices$price_category <- c(1:nrow(listingprices))

#converting it into a matrix
m.listingprices <- as.matrix(listingprices)
#creating conditional if else loop to assign "cheap", "moderate" and "expensive" categories
for(i in 1: nrow(listingprices))
{
  if(as.integer(m.listingprices[i,61]) >= 10 & as.integer(m.listingprices[i,61] )<=80)
  {
    m.listingprices[i,96] <- "cheap"
  }
  else if(as.integer(m.listingprices[i,61]) >= 81 & as.integer(m.listingprices[i,61] )<=145)
  {
    m.listingprices[i,96] <- "moderate"
  }
  else
  {
    m.listingprices[i,96] <- "expensive"
  }
}
#final data frame which has all the values
final_listingprices <- as.data.frame(m.listingprices)
View(final_listingprices)

#creating a testing and training data set 
samp_size <- floor(0.70 * nrow(final_listingprices))
#set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(final_listingprices)), size = samp_size)

train_price_ltngs <- final_listingprices[train_ind, ]
test_price_ltngs <- final_listingprices[-train_ind, ]

#View(train_price_ltngs)

#creating a subset of the training data set to include just features that might have an effect on a listing price
train_df_price_lstngs <- subset(train_price_ltngs, select = c(host_response_rate,host_acceptance_rate,
                                                              host_is_superhost,
                                                              host_has_profile_pic,host_identity_verified,
                                                              accommodates,
                                                              bathrooms,bedrooms,
                                                              beds,guests_included,number_of_reviews,
                                                              instant_bookable,reviews_per_month,
                                                              price_category
                                                              ))
#View(train_df_price_lstngs)

View(test_price_ltngs)
#creating a subset of the testing data set to include just features that might have an effect on a listing price
test_df_price_lstngs <- subset(test_price_ltngs, select = c(host_response_rate,host_acceptance_rate,
                                                            host_is_superhost,
                                                            host_has_profile_pic,
                                                            host_identity_verified,accommodates,
                                                            bathrooms,bedrooms,
                                                            beds,guests_included,
                                                            number_of_reviews,
                                                            instant_bookable,reviews_per_month,
                                                            price_category))

#View(test_df_price_lstngs)
#loading the Naive Bayes package in R
library(e1071)
#creating the Naive Bayes classifier
lstngs_classifier <- naiveBayes(price_category~.,data = train_df_price_lstngs)

#using the Naive Bayes classifier on our testing data set
lsting_price_pred <- predict(lstngs_classifier,test_df_price_lstngs)

install.packages("gmodels")
library(gmodels)

#generating the Confusion Matrix
CrossTable(lsting_price_pred,test_df_price_lstngs$price_category,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted','actual'))

#Adding Laplace Estimator to create the Naive Bayes classifier
lstngs_classifier2 <- naiveBayes(price_category~.,data = train_df_price_lstngs,laplace = 1)

#using the Naive Bayes classifier on our testing data set
lsting_price_pred2 <- predict(lstngs_classifier2,test_df_price_lstngs)

#generating the Confusion Matrix
CrossTable(lsting_price_pred2,test_df_price_lstngs$price_category,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted','actual'))

#***********************----------------------------------*********************************
#creating a subset of the training data set to include just features that might have an effect on a listing price
train_df_price_lstngs <- subset(train_price_ltngs, select = c(host_is_superhost,
                                                              host_identity_verified,
                                                              accommodates,
                                                              bathrooms,bedrooms,
                                                              beds,guests_included,number_of_reviews,
                                                              instant_bookable,reviews_per_month,
                                                              price_category
))
#View(train_df_price_lstngs)

#View(test_price_ltngs)
#creating a subset of the testing data set to include just features that might have an effect on a listing price
test_df_price_lstngs <- subset(test_price_ltngs, select = c(host_is_superhost,
                                                            host_identity_verified,
                                                            accommodates,
                                                            bathrooms,bedrooms,
                                                            beds,guests_included,number_of_reviews,
                                                            instant_bookable,reviews_per_month,
                                                            price_category))

#View(test_df_price_lstngs)
#loading the Naive Bayes package in R
library(e1071)
#creating the Naive Bayes classifier
lstngs_classifier <- naiveBayes(price_category~.,data = train_df_price_lstngs)

#using the Naive Bayes classifier on our testing data set
lsting_price_pred <- predict(lstngs_classifier,test_df_price_lstngs)

install.packages("gmodels")
library(gmodels)

#generating the Confusion Matrix
CrossTable(lsting_price_pred,test_df_price_lstngs$price_category,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted','actual'))

#Adding Laplace Estimator to create the Naive Bayes classifier
lstngs_classifier2 <- naiveBayes(price_category~.,data = train_df_price_lstngs,laplace = 1)

#using the Naive Bayes classifier on our testing data set
lsting_price_pred2 <- predict(lstngs_classifier2,test_df_price_lstngs)

#generating the Confusion Matrix
CrossTable(lsting_price_pred2,test_df_price_lstngs$price_category,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted','actual'))
