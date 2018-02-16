#This R script uses Logistic Regression classification technique to classify a listing whether 
#it gets a visit or not based on other features in the listings.csv data set.

#Reading the listings file
listing <- read.csv(file.choose(),stringsAsFactors = FALSE)

#Checking the class of the "price" column
class(listing$price)

#Price is in character format - So converting it into factor format
listing$price <- as.factor(listing$price)

#Splitting the string in the price column, ie, removing the dollar sign from all the values in the column - $xyz => xyz
listing$price  <- substr(listing$price,2,10)

#Converting price column into integer format
listing$price <- as.integer(listing$price)

#Changing the column name of a particular column
colnames(listing)[colnames(listing)=="neighbourhood_cleansed"] <- "neighborhoods"

#Check for outliers in the price column of the listings dataset
#This is done by checking whether there are any extreme values present

#Now we check the top maximum and minimum vaues of the "price" column
max(listing$price,na.rm = TRUE)
min(listing$price,na.rm = TRUE)


#This shows us that there is a "0" value as the minimum value and "999" value as the maximum value.
neighborhoods0 <- subset(listing, price == 0, select = c(id,neighborhoods,room_type,amenities,price))
View(neighborhoods0)

#We go on remove the value that has a "0" in the price as it is not a valid value for price!
final_listings <- subset(listing, !(listing$price == 0 | is.na(listing$price)))
View(final_listings)

#selecting relevant variables for the dataset
listingSubset<- subset(final_listings, select = c(id,host_since, host_response_time, 
                                 host_is_superhost,host_response_rate, host_neighbourhood, 
                                 host_has_profile_pic,host_identity_verified,neighborhoods, 
                                 security_deposit,cleaning_fee, guests_included, 
                                 extra_people,minimum_nights,number_of_reviews,instant_bookable,cancellation_policy,
                                 require_guest_profile_picture,require_guest_phone_verification))

#add a column to the data frame called as "GetsMoreVisits" which can take 2 values yes/No
#This column is our dependent variable which we will be predicting in our testing data set
#This column tells us if a listing gets more visits or not

View(listingSubset)

#********************-------------------------------**********************
#Start from here to run the script again
Rental_Listings <- read.csv(file.choose(),header=T)
#removing blank valued columns from the data frame
RentalListings <- subset(listingSubset, listingSubset$host_response_time != "" | listingSubset$host_is_superhost != "" 
                                        | listingSubset$host_has_profile_pic != "" | listingSubset$host_identity_verified != "")
View(RentalListings)

#cleaning columns security_deposit, cleaning_fee,extra_people to remove $ value
class(RentalListings$security_deposit)
RentalListings$security_deposit <- as.factor(RentalListings$security_deposit)
RentalListings$security_deposit <- substr(RentalListings$security_deposit,2,10)
RentalListings$security_deposit <- as.integer(RentalListings$security_deposit)

#cleaning_fee column
class(RentalListings$cleaning_fee)
RentalListings$cleaning_fee <- as.factor(RentalListings$cleaning_fee)

RentalListings$cleaning_fee <- substr(RentalListings$cleaning_fee,2,10)
RentalListings$cleaning_fee <- as.integer(RentalListings$cleaning_fee)

#extra_people column
class(RentalListings$extra_people)
RentalListings$extra_people <- as.factor(RentalListings$extra_people)
RentalListings$extra_people <- substr(RentalListings$extra_people,2,10)
RentalListings$extra_people <- as.integer(RentalListings$extra_people)

#converting all NA values in security_deposit to 0
RentalListings[is.na(RentalListings)] <- 0

#create a new column "charge_for_extra_people" as a categorical variable with 2 values
#YES and NO
RentalListings$charge_for_extra_people <- c(1:nrow(RentalListings))
#converting the data frame to a matrix 
m.rentalListings <- as.matrix(RentalListings)

#assigns a yes to "charge_for_extra_people" column if value in "extra_people" column
#is greater than 0 else assigns a No
for(i in 1:nrow(RentalListings))
{
  if(as.integer(m.rentalListings[i,13]) == 0)
  {
    m.rentalListings[i,20] <- "No"
  }
  else
  {
    m.rentalListings[i,20] <- "Yes"
  }
}

#converting the m.rentalListings matrix back to RentalListings data frame
RentalListings <- as.data.frame(m.rentalListings)
RentalListings <- subset(RentalListings,RentalListings$cancellation_policy != 'no_refunds')
View(RentalListings)

#write.csv(RentalListings, "RentalListings.csv")
#creating a testing and training data set 
samp_size <- floor(0.70 * nrow(RentalListings))
#set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(RentalListings)), size = samp_size)

train_rent_ltngs <- RentalListings[train_ind, ]
test_rent_ltngs <- RentalListings[-train_ind, ]
#adding a new column in the training data set to describe if rentals get more visits or not
#Column values with 0 are assigned a NO
#Column values not equal to 0 are assigned a YES
#Column values in number_of_reviews range from 0 to 506
View(train_rent_ltngs)
write.csv(train_rent_ltngs,"TrainingRentalListings.csv")
write.csv(test_rent_ltngs,"TestingRentalListings.csv")
#***********************************---------------**************************
#Start from here to perform glm on training data set
train_rent_ltngs <- read.csv(file.choose(),header=T)
View(train_rent_ltngs)
#adding a new column to assign labels for our dependent variable "get_more_reviews" 
train_rent_ltngs$get_more_visits <- c(1:nrow(train_rent_ltngs))

#converting the training data frame into a matrix
m.train_rent_ltngs <- as.matrix(train_rent_ltngs)

#assigns a label 1(yes) to "get_more_visits" column of the training data set 
#if value in "number_of_reviews" column
#is greater than 0 else assigns a label 0 (No)
for(i in 1:nrow(train_rent_ltngs))
{
  if(as.integer(m.train_rent_ltngs[i,15]) > 0)
  {
    m.train_rent_ltngs[i,21] <- 1
  }
  else
  {
    m.train_rent_ltngs[i,21] <- 0
  }
}

#convert the "m.train_rent_ltngs" matrix to "train_rent_ltngs" data frame
train_rent_ltngs <- as.data.frame(m.train_rent_ltngs)
View(train_rent_ltngs)
# 11 identified independent features
#host_response_time,host_is_superhost,host_has_profile_pic
#host_identity_verified,security_deposit,cleaning_fee,instant_bookable,
#cancellation_policy,require_guest_profile_picture,
#require_guest_phone_verification,charge_for_extra_people

#create the logistic model on training data set
attach(train_rent_ltngs)
train_rent_ltngs$host_response_time <- factor(train_rent_ltngs$host_response_time)
train_rent_ltngs$host_is_superhost <- factor(train_rent_ltngs$host_is_superhost)
train_rent_ltngs$host_has_profile_pic <- factor(train_rent_ltngs$host_has_profile_pic)
train_rent_ltngs$host_identity_verified <- factor(train_rent_ltngs$host_identity_verified)
train_rent_ltngs$instant_bookable <- factor(train_rent_ltngs$instant_bookable)
train_rent_ltngs$cancellation_policy <- factor(train_rent_ltngs$cancellation_policy)
train_rent_ltngs$require_guest_profile_picture <- factor(train_rent_ltngs$require_guest_profile_picture)
train_rent_ltngs$require_guest_phone_verification <- factor(train_rent_ltngs$require_guest_phone_verification)
train_rent_ltngs$charge_for_extra_people <- factor(train_rent_ltngs$charge_for_extra_people)

install.packages("aod")
library(aod)
logitRentalModel <- glm(get_more_visits~ host_response_time+host_is_superhost
                          +host_has_profile_pic+host_identity_verified
                          +instant_bookable+as.integer(security_deposit)
                          +as.integer(cleaning_fee)+cancellation_policy
                          +require_guest_profile_picture+require_guest_phone_verification
                          +charge_for_extra_people,
                          data = train_rent_ltngs,family = binomial)

levels(train_rent_ltngs$host_response_time)
levels(train_rent_ltngs$host_is_superhost)
levels(train_rent_ltngs$host_has_profile_pic)
levels(train_rent_ltngs$host_identity_verified)
levels(train_rent_ltngs$instant_bookable)
levels(train_rent_ltngs$cancellation_policy)
levels(train_rent_ltngs$require_guest_profile_picture)
levels(train_rent_ltngs$require_guest_phone_verification)
levels(train_rent_ltngs$charge_for_extra_people)

logitRentalModel
summary(logitRentalModel)

#eliminating non-significant features in our model.
final_logit <- glm(get_more_visits~ host_response_time+host_is_superhost
                   +host_has_profile_pic+host_identity_verified
                   +instant_bookable
                   +as.integer(cleaning_fee)+cancellation_policy
                   +require_guest_profile_picture+require_guest_phone_verification
                   +charge_for_extra_people,
                   data = train_rent_ltngs,family = binomial)
summary(final_logit)

final_logit
#computing odd ratios only
exp(coef(final_logit))

#*********************-------------------------------------******************************
#reading the testing data set1
test_rent_ltngs <- read.csv(file.choose(),header=T)

#adding a new column to assign labels for our dependent variable "get_more_reviews" 
test_rent_ltngs$get_more_visits <- c(1:nrow(test_rent_ltngs))

#converting the testing data frame into a matrix
m.test_rent_ltngs <- as.matrix(test_rent_ltngs)

#assigns a label 1(yes) to "get_more_visits" column of the testing data set 
#if value in "number_of_reviews" column
#is greater than 0 else assigns a label 0 (No)
for(i in 1:nrow(test_rent_ltngs))
{
  if(as.integer(m.test_rent_ltngs[i,15]) > 0)
  {
    m.test_rent_ltngs[i,21] <- 1
  }
  else
  {
    m.test_rent_ltngs[i,21] <- 0
  }
}

#convert the "m.test_rent_ltngs" matrix to "test_rent_ltngs" data frame
test_rent_ltngs <- as.data.frame(m.test_rent_ltngs)

#adding a column "predict_more_visits" to the testing data set for predicting the response
test_rent_ltngs$predict_more_visits <- c(1:nrow(test_rent_ltngs))

View(test_rent_ltngs)
#predicting the response
test_rent_ltngs$predict_more_visits <- predict(final_logit,newdata = test_rent_ltngs, 
                                               type="response")
#adding a column to convert probability values into yes(1) and no(0) labels
test_rent_ltngs$predict_final_visits <- c(1:nrow(test_rent_ltngs))

#testing with a threshold value as 0.5
test_rent_ltngs$predict_final_visits <- ifelse(test_rent_ltngs$predict_more_visits > 0.5, 1,0)

View(test_rent_ltngs)

#calculating accuracy
#converting data frame to a matrix
m.test_rent_ltngs <- as.matrix(test_rent_ltngs)

countVisits <- 0
for(i in 1:nrow(test_rent_ltngs))
{
  if(as.integer(m.test_rent_ltngs[i,21]) == as.integer(m.test_rent_ltngs[i,23]))
  {
    countVisits <- countVisits + 1
  }
}

#accuracy of our model
accuracy <- countVisits/nrow(test_rent_ltngs)
accuracy
