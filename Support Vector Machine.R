#This R script uses Logistic Regression classification technique to classify a listing whether 
#it gets a visit or not based on other features in the listings.csv data set.

#Reading the listings file
listing <- read.csv(file.choose(),header = T)

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
listingSubset<- subset(final_listings, select = c(id, host_response_time,host_response_rate, host_acceptance_rate,
                                                  host_is_superhost,host_has_profile_pic,host_identity_verified,
                                                  host_listings_count, room_type, accommodates,bathrooms,bedrooms,beds,
                                                  price, security_deposit,cleaning_fee, guests_included, extra_people,
                                                  minimum_nights, maximum_nights,availability_365,number_of_reviews,
                                                  instant_bookable,cancellation_policy,require_guest_profile_picture,require_guest_phone_verification
                                                  ))

#add a column to the data frame called as "GetsMoreVisits" which can take 2 values yes/No
#This column is our dependent variable which we will be predicting in our testing data set
#This column tells us if a listing gets more visits or not

View(listingSubset)
class(listingSubset$price)

#cleaning columns security_deposit, cleaning_fee,extra_people to remove $ value
class(listingSubset$security_deposit)
listingSubset$security_deposit <- as.factor(listingSubset$security_deposit)
listingSubset$security_deposit <- substr(listingSubset$security_deposit,2,10)
listingSubset$security_deposit <- as.integer(listingSubset$security_deposit)

#cleaning_fee column
class(listingSubset$cleaning_fee)
listingSubset$cleaning_fee <- as.factor(listingSubset$cleaning_fee)
listingSubset$cleaning_fee <- substr(listingSubset$cleaning_fee,2,10)
listingSubset$cleaning_fee <- as.integer(listingSubset$cleaning_fee)

#extra_people column
class(listingSubset$extra_people)
listingSubset$extra_people <- as.factor(listingSubset$extra_people)
listingSubset$extra_people <- substr(listingSubset$extra_people,2,10)
listingSubset$extra_people <- as.integer(listingSubset$extra_people)


listingSubset$get_visits <- c(1:nrow(listingSubset))
View(listingSubset)

RentalListings <- as.matrix(listingSubset)

for(i in 1:nrow(RentalListings))
{
  if (RentalListings[i,3] == 'N/A')
  {
    RentalListings[i,3] = NA;
  }
  
}


for(i in 1:nrow(RentalListings))
{
  if (RentalListings[i,4] == 'N/A')
  {
    RentalListings[i,4] = NA;
  }
  
}



########Saving the ListingsDF#############################################################

for(i in 1:nrow(RentalListings))
{
  RentalListings[i,3]<-substr(RentalListings[i,3],1, nchar(RentalListings[i,3])-1)
  RentalListings[i,4] <- substr(RentalListings[i,4],1, nchar(RentalListings[i,4])-1)
}


for(i in 1:nrow(RentalListings))
{
  if(as.integer(RentalListings[i,22]) > 0)
  {
    RentalListings[i,27] <- 'Yes' 
  }
  else
  {
    RentalListings[i,27] <- 'No'
  }
  
}

ListingsDF <- as.data.frame(RentalListings)

ListingsDF <- subset(ListingsDF, ListingsDF$host_response_time != "" | ListingsDF$host_is_superhost != "" 
                     | ListingsDF$host_has_profile_pic != "" | ListingsDF$host_identity_verified != "")

ListingsDF <- subset(ListingsDF, ListingsDF$cancellation_policy != 'no_refunds')

View(ListingsDF)


FinalListings <- cbind(with(ListingsDF, model.matrix(~ host_response_time + 0)),
                            with(ListingsDF, model.matrix(~ host_is_superhost + 0)),
                            with(ListingsDF, model.matrix(~ host_has_profile_pic + 0)),
                            with(ListingsDF, model.matrix(~ host_identity_verified + 0)),
                            with(ListingsDF, model.matrix(~ room_type + 0)),
                            with(ListingsDF, model.matrix(~ instant_bookable + 0)),
                            with(ListingsDF, model.matrix(~ cancellation_policy + 0)),
                            with(ListingsDF, model.matrix(~ require_guest_profile_picture + 0)),
                            with(ListingsDF, model.matrix(~ require_guest_phone_verification + 0))
                            )

FinalListingsDF <- as.data.frame(FinalListings)


View(FinalListingsDF)

FinalListingsDF$host_response_time <- NULL
FinalListingsDF$host_is_superhost <- NULL
FinalListingsDF$host_has_profile_pic <- NULL
FinalListingsDF$host_identity_verified <- NULL
FinalListingsDF$host_response_rate <- ListingsDF$host_response_rate
FinalListingsDF$host_acceptance_rate <- ListingsDF$host_acceptance_rate
FinalListingsDF$host_listings_count <- ListingsDF$host_listings_count
FinalListingsDF$accommodates <- ListingsDF$accommodates
FinalListingsDF$bathrooms <- ListingsDF$bathrooms
FinalListingsDF$bedrooms <- ListingsDF$bedrooms
FinalListingsDF$beds <- ListingsDF$beds
FinalListingsDF$price <- ListingsDF$price
FinalListingsDF$securitydeposit <- ListingsDF$security_deposit
FinalListingsDF$cleaning_fee <- ListingsDF$cleaning_fee
FinalListingsDF$guests_included <- ListingsDF$guests_included
FinalListingsDF$extra_people <- ListingsDF$extra_people
FinalListingsDF$minimum_nights <- ListingsDF$minimum_nights
FinalListingsDF$maximum_nights <- ListingsDF$maximum_nights
FinalListingsDF$availability_365 <- ListingsDF$availability_365
FinalListingsDF$get_visits <- ListingsDF$get_visits

#saving the data frame
write.csv(FinalListingsDF, "FinalListingsSVM.csv")

#Creating a the data frame that contains only integer values
FinalListingsDF$`host_response_timea few days or more` <- as.integer(as.character(FinalListingsDF$`host_response_timea few days or more`))
FinalListingsDF$`host_response_timeN/A` <- as.integer(as.character(FinalListingsDF$`host_response_timeN/A`))
FinalListingsDF$`host_response_timewithin a day` <- as.integer(as.character(FinalListingsDF$`host_response_timewithin a day`))
FinalListingsDF$`host_response_timewithin a few hours` <- as.integer(as.character(FinalListingsDF$`host_response_timewithin a few hours`))
FinalListingsDF$`host_response_timewithin an hour` <- as.integer(as.character(FinalListingsDF$`host_response_timewithin an hour`))

FinalListingsDF$host_is_superhostf <- as.integer(as.character(FinalListingsDF$host_is_superhostf))
FinalListingsDF$host_is_superhostt <- as.integer(as.character(FinalListingsDF$host_is_superhostt))

FinalListingsDF$host_has_profile_picf <- as.integer(as.character(FinalListingsDF$host_has_profile_picf))
FinalListingsDF$host_has_profile_pict <- as.integer(as.character(FinalListingsDF$host_has_profile_pict))

FinalListingsDF$host_identity_verifiedf <- as.integer(as.character(FinalListingsDF$host_identity_verifiedf))
FinalListingsDF$host_identity_verifiedt <- as.integer(as.character(FinalListingsDF$host_identity_verifiedt))

FinalListingsDF$`room_typeEntire home/apt` <- as.integer(as.character(FinalListingsDF$`room_typeEntire home/apt`))
FinalListingsDF$`room_typePrivate room` <- as.integer(as.character(FinalListingsDF$`room_typePrivate room`))
FinalListingsDF$`room_typeShared room` <- as.integer(as.character(FinalListingsDF$`room_typeShared room`))

FinalListingsDF$instant_bookablef <- as.integer(as.character(FinalListingsDF$instant_bookablef))
FinalListingsDF$instant_bookablet <- as.integer(as.character(FinalListingsDF$instant_bookablet))

FinalListingsDF$cancellation_policyflexible <- as.integer(as.character(FinalListingsDF$cancellation_policyflexible))
FinalListingsDF$cancellation_policymoderate <- as.integer(as.character(FinalListingsDF$cancellation_policymoderate))
FinalListingsDF$cancellation_policystrict <- as.integer(as.character(FinalListingsDF$cancellation_policystrict))
FinalListingsDF$cancellation_policysuper_strict_60 <- as.integer(as.character(FinalListingsDF$cancellation_policysuper_strict_60))

FinalListingsDF$require_guest_profile_picturef <- as.integer(as.character(FinalListingsDF$require_guest_profile_picturef))
FinalListingsDF$require_guest_profile_picturet <- as.integer(as.character(FinalListingsDF$require_guest_profile_picturet))

FinalListingsDF$require_guest_phone_verificationf <- as.integer(as.character(FinalListingsDF$require_guest_phone_verificationf))
FinalListingsDF$require_guest_phone_verificationt <- as.integer(as.character(FinalListingsDF$require_guest_phone_verificationt))

FinalListingsDF$host_response_rate <- as.integer(as.character(FinalListingsDF$host_response_rate))
FinalListingsDF$host_acceptance_rate <- as.integer(as.character(FinalListingsDF$host_acceptance_rate))
FinalListingsDF$host_listings_count <- as.integer(as.character(FinalListingsDF$host_listings_count))
FinalListingsDF$accommodates <- as.integer(as.character(FinalListingsDF$accommodates))
FinalListingsDF$bathrooms <- as.integer(as.character(FinalListingsDF$bathrooms))
FinalListingsDF$bedrooms <- as.integer(as.character(FinalListingsDF$bedrooms))
FinalListingsDF$beds <- as.integer(as.character(FinalListingsDF$beds))
FinalListingsDF$price <- as.integer(as.character(FinalListingsDF$price))
FinalListingsDF$securitydeposit <- as.integer(as.character(FinalListingsDF$securitydeposit))
FinalListingsDF$cleaning_fee <- as.integer(as.character(FinalListingsDF$cleaning_fee))
FinalListingsDF$guests_included <- as.integer(as.character(FinalListingsDF$guests_included))
FinalListingsDF$extra_people <- as.integer(as.character(FinalListingsDF$extra_people))
FinalListingsDF$minimum_nights <- as.integer(as.character(FinalListingsDF$minimum_nights))
FinalListingsDF$maximum_nights <- as.integer(as.character(FinalListingsDF$maximum_nights))
FinalListingsDF$availability_365 <- as.integer(as.character(FinalListingsDF$availability_365))

View(FinalListingsDF)

str(FinalListingsDF)


#creating a testing and training data set 
samp_size <- floor(0.70 * nrow(FinalListingsDF))
#set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(FinalListingsDF)), size = samp_size)

train_rent_ltngs <- FinalListingsDF[train_ind, ]
test_rent_ltngs <- FinalListingsDF[-train_ind, ]

#Omitting NA values from the data set
train_rent_listingsNA <- na.omit(train_rent_ltngs)
test_rent_listingsNA <- na.omit(test_rent_ltngs)

str(train_rent_listingsNA)

#Loading kernel package for implementing SVM
library(kernlab)

#Creating the classifier 
listings_svm_classifier <- ksvm(get_visits ~ ., data = train_rent_listingsNA, kernel ="vanilladot")
listings_svm_classifier

listings_predictions <- predict(listings_svm_classifier,test_rent_listingsNA)
head(listings_predictions)

#Checking for the length of both predicted and actual values
length(listings_predictions)
length(test_rent_listingsNA$get_visits)


#Creating the Confusion Matrix for Linear SVM
svmResults <- table(listings_predictions,test_rent_listingsNA$get_visits)
svmResults

install.packages("caret")
library(caret)
confusionMatrix(svmResults)


#Creating the classifier using non-linear kernel function
listings_svm_classifier2 <- ksvm(get_visits ~ ., data = train_rent_listingsNA, kernel ="rbfdot")
listings_svm_classifier2

listings_predictions2 <- predict(listings_svm_classifier2,test_rent_listingsNA)
head(listings_predictions2)

#Checking for the length of both predicted and actual values
length(listings_predictions2)
length(test_rent_listingsNA$get_visits)


#Creating the Confusion Matrix for Non-Linear SVM
nonLinearSvmResults <- table(listings_predictions2,test_rent_listingsNA$get_visits)
nonLinearSvmResults

install.packages("caret")
library(caret)
confusionMatrix(nonLinearSvmResults)


