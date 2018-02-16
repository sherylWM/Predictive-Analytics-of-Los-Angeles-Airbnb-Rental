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
listingSubset<- subset(final_listings, select = c(id,host_response_time,host_response_rate, host_acceptance_rate,
                                                  host_is_superhost,host_has_profile_pic,host_identity_verified,
                                                  host_listings_count, room_type, accommodates,bathrooms,bedrooms,beds,
                                                  price, security_deposit,cleaning_fee, guests_included, extra_people,
                                                  minimum_nights, maximum_nights,availability_365,number_of_reviews,
                                                  instant_bookable,cancellation_policy,require_guest_profile_picture,require_guest_phone_verification
))


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
    RentalListings[i,27] <- 1 
  }
  else
  {
    RentalListings[i,27] <- 0
  }
  
}

ListingsDF <- as.data.frame(RentalListings)

ListingsDF <- subset(ListingsDF, ListingsDF$host_response_time != "" | ListingsDF$host_is_superhost != "" 
                     | ListingsDF$host_has_profile_pic != "" | ListingsDF$host_identity_verified != "")

ListingsDF <- subset(ListingsDF, ListingsDF$cancellation_policy != 'no_refunds')

ListingsDF <- na.omit(ListingsDF)

View(ListingsDF)

#Performing one-hot encoding for categorical variables with 3 or more values
FinalListings <- cbind(with(ListingsDF, model.matrix(~ host_response_time + 0)),
                       with(ListingsDF, model.matrix(~ room_type + 0)),
                       with(ListingsDF, model.matrix(~ cancellation_policy + 0))
                       )
View(FinalListings)
#converting the matrix into data frame
FinalListingsDF <- as.data.frame(FinalListings)

#add a new column for host_response_rate and host_acceptance_rate in percent format
ListingsDF$host_response_rate <- as.integer(as.character(ListingsDF$host_response_rate))
ListingsDF$host_acceptance_rate <- as.integer(as.character(ListingsDF$host_acceptance_rate))

FinalListingsDF$host_response_rate <- c(1:nrow(ListingsDF))
FinalListingsDF$host_acceptance_rate <- c(1:nrow(ListingsDF))

FinalListingsDF$host_response_rate <- ifelse(is.na(ListingsDF$host_response_rate) == TRUE, NA,ListingsDF$host_response_rate/100)
FinalListingsDF$host_acceptance_rate <- ifelse(is.na(ListingsDF$host_acceptance_rate) == TRUE, NA,ListingsDF$host_acceptance_rate/100)

#Converting all predictive binary features into 1 and -1
FinalListingsDF$host_is_superhost <- c(1:nrow(ListingsDF))
FinalListingsDF$host_has_profile_pic <- c(1:nrow(ListingsDF))
FinalListingsDF$host_identity_verified <- c(1:nrow(ListingsDF))
FinalListingsDF$instant_bookable <- c(1:nrow(ListingsDF))
FinalListingsDF$require_guest_profile_picture <- c(1:nrow(ListingsDF))
FinalListingsDF$require_guest_phone_verification <- c(1:nrow(ListingsDF))

FinalListingsDF$host_is_superhost <- ifelse(ListingsDF$host_is_superhost == 't', 1, -1)
FinalListingsDF$host_has_profile_pic <- ifelse(ListingsDF$host_has_profile_pic == 't', 1, -1)
FinalListingsDF$host_identity_verified <- ifelse(ListingsDF$host_identity_verified == 't', 1, -1)
FinalListingsDF$instant_bookable <- ifelse(ListingsDF$instant_bookable == 't', 1, -1)
FinalListingsDF$require_guest_profile_picture <- ifelse(ListingsDF$require_guest_profile_picture == 't', 1, -1)
FinalListingsDF$require_guest_phone_verification <- ifelse(ListingsDF$require_guest_phone_verification == 't', 1, -1)

FinalListingsDF$host_response_time <- NULL

#creating a column for get_visits (binary outcome)into 1s(Yes) and 0s(No)
FinalListingsDF$get_visits <- c(1:nrow(ListingsDF))
FinalListingsDF$get_visits <- ListingsDF$get_visits

#Creating the min-max normalzation function
normalize <- function(x)
{
  return((x - min(x))/ (max(x) - min(x)))
}

#Normalizing all continuous variables between values 0 and 1 using min-max normalization
FinalListingsDF$host_listings_count <- c(1:nrow(ListingsDF))
FinalListingsDF$accommodates <- c(1:nrow(ListingsDF))
FinalListingsDF$bathrooms <- c(1:nrow(ListingsDF))
FinalListingsDF$bedrooms <- c(1:nrow(ListingsDF))
FinalListingsDF$beds <- c(1:nrow(ListingsDF))
FinalListingsDF$price <- c(1:nrow(ListingsDF))
FinalListingsDF$security_deposit <- c(1:nrow(ListingsDF))
FinalListingsDF$cleaning_fee <- c(1:nrow(ListingsDF))
FinalListingsDF$guests_included <- c(1:nrow(ListingsDF))
FinalListingsDF$extra_people <- c(1:nrow(ListingsDF))
FinalListingsDF$min_nights <- c(1:nrow(ListingsDF))
FinalListingsDF$max_nights <- c(1:nrow(ListingsDF))
FinalListingsDF$availability_365 <- c(1:nrow(ListingsDF))


View(FinalListingsDF)

FinalListingsDF$host_listings_count <- normalize(as.integer(as.character(ListingsDF$host_listings_count)))
FinalListingsDF$accommodates <- normalize(as.integer(as.character(ListingsDF$accommodates)))

FinalListingsDF$bathrooms <- as.integer(as.character(ListingsDF$bathrooms))
FinalListingsDF$bedrooms <- as.integer(as.character(ListingsDF$bedrooms))
FinalListingsDF$beds <- as.integer(as.character(ListingsDF$beds))
FinalListingsDF$security_deposit <- as.integer(as.character(ListingsDF$security_deposit))
FinalListingsDF$cleaning_fee <- as.integer(as.character(ListingsDF$cleaning_fee))


FinalListingsDF$price <- normalize(as.integer(as.character(ListingsDF$price)))
FinalListingsDF$guests_included <- normalize(as.integer(as.character(ListingsDF$guests_included)))
FinalListingsDF$extra_people <- normalize(as.integer(as.character(ListingsDF$extra_people)))
FinalListingsDF$min_nights <- normalize(as.integer(as.character(ListingsDF$minimum_nights)))
FinalListingsDF$max_nights <- normalize(as.integer(as.character(ListingsDF$maximum_nights)))
FinalListingsDF$availability_365 <- normalize(as.integer(as.character(ListingsDF$availability_365)))


#Normaling continuous variables that had NA values earlier
FinalListingsDF$bathrooms <- normalize(as.integer(as.character(ListingsDF$bathrooms)))
FinalListingsDF$bedrooms <- normalize(as.integer(as.character(ListingsDF$bedrooms)))
FinalListingsDF$beds <- normalize(as.integer(as.character(ListingsDF$beds)))
FinalListingsDF$security_deposit <- normalize(as.integer(as.character(ListingsDF$security_deposit)))
FinalListingsDF$cleaning_fee <- normalize(as.integer(as.character(ListingsDF$cleaning_fee)))

FinalListingsDF$get_visits <- as.numeric(as.character(FinalListingsDF$get_visits))

write.csv(FinalListingsDF,"FinalListingsCaret.csv")

#FinalListingsDF is the data frame to be used for applying NN classifier
#Creating testing and training data sets from FinalListingsDF

#creating a sample size
samp_size <- floor(0.70 * nrow(FinalListingsDF))
#set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(FinalListingsDF)), size = samp_size)

ListingsNNTrain <- FinalListingsDF[train_ind, ]
ListingsNNTest <- FinalListingsDF[-train_ind, ]

View(ListingsNNTrain)
View(ListingsNNTest)


install.packages("neuralnet")
library(neuralnet)
?neuralnet

#Creating a multi-layer perceptron model using neuralnet package using 
listings_NNModel <- neuralnet(formula = get_visits ~ ListingsNNTrain$`host_response_timea few days or more`
                              + ListingsNNTrain$`host_response_timeN/A` + ListingsNNTrain$`host_response_timewithin a day`
                              + ListingsNNTrain$`host_response_timewithin a few hours` + ListingsNNTrain$`host_response_timewithin an hour`
                              + ListingsNNTrain$`room_typeEntire home/apt` + ListingsNNTrain$`room_typePrivate room`
                              + ListingsNNTrain$`room_typeShared room` + ListingsNNTrain$cancellation_policyflexible
                              + ListingsNNTrain$cancellation_policymoderate + ListingsNNTrain$cancellation_policyno_refunds
                              + ListingsNNTrain$cancellation_policystrict + ListingsNNTrain$cancellation_policysuper_strict_60
                              + ListingsNNTrain$host_response_rate + ListingsNNTrain$host_acceptance_rate
                              + ListingsNNTrain$host_has_profile_pic + ListingsNNTrain$host_is_superhost
                              + ListingsNNTrain$host_identity_verified + ListingsNNTrain$instant_bookable
                              + ListingsNNTrain$require_guest_profile_picture + ListingsNNTrain$require_guest_phone_verification
                              + ListingsNNTrain$host_listings_count + ListingsNNTrain$accommodates
                              + ListingsNNTrain$bathrooms + ListingsNNTrain$bedrooms + ListingsNNTrain$beds
                              + ListingsNNTrain$price + ListingsNNTrain$security_deposit + ListingsNNTrain$cleaning_fee
                              + ListingsNNTrain$guests_included + ListingsNNTrain$extra_people 
                              + ListingsNNTrain$min_nights + ListingsNNTrain$max_nights
                              + ListingsNNTrain$availability_365, data = ListingsNNTrain, hidden = 4, act.fct = "logistic"
                             )
#Visualizing the network topology
plot(listings_NNModel)

##get_visits is 21st column
names(ListingsNNTest)

#Creating the data frame by eliminating the get_visits outcome feature
ListingsNNTestingDF <- ListingsNNTest[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30
                                    ,31,32,33,34,35)]

View(ListingsNNTest)

#predicting if a listing gets visits or not using testing data set
NNModel1results <- compute(listings_NNModel,ListingsNNTestingDF)

#Obtaining predicted outcome value
predicted_get_visits <- NNModel1results$net.result

#testing the accuracy by creating a confusion matrix as NN is here used as a classifier
norm_predicted_get_visits <- c(1:nrow(ListingsNNTest))
norm_predicted_get_visits <- ifelse(predicted_get_visits > 0.5, 1,0)
table(norm_predicted_get_visits, ListingsNNTest$get_visits)


#Creating a multi-layer perceptron model 2 using neuralnet package with 2 hidden layers
listings_NNModel2 <- neuralnet(formula = get_visits ~ ListingsNNTrain$`host_response_timea few days or more`
                              + ListingsNNTrain$`host_response_timeN/A` + ListingsNNTrain$`host_response_timewithin a day`
                              + ListingsNNTrain$`host_response_timewithin a few hours` + ListingsNNTrain$`host_response_timewithin an hour`
                              + ListingsNNTrain$`room_typeEntire home/apt` + ListingsNNTrain$`room_typePrivate room`
                              + ListingsNNTrain$`room_typeShared room` + ListingsNNTrain$cancellation_policyflexible
                              + ListingsNNTrain$cancellation_policymoderate + ListingsNNTrain$cancellation_policyno_refunds
                              + ListingsNNTrain$cancellation_policystrict + ListingsNNTrain$cancellation_policysuper_strict_60
                              + ListingsNNTrain$host_response_rate + ListingsNNTrain$host_acceptance_rate
                              + ListingsNNTrain$host_has_profile_pic + ListingsNNTrain$host_is_superhost
                              + ListingsNNTrain$host_identity_verified + ListingsNNTrain$instant_bookable
                              + ListingsNNTrain$require_guest_profile_picture + ListingsNNTrain$require_guest_phone_verification
                              + ListingsNNTrain$host_listings_count + ListingsNNTrain$accommodates
                              + ListingsNNTrain$bathrooms + ListingsNNTrain$bedrooms + ListingsNNTrain$beds
                              + ListingsNNTrain$price + ListingsNNTrain$security_deposit + ListingsNNTrain$cleaning_fee
                              + ListingsNNTrain$guests_included + ListingsNNTrain$extra_people 
                              + ListingsNNTrain$min_nights + ListingsNNTrain$max_nights
                              + ListingsNNTrain$availability_365, data = ListingsNNTrain, hidden = c(3,2), act.fct = "tanh"
                              )

plot(listings_NNModel2)


#predicting if a listing gets visits or not using testing data set
NNModel2results <- compute(listings_NNModel2,ListingsNNTestingDF)

#Obtaining predicted outcome value
predicted_get_visits2 <- NNModel2results$net.result

#testing the accuracy by creating a confusion matrix as NN is here used as a classifier
norm_predicted_get_visits2 <- c(1:nrow(ListingsNNTest))
norm_predicted_get_visits2 <- ifelse(predicted_get_visits2 > 0.5, 1,0)
table(norm_predicted_get_visits2, ListingsNNTest$get_visits)


