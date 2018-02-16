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
ListingsDF <- na.omit(ListingsDF)

#Clustering Methods

#K-Means
library(ggplot2)
library(datasets)
library(MASS)
library(klaR)

set.seed(20)
Cluster <- kmeans(ListingsDF[, 14:15], 2, nstart=20)
Cluster
Cluster$cluster <- as.factor(Cluster$cluster)
ggplot(ListingsDF, aes(price, cleaning_fee, color = Cluster$cluster)) + geom_point()

#Heirarchial
hierclust2 <-hclust(dist(ListingsDF[c(-1,-15)]))
plot(hierclust2)

#heirarchy clustering with a small sample 
idx <- sample(1:dim(ListingsDF)[1], 100)
Sample <- ListingsDF[idx,]
Sample$get_visits <- NULL
hc <- hclust(dist(Sample), method="ave")
plot(hc, hang = -1, labels=ListingsDF$get_visits[idx])


#Density Clustering
library(dbscan)
library(ggplot2)
ListingsDF$price <- as.numeric(ListingsDF$price)
ListingsDF$cleaning_fee <- as.numeric(ListingsDF$cleaning_fee)
ListingsDF$security_deposit <- as.numeric(ListingsDF$security_deposit)
DensityCluster <- dbscan(ListingsDF[,14:15], eps=10, minPts = 6)
DensityCluster
DensityCluster$cluster <- as.factor(DensityCluster$cluster)
ggplot(ListingsDF,aes(price, cleaning_fee, D45, color=DensityCluster$cluster)) + geom_point()

#To find best eps value:
dbscan::kNNdistplot(ListingsDF[,14:15], k =  3)
abline(h=0.15, lty=2)


set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
ListingsDF.scaled <- ListingsDF[ ,14:15]
data <- ListingsDF.scaled
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)
