#Reading the listings file
list <- read.csv(file.choose(),stringsAsFactors = FALSE)

#Checking the class of the "price" column
class(list$price)

#Price is in character format - So converting it into factor format
list$price <- as.factor(list$price)

#Splitting the string in the price column, ie, removing the dollar sign from all the values in the column - $xyz => xyz
list$price  <- substr(list$price,2,10)

#Converting price column into integer format
list$price <- as.integer(list$price)

#----------------------------# Data cleaning pertainging to Question 2 and Question 3 #------------------------------------------#

#3.	Create a model that helps hosts in suggesting how to price their listing.
#2.	Suggest improvements/features for a listing based on the neighborhood the listing is located in.

#Selecting only the required columns from the existing dataset
list1 <- subset(list, select = c(id,name,summary,space,description,neighborhood_overview,
                                 notes,host_id,host_response_time,host_response_rate,neighbourhood_cleansed,room_type,
                                 accommodates,bedrooms,bathrooms,amenities,price))

#Changing the column name of a particular column
colnames(list1)[colnames(list1)=="neighbourhood_cleansed"] <- "neighborhoods"

#Check for outliers in the price column of the listings dataset
#This is done by checking whether there are any extreme values present

#Now we check the top maximum and minimum vaues of the "price" column
max(list1$price,na.rm = TRUE)
min(list1$price,na.rm = TRUE)
#This shows us that there is a "0" value as the minimum value and "999" value as the maximum value.

#Exploring which neighborhoods the maximum value belongs to. This is done to determine whether the values are garbage values or not
neighborhoods1 <- subset(list1, price == 999, select = c(neighborhoods,room_type,amenities,price))
View(neighborhoods1)
#The output reveals almost all the listings in the "hollywood hills" area, and so we are not going to discard any of the 
#values as the analysis is going to be done on the basis of neighbirhoods

#We go on remove the value that has a "0" in the price as it is not a valid value for price!
list1<-list1[!(list$price ==0),]

#In order to perform efficient predictions based on neighborhood clustering, we eliminate all 
#the neighborhoods whose frequency is less than five
# Calculating the frequencies of neighborhoods
freq <- table(list1$neighborhoods)

#Sorting the frequencies of neighborhoods obtained 
freq_s <- sort(freq)

#On observation it was found that the top 61 entries had a frequency of five or less than five.
#Thus we extract all the frequencies except the top 61
greatest <- tail(names(freq_s), 187)

#Now we remove all the neighborhoods that are present in "greatest" from "list1" and form a new dataset "list2"
list2 <- subset(list1, list1$neighborhoods %in% greatest)

#Converting the format of the "neighborhoods" and "room_type" columns to factor type
class(list2$neighborhoods)
class(list2$room_type)
list2$neighborhoods = as.factor(list2$neighborhoods )
list2$room_type = as.factor(list2$room_type)

#Performing ANOVA
a <- aov(list2$price~list2$room_type)
b <- aov(list2$price~list2$neighborhoods)

#Printing the summary of ANOVA
summary(a)
summary(b)

#Calculating R square value for a
90722079/(90722079+341243328)
0.2100216

#Calculating R square value for b
70182903/(70182903+361782504)
0.1624734

#Conducting pairwise t test for the "room_type" column
pairwise.t.test(list2$price,list2$room_type,p.adj="bonferroni")

#Assumptions for ANOVA
#Normality
#Creating a subset for "Entire home/apt" and a historam
s.ER <- subset(list2,list2$room_type=="Entire home/apt")
hist(s.ER$price) 

#Creating a subset for "Private room" and create a historam
s.PR <- subset(list2,list2$room_type=="Private room")
hist(s.PR$price) 

#Creating a subset for "Shared room" and create a historam
s.SR <- subset(list2,list2$room_type=="Shared room")
hist(s.SR$price) 

#Homogeneity of Variance - Levene Test
#H0: variances are equal
#Ha: at least one variance is not equal
#p < significance level (0.05)
#Reject H0
#Conclude assumption of homogeneity of variances is violated 
library(car)
leveneTest(list2$price~list2$room_type)

#Finding out the summary of the "price" column
summary(list2$price)

#Find out the mean of the "price" column, based on the neighborhoods
tapply(list2$price,list2$neighborhoods,mean,na.rm=TRUE)

#Finding out the neighbourhood with the maximum price average
avgs <- tapply(list2$price,list2$neighborhoods,mean,na.rm=TRUE)
names(avgs)[which.max(avgs)]
max(tapply(list2$price,list2$neighborhoods,mean,na.rm=TRUE))

#Finding out the neighbourhood with the minimum price average
names(avgs)[which.min(avgs)]
min(tapply(list2$price,list2$neighborhoods,mean,na.rm=TRUE))

#Find out the mean of the "price" column based on the room types
tapply(list2$price,list2$room_type,mean,na.rm=TRUE)

#Finding out the room type with the maximum price average
avgs1 <-tapply(list2$price,list2$room_type,mean,na.rm=TRUE)
names(avgs1)[which.max(avgs1)]
max(tapply(list2$price,list2$room_type,mean,na.rm=TRUE))

#Finding out the room type with the minimum price average
names(avgs1)[which.min(avgs1)]
min(tapply(list2$price,list2$room_type,mean,na.rm=TRUE))

#Create a separate file based on the mean of the overall price and the mean of the price based on room types - read in "ggplot_file"
q <- read.csv(file.choose()) #READING THE FILE BASED ON WHICH WE PLOT ggplot

#import library "ggplot2"
library(ggplot2)

#Creating a ggplot to present a visual representation of the mean of the overall prices and 
#the mean of the prices based on room types 
ggplot(q,aes(x=room,y=price,fill=factor(variable)))+
geom_bar(stat="identity",position="dodge")+
scale_fill_discrete(name="Legend",
                      breaks=c(1, 2),
                      labels=c("Overall Mean Price", "Room Type Based Mean Price"))+
xlab("Room Type")+ylab("Price")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#----------------------------# Data cleaning pertainging to Question 1 #------------------------------------------#

#1.	Classify listings under three labels – “good”, “average” and “bad”

#Import the reviews dataset
reviews <- read.csv(file.choose(), stringsAsFactors = FALSE)

#Checking whether there any NULL values in the reviews dataset
reviews <- reviews[complete.cases(reviews), ]

#Deleting columns that are not required
reviews1$id <- NULL
reviews1$date <- NULL
reviews1$reviewer_name <- NULL












