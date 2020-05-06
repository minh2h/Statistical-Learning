
# Exploratory Data Analysis
#how long are guests staying for?
library(dplyr)
library(ggplot2)
dataset <- read.csv("hotel_bookings.csv", stringsAsFactors = FALSE) %>% 
  filter(hotel == "City Hotel")   # Removed all Resort Hotels
head(dataset$country)

# DURATION OF STAY
stay_day<- dataset$stays_in_week_nights
stay_end<- dataset$stays_in_weekend_nights
stay_together <- stay_end + stay_day
summary(stay_together)

stay_summary <-dataset %>% 
  group_by(stay_day+stay_end) %>% # grouping by 'type' column
  summarise(duration = n())  # calculate the name count for each group
stay_summary

ggplot(stay_summary, aes(`stay_day + stay_end`, duration), label) +  # 
  geom_bar(aes(fill = duration), stat = 'identity', fill="lightgreen") + 
  geom_text(aes(label = duration, y = duration), size = 3) + 
  labs(title = "Duration of stay") + xlab("Number of days stayed") +
  ylab("No. of guests") +
  scale_fill_discrete(name = "") + # for legend
  theme_bw() + theme(plot.title = element_text(hjust=0.5))

#which countries do guests come from?
##if want to change country codes to their full names
install.packages("countrycode")
library(countrycode) #Changes country codes into country names
list_of_countries <-countrycode(dataset$country, origin = 'genc3c', destination = 'country.name')  
list_of_countries

list_of_ptr_countries <- dataset %>%
  filter(country=='PRT') %>%
  summarise(count=n())
list_of_esp_countries <- dataset %>%
  filter(country=='ESP') %>%
  summarise(count=n())
list_of_otr_countries <- dataset %>%
  filter(country!='OTR') %>%
  summarise(count=n())
list_of_usa_countries <- dataset %>%
  filter(country=='USA') %>%
  summarise(count=n())
list_of_gbr_countries <- dataset %>%
  filter(country=='GBR') %>%
  summarise(count=n())

slices<-c(list_of_ptr_countries$count, 
          list_of_otr_countries$count,
          list_of_gbr_countries$count)
pie(slices, labels = c("Portugal", "Others", "UK"), main="Guests' Nationalities")

#which distribution channels are used more?
hotel <- read.csv("hotel_bookings.csv",stringsAsFactors = FALSE) %>% filter(hotel=="City Hotel") %>% filter(!is.na(children))
channel.data <- hotel %>% select(distribution_channel) %>% mutate(distribution_channel = as.factor(distribution_channel)) %>% group_by(distribution_channel) %>% summarise(count=n())
channel.data
ggplot(channel.data, aes(x="",y=count)) + geom_bar(stat = "identity", width = 1,aes(fill=distribution_channel)) + coord_polar("y", start = 0) + theme_void() + scale_fill_discrete(name="Channels") + labs(title = 'Pie Chart of Distribution Channels') + theme(plot.title = element_text(hjust = 0.5))

#which month has the highest number of cancellation/bookings
hotel <- read.csv("hotel_bookings.csv",stringsAsFactors = FALSE) %>% filter(hotel=="City Hotel") %>% filter(!is.na(children)) %>% filter(arrival_date_year==2016)
hotel.clean <- hotel %>% select(yr=arrival_date_year,mth=arrival_date_month,Status=reservation_status) %>% arrange(yr) %>% group_by(Status,mth) %>% summarise(n=n()) 
hotel.clean <- hotel.clean %>% mutate(mth = as.integer(case_when(mth == "January" ~ "1",
                                                                 mth == "February" ~ "2",
                                                                 mth == "March" ~ "3",
                                                                 mth == "April" ~ "4",
                                                                 mth == "May" ~ "5",
                                                                 mth == "June" ~ "6",
                                                                 mth == "July" ~ "7",
                                                                 mth == "August" ~ "8",
                                                                 mth == "September" ~ "9",
                                                                 mth == "October" ~ "10",
                                                                 mth == "November" ~ "11",
                                                                 mth == "December" ~ "12"))) %>% arrange(mth) 

ggplot(hotel.clean,aes(x=as.factor(mth),y=n)) + geom_bar(position = "stack",stat = "identity",aes(fill=Status)) + labs(title = "Reservation Status of Bookings in 2016", x="Months in 2016",y="Count") + theme_bw() +theme(plot.title = element_text(hjust = 0.5))

#what party sizes do guests come in?
cityhotel <- read.csv("hotel_bookings.csv") %>% filter(hotel=="City Hotel") %>% select(adults, children, babies, country) %>% filter(!is.na(children))

new <- cityhotel %>% mutate(total = adults + children + babies) %>% arrange(total) %>% filter(total!=0)

new <- new %>% mutate(total = case_when(total == "1" ~ "1",
                                        total == "2" ~ "2",
                                        total == "3" ~ "3",
                                        total == "4" ~ "4",
                                        TRUE ~ "5 or more")) %>% mutate(total=as.factor(total)) %>% group_by(total) %>% mutate(Adults=sum(adults), Children=sum(children), Babies=sum(babies)) %>% select(Adults,Children,Babies,total) 

new <- new %>% unique()
new$total<-as.character(new$total)
clean <- new %>% group_by(total) %>% gather(,-total,key="ppl") %>% arrange(total)
ggplot(clean, aes(x=total,y=value)) + geom_bar(stat = "identity", aes(fill = as.factor(ppl))) + labs(title = "Age Demographic of Party Sizes", x="Party Size",y="Count") + scale_fill_discrete(name="Age Group") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))  






# Price Forecasting
hotel_data <- read.csv("hotel_bookings.csv")
names(hotel_data)
glimpse(hotel_data)

# Convert some of the columns from integer data type to factors
hotel_data$is_canceled <- as.factor(hotel_data$is_canceled)
hotel_data$arrival_date_year <- as.factor(hotel_data$arrival_date_year)
hotel_data$arrival_date_month <- as.factor(hotel_data$arrival_date_month)
hotel_data$arrival_date_week_number <- as.factor(hotel_data$arrival_date_week_number)
hotel_data$arrival_date_day_of_month <- as.factor(hotel_data$arrival_date_day_of_month)

# Drop DATE from model. 
hotel_data <- select(hotel_data, -reservation_status_date)

# We will only use city hotel
city_hotel <- filter(hotel_data, hotel_data$hotel == "City Hotel")

# Removing rows with null values and one observation with ADR of 5400 (it is an outlier
# which affects the visualization of data)
dim(city_hotel)
sum(is.na(city_hotel))
city_hotel <- na.omit(city_hotel)
dim(city_hotel)
sum(is.na(city_hotel))
city_hotel <- subset(city_hotel, adr != 5400)
max(city_hotel$adr)    # maximum adr is now 510 which is a huge drop from 5400

#DEMOGRAPHICS OF VISITORS

#We will segregate the booker's nationalities into Local and Foreigners
city_hotel$citizenship <- as.character(city_hotel$country)
city_hotel$citizenship[city_hotel$citizenship != "PRT"] <- "others"
city_hotel$citizenship <- as.factor(city_hotel$citizenship)
attach(city_hotel)
# Citizenship
lm.Nat <- lm(adr~citizenship, data = city_hotel)
summary(lm.Nat)
boxplot(adr~citizenship, main = "Relationship between ADR and Country",
        xlab = "Country", ylab = "ADR", col = "red")
# Foreigner increases the ADR of the hotel rooms. 

# Find out about the interaction effect among the adults, children and babies
lm.adr2 <- lm(adr~adults*children+babies, data = city_hotel)   # Adjusted R-squared:  0.2137 
summary(lm.adr2)

lm.adr3 <- lm(adr~adults+children*babies, data = city_hotel)   # Adjusted R-squared:  0.2057 
summary(lm.adr3)

lm.adr4 <- lm(adr~children+adults*babies, data = city_hotel)   # Adjusted R-squared:  0.2057
summary(lm.adr4)

lm.adr5 <- lm(adr~children*adults+adults*babies, data = city_hotel)    # Adjusted R-squared:  0.2137 
summary(lm.adr5)

lm.adr6 <- lm(adr~adults*children, data = city_hotel)    # Adjusted R-squared:  0.2137 
summary(lm.adr6)
boxplot(adr~adults, main = "Relationship between ADR and Number of Adults",
        xlab = "Number of Adults", ylab = "ADR", col = "blue")
boxplot(adr~children, main = "Relationship between ADR and Number of Children",
        xlab = "Number of Children", ylab = "ADR", col = "yellow")
boxplot(adr~babies, main = "Relationship between ADR and Number of Babies",
        xlab = "Number of Babies", ylab = "ADR", col = "green")


# Customer-Type
lm.custType <- lm(adr~customer_type, data = city_hotel)
summary(lm.custType)

# Overall multilinear regression model for visitors' demographics
lm.visitor <- lm(adr~citizenship+adults*children+customer_type, data = city_hotel)
summary(lm.visitor)

# Types of nights they stayed
summary(lm(adr~stays_in_week_nights, data = city_hotel))           # Adjusted R-squared:  0.004156 
summary(lm(adr~stays_in_weekend_nights, data = city_hotel))        # Adjusted R-squared:  0.001796 
summary(lm(adr~stays_in_week_nights*stays_in_weekend_nights, data = city_hotel)) # Adjusted R-squared:  0.008875 
summary(lm(adr~stays_in_week_nights+stays_in_weekend_nights, data = city_hotel)) # Adjusted R-squared:  0.004988

# Types of services used 
summary(lm(adr~meal, data = city_hotel))                         # Adjusted R-squared:  0.01514 
summary(lm(adr~required_car_parking_spaces, data = city_hotel))  # Adjusted R-squared:  0.004428 
summary(lm(adr~reserved_room_type, data = city_hotel))           # Adjusted R-squared:  0.2841 
summary(lm(adr~total_of_special_requests, data = city_hotel))    # Adjusted R-squared:  0.0381 
summary(lm(adr~meal+required_car_parking_spaces+reserved_room_type+total_of_special_requests,
           data = city_hotel))                                   # Adjusted R-squared:  0.3177 
boxplot(adr~meal, main = "Relationship between ADR and Type of Meal Package booked",
        xlab = "Type of Meal Package booked", ylab = "ADR", col = "red")
boxplot(adr~meal, main = "Relationship between ADR and Number of Required Car Parking Spaces",
        xlab = "Number of Required Car Parking Spaces", ylab = "ADR", col = "blue")
boxplot(adr~reserved_room_type, main = "Relationship between ADR and Reserved Room Type",
        xlab = "Reserved Room Type", ylab = "ADR", col = "orange")
boxplot(adr~total_of_special_requests, main = "Relationship between ADR and Total of Special Requests",
        xlab = "Total of Special Requests", ylab = "ADR", col = "green")

# Find out which months can drive the sales via linear regression
lm.adr6 <- lm(adr~arrival_date_month, data = city_hotel)
summary(lm.adr6)





# Predict Cancellations
library(ISLR)
hotel <- read.csv("hotel_bookings.csv")
hotel <- hotel[-1]
options(max.print=9999999)
View(hotel)
attach(hotel)
which(! complete.cases(hotel))

boxplot(lead_time ~ is_canceled, data=hotel, col=c("blue","red"), xlab="is_canceled", y_lab="lead_time")

boxplot(lead_time ~ is_canceled*country, data=hotel, col=c("blue","red","yellow","green"), names=c("Not canceled; others", "Canceled; others", "Not canceled; portugal", "Canceled; portugal"), y_lab="lead_time")

boxplot(lead_time ~ is_canceled*distribution_channel, data=hotel, col=c("blue","red","yellow","green"), names=c("Not canceled; others", "Canceled; others", "Not canceled; TA/TO", "Canceled; TA/TO"), y_lab="lead_time")

g1 <- ggplot(data=hotel, aes(y=lead_time, x=previous_cancellations))
g1+geom_point(aes(color=is_canceled), size=1)

g1+geom_point(aes(color=is_canceled),size=1)+facet_wrap(~distribution_channel)+labs(title="Plot of lead time and number of previous cancellations as a function of booking status and deposit type", subtitle= "Deposit type")

g1 <- ggplot(data=hotel, aes(y=lead_time, x=previous_cancellations))
g1+geom_point(aes(color=is_canceled), size=1)

g1+geom_point(aes(color=is_canceled),size=1)+facet_wrap(~customer_type)+labs(title="Plot of lead time and number of previous cancellations as a function of booking status and customer type", subtitle= "Customer type")

library(knitr)
library(tidyverse)
library(ggplot2)
library(mice)
library(lattice)
library(reshape2)
library(DataExplorer)
plot_correlation(hotel)

set.seed(1)

# Shuffling the dataset
n <- nrow(hotel)
dfs <- hotel[sample(n),]

# Split the data in train and test
train_indices <- 1:round(0.7 * n)
train <- dfs[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
test <- dfs[test_indices, ]

library(rpart)
tree <- rpart(is_canceled~., train, method = "class", minsplit=2, minbucket=1)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(tree)

pred_test <- predict(tree, test, type = "class")

conf <- table(test$is_canceled, pred_test)

acc <- sum(diag(conf))/sum(conf)
acc*100





#  Predict Room Type
#run the multinomial regression
library(nnet)
multinom.reg <- multinom(reserved_room_type~., data=hotel)
summary(multinom.reg)
#checking p-values of respective variables
z <- summary(multinom.reg)$coefficients/summary(multinom.reg)$standard.errors
p <- (1-pnorm(abs(z),0,1))*2
#deviance statistic
pvalue <- 1-pchisq(multinom.reg$deviance,79319-16-1) #since p-value=1>alpha, we do not reject the null hypothesis. Therefore it is a very good fitting model
pvalue
#tabulate model's coefficients and its significance
library(stargazer)
stargazer(multinom.reg, type="text")
#create training and test data
set.seed(100)
train <- sample(1:nrow(RoomType),nrow(RoomType)*0.5)
test <- -train
room.train <- RoomType[train,]
room.test <- RoomType[test,]
#confusion matrix
library(nnet)
multinom.fit <- multinom(reserved_room_type~., data=room.train)
summary(multinom.fit)
multinom.pred <- predict(multinom.fit,room.test,"probs")
predicted_room <- predict(multinom.fit,room.test)
confusion.matrix <- table(predicted_room,room.test$reserved_room_type)
confusion.matrix
sum(diag(confusion.matrix))/sum(confusion.matrix) #accuracy rate
#prediction
multinom.pred1 <- predict(multinom.reg, data.frame(total_stay=3, adults=2, children=0, babies=0, meal="BB", PRT=1, market=1, distribution=1, is_repeated_guest="No", deposit_type="No Deposit", adr=100, required_car_parking_spaces=1, total_of_special_requests=0), "probs")
which.max(multinom.pred1)
multinom.pred2 <- predict(multinom.reg, data.frame(total_stay=3, adults=2, children=0, babies=1, meal="BB", PRT=1, market=1, distribution=1, is_repeated_guest="Yes", deposit_type="Non Refund", adr=150, required_car_parking_spaces=0, total_of_special_requests=0), "probs")
which.max(multinom.pred2)
multinom.pred3 <- predict(multinom.reg, data.frame(total_stay=3, adults=2, children=2, babies=0, meal="BB", PRT=1, market=1, distribution=1, is_repeated_guest="No", deposit_type="Refundable", adr=200, required_car_parking_spaces=1, total_of_special_requests=0), "probs")
which.max(multinom.pred3)
multinom.pred4 <- predict(multinom.reg, data.frame(total_stay=3, adults=4, children=0, babies=0, meal="BB", PRT=1, market=1, distribution=1, is_repeated_guest="No", deposit_type="No Deposit", adr=200, required_car_parking_spaces=1, total_of_special_requests=0), "probs")
which.max(multinom.pred4)
multinom.pred5 <- predict(multinom.reg, data.frame(total_stay=3, adults=4, children=0, babies=0, meal="FB", PRT=1, market=1, distribution=1, is_repeated_guest="Yes", deposit_type="No Deposit", adr=200, required_car_parking_spaces=1, total_of_special_requests=0), "probs")
which.max(multinom.pred5)


