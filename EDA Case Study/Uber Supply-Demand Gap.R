# creating & setting working directory

# setwd(file.path("/Users/raj/Documents/Vijaya/Upgrad/Course 2/", "Uber Case Study"))
# getwd()

# loading relevant packages
library(stringr)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)

# Loading Data 
uber_req <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)

# 1.Analysing&Understanding the Data

# looking at data
str(uber_req)
head(uber_req)
nrow(uber_req)
summary(uber_req)

# There are 6745 records with 6 variables with 5 days of data from Uber between July 11 2016 to July 16 2016
# Request.id is unique column in the dataset.
# Pickup point will be either City/Airport .there are no uppercase and lower case issues.
# There are  2650 are NA's in Driver id .
# Status is either Cancelled/No Cars Available.there are no uppercase and lower case issues.
# Request.timestamp is having differant data time formats.
# Drop.timestamp is having differant data time formats.NA Records exists in the dataset.

# 1.1.checking for blanks
sapply(uber_req, function(x) length(which(x == ""))) # checking for blank "" values

# 1.2. look for duplicate values in the Request Id's.
#duplicated(uber_req$Request.id)
sum(duplicated("uber_req$Request id")) # no Duplciate Request id's

# 1.3. Missing values
sum(is.na(uber_req)) #6564 NA
summary(uber_req)

sum(is.na(uber_req$Request.timestamp))
# counting NA's in Driver id.
sum(is.na(uber_req$Driver.id))  # 2650 Na's in Driver Id.

# counting NA's in Drop time.
sum(is.na(uber_req$Drop.timestamp)) # 3914 NA's in Drop time so these records are for status "Cancelled" and "No Cars Available".

# finding how many rows by each status to validate 3914 NA rows.

summary(factor(uber_req$Status)) # 3 levels 

# Cancelled 1264 + No cars Avilable 2650 is 3914 NA's in Drop time so this is perfectly fine in the dro time stamp.

# 1.4 We need to find any repetitions in the dataset for each column. It seems same drivers have been doing multiple trips.

apply(uber_req,2,function(x) sum(duplicated(x)))

# 1.5 each request is unique and this data is of July 2016

apply(uber_req,2,function(x) length(unique(x)))

# 2.Data Cleaning and Preparation

# Converting Categorical variables as factors
# Request ID,pickup point,status and Driver.ID Needs to change int to "factor" data type 
str(uber_req)
uber_req$Pickup.point <- as.factor(uber_req$Pickup.point)
# uber_req$Request.id <- as.factor(uber_req$Request.id)
uber_req$Status <- as.factor(uber_req$Status)
uber_req$Driver.id <- as.factor(uber_req$Driver.id)

str(uber_req)

# Data Cleaning - Date formats

# Standardizing date fomat separator form "/" to "-" and removing seconds as this granular level analysis is not required
uber_req$Request.timestamp = str_replace_all(uber_req$Request.timestamp,"\\/","-")
uber_req$Drop.timestamp = str_replace_all(uber_req$Drop.timestamp,"\\/","-")

# converting into standard R date-time format 

uber_req$Request.timestamp<-parse_date_time(uber_req$Request.timestamp,orders=c("%d-%m-%Y %H:%M","%d/%m/%Y %H:%M:%S"), exact = FALSE)
uber_req$Drop.timestamp<-parse_date_time(uber_req$Drop.timestamp,orders=c("%d-%m-%Y %H:%M","%d/%m/%Y %H:%M:%S"), exact = FALSE)

str(uber_req) # Verifying the date and time column formats


# Derive new variables on Date and Time 

# Extracting hour of day from Request time and Drop time

# creating separate columns from the date & time: day, month, year, hours and minutes

uber_req$Request.timestamp1 <- uber_req$Request.timestamp
uber_req$Drop.timestamp1 <- uber_req$Drop.timestamp
uber_req <- separate(data = uber_req, col = "Request.timestamp1", into = c("req.date","req.time"), sep = " ")
uber_req <- separate(data = uber_req, col = "Drop.timestamp1", into = c("drop.date","drop.time"), sep = " ")

uber_req$request_day <- format(uber_req$Request.timestamp, "%d")
# uber_req1$request_month = format(uber_req1$Request.timestamp, "%m")
# uber_req1$request_year = format(uber_req1$Request.timestamp, "%Y")
uber_req$Req.hrs <- format(uber_req$Request.timestamp, "%H")
# uber_req1$Req.minutes <- format(uber_req1$Request.timestamp, "%M")
uber_req$drop_day <- format(uber_req$Drop.timestamp, "%d")
uber_req$drop.hrs <- format(uber_req$Drop.timestamp, "%H")

#Converting Request_day,Req.hrs,drop_day,drop.hrs as factor
uber_req$Req.hrs <- as.numeric(uber_req$Req.hrs)
str(uber_req)

#for time slots variable,Assumiptions are
# 00 to 05 as Early Morning,
# great than or equal to 05 to 9 as Morning,
# great than or equal to 10 to 12 (12 PM) as before noon,
# great than or equal to 12(12 PM) to 17 (5PM) as afternoon,
# great than or equal to 17(5 PM) to 21(9PM) as Evening,
# great than or equal to 21(9 PM) as Late Evening

uber_req$Time_Slot[uber_req$Req.hrs >= 00 & uber_req$Req.hrs < 5] <- c("00-4AM")
uber_req$Time_Slot[uber_req$Req.hrs >= 05 & uber_req$Req.hrs <= 09] <- c("05-9AM")
uber_req$Time_Slot[uber_req$Req.hrs >= 10 & uber_req$Req.hrs < 14] <- c("10AM-2PM")
uber_req$Time_Slot[uber_req$Req.hrs >= 14 & uber_req$Req.hrs < 17] <- c("14-17PM")
uber_req$Time_Slot[uber_req$Req.hrs >= 17 & uber_req$Req.hrs <= 21] <- c("17-21PM")
uber_req$Time_Slot[uber_req$Req.hrs > 21] <- c("21 - 23PM")

# Verifying the count hourly to time slot
summary(factor(uber_req$Time_Slot))
summary(factor(uber_req$Req.hrs))

uber_req$Time_Slot <- as.factor(uber_req$Time_Slot)
uber_req$Req.hrs <- as.factor(uber_req$Req.hrs)
uber_req$request_day <- as.factor(uber_req$request_day)
uber_req$drop_day <- as.factor(uber_req$drop_day)
uber_req$drop.hrs <- as.factor(uber_req$drop.hrs)
str(uber_req)

# Sorting Data based on Driver.id and Request Time

uber_req <- uber_req[order(uber_req$Driver.id, uber_req$Request.timestamp),]

# Task2 : Data Analysis

# Univariate and segmented analysis

# Plots 1 : This plot gives frequencies of requests by pick up points in each status this will help to where is Most problem in either Airport/City.Selected bar chart(stack) based on Bar height we can assess on high level where is most problem .
  
ggplot(data = uber_req, 
         mapping = aes(x = Status,fill = Pickup.point)) + 
  geom_bar(position = "Stack") +
  labs(title ="Frequency of Requests by Status,Airport/City", 
       x= "Booking requests by Status", y= "Count of Requests") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5))


# Obseravation : Cancelled + No Cars Available from the Airport is higher than City.Trips completed from the city is clearly states it is higher than Airport.Cars Availabilty at Airport seems be major problem.

# Plot 2 : The below plot will identify which are peak hours 
  
  ggplot(uber_req, 
         mapping = aes(x = uber_req$Req.hrs,fill = Pickup.point)) + 
    geom_bar(position = "stack") +
    labs(title ="Frequency of Requests by Cabs  at Airport/city", x= "Booking requests in a day (hrs)", y= "Count of Requests") + 
    theme(plot.title = element_text(hjust = 0.5))+
    geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5))

# Obseravation : Number requests from City to Aiport between 5 Am to 10 AM is high and Airport to City between 17 PM to 21 PM is high. 

  uber_Airport <- uber_req %>%
    filter(Pickup.point == "Airport")
  
  uber_Airport_drop <- uber_req %>%
    filter(Pickup.point == "Airport" & Status == 'Trip Completed')
  
  uber_City <- uber_req %>%
    filter(Pickup.point == "City")
  
  uber_City_drop <- uber_req %>%
    filter(Pickup.point == "City" & Status == 'Trip Completed')
  
# Plot 3: The below plot will identify which status is the problematic 
  
Requests_Airport1 <-  ggplot(uber_Airport, 
         mapping = aes(x = Req.hrs,fill = Status)) + 
    geom_bar(position = "stack") + 
    labs(title ="Frequency of Requests by Cabs  at Airport", x= "Booking requests in a day (hrs)", y= "Count of Requests") + 
    theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
    geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5),check_overlap = TRUE)
  
Requests_City1 <- ggplot(uber_City, 
         mapping = aes(x = Req.hrs,fill = Status)) + 
    geom_bar(position = "stack") + 
    labs(title ="Frequency of Requests by Cabs  at city", x= "Booking requests in a day (hrs)", y= "Count of Requests") + 
    theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
    geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5),check_overlap = TRUE)
  
grid.arrange(Requests_Airport1, Requests_City1, nrow = 2)

# Obseravation :  Cars unavailabilty seems to higher than cancellation at Aiport and in City Cancellation are high during the Peak hours.
    
# 1.Visually identify the most pressing problems for Uber. 
# Hint: Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; identify the most problematic types of requests (city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) using plots. 
#  Plot 1 To visualise frequency of requests & drops to analyse the Peak Hours (city to airport / airport to city)

# Plot 1 :Show the  frequency of number of requests& drops by status during each hour of the day by pick up point
ggplot(data = uber_req, 
       mapping = aes(x = uber_req$Req.hrs,fill = Status)) + 
  geom_histogram(position = "stack", stat = "count") + facet_wrap(req.date~Pickup.point) +
  labs(title ="Frequency of Daily Requests by Cab Status,Airport/City", x= "Booking requests in a day (hrs)", y= "Count of Requests") + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Observation :Cancellations and cab unavailability are consistent on all the days. Airport to city or city to Airport cab request were not catered due to either unavailability or cancellations of the cab booking.
# Also Cab unavailability is high for Airport pickup requests and Cancellations are high for City pickup requests  

# summary(uber_req$Status)

# Plot 2 :Will Show the  frequency of number of request by Cancelled or No cars Available during each hour of the day by pick up point

uber_can_nocars <- uber_req %>%
  filter(Status == "Cancelled" | Status == "No Cars Available") 

# summary(uber_can_nocars$Status)

# to Display Frequency of requests by Cancelled/"No cars Available" by Date wise in Airport/City
ggplot(uber_can_nocars, 
       mapping = aes(x = uber_can_nocars$Req.hrs, fill = Status)) + 
  geom_histogram(position = "dodge", stat = "count") + facet_wrap(req.date ~ Pickup.point)  +
  labs(title ="Frequency of Requests by Cabs Cancelled/No cars Available at Airport,Date Wise", x= "Booking requests in a day (hrs)", y= "Count of Requests") + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Observation : Cancellations are high on most of days from the city and Cars Availability Seems to be the issue at Airport during peak hours most of the days.

#Plot 3 :to Display Frequency of requests by Cancelled/"No cars Available" by time slot by Date wise

ggplot(data = uber_can_nocars, 
       mapping = aes(x = Time_Slot,fill = Status)) + 
  geom_histogram(position = "stack", stat = "count") + facet_wrap(~req.date) +
  labs(title ="Frequency of Requests by Cab Status by Date", x= "Booking requests by Time Slot", y= "Count of Requests") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5))

# Observation : Cancellations are high on most of days from the city and Cars Availability Seems to be the issue at Airport during peak hour time_slots.

# to Display Frequency of requests by Cancelled/"No cars Available",Airport/City
ggplot(data = uber_can_nocars, 
       mapping = aes(x = Time_Slot, fill = Status )) + 
  geom_histogram(position = "Stack", stat = "count") + facet_wrap(~Pickup.point) +
  labs(title ="Frequency of Requests by Cabs Cancelled/No cars Available,Airport/City", x= "Booking requests by Time Slot", y= "Count of Requests") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5))

# Observation : This Chart clearly shows at Airport there are no Cabs Available (between 5PM to 9 PM).More Cancellation from City Between 5 AM to 9 AM.

# 2.Find out the gap between supply and demand and show the same using plots.
# Find the time slots when the highest gap exists
# Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

# Gap Analysis by hourly wise

# Demand and supply

# Assumptions : Demand = number of cab requests done. Trip completed + Trip cancelled + Cabs unavailable
# Supply = Number of cab availability. Only trip completed is considered as supply. Trip cancelled / cab unavailability so did not consider as part of supply

uber_req$Status_Flag <- (uber_req$Status %in% 'Trip Completed')
summary(uber_req$Status_Flag)

uber_req$Status_Flag[uber_req$Status_Flag == TRUE] <- c("Trip Completed")
uber_req$Status_Flag[uber_req$Status_Flag == FALSE] <- c("Requests not Served")

# Demand Vs Supply by Date Wise

ggplot(uber_req,aes(x=Req.hrs,fill=Status_Flag)) + geom_bar(stat='count',position = "stack")+
  ggtitle("Daily Demand for Cabs by Hourly")+ facet_wrap(~req.date) +
  labs(x="Time in Hours", y="Number of Cabs Requested")+
  labs(fill="Demand Vs Supply") 

# Observation : Most of the days requests are not fulfilled during peack hours at both the pick up points.

# Demand Vs Supply by pick up point

ggplot(uber_req,aes(x=Req.hrs,fill=Status_Flag)) + geom_bar(stat='count',position = "stack")+
  ggtitle("Hourly Demand for Cabs")+ facet_wrap(~Pickup.point) +
  labs(x="Time in Hours", y="Number of Cabs Requested")+
  labs(fill="Demand Vs Supply")  +
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5))


# Observation : There is a huge gap between deamnd and Supply during peak hours.


# summary(factor(uber_req$drop.hrs))
# summary(factor(uber_req$Req.hrs))

# Gap Analysis by Time slot wise

# Demand Vs Supply by date by time slot

ggplot(uber_req,aes(x=Time_Slot,fill=Status_Flag)) + geom_bar(stat='count',position = "stack")+
  ggtitle("Daily Demand for Cabs by time slots")+ facet_wrap(~req.date) +
  labs(x="Time slots", y="Number of Cabs Requested")+
  labs(fill="Demand Vs Supply")+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5))

# Demand Vs Supply by pick up point by time slot

ggplot(uber_req,aes(x=Time_Slot,fill=Status_Flag)) + geom_bar(stat='count',position = "stack")+
  ggtitle("Hourly Demand & Supply for Cabs by Time slots")+ facet_wrap(~Pickup.point) +
  labs(x="Time slots", y="Number of Cabs Requested")+
  labs(fill="Demand Vs Supply") +
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5))


# Demand Vs Supply by pick up point by time slot

Requests_Airport <- ggplot(uber_Airport, aes(x = Time_Slot)) +
  geom_bar(stat='count',fill="darkblue") +
  labs(title ="Frequency of Requests at Airport", x= "Booking requests in a day (Time_slot)", y= "Count of Requests") + 
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y=(..count..),label=(..count..),colour = "white",fontface = "bold"),size=3,stat='count',position = position_stack(vjust = 0.5))
# summary(uber_Airport$Time_Slot)

Drops_Airport <- ggplot(uber_Airport_drop, aes(x = Time_Slot)) +
  geom_bar(stat='count',fill="Orange") +
  labs(title ="Frequency of drops at Airport", x= "Drops in a day (Time_slot)", y= "Count of Drops") + 
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5))

# summary(uber_Airport_drop$Time_Slot)

Requests_City <- ggplot(uber_City, aes(x = Time_Slot)) +
  geom_bar(stat='count',fill="darkblue") +
  labs(title ="Frequency of Requests at City", x= "Booking requests in a day (Time_slot)", y= "Count of Requests") + 
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  geom_text(aes(y=(..count..),label=(..count..),colour = "white", fontface = "bold"),size=3,stat='count',position = position_stack(vjust = 0.5))

# summary(uber_City$Time_Slot)

Drops_City <- ggplot(uber_City_drop, aes(x = Time_Slot)) +
  geom_bar(stat='count',fill="Orange") +
  labs(title ="Frequency of drops at City", x= "Drops in a day (Time_slot)", y= "Count of Drops") + 
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5))
# summary(uber_City_drop$Time_Slot)
grid.arrange(Requests_Airport, Drops_Airport,Requests_City, Drops_City, nrow = 2)
# Observation : There is a hige gap between deamnd and Supply during peak hours.There are two problemd identified from the above visualisations are why are more cancellations in the morning slot?Why there are no cabs at Airport between 5PM to 9 PM.

# calculating the Total Triptime 
uber_req$triptime <- as.numeric(uber_req$Drop.timestamp-uber_req$Request.timestamp)
Average_trip_time <- mean(!is.na(uber_req$triptime))*60
Average_trip_time 

#Average_trip_time <- uber_req %>%
#  filter(Status == 'Trip Completed') %>%
#  group_by(Pickup.point) %>%
#   summarize(avg_trip_time = mean(!is.na(uber_req$triptime))*60)

# to Understand total Jorney time from Airport/city or City/Airport

ggplot(data = uber_req, mapping = aes(y = triptime, x = Req.hrs, fill = Pickup.point)) + 
  geom_boxplot() + theme(title = element_text(size=9, face="bold"))+
  labs(title ="Total trip Duration pattern in a weekday ", 
          x= "Booking requests in a day (Hrs)", y = "total Trip time") 

# Observation: total Trip time is relatively high to Airport in the morning (5 am to 10 am) & high to city in evening(5pm to 21pm)"
# this will lead more cancellations to Airport by cab drivers so that they can avoid long waiting time at airport and have more trips with in City.



# Exporting clean data frame for operations in tableau
write.csv(uber_req, "Uber Request Final.csv", na = "",row.names = FALSE)
