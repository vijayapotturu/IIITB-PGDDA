# install and Load essential libraries
install.packages("car") # VIF For the calculation of VIF you need to install the package "car", 
install.packages("MASS") 
library(car)
library(MASS)
library(tidyverse)
library(plyr)
library(tools)

#### Clear environment variables.
rm(list=ls())

CarPrices <- read.csv("CarPrice_Assignment.csv")
CarPrices_original <- CarPrices
## viewing the imported data 
View(CarPrices)
#checking the structure of the dataset
str(CarPrices)

######################################################################################
# DATA CLEANSING                                                         
######################################################################################
#Check for duplicate loan ID or member ID
anyDuplicated(CarPrices$car_ID) # There are no duplicates

#Check for NA values in primary columns
sum(is.na(CarPrices)) # there are no NA Values in the Whole dataset

# checking for NA values
sapply(CarPrices, function(x) sum(is.na(x)))

# checking for blanks Missing values
sapply(CarPrices, function(x) length(which(x == ""))) # checking for blank "" values
# checking for unique values
sapply(CarPrices, function(x) length(unique(x)))

## correcting carName (company name)
str(CarPrices)
# seperating CarName into CarName, Types, Subtype
CarPrices$CarName <- as.character(CarPrices$CarName)
CarPrices$CarName <- toTitleCase(CarPrices$CarName)
CarPrices$Company<- sub(" .*$","",CarPrices$CarName)
#CarPrices <- separate(CarPrices, CarName,into = c("Company", "Model"),sep = " ")
#CarPrices <- separate(CarPrices, CarName, into=c("Company","ModelName"),sep = " ",extra = "merge", fill = "right")
View(CarPrices)
#Checking if any  NA values are coerced in the new variable
anyNA(CarPrices$Company)
#No NA values are coerced

#Checking for spelling mistakes
table(CarPrices$Company)
unique(CarPrices$Company)
#from the above code it can be seen that there are some spelling mistakes like "porsche" and "porcshce"

#levels(as.factor(CarPrices$company))
CarPrices$Company <- mapvalues(CarPrices$Company, from = factor(c("Maxda", "Porcshce", "Vokswagen", 
                                                         "Vw", "Toyouta")), to = c("Mazda", 
                                                                                   "Porsche", "Volkswagen", "Volkswagen","Toyota"))


CarPrices$Company <- as.factor(CarPrices$Company)
table(CarPrices$Company)
str(CarPrices)

# Converting Symboling to less levels.
CarPrices$symboling <- as.factor(CarPrices$symboling)
#CarPrices$symboling <- CarPrices_original$symboling
table(CarPrices$symboling)
#levels(CarPrices$symboling)[1:3] <- "pretty safe"
#levels(CarPrices$symboling)[2:3] <- "safe"
#levels(CarPrices$symboling)[3:6] <- "risky"
#View(CarPrices)

#Rounding some numeric columns to 2 digits
CarPrices$stroke<-round(CarPrices$stroke,2)

######### Verifying outliers:
#CarPrices$wheelbase <- CarPrices_original$wheelbase

quantile(CarPrices$wheelbase,seq(0,1,0.01)) # >97% there is a slight increase so capping >99%
boxplot(CarPrices$wheelbase) 
CarPrices$wheelbase[which(CarPrices$wheelbase>114.056)]<-114.056
boxplot(CarPrices$wheelbase) 

quantile(CarPrices$carlength,seq(0,1,0.01))
boxplot(CarPrices$carlength)  # outliers are between < 3% 
CarPrices$carlength[which(CarPrices$carlength<155.900)]<-155.900
# >99% not hanlded based on box plot
#CarPrices$carlength[which(CarPrices$carlength>202.480)]<-202.480
boxplot(CarPrices$carlength) 

quantile(CarPrices$carwidth,seq(0,1,0.01))
boxplot(CarPrices$carwidth)  #  outliers at >96%
CarPrices$carwidth[which(CarPrices$carwidth>70.852)]<-70.852
boxplot(CarPrices$carwidth) 

quantile(CarPrices$curbweight,seq(0,1,0.01))
boxplot(CarPrices$curbweight)  # No outliers

#CarPrices$enginesize <- CarPrices_original$enginesize
quantile(CarPrices$enginesize,seq(0,1,0.01))
boxplot(CarPrices$enginesize)  # there are outliers in enginesize > 94% but we cann't replace value here just handling at 96 %
#CarPrices$enginesize[which(CarPrices$enginesize>201.20)]<-201.20

#Check count of enginesize >3rd quartile.
quartile3 <- quantile(CarPrices$enginesize, probs = 0.75, na.rm = TRUE)
quartile3
filtered <- filter(CarPrices, enginesize > quartile3)
nrow(filtered)
# 51 cars have enginesize >3rd quartile 
cap95pct <- quantile(CarPrices$enginesize, probs = 0.95, na.rm = TRUE)
cap95pct
H <- 1.5 * IQR(CarPrices$enginesize, na.rm = TRUE)
H
CarPrices$enginesize[CarPrices$enginesize > (quartile3 + H)] <- cap95pct
boxplot(CarPrices$enginesize) 
quantile(CarPrices$boreratio,seq(0,1,0.01))
boxplot(CarPrices$boreratio)  # No outliers

CarPrices$stroke <- CarPrices_original$stroke
quantile(CarPrices$stroke,seq(0,1,0.01))
boxplot(CarPrices$stroke)  # # there are outliers in in lower qurtile and upper

CarPrices$stroke[which(CarPrices$stroke<2.7056)]<-2.7056
CarPrices$stroke[which(CarPrices$stroke>3.8248)]<-3.8248
boxplot(CarPrices$stroke) 
#CarPrices$compressionratio <- CarPrices_original$compressionratio
quantile(CarPrices$compressionratio,seq(0,1,0.01))
boxplot(CarPrices$compressionratio)  # there are outliers >89%
CarPrices$compressionratio[which(CarPrices$compressionratio<7.5000)]<-7.5000
CarPrices$compressionratio[which(CarPrices$compressionratio>10.0000)]<-10.0000
boxplot(CarPrices$compressionratio)

quantile(CarPrices$horsepower,seq(0,1,0.01))
boxplot(CarPrices$horsepower)  # there are outliers >99%
CarPrices$horsepower[which(CarPrices$horsepower>162.00)]<-162.00
boxplot(CarPrices$horsepower)

quantile(CarPrices$peakrpm,seq(0,1,0.01))
boxplot(CarPrices$peakrpm)  # 99%
CarPrices$peakrpm[which(CarPrices$peakrpm>6000)]<-6000
boxplot(CarPrices$peakrpm)

quantile(CarPrices$citympg,seq(0,1,0.01))
boxplot(CarPrices$citympg)  # >99% has outliers
CarPrices$citympg[which(CarPrices$citympg>44.72)]<-44.72
boxplot(CarPrices$citympg) 

quantile(CarPrices$highwaympg,seq(0,1,0.01))
boxplot(CarPrices$highwaympg)  # >99% has outliers
CarPrices$highwaympg[which(CarPrices$highwaympg>46.92)]<-46.92
boxplot(CarPrices$highwaympg)

quantile(CarPrices$price,seq(0,1,0.01))
boxplot(CarPrices$price)  # there are outliers but we no need to handle on this varaible.


#Data understanding, preparation and EDA :DUMMY VARIABLE CREATION.

# 1.converting two level data to c(0,1) and to numeric data type

#1.1 fueltype
levels(CarPrices$fueltype) <- c(0,1) # diesel (0), gas(1)
CarPrices$fueltype <- as.numeric(levels(CarPrices$fueltype))[CarPrices$fueltype]
#1.2 aspiration
levels(CarPrices$aspiration) <- c(0,1) # std (0), turbo(1)
CarPrices$aspiration <- as.numeric(levels(CarPrices$aspiration))[CarPrices$aspiration]
#1.3 doornumber
levels(CarPrices$doornumber) <- c(0,1) # four (0), two (1)
CarPrices$doornumber <- as.numeric(levels(CarPrices$doornumber))[CarPrices$doornumber]
#1.4 enginelocation
levels(CarPrices$enginelocation) <- c(0,1) # front (0), rear(1)
CarPrices$enginelocation <- as.numeric(levels(CarPrices$enginelocation))[CarPrices$enginelocation]

## Derived metrices
#1. Overall mpg
CarPrices$Ompg <- round(mean(CarPrices$citympg + CarPrices$highwaympg),2)

#2. Stroke2Bore Ratio
CarPrices$sbr <- round(CarPrices$stroke/CarPrices$boreratio,2)

#3. Overall mpg to Horsepower ratio
CarPrices$Ohp <- round(CarPrices$Ompg/CarPrices$horsepower, 2)

#4. Overall mpg to curbweight ratio (FE)
CarPrices$FE <- round(CarPrices$Ompg/CarPrices$curbweight, 4)

#5. Car height to carwidth ration

CarPrices$carHWratio <- round(CarPrices$carheight / CarPrices$carwidth,2)
#6. Car height to carlength ration

CarPrices$carHLratio <- round(CarPrices$carheight / CarPrices$carlength,2)

#7. Car weight against length ratio

CarPrices$carWLratio <- round(CarPrices$curbweight / CarPrices$carlength,2)

#8. Overall mpg to peakrpm ratio

CarPrices$carOmpgrpmratio <- round(CarPrices$Ompg / CarPrices$peakrpm,2)

str(CarPrices)

View(CarPrices)


# 2.converting multilevel variables to dummy and then to numbers

#1.symboling

dummy_symboling <- data.frame(model.matrix( ~symboling, data = CarPrices))
View(dummy_symboling)
dummy_symboling <- dummy_symboling[,-1]
#str(CarPrices)
#2. carbody
dummy_cb <- data.frame(model.matrix( ~carbody, data = CarPrices))
View(dummy_cb)
dummy_cb <- dummy_cb[,-1]

#3.drivewheel

dummy_dwl <- data.frame(model.matrix( ~drivewheel, data = CarPrices))
View(dummy_dwl)
dummy_dwl <- dummy_dwl[,-1]

#4. enginetype
dummy_et <- data.frame(model.matrix( ~enginetype, data = CarPrices))
View(dummy_et)
dummy_et <- dummy_et[,-1]


#5. cylindernumber
dummy_cyn <- data.frame(model.matrix( ~cylindernumber, data = CarPrices))
View(dummy_cyn)
dummy_cyn <- dummy_cyn[,-1]

#6. fuelsystem
dummy_fsys <- data.frame(model.matrix( ~fuelsystem, data = CarPrices))
View(dummy_fsys)
dummy_fsys <- dummy_fsys[,-1]

str(CarPrices)

#7.Company
dummy_Comp <- data.frame(model.matrix( ~Company, data = CarPrices))
View(dummy_Comp)
dummy_Comp<- dummy_Comp[,-1]



## removing car models and Combining dummy variable in data set called carPrices1
CarPrices_1 <- cbind(CarPrices[,setdiff(names(CarPrices),
                                           c("carbody","enginetype",
                                             "cylindernumber","fuelsystem","drivewheel","Company","symboling"))], 
                    dummy_cb,dummy_et, dummy_cyn, dummy_fsys,dummy_dwl,dummy_Comp,dummy_symboling)
View(CarPrices_1)
str(CarPrices_1)

# checking for NA values
sapply(CarPrices_1, function(x) sum(is.na(x)))

# checking for blanks Missing values
sapply(CarPrices_1, function(x) length(which(x == ""))) # checking for blank "" values

#removing the car_ID  and CarName from the dataset which may not be helpful during model build
CarPrices_1 <- subset(CarPrices_1, select = -c(car_ID, CarName))


## Setting seed to achieve reproducibility
set.seed(100)


## seperating Training and test datasets
trainindices= sample(1:nrow(CarPrices_1), 0.7*nrow(CarPrices_1))
train = CarPrices_1[trainindices,]
test = CarPrices_1[-trainindices,]

str(CarPrices_1)
# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1) # R-squared:  0.9827,	Adjusted R-squared:  0.9681

# Now, lets see with stepAIC

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 

step <- stepAIC(model_1, direction="both")
# so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 

step
# Let's execute this model here,

model_2 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carlength + carheight + curbweight + enginesize + boreratio + 
                stroke + sbr + Ohp + FE + carHWratio + carHLratio + carWLratio + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + drivewheelrwd + 
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyRenault + CompanyToyota + symboling2, data = train)

## check summary of the model
summary(model_2) # R-squared:  0.9811,	Adjusted R-squared:  0.9737

# If the VIF is above 2 or 5 as the business goal says, we would remove 
# the variables if they are statistically insignificant
vif(model_2)

#model_2 has a R-squared:  0.9811,	Adjusted R-squared:  0.9737 with a lot of insignificant variables 
# removing the varaibles based Highest VIF

#so removing carlength with pvalue 4.97e-05 and very high vif 1154.785755 leads to model_3 .

model_3 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carheight + curbweight + enginesize + boreratio + 
                stroke + sbr + Ohp + FE + carHWratio + carHLratio + carWLratio + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + drivewheelrwd + 
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyRenault + CompanyToyota + symboling2, data = train)

## check summary of the model
summary(model_3) # R-squared:  0.9778,	Adjusted R-squared:  0.9694
## Let us check for VIF
vif(model_3)

#model_3 has a R-squared:  0.9778,	Adjusted R-squared:  0.9694 with a lot of insignificant variables
#removing variable sbr with high pvalue 0.005803 and vif 498.167816 leads to model_4 .

# removing sbr  and Building model_4
model_4 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carheight + curbweight + enginesize + boreratio + 
                stroke + Ohp + FE + carHWratio + carHLratio + carWLratio + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + drivewheelrwd + 
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyRenault + CompanyToyota + symboling2, data = train)
# summary of the model
summary(model_4) #R-squared:  0.9761,	Adjusted R-squared:  0.9674
## Let us check for VIF
vif(model_4)

#model_4 has a R-squared:  0.9761,	Adjusted R-squared:  0.9674 with a lot of insignificant variables
#removing variable  FE with high pvalue 0.170228 and vif 198.530445 leads to model_5 .

# removing  FE  and Building model_5

model_5 <-lm(formula = price ~ fueltype + aspiration + enginelocation + 
               carheight + curbweight + enginesize + boreratio + 
               stroke + Ohp +  carHWratio + carHLratio + carWLratio + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + cylindernumberthree + drivewheelrwd + 
               CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
               CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
               CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_5) #R-squared:  0.9757,	Adjusted R-squared:  0.9671
## Let us check for VIF
vif(model_5)
#model_5 has a R-squared:  0.9757,	Adjusted R-squared:  0.9671 with a lot of insignificant variables
#removing variable carWLratio with high pvalue 0.000289 and vif 190.952065 leads to model_6 .

# removing carWLratio  and Building model_6

model_6 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carheight + curbweight + enginesize + boreratio + 
                stroke + Ohp +  carHWratio + carHLratio +  
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + drivewheelrwd + 
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_6) # R-squared:  0.9724,	Adjusted R-squared:  0.963
## Let us check for VIF
vif(model_6)

#model_6 has a R-squared:  0.9724,	Adjusted R-squared:  0.963 with a lot of insignificant variables
#removing variable carbodysedan with high pvalue 0.062141 . and vif 21.373116 leads to model_7 .

# removing carbodysedan  and Building model_7

model_7 <-lm(formula = price ~ fueltype + aspiration + enginelocation + 
               carheight + curbweight + enginesize + boreratio + 
               stroke + Ohp +  carHWratio + carHLratio +  
               carbodyhardtop + carbodyhatchback +  carbodywagon + 
               enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + cylindernumberthree + drivewheelrwd + 
               CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
               CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
               CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_7) #R-squared:  0.9715,	Adjusted R-squared:  0.9622
## Let us check for VIF
vif(model_7)

#model_7 has R-squared:  0.9715,	Adjusted R-squared:  0.9622 with a lot of insignificant variables
#removing variable enginesize with high pvalue 0.002553 and vif 28.457576 leads to model_8 .

# removing enginesize  and Building model_8

model_8 <-lm(formula = price ~ fueltype + aspiration + enginelocation + 
               carheight + curbweight + boreratio + 
               stroke + Ohp +  carHWratio + carHLratio +  
               carbodyhardtop + carbodyhatchback +  carbodywagon + 
               enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + cylindernumberthree + drivewheelrwd + 
               CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
               CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
               CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_8) # R-squared:  0.9689,	Adjusted R-squared:  0.9592
## Let us check for VIF
vif(model_8)

#model_8 has a R-squared:  0.9689,	Adjusted R-squared:  0.9592 with a lot of insignificant variables
#removing variable cylindernumberfour with high pvalue 0.044073 and vif 17.636969 leads to model_9 .

# removing cylindernumberfour  and Building model_9

model_9 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carheight + curbweight + boreratio + 
                stroke + Ohp +  carHWratio + carHLratio +  
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetypeohcv + cylindernumberfive +  
                cylindernumbersix + cylindernumberthree + drivewheelrwd + 
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_9) #R-squared:  0.9677,	Adjusted R-squared:  0.958
## Let us check for VIF
vif(model_9)

#model_9 has a R-squared:  0.9677,	Adjusted R-squared:  0.958 with a lot of insignificant variables .
#removing variable carHWratio with high pvalue 8.77e-05 and vif 21.484329 leads to model_10 .

# removing carHWratio  and Building model_10

model_10 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                 carheight + curbweight + boreratio + 
                 stroke + Ohp +  carHLratio +  
                 carbodyhardtop + carbodyhatchback +  carbodywagon + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetypeohcv + cylindernumberfive +  
                 cylindernumbersix + cylindernumberthree + drivewheelrwd + 
                 CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                 CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                 CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_10) #R-squared:  0.9628,	Adjusted R-squared:  0.952
## Let us check for VIF
vif(model_10)

#model_10 has a R-squared:  0.9628,	Adjusted R-squared:  0.952 with a lot of insignificant variables .
#removing variable ohp with high pvalue 0.465819 and vif 13.026525 leads to model_11 .

# removing ohp  and Building model_11

model_11 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                 carheight + curbweight + boreratio + 
                 stroke +   carHLratio +  
                 carbodyhardtop + carbodyhatchback +  carbodywagon + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetypeohcv + cylindernumberfive +  
                 cylindernumbersix + cylindernumberthree + drivewheelrwd + 
                 CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                 CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                 CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_11) #R-squared:  0.9627,	Adjusted R-squared:  0.9522
## Let us check for VIF
vif(model_11)
#model_11 has a R-squared:  0.9627,	Adjusted R-squared:  0.9522 with a lot of insignificant variables .
#removing variable carheight with high pvalue 0.201574 and vif 6.391967 leads to model_12 .

# removing carheight  and Building model_12

model_12 <-lm(formula = price ~ fueltype + aspiration + enginelocation + 
                curbweight + boreratio + 
                stroke +   carHLratio +  
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetypeohcv + cylindernumberfive +  
                cylindernumbersix + cylindernumberthree + drivewheelrwd + 
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_12) #R-squared:  0.9621,	Adjusted R-squared:  0.9519
## Let us check for VIF
vif(model_12)
#model_12 has a R-squared:  0.9621,	Adjusted R-squared:  0.9519 with a lot of insignificant variables .
#removing variable cylindernumbersix with high pvalue 0.741352 and vif 5.405275 leads to model_13 .

# removing cylindernumbersix  and Building model_13
model_13 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                 curbweight + boreratio + 
                 stroke +   carHLratio +  
                 carbodyhardtop + carbodyhatchback +  carbodywagon + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetypeohcv + cylindernumberfive +  
                 cylindernumberthree + drivewheelrwd + 
                 CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                 CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                 CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_13) #R-squared:  0.9621,	Adjusted R-squared:  0.9523
## Let us check for VIF
vif(model_13)

#model_13 has a R-squared:  0.9621,	Adjusted R-squared:  0.9523 with a lot of insignificant variables .
#removing variable CompanyRenault with high pvalue 0.101633 and vif 4.201800 leads to model_14 .

# removing CompanyRenault  and Building model_14  
model_14 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                 curbweight + boreratio + 
                 stroke +     
                 carbodyhardtop + carbodyhatchback +  carbodywagon + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetypeohcv + cylindernumberfive +  
                 cylindernumberthree + drivewheelrwd + 
                 CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                 CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                 CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_14) # R-squared:  0.9611,	Adjusted R-squared:  0.9516
## Let us check for VIF
vif(model_14)

#model_14 has a R-squared:  0.9683,	Adjusted R-squared:  0.9606 with a lot of insignificant variables
#removing variable boreratio with high pvalue 0.117231 and vif 3.959736 leads to model_15 .

# removing boreratio  and Building model_15

model_15 <-  lm(formula = price ~ fueltype + aspiration + enginelocation + 
                  curbweight +
                  stroke +     
                  carbodyhardtop + carbodyhatchback +  carbodywagon + 
                  enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                  enginetypeohcv + cylindernumberfive +  
                  cylindernumberthree + drivewheelrwd + 
                  CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                  CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                  CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_15) # R-squared:  0.9603,	Adjusted R-squared:  0.951
## Let us check for VIF
vif(model_15)

#model_15 has a R-squared:  0.9603,	Adjusted R-squared:  0.951 with a lot of insignificant variables.
#removing variable enginetypeohcv with high pvalue 0.451350 and vif 2.443033 leads to model_16 .

# removing enginetypeohcv  and Building model_16
model_16<- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                curbweight +
                stroke +     
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree + drivewheelrwd + 
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_16) # R-squared:  0.9601,	Adjusted R-squared:  0.9512
## Let us check for VIF
vif(model_16)

#model_16 has a R-squared:  0.9601,	Adjusted R-squared:  0.9512 with a lot of insignificant variables
#removing variable drivewheelrwd with high pvalue 0.081903 and vif 3.252524 leads to model_17 .

# removing symboling2  and Building model_17

model_17<- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                curbweight +
                stroke +     
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree +  
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_17) # R-squared:  0.959,	Adjusted R-squared:  0.9503
## Let us check for VIF
vif(model_17)

#model_17 has a R-squared:  0.959,	Adjusted R-squared:  0.9503 with a lot of insignificant variables.
#removing variable carbodyhatchback with high pvalue 0.715148 and vif 1.585352 leads to model_18 .

# removing carbodyhatchback  and Building model_18
model_18<- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                curbweight +
                stroke +     
                carbodyhardtop +  carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree +  
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyRenault + CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_18) #R-squared:  0.959,	Adjusted R-squared:  0.9507
## Let us check for VIF
vif(model_18)
#model_18 has a R-squared:  0.959,	Adjusted R-squared:  0.9507 with a lot of insignificant variables 
#removing variable CompanyRenault with high pvalue 0.498372 and vif 1.221016 leads to model_19 .

# removing CompanyRenault  and Building model_19
model_19<- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                curbweight +
                stroke +     
                carbodyhardtop +  carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree +  
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_19) # R-squared:  0.9588,	Adjusted R-squared:  0.9509
## Let us check for VIF
vif(model_19)

#model_19 has a R-squared:  0.9588,	Adjusted R-squared:  0.9509 with a lot of insignificant variables
#removing variable carbodyhardtop with high pvalue 0.328455 and vif 1.512373 leads to model_20 .


# removing carbodyhardtop  and Building model_20
model_20<- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                curbweight +
                stroke +     
                carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree +  
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyToyota + symboling2, data = train)
# Let us look at the summary of the model
summary(model_20) #R-squared:  0.9585,	Adjusted R-squared:  0.9509 
## Let us check for VIF
vif(model_20)

#model_20 has a R-squared:  0.9579,	Adjusted R-squared:  0.9502 with a lot of insignificant variables .
#removing variable symboling2 with high pvalue 0.242728  and vif 1.355153 leads to model_21 .

# removing symboling2  and Building model_21
model_21<- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                curbweight +
                stroke +     
                carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree +  
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMazda + CompanyMitsubishi + CompanyPlymouth + 
                CompanyToyota , data = train)
# Let us look at the summary of the model
summary(model_21) #  R-squared:  0.958,	Adjusted R-squared:  0.9507
## Let us check for VIF
vif(model_21)

#model_21 has a  R-squared:  0.958,	Adjusted R-squared:  0.9507 with a lot of insignificant variables.
#removing variable CompanyMazda  with high pvalue 0.178796 and vif 1.207178 leads to model_22 .

# removing CompanyMazda   and Building model_22

model_22<- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                curbweight +
                stroke +     
                carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree +  
                CompanyAudi + CompanyBmw + CompanyBuick + CompanyDodge + 
                CompanyJaguar + CompanyMitsubishi + CompanyPlymouth + 
                CompanyToyota , data = train)
# Let us look at the summary of the model
summary(model_22) #R-squared:  0.9574,	Adjusted R-squared:  0.9504
## Let us check for VIF
vif(model_22)

#model_22 has a  R-squared:  0.9574,	Adjusted R-squared:  0.9504 with a lot of insignificant variables with Pvalue.
#removing variable CompanyDodge with high pvalue 0.156465  and vif 1.139523 leads to model_23 .

# removing CompanyDodge  and Building model_23 
model_23<- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                curbweight +
                stroke +     
                carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree +  
                CompanyAudi + CompanyBmw + CompanyBuick +  
                CompanyJaguar + CompanyMitsubishi + CompanyPlymouth + 
                CompanyToyota , data = train)
# Let us look at the summary of the model
summary(model_23) #R-squared:  0.9567,	Adjusted R-squared:   0.95
## Let us check for VIF
vif(model_23)

#model_23 has a  R-squared:  0.9567,	Adjusted R-squared:   0.95 with a lot of insignificant variables.
#removing variable aspiration with high pvalue 0.176758 and vif 1.717450 leads to model_24 .

# removing aspiration  and Building model_24 


model_24<- lm(formula = price ~ fueltype + enginelocation + 
                curbweight +
                stroke +     
                carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree +  
                CompanyAudi + CompanyBmw + CompanyBuick +  
                CompanyJaguar + CompanyMitsubishi + CompanyPlymouth + 
                CompanyToyota , data = train)
# Let us look at the summary of the model
summary(model_24) # R-squared:  0.956,	Adjusted R-squared:  0.9496
## Let us check for VIF
vif(model_24)

#model_24 has a  R-squared:  0.956,	Adjusted R-squared:  0.9496 with insignificant variables
#removing variable fueltype with high pvalue 0.263793  and vif 1.491679 leads to model_25 .

# removing fueltype  and Building model_25 

model_25<- lm(formula = price ~ enginelocation + 
                curbweight +
                stroke +     
                carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree +  
                CompanyAudi + CompanyBmw + CompanyBuick +  
                CompanyJaguar + CompanyMitsubishi + CompanyPlymouth + 
                CompanyToyota , data = train)
# Let us look at the summary of the model
summary(model_25) # R-squared:  0.9556,	Adjusted R-squared:  0.9495 
## Let us check for VIF
vif(model_25)

#model_25 has a  R-squared:  0.9556,	Adjusted R-squared:  0.9495 with insignificant variables
#removing variable CompanyPlymouth with high pvalue 0.256783  and vif 1.066458 leads to model_26 .

# removing CompanyPlymouth  and Building model_26 

model_26<- lm(formula = price ~ enginelocation + 
                curbweight +
                stroke +     
                carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree +  
                CompanyAudi + CompanyBmw + CompanyBuick +  
                CompanyJaguar + CompanyMitsubishi +  
                CompanyToyota , data = train)
# Let us look at the summary of the model
summary(model_26) # R-squared:  0.9551,	Adjusted R-squared:  0.9494 
## Let us check for VIF
vif(model_26)

#model_26 has a  R-squared:  0.9551,	Adjusted R-squared:  0.9494 with a one variable with low significance
#removing variable CompanyMitsubishi with high pvalue 0.038041  and vif 1.080357 leads to model_27 .

# removing CompanyMitsubishi  and Building model_27 

model_27<- lm(formula = price ~ enginelocation + 
                curbweight +
                stroke +     
                carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                cylindernumberthree +  
                CompanyAudi + CompanyBmw + CompanyBuick +  
                CompanyJaguar +   
                CompanyToyota , data = train)
# Let us look at the summary of the model
summary(model_27) # R-squared:  0.9536,	Adjusted R-squared:  0.9481
## Let us check for VIF
vif(model_27)

#model_27 has a  R-squared:  0.9461,	Adjusted R-squared:  0.9406 with a one variable with low significance
#removing variable cylindernumberthree with high pvalue 0.007256  and vif 1.383413 leads to model_28 .

# removing cylindernumberthree  and Building model_28 

model_28<- lm(formula = price ~ enginelocation + 
                curbweight +
                stroke +     
                carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                CompanyAudi + CompanyBmw + CompanyBuick +  
                CompanyJaguar +   
                CompanyToyota , data = train)
# Let us look at the summary of the model
summary(model_28) # R-squared:  0.9508,	Adjusted R-squared:  0.9455 
## Let us check for VIF
vif(model_28)
#model_28 has a  R-squared:  0.9508,	Adjusted R-squared:  0.9455  with few varaibles with * significance.
#removing variable stroke with high pvalue 0.004941  and vif 1.872952 leads to model_29 .

# removing stroke  and Building model_29 

model_29<- lm(formula = price ~ enginelocation + 
                curbweight +
                carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                cylindernumberfive +  
                CompanyAudi + CompanyBmw + CompanyBuick +  
                CompanyJaguar +   
                CompanyToyota , data = train)
# Let us look at the summary of the model
summary(model_29) # R-squared:  0.9477,	Adjusted R-squared:  0.9424
## Let us check for VIF
vif(model_29)

#model_29 has a  R-squared:  0.9477,	Adjusted R-squared:  0.9424 

## As now our model has only significant parameters in our model  so 
## we can use this model for our prediction. 

# predicting the results in test dataset
Predict_1 <- predict(model_29,test[,-18])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted price.
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

plot(model_29)


## As now our model has only significant parameters in our model  so 
## we can use this model for our prediction. 

######Identified the below driver varaibles which are most significant with Adjusted R-squared:  0.9424 from model 29 and test rsquared is  0.8486221 of ~10 %.
##so We can consider model 29 as the final model below driver varaibles.

#1.enginelocation ,
#2.curbweight
#3.carbodywagon
#4.enginetypedohcv
#5.enginetypel
#6.enginetypeohc
#7.enginetypeohcf
#8.cylindernumberfive
#9.CompanyAudi
#10.CompanyBmw
#11.CompanyBuick
#12.CompanyJaguar
#13.CompanyToyota


