################################################
#############HR Analytics Case Study ###########
################################################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:
# The level of attrition is high hence the company wants to investigate on
# the probable factors as it impacting and amounting to significant effort in
# the project deliverables, company image, training to new staff etc.

## AIM:

# The aim is to understand what factors the company should focus on, in order 
# to curb attrition. In other words, they want to know what changes they should 
# make to their workplace, in order to get most of their employees to stay

#1. Behaviour of the employee in the recent past
#2. Determine the factors to help curb the attrition rate

######################################################################################
# Getting ready for analysis and Loading the data                                                         
######################################################################################

# Check libraries and install and load required libraries
options(warn = -1)
libs = c("plyr","dplyr","tidyr","tidyverse", "scales","MASS","car","ROCR",
         "ggthemes","ggplot2","gridExtra","corrplot","lubridate", "cowplot", 
         "caret", "e1071", "caTools","GGally","dotwhisker","modEvA")
install.lib <- libs[!libs %in% installed.packages()]
for (pkg in install.lib)
  install.packages(pkg, dependencies = T)

loadlib     <- lapply(libs, library, character.only = T) # load them

remove(list = ls())
options(warn = 0)


### Set directory to load file -- setwd("E:/Upgrad Course/Input")

### Data Loading - read the CSV file into R, for processing. View the structure and understand data

### Importing the datasets
employee.survey <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
general <- read.csv("general_data.csv", stringsAsFactors = FALSE)
in.time <- read.csv("in_time.csv", stringsAsFactors = FALSE)
out.time <- read.csv("out_time.csv", stringsAsFactors = FALSE)
manager.survey <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)


#####################################################################################
# Data understanding                                                               #
#####################################################################################

##***************************************************************************##
# general  ->  Contains demographic data (such as age) and other 
#   behavioural deatils(such as travel) of the employees in a company
# employee.survey  ->  Contains survey data from employees depicting how much  
#   satsified are they with the job, the environment etc...
# manager.survey   ->  Contains survey details from the manager about  
#   each employee on how much involved and how well they perform on the job 
# in.time       -> Contains a year worth data of the employees' office in time
#   in date time format
# out.time       -> Contains a year worth data of the employees' office out time
#   in date time format
##***************************************************************************##
## View the structure of dataframes

str(employee.survey)
glimpse(employee.survey)
#Observations: 4 variables, int type, some of values has NA, 4410 observations.

str(general)
glimpse(general)
View(general)
#Observations: 24 variables, 4410 observations, Contains general info about the employee, 
#Contains many categorical variables which can be used for further analysis 

str(manager.survey)
glimpse(manager.survey)
#Observations: 3 variables, int type, 4410 observations.

#str(in.time)
glimpse(in.time)
dim(in.time)
#Observations: 262 variables - variables are dates so its employees in-time given by date, 
#Although its date-time, column type is char, 
#Total 4410 observations.
#Many variables contain NA values whihc needs to be handled during further analysis

#str(out.time)
glimpse(out.time)
dim(out.time)
#Observations: 262 variables - Columns are dates so its employees out-time given by date, Date range is same as in-time dates
#Although its date-time, column type is char
#Total 4410 observations.
#Many columns contain NA values whihc needs to be handled during further analysis

##Since all the datasets contain Employee ID, These can be merged into one file on the basis of Employee ID.

#Check the number of unique EmployeeIDs in each dataframe

length(unique(general$EmployeeID))    # 4410 observations
length(unique(employee.survey$EmployeeID)) # 4410 observations
length(unique(manager.survey$EmployeeID)) # 4410 observations
length(unique(in.time$X))  # 4410 observations. column "X" being EmployeeID
length(unique(out.time$X)) # 4410 observations. column "X" being EmployeeID

# Are there any differences in the Employeed IDs ? 
setdiff(general$EmployeeID,employee.survey$EmployeeID) #0 records difference
setdiff(general$EmployeeID,manager.survey$EmployeeID) #0 records difference
setdiff(general$EmployeeID,in.time$X)  #0 records difference
setdiff(general$EmployeeID,out.time$X) #0 records difference
# Identical EmployeeID across all these datasets. 

#####################################################################################
# Data Preparation & Exploratory Data Analysis                                                            #
#####################################################################################

# Data quality checks for 'general' data frame
anyDuplicated(general$EmployeeID)         # No duplicate employee IDs.
sum(is.na(general))                       # Total of 28 NA values.# 19 NAs for NumCompaniesWorked, # 9 NAs for TotalWorkingYears.
summary(general)                          
                                         
#Check blank values
sapply(general, function(x) length(which(x == ""))) # No blank values

#Check if column has same value repeated for all the rows
vapply(general, function(x) length(unique(x)) > 1, logical(1L)) 

# From above commands its clear that Over18, StandardHours and EmployeeCount contains unique single value
# Check values of these columns so it can be removed.

# The following columns each contain a single value, so can be removed.
unique(general$Over18)
unique(general$StandardHours)
unique(general$EmployeeCount)

# Remove the columns detailed above.
general <- general[vapply(general, function(x) length(unique(x)) > 1, logical(1L))]

# Handle NA values for TotalWorkingYears. 
# Assume Employee started working at the median age from the dataset.

median(general$Age[which(!is.na(general$TotalWorkingYears))] - general$TotalWorkingYears[which(!is.na(general$TotalWorkingYears))]) 
# 24 years - median age to start work

#Impute values to the TotalWorkingYears column where TotalWorkingYears is NA, by 'age - median' (median calculated in above step) 
#Thus handling NAs in the TotalWorkingYears column
general$TotalWorkingYears[which(is.na(general$TotalWorkingYears))] <- 
  general$Age[which(is.na(general$TotalWorkingYears))] - 24
#Check for NA again
sum(is.na(general)) 
#There are still 19 NA values

#Check which column has NA values
colSums(is.na(general)) 
View(general)

# This indicates NumCompaniesWorked has 19 values with NA
# As observed in the data, in addition to NA values, NumCompaniesWorked has other issues,  It contains many 0s,
# not possible as every row represents an employee (must have worked for a minimum of
# one company). Maybe it represents number of companies before the employee joined
# the current company? Not possible, e.g. EmployeeID = 2 has TotalWorkingYears = 6 and 
# YearsAtCompany = 5 (so must have worked for at least one company before the current
# company), yet NumCompaniesWorked = 0. 
# Due to the confusion and small number of NA rows, handle the NAs by simply removing these rows.

general <- na.omit(general)

sum(((general$TotalWorkingYears - general$YearsAtCompany) > 0 & general$NumCompaniesWorked == 0)) 
# 585 records has NumCompaniesWorked = 0 and TotalWorkingYears and YearsAtCompany has confusing data 
#so we didnt remove given the high number of records.

# Convert relevant categorical columns to Factor type.
factorColumns <- c("Attrition", "BusinessTravel", "Department", "Education",
                   "EducationField", "Gender", "JobLevel", "JobRole", "MaritalStatus",
                   "StockOptionLevel")
general[, factorColumns] <- lapply(general[, factorColumns], as.factor)

# Rename factor levels for Education column.These values are picked up from the data dictionary provided
factor.levels <- c("1" = "Below College", "2" = "College", "3" = "Bachelor", "4" = "Master", "5" = "Doctor")

general$Education <-  revalue(general$Education, factor.levels)

#######################

# Data quality checks for in.time and out.time dataframes - 
# first remove the employees that were removed from general dataframe.
in.time <- in.time[(in.time$X %in% general$EmployeeID), ]
out.time <- out.time[(out.time$X %in% general$EmployeeID), ]

summary(in.time) 
summary(out.time)

##############Changing the names of the column in the 2 files
colnames(in.time)[1]<-c("EmployeeID")
colnames(out.time)[1]<-c("EmployeeID")

anyDuplicated(in.time$EmployeeID)         # No duplicate employee IDs.
anyDuplicated(out.time$EmployeeID)        # No duplicate employee IDs.

sum(is.na(in.time))                       # 108600 NA values
sum(is.na(out.time))                      # 108600 NA values

# All the column header contains X in the begining
# Removing X (Replacing with blank) from all the column names in in.time and out.time datasets.
colnames(in.time) <- gsub(pattern = "X", "", colnames(in.time))
colnames(out.time) <- gsub(pattern=  "X", "", colnames(out.time))

###Checking the which columns are all NA's in both the datasets
colnames(in.time)[which(sapply(in.time,function(x) sum(is.na(x))) == nrow(in.time))]
colnames(out.time)[which(sapply(out.time,function(x) sum(is.na(x))) == nrow(out.time))]

# Remove columns identified in above step, i.e. All columns with unique value of 'NA'
in.time <- in.time[vapply(in.time, function(x) length(unique(x)) > 1, logical(1L))]
out.time <- out.time[vapply(out.time, function(x) length(unique(x)) > 1, logical(1L))]

##############Changing the Wide format to Long format

in.time_long <- gather(in.time, date, timein, -EmployeeID)
out.time_long <- gather(out.time, date, timeout, -EmployeeID)

##############Merging the in.time and out.time files
timediff_file <- merge(in.time_long, out.time_long, by=c("EmployeeID", "date"))

##############Checking whether there are some cases in which employee missed to punch in or out
sum(is.na(timediff_file$timein) & !is.na(timediff_file$timeout)) # 0 records
sum(!is.na(timediff_file$timein) & is.na(timediff_file$timeout)) # 0 records

# Convert each string to POSIXct type.
timediff_file[, 3:4] <- lapply(timediff_file[, 3:4], as.POSIXct)
timediff_file$timeDifference <- round(as.numeric(difftime(timediff_file$timeout, timediff_file$timein)), 2)
str(timediff_file)

###1.Calculating average working hours
emp_avg <- aggregate(timediff_file$timeDifference, by = list(timediff_file$EmployeeID), mean, na.rm=TRUE)

###Renaming the columns
colnames(emp_avg) <- c("EmployeeID", "mean.hours") 
emp_avg$mean.hours <- round(emp_avg$mean.hours,digits=2)

###2.calcualting number of leaves.

timediff_file$leaves <- sapply(timediff_file$timeDifference, function(x) sum(is.na(x)))

emp_leaves <- aggregate(timediff_file$leaves, by = list(timediff_file$EmployeeID), sum, na.rm = TRUE)

###Renaming the columns
colnames(emp_leaves) <- c("EmployeeID", "Leaves") 

# Remove in.time and out.time from memory, as no longer required.
rm(in.time_long, out.time_long)

# Merge average hours and number of leaves.
emp_worked.time <- list(emp_avg, emp_leaves) %>% 
  reduce(left_join, by = "EmployeeID")

# Remove emp_avg,emp_leaves,timediff_file from memory, as no longer required.
rm(emp_avg, emp_leaves, timediff_file)

###calcualting overtime derived metric for employees averaging >10 or <7 hours.
emp_worked.time$overtime      <-  ifelse(emp_worked.time$mean.hours > 10, "Over",
                                 (ifelse(emp_worked.time$mean.hours < 7, "Less", "Normal")))
table(emp_worked.time$overtime )                        

#######################

### Data quality checks from employee_survey_data.csv (employee.survey dataframe)- 
### first remove the employees that were removed from general dataframe.
employee.survey <-
  employee.survey[(employee.survey$EmployeeID %in% general$EmployeeID), ]

anyDuplicated(employee.survey$EmployeeID)   # No duplicate employee IDs.
sum(is.na(employee.survey))                 # 83 NAs in total.

# Check which columns have the NA values.
colSums(is.na(employee.survey))[which(colSums(is.na(employee.survey)) > 0)] 
# EnvironmentSatisfaction (25),JobSatisfaction(20),WorkLifeBalance(38)

# Lets check the % of NA values
employee.survey %>%
  summarise_all(funs(sum(is.na(.))/n()))
# Less than 1% values. We can either ignore these or treat them. 
# But since less tahn 1% lets replace NA values with median

# Replace NA values with the median value for that column.
employee.survey$EnvironmentSatisfaction[is.na(employee.survey$EnvironmentSatisfaction)] <-
  median(employee.survey$EnvironmentSatisfaction, na.rm = TRUE)
employee.survey$JobSatisfaction[is.na(employee.survey$JobSatisfaction)] <-
  median(employee.survey$JobSatisfaction, na.rm = TRUE)
employee.survey$WorkLifeBalance[is.na(employee.survey$WorkLifeBalance)] <-
  median(employee.survey$WorkLifeBalance, na.rm = TRUE)

# Convert columns to Factor type.
factorColumns <- c("EnvironmentSatisfaction", "JobSatisfaction", "WorkLifeBalance")
employee.survey[, factorColumns] <- lapply(employee.survey[, factorColumns], as.factor)

# Rename factor levels. These values as per data dictionary for EnvironmentSatisfaction and JobSatisfaction
factor.levels <- c("1" = "Low", "2" = "Medium", "3" = "High", "4" = "Very High")

employee.survey$EnvironmentSatisfaction <-
  revalue(employee.survey$EnvironmentSatisfaction, factor.levels)
employee.survey$JobSatisfaction <-
  revalue(employee.survey$JobSatisfaction, factor.levels)

# Rename factor levels. These values as per data dictionary for WorkLifeBalance
factor.levels <- c("1" = "Bad", "2" = "Good", "3" = "Better", "4" = "Best")

employee.survey$WorkLifeBalance <-
  revalue(employee.survey$WorkLifeBalance, factor.levels)

                              #######################

# Data quality checks  from manager_survey_data.csv (manager.survey) 
# first remove the employees that were removed from general dataframe.
manager.survey <-
  manager.survey[(manager.survey$EmployeeID %in% general$EmployeeID), ]

anyDuplicated(manager.survey$EmployeeID)   # No duplicate employee IDs.
sum(is.na(manager.survey))                 # 0 NAs.

# Convert columns to Factor type.
factorColumns <- c("JobInvolvement", "PerformanceRating")
manager.survey[, factorColumns] <- lapply(manager.survey[, factorColumns], as.factor)

# Rename factor levels.These values as per data dictionary for  JobInvolvement
factor.levels <- c("1" = "Low", "2" = "Medium", "3" = "High", "4" = "Very High")

manager.survey$JobInvolvement <- revalue(manager.survey$JobInvolvement, factor.levels)

# From data, looks like there are no PerformanceRating of 1 and 2, only 3 and 4 are present, Confirm this finding by summary
summary(manager.survey$PerformanceRating) 
# No employees have rating of 1 or 2, Dataset contains only rating 3 and 4

# Rename factor levels. These values as per data dictionary for  PerformanceRating
factor.levels <- c("3" = "Excellent", "4" = "Outstanding")

manager.survey$PerformanceRating <- revalue(manager.survey$PerformanceRating, factor.levels)

                              #######################

# Merge all the files into a single data frame.
master <- list(general,employee.survey, manager.survey,emp_worked.time) %>% 
  reduce(left_join, by = "EmployeeID")

str(master)
View(master)

# Check which columns have the NA values.
colSums(is.na(master))[which(colSums(is.na(master)) > 0)] 

master_bkp <- master # created specific to plots because during model prep we are convering variables and removing.

#######################

# EDA: Plot charts before scaling variables and creating dummy variables, as values
# will be easier to interpret at this point. Only the most significant plots have
# been retained below to reduce length of code.
#######################
### check overall attrition rate in 2015
master$Attrition <- ifelse(master$Attrition=="Yes",1,0)
sum(master$Attrition) / nrow(master)      # 16% attrition among all employees

# Barcharts for categorical features with stacked attrition information

# univariate plots with categorical variables with attrition

###the below plots could have been added to one pager but due to performance and time taking 
#to run all the plots.

plot_grid(ggplot(master_bkp, aes(x=BusinessTravel,fill=Attrition))+ geom_bar() + theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(master_bkp, aes(x=Education,fill=Attrition))+ geom_bar()+ theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(master_bkp, aes(x=EducationField,fill=Attrition))+ geom_bar()+ theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(master_bkp, aes(x=Gender,fill=Attrition))+ geom_bar()+ theme_pander() + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(master_bkp, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(show.legend = F)+theme_pander() + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)),
          ggplot(master_bkp, aes(x=Department,fill=Attrition))+ geom_bar(show.legend = F)+theme_pander()+ theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)),
          ggplot(master_bkp, aes(x=JobLevel,fill=Attrition))+ geom_bar(show.legend = F)+theme_pander(),
          ggplot(master_bkp, aes(x=JobRole,fill=Attrition))+ geom_bar(show.legend = F)+theme_pander() + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
align = "h")

table(master$Attrition, master$BusinessTravel)
#Although from graph those who travelled rarely tend to leave company more as per the counts, 
#If we look at the % of attrition with frequent travel, that is significantly high
#25% Attrition for Travel_Frequently, 8% for Non-Travel and 15% for Travel_Rarely
#Attrition rate is more among male employees as we can assume compared to female employees they tend to have more
#dependants which results in seeking a good package
#We will check % wise distribution of attrition for Education field and Education in further analysis

table(master$Attrition, master$JobRole)
#Research & #Development Dept. have high attrition.
#Assuming Joblevel 1 denotes employees who have just entered the role/field and 5 denoting highly experienced or 
#experts of the field. From the plot we understand that as the level increases people are less tend to leave company
#Low proportion of HR employees are there in an organization
#Attrition is more among singles and less among divorcees

plot_grid(ggplot(master_bkp, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(show.legend = F)+theme_pander(),
          ggplot(master_bkp, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(show.legend = F)+theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(master_bkp, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(show.legend = F)+theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(master_bkp, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(show.legend = F)+theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(master_bkp, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(show.legend = F)+theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(master_bkp, aes(x=PerformanceRating,fill=Attrition))+ geom_bar(show.legend = F)+theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(master_bkp, aes(x=overtime,fill=Attrition))+ geom_bar()+theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="centre"),
          align = "h")

#StockOptionLevel - More employees quit in lower levels
#EnvironmentSatisfaction - More employees quit where the statisfaction level is low compared to other levels. But not
#showing any significant trend
#JobSatisfaction - Higher attrition among Job Satisfaction level low.
#WorkLifeBalance - if considering the proportion of data, attrition is more in level 1(Bad) which is as expected.
#JobInvolvement  - Attrition is more with involvement level 3 (High)
#PerformanceRating - compared to rating 4, rating 3 has high attrition amount

table (master$Attrition, master$overtime)
#overtime - Although number wise working Normal likely to leave but attrition % is high for overtime over working employee .

# Bivariate plots
multiBivar <- function(xfeature, yfeature, xlabel, ylabel) {
  ggplot(master_bkp, aes(x = xfeature, y = yfeature, fill = xfeature)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1, show.legend = F) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    labs(x = xlabel, y = ylabel, title = paste(ylabel, "vs", xlabel)) +
    theme_pander()
}
# Boxplots of numeric variables relative to attrition status

plot_grid(multiBivar(master_bkp$Attrition, master_bkp$Age,"Attrition", "Age"),
          multiBivar(master_bkp$Attrition, master_bkp$DistanceFromHome,"Attrition", "DistanceFromHome"),
          multiBivar(master_bkp$Attrition, master_bkp$MonthlyIncome,"Attrition", "MonthlyIncome"),
          multiBivar(master_bkp$Attrition, master_bkp$NumCompaniesWorked,"Attrition", "NumCompaniesWorked"),
          multiBivar(master_bkp$Attrition, master_bkp$PercentSalaryHike,"Attrition", "PercentSalaryHike"),
          multiBivar(master_bkp$Attrition, master_bkp$TotalWorkingYears,"Attrition", "TotalWorkingYears"),
          multiBivar(master_bkp$Attrition, master_bkp$TrainingTimesLastYear,"Attrition", "TrainingTimesLastYear"),
          multiBivar(master_bkp$Attrition, master_bkp$YearsAtCompany,"Attrition", "YearsAtCompany"),
          multiBivar(master_bkp$Attrition, master_bkp$YearsSinceLastPromotion,"Attrition", "YearsSinceLastPromotion"),
          multiBivar(master_bkp$Attrition, master_bkp$YearsWithCurrManager,"Attrition", "YearsWithCurrManager"),
          multiBivar(master_bkp$Attrition, master_bkp$mean.hours,"Attrition", "mean.hours"),
          multiBivar(master_bkp$Attrition, master_bkp$Leaves,"Attrition", "Leaves"))
          
# Create Continuous variables vector
(contvarnames <- names(Filter(is.numeric, master_bkp)))
# Check the Continuous columns 
sapply(master_bkp[contvarnames], summary)

# Correlation between numeric variables

ggpairs(master_bkp[, c("MonthlyIncome","PercentSalaryHike","Age","TrainingTimesLastYear")])
ggpairs(master_bkp[, c("NumCompaniesWorked","TotalWorkingYears","YearsWithCurrManager","YearsAtCompany","TrainingTimesLastYear")])
ggpairs(master_bkp[, c("EmployeeID","Leaves","mean.hours","DistanceFromHome")])


# Attrition Rate by Age
group <- group_by(master_bkp, Age)
#detach("package:plyr", unload=TRUE) 
by_Age <- summarize(group, Total.Employees = n(),
                    No.Attrition = sum(as.character(Attrition) == "Yes"),
                    Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_Age, aes(x = round(Age / 10, digits = 0) * 10, y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Age", y = "Attrition Rate (%)", title = "Attrition Rate by Age") +
  theme_pander()

# Youngest employees (in their 20s and 30s) are most likely to leave. There is
# another peak in the 60s, likely due to retirement. Boxplot confirms that leaers
# have a lower median age.
boxplot(Age ~ Attrition, data = master_bkp)

# Attrition Rate by % Total Working Years
group <- group_by(master_bkp, TotalWorkingYears)
by_TotalWorkingYears <- summarize(group, Total.Employees = n(),
                                  No.Attrition = sum(as.character(Attrition) == "Yes"),
                                  Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_TotalWorkingYears, aes(x = round(TotalWorkingYears / 10, digits = 0) * 10, y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Total Working Years", y = "Attrition Rate (%)", title = "Attrition Rate by Total Working Years") +
  theme_pander()
  
# N.B. only 6 employees with >=40 total working years (very small group). Other than
# this small group, there is higher attrition amongst the employees with fewest total
# working years. Group >40 is likely to represent employees retiring. Boxplot
# confirms that leavers have lower median total working years.
boxplot(TotalWorkingYears ~ Attrition, data = master_bkp)

# Attrition Rate by Years at Company
group <- group_by(master_bkp, YearsAtCompany)
by_YearsAtCompany <- summarize(group, Total.Employees = n(),
                                  No.Attrition = sum(as.character(Attrition) == "Yes"),
                                  Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_YearsAtCompany, aes(x = round(YearsAtCompany / 10, digits = 0) * 10, y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Years at Company", y = "Attrition Rate (%)", title = "Attrition Rate by Years at Company") +
  theme_pander()
# N.B. only three employees >=40 years (very small sample) likely to represent
# employees retiring. Boxplot shows that leavers have a lower median number of years ar
# company.
boxplot(YearsAtCompany ~ Attrition, data = master_bkp)


# Attrition Rate by years with current manager
group <- group_by(master_bkp, YearsWithCurrManager)
by_YearsWithCurrManager <- summarize(group, Total.Employees = n(),
                                        No.Attrition = sum(as.character(Attrition) == "Yes"),
                                        Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_YearsWithCurrManager, aes(x = reorder(factor(YearsWithCurrManager),-Pct.Attrition), y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Years With Current Manager", y = "Attrition Rate (%)", title = "Attrition Rate by Years With Manager") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3) 

# Higher attrition amongst employees who have been with their manager <10 years.
# Anomoly ar 14 years but boxplot confirms that leavers have been with their
# current manager for a lower median time.
boxplot(YearsWithCurrManager ~ Attrition, data = master_bkp)

# Attrition Rate by Business Travel
group <- group_by(master_bkp, BusinessTravel)
by_BusinessTravel <- summarize(group, Total.Employees = n(),
                         No.Attrition = sum(as.character(Attrition) == "Yes"),
                         Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_BusinessTravel, aes(x = reorder(BusinessTravel,-Pct.Attrition), y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Business Travel", y = "Attrition Rate (%)", title = "Attrition Rate by Rate of Business Travel") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3) 
# Attrition rate is higher among employees traveling frequently for business travel.

# Attrition Rate by Department
group <- group_by(master_bkp, Department)
by_Department <- summarize(group, Total.Employees = n(),
                               No.Attrition = sum(as.character(Attrition) == "Yes"),
                               Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_Department, aes(x = Department, y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Department", y = "Attrition Rate (%)", title = "Attrition Rate by Department") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3) 
# Attrition rate is higher in HR.

# Attrition Rate by Education
group <- group_by(master_bkp, Education)
by_Education <- summarize(group, Total.Employees = n(),
                           No.Attrition = sum(as.character(Attrition) == "Yes"),
                           Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_Education, aes(x = reorder(Education,-Pct.Attrition), y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Education", y = "Attrition Rate (%)", title = "Attrition Rate by Education") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3) 
# Attrition rate is higher in college educated employees.

# Attrition Rate by Education Field
group <- group_by(master_bkp, EducationField)
by_EducationField <- summarize(group, Total.Employees = n(),
                          No.Attrition = sum(as.character(Attrition) == "Yes"),
                          Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_EducationField, aes(x = reorder(EducationField,-Pct.Attrition), y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Education Field", y = "Attrition Rate (%)", title = "Attrition Rate by Education Field") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3) 
# Attrition rate is higher in HR-educated employees.

# Attrition Rate by job role
group <- group_by(master_bkp, JobRole)
by_JobRole <- summarize(group, Total.Employees = n(),
                         No.Attrition = sum(as.character(Attrition) == "Yes"),
                         Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_JobRole, aes(x = reorder(JobRole,-Pct.Attrition), y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Job Role", y = "Attrition Rate (%)", title = "Attrition Rate by Job Role") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3) 
# Research directors, research scientist and sales exec have the highest attrition.

# Attrition Rate by marital status
group <- group_by(master_bkp, MaritalStatus)
by_MaritalStatus <- summarize(group, Total.Employees = n(),
                        No.Attrition = sum(as.character(Attrition) == "Yes"),
                        Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_MaritalStatus, aes(x = reorder(MaritalStatus,-Pct.Attrition),y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Marital Status", y = "Attrition Rate (%)", title = "Attrition Rate by Marital Status") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3)
# Single employees more likely to leave, divorced are least likely.

# Attrition Rate by environment satisfaction
group <- group_by(master_bkp, EnvironmentSatisfaction)
by_EnvironmentSatisfaction <- summarize(group, Total.Employees = n(),
                                 No.Attrition = sum(as.character(Attrition) == "Yes"),
                                 Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_EnvironmentSatisfaction, aes(x = reorder(EnvironmentSatisfaction,-Pct.Attrition), y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Environment Satisfaction", y = "Attrition Rate (%)", title = "Attrition Rate by Environment Satisfaction") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3)
# Attrition highest for those with low environmental satisfaction.

# Attrition Rate by job satisfaction
group <- group_by(master_bkp, JobSatisfaction)
by_JobSatisfaction <- summarize(group, Total.Employees = n(),
                                        No.Attrition = sum(as.character(Attrition) == "Yes"),
                                        Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_JobSatisfaction, aes(x = reorder(JobSatisfaction,-Pct.Attrition), y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Job Satisfaction", y = "Attrition Rate (%)", title = "Attrition Rate by Job Satisfaction") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3)
# Attrition highest for those with low job satisfaction, low for those with very high
# satisfaction.

# Attrition Rate by work-life balance
group <- group_by(master_bkp, WorkLifeBalance)
by_WorkLifeBalance <- summarize(group, Total.Employees = n(),
                                No.Attrition = sum(as.character(Attrition) == "Yes"),
                                Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_WorkLifeBalance, aes(x = reorder(WorkLifeBalance, -Pct.Attrition),y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Work-Life Balance", y = "Attrition Rate (%)", title = "Attrition Rate by Work-Life Balance") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3)
# Attrition highest for those with bad worklife balance .


# Attrition Rate by TrainingTimesLastYear
group <- group_by(master_bkp, TrainingTimesLastYear)
by_TrainingTimesLastYear <- summarize(group, Total.Employees = n(),
                                No.Attrition = sum(as.character(Attrition) == "Yes"),
                                Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_TrainingTimesLastYear, aes(x = reorder(TrainingTimesLastYear, -Pct.Attrition),y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "TrainingTimesLastYear", y = "Attrition Rate (%)", title = "Attrition Rate by TrainingTimesLastYear") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3)
# Attrition highest for those with < 3 times TrainingTimesLastYear.

# Attrition Rate by PerformanceRating
group <- group_by(master_bkp, PerformanceRating)
by_PerformanceRating <- summarize(group, Total.Employees = n(),
                                      No.Attrition = sum(as.character(Attrition) == "Yes"),
                                      Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_PerformanceRating, aes(x = reorder(PerformanceRating, -Pct.Attrition),y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "PerformanceRating", y = "Attrition Rate (%)", title = "Attrition Rate by PerformanceRating") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3)
# Attrition highest for those with Outstanding performers.

# Attrition Rate by % Salary Hike
group <- group_by(master_bkp, PercentSalaryHike)
by_PercentSalaryHike <- summarize(group, Total.Employees = n(),
                                  No.Attrition = sum(as.character(Attrition) == "Yes"),
                                  Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_PercentSalaryHike, aes(x = factor(PercentSalaryHike), y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Percent Salary Hike", y = "Attrition Rate (%)", title = "Attrition Rate by Salary Hike") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3)

#boxplot PercentSalaryHike vs Attrition
boxplot(PercentSalaryHike ~ Attrition, data = master)

#It shows when the salary hike is maximum attrition rate is max.

# Attrition Rate by overtime
group <- group_by(master_bkp, overtime)
by_overtime <- summarize(group, Total.Employees = n(),
                                      No.Attrition = sum(as.character(Attrition) == "Yes"),
                                      Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_overtime, aes(x = reorder(overtime, -Pct.Attrition),y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "overtime", y = "Attrition Rate (%)", title = "Attrition Rate by overtime") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3)
# Attrition highest for those who worked over time more.

# Attrition Rate by Distance from Home
group <- group_by(master_bkp, DistanceFromHome)
by_DistanceFromHome <- summarize(group, Total.Employees = n(),
                                 No.Attrition = sum(as.character(Attrition) == "Yes"),
                                 Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_DistanceFromHome, aes(x = DistanceFromHome , y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge", fill='#1567b6') +
  labs(x = "Distance from Home", y = "Attrition Rate (%)", title = "Attrition Rate by Distance From Home") +
  theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3)

ggplot(by_DistanceFromHome, aes(x = round(DistanceFromHome / 10, digits = 0) * 10 , y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Distance from Home", y = "Attrition Rate (%)", title = "Attrition Rate by Distance From Home") +
  theme_pander()

#Attrition rate is higher at distance at ~20 and again reduces at ~30, so gives mixed signals.
#importance of these can be validated during model building

boxplot(DistanceFromHome ~ Attrition, data = master)
# Attrition Rate by Monthly Income
min(master_bkp$MonthlyIncome)
master_bkp$monthlyincomebin <- round(master_bkp$MonthlyIncome / 10000, digits = 0) * 10000

group <- group_by(master_bkp, monthlyincomebin)

by_MonthlyIncome <- summarize(group, Total.Employees = n(),
                              No.Attrition = sum(as.character(Attrition) == "Yes"),
                              Pct.Attrition = round(No.Attrition / Total.Employees * 100))

ggplot(by_MonthlyIncome, aes(x = monthlyincomebin, y = Pct.Attrition)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Monthly Income", y = "Attrition Rate (%)", title = "Attrition Rate by Monthly Income")+ theme_pander()+
  geom_text(aes(y = Pct.Attrition + .5,    # nudge above top of bar
                label = paste0(Pct.Attrition, '%')),    # prettify
            position = position_dodge(width = .9), size = 3)

#Boxplot monthly income and attrition

boxplot(MonthlyIncome ~ Attrition, data = master)
#Attrition peaks at 1,60,000 and is higher at 60,000. Importance of this can be validated during building a model

##***************************************************************************##
#                                                                             #
#                         Multivariate Analysis                               #
#                        (Continuous Variables)                               #
#                                                                             #
##***************************************************************************##
emp_model <-  master_bkp

emp_model$Attrition <- as.numeric(ifelse(as.character(emp_model$Attrition) == "Yes", 1, 0))
# Create Continuous variables vector
contvarnames <- names(Filter(is.numeric, emp_model))
contvarnames
# Check the Continuous columns 
sapply(emp_model[contvarnames], summary)

corrplot.mixed(cor(cbind(emp_model[contvarnames], emp_model$Attrition)), 
               upper = "ellipse", number.cex = .8,
               tl.cex = 0.55, tl.pos = 'lt')
#Year At Company and Years with Current Manager are highly correlated.
#Years with Current Manager , total working years, Years since last promotion are highly correlated.
#As expected Age and Total working years are strongly correlated.
#Total working years shows a strong positive correlation with Years at company.
#Attrition shows a positive correlation with mean working hours.
#Attrition shows -ve correlation with :  Age,Total Working Years,Years at Company,Years with current manager

#cor(emp_model[contvarnames])

##***************************************************************************##
#                                                                             #
#                        Outlier Treatment                                    #
#                     (Continuous Variables)                                  #
#                                                                             #
##***************************************************************************##
                           

# Check for outliers in the numeric variables using boxplots.
par(mfrow = c(4,2))
par(mar = rep(2,4))
boxplot(master$Age,main="Age")                                          # No Outliers
boxplot(master$DistanceFromHome,main="DistanceFromHome")                # No Outliers
boxplot(master$MonthlyIncome,main="MonthlyIncome")                      # Outliers present with high income.
boxplot(master$NumCompaniesWorked,main="NumCompaniesWorked")            # Outliers with high no. of companies.
boxplot(master$PercentSalaryHike,main="PercentSalaryHike")              # No Outliers
boxplot(master$TotalWorkingYears,main="TotalWorkingYears")              # Outliers with high no. of years.
boxplot(master$TrainingTimesLastYear,main="TrainingTimesLastYear")      # Outliers with high/low no. of trainings.
boxplot(master$YearsAtCompany,main="YearsAtCompany")                    # Outliers with YearsAtCompany.
boxplot(master$YearsSinceLastPromotion,main="YearsSinceLastPromotion")  # Outliers with YearsSinceLastPromotion.
boxplot(master$YearsWithCurrManager,main="YearsWithCurrManager")        # Outliers with YearsWithCurrManager
boxplot(master$mean.hours,main="mean.hours")                            # Outliers with mean.hours.
boxplot(master$Leaves,main="Leaves")                                    # no Outliers

# Cap outliers at the level of the 5th and 95th centiles.
master$MonthlyIncome <- squish(master$MonthlyIncome, quantile(master$MonthlyIncome, c(.05, .95)))
master$NumCompaniesWorked <- squish(master$NumCompaniesWorked, quantile(master$NumCompaniesWorked, c(.05, .95)))
master$TotalWorkingYears <- squish(master$TotalWorkingYears, quantile(master$TotalWorkingYears, c(.05, .95)))
master$TrainingTimesLastYear <- squish(master$TrainingTimesLastYear, quantile(master$TrainingTimesLastYear, c(.05, .95)))
master$YearsAtCompany <- squish(master$YearsAtCompany, quantile(master$YearsAtCompany, c(.05, .95)))
master$YearsSinceLastPromotion <- squish(master$YearsSinceLastPromotion, quantile(master$YearsSinceLastPromotion, c(.05, .95)))
master$YearsWithCurrManager <- squish(master$YearsWithCurrManager, quantile(master$YearsWithCurrManager, c(.05, .95)))
master$mean.hours <- squish(master$mean.hours, quantile(master$mean.hours, c(.05, .95)))

# Scale numeric variables to standardised ranges.
scaleColumns <- c("Age", "DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                  "TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","mean.hours",
                  "Leaves")

# Scaling Continuous Variables
master[scaleColumns] <-  lapply(master[scaleColumns], function(x) scale(x))

# Create dummy variables to convert categorical variables into numeric variables.

# Start with those factors with two levels. Add new variables, with names that make
# sense with 1/0 values, and then remove the original variables
master$Attrition <- master_bkp$Attrition
levels(master$Attrition) <- c(0, 1)
master$Attrition <- as.numeric(levels(master$Attrition))[master$Attrition]

master$Gender.Male <- master$Gender
levels(master$Gender.Male) <- c(0, 1)
master$Gender.Male <- as.numeric(levels(master$Gender.Male))[master$Gender.Male]
master$Gender <- NULL

master$Performance.Outstanding <- master$PerformanceRating
levels(master$Performance.Outstanding) <- c(0, 1)
master$Performance.Outstanding <- 
  as.numeric(levels(master$Performance.Outstanding))[master$Performance.Outstanding]
master$PerformanceRating <- NULL

# Use model.matrix() function to create dummy variables for the remaining factors,
# which all have more than two levels. cbind the dummy variables to dataframe and
# then remove the original columns.

# Dummy Variables  for Categorical variables
dummycols <- master[,c("BusinessTravel","Department","Education","EducationField","JobLevel","JobRole",
               "MaritalStatus","StockOptionLevel","EnvironmentSatisfaction","JobSatisfaction",
               "WorkLifeBalance","JobInvolvement","overtime")]

#Creating dummy attributes for factor attributes
dummies <- data.frame(sapply(dummycols, function(x)
  data.frame(model.matrix(~x-1, data = dummycols))[,-1]))

str(dummies)

colnames(dummies) <- gsub(pattern=".x","",colnames(dummies))

#Removing the categorical attributes and adding the corresponding dummy attributes.
master <- subset(master, select = -c(BusinessTravel,Department,Education,EducationField,JobLevel,JobRole,
                                     MaritalStatus,StockOptionLevel,EnvironmentSatisfaction,JobSatisfaction,
                                     WorkLifeBalance,JobInvolvement,overtime))

master <-  cbind(master, dummies)
dim(master) # 4391 observations with 60 variables
View(master)  

# EmployeeID is not required as part of the model, remove this variable.
master$EmployeeID <- NULL

#####################################################################################
# Model building and evaluation                                                     #
# You are required to model the probability of attrition using a logistic           #
# regression.                                                                       #
#####################################################################################

# Split data into training and test datasets, use 70% of data for training set.
set.seed(100)
trainindices <- sample(1:nrow(master), 0.7 * nrow(master))
train <- master[trainindices, ]
test <- master[-trainindices, ]

# Choose a stepwise selection method to build a model. Use all variables for model_1
model_1 <- glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)
# AIC 2148.7

# Use stepAIC to eliminate some of the variables
step <- stepAIC(model_1, direction = "both")
step

# Use only the variables that stepAIC considers to be important for model_2.
model_2 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationBachelor + 
                 EducationMaster + EducationDoctor + EducationFieldMarketing + 
                 EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                 JobLevel2 + JobLevel5 + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.ecutive + JobRoleSales.Representative + 
                 MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                 EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                 WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                 JobInvolvementMedium + JobInvolvementHigh + overtimeNormal + 
                 overtimeOver, family = "binomial", data = train)
summary(model_2)
vif(model_2)
# AIC 2122.9 (improved compared to model_1). There are multiple
# insignificant p-values and multicolinearity . JobLevel5
# has the lowest significance(p value 0.169645) and a VIF 1.081308, so remove this first.

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationBachelor + 
                 EducationMaster + EducationDoctor + EducationFieldMarketing + 
                 EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                 JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.ecutive + JobRoleSales.Representative + 
                 MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                 EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                 WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                 JobInvolvementMedium + JobInvolvementHigh + overtimeNormal + 
                 overtimeOver, family = "binomial", data = train)
summary(model_3)
vif(model_3)
# AIC 2122.9 EducationBachelor now most insignificant variable, although it has a low VIF
# of 1.442543 Remove based on significance.

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + 
                 EducationMaster + EducationDoctor + EducationFieldMarketing + 
                 EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                 JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.ecutive + JobRoleSales.Representative + 
                 MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                 EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                 WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                 JobInvolvementMedium + JobInvolvementHigh + overtimeNormal + 
                 overtimeOver, family = "binomial", data = train)
summary(model_4)
vif(model_4)
# AIC 2123 EducationMaster now most insignificant variable, although it has a low VIF of
# 1.046039 Remove based on significance.

model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + 
                 EducationDoctor + EducationFieldMarketing + 
                 EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                 JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.ecutive + JobRoleSales.Representative + 
                 MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                 EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                 WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                 JobInvolvementMedium + JobInvolvementHigh + overtimeNormal + 
                 overtimeOver, family = "binomial", data = train)
summary(model_5)
vif(model_5)
# AIC 2122.1 EducationDoctor now most insignificant variable,
# although it has a low VIF of 1.048227 Remove based on significance.

model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + 
                 EducationFieldMarketing + 
                 EducationFieldMedical + EducationFieldOther + EducationFieldTechnical.Degree + 
                 JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.ecutive + JobRoleSales.Representative + 
                 MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                 EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                 WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                 JobInvolvementMedium + JobInvolvementHigh + overtimeNormal + 
                 overtimeOver, family = "binomial", data = train)
summary(model_6)
vif(model_6)
# AIC 2122.5 EducationFieldOther now most insignificant variable, although it has a low VIF
# of 1.096225 Remove based on significance.

model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + 
                 EducationFieldMarketing + 
                 EducationFieldMedical + EducationFieldTechnical.Degree + 
                 JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.ecutive + JobRoleSales.Representative + 
                 MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                 EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                 WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                 JobInvolvementMedium + JobInvolvementHigh + overtimeNormal + 
                 overtimeOver, family = "binomial", data = train)
summary(model_7)
vif(model_7)
# AIC 2123. JobInvolvementMedium now most insignificant variable, although it has a low
# VIF of 1.900223 Remove based on significance.

model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + 
                 EducationFieldMarketing + 
                 EducationFieldMedical + EducationFieldTechnical.Degree + 
                 JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.ecutive + JobRoleSales.Representative + 
                 MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                 EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                 WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                 JobInvolvementHigh + overtimeNormal + 
                 overtimeOver, family = "binomial", data = train)
summary(model_8)
vif(model_8)
# AIC 2123.5 JobRoleSales.ecutive now most insignificant

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + 
                 EducationFieldMarketing + 
                 EducationFieldMedical + EducationFieldTechnical.Degree + 
                 JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director +  JobRoleSales.Representative + 
                 MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                 EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                 JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                 WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                 JobInvolvementHigh + overtimeNormal + 
                 overtimeOver, family = "binomial", data = train)
summary(model_9)
vif(model_9)
# AIC 2124.3. EducationFieldMedical now most insignificant variable, although it has a low VIF of
# 1.156407 Remove based on significance.

model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + 
                  EducationFieldMarketing + 
                  EducationFieldTechnical.Degree + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director +  JobRoleSales.Representative + 
                  MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                  JobInvolvementHigh + overtimeNormal + 
                  overtimeOver, family = "binomial", data = train)
summary(model_10)
vif(model_10)
# AIC 2125.2 JobRoleResearch.Director now most insignificant variable, although it has a low VIF of
# 1.037171 Remove JobRoleResearch.Director based on significance.

model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + 
                  EducationFieldMarketing + 
                  EducationFieldTechnical.Degree + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                  JobInvolvementHigh + overtimeNormal + 
                  overtimeOver, family = "binomial", data = train)
summary(model_11)
vif(model_11)
# AIC 2126.2 overtimeOver has high vif 6.139973 so remove this

model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + 
                  EducationFieldMarketing + 
                  EducationFieldTechnical.Degree + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                  JobInvolvementHigh + overtimeNormal  
                  , family = "binomial", data = train)
summary(model_12)
vif(model_12)
# AIC 2143.8 overtimeNormal Remove it based on significance

model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + 
                  EducationFieldMarketing + 
                  EducationFieldTechnical.Degree + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_13)
vif(model_13)
# AIC 2141.8 Remove EducationFieldMarketing based on significance

model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + 
                  EducationFieldTechnical.Degree + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_14)
vif(model_14)
# AIC 2143.3, EducationFieldTechnical.Degree remove

model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_15)
vif(model_15)
# AIC 2145, slight improvement. Remove JobLevel2 based on significance.

model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + 
                  JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_16)
vif(model_16)
# AIC 2146.2, remove BusinessTravelTravel_Rarely based on high VIF (4.086981)

model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development + DepartmentSales + 
                  JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + StockOptionLevel1 + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_17)
vif(model_17)
# AIC 2153.8. Remove StockOptionLevel1 based on p value > 0.029403, which has become low significant  and vif 1.010135

model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development + DepartmentSales + 
                  JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_18)
vif(model_18)
# AIC 2156.6. Now remove DepartmentSales based on VIF 3.871755

model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +  
                  JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_19)
vif(model_19)
# AIC 2183.5. Remove DepartmentResearch...Development, which has dropped in significance.

model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBetter + WorkLifeBalanceBest + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_20)
vif(model_20)
# AIC 2181.6, remove WorkLifeBalanceBetter based on VIF.

model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceGood + WorkLifeBalanceBest + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_21)
vif(model_21)
# AIC 2215.2 remove WorkLifeBalanceGood based on significance.

model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  WorkLifeBalanceBest + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_22)
vif(model_22)
# AIC 2213.6, remove WorkLifeBalanceBest, which is now insignificant.

model_23 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManager + JobRoleManufacturing.Director + 
                  JobRoleSales.Representative + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_23)
vif(model_23)
# AIC 2212.1, remove JobRoleSales.Representative based on significance.

model_24 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManager + JobRoleManufacturing.Director + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High + 
                  JobInvolvementHigh , family = "binomial", data = train)
summary(model_24)
vif(model_24)
# AIC 2214.1, remove JobInvolvementHigh based on low significance.

model_25 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManager + JobRoleManufacturing.Director + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High 
                  , family = "binomial", data = train)
summary(model_25)
vif(model_25)
# AIC 2221.4, remove JobRoleManager based on significance.

model_26 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High 
                , family = "binomial", data = train)
summary(model_26)
vif(model_26)
# AIC 2228.2, All remaining variables highly significant and  TotalWorkingYears with VIF <2.347146. Candidate
# model.

# try by removing TotalWorkingYears which has high vif

model_27 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High 
                , family = "binomial", data = train)
summary(model_27)
vif(model_27)
# AIC 2254.3, All remaining variables highly significant and  with VIF <1.581109. Candidate
# for final model there are many variables so which we can validate with anova.
anova(model_26,model_27,test="Chisq")
anova(model_27,test="Chisq")
anova(model_27)
summary(model_27) # AIC: 2254.3 
# removed YearsSinceLastPromotion
model_28 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionMedium + JobSatisfactionHigh + JobSatisfactionVery.High 
                , family = "binomial", data = train)
summary(model_28) # AIC 2278.7
vif(model_28) 

anova(model_28,test="Chisq")
anova(model_28)
# remove JobSatisfactionMedium
model_29 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionHigh + JobSatisfactionVery.High 
                , family = "binomial", data = train)
summary(model_29) # AIC 2287.8
vif(model_29)

anova(model_29,test="Chisq")
anova(model_29)
# remove JobSatisfactionHigh
model_30 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + EnvironmentSatisfactionMedium + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionVery.High 
                , family = "binomial", data = train)
summary(model_30) # AIC: 2297.8
vif(model_30)

anova(model_30,test="Chisq")
anova(model_30)
# remove EnvironmentSatisfactionMedium
model_31 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfactionHigh + EnvironmentSatisfactionVery.High + 
                  JobSatisfactionVery.High 
                , family = "binomial", data = train)
summary(model_31) # AIC: 2322.1
vif(model_31)


anova(model_31,test="Chisq")
anova(model_31)
# remove EnvironmentSatisfactionHigh
model_32 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  mean.hours + BusinessTravelTravel_Frequently +  
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfactionVery.High + 
                  JobSatisfactionVery.High 
                , family = "binomial", data = train)
summary(model_32) # AIC: 2331 all the variables are significant and VIF < 1.237341 this is the candidate model 
                  # to consider as a final model.
vif(model_32)

anova(model_32,test="Chisq")
anova(model_32)
###Model Evuation plots Evaluating discrimination
crPlots(model_32)
par(mfrow=c(2,2))
plot(model_32) ## to plot residual and QQ plot to visualize the model accuracy
dwplot(model_32) ## to plot and visualize the variables $ the co efficient
optiPair(model = model_32, measures = c("Sensitivity", "Specificity"), main = "Optimal balance")
HLfit(model = model_32, bin.method = "n.bins", main = "Hosmer-Lemeshow GOF, N bins")
HLfit(model = model_32, bin.method = "quantiles", main = "Hosmer-Lemeshow GOF, quantiles")
threshMeasures(model = model_32, thresh = "preval", ylim = c(0, 1), main = "Threshold measures")

 # please press any key from the keyboard to get the plot drawn

##***************************************************************************##
#                                                                             #
#                           Model Evaluation                                  #
#                                                                             #
##***************************************************************************##
# Accuracy 
# Sensitivity of a model is the proportion of yeses (or positives) correctly
# predicted by the model as yeses (or positives). 
# Specificity is equal to the proportion of nos (or negatives) correctly 
# predicted by the model as nos (or negatives)
final_model <- model_32

# predicted probabilities of Attrition = 1 for test data
test$prob = predict(final_model, type = "response", 
                    newdata =  dplyr::select(test, -c(Attrition)))

summary(test$prob)

# Let's use the probability cutoff of 50%
test_pred <- factor(ifelse(test$prob >= 0.50, "Yes", "No"))
test_actual <- factor(ifelse(test$Attrition == 1,"Yes","No"))
table(test_pred,test_actual)

test_conf <- confusionMatrix(test_pred, test_actual, positive = "Yes")
(acc <- test_conf$overall[1])
(sens <- test_conf$byClass[1])
(spec <- test_conf$byClass[2])

# Accuracy    : 86%   
# Sensitivity : 16%      
# Specificity : 98%

# Let's use the probability cutoff of 40%
test_pred <- factor(ifelse(test$prob >= 0.40, "Yes", "No"))
test_actual <- factor(ifelse(test$Attrition == 1,"Yes","No"))
table(test_pred,test_actual)
#         test_actual
#test_pred   No  Yes
#No         1075  149
#Yes          45   49

test_conf <- confusionMatrix(test_pred, test_actual, positive = "Yes")
(acc <- test_conf$overall[1])
(sens <- test_conf$byClass[1])
(spec <- test_conf$byClass[2])

# Accuracy    : 85%  
# Sensitivity : 25%
# Specificity : 96%

# Lets create the metrics for variaous cutoff values from 0.01 to 0.80 
# and then plot them. 

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(test$prob >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
summary(test_pred)

# Initiallizing a matrix of 100 X 3 for storing the metrcis for various cutoffs
s = seq(.01,.80,length = 100)
OUT = matrix(0,100,3)
for (i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
### it is giving warning message on refactoring the variables in model 33 i.e 
### okay to ignore the warning becuase no impact on the model statistics.

# Plot the values obtained
plot(s, OUT[,1],xlab = "Cutoff",ylab = "Value",cex.lab = 1.5,cex.axis = 1.5,
     ylim = c(0,1),type = "l",lwd = 2,axes = FALSE,col = 2)
axis(1,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
axis(2,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
lines(s,OUT[,2],col = "darkgreen",lwd = 2)
lines(s,OUT[,3],col = 4,lwd = 2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Identify the cutoff value where all 3 (Sensitivity, Specificity, Accuracy)
#  merge at one point
(cutoff <- s[which(abs(OUT[,1] - OUT[,2]) < 0.01)])
# 0.169596

# Let's use the above cutoff probabality. 
test_pred <- factor(ifelse(test$prob >= cutoff, "Yes", "No"))
test_actual <- factor(ifelse(test$Attrition == 1,"Yes","No"))
table(test_pred,test_actual)

test_conf <- confusionMatrix(test_pred, test_actual, positive = "Yes")
(acc <- test_conf$overall[1])
(sens <- test_conf$byClass[1])
(spec <- test_conf$byClass[2])

## Summary
# Accuracy    : 72%   
# Sensitivity : 72%         
# Specificity : 72%   

##***************************************************************************##
### KS -statistic - Test Data ######

# KS statistic is an indicator of how well the model discriminates between 
# the two classes.
# It is equal to 0% for the random model, and 100% for the perfect model

test_pred_attrition <- ifelse(test_pred == "Yes", 1, 0)
test_actual_attrition <- ifelse(test_actual == "Yes", 1, 0)

pred_object_test <- prediction(test_pred_attrition, test_actual_attrition)
performance_measures_test <- performance(pred_object_test, "tpr", "fpr")

##############################################################################
### Plot Receiver Operating Characteristics (ROC) Curve: AUC calculation ######
##################################################################################
# Lets plot the Area under curve
auc <- performance(pred_object_test,"auc")
unlist(auc@y.values)
# AUC 0.7140963

AUC(model = model_32)
#plot(performance_measures_test,col = "red")
#abline(0,1, lty = 8, col = "grey")
#text(0.8, 0.23, labels=sprintf("AUC: %0.3f", auc@y.values))

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
(max(ks_table_test))

## Summary 
# KS Static - 42%  (KS Static > 40 % indicates a good model)

##***************************************************************************##

# Lift & Gain Chart 

# Gain chart is a popular method to visually inspect model performance 
# in binary prediction. It presents the percentage of captured 
# positive responses as a function of selected percentage of a sample

# Lift basically just tells you the factor by which your model is 
# outperforming a random model, i.e. a model-less situation

lift <- function(labels , predicted_prob,groups=10) {
  if (is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if (is.factor(predicted_prob)) predicted_prob <- 
      as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp = sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain = Cumresp/sum(totalresp)*100,
           Cumlift = Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attr_decile = lift(test_actual_attrition, test_pred_attrition, groups = 10)

ggplot(Attr_decile,aes(x = bucket, y = Gain)) + 
  geom_line() + 
  geom_point(col = 'red2', size = 5) +
  geom_text(aes(label = round(Gain,2)),  
            nudge_x = -0.40, nudge_y = -0.40)

ggplot(Attr_decile,aes(x = bucket, y = Cumlift)) + 
  geom_line() + 
  geom_point(col = 'red2', size = 5) +
  geom_text(aes(label = round(Cumlift,2)), 
            nudge_x = 0.10, nudge_y = 0.10)

# Gain  for model 32 is 72.73% by the 4th decile. 
# Lift is equal to 2.12 by the 3rd decile. 

##***************************************************************************##
#                                                                             #
#                               Summary                                       #
#                                                                             #
##***************************************************************************##

# No. of model iterations = 32. The final model selected is model 32 based on Annova 
# and multicollienarity.                         

# Interpretation of model_32.

summary(model_32) # AIC: 2331,Null deviance: 2758.9 and Residual deviance: 2309.0

# VARIABLE                        COEFFICIENT   INTERPRETATION

# Age                             -0.46173     younger employees are more likely to leave.
# NumCompaniesWorked               0.21871     Employees with many previous companies are more likely to leave.
# TrainingTimesLastYear           -0.23732     Employees taking more training are less likely to leave.
# YearsWithCurrManager            -0.38914     Employees that have been under the same manager for a long period are less likely to leave.
# mean.hours                       0.54652     Employees working more hours are more likely to leave.
# BusinessTravelTravel_Frequently  0.74532     Employees who travel frequently are more likely to leave.
# JobRoleManufacturing.Director   -0.70223     Employees with the job role Manufacturing.Director are less likely to leave.
# MaritalStatusSingle              0.95056     Single employees more likely to leave than married or divorced.
# EnvironmentSatisfactionVery.High -0.55117    Employees with very high environment satisfaction are less likely to leave. 
# JobSatisfactionVery.High         -0.62901    Employees with very.high job satisfaction are less likely to leave.


# Magnitude of the coefficients show that among the above variables, those are with the
# largest effect for attrition.

# Cutoff chosen = 0.169596
# Accuracy    : 72%   
# Sensitivity : 72%         
# Specificity : 72%   

# AUC 0.7140963

# KS Static - 42%  (KS Static > 40% indicates a good model)

#             test_actual
# test_pred     No   Yes
#   No          802  57
#   Yes         318 141

# gain 72.73 at 4th decile
# lift 2.12 %

# Conclusion: 
# The company should focus on the following factors to curb attrition
# 1. Lesser Age 
# 2. Less Number of companies worked
# 3. Less TrainingTimesLastYear
# 4. Less YearsWithCurrManager
# 5. Higher average working hours
# 6. Frequent BusinessTravel 
# 7. Focus on employees with job roles other than with JobRoleManufacturing.Director
# 8. single (Marital Status)
# 9.low Environment Satisfaction
# 10. low Job Satisfaction


