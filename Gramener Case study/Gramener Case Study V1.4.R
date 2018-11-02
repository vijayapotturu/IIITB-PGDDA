################# Gramener Case Study - R Code  ###################

#### Load Required packages and Data set####

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyverse)
# library(RColorBrewer)
library(ggthemes)
library(zipcode)
library(maps)
library(DescTools)

#### Clear environment variables.
rm(list=ls())

#### Data Loading - read the CSV file into R for processing, view the structure and understand data #####

loan_original <- read.csv("loan.csv", stringsAsFactors = FALSE,check.names = FALSE)
loan <- loan_original # Copy for editing

######################################################################################
# DATA CLEANSING                                                         
######################################################################################
#Check for duplicate loan ID or member ID
anyDuplicated(loan$id) # There are no duplicates
anyDuplicated(loan$member_id) # No duplicates

#Check for NA values
summary(loan) 
#There are many columns which has only values NA. 
#There are few columns which has only o and NA and 
#There are few columns with only one unique value across all rows
#Need to remove these columns from analysis and create new subset of data excluding these columns

#Check for NA values in primary columns
sum(is.na(loan)) # there are 2208180 NA Values in the Whole dataset
sum(is.na(loan$id)) # there are no NA's in id which is unique to dataset
sum(is.na(loan$member_id)) # there are no NA's in member_id which is unique to dataset
sum(is.na(loan$loan_status)) # there are no NA's in loan_status

# Create a MetaData of the Data Frame
MetaData<-data.frame(colnames(loan),paste(head(loan),sep = ","),sapply(loan, function(x) class(x)),colSums(is.na(loan)),sapply(loan, function(x) length(unique(x))),sapply(loan, function(x) length(x))-sapply(loan, function(x) length(unique(x))),row.names = NULL)
names(MetaData)<-c("Var_Name","Head","class","NA_Count","Uniques","Duplicates")

write.csv(MetaData,file = "Metadata.csv", na = "")

###from the Metadata,There are 39717 observations with 111 Variables in the Original dataset.there are more than 54 columns which has just NA's.

# checking for blanks,NA values & Missing values
sapply(loan, function(x) length(which(x == ""))) # checking for blank "" values

# There are significant number of rows which has blanks in 
#desc,next_pymnt_d,emp_title,revol,util,last_pymnt_d,last_credit_pull_d columns

#Removing columns with unique values - e.g. Many columns containing only value - NA, 0, 'f', 'n' etc.
loan <- loan[vapply(loan, function(x) length(unique(x)) > 1, logical(1L))]

#Verify values contained in these last 4 columns - 
#collections_12_mths_ex_med ,chargeoff_within_12_mths,pub_rec_bankruptcies, tax_liens                       

sapply(loan, function(x) length(unique(x)))

#From above command, collections_12_mths_ex_med, chargeoff_within_12_mths and tax_liens 
#contains only 2 unique values i.e 0 and NA so these columns can be omitted
loan <- subset(loan, select = -c(collections_12_mths_ex_med, chargeoff_within_12_mths, tax_liens) )

#' Url' and 'desc' columns may not be required for analysis, These can be removed. 
loan <- subset(loan, select = -c(desc, url))

# Some of the unwanted columns are removed, Now need to cleanse remaining columns and remove more unwanted columns in order to use for analysis
# Review remaining columns with NA values. We have no further information about what
# NA means for any columns, so work through each in turn and make a decision about how to handle NA.

# Data Cleaning - Date conversions #
# Update issue_d to date 
loan <- loan %>% mutate(issue_d = paste("01",issue_d,sep="-"),
                        issue_d = parse_date_time(issue_d, orders = c("dmy"), locale = "C"))

# Update earliest_cr_line to R date format.
loan$earliest_cr_line <- parse_date(loan$earliest_cr_line, "%b-%y")

# earliest_cr_line dates before 1969 have incorrectly been converted to 21st century
# instead of 20th century dates. Fix this.
loan$earliest_cr_line <- as.Date(ifelse(loan$earliest_cr_line >= "2009-01-01",
                                        format(loan$earliest_cr_line, "19%y-%m-%d"),
                                        format(loan$earliest_cr_line)))

#Convert last_pymnt_d,next_pymnt_d and last_credit_pull_d to R Date format
loan <- loan %>% mutate(last_pymnt_d = paste("01", last_pymnt_d, sep ="-"),
                        last_pymnt_d = parse_date_time(last_pymnt_d, orders = c("dmy"), locale = "C"),
                        next_pymnt_d = paste("01",next_pymnt_d , sep ="-"),
                        next_pymnt_d = parse_date_time(next_pymnt_d, orders = c("dmy"), locale = "C"),
                        last_credit_pull_d = paste("01", last_credit_pull_d, sep ="-"),
                        last_credit_pull_d = parse_date_time(last_credit_pull_d, orders = c("dmy"), locale = "C"))

###There are warning when we converted to date due to NA's.

# Create Year field from earliest_cr_line.
loan$earliest_cr_line_year <- as.factor(year(loan$earliest_cr_line))

#Remove extra characters from values and conver to numeric
loan$int_rate <-  as.numeric(str_replace_all(loan$int_rate, "[%]", ""))
loan$revol_util <- as.numeric(str_replace_all(loan$revol_util, "[%]", ""))

## Removed the xx from zip code column
loan$zip_code <- loan$zip_code %>% str_replace("xx", "") 

#Columns 'term' contains char "months" which makes it non-numerical, so remove chars and make the column numeric
loan <- loan %>% mutate(term = gsub(" months", "", term),
                        term = gsub(" ", "", term),
                        term = as.numeric(term))

#Check unique values in emp_length column
unique(loan$emp_length)
# 'emp_length' column contains chars like "+", "years", "year", "<". Tidy these.
loan <- loan %>% mutate(emp_length = gsub(" years", "", emp_length),
                        emp_length = gsub(" year", "", emp_length))
loan$emp_length <- if_else(loan$emp_length %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"),  str_pad(loan$emp_length, 2, pad = "0"), loan$emp_length)
#table(loan$emp_length)
################# Type Driven Metrics: Issue Month and year column - issue_dyr (Year) Issue_dm (Month) #################

#Extract the Year and Month data from the issue_d
loan$issue_dyr <- format(loan$issue_d, "%Y")
loan$issue_dmt <- format(loan$issue_d, "%b")
loan$issue_dyr <- as.factor(loan$issue_dyr)
loan$issue_dmt <- as.factor(loan$issue_dmt)

######################################################################################
# DATA CLEANSING - NA values                                                         #
######################################################################################

# title is a title for the loan entered by the borrower. Set NA to empty string.
index <- which(is.na(loan$title))
loan$title[index] <- ""

# revol_util is revolving line utilization rate, or the amount of credit the borrower
# is using relative to all available revolving credit.
filtered <- filter(loan, is.na(revol_util))
sum(filtered$revol_bal == 0)
# 49 of the 50 applicants with NA revol_util have zero total credit revolving balance,
# and so NA is correct in these cases. We do not have sufficient information to
# calculate the correct revol_util for the one remaining record. Instead set it to the
# median of non-NA revol_utils as an approximation.
index <- which(is.na(loan$revol_util) & loan$revol_bal != 0)
loan$revol_util[index] <- median(loan$revol_util, na.rm = TRUE)

# last_credit_pull_d is most recent month LC pulled credit for this loan. Filter to
# look at NAs only.
filtered <- filter(loan, is.na(last_credit_pull_d))
head(filtered)
# Of the two loan applicants, one (id 186499) has one inq_last_6mths. Set
# last_credit_pill_d to the mid-point of this range (3 months before issue_d).
loan$last_credit_pull_d[which(loan$id == 186499)] <- 
  loan$issue_d[which(loan$id == 186499)] %m-% months(3)

# Set the other applicant (id 575712) to the median date of non-NA values.
loan$last_credit_pull_d[which(loan$id == 575712)] <- 
  median(loan$last_credit_pull_d, na.rm = TRUE)

# pub_rec_bankruptcies is number of public record bankruptcies. Replace NAs with
# median figure, which is 0.
loan$pub_rec_bankruptcies[which(is.na(loan$pub_rec_bankruptcies))] <-
  median(loan$pub_rec_bankruptcies, na.rm = TRUE)

######################################################################################
# DATA CLEANSING - outliers                                                          #
######################################################################################

# OUTLIERS - Maximum annual income of $6,000,000 is far greater than the median
# ($59,000) and 3rd quartile ($82,300).

# Check count of incomes >3rd quartile.
quartile3 <- quantile(loan$annual_inc, probs = 0.75, na.rm = TRUE)
filtered <- filter(loan, annual_inc > quartile3)
nrow(filtered)
# Applicants of 9929 loans have an annual income >3rd quartile ($82,300).

boxplot(loan$annual_inc)
quantile(loan$annual_inc,c(1:100)/100)
loan2 <- subset(loan,loan$annual_inc<=quartile3)
boxplot(loan2$annual_inc)

# Outlier high salary does not seem to make much difference, but handling of outliers
# is on the evaluation rubric, so do it anyway. Cap high incomes (above 1.5 * IQR = 
# $145,144) at the value of the 95th centile income ($142,000).
cap95pct <- quantile(loan$annual_inc, probs = 0.95, na.rm = TRUE)
H <- 1.5 * IQR(loan$annual_inc, na.rm = TRUE)
loan$annual_inc[loan$annual_inc > (quartile3 + H)] <- cap95pct

# Data Driven Metric: re-do the quantile buckets for Loan Data to adjust them:
# Annual income range quantile buckets:
groupvec = quantile(loan$annual_inc, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
loan = mutate(loan, annual_inc_range = cut(loan$annual_inc, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))
# loan <- subset(loan, select=-c(annual_inc_bucket))

# DTI quantile buckets:
groupvec = quantile(loan$dti, seq(0,1,0.1))
labels = c(0, prettyNum(groupvec[2:10], big.mark = ","), "+inf")
labels = paste(labels[1:10], labels[2:11], sep = "-")
loan = mutate(loan, dti_bucket = cut(loan$dti, breaks = groupvec, labels = factor(labels), include.lowest=TRUE))

################# Formatting Income #################
loan$income_bin <- round(loan$annual_inc / 10000, digits = 0) * 10000
loan$income_bin <- as.factor(loan$income_bin)
loan$loan_amnt_bin <- round(loan$loan_amnt / 2500, digits = 0) * 2500
loan$dti_bin <- round(loan$dti, digits = 0)
loan$dti_bin <- as.factor(loan$dti_bin)
loan$revol_util_bin <- round(loan$revol_util / 10, digits = 0) * 10
loan$revol_util_bin <- as.factor(loan$revol_util_bin)

# Business Driven Metric: factorise the columns
factorColumns <- c('grade','sub_grade','home_ownership','verification_status','loan_status','purpose',
                   'addr_state','open_acc','pub_rec_bankruptcies','term','emp_length'
                   )
loan[,factorColumns] <- lapply(loan[,factorColumns], as.factor)

##Standardise Numbers - Over-precision in funded_amnt_inv column
IntegerColumns <- c('total_pymnt','total_pymnt_inv','total_rec_prncp','total_rec_int','total_rec_late_fee','funded_amnt_inv','collection_recovery_fee','recoveries')
loan[,IntegerColumns] <- lapply(loan[,IntegerColumns], as.integer)

######################################################################################
# DATA ANALYSIS - addition of data metrics                                           #
######################################################################################

# There are three possible loan scenarios/statuses: fully paid, current, charged-off.
# We are interested in identifying clients who default (charged-off status) so we can
# create an additional column to simplify the three statuses into a defaulted binary.
loan$defaulted <- if_else(loan$loan_status == "Charged Off", TRUE, FALSE)

# From zip_code we can generate a latitude and longitude for plotting data on a map,
# useful to visualise where the highest default rates occur geographically.
data(zipcode)

# We only need the first three digits of the ZIP codes
zipcode$zip <- substr(zipcode$zip, 1, 3)

# Removing the last two digits has left us with duplicate ZIP codes. Just keep the
# first entry of each three digit ZIP.
zipcode = zipcode[!duplicated(zipcode$zip),]
loan <- left_join(loan, zipcode, by=c("zip_code" = "zip"))
loan$zip_code <- as.factor(loan$zip_code) 

########## this data frame is used further for all the plots.
Loan_Final <- loan 
########## Writing file for Tableau representation #################
write.csv(Loan_Final,file = "Loan_Final.csv", na = "")

###################################

######################################################################################
# DATA ANALYSIS - univariate                                                         #
######################################################################################

################# Loan Amount

ggplot(loan,aes(loan_amnt_bin,fill=defaulted))+geom_bar() +
  labs(title = "Loan Amount (Loan Paid Defaulted)", x = "Loan Amount Range", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+ scale_fill_manual("defaulted", values = c("#377EB8", "#E41A1C"))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

# Plot of default rates by loan_amnt.
group <- group_by(loan, loan_amnt_bin)
by_loan_amnt <- summarize(group, Total.Loans = n(),
                          No.Defaulted = sum(defaulted == TRUE),
                          Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0
                          )

by_loan_amt_ref_line <-summarize(by_loan_amnt, 
                        avgy= mean(Pct.Defaulted))      
by_loan_amt_ref_line


ggplot(by_loan_amnt, aes(x = loan_amnt_bin, y = Pct.Defaulted)) +
  geom_bar(stat = "identity",fill="darkblue") + geom_hline(yintercept=by_loan_amt_ref_line$avgy) +
  labs(x = "Loan Amount ($)", y = "Default Rate (%)", title = "Default Rate by Loan Amount") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
              label = paste0(Pct.Defaulted, '%')),    # prettify
          position = position_dodge(width = .9), 
          size = 3)

# Loans > $17,500 have default rate >15%, those for smaller amounts all have <15% defaults.

################# Term ##############

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(term))+geom_bar(fill='#E41A1C')+ 
  labs(title = "Term in Months (Loan Defaulted)", x = "Term In Months", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

### There are more defaulters with 36 months tenure with counts 

### Plot of default rates by term.
group <- group_by(loan, term)
by_term <- summarize(group, Total.Loans = n(),
                     No.Defaulted = sum(defaulted == TRUE),
                     Pct.Defaulted = round(No.Defaulted / Total.Loans * 100,digits =0))

ggplot(by_term, aes(x = term, y = Pct.Defaulted)) + 
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Loan Term (Months)", y = "Default Rate (%)", title = "Default Rate by Loan Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)

# There is a higher (approximately double, 23 % ) default rate on loans with a 60-month term than 36-months.

################# Grade

################# Bar Plot - Grade vs Status #################
h <- ggplot(loan, aes(x=factor(loan$grade), fill = loan$defaulted)) + geom_bar(position = "dodge") 
h +  labs(title = "Grade vs Defaulted", x = "Grade", y = "Count",fill='Loan Defaulted:') + 
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(grade))+geom_bar(fill='#E41A1C')+
  labs(title = "Grade (Loan Defaulted)", x = "Grade", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

###B,C,D grades are more risky for banks as per the counts but this is not true with ratio.

################# Grade Distribution among the data set ################# 
Desc(loan$grade, main="Grade Distribution", plotit = 1)

# Plot of default rates by grade.
group <- group_by(loan, grade)
by_grade <- summarize(group, Total.Loans = n(),
                     No.Defaulted = sum(defaulted == TRUE),
                     Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

ggplot(by_grade, aes(x = grade, y = Pct.Defaulted)) + 
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Loan Grade", y = "Default Rate (%)", title = "Default Rate by Grade") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Default rate increases steadily with grade and is above 30% for grade F and G.

################# Sub Grade Distribution among the data set ################# 
Desc(loan$sub_grade, main="Grade Distribution", plotit = 1)

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(sub_grade))+geom_bar(fill='#E41A1C')+
  labs(title = "Sub Grade (Loan Defaulted)", x = "sub Grade", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

# Plot of default rates by sub_grade.
group <- group_by(loan, sub_grade)
by_sub_grade <- summarize(group, Total.Loans = n(),
                      No.Defaulted = sum(defaulted == TRUE),
                      Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

ggplot(by_sub_grade, aes(x = sub_grade, y = Pct.Defaulted)) + 
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Loan Sub-Grade", y = "Default Rate (%)", title = "Default Rate by Sub-Grade") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)

# Default rate increases fairly steadily with sub-grade. There is some variation,among higher sub grades(with in E ,subgrade E4 >28% ,F -> F4,F5 >28%, G->G2,G3 and G5 >28%).

################# Emp_length ######

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(emp_length))+geom_bar(fill='#E41A1C')+ggtitle('Employee Length (Loan Defaulted)') +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

## <1 years / 10 + emloyee length is Risky to the bank with counts may not true with ratio
Desc(loan$emp_length, main="emp_length Distribution", plotit = 1)
# Plot of default rates by emp_length.
group <- group_by(loan, emp_length)
by_emp_length <- summarize(group, Total.Loans = n(),
                          No.Defaulted = sum(defaulted == TRUE),
                          Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits = 0)

ggplot(by_emp_length, aes(x = emp_length, y = Pct.Defaulted)) + 
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Employment Length (Years)", y = "Default Rate (%)", title = "Default Rate by Employment Length") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Default rate is fairly consistent amongst employed applicants, regardless of length
# of employment. "n/a" (?unemployed) applicants have a higher default rate at ~21%.

################# Home Ownership #############
summary(loan$home_ownership)

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(home_ownership))+geom_bar(fill='#E41A1C')+
  labs(title = "Home Ownership (Loan Defaulted)", x = "home_ownership", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

### Here we can clearly observe Mortage and Rent Home owner ship are defaulting.

################# Loan home ownership distribution among the data set ################# 
Desc(loan$home_ownership, main = "Loan home ownership", plotit = TRUE)

# Plot of default rates by home_ownership. Only three loans had NONE ownership,
# sample size is too small, so filter these.

group <- group_by(loan, home_ownership)
by_home_ownership <- summarize(group, Total.Loans = n(),
                               No.Defaulted = sum(defaulted == TRUE),
                               Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)
filtered <- filter(by_home_ownership, Total.Loans >= 30)

ggplot(filtered, aes(x = home_ownership, y = Pct.Defaulted)) +
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Home Ownership Status", y = "Default Rate (%)",
       title = "Default Rate by Home Ownership Status") + 
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Default rate is slightly higher for applicants with home_ownership of "OTHER".

################# Annual Income #########

################# Annual Income Range distribution among the data set #################
Desc(loan$annual_inc_range, main = "Loan home ownership", plotit = TRUE)

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(income_bin)) + 
  geom_bar(fill='#E41A1C')  + 
  labs(title = "Annual Income (Loan Defaulted)", x = "Income Range", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

# Low income range we see there are more defaulters with counts in the dataset.

# Plot of default rates by annual_inc.
summary(loan$income_bin)

# Filter to only include income_bins with >= 30 loans.
group <- group_by(loan, income_bin)
by_income <- summarize(group, Total.Loans = n(),
                       No.Defaulted = sum(defaulted == TRUE),
                       Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)
filtered <- filter(by_income, Total.Loans >= 30)

ggplot(filtered, aes(x = income_bin, y = Pct.Defaulted)) +
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Annual Income ($)", y = "Default Rate (%)",
       title = "Default Rate by Annual Income") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Annual income <$20,000 has much higher default rate, ~24%. Others all <17.5%

################# Bar Plot - Verification vs Status #################
p <- ggplot(loan, aes(x=loan$verification_status, fill = defaulted)) + geom_bar(position = "dodge") 

p + labs(title ="Verification vs Loan defaulted", x= "Verification Status", y= "Count",fill='Loan Defaulted:') + 
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

################# Verification Status

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(verification_status))+geom_bar(fill='#E41A1C')+
  labs(title = "Verification Status (Loan Defaulted)", x = "verification_status", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

################# verifications status distribution among the data set #################
Desc(loan$verification_status, main = "Loan Verification Status", plotit = TRUE)

# Plot of default rates by verification_status.
group <- group_by(loan, verification_status)
by_verification_status <- summarize(group, Total.Loans = n(),
                       No.Defaulted = sum(defaulted == TRUE),
                       Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

ggplot(by_verification_status, aes(x = verification_status, y = Pct.Defaulted)) +
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Verification Status", y = "Default Rate (%)", title = "Default Rate by Verification Status") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Default rates % is higher for source verified and lower for not verified.

################# verifications status distribution among the data set #################
Desc(loan$issue_dmt, main = "Loan issued month", plotit = TRUE)

# Plot of default rates by issue_d_month.
ggplot(loan, aes(x = issue_dmt, fill = defaulted)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") +
  labs(x = "Issue Month",
       y = "Ratio of Loans", title = "Default Rate by Issue Month") +
  theme_solarized() + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C"))

# No great variation in default rate by issue month.

################# purpose #################

################# Purpose distribution of the Loan Amount #################
Desc(loan$purpose, main = "Purpose Distribution of Loan Amount", plotit = 1)

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(purpose))+geom_bar(fill='#E41A1C')+
  labs(title = "Purpose (Loan Defaulted)", x = "purpose", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

### debt_consolidation purpose is definatly risky defaulter.

# Plot of default rates by purpose.
group <- group_by(loan, purpose)
by_purpose <- summarize(group, Total.Loans = n(),
                       No.Defaulted = sum(defaulted == TRUE),
                       Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

ggplot(by_purpose, aes(x = purpose, y = Pct.Defaulted)) + 
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Loan Purpose", y = "Default Rate (%)", title = "Default Rate by Loan Purpose") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Default rate is particularly high(26%) for small_business.

# Plot of default rates by zip_code
str(loan$zip_code)
# There are 823 different ZIP codes - too many to clearly see on a plot. However the
# above plot does show that there are several zip codes with a default rate >50%.
# Group by ZIP code for further analysis.
group <- group_by(loan, zip_code)
by_zip_code <- summarize(group, Total.Loans = n(),
                         No.Defaulted = sum(defaulted == TRUE),
                         Pct.Defaulted = round(No.Defaulted / Total.Loans * 100,digits =0),
                         No.NotDefaulted = sum(defaulted == FALSE),
                         Pct.NotDefaulted = round(No.NotDefaulted / Total.Loans * 100,digits=0),
                         latitude = mean(latitude), longitude = mean(longitude))

# Filter on Total.Loans >= 30 as a very small sample size in a ZIP code could
# allow just one defaulter to greatly affect the default rate.
filtered <- filter(by_zip_code, Total.Loans >= 30)
# 337 zip codes remain

# Load US State map data
us <- map_data("state")

# Plot map showing default rates in each ZIP code.
ggplot(filtered, aes(longitude, latitude)) +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), color = "gray",
               fill = NA, alpha = 0.35) +
  geom_point(aes(color = Pct.Defaulted), size = 2, alpha = 0.3) +
  xlim(-125, -65) + ylim(20, 50)
# No obvious conclusions to draw from this.

################# Loan addr_state distribution among the data set ################# 
Desc(loan$addr_state, main = "Loan home ownership", plotit = TRUE)

group <- group_by(loan, state)
by_addr_state <- summarize(group, Total.Loans = n(),
                           No.Defaulted = sum(defaulted == TRUE),
                           Pct.Defaulted = round(No.Defaulted / Total.Loans * 100,digits=0),
                           No.NotDefaulted = sum(defaulted == FALSE),
                           Pct.NotDefaulted = No.NotDefaulted / Total.Loans * 100,
                           latitude = mean(latitude), longitude = mean(longitude))

# Filter only states with sufficient number of loan applicants, >30.
filtered <- filter(by_addr_state, Total.Loans >= 30)

ggplot(filtered, aes(x = state, y = Pct.Defaulted)) +
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Address State", y = "Default Rate", title = "Default Rate by State") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Nevada (22%) has a considerably higher default rate than other states

################# DTI

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(dti_bucket))+geom_bar(fill='#E41A1C')+
  labs(title = "DTI (Loan Defaulted)", x = "dti Range", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

# Plot of default rates by dti.
group <- group_by(loan, dti_bin)
by_dti_bin <- summarize(group, Total.Loans = n(),
                        No.Defaulted = sum(defaulted == TRUE),
                        Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

ggplot(by_dti_bin, aes(x = dti_bin, y = Pct.Defaulted)) + 
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "DTI", y = "Default Rate (%)", title = "Default Rate by DTI") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# No clear pattern seen.

################# Delinq_2yr

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(delinq_2yrs))+geom_bar(fill='#E41A1C')+
  labs(title = "Delinq_2yrs (Loan Defaulted)", x = "delinq_2yrs", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

# Plot of default rates by delinq_2yrs.
group <- group_by(loan, delinq_2yrs)
by_delinq_2yrs <- summarize(group, Total.Loans = n(),
                            No.Defaulted = sum(defaulted == TRUE),
                            Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

# Filter only states with sufficient number of loan applicants, >30.
filtered <- filter(by_delinq_2yrs, Total.Loans >= 30)

ggplot(filtered, aes(x = delinq_2yrs, y = Pct.Defaulted)) +
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Delinquincies in Last 2 Years", y = "Default Rate (%)", 
       title = "Default Rate by Number of Delinquincies in Last 2 Years") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# In general there is an increase in default rate as number of delinquincies increases.

# Plot of default rates by earliest_cr_line. Filter to only include years with >= 30
# loans.
group <- group_by(loan, earliest_cr_line_year)
by_earliest_cr_line_year <- summarize(group, Total.Loans = n(),
                                      No.Defaulted = sum(defaulted == TRUE),
                                      Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter only states with sufficient number of loan applicants, >30.
filtered <- filter(by_earliest_cr_line_year, Total.Loans >= 30)

ggplot(filtered, aes(x = earliest_cr_line_year, y = Pct.Defaulted)) +
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Year of Earliest Credit Line", y = "Default Rate (%)", 
       title = "Default Rate by Year of Earliest Credit Line") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Loans with most recent year of earliest credit line (>= 2005) all have >15% default rate.

######### No of inquiries in last 6 months #########

boxplot(loan$inq_last_6mths)
summary(loan$inq_last_6mths)
# Plot of default rates by inq_last_6mths.
group <- group_by(loan, inq_last_6mths)
by_inq_last_6mths <- summarize(group, Total.Loans = n(),
                               No.Defaulted = sum(defaulted == TRUE),
                               Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

# Filter only states with sufficient number of loan applicants, >30.
filtered <- filter(by_inq_last_6mths, Total.Loans >= 30)

ggplot(filtered, aes(x = inq_last_6mths, y = Pct.Defaulted)) +
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Number of inquiries in past 6 months", y = "Default Rate (%)", 
       title = "Default Rate by Number of inquiries in past 6 months") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)

# Default rate increases with number of inquiries. It is well above 20% for >= 6 inquiries.

########### Months since last delinquency #######
boxplot(loan$mths_since_last_delinq)
summary(loan$mths_since_last_delinq)

# Plot of default rates by mths_since_last_delinq.
ggplot(loan, aes(x = mths_since_last_delinq, fill = defaulted)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), position = "fill", binwidth = 20) +
  labs(x = "Months Since Last Delinquincy", y = "Ratio of Loans", 
       title = "Default Rate by Months Since Last Delinquincy") +
  theme_solarized() + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C"))
# Peak at 100.

############# Months since last record ###############
boxplot(loan$mths_since_last_record)
summary(loan$mths_since_last_record)

# Plot of default rates by mths_since_last_record.
ggplot(loan, aes(x = mths_since_last_record, fill = defaulted)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), position = "fill", binwidth = 20) +
  labs(x = "Months Since Last Public Record", y = "Ratio of Loans", 
       title = "Default Rate by Months Since Last Public Record") +
  theme_solarized() + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C"))
# No significant variation.

################# Open_acc #########

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(open_acc))+geom_bar(fill='#E41A1C')+
  labs(title = "Open_acc (Loan Defaulted)", x = "open_acc", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))
# geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

# Plot of default rates by open_acc.

ggplot(loan, aes(x = open_acc, fill = defaulted)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") +
  labs(x = "Number of Open Credit Lines", y = "Ratio of Loans", 
       title = "Default Rate by Number of Open Credit Lines") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C"))


# Slight peaks at 0 and >= 35.

################# Public Record

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(pub_rec))+geom_bar(fill='#E41A1C')+
  labs(title = "Public Record (Loan Defaulted)", x = "pub_rec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)


# Plot of default rates by pub_rec.
ggplot(loan, aes(x = pub_rec, fill = defaulted)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") +
  labs(x = "Number of Derogotory Public Records", y = "Ratio of Loans", 
       title = "Default Rate by Number of Derogotory Public Records") +
  theme_solarized() + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C"))


########## Revolving Balance ###########
boxplot(loan$revol_bal)
summary(loan$revol_bal)

# Plot of default rates by revol_bal.
ggplot(loan, aes(x = revol_bal, fill = defaulted)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), position = "fill", binwidth = 10000) +
  labs(x = "Total Credit Revolving Balance", y = "Ratio of Loans", 
       title = "Default Rate by Total Credit Revolving Balance") +
  theme_solarized() + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C"))
# No very clear pattern.

Desc(loan$revol_util_bin, main = "Loan revol_util_bin", plotit = TRUE)

# Plot of default rates by revol_util.
group <- group_by(loan, revol_util_bin)
by_revol_util_bin <- summarize(group, Total.Loans = n(),
                               No.Defaulted = sum(defaulted == TRUE),
                               Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

ggplot(by_revol_util_bin, aes(x = revol_util_bin, y = Pct.Defaulted)) +
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Revolving Line Utilization Rate", y = "Default Rate (%)", 
       title = "Default Rate by Revolving Line Utilization Rate") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Default rate generally increases with revol_util but is by far the highest for NA,
# which corresponds to loan applicants with no revolving credit balance.

######### Total Acc #########
boxplot(loan$total_acc)
summary(loan$total_acc)
# Plot of default rates by total_acc.
ggplot(loan, aes(x = total_acc, fill = defaulted)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), position = "fill", binwidth = 5) +
  labs(x = "Total Number of Credit Lines", y = "Ratio of Loans", 
       title = "Default Rate by Total Number of Credit Lines") +
  theme_solarized() + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C"))
# Highest peak at 0.

################# Public Record Bankrupcy

ggplot(subset(loan,loan$loan_status=="Charged Off"),aes(pub_rec_bankruptcies))+geom_bar(fill='#E41A1C')+
  labs(title = "Public Record bankruptcies (Loan Defaulted)", x = "pub_rec_bankruptcies", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_dodge(0.9),vjust = -0.3)

# Plot of default rates by pub_rec_bankruptcies.
group <- group_by(loan, pub_rec_bankruptcies)
by_pub_rec_bankruptcies <- summarize(group, Total.Loans = n(),
                               No.Defaulted = sum(defaulted == TRUE),
                               Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

# Filter only groups with >= 30 loans.
filtered <- filter(by_pub_rec_bankruptcies, Total.Loans >= 30)

ggplot(filtered, aes(x = pub_rec_bankruptcies, y = Pct.Defaulted)) +
  geom_bar(stat = "identity",fill="darkblue") +
  labs(x = "Number of Public Record Bankruptcies", y = "Default Rate (%)", 
       title = "Default Rate by Number of Public Record Bankruptcies") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Default rate is higher with previous bankruptcy.pub_rec_bankruptcies of 1 are >(22%) compared to 14% with no previous bankruptcy.


############ Total Payment & payment inverse ########
boxplot(loan$total_pymnt)
summary(loan$total_pymnt)

boxplot(loan$total_pymnt_inv)
summary(loan$total_pymnt_inv)


######################################################################################
# DATA ANALYSIS - bivariate                                                          #
######################################################################################

# Start by looking at combinations of the variables that showed some relationship with
# default rate in univariate analysis.

################# Loan Amount By Term among the data set ################# 
ggplot(loan, aes(loan$term,loan$loan_amnt)) + geom_boxplot(aes(fill = term)) +
  labs(title = "Loan amount by term",  x = "Term in Months", y = "Loan amount" ,fill='Term in Months:') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") 

# Default rate by loan_amount and term
group <- group_by(loan, loan_amnt_bin, term)
by_loan_amnt <- summarize(group, Total.Loans = n(),
                          No.Defaulted = sum(defaulted == TRUE),
                          Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

ggplot(by_loan_amnt, aes(x = loan_amnt_bin, y = Pct.Defaulted, fill = factor(term))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Term (Months)") +
  labs(x = "Loan Amount ($)", y = "Default Rate (%)", title = "Default Rate by Loan Amount and Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
  
# Default rate is higher for 60-month term at all loan amounts. No clear pattern
# within each loan term.

# Effect of grade, grouped by term.
group <- group_by(loan, grade, term)
by_grade <- summarize(group, Total.Loans = n(),
                          No.Defaulted = sum(defaulted == TRUE),
                          Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

ggplot(by_grade, aes(x = grade, y = Pct.Defaulted, fill = factor(term))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Term (Months)") +
  labs(x = "Grade", y = "Default Rate (%)", title = "Default Rate by Grade and Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Default rate increases with grade for each loan-term. The highest default rate of
# all is for grade G at 36-month term. 60-month loans reach a plateau of default rate
# at higher grades.

# Plot of default rates by emp_length, grouped by term.
group <- group_by(loan, emp_length, term)
by_emp_length <- summarize(group, Total.Loans = n(),
                      No.Defaulted = sum(defaulted == TRUE),
                      Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

ggplot(by_emp_length, aes(x = emp_length, y = Pct.Defaulted, fill = factor(term))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Term (Months)") +
  labs(x = "Employment Length", y = "Default Rate (%)", title = "Default Rate by Employment Length and Loan Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Longer term loans more risky across all lengths of employment. n/a maybe Unemployed most risky
# for both loan terms.

# Plot of default rates by home_ownership, grouped by term.
group <- group_by(loan, home_ownership, term)
by_home_ownership <- summarize(group, Total.Loans = n(),
                           No.Defaulted = sum(defaulted == TRUE),
                           Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

# Filter out groups with <30 loans
filtered <- filter(by_home_ownership, Total.Loans >= 30)

ggplot(filtered, aes(x = home_ownership, y = Pct.Defaulted, fill = factor(term))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Term (Months)") +
  labs(x = "Home Ownership", y = "Default Rate (%)", title = "Default Rate by Home Ownership and Loan Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Default rates are highest for 60-month loans across all home ownerships (but small
# sample size for OTHER status). Highest default rates are for owners and renters
# taking 60 month loans (25% rate)

# Plot of default rates by annual_inc, grouped by term.
group <- group_by(loan, income_bin, term)
by_income_bin <- summarize(group, Total.Loans = n(),
                          No.Defaulted = sum(defaulted == TRUE),
                          Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

# Filter out groups with <30 loans
filtered <- filter(by_income_bin, Total.Loans >= 30)

ggplot(filtered, aes(x = income_bin, y = Pct.Defaulted, fill = factor(term))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Term (Months)") +
  labs(x = "Annual Income ($)", y = "Default Rate (%)", title = "Default Rate by Annual Income and Loan Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# For both loan terms, lower incomes are more risky. 60-month term has higher default
# rate for all incomes. 60-month loans to applicants with annual income <$50,000
# have >25% default rate. 35% default rate for incomes <$10,000.

################# Jitter Plot - "Annual Income vs Loan Defaulter" #################
ggplot(loan, aes(x=loan$defaulted, y= loan$annual_inc)) + 
  geom_jitter(position = position_jitter(width = 0.5)) +
  labs(title = "Loan defaulted Vs Annual Income",
       x = "Loan defaulted",
       y = "Annual Income")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")

###Here we can clearly observer no of observation are high >25000 and <75000 annual income

# Plot of default rates by verification_status, grouped by term.
group <- group_by(loan, verification_status, term)
by_verification_status <- summarize(group, Total.Loans = n(),
                           No.Defaulted = sum(defaulted == TRUE),
                           Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

ggplot(by_verification_status, aes(x = verification_status, y = Pct.Defaulted, fill = factor(term))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Term (Months)") +
  labs(x = "Verification Status", y = "Default Rate (%)", title = "Default Rate by Verification Status and Loan Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Higher default rate at 60-months across all statuses. Verification status only
# seems to matter for 60-term loans.

# Plot of default rates by purpose and term.
group <- group_by(loan, purpose, term)
by_purpose <- summarize(group, Total.Loans = n(),
                                    No.Defaulted = sum(defaulted == TRUE),
                                    Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

# Filter out groups with <30 loans
filtered <- filter(by_purpose, Total.Loans >= 30)

ggplot(filtered, aes(x = purpose, y = Pct.Defaulted, fill = factor(term))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Term (Months)") +
  labs(x = "Loan Purpose", y = "Default Rate (%)", title = "Default Rate by Loan Purpose and Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Longer term loans are generally more risky, particularly for educational and small
# business

# Plot of default rates by addr_state and term.
group <- group_by(loan, addr_state, term)
by_addr_state <- summarize(group, Total.Loans = n(),
                        No.Defaulted = sum(defaulted == TRUE),
                        Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

# Filter out groups with <30 loans
filtered <- filter(by_addr_state, Total.Loans >= 30)

ggplot(filtered, aes(x = addr_state, y = Pct.Defaulted, fill = factor(term))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Term (Months)") +
  labs(x = "Loan Purpose", y = "Default Rate (%)", title = "Default Rate by Address State and Loan Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# 60-month loans all generally have higer default rate, no clear pattern between
# states.

# Plot of default rates by inq_last_6mths, grouped by term.
ggplot(loan, aes(x = inq_last_6mths, fill = defaulted)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") +
  labs(x = "Number of inquiries in past 6 months", y = "Ratio of Loans", 
       title = "Default Rate by Number of Inquiries in Past 6 Months") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C")) +
  facet_wrap(~term)
# No real difference between loan terms.

# Plot of default rates by mths_since_last_delinq, grouped by term.
ggplot(loan, aes(x = mths_since_last_delinq, fill = defaulted)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), position = "fill", binwidth = 20) +
  labs(x = "Months Since Last Delinquincy", y = "Ratio of Loans", 
       title = "Default Rate by Months Since Last Delinquincy and Loan Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C")) +
  facet_wrap(~term)
# Default rates both peak at 100 months, 60-month term more risky at all counts of  months.

# Plot of default rates by revol_util, grouped by term.
ggplot(loan, aes(x = revol_util, fill = defaulted)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), position = "fill", binwidth = 20) +
  labs(x = "Revolving Line Utilization Rate", y = "Ratio of Loans", 
       title = "Default Rate by Revolving Line Utilization Rate and Loan Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C")) +
  facet_wrap(~term)
# Default rate steadily increases with revol_util with another peak at 0%. 60-month term more risky for all utilisations.

# Plot of default rates by total_acc, grouped by term.
ggplot(loan, aes(x = total_acc, fill = defaulted)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), position = "fill", binwidth = 5) +
  labs(x = "Total Number of Credit Lines", y = "Ratio of Loans", 
       title = "Default Rate by Total Number of Credit Lines and Loan Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C")) +
  facet_wrap(~term)

# Plot of default rates by pub_rec_bankruptcies, grouped by term.
group <- group_by(loan, pub_rec_bankruptcies, term)
by_pub_rec_bankruptcies <- summarize(group, Total.Loans = n(),
                           No.Defaulted = sum(defaulted == TRUE),
                           Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

# Filter out groups with <30 loans
filtered <- filter(by_pub_rec_bankruptcies, Total.Loans >= 30)

ggplot(filtered, aes(x = pub_rec_bankruptcies, y = Pct.Defaulted, fill = factor(term))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Term (Months)") +
  labs(x = "Number of Public Record Bankruptcies", y = "Default Rate (%)", title = "Default Rate by Number of Public Record Bankruptcies and Loan Term") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# 35% default rate for 60-month loans to applicants with previous bankruptcies.

################

# Try grouping by home_ownership.

# Effect of loan_amnt, grouped by home_ownership.
group <- group_by(loan, loan_amnt_bin, home_ownership)
by_home_ownership <- summarize(group, Total.Loans = n(),
                                     No.Defaulted = sum(defaulted == TRUE),
                                     Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

# Filter out groups with <30 loans
filtered <- filter(by_home_ownership, Total.Loans >= 30)

ggplot(filtered, aes(x = loan_amnt_bin, y = Pct.Defaulted, fill = factor(home_ownership))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Home Ownership") +
  labs(x = "Loan Amount ($)", y = "Default Rate (%)", title = "Default Rate by Loan Amount and Home Ownership") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)

# Effect of grade, grouped by home_ownership.
group <- group_by(loan, grade, home_ownership)
by_grade <- summarize(group, Total.Loans = n(),
                               No.Defaulted = sum(defaulted == TRUE),
                               Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_grade, Total.Loans >= 30)

ggplot(filtered, aes(x = grade, y = Pct.Defaulted, fill = factor(home_ownership))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "home_ownership") +
  labs(x = "Grade", y = "Default Rate (%)", title = "Default Rate by Grade and Home Ownership") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Rick increases with grade but not much variation between home ownership.

# Plot of default rates by emp_length, grouped by home_ownership.
group <- group_by(loan, emp_length, home_ownership)
by_emp_length <- summarize(group, Total.Loans = n(),
                      No.Defaulted = sum(defaulted == TRUE),
                      Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

# Filter out groups with <30 loans
filtered <- filter(by_emp_length, Total.Loans >= 30)

ggplot(filtered, aes(x = emp_length, y = Pct.Defaulted, fill = factor(home_ownership))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "home_ownership") +
  labs(x = "Employment Length", y = "Default Rate (%)", title = "Default Rate by Employment Length and Home Ownership") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Peak for n/a employment length, no variation with home ownership

# Plot of default rates by annual_inc, grouped by home_ownership.
group <- group_by(loan, income_bin, home_ownership)
by_income_bin <- summarize(group, Total.Loans = n(),
                           No.Defaulted = sum(defaulted == TRUE),
                           Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_income_bin, Total.Loans >= 30)

ggplot(filtered, aes(x = income_bin, y = Pct.Defaulted, fill = factor(home_ownership))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "home_ownership") +
  labs(x = "Annual Income", y = "Default Rate (%)", title = "Default Rate by Annual Income and Home Ownership") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Low income (<$10,000) renters stand out with a default rate of 26%

# Plot of default rates by verification_status, grouped by home_ownership
group <- group_by(loan, verification_status, home_ownership)
by_verification_status <- summarize(group, Total.Loans = n(),
                           No.Defaulted = sum(defaulted == TRUE),
                           Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_verification_status, Total.Loans >= 30)

ggplot(filtered, aes(x = verification_status, y = Pct.Defaulted, fill = factor(home_ownership))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "home_ownership") +
  labs(x = "Verification Status", y = "Default Rate (%)", title = "Default Rate by Verification Status and Home Ownership") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)

# Plot of default rates by purpose, grouped by home_ownership
group <- group_by(loan, purpose, home_ownership)
by_purpose <- summarize(group, Total.Loans = n(),
                                    No.Defaulted = sum(defaulted == TRUE),
                                    Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits= 0)

# Filter out groups with <30 loans
filtered <- filter(by_purpose, Total.Loans >= 30)

ggplot(filtered, aes(x = purpose, y = Pct.Defaulted, fill = factor(home_ownership))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "home_ownership") +
  labs(x = "Loan Purpose", y = "Default Rate (%)", title = "Default Rate by Loan Purpose and Home Ownership") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Small business purpose is most risky, particularly for owners (32%) and renters (29%).

# Plot of default rates by inq_last_6mths, grouped by home_ownership
group <- group_by(loan, inq_last_6mths, home_ownership)
by_inq_last_6mths <- summarize(group, Total.Loans = n(),
                        No.Defaulted = sum(defaulted == TRUE),
                        Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_inq_last_6mths, Total.Loans >= 30)

ggplot(filtered, aes(x = inq_last_6mths, y = Pct.Defaulted, fill = factor(home_ownership))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "home_ownership") +
  labs(x = "Number of Inquiries in Past 6 Months", y = "Default Rate (%)", title = "Default Rate by Number of Inquiries in Past 6 Months and Home Ownership") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)

# Plot of default rates by revol_util, grouped by home_ownership
group <- group_by(loan, revol_util_bin, home_ownership)
by_revol_util_bin <- summarize(group, Total.Loans = n(),
                               No.Defaulted = sum(defaulted == TRUE),
                               Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_revol_util_bin, Total.Loans >= 30)

ggplot(filtered, aes(x = revol_util_bin, y = Pct.Defaulted, fill = factor(home_ownership))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "home_ownership") +
  labs(x = "Revolving Line Utilization Rate", y = "Default Rate (%)", title = "Default Rate by Revolving Line Utilization Rate and Home Ownership") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# No real variation with home ownership.

# Plot of default rates by pub_rec_bankruptcies, grouped by home_ownership.
group <- group_by(loan, pub_rec_bankruptcies, home_ownership)
by_pub_rec_bankruptcies <- summarize(group, Total.Loans = n(),
                               No.Defaulted = sum(defaulted == TRUE),
                               Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits =0)

# Filter out groups with <30 loans
filtered <- filter(by_pub_rec_bankruptcies, Total.Loans >= 30)

ggplot(filtered, aes(x = pub_rec_bankruptcies, y = Pct.Defaulted, fill = factor(home_ownership))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "home_ownership") +
  labs(x = "Number of Public Record Bankruptcies", y = "Default Rate (%)", title = "Default Rate by Number of Public Record Bankruptcies and Home Ownership") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Higher rate with previous bankruptcies, no real variation with home ownership.

################

# Try grouping by grade.

# Effect of loan_amnt, grouped by grade.
ggplot(loan, aes(x = loan_amnt, fill = defaulted)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), position = "fill",
                 binwidth = 5000) +
  labs(x = "Loan Amount", y = "Ratio of Loans", title = "Default Rate by Loan Amount and Grade") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C")) +
  facet_wrap(~grade)
# In general, increase in default rate across all loan amounts as grade increases.

# Plot of default rates by emp_length, grouped by grade
ggplot(loan, aes(x = emp_length, fill = defaulted)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") +
  labs(x = "Employment Length", y = "Ratio of Loans",
       title = "Default Rate by Employment Length and Grade") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C")) +
  facet_wrap(~grade)

# Plot of default rates by annual_inc, grouped by grade.
group <- group_by(loan, income_bin, grade)
by_income_bin <- summarize(group, Total.Loans = n(),
                                     No.Defaulted = sum(defaulted == TRUE),
                                     Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_income_bin, Total.Loans >= 30)

ggplot(filtered, aes(x = income_bin, y = Pct.Defaulted, fill = factor(grade))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "grade") +
  labs(x = "Annual Income ($)", y = "Default Rate (%)", title = "Default Rate by Annual Income and Grade") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)

# Plot of default rates by verification_status, grouped by grade.
ggplot(loan, aes(x = verification_status, fill = defaulted)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") +
  labs(x = "Verification Status",
       y = "Ratio of Loans", title = "Default Rate by Verification Status and Grade") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") + scale_fill_manual("Defaulted", values = c("#377EB8", "#E41A1C")) +
  facet_wrap(~grade)
# Default rate is lowest for lower grades only.

################# Interest Rate by Grade among the data set #################
ggplot(loan, aes(loan$grade,loan$int_rate)) + geom_boxplot(aes(fill = grade)) +
  labs(title = "Interest rate by grade", x = "Grade",   y = "Interest rate",fill='Grade:') +
  theme(plot.title = element_text(hjust = 0.5))
## the higher the grade (more risky loan), the higher the interest rates.

################# Annual Income by Grade among the data set #################

ggplot(loan, aes(loan$grade,loan$annual_inc)) + geom_boxplot(aes(fill = grade)) +
  labs(title = "Annual Income by grade", x = "Grade",   y = "Annual Income",fill='Grade:') +
  theme(plot.title = element_text(hjust = 0.5))

################# Loan Issued per Month #################

ggplot(loan, aes(loan$issue_dmt, fill = loan$defaulted)) + 
  geom_histogram(stat="count", bandwidth = 10) + 
  labs(title = "Issue Month and frequency",  x = "Month Numbers", y = "Count",fill='Defaulted:') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y=(..count..),label=(..count..)),size=3,stat='count',position = position_stack(vjust = 0.5))

################# Group by pub_rec_bankruptcies 

# Plot of annual_income grouped by pub_rec_bankruptcies 
group <- group_by(loan, income_bin, pub_rec_bankruptcies)
by_income_bin <- summarize(group, Total.Loans = n(),
                           No.Defaulted = sum(defaulted == TRUE),
                           Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_income_bin, Total.Loans >= 30)

ggplot(filtered, aes(x = income_bin, y = Pct.Defaulted, fill = factor(pub_rec_bankruptcies))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "pub_rec_bankruptcies") +
  labs(x = "Annual Income ($)", y = "Default Rate (%)", title = "Default Rate by Annual Income and Public Record Bankruptcies") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# For applicants with no previous bankruptcy, default rate decreases with increasing
# income. Applicants with previous bankruptcy do not have such an obvious variation with income.

# Plot of loan_amount grouped by pub_rec_bankruptcies
group <- group_by(loan, loan_amnt_bin, pub_rec_bankruptcies)
by_loan_amnt_bin <- summarize(group, Total.Loans = n(),
                           No.Defaulted = sum(defaulted == TRUE),
                           Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_loan_amnt_bin, Total.Loans >= 30)

ggplot(filtered, aes(x = loan_amnt_bin, y = Pct.Defaulted, fill = factor(pub_rec_bankruptcies))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "pub_rec_bankruptcies") +
  labs(x = "Loan Amount ($)", y = "Default Rate (%)", title = "Default Rate by Loan Amount and Public Record Bankruptcies") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Highest default rates (> 25%) are for higher value loans (>= $12,500) to applicants with previous bankruptcy.

# Plot of emp_length by pub_rec_bankruptcies
group <- group_by(loan, emp_length, pub_rec_bankruptcies)
by_emp_length <- summarize(group, Total.Loans = n(),
                              No.Defaulted = sum(defaulted == TRUE),
                              Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_emp_length, Total.Loans >= 30)

ggplot(filtered, aes(x = emp_length, y = Pct.Defaulted, fill = factor(pub_rec_bankruptcies))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "pub_rec_bankruptcies") +
  labs(x = "Employment Length", y = "Default Rate (%)", title = "Default Rate by Employment Length and Public Record Bankruptcies") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)

# Plot of verification_status by pub_rec_bankruptcies
group <- group_by(loan, verification_status, pub_rec_bankruptcies)
by_verification_status <- summarize(group, Total.Loans = n(),
                           No.Defaulted = sum(defaulted == TRUE),
                           Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_verification_status, Total.Loans >= 30)
ggplot(filtered, aes(x = verification_status, y = Pct.Defaulted, fill = factor(pub_rec_bankruptcies))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "pub_rec_bankruptcies") +
  labs(x = "Verification Status", y = "Default Rate (%)", title = "Default Rate by Verification Status and Public Record Bankruptcies") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Default rates highest for previous bankrupt and verified.

# Plot of purpose by pub_rec_bankruptcies
group <- group_by(loan, purpose, pub_rec_bankruptcies)
by_purpose <- summarize(group, Total.Loans = n(),
                                    No.Defaulted = sum(defaulted == TRUE),
                                    Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_purpose, Total.Loans >= 30)

ggplot(filtered, aes(x = purpose, y = Pct.Defaulted, fill = factor(pub_rec_bankruptcies))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "pub_rec_bankruptcies") +
  labs(x = "Loan Purpose", y = "Default Rate (%)", title = "Default Rate by Loan Purpose and Public Record Bankruptcies") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Small business loans with a previous bankruptcy have a 33% default rate but there were only 81 loans.

# Plot of revol_util grouped by pub_rec_bankruptcies
group <- group_by(loan, revol_util_bin, pub_rec_bankruptcies)
by_revol_util_bin <- summarize(group, Total.Loans = n(),
                        No.Defaulted = sum(defaulted == TRUE),
                        Pct.Defaulted = round(No.Defaulted / Total.Loans * 100),digits=0)

# Filter out groups with <30 loans
filtered <- filter(by_revol_util_bin, Total.Loans >= 30)

ggplot(filtered, aes(x = revol_util_bin, y = Pct.Defaulted, fill = factor(pub_rec_bankruptcies))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "pub_rec_bankruptcies") +
  labs(x = "Revolving Line Utilization Rate (%)", y = "Default Rate (%)", title = "Default Rate by Revolving Line Utilization Rate and Public Record Bankruptcies") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")+
  geom_text(aes(y = Pct.Defaulted + .5,    # nudge above top of bar
                label = paste0(Pct.Defaulted, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
# Default rate increases with revol_util, highest for NA (?no revolving credit).

################### loans of different grades was changing over time ######################

amnt_df_grade <- loan %>% 
  select(issue_dyr, loan_amnt, grade) %>% 
  group_by(issue_dyr, grade) %>% 
  summarise(Amount = sum(loan_amnt))

ts_amnt_grade <- ggplot(amnt_df_grade, 
                        aes(x = issue_dyr, y = Amount))
ts_amnt_grade + geom_col(aes(x=issue_dyr,y=Amount,fill=grade)) + xlab("Date issued")

amnt_df_grade1 <- loan %>% 
  select(issue_dmt, loan_amnt, grade) %>% 
  group_by(issue_dmt, grade) %>% 
  summarise(Amount = sum(loan_amnt))

ts_amnt_grade1 <- ggplot(amnt_df_grade1, 
                         aes(x = issue_dmt, y = Amount))
ts_amnt_grade1 + geom_col(aes(x=issue_dmt,y=Amount,fill=grade)) + xlab("Date issued")



