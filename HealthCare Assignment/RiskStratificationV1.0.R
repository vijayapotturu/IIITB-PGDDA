########################################################
#############Assignment - Risk Stratification  ###########
########################################################

# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
# 5. Hyperparameter tuning and cross validation

############################################################

### Business Understanding:
# A care management organisation called WeCare wants to identify among its diabetic patients,
# the ones that are at high risk of getting re-admitted to the hospital.
#They wish to intervene by providing some incentive to these patients that will help them improve their health.

## AIM:

# to identify high-risk diabetic patients through risk stratification. 
# This will help the payer to decide what are the right intervention programs for these patients.

###So what is Risk stratification.????
###Risk stratification is the foundational step for targeting patients at various levels of risks, 
#and further, scheduling follow-ups and keeping them aligned with their care plans. 
#Here's why risk stratification is important:

#Predict risks: Risk stratification can help providers to proactively identify patients at risk of unplanned hospital admissions. Almost one-third of all the readmissions that take place in the United States are preventable.
#Patient-specific care plans: Identifying patient-specific risk factors that may pose a threat in future can help physicians and health coaches develop care plans tailored to their needs.
#Understanding population health trends: With a continuous assessment of risk factors and the use of risk scores, providers can understand their patient population and find answers to critical questions.

##############################################################

#Loading Neccessary libraries

# Check libraries and install and load required libraries
options(warn = -1)
libs = c("readr","plyr","dplyr","tidyverse","stringr","scales","GGally","DescTools",
         "ggthemes","ggplot2","gridExtra","corrplot","cowplot","formattable","splitstackshape",
         "caret", "kernlab","caTools","doParallel","car","MASS","dotwhisker",
         "modEvA","e1071","AUC","glmnet",
         "rpart","rpart.plot","caret","RColorBrewer","rattle","randomForest","nnet")
install.lib <- libs[!libs %in% installed.packages()]
for (pkg in install.lib)
  install.packages(pkg, dependencies = T)

loadlib     <- lapply(libs, library, character.only = T) # load them

remove(list = ls())
options(warn = 0)

### Set directory to load file -- setwd("E:Upgrad/Course 6/")

### Data Loading - read the CSV file into R, for processing. View the structure and understand data

## Importing the data from source file
diabetic<- read.csv("diabetic_data.csv",stringsAsFactors=FALSE)
#####################################################################################
# Task 0:.Data understanding                                                        #
#####################################################################################
### In this step basically trying to understand every column from the dataset
##View the structure of train and test dataframes

str(diabetic)
glimpse(diabetic)
dim(diabetic)
summary(diabetic)
#diabetic data set has 101766 Observations with 50 variables.

#printing and checking first few rows
head(diabetic[ ,1:10])

#checking class of all variables in dataframe
lapply(diabetic, class)

#Checking for missing values
anyNA(diabetic) # No NA's
sum(is.na(diabetic)) # no missing values.

# Check which columns have the NA values.
# colSums(is.na(diabetic))[which(colSums(is.na(diabetic)) > 0)] 
# Lets check the % of NA values
# diabetic %>%
#   summarise_all(funs(sum(is.na(.))/n()))

#Check blank values
sapply(diabetic, function(x) length(which(x == ""))) # No blank values in dataset

# check the duplicate values in the dataset on imp columns
anyDuplicated(diabetic$encounter_id)         # No duplicate encounter_id.
anyDuplicated(diabetic$patient_nbr)  # 80 duplciates with patient number but this can be revisits
sum(duplicated(diabetic)) # No duplicates in diabetic dataset

#Check if column has same value repeated for all the rows
vapply(diabetic, function(x) length(unique(x)) > 1, logical(1L)) 

# From above commands its clear that examide and citoglipton contains unique single value
# Check values of these columns if it can be removed.
# The following columns each contain a single value, so per data dictionary examide is is part of 24 features for medications but it has a single values may not very for modelling.
unique(diabetic$examide)
unique(diabetic$citoglipton)

#Remove the columns detailed above.
diabetic <- diabetic[vapply(diabetic, function(x) length(unique(x)) > 1, logical(1L))]
dim(diabetic) # 101766 rows and 48 columns
str(diabetic)

# Check unique values by each column from the dataset
sapply(diabetic, function(x) length(unique(x)))

# Lets's check distinct values by some imp categotical columns.

catColumns <- c("race","gender","age","weight","payer_code","medical_specialty",     
                   "max_glu_serum","A1Cresult","change","diabetesMed","readmitted")

sapply(diabetic[,catColumns], function(x) unique(x))
sapply(diabetic[,catColumns], function(x) table(x))

#Lets verify what are the unique values by column wise on above columns if it matches with data dictionary.

table(diabetic$race) # as per dictionary values are Caucasian, Asian, African American, Hispanic, and other so "?"is extra we have to handle this.
table(diabetic$gender) # as per the dictionary there are male, female, and unknown/invalid it is matching

length(unique(diabetic$age))# as per the dictionary there are 10 intervals it is matching and replacing ")" with "]"
table(diabetic$age)
diabetic$age <- str_replace_all(diabetic$age, "[)]", "]" )
unique(diabetic$age)

length(unique(diabetic$weight)) 
unique(diabetic$weight) # there are many records with "?" and replacing ")" with "]"
table(diabetic$weight)
diabetic$weight <- str_replace_all(diabetic$weight, "[)]", "]" )
table(diabetic$weight)
sum(is.na(diabetic$weight)) # no missing values.


length(unique(diabetic$admission_type_id) ) # as per the dictionary there are values like 9 distinct values, for example, emergency, urgent, elective, newborn, and not available but dataset has only 8 values.
table(diabetic$admission_type_id)

length(unique(diabetic$discharge_disposition_id))# as per the dictionary there are values like 29 distinct values, but dataset contains only 26 unique values.
table(diabetic$discharge_disposition_id)

length(unique(diabetic$admission_source_id)) # as per the dictionary there are values like 21 distinct values, but dataset contains only 17 unique values.
table(diabetic$admission_source_id)

unique(diabetic$time_in_hospital) # as per the dictionary it should contain integers and it is matching with dataset.
table(diabetic$time_in_hospital)

length(unique(diabetic$payer_code))# as per the dictionary there are values like 23 distinct values, but dataset contains only 18 with "?" unique values.
table(diabetic$payer_code)

length(unique(diabetic$medical_specialty))# as per the dictionary there are values like 84 distinct values, but dataset contains only 73 distinct records with "?" unique values.
table(diabetic$medical_specialty)


length(unique(diabetic$num_lab_procedures))# as per the dictionary there are values like 84 distinct values, but dataset contains only 73 distinct records with "?" unique values.
table(diabetic$num_lab_procedures)

length(unique(diabetic$num_procedures))# as per the dictionary there are values like 7 distinct values
table(diabetic$num_procedures)

length(unique(diabetic$num_medications))# as per the dictionary there are values like 75 distinct values
table(diabetic$num_medications)

length(unique(diabetic$number_outpatient))# as per the dictionary there are values like 39 distinct values
table(diabetic$number_outpatient)

length(unique(diabetic$number_emergency))# as per the dictionary there are values like 33 distinct values.
table(diabetic$number_emergency)

length(unique(diabetic$number_inpatient))# as per the dictionary there are values like 21 distinct values
table(diabetic$number_inpatient)

length(unique(diabetic$diag_1))   # as per the dictionary there are values like 848 distinct values, but dataset contains only 717 distinct values and it has some data issues.
table(diabetic$diag_1)

length(unique(diabetic$diag_2))   # as per the dictionary there are values like 923 distinct values, but dataset contains only 749 distinct values and it has some data issues.
table(diabetic$diag_2)

length(unique(diabetic$diag_3))   # as per the dictionary there are values like 954 distinct values, but dataset contains only 790 distinct values and it has some data issues
table(diabetic$diag_3)

length(unique(diabetic$number_diagnoses))   
table(diabetic$number_diagnoses)

length(unique(diabetic$change))   # matching with data dictionary
table(diabetic$change)

length(unique(diabetic$diabetesMed))   # matching with data dictionary
table(diabetic$diabetesMed)

length(unique(diabetic$readmitted))   # matching with data dictionary
table(diabetic$readmitted)

length(unique(diabetic$max_glu_serum)) # matching with data dictionary 
table(diabetic$max_glu_serum) # it has "None" ">300" "Norm" ">200"

length(unique(diabetic$A1Cresult))    # matching with data dictionary
table(diabetic$A1Cresult) #  it has "None" ">7"   ">8"   "Norm"

featuresmedcolumns <- c("max_glu_serum","metformin","repaglinide",  
                   "nateglinide", "chlorpropamide" ,"glimepiride","acetohexamide",        
                   "glipizide","glyburide","tolbutamide", "pioglitazone","rosiglitazone","acarbose",                
                   "miglitol","troglitazone","tolazamide","glyburide.metformin",
                   "glipizide.metformin","glimepiride.pioglitazone","metformin.rosiglitazone",
                   "metformin.pioglitazone")

sapply(diabetic[,featuresmedcolumns], function(x) table(x))

#####################################################################################
#Task 1: Data preparation                                                              #
#####################################################################################
str(diabetic)

#1.Remove redundant variables.

# citoglipton and examide are removed in the above steps.
# from the above data understanding there are columns like weight,payer_code, medical_specialty have 
# large number of unknown(?) values so these will be dropped in the next step.


# removing all feature related columns anyway we will use diabeticmed column for modelling

diabetic <- diabetic[,!(colnames(diabetic) %in% featuresmedcolumns)]

# removed max_glu_Serum also along wit other feature columns may not be so important and included insulin in the data set 
# as explained by SME during the course.

dim(diabetic) # 101766  rows 27 columns
colnames((diabetic))

#2.Check for missing values and treat them accordingly.

#Converting '?' to NA's and Unknown/Invalid
diabetic[diabetic== '?'] <- NA
diabetic[diabetic == 'Unknown/Invalid'] <- NA

sum(is.na(diabetic)) # 192852 missing values.

# Treating invalid values, any variable having more than 15% of data points
# missing is not eligible for imputation hence it makes sense to compute
# and drop those variables

missing_values <- diabetic %>%
  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()

# there are some reduntant features 
# finding out relevant/good features where the missing % < 15%

good_features <- filter(missing_values,missing_percentage<0.15)

good_features <- (good_features$feature) #the reason for deleting remaining features
# we cannot impute missing values with more than 15% obs

# Removing all the columns which have redundant information. 
diabetic <- diabetic[,(colnames(diabetic) %in% good_features)]

# Let's summarise the data
colnames(diabetic) # Weight,payer_code, medical_specialty columns are dropped
dim(diabetic) # 101766     24 columns
sum(is.na(diabetic)) # 4078 missing values.

# We have missing values in diag_1,diag_2,diag_3 which we cannt drop so lets understand and impute with mode

diabetic %>% summarise_all(funs(sum(is.na(.))/n()))
#Check which column has NA values
colSums(is.na(diabetic))
#Column names with missing Values
names(diabetic)[colSums(is.na(diabetic)) > 0]

# Handling missing values "?" in race columns these records are assuming as other records and clubbed with other.
diabetic$race[which(is.na(diabetic$race))] <- "Other"
table(diabetic$race)
sum(is.na(diabetic)) # 1805 missing values.

(genmode <- names(which.max(table(diabetic$gender,useNA="no"))))
# It means that 1805/101766 = 0.017736769 i.e 0.1%, best is to remove these observations from the analysis
#diabetic <- diabetic[!is.na(diabetic$diag_1),]
#diabetic <- diabetic[!is.na(diabetic$diag_2),]
#diabetic <- diabetic[!is.na(diabetic$diag_3),]

diabetic$gender[which(is.na(diabetic$gender))] <- genmode

str(diabetic)
names(diabetic)[colSums(is.na(diabetic)) > 0] # NA's in "diag_1" "diag_2" "diag_3" will be handled during comorbidity derivided metric.
sum(is.na(diabetic)) # 1802 rows with NA's

diabetic_bkp <- diabetic
#str(diabetic_bkp)

##################################################################################################
# Deriving metrics for the analysis
##################################################################################################
#5.Create the derived metric 'comorbidity', according to the following scheme 

diabetic <-  diabetic %>%
  mutate(Diagnosis_1 = case_when((diag_1 >= 390 & diag_1 <= 459)  ~ 'Circulatory_Disease',
                                 startsWith(diag_1,'250') ~ 'Diabetes',
                                 TRUE ~ 'Others'))
diabetic <-  diabetic %>%
  mutate(Diagnosis_2 = case_when((diag_2 >= 390 & diag_2 <= 459)  ~ 'Circulatory_Disease',
                                 startsWith(diag_2,'250') ~ 'Diabetes',
                                 TRUE ~ 'Others'))

diabetic <-  diabetic %>%
  mutate(Diagnosis_3 = case_when((diag_3 >= 390 & diag_3 <= 459)  ~ 'Circulatory_Disease',
                                 startsWith(diag_3,'250') ~ 'Diabetes',
                                 TRUE ~ 'Others'))

table(diabetic$Diagnosis_1)
table(diabetic$Diagnosis_2)
table(diabetic$Diagnosis_3)

diabetic <-  diabetic %>%
  mutate(diabetis = case_when((Diagnosis_1 == 'Diabetes' | Diagnosis_2 == 'Diabetes' | Diagnosis_3 == 'Diabetes')  ~ 'YES',
                              TRUE ~ 'NO'))

diabetic <-  diabetic %>%
  mutate(Circulatory = case_when((Diagnosis_1 == 'Circulatory_Disease' | Diagnosis_2 == 'Circulatory_Disease' | Diagnosis_3 == 'Circulatory_Disease')  ~ 'YES',
                                 TRUE ~ 'NO'))

table(diabetic$diabetis)
table(diabetic$Circulatory)


diabetic <-  diabetic %>%
  mutate(comorbidity = case_when((diabetis == 'YES' & Circulatory == 'YES'   ~ '3'),
                                 (diabetis == 'NO' & Circulatory == 'NO'   ~ '0'),
                                 (diabetis == 'YES' & Circulatory == 'NO'   ~ '1'),
                                 (diabetis == 'NO' & Circulatory == 'YES'   ~ '2'),
                                 TRUE ~ 'NO'))

table(diabetic$comorbidity)

diabetic %>% group_by(comorbidity) %>% tally()

# removing unnecessary daignosis columns which may not be useful for modelling

diabetic <- subset(diabetic, select=-c(diag_1,diag_2,diag_3,Diagnosis_1,Diagnosis_2,Diagnosis_3,diabetis,Circulatory))


#Column names with missing Values
names(diabetic)[colSums(is.na(diabetic)) > 0]

sum(is.na(diabetic)) # ALL Na's are handled now.

################################################################
# Feature standardisation
################################################################

#4.Change the variable 'readmitted' to binary type by clubbing the values ">30" and "<30" as "YES".
diabetic$readmitted <- ifelse(diabetic$readmitted!="NO","YES","NO")
table(diabetic$readmitted)

#converting Change variable also to binary
table(diabetic$change)
diabetic$change <- ifelse(diabetic$change!="No","Yes","No")
table(diabetic$change)
table(diabetic$diabetesMed)


#discharge_disposition_id = 11,19,20,21 means Expired as per the UCI datasets IDS_mapping file these may not be helpful for readmission predicting so filter out these records.
sum(diabetic$discharge_disposition_id == 11 | diabetic$discharge_disposition_id == 19 | 
      diabetic$discharge_disposition_id == 20 | diabetic$discharge_disposition_id == 21)
# there are 1652 records so let's drop these because these are expired may not correct for readmission prediction
diabetic <- subset(diabetic,diabetic$discharge_disposition_id !=11 & diabetic$discharge_disposition_id != 19 & 
                     diabetic$discharge_disposition_id != 20 & diabetic$discharge_disposition_id != 21)
table(diabetic$discharge_disposition_id)

# Here we have completed all the data preparation steps so Lets take back up of dataframe
diabetic_master <- diabetic # This can be used for plotting and random forests or decision trees.

#1 - Discharge Home; 2 - Admitted to Any other Healthcare Provider; 3 - Discharge Home w/ Care

diabetic$discharge_disposition_id <- recode(diabetic$discharge_disposition_id, "c('1')='1';
                                            c('6','8') = '3';
                                            else='2'")

diabetic$discharge_disposition_id <- recode(diabetic$discharge_disposition_id,"c('1') ='Discharge Home';
                                            c('2') ='transferred';else = 'Discharge Home w/ Care' ")
table(diabetic$discharge_disposition_id)

sum(diabetic$admission_type_id == '5' | diabetic$admission_type_id =='6' | 
      diabetic$admission_type_id == '8')
# there are 10012 records we can't drop these many records so i'm grouping as one bucket 'not mapped'

diabetic$admission_type_id <- recode(diabetic$admission_type_id,"c('1')= 'Emergency';
                                     c('2')= 'Urgent';
                                     c('3') = 'Elective';
                                     c('4') = 'Newborn';
                                     c('7') = 'Trauma Center';
                                     else =  'Not Mapped'")
table(diabetic$admission_type_id)

table(diabetic$admission_source_id)

# # New levels:
# Admitted by Referral = 1
# 1 <- 1-3,11-12,23
# Transfered from Other Care Center = 2
# 2 <- 4-6,8,10,18,19,22,25,26
# Emergency Room / Visit =3
# 3 <- 7,13,14,24
#Referral = 1; Transfered = 2; Emergency Room / Visit =3  Not Mapped=4

diabetic$admission_source_id=recode(diabetic$admission_source_id,"c('1','2','3','11') ='Referral';
                                    c('4','5','6','8','10','22','25') = 'Transfered';
                                    c('7','13','14') = 'Emergency Room';
                                    else =  'Not Mapped'")

table(diabetic$admission_source_id)

str(diabetic)
dim(diabetic) # 100111     22 columns

# Check the outliers in numeric columns:

# Check for outliers in the numeric variables using boxplots.
                                 
boxplot(diabetic$time_in_hospital,main="time_in_hospital")  
boxplot(diabetic$num_lab_procedures,main="num_lab_procedures")    
boxplot(diabetic$num_procedures,main="num_procedures")   
boxplot(diabetic$num_medications,main="num_medications")   
boxplot(diabetic$number_outpatient,main="number_outpatient")   
boxplot(diabetic$number_emergency,main="number_emergency")   
boxplot(diabetic$number_inpatient,main="number_inpatient")   
boxplot(diabetic$number_diagnoses,main="number_diagnoses")  

#Boxplot is showing outliers, confirming it also with percentiles
sapply(diabetic[,c("time_in_hospital","num_lab_procedures","num_procedures","num_medications",
                    "number_outpatient","number_emergency","number_inpatient","number_diagnoses")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) 

#There are outliers num_lab_procedures,num_medications,number_outpatient,number_emergency,number_inpatient and number_diagnoses

#####################################################################################
#Task 2: Data Exploration                                                              #
#####################################################################################

#1.Perform basic data exploration for some categorical attributes

# univariate plots with categorical variables with readmitted

###the below plots could have been added to one pager but due to performance and time taking 
#to run all the plots.

plot_grid(ggplot(diabetic, aes(x=race,fill=readmitted))+ geom_bar() + theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(diabetic, aes(x=gender,fill=readmitted))+ geom_bar()+ theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(diabetic, aes(x=age,fill=readmitted))+ geom_bar()+ theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(diabetic, aes(x=change,fill=readmitted))+ geom_bar()+ theme_pander() + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(diabetic, aes(x=diabetesMed,fill=readmitted))+ geom_bar(show.legend = T)+theme_pander() + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)),
                  align = "h")

plot_grid(ggplot(diabetic, aes(x=admission_type_id,fill=readmitted))+ geom_bar() + theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(diabetic, aes(x=discharge_disposition_id,fill=readmitted))+ geom_bar()+ theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(diabetic, aes(x=admission_source_id,fill=readmitted))+ geom_bar()+ theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)),
          align = "h")

plot_grid(ggplot(diabetic, aes(x=A1Cresult,fill=readmitted))+ geom_bar() + theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(diabetic, aes(x=insulin,fill=readmitted))+ geom_bar()+ theme_pander() +theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="none"),
          ggplot(diabetic, aes(x=comorbidity,fill=readmitted))+ geom_bar()+ theme_pander() + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)),
          align = "h")

table(diabetic$readmitted)
# Readmission Rate by race
group <- group_by(diabetic, race)
by_race <- summarize(group, Total.Count = n(),
                    No.readmitted = sum(as.character(readmitted) == "YES"),
                    Pct.readmitted = round(No.readmitted / Total.Count * 100))

ggplot(by_race, aes(x = reorder(factor(race),-Pct.readmitted), y = Pct.readmitted)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Race", y = "Readmission (%)", title = "Readmission rate by Race") +
  theme_pander()+
  geom_text(aes(y = Pct.readmitted + 1,    # nudge above top of bar
                label = paste0(Pct.readmitted, '%')),    # prettify
            position = position_dodge(width = .9), size = 3.5) 
##Observation : Caucasian and AfricanAmerican categories are readmitting more than others.
# Readmission Rate by gender
group <- group_by(diabetic, gender)
by_gender <- summarize(group, Total.Count = n(),
                     No.readmitted = sum(as.character(readmitted) == "YES"),
                     Pct.readmitted = round(No.readmitted / Total.Count * 100))

ggplot(by_gender, aes(x = reorder(factor(gender),-Pct.readmitted), y = Pct.readmitted)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Gender", y = "Readmission (%)", title = "Readmission Rate by Gender") +
  theme_pander()+
  geom_text(aes(y = Pct.readmitted + 1,    # nudge above top of bar
                label = paste0(Pct.readmitted, '%')),    # prettify
            position = position_dodge(width = .9), size = 3.5) 
##Observation : Female are readmitting more than others.

# Readmission Rate by Age
group <- group_by(diabetic, age)
by_Age <- summarize(group, Total.Count = n(),
                    No.readmitted = sum(as.character(readmitted) == "YES"),
                    Pct.readmitted = round(No.readmitted / Total.Count * 100))

ggplot(by_Age, aes(x = reorder(factor(age),-Pct.readmitted), y = Pct.readmitted)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "Age", y = "Readmission Rate (%)", title = "Readmission rate by Age") +
  theme_pander()+
  geom_text(aes(y = Pct.readmitted + 1,    # nudge above top of bar
                label = paste0(Pct.readmitted, '%')),    # prettify
            position = position_dodge(width = .9), size = 3.5) 

##Observation : above 60 age group is admitting more than other


# Readmission Rate by change
group <- group_by(diabetic, change)
by_change <- summarize(group, Total.Count = n(),
                                  No.readmitted = sum(as.character(readmitted) == "YES"),
                                  Pct.readmitted = round(No.readmitted / Total.Count * 100))

ggplot(by_change, aes(x = change, y = Pct.readmitted)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "change", y = "Readmission Rate (%)", title = "Readmission Rate by change") +
  theme_pander()+
  geom_text(aes(y = Pct.readmitted + 1,    # nudge above top of bar
                label = paste0(Pct.readmitted, '%')),    # prettify
            position = position_dodge(width = .9), size = 3.5)
###Observation : When diabetis medication change then more % of patients are readmitting.

# Readmission Rate by comorbidity
group <- group_by(diabetic, comorbidity)
by_change <- summarize(group, Total.Count = n(),
                       No.readmitted = sum(as.character(readmitted) == "YES"),
                       Pct.readmitted = round(No.readmitted / Total.Count * 100))

ggplot(by_change, aes(x = reorder(factor(comorbidity),-Pct.readmitted), y = Pct.readmitted)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "comorbidity", y = "Readmission Rate (%)", title = "Readmission Rate by comorbidity") +
  theme_pander()+
  geom_text(aes(y = Pct.readmitted + 1,    # nudge above top of bar
                label = paste0(Pct.readmitted, '%')),    # prettify
            position = position_dodge(width = .9), size = 3.5)
###Observation : Readmission rate by comorbidity is almost equal aprox > ~45% but higher with circulatory disease.

# Readmission Rate by admission_type_id
group <- group_by(diabetic, admission_type_id)
by_change <- summarize(group, Total.Count = n(),
                       No.readmitted = sum(as.character(readmitted) == "YES"),
                       Pct.readmitted = round(No.readmitted / Total.Count * 100))

ggplot(by_change, aes(x = reorder(factor(admission_type_id),-Pct.readmitted), y = Pct.readmitted)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "admission_type_id", y = "Readmission Rate (%)", title = "Readmission Rate by admission_type_id") +
  theme_pander()+
  geom_text(aes(y = Pct.readmitted + 1,    # nudge above top of bar
                label = paste0(Pct.readmitted, '%')),    # prettify
            position = position_dodge(width = .9), size = 3.5)
###Observation : Readmission rate by admission_type_id  with Emrgency and Urgent are high if we exlude Not mapped. .

# Readmission Rate by discharge_disposition_id
group <- group_by(diabetic, discharge_disposition_id)
by_change <- summarize(group, Total.Count = n(),
                       No.readmitted = sum(as.character(readmitted) == "YES"),
                       Pct.readmitted = round(No.readmitted / Total.Count * 100))

ggplot(by_change, aes(x = reorder(factor(discharge_disposition_id),-Pct.readmitted), y = Pct.readmitted)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "discharge_disposition_id", y = "Readmission Rate (%)", title = "Readmission Rate by discharge_disposition_id") +
  theme_pander()+
  geom_text(aes(y = Pct.readmitted + 1,    # nudge above top of bar
                label = paste0(Pct.readmitted, '%')),    # prettify
            position = position_dodge(width = .9), size = 3.5)
###Observation : Readmission rate by discharge_disposition_id is high with categories Discharge Home w/care.

# Readmission Rate by admission_source_id
group <- group_by(diabetic, admission_source_id)
by_change <- summarize(group, Total.Count = n(),
                       No.readmitted = sum(as.character(readmitted) == "YES"),
                       Pct.readmitted = round(No.readmitted / Total.Count * 100))

ggplot(by_change, aes(x = reorder(factor(admission_source_id),-Pct.readmitted), y = Pct.readmitted)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "admission_source_id", y = "Readmission Rate (%)", title = "Readmission Rate by admission_source_id") +
  theme_pander()+
  geom_text(aes(y = Pct.readmitted + 1,    # nudge above top of bar
                label = paste0(Pct.readmitted, '%')),    # prettify
            position = position_dodge(width = .9), size = 3.5)
###Observation : Readmission rate by admission_source_id is high with Emergency Room
# Create Continuous variables vector
contvarnames <- names(Filter(is.numeric, diabetic))
contvarnames

# Check the Continuous columns 
sapply(diabetic[contvarnames], summary)

corrplot.mixed(cor(diabetic[contvarnames]), 
               upper = "ellipse", number.cex = .8,
               tl.cex = 0.55, tl.pos = 'lt')

# Number of medications and time in hospital are highly correlated.
# Number of medications and number of procedures are highly correlated.

#2.Perform basic data exploration for some numerical attributes
# Correlation between numeric variables
#library(GGally)
ggpairs(diabetic[, c("time_in_hospital","num_lab_procedures","num_procedures","num_medications")])
ggpairs(diabetic[, c("number_outpatient","number_emergency","number_inpatient","number_diagnoses")])

# time_in_hospital and num_medications are highly correlated (corr 0.466) from the first chart 

################# time in hospital by age among the data set ################# 
ggplot(diabetic, aes(diabetic$age,time_in_hospital)) + geom_boxplot(aes(fill = gender)) +
  labs(title = "time in hospital by Age",  x = "Age", y = "time_in_hospital" ,fill='Gender:') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") 
###Observation :Female patients above 60 who are spending in hospital are more than other age groups.
# Men above age 70 are almost equal with Female category.

################# number of lab procedures by age among the data set ################# 
ggplot(diabetic, aes(diabetic$age,num_lab_procedures)) + geom_boxplot(aes(fill = gender)) +
  labs(title = "number of lab procedures by Age",  x = "Age", y = "number of lab procedures" ,fill='Gender:') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") 
###Observation : number of lab procedures with age between 20 and 30 are high with Male across the age groups.
#and between 10 to 20 age group are high with Female 

################# number of procedures by age among the data set ################# 
ggplot(diabetic, aes(diabetic$age,num_procedures)) + geom_boxplot(aes(fill = gender)) +
  labs(title = "number of procedures by Age",  x = "Age", y = "number of procedures" ,fill='Gender:') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") 
###Observation :between age group 40 and 80 with male and female number procedures are almost equal.

################# number of Medications by age among the data set ################# 
ggplot(diabetic, aes(diabetic$age,num_medications)) + geom_boxplot(aes(fill = gender)) +
  labs(title = "number of Medications by Age",  x = "Age", y = "number of medications" ,fill='Gender:') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") 
###Observation :Number of medications with female are higher than male till age 70.

################# number of outpatient by age among the data set ################# 
ggplot(diabetic, aes(diabetic$age,number_outpatient)) + geom_boxplot(aes(fill = gender)) +
  labs(title = "number of outpatient by Age",  x = "Age", y = "number of outpatient" ,fill='Gender:') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")
###Observation :hard to intrepret due to outliers.

################# number of emergency by age among the data set ################# 
ggplot(diabetic, aes(diabetic$age,number_emergency)) + geom_boxplot(aes(fill = gender)) +
  labs(title = "number of emergency by Age",  x = "Age", y = "number of emergency" ,fill='Gender:') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")
###Observation :hard to intrepret due to outliers.

################# number of inpatient by age among the data set ################# 
ggplot(diabetic, aes(diabetic$age,number_inpatient)) + geom_boxplot(aes(fill = gender)) +
  labs(title = "number of inpatient by Age",  x = "Age", y = "number of inpatient" ,fill='Gender:') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")
###Observation :hard to intrepret due to outliers.

################# number_diagnoses by age among the data set ################# 
ggplot(diabetic, aes(diabetic$age,number_diagnoses)) + geom_boxplot(aes(fill = gender)) +
  labs(title = "number of diagnoses by Age",  x = "Age", y = "number diagnoses" ,fill='Gender:') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")
###Observation :number of diagnoses by Age after 70 are high in both male and female category.

# Plot of default readmission rates by age, grouped by race
group <- group_by(diabetic, age, race)
by_age_race <- summarize(group, Total.Count = n(),
                           No.Diabetic = sum(readmitted == "YES"),
                           Pct.Diabetic = round(No.Diabetic / Total.Count * 100),digits=0)

ggplot(by_age_race, aes(x = age, y = Pct.Diabetic, fill = factor(race))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "race") +
  labs(x = "Age", y = "Readmission Rate (%)", title = "Readmission Rate by age and race") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Diabetic + .5,    # nudge above top of bar
                label = paste0(Pct.Diabetic, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
###Observation :Race "Hispanic" with above 90 age readmission rate is high. 

# Plot of default readmission rates by age, grouped by gender
group <- group_by(diabetic, age, gender)
by_age_gender <- summarize(group, Total.Count = n(),
                         No.Diabetic = sum(readmitted == "YES"),
                         Pct.Diabetic = round(No.Diabetic / Total.Count * 100),digits=0)

ggplot(by_age_gender, aes(x = age, y = Pct.Diabetic, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "gender") +
  labs(x = "Age", y = "Readmission Rate (%)", title = "Readmission Rate by age and gender") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Diabetic + .5,    # nudge above top of bar
                label = paste0(Pct.Diabetic, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
###Observation :Across the age groups female admission rate are higher than male except between age groups 0 to 10 and 30 to 40 groups.


# Plot of default readmission rates by age, grouped by A1Cresult
group <- group_by(diabetic, age, A1Cresult)
by_age_A1Cresult <- summarize(group, Total.Count = n(),
                                  No.Diabetic = sum(readmitted == "YES"),
                                  Pct.Diabetic = round(No.Diabetic / Total.Count * 100),digits=0)

ggplot(by_age_A1Cresult, aes(x = age, y = Pct.Diabetic, fill = factor(A1Cresult))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "A1Cresult") +
  labs(x = "Age", y = "Readmission Rate (%)", title = "Readmission Rate by age and A1Cresult") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Diabetic + .5,    # nudge above top of bar
                label = paste0(Pct.Diabetic, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
###Observation : Readmission Rate is high with A1Cresult >7 age between 10 to 20.

# Plot of default readmission rates by age, grouped by diabetesMed
group <- group_by(diabetic, age, diabetesMed)
by_age_diabetesMed <- summarize(group, Total.Count = n(),
                                  No.Diabetic = sum(readmitted == "YES"),
                                  Pct.Diabetic = round(No.Diabetic / Total.Count * 100),digits=0)

ggplot(by_age_diabetesMed, aes(x = age, y = Pct.Diabetic, fill = factor(diabetesMed))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "diabetesMed") +
  labs(x = "Age", y = "Readmission Rate (%)", title = "Readmission Rate by age and diabetesMed") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Diabetic + .5,    # nudge above top of bar
                label = paste0(Pct.Diabetic, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
###Observation : Readmission Rate is high if they are already using diabetic medication across the age group.

# Plot of default readmission rates by age, grouped by change
group <- group_by(diabetic, age, change)
by_age_change <- summarize(group, Total.Count = n(),
                                  No.Diabetic = sum(readmitted == "YES"),
                                  Pct.Diabetic = round(No.Diabetic / Total.Count * 100),digits=0)

ggplot(by_age_change, aes(x = age, y = Pct.Diabetic, fill = factor(change))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "change") +
  labs(x = "Age", y = "Readmission Rate (%)", title = "Readmission Rate by age and change") +
  theme(plot.title = element_text(hjust = 0.5),legend.position="bottom") +
  geom_text(aes(y = Pct.Diabetic + .5,    # nudge above top of bar
                label = paste0(Pct.Diabetic, '%')),    # prettify
            position = position_dodge(width = .9), 
            size = 3)
###Observation : Readmission Rate is high if they is a change diabetic medication across the age group.

# Readmission Rate by A1Cresult
group <- group_by(diabetic, A1Cresult)
by_change <- summarize(group, Total.Count = n(),
                       No.readmitted = sum(as.character(readmitted) == "YES"),
                       Pct.readmitted = round(No.readmitted / Total.Count * 100))

ggplot(by_change, aes(x = reorder(factor(A1Cresult),-Pct.readmitted), y = Pct.readmitted)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "A1Cresult", y = "Readmission Rate (%)", title = "Readmission Rate by A1Cresult") +
  theme_pander()+
  geom_text(aes(y = Pct.readmitted + 1,    # nudge above top of bar
                label = paste0(Pct.readmitted, '%')),    # prettify
            position = position_dodge(width = .9), size = 3.5)
###Observation : Readmission rate by A1Cresult is high with >8

# Readmission Rate by insulin
group <- group_by(diabetic, insulin)
by_change <- summarize(group, Total.Count = n(),
                       No.readmitted = sum(as.character(readmitted) == "YES"),
                       Pct.readmitted = round(No.readmitted / Total.Count * 100))

ggplot(by_change, aes(x = reorder(factor(insulin),-Pct.readmitted), y = Pct.readmitted)) +
  geom_bar(stat = "identity", position = "dodge",fill='#1567b6') +
  labs(x = "insulin", y = "Readmission Rate (%)", title = "Readmission Rate by insulin") +
  theme_pander()+
  geom_text(aes(y = Pct.readmitted + 1,    # nudge above top of bar
                label = paste0(Pct.readmitted, '%')),    # prettify
            position = position_dodge(width = .9), size = 3.5)
###Observation : Readmission rate by insulin is high with Down and Up category.

#3.Scale numeric attributes and create dummy variables for categorical ones.

colnames(diabetic)
catvarnames<- names(Filter(is.character, diabetic))
catvarnames

# converting diabetesMed variable from No/Yes character to factorwith levels 0/1 
diabetic$diabetesMed<- ifelse(diabetic$diabetesMed=="Yes",1,0)
diabetic$change<- ifelse(diabetic$change=="Yes",1,0)
diabetic$readmitted <- as.numeric(ifelse(as.character(diabetic$readmitted) == "YES", 1,0))
table(diabetic$readmitted)

# converting diabetesMed variable from No/Yes character to factorwith levels 0/1 
diabetic_master$diabetesMed<- ifelse(diabetic_master$diabetesMed=="Yes",1,0)
diabetic$change<- ifelse(diabetic_master$change=="Yes",1,0)
diabetic_master$readmitted <- as.numeric(ifelse(as.character(diabetic_master$readmitted) == "YES", 1,0))

# Checking readmitted rate

readmissionrate <- sum(diabetic$readmitted)/nrow(diabetic)
readmissionrate # 47.06 % readmission Rate is half of population it is high. 

# Convert columns except which are ids &number rest all converting to Factor type.
factorColumns <- c("race","gender","age","A1Cresult","insulin","change","diabetesMed","comorbidity","readmitted","admission_type_id","discharge_disposition_id","admission_source_id")


# creating a dataframe of categorical features
diabetic_chr<- diabetic[,c("race","gender","age","A1Cresult","insulin","change","diabetesMed","comorbidity","admission_type_id","discharge_disposition_id","admission_source_id")]

# converting categorical attributes to factor
diabetic_fact<- data.frame(sapply(diabetic_chr, function(x) factor(x)))
str(diabetic_fact)

#diabetic <- Diabetic_BKP
diabetic[, factorColumns] <- lapply(diabetic[, factorColumns], as.factor)
str(diabetic)
diabetic_master[, factorColumns] <- lapply(diabetic_master[, factorColumns], as.factor)
str(diabetic_master)
# creating dummy variables for factor attributes
dummies<- data.frame(sapply(diabetic_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =diabetic_fact))[,-1]))

# For variables having only two levels, change","diabetesMed" "yes" is 1 gender "male" is 1
colnames(dummies) <- gsub(pattern=".x","",colnames(dummies))

# Final dataset
diabetic_final<- cbind(diabetic[,c('readmitted','time_in_hospital','num_lab_procedures','num_procedures','num_medications',
                                   'number_outpatient','number_emergency','number_inpatient','number_diagnoses')],dummies) 
str(diabetic_final) #100114 obs. of  44 variables

# Scale numeric variables to standardised ranges.
scaleColumns <- c("time_in_hospital","num_lab_procedures","num_procedures","num_medications",
                  "number_outpatient","number_emergency","number_inpatient","number_diagnoses")

# Scaling Continuous Variables
diabetic_final[scaleColumns] <-  lapply(diabetic_final[scaleColumns], function(x) scale(x))

diabetic[scaleColumns] <-  lapply(diabetic[scaleColumns], function(x) scale(x))
diabetic_master[scaleColumns] <-  lapply(diabetic_master[scaleColumns], function(x) scale(x))
#apply(diabetic_final, 2, function(x) length(unique(x)))

dim(diabetic_final) # 100114     44 variables
str(diabetic_final)  
str(diabetic)
diabetic$encounter_id <- NULL
diabetic$patient_nbr <- NULL
#remove the some of the unnncessary temp dataframes from the session.
rm(diabetic_bkp,diabetic_chr,diabetic_fact,dummies,missing_values)

#####################################################################################
#Task 3 : Model Building                                                             #
#####################################################################################
# baseline accuracy
prop.table(table(diabetic_final$readmitted))  # 0.47                                                            

# 3.1.Divide your data into training and testing dataset

########################################################################
# splitting the data between train and test
set.seed(123)

indices = sample.split(diabetic_final$readmitted, SplitRatio = 0.7)

train = diabetic_final[indices,]

test = diabetic_final[!(indices),]

#####################################################################
#### Logistic Regression: 
#### Model 1 :Logistic Regression:
#####################################################################

#Initial model
model_1 = glm(readmitted ~ ., data = train, family = "binomial")
summary(model_1) #AIC 90640

# Stepwise selection
# this below step is taking longer time but it executes with out any errors.
model_2<- stepAIC(model_1, direction="both") 

summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)
#Excluding age.70.80.
model_3 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 gender + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
                 age.50.60. + age.60.70. + age.80.90. + age.90.100. + 
                 A1CresultNone + A1CresultNorm + insulinNo + insulinSteady + 
                 insulinUp + change + diabetesMed + comorbidity1 + comorbidity2 + 
                 comorbidity3 + admission_type_idNot.Mapped + admission_type_idTrauma.Center + 
                 admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                 discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)
summary(model_3) 

vif(model_3) 

#Excluding admission_type_idTrauma.Center
model_4<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                num_procedures + number_outpatient + number_emergency + number_inpatient + 
                number_diagnoses + raceAsian + raceHispanic + raceOther + 
                gender + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
                age.50.60. + age.60.70. + age.80.90. + age.90.100. + 
                A1CresultNone + A1CresultNorm + insulinNo + insulinSteady + 
                insulinUp + change + diabetesMed + comorbidity1 + comorbidity2 + 
                comorbidity3 + admission_type_idNot.Mapped +  
                admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                admission_source_idReferral + admission_source_idTransfered, 
              family = "binomial", data = train)
summary(model_4) 

vif(model_4) 

#Excluding age.80.90.
model_5<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                num_procedures + number_outpatient + number_emergency + number_inpatient + 
                number_diagnoses + raceAsian + raceHispanic + raceOther + 
                gender + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
                age.50.60. + age.60.70. +  age.90.100. + 
                A1CresultNone + A1CresultNorm + insulinNo + insulinSteady + 
                insulinUp + change + diabetesMed + comorbidity1 + comorbidity2 + 
                comorbidity3 + admission_type_idNot.Mapped +  
                admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                admission_source_idReferral + admission_source_idTransfered, 
              family = "binomial", data = train)

summary(model_5) 

vif(model_5) 

#Excluding insulinUp which has high P values
model_6<-glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
               num_procedures + number_outpatient + number_emergency + number_inpatient + 
               number_diagnoses + raceAsian + raceHispanic + raceOther + 
               gender + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
               age.50.60. + age.60.70. +  age.90.100. + 
               A1CresultNone + A1CresultNorm + insulinNo + insulinSteady + 
               change + diabetesMed + comorbidity1 + comorbidity2 + 
               comorbidity3 + admission_type_idNot.Mapped +  
               admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
               discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
               admission_source_idReferral + admission_source_idTransfered, 
             family = "binomial", data = train)  

summary(model_6) 

vif(model_6) 

#Excluding change + which has high P value
model_7<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                num_procedures + number_outpatient + number_emergency + number_inpatient + 
                number_diagnoses + raceAsian + raceHispanic + raceOther + 
                gender + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
                age.50.60. + age.60.70. +  age.90.100. + 
                A1CresultNone + A1CresultNorm + insulinNo + insulinSteady + 
                 diabetesMed + comorbidity1 + comorbidity2 + 
                comorbidity3 + admission_type_idNot.Mapped +  
                admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                admission_source_idReferral + admission_source_idTransfered, 
              family = "binomial", data = train)   

summary(model_7) 

vif(model_7) 

#Excluding insulinNo  which has  P value insignificant
model_8<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                num_procedures + number_outpatient + number_emergency + number_inpatient + 
                number_diagnoses + raceAsian + raceHispanic + raceOther + 
                gender + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
                age.50.60. + age.60.70. +  age.90.100. + 
                A1CresultNone + A1CresultNorm + insulinSteady + 
                diabetesMed + comorbidity1 + comorbidity2 + 
                comorbidity3 + admission_type_idNot.Mapped +  
                admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                admission_source_idReferral + admission_source_idTransfered, 
              family = "binomial", data = train) 

summary(model_8) 

vif(model_8) 

#Excluding age.10.20.which has  P value insignificant
model_9<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                num_procedures + number_outpatient + number_emergency + number_inpatient + 
                number_diagnoses + raceAsian + raceHispanic + raceOther + 
                gender + age.20.30. + age.30.40. + age.40.50. + 
                age.50.60. + age.60.70. +  age.90.100. + 
                A1CresultNone + A1CresultNorm + insulinSteady + 
                diabetesMed + comorbidity1 + comorbidity2 + 
                comorbidity3 + admission_type_idNot.Mapped +  
                admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                admission_source_idReferral + admission_source_idTransfered, 
              family = "binomial", data = train) 
summary(model_9) 

vif(model_9) 

#Excluding age.90.100.  which has  P value insignificant
model_10<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 gender + age.20.30. + age.30.40. + age.40.50. + 
                 age.50.60. + age.60.70. +  
                 A1CresultNone + A1CresultNorm + insulinSteady + 
                 diabetesMed + comorbidity1 + comorbidity2 + 
                 comorbidity3 + admission_type_idNot.Mapped +  
                 admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                 discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train) 

summary(model_10) 

vif(model_10) 

#Excluding age.60.70. which has  P value insignificant
model_11<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 gender + age.20.30. + age.30.40. + age.40.50. + 
                 age.50.60. + 
                 A1CresultNone + A1CresultNorm + insulinSteady + 
                 diabetesMed + comorbidity1 + comorbidity2 + 
                 comorbidity3 + admission_type_idNot.Mapped +  
                 admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                 discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)   

summary(model_11) 

vif(model_11) 

#Excluding A1CresultNone which has  P value insignificant
model_12<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 gender + age.20.30. + age.30.40. + age.40.50. + 
                 age.50.60. + 
                 A1CresultNorm + insulinSteady + 
                 diabetesMed + comorbidity1 + comorbidity2 + 
                 comorbidity3 + admission_type_idNot.Mapped +  
                 admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                 discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)

summary(model_12) 

vif(model_12) 

#Excluding age.20.30. which has  P value insignificant
model_13<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 gender + age.30.40. + age.40.50. + 
                 age.50.60. + 
                 A1CresultNorm + insulinSteady + 
                 diabetesMed + comorbidity1 + comorbidity2 + 
                 comorbidity3 + admission_type_idNot.Mapped +  
                 admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                 discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)

summary(model_13) 

vif(model_13) 

#Excluding gender  which has  P value insignificant
model_14<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 age.30.40. + age.40.50. + 
                 age.50.60. + 
                 A1CresultNorm + insulinSteady + 
                 diabetesMed + comorbidity1 + comorbidity2 + 
                 comorbidity3 + admission_type_idNot.Mapped +  
                 admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                 discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)  

summary(model_14) 

vif(model_14) 

###All variables are significant P value and VIF < 2 but too many variables so going with anova test and rmeoving some more variables.
anova(model_14,test="Chisq")

#Excluding comorbidity1  which is insignificant
model_15<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 age.30.40. + age.40.50. + 
                 age.50.60. + 
                 A1CresultNorm + insulinSteady + 
                 diabetesMed + comorbidity2 + 
                 comorbidity3 + admission_type_idNot.Mapped +  
                 admission_type_idUrgent + discharge_disposition_idDischarge.Home.w..Care + 
                 discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)    

summary(model_15) 

vif(model_15) 

anova(model_15,test="Chisq")

#Excluding admission_type_idUrgent  which is insignificant
model_16<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 age.30.40. + age.40.50. + 
                 age.50.60. + 
                 A1CresultNorm + insulinSteady + 
                 diabetesMed + comorbidity2 + 
                 comorbidity3 + admission_type_idNot.Mapped +  
                discharge_disposition_idDischarge.Home.w..Care + 
                 discharge_disposition_idtransferred + admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)     

summary(model_16) 

vif(model_16) 
anova(model_16,test="Chisq")

#Excluding insulinSteady
model_17<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 age.30.40. + age.40.50. + 
                 age.50.60. + age.90.100. + 
                 A1CresultNone + 
                 diabetesMed + comorbidity2 + 
                 comorbidity3 + admission_type_idNot.Mapped + 
                 discharge_disposition_idDischarge.Home.w..Care + 
                 admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)  

summary(model_17) 

vif(model_17)
anova(model_17,test="Chisq")

#Excluding A1CresultNone
model_18<- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 age.30.40. + age.40.50. + 
                 age.50.60. + age.90.100. + 
                 diabetesMed + comorbidity2 + 
                 comorbidity3 + admission_type_idNot.Mapped + 
                 discharge_disposition_idDischarge.Home.w..Care + 
                 admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)
summary(model_18) 

vif(model_18)

#anova(model_18,test="Chisq")

#Excluding num_lab_procedures
model_19<- glm(formula = readmitted ~ time_in_hospital +  
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 age.30.40. + age.40.50. + 
                 age.50.60. + age.90.100. + 
                 diabetesMed + comorbidity2 + 
                 comorbidity3 + admission_type_idNot.Mapped + 
                 discharge_disposition_idDischarge.Home.w..Care + 
                 admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)

summary(model_19) 

vif(model_19)
anova(model_19,test="Chisq")

#Excluding comorbidity2 +  
model_20<- glm(formula = readmitted ~ time_in_hospital +  
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceHispanic + raceOther + 
                 age.30.40. + age.40.50. + 
                 age.50.60. + age.90.100. + 
                 diabetesMed + 
                 comorbidity3 + admission_type_idNot.Mapped + 
                 discharge_disposition_idDischarge.Home.w..Care + 
                 admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)

summary(model_20) 

vif(model_20)
anova(model_20,test="Chisq")

#Excluding raceHispanic
model_21<- glm(formula = readmitted ~ time_in_hospital +  
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + raceAsian + raceOther + 
                 age.30.40. + age.40.50. + 
                 age.50.60. + age.90.100. + 
                 diabetesMed + 
                 comorbidity3 + admission_type_idNot.Mapped + 
                 discharge_disposition_idDischarge.Home.w..Care + 
                 admission_source_idNot.Mapped + 
                 admission_source_idReferral + admission_source_idTransfered, 
               family = "binomial", data = train)

summary(model_21) 

vif(model_21)
anova(model_21,test="Chisq")

# This model has all significant variables which are significant and VIF <2. and with ANOVA also unable to decide which variables to drop further so stopping here.

###Model Evuation plots Evaluating discrimination
# please press any key from the keyboard to get the plot drawn
plot(model_21) ## to plot residual and QQ plot to visualize the model accuracy
dwplot(model_21) 
optiPair(model = model_21, measures = c("Sensitivity", "Specificity"), main = "Optimal balance")
HLfit(model = model_21, bin.method = "n.bins", main = "Hosmer-Lemeshow GOF, N bins")
HLfit(model = model_21, bin.method = "quantiles", main = "Hosmer-Lemeshow GOF, quantiles")

final_model_logistic<- model_21

### Model Evaluation with test data

#predicted probabilities of readmission for test data

test_pred = predict(final_model_logistic, type = "response", 
                    newdata = test[,-1])

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual <- factor(ifelse(test$readmitted==1,"Yes","No"))

table(test_actual,test_pred)

test_conf <- confusionMatrix(test_actual,test_pred, positive = "Yes")
test_conf

(acc <- test_conf$overall[1]) # Accuracy : 0.6250042
(sens <- test_conf$byClass[1]) # 0.6434703
(spec <- test_conf$byClass[2]) # 0.6160798

#######################################################################
test_pred <- factor(ifelse(test$prob >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred, test_actual, positive = "Yes")
test_conf

(acc <- test_conf$overall[1])
(sens <- test_conf$byClass[1])
(spec <- test_conf$byClass[2])

# Lets plot the Area under curve
#library(AUC)
#auc(roc(test_pred,test_actual))
AUC(model = final_model_logistic)

#----------------------------------------------------------------------------------
#------------------------Ridge Regression-------------------------------------
#-----------------------------------------------------------------------------
library(varhandle)
diabetic$readmitted <- unfactor(diabetic$readmitted)

# Create a matrix "x" of all independent variables
# and store dependent variable in "y".

x <- model.matrix(readmitted~.,data=diabetic)[,-1]
y <- diabetic$readmitted 

# Divide you data in 70:30 

set.seed(123)
train= sample(1:nrow(x), 0.7*nrow(x))

# Store indices into test which is not present in train . 
test = (-train)
y.test = y[test]

#library(glmnet)
# Cross Validation

cv.out <- cv.glmnet(x[train,],y[train],family="binomial", nfolds = 10, type.measure = "deviance")
plot(cv.out)

# Optimal lamda 
(minlamda <- cv.out$lambda.min)

# Apply model on train dataset at lambda equal to minlamda
ridge.mod <- glmnet(x[train,],y[train],family="binomial",lambda =minlamda)

# Prediction on test dataset
ridge.pred <- predict(ridge.mod,s=minlamda,newx=x[test,])

# MSE with ridge 
mean((ridge.pred-y.test)^2)

# Apply model on train dataset at lambda equal to zero

ridge.mod <- glmnet(x[train,],y[train],alpha=0,lambda = 0)

# Linear Regression model
ridge.pred_0 <- predict(ridge.mod,s=0,newx=x[test,])

# MSE Linear regression model
mean((ridge.pred_0-y.test)^2)

# Final model Coefficents 
ridge_coef <- predict(ridge.mod,type="coefficients",s=minlamda)
ridge_coef

# Non zero coefficients in final model
ridge_coef[ridge_coef!=0]

diabetic$readmitted <- as.factor(diabetic$readmitted)

#######################################################################
######## SVM Model #########################
#######################################################################
set.seed(123)
#diabetic$readmitted <- as.factor(diabetic$readmitted)

train_sample <- stratified(diabetic_master, "readmitted", size = 5000)
test_sample <-  stratified(diabetic_master, "readmitted", size = 5000)
#train_sample$readmitted <- as.factor(train_sample$readmitted)
#test_sample$readmitted <- as.factor(test_sample$readmitted)
# Lets again check the proportion of the target variables in the Datasets
percent(prop.table(table(train_sample$readmitted)))
percent(prop.table(table(test_sample$readmitted)))
# The proportion comes to 50 % across all the readmitted in train dataset
# matching closely with the proportion of the original dataset.

####building the Model using train_sample dataset

#####################################################################
#### # SVM Model - RBF
#####################################################################

(Model_RBF <- ksvm(readmitted~ ., data = train_sample, scale = FALSE, kernel = "rbfdot"))
# Model_RBF
# There are warnings regarding Scaling of Data. but we can ignore these.
#### RBF Model Metrics ####
# parameter : epsilon = 0.1  cost C = 1   
# Hyperparameter : sigma =  0.0468450041210778 
# Number of Support Vectors : 8129 

#####################################################################
#### RBF Model Evaluation ####
#####################################################################
Eval_RBF_test<- predict(Model_RBF, test_sample)
confusionMatrix(Eval_RBF_test,test_sample$readmitted)
# Accuracy : 0.647
# Sensitivity : 0.6832  
# Specificity : 0.6108

#####################################################################
#### # Classification Trees ####
#####################################################################

# splitting the data between train and test

model_dataset <- diabetic_master
model_dataset$patient_nbr <- NULL
model_dataset$encounter_id <- NULL

indices = sample.split(model_dataset$readmitted, SplitRatio = 0.7)

train = model_dataset[indices,]

test = model_dataset[!(indices),]

#3 Tune the hyperparameters ----------------------------------------------------------
tree.model <- rpart(readmitted ~ .,                                # formula
                    data = train,                             # training data
                    method = "class",                         # classification or regression
                    control = rpart.control(minsplit = 1000,  # min observations for node
                                            minbucket = 1000, # min observations for leaf node
                                            cp = 0.05))       # complexity parameter

# display decision tree
prp(tree.model)
# make predictions on the test set
tree.predict <- predict(tree.model, test, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test$readmitted, positive = "1")  # 0.6155

#building a decision tree model-----------------------------------------------------------------
tree.model <- rpart(readmitted ~ .,                                # formula
                    data = train,                             # training data
                    method = "class",                         # classification or regression
                    control = rpart.control(minsplit = 1,     # min observations for node
                                            minbucket = 1,    # min observations for leaf node
                                            cp = 0.001))      # complexity parameter

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test$readmitted, positive = "1") #Accuracy :0.6234

# look at tree
tree.model
fancyRpartPlot(tree.model)
rpart.plot(tree.model)
#####################################################################################################
###  RandomForrest Model:
#####################################################################################################

Rf_fit <- randomForest(readmitted ~ ., data=train, proximity=FALSE,
                       ntree=500, mtry=5, do.trace=TRUE, na.action=na.omit,importance=TRUE)
print(Rf_fit)
test$pred_readmit <- predict(Rf_fit, test, type = "response")
table(test$readmitted, test$pred_readmit)
confusionMatrix(test$pred_readmit, test$readmitted) #Accuracy : 0.6507
prop.table(table(test$readmitted, test$pred_readmit),1)
Rf_fit$importance


#importance(Rf_fit)
#####################################################################################################
#### NEURAL NET
#####################################################################################################
#library(nnet)
nnet_model <- nnet(formula = readmitted ~ ., 
                   data=train, size = 5, maxit = 100)

test$pred_readmit <- predict(nnet_model, test, type = "class")
test$pred_readmit <- as.factor(test$pred_readmit)
prop.table(table(test$readmitted, test$pred_readmit),1)
confusionMatrix(test$pred_readmit, test$readmitted) # Accuracy : 0.6255

#####################################################################################################
#### Naive bayes
#####################################################################################################

nbayesmodel <- naiveBayes(readmitted ~ ., 
                          data = train)
pred <- predict(nbayesmodel, test, type = "class")
pred_nbayes <- pred

confusionMatrix(pred_nbayes, test$readmitted) # Accuracy : 0.5976
###################################

### Summary of Machine Learning Models:

# Logistic Regression Accuracy : 0.6250042
# SVM           Accuracy    : 0.647
# decision Tree Accuracy : 0.6234
# random forest Accuracy : 0.6507
# NEURAL NET  Accuracy : 0.6255
# Naive bayes Accuracy : 0.5976

# Random forest Accuracy : 0.6507 is better than all other models so we can use random forest model
# for predicting risk of readmission for the patient.

# Task 3.3 Stratify your population into 3 risk buckets:
# predicted probabilities of readmitted = 1 for test data
probs<- predict(Rf_fit, diabetic_master, type = "prob")
#View(probs)
probstemp <- as.data.frame(probs)
#View(probstemp)
prob <- probstemp[2]
diabetic_master <- cbind(diabetic_master,prob)
names(diabetic_master)[23] <-"prob" 
str(diabetic_master)

summary(diabetic_master$prob)

diabetic_master$riskbucket <- (ifelse((diabetic_master$prob >= 0.70) , 'High risk',
                            ifelse(diabetic_master$prob >= 0.30 & diabetic_master$prob <  0.70, 'Medium risk','Low risk')))

table(diabetic_master$riskbucket)

diabetic_master %>% group_by(riskbucket) %>% tally()

ggplot(diabetic_master, aes(x=riskbucket))+ 
  geom_bar() + 
  theme_pander() +
  labs(x = "Riskbucket", y = "Count", title = "Risk Bucket Distribution") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),legend.position="bottom")
