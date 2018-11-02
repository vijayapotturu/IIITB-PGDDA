########################################################
#############Handwritten Digit recognition.  ###########
########################################################

# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

############################################################

### 1. Business Understanding: 

#A classic problem in the field of pattern recognition is that of handwritten digit recognition. 
#Suppose that you have an image of a digit submitted by a user via a scanner, a tablet, or other digital devices.
#The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image. 

### Objective:
#develop a model using Support Vector Machine which should correctly classify the handwritten digits 
#based on the pixel values given as features.

##############################################################

#Loading Neccessary libraries

# Check libraries and install and load required libraries
options(warn = -1)
libs = c("readr","plyr","dplyr","DescTools",
         "ggthemes","ggplot2","gridExtra", "cowplot","formattable","splitstackshape",
         "caret", "kernlab","caTools","doParallel")
install.lib <- libs[!libs %in% installed.packages()]
for (pkg in install.lib)
  install.packages(pkg, dependencies = T)

loadlib     <- lapply(libs, library, character.only = T) # load them

remove(list = ls())
options(warn = 0)

### Set directory to load file -- setwd("E:Upgrad/Course 4/SVM/Assignment/SVM Dataset")

### Data Loading - read the CSV file into R, for processing. View the structure and understand data

### Importing the datasets

#import the training data .
digit_train <- read.csv("mnist_train.csv",stringsAsFactors = FALSE,header = FALSE)

#Import the mnist test data
digit_test <- read.csv("mnist_test.csv",stringsAsFactors = FALSE,header = FALSE)


#####################################################################################
# 2.Data understanding                                                               #
#####################################################################################
## View the structure of train and test dataframes

str(digit_train)
dim(digit_train)
#train data set has Observations: 785 variables, 60000 observations.
str(digit_test)
dim(digit_test)
# test data set has Observations: 785 variables, 10000 observations.

#printing and checking first few rows

head(digit_train[ ,1:10])
head(digit_test[ ,1:10])
#printing and checking last few rows
tail(digit_train[,1:10])
tail(digit_test[,1:10])

#Checking for missing values
anyNA(digit_train)
anyNA(digit_test)
sum(is.na(digit_train))
#sapply(digit_train, function(x) sum(is.na(x)))
#no NA&missing values for training data
sum(is.na(digit_test))
#no NA & missing values for test data as well

#Check blank values
sapply(digit_train, function(x) length(which(x == ""))) # No blank values in train dataset
sapply(digit_test, function(x) length(which(x == ""))) # No blank values in test dataset

# checking for duplicates rows in test and train combined set.

sum(duplicated(rbind(digit_train, digit_test))) # No duplicates in combined dataset

#renaming the columns for both test data and training data
colnames(digit_train)[1] <- "label"
colnames(digit_train)[2:785] = c(paste0("pix", 2:785))
head(digit_train[ ,1:10])
tail(digit_train[ ,1:10])
#glimpse(digit_train)
#glimpse(digit_test)
colnames(digit_test)[1] <- "label"
colnames(digit_test)[2:785] = c(paste0("pix", 2:785))
head(digit_test[ ,1:10])
tail(digit_test[ ,1:10])

# checking if some features have same value in them
(cols_with_same_value <- which(sapply(rbind(digit_train, digit_test),function(x) length(unique(x)) == 1)))
length(cols_with_same_value)
(zero_value_cols  <- which(colSums(rbind(digit_train, digit_test)) == 0))
# there are 65 columns which has same data in both the datasets i.e 0 
# so assuming this may not give any differance because these are handwritten images.
# even whether we remove or not in the model prediction so not removing from datasets.
#cols_with_same_value
#zero_value_cols

digit_train[,c(2:785)] <- sapply(digit_train[,c(2:785)],function(x) as.numeric(x))
digit_test[,c(2:785)] <- sapply(digit_test[,c(2:785)],function(x) as.numeric(x))

d <- summary(digit_train)
View(d)

which(rbind(digit_train,digit_test)> 255)
# No value of pixel is greater than 255
#After checking the summary of the dataframe digit_train it is evident all the columns have ranges between minum value 0 and 
#maximum value 255. No column is exceeding 255.

which(rbind(digit_train,digit_test) %>% select(label) > 9 | rbind(digit_train,digit_test) %>% select(label) < 0)
# No number given is more than 9 or less than zero.

#Making our target class to factor

digit_train$label <- as.factor(digit_train$label)
digit_test$label <- as.factor(digit_test$label)
str(digit_train)
#str(digit_test)

#check the Distribution percentage of each digit in both the data sets
table(digit_train$label)/nrow(digit_train) *100  
table(digit_test$label)/nrow(digit_test) *100
# Both test & train datasets have uniform & similar distribution of each digit in the both the datasets

###########################        EDA           ##############################

### Check the Distrubution of Target Variable

ggplot(digit_train,aes(x = label,fill=label))+geom_bar()
#### Most of the classes are proportionate and having the 0-9 classes

ggplot(digit_test,aes(x = label,fill=label))+geom_bar()

#### distribution if the datasets in Training and having 0-9 classes and looks similar.

digit_eda <- digit_train

# check and visualize each digits by taking some random sample observations from the rain dataset.
for (i in seq(0, 9)) {
  sample <- digit_eda[digit_eda$label == i, ]
  sample <-  sample[ ,-1]
  par(mar = c(1,1,1,1))  
  par(mfrow = c(5,10)) 
  for (j in seq(10, 5000, by = 100)) {
    digit <- t(matrix(as.numeric(sample[j, ]), nrow = 28)) 
    image(t(apply(digit, 2, rev)), col = grey.colors(255))
  }
}

#verifying by plotiing image of any random number,first creating a matrix of 28 by 28 and then using image function to plot the number
#8 inverse
m1 <- matrix(as.numeric(digit_eda[56,-1]),nrow=28)
image(m1,col=heat.colors(55))
#6 inverse
m2 <- matrix(as.numeric(digit_eda[67,-1]),nrow=28)
image(m2,col=heat.colors(255))

########################  Derive New variables   ##############################

#calculate average intensity of each number 0 to 9 in a row , first calcuate average mean of pixels of each row
#and then aggregate for each number 

digit_eda$intensity <- apply(digit_eda[,-1], 1, mean) #takes the mean of each row in training set

#computitng the average intesnity of each digit 0 to 9
intensity <- aggregate(digit_eda$intensity,by=list(digit_eda$label),FUN=mean)

# test data set.
digit_eda_test <- digit_test
digit_eda_test$intensity <- apply(digit_test[,-1], 1, mean) #takes the mean of each row in test set

### visualising the digit intensity in train dataset.
ggplot(intensity,aes(x=factor(Group.1),y=x)) + geom_bar(stat = "identity",fill="darkblue")
#plot shows number 0,8 have highest intensity followed by 2 and 3.
#1 has least intensity

ggplot(digit_eda,aes(x=intensity)) + geom_histogram(binwidth = 1.1) + facet_wrap(~label)
#intensity graph shows almost all the numbers intensity distribution in pixel are normally distributed,however for
#digit 1 variations in the graph from normal distribution.

#Checking distribution of few digits
p0 <- qplot(subset(digit_eda, label ==0)$intensity, binwidth = .75, xlab = "Intensity Hist for 0")
p1 <- qplot(subset(digit_eda, label ==1)$intensity, binwidth = .75, xlab = "Intensity Hist for 1")
p2 <- qplot(subset(digit_eda, label ==2)$intensity, binwidth = .75, xlab = "Intensity Hist for 2")
p3 <- qplot(subset(digit_eda, label ==3)$intensity, binwidth = .75, xlab = "Intensity Hist for 3")
p4 <- qplot(subset(digit_eda, label ==4)$intensity, binwidth = .75, xlab = "Intensity Hist for 4")
p5 <- qplot(subset(digit_eda, label ==5)$intensity, binwidth = .75, xlab = "Intensity Hist for 5")
p6 <- qplot(subset(digit_eda, label ==6)$intensity, binwidth = .75, xlab = "Intensity Hist for 6")
p7 <- qplot(subset(digit_eda, label ==7)$intensity, binwidth = .75, xlab = "Intensity Hist for 7")
p8 <- qplot(subset(digit_eda, label ==8)$intensity, binwidth = .75, xlab = "Intensity Hist for 8")
p9 <- qplot(subset(digit_eda, label ==9)$intensity, binwidth = .75, xlab = "Intensity Hist for 9")
grid.arrange(p0,p1, p2, p3,p4,p5,p6,p7,p8,p9, ncol = 3)

# all the digits looks normally distributed 

#### Distrubution of Target variable i.e label

Desc(digit_eda$label)
Desc(digit_eda_test$label)

#computitng the average intesnity of each digit 0 to 9 in test dataset
intensity_test <- aggregate(digit_eda_test$intensity,by=list(digit_eda_test$label),FUN=mean)

ggplot(intensity_test,aes(x=factor(Group.1),y=x)) + geom_bar(stat = "identity",fill="darkblue")
#plot shows number 0,8 have highest intensity followed by 2 and 3.
#1 has least intensity

plot_grid(ggplot(digit_eda,aes(x = label, y = intensity, fill = label)) + 
            geom_violin(show.legend = F) + 
            labs(x = "Labels", title = "Avg. Intensity Distribution in Train Dataset") +theme_pander(),
          ggplot(digit_eda_test,aes(x = label, y = intensity, fill = label)) + 
          geom_violin(show.legend = F) + 
          labs(x = "Labels", title = "Avg. Intensity Distribution in Test Dataset")+theme_pander(),nrow=2)

###Intenisity distribtuions seems to be normal in both test and train datasets there are variations in 0,1  and 7.

#####################################################################################
                                   ####Model Building###
#####################################################################################
# Since the dataset is huge lets do a stratified sampling that can be used for our modeling 
# Lets check the proportion of the target variables in the dataset
percent(prop.table(table(digit_train$label)))
percent(prop.table(table(digit_test$label)))
# The proportion comes on an average of 10 % in both test and train datasets

set.seed(100)

indices <- sample.split(1:nrow(digit_train),0.15*nrow(digit_train))

train_sample <- digit_train[indices,]
#train.indices = sample(1:nrow(DATA), 0.7*nrow(DATA))
#train_sample = DATA[train.indices, ]
#test = DATA[-train.indices, ]

#train_sample <- stratified(digit_train, "label", size = 500)

# Lets again check the proportion of the target variables in the Datasets
percent(prop.table(table(train_sample$label)))

# The proportion comes to 10 % across all the labels in train dataset
# matching closely with the proportion of the original dataset.

####building the Model using train_sample dataset

#####################################################################
###Linear model - SVM 
#####################################################################
(Model_linear <- ksvm(label~ ., data = train_sample, scale = FALSE, kernel = "vanilladot"))
#proc.time()
###there are warning related to scaling we can ignore these.
#### Linear Model 
# parameter : cost C = 1 
# Number of Support Vectors : 2514

#####################################################################
#Using RBF Kernel
#####################################################################
(Model_RBF <- ksvm(label~ ., data = train_sample, scale = FALSE, kernel = "rbfdot"))
# Model_RBF
# There are warnings regarding Scaling of Data. but we can ignore these.
#### RBF Model Metrics ####
# parameter : cost C = 1   
# Hyperparameter : sigma =  1.64589960895441e-07 
# Number of Support Vectors : 3516

#####################################################################
#Using polynomial Kernel
#####################################################################
(Model_poly <- ksvm(label~ ., data = train_sample, scale = FALSE, kernel = "polydot"))
# Model_poly
# There are warnings regarding Scaling of Data. but we can ignore these.
#### RBF Model Metrics ####
# parameter : cost C = 1   
# Hyperparameter : degree =  1  scale =  1  offset =  1  
# Number of Support Vectors : 2514

#####################################################################################
                              ####Model Evaluation####
#####################################################################################

#####################################################################
#### SVM Linear Model Evaluation ####
#####################################################################
# check with digit_test dataset

Eval_linear_sample<- predict(Model_linear, train_sample)
confusionMatrix(Eval_linear_sample,train_sample$label)

#Eval_linear_sample_test<- predict(Model_linear, test)
#confusionMatrix(Eval_linear_sample_test,test$label)

Eval_linear_train<- predict(Model_linear, digit_train)
#confusionMatrix(Eval_linear_train,digit_train$label)
(confmat_train <- confusionMatrix(Eval_linear_train,digit_train$label))

colMeans(confmat_train$byClass)

# Accuracy    : 0.9207
# Sensitivity : 0.91946249 
# Specificity : 0.99118940

# check with digit_test dataset

Eval_linear_test<- predict(Model_linear, digit_test)
#confusionMatrix(Eval_linear_test,digit_test$label)
(confmat_test <- confusionMatrix(Eval_linear_test,digit_test$label))
colMeans(confmat_test$byClass)
# Accuracy    : 0.9093
# Sensitivity : 0.9076850   
# Specificity : 0.9899278 

# The Accuracy is almost consistent between the whole train and test datasets

#####################################################################
#### RBF Model Evaluation ####
#####################################################################

Eval_RBF_sample<- predict(Model_RBF, train_sample)
confusionMatrix(Eval_RBF_sample,train_sample$label)
#Accuracy : 0.9816

Eval_RBF_train<- predict(Model_RBF, digit_train)
#confusionMatrix(Eval_RBF_train,digit_train$label)
(confmattrain <- confusionMatrix(Eval_RBF_train,digit_train$label))
colMeans(confmattrain$byClass)
# Accuracy    : 0.954397
# Sensitivity : 0.95951808 
# Specificity : 0.99552931

Eval_RBF_test<- predict(Model_RBF, digit_test)
#confusionMatrix(Eval_RBF_test,digit_test$label)

(confmattest <- confusionMatrix(Eval_RBF_test,digit_test$label))
colMeans(confmattest$byClass)
# Accuracy    : 0.9568
# Sensitivity : 0.9564314   
# Specificity : 0.9952027

# The Accuracy is consistent between the whole train and test datasets

#####################################################################
#### poly Model Evaluation ####
#####################################################################


Eval_poly_sample<- predict(Model_poly, train_sample)
confusionMatrix(Eval_poly_sample,train_sample$label)
#Accuracy : 1

# check with digit_train dataset
Eval_poly_train<- predict(Model_poly, digit_train)
#confusionMatrix(Eval_linear_train,digit_train$label)
(confmat_train <- confusionMatrix(Eval_poly_train,digit_train$label))
#Accuracy : 0.9207
colMeans(confmat_train$byClass)
# Sensitivity : 0.91946249   
# Specificity : 0.99118940 

# check with digit_test dataset

Eval_poly_test<- predict(Model_poly, digit_test)
#confusionMatrix(Eval_linear_test,digit_test$label)
(confmat_test <- confusionMatrix(Eval_poly_test,digit_test$label))
colMeans(confmat_test$byClass)
# Accuracy    : 0.9093
# Sensitivity : 0.9076850   
# Specificity : 0.9899278 

############   Hyperparameter tuning and Cross Validation #####################

### hyperparameter tuning and Cross validation is done for RBF model only because from the 
### above models RBF model accuracy is better than linear and ploynomial.

no_cores <- detectCores() 
cl <- makeCluster(no_cores)
cl
registerDoParallel(cl)
# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
### RBF Model c =1 and sigma = 1.64e-7 so let's take c= 3 and sigma values as 0.64e-7, 1.64e-7, 2.64e-7, 3.64e-7 
#(i.e -1+sigma, sigma and 1+sigma)
grid <- expand.grid(.sigma=c(0.64e-7, 1.64e-7, 2.64e-7, 3.64e-7), .C=c(1,2,3) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

(fit.svm <- train(label~., data=train_sample, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl))

#this particular step for 15 % of data i.e 9000 records even after 6 hours also no result 
#so I have to use multiple threads explicit method.
# There are warnings regarding Scaling of Data. we can ignore these.this step is taking very long time.

# sigma     C  Accuracy  Kappa    
#6.40e-08  1  0.9332240  0.9257643
#6.40e-08  2  0.9407802  0.9341677
#6.40e-08  3  0.9447798  0.9386150
#1.64e-07  1  0.9534460  0.9482498
#1.64e-07  2  0.9574453  0.9526961
#1.64e-07  3  0.9601120  0.9556600
#2.64e-07  1  0.9598903  0.9554143
#2.64e-07  2  0.9638902  0.9598602
#2.64e-07  3  0.9656668  0.9618354
#3.64e-07  1  0.9632237  0.9591201
#3.64e-07  2  0.9665567  0.9628249
#3.64e-07  3  0.9673338  0.9636889

print(fit.svm)

plot(fit.svm)

stopCluster(cl)

### Accuracy was used to select the optimal model using the largest value.
### The final values used for the model were sigma = 3.64e-07 and C = 3

###########################    Final model building    #######################

#Build our final model with C = 2 and sigma = 3.62e-07
(RBF_model_final  <- ksvm(label~ ., data = train_sample, 
                         scaled = F, C = 3, kpar = list(sigma = 3.64e-7),
                         kernel = "rbfdot"))
#parameter : cost C = 3
#Number of Support Vectors : 4163 
#Hyperparameter : sigma =  3.64e-07

###########################    Final model evaluation   #######################

RBF_model_final_eval_train_sample  <- predict(RBF_model_final, train_sample)
(confmat_train_sample <- confusionMatrix(RBF_model_final_eval_train_sample, train_sample$label))
# Accuracy : 0.9999

# Lets check at other metrics : 
colMeans(confmat_train_sample$byClass)
#Sensitivity          Specificity        
#0.99988926           0.9999875

RBF_model_final_eval_train  <- predict(RBF_model_final, digit_train)
(confmat_train <- confusionMatrix(RBF_model_final_eval_train, digit_train$label))
# Accuracy : 0.9742

# Lets check at other metrics : 
colMeans(confmat_train$byClass)
#Sensitivity          Specificity        
#0.9740904           0.9971359

RBF_model_final_eval_test   <- predict(RBF_model_final, digit_test)

(confmat_test <-  confusionMatrix(RBF_model_final_eval_test, digit_test$label))
#Accuracy : 0.9691
colMeans(confmat_test$byClass)

#Sensitivity          Specificity       
#0.9688687            0.9965674


###############################  Final Summary  ############################## 

### Final model is a Non linear model : RBF model (RBF_model_final) 
### after performing Cross validation

### Finalized Hyperparameters : 
### C     = 3  Sigma = 3.64e-07 

# Evaluation metrics on complete Train dataset with 60000 observations 
# Accuracy  : 0.9742  Kappa :  0.9713

# Average values across all classes 
# Sensitivity          Specificity    Balanced Accuracy          
# 0.9740904            0.9971359       0.9856132

# Evaluation metrics on Test dataset with 10000 observations 
# Accuracy  : 0.9691 Kappa : 0.9657

# Sensitivity          Specificity    Balanced Accuracy   
# 0.9688687             0.9965674       0.9827181

# The test accuracy is the almost same as train so this indicates model is a good model.
#  no overfitting or underfitting and our model is able to predict correct digits 
#  using Non-Linear SVM to a large extent.

#************************************End of file***************************
