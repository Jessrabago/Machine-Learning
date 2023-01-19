#Assignment 2_2 .R

#libraries
library(missForest)
library(Hmisc)
library(VIM)
library(rpart)
library(rpart.plot)
library(rattle)
library(dplyr)
library(caret)


####################### First load the csv file
data_wisc = read.csv("wisconsin.csv", header=TRUE,sep = ',')
#print the first 6 values from the dataset
head(data_wisc)



##################Pre--processing data
##some data are empty NA, let see how many 
sum(is.na(data_wisc))
#after knew that are 16 NA we make a imputation

###imputation with a "mean"value
#Copy data into another variable
imputation_wisc=data_wisc
#impute with mean value
#the values will display with a *
impute.data1_mean=with(imputation_wisc, impute(imputation_wisc$Bare.nuclei, mean))

#add mean values to the dataset
imputation_wisc$Bare.nuclei = ifelse(is.na(imputation_wisc$Bare.nuclei),
                           ave(imputation_wisc$Bare.nuclei, FUN = function (x)mean(x, na.rm = TRUE)),
                           imputation_wisc$Bare.nuclei)
#we do not want decimal numbers lets round it
imputation_wisc$Bare.nuclei=round(imputation_wisc$Bare.nuclei)

#verify that are not NA's
sum(is.na(imputation_wisc))
head(imputation_wisc)
str(imputation_wisc)

#change Class to number
new_wisc=imputation_wisc
#use factor to categorize the data
new_wisc$Class=factor(new_wisc$Class,
                           levels = c("benign", "malignant"),
                           labels = c(0, 1))
#to use lm needs to change factor 
new_wisc$Class=as.numeric(as.character(new_wisc$Class))


###let see independent variables
#on the distribution 

par(mfrow = c(5,2))
boxplot(Cl.thickness~Class, ylab="Cl.thickness", xlab= "Class", col="yellow",data = new_wisc)
boxplot(Bl.cromatin~Class, ylab="Bl.cromatin", xlab= "Class", col="yellow",data = new_wisc)
boxplot(Cell.shape~Class, ylab="Bl.cromatin", xlab= "Class", col="yellow",data = new_wisc)
boxplot(Marg.adhesion~Class, ylab="Bl.cromatin", xlab= "Class", col="yellow",data = new_wisc)
boxplot(Epith.c.size~Class, ylab="Bl.cromatin", xlab= "Class", col="yellow",data = new_wisc)
boxplot(Bare.nuclei~Class, ylab="Bl.cromatin", xlab= "Class", col="yellow",data = new_wisc)
boxplot(Normal.nucleoli~Class, ylab="Bl.cromatin", xlab= "Class", col="yellow",data = new_wisc)
boxplot(Mitoses, ylab="Bl.cromatin", xlab= "Class", col="yellow",data = new_wisc)
boxplot(Cell.size~Class, ylab="Bl.cromatin", xlab= "Class", col="yellow",data = new_wisc)
#it is higher for people that have a malign class in all the variables



##########################train and test data
#copy data in new variable
train_test_data=new_wisc
#test code
Class=train_test_data$Class
train_test_data1=scale(new_wisc[1:9])
train_test_data=cbind(train_test_data1,Class)
train_test_data=data.frame(train_test_data)


#using Index to divide the data
#select  75% for training data and 25% for test data
index= sample(nrow(train_test_data), 0.75 * nrow(train_test_data))
#training data
train_data= train_test_data[index,]
#test data
test_data=train_test_data[-index,]



#######################create model 
#tree model
#from course train the data
par(mfrow = c(1,1))
treemodel = rpart(Class ~ . , data=train_data)
plot(treemodel, margin=0.25)
text(treemodel, use.n = T)

#test the data
fancyRpartPlot(treemodel)
prediction = predict(treemodel, newdata=test_data, type='class')
table(prediction, test_datat$Class)




#####################linear regression model (LM)
#train the model
lm_data=lm(train_data$Class ~., data=train_data)
summary(lm_data)
#verify the data obtained  
# insert the data test 
predicted_data=predict(lm_data, newdata = test_data)
summary(predicted_data)





#########################PCA
#Data reduction using PCA
pca_data=train_test_data
#verify data types
str(pca_data)
pca_result=prcomp(pca_data, scale = TRUE)
summary(pca_result)
#We shall apply Kaisers criterion to see how components to keep
pca_result$sdev^2
#According to Kaisers rule we keep any Principal Component with a 
#Eigenvalue of 1.0 or larger, The Jolliffe rule selects a value of 0.7
#in this case we keep with 1 and 2

##screeplot
screeplot(pca_result, type="lines",col=3)

#if we just check the first 4 first variables
lm_w_PCA=lm(train_data$Class ~train_data$Cl.thickness+train_data$Cell.size+train_data$Cell.shape+train_data$Marg.adhesion,
                        data=train_data)


summary(lm_w_PCA)
#the r squared is lower
#it is not a good model







##############################cross validation
# fit linear model using 10-fold cross-validation
set.seed(123)
lm_model = train(Class ~. , train_data, method="lm", 
                 trControl = trainControl(method="cv", number=10, verboseIter = TRUE))

print(lm_model)
#same rsquare or no much variation

#repeat cross-validation
lm_model = train(Class ~. , train_data, method="lm", 
                 trControl = trainControl(method="repeatedcv", number=10, repeats=3, verboseIter = TRUE))

print(lm_model)
