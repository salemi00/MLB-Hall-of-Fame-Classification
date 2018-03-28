#DATA630 9042
#Assignment 3
#By: Sina Alemi

library(party)

#This project uses a dataset of baseball player statistics to predict whether they will be inducted into the Hall of Fame

#dataframe creation
setwd("D:/school/DATA630/module 4/ass 3")
bb<-read.csv(file='MLBHOF.csv',head=TRUE, sep=",")
str(bb)
summary(bb)

#data pre-processing
#removing unncessary variables
bb$Name<-NULL

#replacing values of '2' with '1' in Hall of Fame Membership
bb$Hall.Of.Fame.Membership<-replace(bb$Hall.Of.Fame.Membership, bb$Hall.Of.Fame.Membership==2,1)

#factorizing some variables that should be treated as categorical
bb$Hall.Of.Fame.Membership<-factor(bb$Hall.Of.Fame.Membership)
bb$Primary.Position.Played<-factor(bb$Primary.Position.Played)

#splitting data into training and test sets
set.seed(1234)
ind <- sample(2, nrow(cars), replace = TRUE, prob = c(0.7, 0.3))
train.data <- bb[ind == 1, ]
test.data <- bb[ind == 2, ]


#MODEL 1
#----------
#Decision tree model
model<-ctree(Hall.Of.Fame.Membership~., data=train.data)
print(model)

#Decision tree plot
plot(model)

#Classification Accuary
#A confusion matrix is built to compare values of predicted class vs actual class
#test data evaluation
table(predict(model, test.data), test.data$Hall.Of.Fame.Membership, dnn=c("predicted", "actual"))
#mosaic plot of test data evaluation
mosaicplot(table(predict(model, test.data), test.data$Hall.Of.Fame.Membership),shade=TRUE, xlab="Actual Values", ylab= "Predicted Values", main='Classification Accuracy')



#MODEL 2
#-------
#creating smaller training/test datasets from the original sets
smalltrain<-train.data[,2:10]
smalltrain$Stolen.Bases<-train.data$Stolen.Bases
smalltrain$Stolen.Base.Runs<-train.data$Stolen.Base.Runs
smalltrain$Hall.Of.Fame.Membership <- train.data$Hall.Of.Fame.Membership

smalltest<-test.data[,2:10]
smalltest$Stolen.Bases<-test.data$Stolen.Bases
smalltest$Stolen.Base.Runs<-test.data$Stolen.Base.Runs
smalltest$Hall.Of.Fame.Membership <- test.data$Hall.Of.Fame.Membership

#model for smaller set
smallmodel<-ctree(Hall.Of.Fame.Membership~., data=smalltrain)
print(smallmodel)
plot(smallmodel)

#evaluating the small model
#test data evaluation
table(predict(smallmodel, smalltest), smalltest$Hall.Of.Fame.Membership, dnn=c("predicted", "actual"))
mosaicplot(table(predict(smallmodel, smalltest), smalltest$Hall.Of.Fame.Membership),shade=TRUE,  xlab="Actual Values", ylab= "Predicted Values", main='Classification Accuracy')
