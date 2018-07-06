#1 linear regression model
#predicting the selling prices in the real estate industry 

#import the computer data from the .csv file 
data<-read.csv("C:/Users/REVANTH/Desktop/data/Computer_Data.csv")

#to view the data 
View(data)

#as the first columns of the data arent important for the prediction we drop those columns from the data frame 
#to drop the 1st variable from the data set
data<-data[-c(1)]

#data exploration

#gives the names of the variables in the data set
colnames(data)

#displays top 10 observations of the data set
head(data)

#displays bottom  10 observations of the data set
tail(data)

#displays structure of the variables in the data set
str(data)

#gives the summary of the variables of the data set 
summary(data)

#gives the dimensions of the data set
dim(data)


#library for the sample.split function
library(caTools)

#splitting the data into training and testing data 
# sample the input data with 70% for training and 30% for testing
sample<-sample.split(data$price,SplitRatio = 0.7) 

#this sample contains true and false values of data depending on the splitting ratio
sample

#assign the splitted data using subset command
training_data<-subset(data,split=="TRUE")
testing_data<-subset(data,split=="FALSE")

#multiple linear regression model 
model<-lm(price~.,data = training_data)

summary(model)

prediction<-predict(model,testing_data)
prediction

predtest1<-data.frame(prediction)

#bind it to test data
final_data <-cbind(test_data,predtest1)
final_data

write.csv(final_data,"linear_output.csv")

plot(testing_data$price,type = "l",lty = 1.8,col = "green")
lines(prediction,type = "l",col = "blue")


####
####
####
#logistic regression model
####
####

#data set is present in library AER
library(AER)

data("Affairs")
data <-Affairs

#convering the data according to the required model criterion 
aff <- ifelse(data$affairs == 0,0,1)

#binding the variable
data<- cbind(data,aff)

#renaming the variable
colnames(data)[10]<-"Affairsnew"

#skipping of the unnecessary first variable
data<-data[,c(10,2:9)]
data


#data exploration

#gives the dimensions of the data set
dim(data)

#gives the names of the variables in the data set
names(data)

#displays top 10 observations of the data set
head(data)

#displays bottom top 10 observations of the data set
tail(data)

#displays structure of the variables in the data set
str(data)

#gives the summary of the variables of the data set 
summary(data)

#splitting the data into training and testing data 
# sample the input data with 80% for training and 20% for testing
train_obs <- floor (0.8*nrow (data))

set.seed(200)#to reproduce the sample

train_ind<-sample(seq_len(nrow(data)),size=train_obs)
train_ind

test = -train_ind

train_data<-data[train_ind,]#no of obs in train dataset

test_data<-data[-train_ind,]# no of obs in test dataset

testing_high = data$Affairsnew[test]

testing_high

#logistic regressiion model
regmod<- glm(Affairsnew ~ .,data = train_data,family=binomial())

summary(regmod)

#####the sig variables are
#####years married,religiousness,rating

# predicting the model using test data
prob<-predict(regmod,test_data,type ="response")
prob

prob1<- data.frame(prob)

results <- ifelse(prob1 > 0.5,1,0)#setting the cutoff for probability values
results

#confusion matrix
table(testing_high,results)

misclassificationerror <-mean(results!=testing_high)
misclassificationerror

accurarcyrate <- 1-misclassificationerror
accurarcyrate


final_data<- cbind(test_data,prob1)

