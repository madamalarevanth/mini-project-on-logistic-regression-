#predicting the cancer remission in the health care industry 

#import the cancer remission data from the .csv file 
data1 <-read.csv("C:/Users/REVANTH/Desktop/data/Remission.csv",header= TRUE,sep=",")
#to view the data 
View(data1)


#data exploration

#gives the dimensions of the data set
dim(data1)

#gives the names of the variables in the data set
names(data1)

#displays top 10 observations of the data set
head(data1)

#displays bottom top 10 observations of the data set
tail(data1)

#displays structure of the variables in the data set
str(data1)

#gives the summary of the variables of the data set 
summary(data1)


#splitting the data into training and testing data 
# sample the input data with 80% for training and 20% for testing
train_obs <- floor (0.8*nrow (data1))

set.seed(200) #to reproduce the sample
train_ind<-sample(seq_len(nrow(data1)),size=train_obs)
train_ind

test = -train_ind

train_data<-data1[train_ind,]#no of obs in train dataset
View(train_data)

test_data<-data1[-train_ind,]# no of obs in test dataset
View(test_data)

testing_high = data1$Remiss[test]
testing_high

#LOGISTIC REGRESSION EQUATION : 
  
#LOG P/1-P = B0+B1X1+B2X2+B3X3 + ........ BNXN 

#LOGP /1-P = LOGIT 

#B0 - INTERCEPT OR CONSTANT 
#B1 - SLOPE OF X1 
#X1 - FIRST INDEPENDENT VAR (CELL) 
#B2 - SLOPE OF X2 
#X2 - SECOND INDEP VAR ( SMEAR) 
#B3 - SLOPE OF X3 
#X3 - THIRD INDEP VAR ( INFIL) 

#LOG P/1-P = LOGIT 

#logistic regressiion model
regmod<-glm(Remiss~.,data = train_data,family=binomial())

summary(regmod)

#sig var is blast 
# slope is 0.83 so its pos correlated
#rsq val is 


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

#to export the data set
write.csv(final_data,"logistic_output.csv")


