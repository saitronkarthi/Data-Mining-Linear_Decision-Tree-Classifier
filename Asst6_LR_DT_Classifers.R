# Last Name: Rajamani
# First Name: Karthikeyan
# UTA Id: 1001267157
#Linear Regression Classifier
library(plyr)
library(rpart)
library(rpart.plot)
InData<-read.csv("C:\\Users\\Admin\\Desktop\\11.Data Mining-(CSE5334)\\Assignments\\Assignment6\\processed_cleveland_heartdisease.csv");
myData<-na.omit(InData)

# find the correlation between heartdiseas & age
cor(myData$heartdisease,myData$age);
# plot them for inspection
plot( myData$heartdisease~myData$age);

#split the data into K folds(10)
folds <- split(myData, cut(sample(1:nrow(myData)),10));
n=length(folds);
LMacc <- rep(NA,length(folds))
DTacc <- rep(NA,length(folds))
#K fold training & testing the classifier
for(i in 1:n){
  #attach(myData);
  test<-ldply(folds[i])
  train<-ldply(folds[-i])
  #Linear Model
  LR_train_model<-lm(heartdisease~age, data=train);
  print( LR_train_model);
  LMpred<-predict(LR_train_model,newdata=test,type="response")
  LMpred.round <- round(LMpred)
  # Genetare the confusion matrix for Linear Reg Classifier
  LMconfusion.matrix <- table(test$heartdisease,LMpred.round)
  # append acc of each iteration to the list
  LMacc[i] <- sum(diag(LMconfusion.matrix)/sum(LMconfusion.matrix))
  # print for debugging
  #cat("The accuracy is" ,acc)
  # Decision Tree Model
  DT_train_model<-rpart(heartdisease ~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal,train, method="class");
  #Test for Decision Tree
  DMpred<-predict(DT_train_model,newdata=test,type="class")
  DMpred.round<-round(as.numeric(DMpred))
  DTconfusion.matrix<-table(test$heartdisease,DMpred.round)
  DTacc[i]<-sum(diag(DTconfusion.matrix)/sum(DTconfusion.matrix))
}

cat (" The Accuracy for Kfold Linear Regression Clasifier is",mean(LMacc))
# plot the training model
plot(LR_train_model)
abline(LR_train_model,col="red");
# summarize the training model
summary(LR_train_model)
#plot the decision tree model
rpart.plot(DT_train_model)
cat (" The Accuracy for Kfold Decision Tree Clasifier is",mean(DTacc))
