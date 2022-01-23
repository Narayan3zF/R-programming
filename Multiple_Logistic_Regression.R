#Multiple Logistic Regression:
#1.	Building the 1st logistic regression model
#a.	Dividing the data into ‘train’ & ‘test’ sets
library(caTools)
###
#install.packages('caret')
#install.packages('lattice')
library('caret')
library('dplyr')
library('lattice')
library('ggplot2')

#install.packages('e1071')
library('e1071')
customer_churn = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\customer_churn_telecom.csv')
#View(customer_churn)

colSums(is.na(customer_churn))


customer_churn$Dependents <- as.factor(customer_churn$Dependents)
customer_churn$gender <- as.factor(customer_churn$gender)
customer_churn$InternetService <- as.factor(customer_churn$InternetService)
customer_churn$Contract <- as.factor(customer_churn$Contract)

sample.split(customer_churn$Churn,SplitRatio = 0.65)-> split_tag

subset(customer_churn, split_tag==T)->train_data
subset(customer_churn, split_tag==F)->test_data


class(train_data$gender)
#b.	Building the 1st model on train set set where the dependent variable is 'gender' & the independent variables are 'Dependents', 'InternetService' & 'Contract' & store the result in 'log_mod_multi'
glm(gender~Dependents+InternetService+Contract, data= train_data, family="binomial") ->log_mod_multi
log_mod_multi
#c.	Predict the values on top of the test set & store the result in 'result_log_multi'
predict(log_mod_multi,newdata = test_data,type="response") -> result_log_multi
#d.	Have a look at the range of 'result_log_multi' & build a confusion matrix where the threshold of predicted values is greater than '0.49'
range(result_log_multi)
table(test_data$gender, result_log_multi>0.49)->tab
#e.	Calculate the accuracy of the model from the confusion matrix
accuracy<-sum(diag(tab))/sum(tab)
accuracy
#or 
pred1 <- ifelse(result_log_multi>0.49,1,0)
pred1
pred1 <- as.factor(pred1)
test_data$gender <- ifelse(test_data$gender == "Female", 1, 0)
test_data$gender <- as.factor(test_data$gender)
accuracy_table_1 <- table(test_data$gender, pred1)
accuracy_table_1
accuracy_matrix_1 <- confusionMatrix(test_data$gender, pred1)
accuracy_matrix_1
#2.	Build second logistic regression model on the same 'train' & 'test' sets
#a. In this case dependent variable is 'gender' & the independent variables are 'tenure', 'MonthlyCharges' & 'PaymentMethod
glm(gender~tenure+MonthlyCharges+PaymentMethod, data= train_data, family="binomial") ->log_mod_multi2
#b.	Predict the values on top of the test set & store the result in 'result_log_multi2'
predict(log_mod_multi2,newdata = test_data,type="response") -> result_log_multi2
#c.	Have a look at the range of 'result_log_multi2' & build a confusion matrix where the threshold of predicted values is greater than 0.49
range(result_log_multi2)
table(test_data$gender, result_log_multi2>0.49)->tab
tab
#d.	Calculate the accuracy of the model from the confusion matrix
pred2 <- ifelse(result_log_multi2>0.49,1,0)
pred2
pred2 <- as.factor(pred2)
test_data$gender <- ifelse(test_data$gender == "Female", 1, 0)
test_data$gender <- as.factor(test_data$gender)
accuracy_table <- table(test_data$gender, pred2)
accuracy_table
accuracy_matrix <- confusionMatrix(test_data$gender, pred2)
accuracy_matrix
