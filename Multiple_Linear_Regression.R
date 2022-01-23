#Multiple Linear Regression:
#1.	Building a multiple linear regression model
#a.	Divide the data-set into train & test sets
library(caTools)

customer_churn = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\customer_churn_telecom.csv')
set.seed(1)
sample.split(customer_churn$MonthlyCharges,SplitRatio = 0.75)-> split_model
subset(customer_churn, split_model==T)->train_data
subset(customer_churn, split_model==F)->test_data

nrow(train_data)
nrow(test_data)
#b.	Building the multiple linear model
names(customer_churn)
lm(MonthlyCharges~Dependents+MultipleLines+OnlineSecurity+OnlineBackup+DeviceProtection, data=train_data)-> mod_multi_linear

mod_multi_linear
#c.	Predicting the values on top of the test set

predict(mod_multi_linear, newdata=test_data)->predicted_multi_linear

head(predicted_multi_linear)
#d.	Binding the actual values & the predicted values
cbind(Actual=test_data$MonthlyCharges,Predicted=predicted_multi_linear)->final_data
final_data
as.data.frame(final_data) -> final_data
final_data
head(final_data)
#e.	Finding the error in prediction
final_data$Actual - final_data$Predicted ->error
head(error)

#f.	Binding the error to final_data object
cbind(final_data,error)-> final_data

head(final_data)

#g.	Finding the root mean square error
sqrt(mean((final_data$error)^2))




