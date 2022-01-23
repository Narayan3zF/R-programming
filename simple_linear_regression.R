library(caTools)
#Questions on simple linear regression
churn = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\customer_churn_telecom.csv')

#1.	Build a simple linear model:
#a.	Divide the data-set into train & test sets in 70:30 ratio. Splitting criteria would be determined by the 'tenure' column.
set.seed(1)
sample.split(churn$tenure,SplitRatio = 0.70)-> split_tag
View(split_tag)
table(split_tag)
subset(churn, split_tag==T)->train_data
subset(churn, split_tag==F)->test_data

View(train_data)
View(test_data)

#b.	Build a simple linear model on the 'train' set, where the dependent variable is 'tenure' & the independent variable is 'Contract' & store the result in 'model1'
lm(tenure~Contract, data=train_data)-> model1

model1
#c.	Predict the values on top of the 'test' set & store the result in 'predicted_values'
predict(model1, newdata=test_data)->predicted_values

head(predicted_values)
#d.	Bind the actual values & predicted values & store the result in 'final_data'
cbind(Actual=test_data$tenure,Predicted=predicted_values)->final_data
final_data
class(final_data)
as.data.frame(final_data) -> final_data
final_data
class(final_data)
head(final_data)
#e.	Find out the error in prediction & store the result in 'error'
final_data$Actual - final_data$Predicted ->error
head(error)
#f.	Bind 'error' to 'final_data' object
cbind(final_data,error)-> final_data

head(final_data)
#g.	Find the root mean square error
sqrt(mean((final_data$error)^2))








