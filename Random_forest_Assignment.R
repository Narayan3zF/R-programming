library('caret')
library('dplyr')
library('lattice')
library('ggplot2')

#install.packages('e1071')
library('e1071')
#Load Library Random FOrest
library('randomForest')
library('rpart')
library('rattle')
library('dplyr')

library(rpart.plot)
library(RColorBrewer)
customer_churn = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\dataset_load\\test_data\\Customer_Churn.csv')
View(customer_churn)

str(customer_churn)

customer_churn$gender <- as.factor(customer_churn$gender)
#a.	Dividing the data into ‘train’ & ‘test’ sets
library(caTools)
sample.split(customer_churn$gender,SplitRatio = 0.65)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test


#Generate Random Forest learning treee
mod_forest1 <- randomForest(gender~MonthlyCharges+tenure, data=train,ntree=35)
table(predict(mod_forest1),train$gender)
print(mod_forest1)

attributes(mod_forest1)

mod_forest1$confusion

mod_forest1$classes

#b.	Building the model on train set
randomForest(gender~MonthlyCharges+tenure, data=train,ntree=35)-> mod_forest1


#c.	Getting the importance of independent variables
randomForest::importance(mod_forest1)

varImpPlot(mod_forest1)

#d.	Predicting the values on the ‘test’ set
predict(mod_forest1,newdata=test,type="class")->result_forest1
head(result_forest1)

#e.	Building the confusion matrix
table(test$gender, result_forest1)

confusionMatrix(result_forest1,test$gender)
## Accuracy : 0.5051
#e.	Calculating the accuracy
(596+649)/(596+649+625+595)
## Accuracy : 0.5050

plot(mod_forest1)

#1.	Building the 2nd Random Forest model
#a.	Building the model on train set

randomForest(gender~MonthlyCharges+tenure, data=train,ntree=350)-> mod_forest2

#b.	Getting the importance of independent variables
randomForest::importance(mod_forest2)

varImpPlot(mod_forest2)

#c.	Predicting the values on the ‘test’ set
predict(mod_forest2,newdata=test,type="class")->result_forest2
head(result_forest2)

#d.	Building the confusion matrix
table(test$gender, result_forest2)

#e.	Calculating the accuracy
(563+651)/(563+651+658+593)
###[1] 0.4924949

#or

confusionMatrix(result_forest2,test$gender)
## Accuracy : 0.4925


plot(mod_forest2)




