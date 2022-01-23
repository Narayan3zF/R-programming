#install.packages('rpart', dependencies = T)
#install.packages('rattle', dependencies = T)
library('rpart')
library('rattle')
library('dplyr')
library(caTools)
library('caret')
library('dplyr')
library('lattice')
library('ggplot2')

#install.packages('e1071')
library('e1071')
customer_churn = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\dataset_load\\test_data\\Customer_Churn.csv')
View(customer_churn)
colSums(is.na(customer_churn))
summary(customer_churn)
str(customer_churn)
boxplot(customer_churn$TotalCharges)
#1.	Building a decision tree model:
churn_dataset<- customer_churn
churn_dataset$Dependents <- as.factor(churn_dataset$Dependents)
set.seed(1)
#a.	Dividing the data into ‘train’ & ‘test’ sets
s1<- sample(nrow(churn_dataset), nrow(churn_dataset)*0.7)
train= churn_dataset[s1,]
test = churn_dataset[-s1,]
nrow(train)
nrow(test)

str(churn_dataset)
#b.	Building the model on train set
mod_tree1=rpart(Dependents~Partner, data=train, method='class')
mod_tree1
#c.	Plotting the tree
plot(mod_tree1)
text(mod_tree1,pretty=1)

library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(mod_tree1)

printcp(mod_tree1)


mod_tree1$cptable
mod_tree1$cptable[which.min(mod_tree1$cptable[,'xerror']), 'CP']

plotcp(mod_tree1)

ptree <- prune(mod_tree1, cp=mod_tree1$cptable[which.min(mod_tree1$cptable[,'xerror']), 'CP'])

ptree

#d.	Predicting the values on the ‘test’ set
predict(mod_tree1,newdata=test,type="class")->result_tree1
head(result_tree1)

#e.	Building the confusion matrix
confusionMatrix(result_tree1,test$Dependents)
###Accuracy : 0.7151 
#or
table(test$Dependents, result_tree1)
###
#f.	Calculating the accuracy
(976+535)/(976+535+491+111)
###Accuracy : 0.715097
#####Accuracy : 0.7075248
#2.	Building the 2nd decision tree model
#a.	Building the 2nd model on train set
mod_tree2=rpart(Dependents~Partner+InternetService, data=train, method='class')
mod_tree2
#b.	Plotting the tree
plot(mod_tree2)
text(mod_tree2)


fancyRpartPlot(mod_tree2)

printcp(mod_tree2)


mod_tree2$cptable
mod_tree2$cptable[which.min(mod_tree2$cptable[,'xerror']), 'CP']

plotcp(mod_tree2)

ptree <- prune(mod_tree2, cp=mod_tree2$cptable[which.min(mod_tree2$cptable[,'xerror']), 'CP'])

ptree

#c.	Predicting the values on the ‘test’ set
predict(mod_tree2,newdata=test,type="class")->result_tree2
head(result_tree2)

#d.	Building the confusion matrix
confusionMatrix(result_tree2,test$Dependents)
####Accuracy : 0.7454 
#or
#d.	Building the confusion matrix
table(test$Dependents, result_tree2)
#e.	Calculating the accuracy
(1224+351)/(1244+351+243+295)
####Accuracy : 0.7383966 









