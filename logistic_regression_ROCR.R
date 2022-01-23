###Questions on ROCR package:
#install.packages('caret')
#install.packages('lattice')
library('caret')
library('dplyr')
library('lattice')
library('ggplot2')

#install.packages('e1071')
library('e1071')
churn_dataset = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\dataset_load\\customer_churn_telecom.csv')
View(churn_dataset)

str(churn_dataset)
colSums(is.na(churn_dataset))
t2<- subset(churn_dataset,select=c(2:21))


colSums(is.na(t2))

which(is.na(t2$TotalCharges))
nrow(t2)

boxplot(t2$TotalCharges)

t2$TotalCharges[is.na(t2$TotalCharges)] <- median(t2$TotalCharges, na.rm = T)

sum(is.na(t2$TotalCharges))

ls(t2)

t2$Churn <- as.factor(t2$Churn)
t2$gender <- as.factor(t2$gender)
t2$SeniorCitizen <- as.factor(t2$SeniorCitizen)
t2$Partner <- as.factor(t2$Partner)
t2$Dependents <- as.factor(t2$Dependents)
t2$PhoneService <- as.factor(t2$PhoneService)
t2$MulitpleLines <- as.factor(t2$MulitpleLines)
t2$InternetService <- as.factor(t2$InternetService)
t2$OnlineSecurity <- as.factor(t2$OnlineSecurity)
t2$OnlineBackup <- as.factor(t2$OnlineBackup)
t2$DeviceProtection <- as.factor(t2$DeviceProtection)

t2$TechSupport <- as.factor(t2$TechSupport)
t2$StreamingTV <- as.factor(t2$StreamingTV)
t2$StreamingMovies <- as.factor(t2$StreamingMovies)
t2$Contract <- as.factor(t2$Contract)
t2$PaperlessBilling <- as.factor(t2$PaperlessBilling)
t2$PaymentMethod <- as.factor(t2$PaymentMethod)



View(t2)
str(t2)

#1.	Build a logistic regression model:
#a.	Start off by dividing the data-set into 'train' & 'test' sets in 80:20 ratio, with the split-criteria being determined by 'Churn' column
s2<- sample(nrow(t2), nrow(t2)*0.8)
train= t2[s2,]
test = t2[-s2,]
nrow(train)
nrow(test)

###b.	Build a logistic regression model on the train set where the dependent variable is 'Churn' & the independent variables are 'MonthlyCharges', 'tenure' & 'TechSupport' & store the result in 'log_mod_roc'

log_mod_roc_1<- glm(Churn ~. , data = train,family = 'binomial')
log_mod_roc_1


summary(log_mod_roc_1)
str(log_mod_roc_1)

log_mod_roc<- glm(Churn ~ tenure+MonthlyCharges+PaymentMethod, data = train,family = 'binomial')
log_mod_roc


summary(log_mod_roc)
str(log_mod_roc)
#c.	Predict the values on top of the test set & store the result in 'result_log_roc'
result_log_roc<-predict(log_mod_roc, newdata=test, type='response')
result_log_roc
head(result_log_roc)
head(test$Churn)

pred2 <- ifelse(result_log_roc>0.5,1,0)
pred2
test$Churn <- ifelse(test$Churn == "Yes", 1, 0)


table(test$Churn, pred2)

pred2 <- as.factor(pred2)
test$Churn <- as.factor(test$Churn)

class(pred2)
class(test$Churn)
confusionMatrix(test$Churn, pred2)

#ROC curve and AUC value
#d.	Use the performance() function from the ROCR package & build the 'Accuracy vs cut-off' plot
#e.	Plot the 'ROC' curve

#install.packages('ROCR')
library('ROCR')

predict_log_roc <- prediction(result_log_roc, test$Churn)
predict_log_roc
acc= performance(predict_log_roc, measure = 'tpr',x.measure = 'fpr')
acc
plot(acc)
#f.	Find out the "area under the curve"
auc<- performance(predict_log_roc, measure = 'auc')
print(auc)
auc = auc@y.values[[1]]
auc
