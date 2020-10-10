###
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

#partion data
s2<- sample(nrow(t2), nrow(t2)*0.8)
train= t2[s2,]
test = t2[-s2,]
nrow(train)
nrow(test)

###create model

churn_m1<- glm(Churn ~. , data = train,family = 'binomial')
churn_m1


summary(churn_m1)
str(churn_m1)

churn_m2<- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = train,family = 'binomial')
churn_m2


summary(churn_m2)
str(churn_m2)

p2<-predict(churn_m1, newdata=test, type='response')
p2
head(p2)
head(test$Churn)

pred2 <- ifelse(p2>0.5,1,0)
pred2
test$Churn <- ifelse(test$Churn == "Yes", 1, 0)


table(test$Churn, pred2)

pred2 <- as.factor(pred2)
test$Churn <- as.factor(test$Churn)

class(pred2)
class(test$Churn)
confusionMatrix(test$Churn, pred2)

#ROC curve and AUC value

#install.packages('ROCR')
library('ROCR')
pred2
pr<- prediction(p2, test$Churn)
pr
prf= performance(pr, measure = 'tpr',x.measure = 'fpr')
prf
plot(prf)

auc<- performance(pr, measure = 'auc')
print(auc)
auc = auc@y.values[[1]]
auc
