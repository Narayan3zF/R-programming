
library(caTools)
#Simple Logistic Regression:
#1.	Building a simple logistic regression model on the customer_churn dataframe, where the dependent variable is Churn & the independent variable is TechSupport

customer_churn = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\customer_churn_telecom.csv')
View(customer_churn)

customer_churn$Churn<- as.factor(customer_churn$Churn)
customer_churn$TechSupport<- as.factor(customer_churn$TechSupport)


log_mod1 <- glm(Churn ~ TechSupport, data = customer_churn, family = 'binomial')
log_mod1

summary(log_mod1)

#b.	Predicting the result when the value of TechSupport is Yes
predict(log_mod1,data.frame(TechSupport="Yes"),type="response")
#c.	Predicting the result when the value of TechSupport is No
predict(log_mod1,data.frame(TechSupport="No"),type="response")

#d.	Predicting the result when the value of TechSupport is No internet service
predict(log_mod1,data.frame(TechSupport="No internet service"),type="response")

#2.	Building a simple logistic regression model on the customer_churn dataframe, where the dependent variable is Dependents & the independent variable is tenure

customer_churn$Dependents<- as.factor(customer_churn$Dependents)

glm(Dependents~tenure, data= customer_churn, family="binomial") ->log_mod2
summary(log_mod2)
#b.	Predicting the result when the value of tenure is 10

predict(log_mod2,data.frame(tenure=10),type="response")
#c.	Predicting the result when the value of tenure is 50
predict(log_mod2,data.frame(tenure=50),type="response")

#d.	Predicting the result when the value of tenure is 70
predict(log_mod2,data.frame(tenure=70),type="response")
