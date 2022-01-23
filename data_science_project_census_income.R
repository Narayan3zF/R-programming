library(dplyr)


#####################################1. Data Preprocessing#####################################################################
#load Data and Replace all the missing values with NA
census_income = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\dataset_load\\test_data\\census-income.csv',na.strings = c(" ?","NA"))


summary(census_income)
colSums(is.na(census_income))
#Rename X as income
census_income %>% 
  rename(
    income = X
  ) -> census
#b) Remove all the rows that contain NA values
census <- na.omit(census)
#c)Remove all whitespaces from the columns
census <- as.data.frame(apply(census,2,function(x)gsub('\\s+', '',x)))
str(census)
summary(census)
barplot(table(census$relationship),main = 'Income Classification',col='blue',xlab ='Categories of Relationships')
#######datasets for logistics,linear,decision& randomforest#################################################################
logistic_census <- census
decision_tress<-census
random_forest<-census
####################################2.Data Manipulation###########################################################################
#a) Extract the 'education' column and store it in 'census_ed'
census$education -> census_ed
census_ed
#b)Extract all the columns from 'age' to relationship and store it in census_seq
census %>% select(1:8) -> census_seq
head(census_seq)
#c)Extract columns number 5,8,11 and store it in census_col
census %>% select(5,8,11) -> census_col
head(census_col)

#d)Extract all the male employess who work in state_gov and store it in male_govcolumns number 5,8,11 and store it in census_col
census_col <- filter(census, sex=="Male" &  workclass == "State-gov")
head(census_col)
#census$native.country
#e)Extract all the 39 year olds who either have abachelor's degree or who are native of unites States and store the result in census_us
census_us <- filter(census, age== 39 & (education=='Bachelors' & native.country == "United-States"))
head(census_us)
#f)Extract 200 random rows from the census census from and store it in census_200
census %>% sample_n(200) -> census_200
head(census_200)
#g) Get the count of different levels of the workclass column
census %>% count(workclass) -> count_workclass
count_workclass

#h) Calculate the mean of capital.gain column grouped according to workclass
census %>% group_by(workclass) %>% summarise(mean(as.numeric(capital.gain)))

####################################3.Data Visualization###########################################################################
#a) Build a bar-plot for the relationship column and fill the bars according to the race column
library(ggplot2)
#i) Set x-axis label to Categories of Relationshipd
ggplot(census,aes(relationship)) + geom_bar()+xlab('relationship')
#ii) Set y-axis label to Categories of Count of Categories
ggplot(census,aes(relationship)) + geom_bar()+xlab('relationship')+ylab('Count of Categories')
#iii)Fill the bars according to Sex
ggplot(census,aes(relationship,fill=sex)) + geom_bar()+xlab('relationship')+ylab('Count of Categories')
#iv).	Set the position of bars to dodge 
ggplot(census,aes(relationship,fill=sex)) + geom_bar(position = position_dodge2(preserve = "single"))+xlab('relationship')+ylab('Count of Categories')
#v).	Set the title of plot to be Distributeion of Relationships by Sex
ggplot(census,aes(relationship,fill=sex)) + geom_bar(position = position_dodge2(preserve = "single"))+ ggtitle('Distribution of Relationships by Sex')+xlab('relationship')+ylab('Count of Categories')

#b) Build a Histogram for the age column with number of bins equal to 50
census$age <- as.numeric(census$age)
#i) Fill the bars of the histogram according to yearly income column i.e X = income
ggplot(census, aes(x=age,fill=income))+geom_histogram(fill='mediumspringgreen', col='azure',bins = 50)
#ii)Set the title of the plot to Distribution of Age
ggplot(census, aes(x=age,fill=income))+geom_histogram(fill='mediumspringgreen', col='azure',bins = 50)+ggtitle('Distribution of Age')
#iii)Set the legend title to yearly income
ggplot(census, aes(x=age,fill=income))+geom_histogram(fill='mediumspringgreen', col='azure',bins = 50)+ggtitle('Distribution of Age')+ labs(title = "Yearly income")
#iv) Set the theme of the plot to black and white
ggplot(census, aes(x=age,fill=income))+geom_histogram(fill='mediumspringgreen', col='azure',bins = 50)+ggtitle('Distribution of Age')+ labs(title = "Yearly income")+theme(panel.background = element_rect(fill = "black"))+theme(plot.background = element_rect(fill="white"))+facet_grid(~income)

#c) Build a scatter-plot between capital.gain & hours.per.week.Map capital.gain on the X- axis and hours.per.week on the y-axis.
#i) Set the transparency of the points to 40% and size as 2.
library(tidyr)
#i) Set the transparency of the points to 40% and size as 2
ggplot(census, aes(y=hours.per.week,x=capital.gain,alpha = 0.4, size = 2)) +  geom_point() 
#ii) Set the color of the points according to the X yearly income column
ggplot(census, aes(y=hours.per.week,x=capital.gain,col=income, alpha = 0.4, size = 2)) +  geom_point() 
#iii) Set the X-axis label to Capital Gain, y-axis label to Hours per Week, title to Capoital Gain vs Hours per Week by Income, and legend label to Yearly income
ggplot(census, aes(y=hours.per.week,x=capital.gain,col=income, alpha = 0.4, size = 2))+ ggtitle('Capoital Gain vs Hours per Week')+xlab('Capital Gain')+ylab('Hours per Week')+ theme(legend.title = element_text(colour="blue", size=10,face="bold"))+ geom_point() 
  
#d)Build a box-plot between education and age column. Map education on the x-axis and age on the y-axis
#i) Fill the box-plots according to the sex column.
ggplot(census,aes(y=age,x=education,fill=sex))+geom_boxplot()
#11)Set the title to Box-Plot of age by Education and sex.
ggplot(census,aes(y=age,x=education,fill=sex))+ ggtitle('Box-Plot of age by Education and sex')+geom_boxplot()

####################################4.Linear Regression###########################################################################
#q) Build a simple linear regression model as follows:
#i) Divide the dataset into training and test sets in 70:30 ratio
library('plotrix')
library('caTools')
set.seed(1)
lr_divide = sample.split(census$income, SplitRatio = 0.70)
lr_divide

lr_train=subset(census,lr_divide==TRUE)
lr_test = subset(census, lr_divide==FALSE)
nrow(lr_train)
nrow(lr_test)


lr_model = lm(hours.per.week ~ education.num, data=lr_train)

summary(lr_model)
#iii) Predict the values on the train set and find the error in prediction

lr_prediction = predict(lr_model, newdata = lr_test)
head(lr_prediction)

lr_df_prediction<- data.frame(Actual = as.numeric(lr_test$hours.per.week), Predicted = lr_prediction)


error <- lr_df_prediction$Actual -lr_df_prediction$Predicted
error
# binding actual and error
lr_df_prediction_bind <- cbind(lr_df_prediction, error)
#iv) Find the root-mean-square error
rmsel <- sqrt(mean((lr_df_prediction_bind$error)^2))
rmsel

mse <-  mean((lr_df_prediction_bind$error)^2)
mse

ggplot(census,aes(x = education.num, y = hours.per.week )) + geom_point(size = 1)+geom_smooth(method = "lm")


#plot of predicted Vs error

ggplot(lr_df_prediction, aes(x = Predicted, y =error)) + geom_point()

qqnorm(lr_df_prediction_bind$error)
####################################5.Logistic Regression###########################################################################
#a) Build A simple logistic regression model as follows:
#i) Divide the dataset into training and test sets in 65:35 ratio.
library('caret')

library('lattice')
library('ggplot2')

#install.packages('e1071')
library('e1071')
set.seed(1)
logistic_census$income <-as.factor(logistic_census$income)
glr_divide = sample.split(logistic_census$income, SplitRatio = 0.65)
glr_divide

glr_train=subset(logistic_census,glr_divide==TRUE)
glr_test = subset(logistic_census, glr_divide==FALSE)
nrow(glr_train)
nrow(glr_test)


#ii)Build a logistic regression model where the dependent variable is yearly income & independent variable is occupation
str(logistic_census)
log_mod_roc_1<- glm(income ~occupation, data = glr_train,family = 'binomial')
log_mod_roc_1

#iii) Predict the values on the test set.
predict(log_mod_roc_1,newdata = glr_test,type="response") -> result_log


#iv) Plot accuracy vs cut-off and pick an ideal value for cut-off
library(ROCR)
prediction(result_log,glr_test$income) -> predict_log_roc

performance(predict_log_roc,"acc")->acc

plot(acc)
#v)Build a confusion matrix and find the accuracy
test_income = as.character(glr_test$income)

pred2 <- ifelse(result_log>0.3,1,0)
pred2
test_income <- ifelse(test_income=='>50K',1,0)

confusionMatrix(as.factor(test_income), as.factor(pred2))
table(glr_test$income, result_log>0.3)->tab
accuracy<-sum(diag(tab))/sum(tab)
accuracy

#VI)Plot the ROC curve and find the auc(Area under Curve)


predict_log_roc <- prediction(result_log,glr_test$income)
predict_log_roc
acc= performance(predict_log_roc, measure = 'tpr',x.measure = 'fpr')
acc
plot(acc)
#Find out the "area under the curve"
auc<- performance(predict_log_roc, measure = 'auc')
print(auc)
auc = auc@y.values[[1]]
auc
#b)Build a multiple logistic regression model as follows:
#i) Divide the dataset into training and test sets in 80:20 ratio.
set.seed(1)
#logistic_census$income <-as.factor(logistic_census$income)
m_glr_divide = sample.split(logistic_census, SplitRatio = 0.8)
m_glr_divide

m_glr_train=subset(logistic_census,m_glr_divide==TRUE)
m_glr_test = subset(logistic_census, m_glr_divide==FALSE)
nrow(m_glr_train)
nrow(m_glr_test)


#ii)Build a logistic regression model where the dependent variable is yearly income & independent variable is age,workclass,education
str(logistic_census)
log_mod_roc_2<- glm(income ~age+workclass+education, data = m_glr_train,family = 'binomial')
log_mod_roc_2

#iii) Predict the values on the test set.
predict(log_mod_roc_2,newdata = m_glr_test,type="response") -> result_log_multi


#iv) Plot accuracy vs cut-off and pick an ideal value for cut-off

prediction(result_log_multi,m_glr_test$income) -> predict_log_roc

performance(predict_log_roc,"acc")->acc

plot(acc)
#v)Build a confusion matrix and find the accuracy
test_m_income = as.character(m_glr_test$income)

pred2 <- ifelse(result_log_multi>0.49,1,0)
pred2
test_m_income <- ifelse(test_m_income=='>50K',1,0)

confusionMatrix(as.factor(test_m_income), as.factor(pred2))

table(m_glr_test$income, result_log_multi>0.49)->tab_multi
accuracy<-sum(diag(tab_multi))/sum(tab_multi)
accuracy

#VI)Plot the ROC curve and find the auc(Area under Curve)


predict_log_multi_roc <- prediction(result_log_multi,m_glr_test$income)
predict_log_multi_roc
acc_multi= performance(predict_log_multi_roc, measure = 'tpr',x.measure = 'fpr')
acc_multi
plot(acc_multi)
#Find out the "area under the curve"
auc<- performance(predict_log_multi_roc, measure = 'auc')
print(auc)
auc = auc@y.values[[1]]
auc

####################################6.Decision Tree###########################################################################
decision_tress$age <- as.factor(decision_tress$age)
decision_tress$sex <- as.factor(decision_tress$sex)
decision_tress$fnlwgt <- as.factor(decision_tress$fnlwgt)

decision_tress$capital.gain <-as.factor(decision_tress$capital.gain)
decision_tress$capital.loss <-as.factor(decision_tress$capital.loss)
decision_tress$hours.per.week <-as.factor(decision_tress$hours.per.week)
decision_tress$native.country <-as.factor(decision_tress$native.country)
set.seed(1)
#i) Divide the dataset into training and test sets in 70:30 ratio
sample.split(decision_tress$income, SplitRatio = 0.70) -> divide_tree

subset(decision_tress, divide_tree==T) -> train_tree
subset(decision_tress, divide_tree==F) -> test_tree


str(decision_tress)
#install.packages('rpart')
#install.packages('rpart.plot')
library('rpart')# Create a model/ build a model
library('rattle')
library(rpart.plot)#/plot the decision tree
library(RColorBrewer)

#ii)Build a decision tress model where the dependent variable is x income and the rest of the variables as independent variables

rpart(income ~ age+workclass+education+education.num+marital.status+marital.status+occupation+relationship+race+capital.gain+capital.loss+hours.per.week+native.country, data=train_tree)-> mod_tree1

rpart.plot(mod_tree1, type=5, extra=0, tweak=1.2)
#iii)Plot the decision tree
plot(mod_tree1)
text(mod_tree1,pretty=1)

fancyRpartPlot(mod_tree1)

printcp(mod_tree1)

ptree <- prune(mod_tree1, cp=mod_tree1$cptable[which.min(mod_tree1$cptable[,'xerror']), 'CP'])



#d.	Predicting the values on the test
predict(mod_tree1,newdata=test_tree,type="class")->result_tree1
head(result_tree1)

#e.	Building the confusion matrix and caculate the accuracy
confusionMatrix(as.factor(result_tree1),as.factor(test_tree$income))
###Accuracy : 0.7151 
#or
table(test_tree$income, result_tree1) -> acc_tree
sum(diag(acc_tree))/sum(acc_tree)

####################################7.Random Forest###########################################################################
library('randomForest')

library(caTools)
random_forest$income <- as.factor(random_forest$income)
set.seed(15)
#i) Divide the dataset into training and test sets in 80:20 ratio
sample.split(random_forest$income, SplitRatio = 0.8) -> divide_rf

subset(random_forest, divide_rf==T) -> train_rf
subset(random_forest, divide_rf==F) -> test_rf

#ii)Build a random forest model where the dependent variable is income and the rest of the variables as independent variables and number of trees as 300
mod_forest1 <- randomForest(income ~ ., data=train_rf,ntree=300)
randomForest::importance(mod_forest1)
varImpPlot(mod_forest1)
#iii)Predict values on the test set
predict(mod_forest1, newdata=test_rf,type="class")-> result_forest1
#iv)Build a confusion matrix and calculate the accuracy
confusionMatrix(test_rf$income, result_forest1)
#or
table(test_rf$income, result_forest1)-> acc_for
sum(diag(acc_for))/sum(acc_for)

