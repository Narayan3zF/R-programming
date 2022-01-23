library('dplyr')

churn = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\intellipaatdatascienceassignmentandregularsession\\Customer_Churn.csv')
View(churn)
#1.	Extract these individual columns
#a.	Extract the 5th column & store it in ‘customer_5’
churn %>% select(5) -> fiveth_column
View(fiveth_column)
#b.	Extract the 15th column & store it in ‘customer_15’
churn %>% select(15) -> fifteenth_column
View(fifteenth_column)

#2.	Extract the column numbers 3,6,9,12,15 & 18 and store the result in ‘customer_3_multiple’
churn %>% select(3,6,9,12,15,18) -> customer_3_multiple
View(customer_3_multiple)

#3.	Extract all the columns from column number-10 to column number-20 and store the result in ‘c_10_20’
churn %>% select(10:20) -> c_10_20
View(c_10_20)

#4.	Extract all the columns which start with letter ‘P’ & store it in ‘customer_P’
churn %>% select(starts_with("P")) -> customer_P
View(customer_P)

#5.	Extract all the columns which end with letter ‘s’ & store it in ‘customer_s’
churn %>% select(ends_with("s")) -> customer_s
View(customer_s)

#Questions on filter() function
#1.	Extract all the customers whose Internet Service is “DSL” & store the result in ‘customer_dsl’
customer_dsl <- filter(churn, InternetService == "DSL")
View(customer_dsl)

#2.	Extract all the customers whose Contract type is ‘Month-to-month’ & store the result in ‘customer_month’
customer_month <- filter(churn, Contract == "Month-to-month")
View(customer_month)
#3.	Extract all the male senior citizens whose Payment Method is Electronic check & store the result in ‘senior_male_electronic’
senior_male_electronic <-filter(churn, gender=="Male" & SeniorCitizen==1 & PaymentMethod == "Electronic check")
View(senior_male_electronic)

#4.	Extract all those customers whose tenure is greater than 70 months or their Total charges is more than 8000$ & store the result in ‘customer_total_tenure’
churn %>% filter(tenure>70 | TotalCharges>8000) -> customer_total_tenure
View(customer_total_tenure)

#5.	Extract all the customers whose Contract is of two years, payment method is Mailed check & the value of Churn is ‘Yes’ & store the result in ‘two_mail_yes’
churn %>% filter(Contract=="Two year" & PaymentMethod=="Mailed check" & Churn=="Yes") -> two_mail_yes
View(two_mail_yes)

#Questions on sample_n(), sample_frac() & count():
#1.	Extract 333 random records from the customer_churn dataframe & store the result in ‘customer_333’

churn %>% sample_n(333) -> customer_333
View(customer_333)

#2.	Extract 1000 random records from the customer_churn dataframe & store the result in ‘customer_1000’
churn %>% sample_n(1000) -> customer_1000
View(customer_1000)
#3.	Randomly extract 23% of the records from the customer_churn dataframe & store the result in ‘customer_23_percent’
churn %>% sample_frac(.23) -> customer_23_percent
View(customer_23_percent)
#4.	Get the count of different levels from the ‘PaymentMethod’ column
churn %>% count(PaymentMethod) -> count_paymenthod
count_paymenthod
#5.	Get the count of different levels from the ‘Churn’ column
churn %>% count(Churn) -> count_chrun
count_chrun

#Questions on summarise() & group_by():
#1.	Get the median, variance & standard deviation for the ‘tenure’ column
#median
churn %>% summarise(tenure_median=median(tenure))
#variance
churn %>% summarise(tenure_variance=var(tenure))
#standard deviation
churn %>% summarise(tenure_standard_deviation=sd(tenure))

#2.	Get the median, variance & standard deviation for the ‘MonthlyCharges’ column
#Median
churn %>% summarise(median_MonthlyCharges=median(MonthlyCharges))
#Variance
churn %>% summarise(variance_MonthlyCharges=var(MonthlyCharges))
#Standard Deviation
churn %>% summarise(standard_deviation_MonthlyCharges=sd(MonthlyCharges))

#3.	Get the standard deviation of ‘tenure’ & group it w.r.t ‘PaymentMethod’ column

churn %>% group_by(PaymentMethod) %>% summarise(sd(tenure))

#4.	Get the median of ‘MonthlyCharges’ & group it w.r.t ‘Contract’ column

churn %>% group_by(Contract) %>% summarise(median(MonthlyCharges))

#5.	Get the variance of ‘TotalCharges’ & group it w.r.t ‘InternetService’ column
churn %>% group_by(InternetService) %>% summarise(var(TotalCharges))







