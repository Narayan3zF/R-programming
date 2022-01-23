#1.	Extract these individual columns with the   $   symbol:

churn_telcom = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\customer_churn_telecom.csv')
read.csv('C:\\Users\\yerran\\Documents\\rcodes\\customer_churn_telecom.csv') -> Cus_nar
View(churn_telcom)
str(churn_telcom)
#a.	Extract   InternetService   column and store it in   customer_internet_service  
customer_internet_service <- churn_telcom$InternetService
customer_internet_service

#b.	Extract   PaperlessBilling   column and store it in   customer_Paperless_Billing  

customer_Paperless_Billing <- churn_telcom$PaperlessBilling 
customer_Paperless_Billing

#c.	Extract   StreamingTV   column and store it in   customer_Streaming_TV  

customer_Streaming_TV <- churn_telcom$StreamingTV 
customer_Streaming_TV


#2.	Extract the 3rd, 6th and 9th columns from the   customer_churn   data.frame & store it in   customer_random_columns  

churn = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\customer_Churn.csv')
View(churn)

customer_random_columns <- churn[c(3,6,9)]
customer_random_columns
#3.	Extract all the columns from column number-10 to column-number 20 and store the result in   customer_10_20  

customer_10_20 <-churn[,c(10:20)]
customer_10_20

#4.	Extract only these row numbers: 65, 765, 3726 & 7000 and store the result in   customer_random_rows   
customer_random_rows <- churn[c(65,765,3726,7000),]
customer_random_rows

#5.	Extract all the rows starting from row number-655 to row number-6550 & store the result in   customer_continuous_rows  
customer_continuous_rows <- churn[c(655:6550),]
customer_continuous_rows

#6.	Extract row numbers- 10, 100 & 1000 & column numbers- 5, 10, 15 & store the result in   customer_random_data  
customer_random_data <- churn[c(10,100,1000),c(5,10,15)]
customer_random_data
#Questions on Flow Control Statements:
#1.	Check if the value in the 6th cell of   PaymentMethod   column is   Electronic check  . If yes, print “Yes, the payment method is Electronic check”
sixth_cell <- churn_telcom$PaymentMethod[6]
sixth_cell

ifelse(sixth_cell == 'Electronic check','YES','NO')

#2.	Check the value present in 12th cell of   Contract   column. 
twelfth_cell <- churn_telcom$Contract[12]
twelfth_cell

if (twelfth_cell == 'month-to-month') {
  print('The contract is on a month to month basis')
} else if (twelfth_cell == 'One year') {
  print('The contract is on a yearly basis')
} else if (twelfth_cell == 'Two year') {
  print('The contract is on a two-year basis')
} else {
  print('Not a valid contract')
}
#3.	Use switch to check the gender in 6th cell of   gender   column.
new_churn_telcom <- churn_telcom[6,]
str(new_churn_telcom)
sixth_cell_gender <- new_churn_telcom$gender
sixth_cell_gender
# by name with complex named expression
discount = switch(sixth_cell_gender,"Male" = {new_churn_telcom$MonthlyCharges * 0.2} ,"Female" = {new_churn_telcom$MonthlyCharges * 0.5})
discount
#4.	Use for loop to get the count of customers whose   InternetService   is   DSL  
count_DSL=0
count_fiber_optic=0
count_no = 0

for(i in 1:nrow(churn_telcom)) {
  if(churn_telcom$InternetService[i]  == 'DSL') {
    count_DSL = count_DSL+1
  } else if(churn_telcom$InternetService[i] == 'Fiber optic') {
    count_fiber_optic= count_fiber_optic+1
  }else if(churn_telcom$InternetService[i] == 'No') {
    count_no = count_no + 1
  }
  
}
count_DSL
count_fiber_optic
count_no
#5.	Use while to find the number of customers whose tenure is exactly   2   months
count_tenure = 0
i= 1
while (i < nrow(churn_telcom))
{
  
  if(churn_telcom$tenure[i] == 2) {
    count_tenure = count_tenure + 1
  }
  i = i+1
}
count_tenure
#1.	Do these operations with the head() function:
#a.	Get the first 4 records from   PhoneService   column
head(churn_telcom$PhoneService, 4)

#b.	Get the first 8 records from   Contract   column
head(churn_telcom$Contract, 8)

#2.	Do these operations with the tail() function:
#a.	Get the last record of   TotalCharges   column
tail(churn_telcom$TotalCharges,1)

#b.	Get the last 5 records of   tenure   column
tail(churn_telcom$TotalCharges, 5)

#3.	Find the average, minimum, maximum & range from the   tenure   column
#average(churn_telcom$tenure)
min(churn_telcom$tenure)
max(churn_telcom$tenure)
range(churn_telcom$tenure, trim = 0, na.rm = FALSE)
mean(churn_telcom$tenure, trim = 0, na.rm = FALSE)

#4.	Get 10 random values from the   TotalCharges   column using sample()
sample(churn_telcom$TotalCharges,10)

#5.	Find the count of different levels in   PaymentMethod   &   Contract   columns using table()
table(churn_telcom$PaymentMethod, churn_telcom$Contract, dnn = c("PaymentMethod", "Contract"))

