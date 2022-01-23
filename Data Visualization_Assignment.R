library('plotrix')
library('ggplot2')

churn = read.csv('C:\\Users\\yerran\\Documents\\rcodes\\intellipaatdatascienceassignmentandregularsession\\Customer_Churn.csv')
View(churn)
#1.	Build a bar-plot for the ‘PhoneService’ column
ggplot(churn,aes(PhoneService)) + geom_bar()+ ggtitle('This is the bar-plot for the PhoneService')+xlab('PhoneService')+ ylab('Count')
#1.a a.	Assign the fill color to be ‘pink’
ggplot(churn,aes(PhoneService)) + geom_bar(fill="pink")+ ggtitle('This is the bar-plot for the PhoneService')+xlab('PhoneService')+ ylab('Count')
#1.a b.	Assign the fill color to be ‘pink’
ggplot(churn,aes(PhoneService)) + geom_bar(fill="pink",col="peru")+ ggtitle('This is the bar-plot for the PhoneService')+xlab('PhoneService')+ ylab('Count')


#2.	Build a bar-plot for the ’InternetService’ column
ggplot(churn,aes(InternetService)) + geom_bar()+ ggtitle('This is the bar-plot for the InternetService')
#a.	Assign ‘InternetService’ to the fill aesthetic
ggplot(churn, aes(InternetService,fill=InternetService))+geom_bar()+ ggtitle('This is the bar-plot for the InternetService')+xlab('InternetService')+ ylab('Count')
#b.	Assign ‘Contract’ to the fill aesthetic
ggplot(churn, aes(InternetService,fill=Contract))+geom_bar()+ ggtitle('This is the bar-plot for the InternetService')+xlab('InternetService')+ ylab('Count')
#c.	Change the position of bars to ‘identity’
ggplot(churn, aes(InternetService,fill=Contract))+geom_bar()+ ggtitle('This is the bar-plot for the InternetService')+xlab('InternetService')+ ylab('Count')+coord_flip()
#or
ggplot(churn, aes(Contract,fill=InternetService))+geom_bar()+ ggtitle('This is the bar-plot for the Contract')+xlab('InternetService')+ ylab('Count')


#3.	Build a bar-plot for ‘TechSupport’ column
ggplot(churn, aes(TechSupport))+geom_bar()+ ggtitle('This is the bar-plot for the TechSupport')+xlab('TechSupport')+ ylab('Count')
#a.	Assign ‘Churn’ to fill aesthetic
ggplot(data = churn,aes(x=InternetService,fill=Churn)) + geom_bar()

#1.	Build a histogram for the ‘tenure’ column

ggplot(churn, aes(x=tenure))+geom_histogram()
#warning `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

#a.	Assign the fill color to be ‘mediumspringgreen’
#b.	Assign the boundary color to be ‘azure’
ggplot(churn, aes(x=tenure))+geom_histogram(fill='mediumspringgreen', col='azure',bins = 10)+
  labs(title='Distribution of Tenure')
#c.	Change the number of bins to be 100
ggplot(churn, aes(x=tenure))+geom_histogram(fill='mediumspringgreen', col='azure',bins = 100)+
  labs(title='Distribution of Tenure')
#2.	Build histogram for the ‘MonthlyCharges’ column
#a.	Assign ‘PaymentMethod’ to fill aesthetic
ggplot(data = churn,aes(x=MonthlyCharges,fill=PaymentMethod)) + geom_histogram()
#b.	Assign ‘OnlineBackup’ to fill aesthetic
ggplot(churn,aes(x=MonthlyCharges,fill=OnlineBackup)) + geom_histogram()
#3.	Build histogram for the ‘TotalCharges’ column
ggplot(churn,aes(x=TotalCharges)) + geom_histogram()
#a.	Assign ‘gender’ to fill aesthetic
ggplot(churn,aes(x=TotalCharges,fill=gender)) + geom_histogram()
#b.	Assign ‘InternetService’ to the fill aesthetic
ggplot(churn,aes(x=TotalCharges,fill=InternetService)) + geom_histogram()

#1.	Build a scatter-plot between ‘TotalCharges’ & ‘tenure’. Map ‘TotalCharges’ to the y-axis & ‘tenure’ to the ‘x-axis’
ggplot(churn,aes(y=TotalCharges,x=tenure)) + geom_point()

#a.	Assign it the color ‘wheat3’
ggplot(churn,aes(y=TotalCharges,x=tenure)) + geom_point(color='wheat3')
#b.	Mapping ‘PaymentMethod’ to col aesthetic
ggplot(churn,aes(y=TotalCharges,x=tenure,col=PaymentMethod)) + geom_point()
#c.	Use ‘col’ as an aesthetic and Map ‘gender’ to col
ggplot(churn,aes(y=TotalCharges,x=tenure,col=gender)) + geom_point()
#d.	Map ‘Dependents’ to both ‘col’ & ‘shape’ aesthetics
ggplot(churn,aes(y=TotalCharges,x=tenure,col=Dependents,shape=Dependents)) + geom_point()
#2.	Build a scatter-plot between ‘tenure’ & ‘MonthlyCharges’. Map ‘tenure’ to the y-axis & ‘MonthlyCharges’ to the ‘x-axis’
ggplot(churn,aes(y=tenure,x=MonthlyCharges)) + geom_point()
#a.	Assign it the color ‘yellowgreen’
ggplot(churn,aes(y=tenure,x=MonthlyCharges)) + geom_point(color='yellowgreen')
#b.	Assign ‘InternetService’ to col aesthetic
ggplot(churn,aes(y=tenure,x=MonthlyCharges,col=InternetService)) + geom_point()
#c.	Use ‘col’ as an aesthetic and Map ‘Contract’ to col
ggplot(churn,aes(y=tenure,x=MonthlyCharges,col=Contract)) + geom_point()

#1.	Build a box-plot between ‘tenure’ & ‘Partner’. Map ‘tenure’ to the y-axis & ‘Partner’ to the ‘x-axis’
ggplot(churn,aes(y=tenure,x=Partner))+geom_boxplot()
#a.	Assign it a fill color of ‘violet’
ggplot(churn,aes(y=tenure,x=Partner))+geom_boxplot(fill="violet")
#b.	Assign it a boundary color of ‘snow3’
ggplot(churn,aes(y=tenure,x=Partner))+geom_boxplot(fill="violet", col="snow3")
#2.	Build a box-plot between ‘tenure’ & ‘MultipleLines’. Map ‘tenure’ to the y-axis & ‘MultipleLines’ to the ‘x-axis’
ggplot(churn,aes(y=tenure,x=MultipleLines))+geom_boxplot()
#a.	Assigning ‘Partner’ to fill aesthetic
ggplot(churn,aes(y=tenure,x=MultipleLines,fill=Partner))+geom_boxplot()
#b.	Assigning ‘PhoneService’ to fill aesthetic
ggplot(churn,aes(y=tenure,x=MultipleLines,fill=PhoneService))+geom_boxplot()
#3.	Build a box-plot between ‘tenure’ & ‘Contract’
#a.	Assign ‘InternetService’ to the fill aesthetic
ggplot(churn,aes(y=tenure,x=Contract,fill=InternetService))+geom_boxplot()
#b.	Assign ‘PaymentMethod’ to the fill aesthetic
ggplot(churn,aes(y=tenure,x=Contract,fill=PaymentMethod))+geom_boxplot()
#3.	Build a box-plot between ‘tenure’ & ‘Contract’
#a.	Facet the plot w.r.t ‘InternetService’
ggplot(churn,aes(y=tenure,x=MultipleLines,fill=InternetService))+geom_boxplot() + facet_grid(~InternetService)
#2.	Build a scatter-plot between ‘TotalCharges’ & ‘tenure’. Map ‘TotalCharges’ on the y-axis & ‘tenure’ on the x-axis. Map ‘Contract’ onto col aesthetic
#a.	Facet the plot w.r.t ‘Contract’ column

ggplot(churn,aes(y=TotalCharges,x=tenure,col=Contract))+geom_point()+facet_grid(~Contract)

#3.	Build a histogram for the ‘MonthlyCharges’ column. Map ‘PaymentMethod’ onto fill aesthetic.
#a.	Facet the plot w.r.t ‘PaymentMethod’ column
ggplot(churn,aes(x=MonthlyCharges,fill=PaymentMethod))+geom_histogram()+facet_grid(~PaymentMethod)

#1.	Build a bar-plot for the ‘gender’ column. Give it a fill color of ‘blue4’
#a.	Give the panel a background color of ‘chartreuse4’
#b.	Giving the plot a background color of ‘cornsilk4’
ggplot(churn,aes(x=gender))+geom_bar(fill="blue4")+theme(panel.background = element_rect(fill = "chartreuse4"))+theme(plot.background = element_rect(fill="cornsilk4"))












