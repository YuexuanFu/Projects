df<-Walmart
View(df)

library(lubridate)
library(RColorBrewer)
library(scales)
library(dplyr)
library(reprex)
library(tidyr) 
library(topicmodels) 
library(tm) 
library(textdata)
library(DT)
library(tmap)
library(scales)
library(lubridate)
dplyr::filter()
colors <- c("#CC1011", "#665555", "#05a399", "#f5e840", "#0683c9", "#e075b0","#ae8ebe","#669933", 
            "#FFCC66","#999999", "#E69F00", "#56B4E9","#9933FF","#33FFFF","#FF6666",
            "#E495A5","#ABB065","#39BEB1","#ACA4E2","#8DD3C7","#FFFFB3","#BEBADA","#FB8072")

df$is_holiday <- df$Date
df$is_holiday[df$is_holiday =='12-02-2010'] <- TRUE
df$is_holiday[df$is_holiday == '11-02-2011'] <- TRUE
df$is_holiday[df$is_holiday =='10-02-2012'] <- TRUE
df$is_holiday[df$is_holiday =='08-02-2013'] <- TRUE
df$is_holiday[df$is_holiday =='10-09-2010'] <- TRUE
df$is_holiday[df$is_holiday =='09-09-2011'] <- TRUE
df$is_holiday[df$is_holiday =='07-09-2012'] <- TRUE
df$is_holiday[df$is_holiday =='06-09-2013'] <- TRUE
df$is_holiday[df$is_holiday =='26-11-2010'] <- TRUE
df$is_holiday[df$is_holiday =='25-11-2011'] <- TRUE
df$is_holiday[df$is_holiday =='23-11-2012'] <- TRUE
df$is_holiday[df$is_holiday =='29-11-2013'] <- TRUE
df$is_holiday[df$is_holiday =='31-12-2010'] <- TRUE
df$is_holiday[df$is_holiday =='30-12-2011'] <- TRUE
df$is_holiday[df$is_holiday =='28-12-2012'] <- TRUE
df$is_holiday[df$is_holiday =='27-12-2013'] <- TRUE
df$is_holiday[df$is_holiday != 'TRUE'] <- FALSE
# split the Date column into month, date, year for analysis
df$year <- year(dmy(df$Date))
df$month <- month(dmy(df$Date)) 
df$day <- day(dmy(df$Date))
df$week <- week(dmy(df$Date))
View(df)

#1.Descriptive analysis

par(mfrow=c(3,2))

hist(df$Temperature, col = 'light green', main = "Temperature")
hist(df$Fuel_Price, col = 'light pink', main = "Fuel Price")
hist(df$CPI, col = 'grey', main = "CPI")
hist(df$Unemployment, col = 'red', main = "Unemployment")
hist(df$Weekly_Sales, col = 'blue', main = "Weekly_Sales")

#Temperature seems normally distributed but left-skewed. 
#Fuel_Price,Unemployment and CPI seem to follow a bimodal distribution

#Weekly Sales is right skewed indicating few large values. 
#This could be caused by the festive Weeks which are few but Sales 
#value for these weeks would be very high in comparison to other weeks


#2. Total weekly sales per store
totalweeklysales <- df %>% group_by(Store) %>% 
  dplyr::summarize(Salesperstore = sum(Weekly_Sales))

ggplot(totalweeklysales, aes(Store, Salesperstore)) + 
  geom_bar( stat = "identity",fill="steelblue") +ggtitle("Weekly Sales Per Store") +
  theme(axis.text.x = element_text( size = 10)) +scale_fill_grey()+
  scale_x_continuous(breaks = breaks_width(1)) + scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_text( angle = 90))

# we can see 20, 4, 24 stores have highest sales


#3. calc of CPI acc to years
CPIvalue <- df %>% group_by(month) %>% 
  dplyr::summarize(CPI = sum(CPI))
datatable(CPIvalue)

ggplot(CPIvalue, aes(month, CPI)) + 
  geom_line( stat = "identity") +ggtitle("CPIvalue") +
  scale_x_continuous(breaks = breaks_width(1))

#The CPI is what is used to measure these average changes in prices 
#over time that consumers pay for goods and services.
#The CPI is one of the most frequently used statistics for 
#identifying periods of inflation or deflation.
#The CPI statistics cover a variety of individuals with different incomes, including unemployment.
#we can see sharp decline in CPI index in the month of 11. 
#this could be caused by the festive Weeks and offers provided by the stores.


#4. monthly sales data per yr
sales <- df %>% group_by(year) %>% 
  dplyr::summarize(Sales_week = sum(Weekly_Sales))

datatable(sales)
ggplot(df, aes(x = month,y = Weekly_Sales )) + 
  geom_col(fill='#FF6666') + facet_wrap(~year) + ggtitle("Montly Sales Distribution Per Year")+
scale_x_continuous(breaks = breaks_width(1)) + scale_y_continuous(labels = comma)

# Data is missing for January in 2010, November and December in 2012.
#There are weeks when Sales peaks in the festive months of November and December.
#Also seems like there is a dip in September - October leading towards the holiday weeks in 2012

#5.which holiday week generated more weeklysales
holidays <- df %>% group_by(month) %>% filter(is_holiday==TRUE) %>%
  dplyr::summarize(WeeklySales =sum(Weekly_Sales))
datatable(holidays)

ggplot(holidays, aes(x=month, y=WeeklySales,label=WeeklySales)) + geom_point() + 
  scale_x_continuous(breaks = breaks_width(1))+ theme_bw()+
  geom_text(aes(label=WeeklySales),hjust=0, vjust=0) + ggtitle("Weekly Sales-Holidays")



ggplot(holidays, aes(x=month, y=WeeklySales)) + geom_bar(stat = "identity",fill='blue') + 
   scale_x_continuous(breaks = breaks_width(1))+ theme_bw()+
  geom_text(aes(label=WeeklySales),hjust=0, vjust=0) 

# superbowl has the highest weekly sales, followed by labour day,thanksgiving and christmas



#6. Which store has maximum standard deviation i.e., the sales vary a lot. 
#Also, find out the coefficient of variance

store_sales_max_std <- df %>% group_by(Store) %>% summarize(SD = sd(Weekly_Sales))
datatable(store_sales_max_std)
ggplot(store_sales_max_std , aes(x = Store,y = SD)) + geom_bar(stat = "identity") + 
  scale_x_continuous(breaks = breaks_width(1)) + scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_text( angle = 90))

COV <- df %>% group_by(Store) %>% summarize(Cov = sd(Weekly_Sales)/mean(Weekly_Sales))
datatable(COV)
ggplot(COV, aes(x = Store,y = Cov)) + geom_bar(stat = "identity") + 
  scale_x_continuous(breaks = breaks_width(1)) + scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_text( angle = 90))

Store_14 <- df[df$Store == 14, ]
ggplot(Store_14, aes(x=Weekly_Sales)) + geom_density(color="darkblue", fill="lightblue",alpha=0.2)+
  geom_vline(aes(xintercept= mean(Weekly_Sales)),color="steelblue", linetype="dashed", size=1)+
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))+ scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6))+ ggtitle('Store 14 Sales distribution')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Weekly Sales") + ylab("Density")

#store 14 has the highest standard deviation and relatively low COV.
#Store 14 sales are right skewed i.e, It had very high sales in few weeks which 
#resulted in increasing of Standard deviation


# 7.times series analysis

x<-as.factor(df$Date)

#defining what is the original format of  date
abis<-strptime(x,format="%d-%m-%Y") 

#defining what is the desired format of your date
df$Mon_Year<-as.Date(abis,format="%Y-%m-%d")

df$Mon_Year = as.yearmon(df$Mon_Year)
#Aggregating data by 'Month -Year' and Finding sum of 'Weekly_Sales' and convrting it into dataframe
Month_Year_Sales<-summarise(group_by(df,Mon_Year),sum(Weekly_Sales))
colnames(Month_Year_Sales)[2] <- "Sales_by_Month"
Month_Year_Sales<- as.data.frame(Month_Year_Sales)

#Converting year-mon to factor for plotting so that order wont change
Month_Year_Sales$Mon_Year<- as.character(Month_Year_Sales$Mon_Year)
Month_Year_Sales$Mon_Year<- factor(Month_Year_Sales$Mon_Year, levels=Month_Year_Sales$Mon_Year)

ggplot(Month_Year_Sales, aes(x=Mon_Year, y=Sales_by_Month, group=1)) +
  geom_line(color="red")+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  ggtitle('Time series analysis - 2010 to 2012')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Month") + ylab("Total Sales in a Month")

#The sales are highest in December and Lowest in January

#8.Which stores has good quarterly growth rate in Q3’2011

data2<-data1

#Creating a month- year column in data2 
df$month_Year = substr(df$Date, 4, 10)

#Subsetting Q3-2012 data (i.e, 07-2012,08-2012,09-2012), Q2-2012 data (i.e, 04-2012,05- 2012,06-2012)

Q2_2011 <- filter(df,month_Year == "04-2011" | month_Year== "05-2011" | month_Year== "06-2011")
Q3_2011 <- filter(df,month_Year == "07-2011" | month_Year== "08-2011" | month_Year== "09-2011")

#Aggregating sales by store for Q3-2012 
Q3_2011_Sales<-summarise(group_by(Q3_2011,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q3_2011_Sales)[2] <- "Q3_2011_Sales_by_Store"

#Aggregating sales by store each Q2-2012 
Q2_2011_Sales<-summarise(group_by(Q2_2011,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q2_2011_Sales)[2] <- "Q2_2011_Sales_by_Store"
#merging two quarters data by store
Q3_2011_Growthrate <- merge ( Q2_2011_Sales , Q3_2011_Sales , by = 'Store')

#Creating Growth rate column for Sales by Store in the above dataframe 
Q3_2011_Growthrate <- mutate(Q3_2011_Growthrate, 
Growth_Rate = ((Q3_2011_Sales_by_Store - Q2_2011_Sales_by_Store)*100) / Q2_2011_Sales_by_Store)

#Creating only positive growth rates
positive_growthrate <- filter(Q3_2011_Growthrate, Growth_Rate > 0 ) 
positive_growthrate<-arrange(positive_growthrate, desc(Growth_Rate)) 
View(positive_growthrate)
positive_growthrate$Store

ggplot(Q3_2011_Growthrate, aes(x=Store, y=Growth_Rate)) +geom_bar(stat ="identity",fill="#56B4E9")+
  ggtitle('Growth rates of Q3- 2011')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Stores") + ylab("Growth rate(%)") +
  scale_x_continuous("Stores", labels = as.character(Q3_2011_Growthrate$Store), breaks =
                       Q3_2011_Growthrate$Store)+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=0.5))

#Store 7 has highest growth rate 38.08%, Store 16 second highest growth rate 30.34%, 
#Store 26 third highest growth rate 15.59%
#It is observed only Store 18 has negative growth rate.

# Regression Analysis

library(doBy)
library(dplyr)
library(foreign)
library(ggplot2)
library(knitr)
library(lmtest)
library(readstata13)
library(sandwich)
library(stargazer)
library(AER)
library(gdata)
library(wooldridge)
library(openintro)
library(tidyr)
library(tidyverse)

df <- read.csv("Desktop/Marketing/Walmart.csv")
str(df)
sum(is.na(df))  # 0 NA
typeof(df)
View(df)


#Linear regression
reg1 = lm(Weekly_Sales ~ CPI, data = df)
reg2 = lm(Weekly_Sales ~ CPI + Store, data = df)
reg3 = lm(Weekly_Sales ~ CPI + Store + Unemployment , data = df)
reg4 = lm(Weekly_Sales ~ CPI + Store + Unemployment + Temperature, data = df)
reg5 = lm(Weekly_Sales ~ CPI + Store + Unemployment + Temperature + Holiday_Flag, data = df)
reg6 = lm(Weekly_Sales ~ CPI + Store + Unemployment + Temperature + Holiday_Flag + Fuel_Price, data = df)
reg7 = lm(Weekly_Sales ~ CPI + Store + Unemployment + Holiday_Flag, data = df)

stargazer(reg1, reg2, reg3, reg4, reg5,reg6,reg7, cse=list(cse(reg1), cse(reg2), cse(reg3), cse(reg4), cse(reg5), cse(reg6), cse(reg7)), title="Weekly Sales", type="text", df=FALSE, digits=3)

set.seed(2022)
sample<- sample.int(n=nrow(df), replace = FALSE, size = .75*(nrow(df)))
trainset<-df[sample,]
testset<-df[-sample,]

# Train model on training data
model<-lm(Weekly_Sales ~ CPI + Store + Unemployment + Temperature + Holiday_Flag, df[sample,])
summary(model)
names(model)

# predict values
coef(model)
# method2
p<-predict(model, newdata = df[-sample,])

# calculate rmse for training versus testset
actual <- trainset$Weekly_Sales
predicted_train <- model$fitted.values
trainsetdf<-data.frame(actual, predicted_train)
rmse_train <- sqrt(mean((trainsetdf$actual-trainsetdf$predicted_train)^2))
rmse_train

# rmse for testset data
actual_test<- testset$Weekly_Sales
predicted_test<- p
testsetdf<-data.frame(actual_test, predicted_test)
rmse_test <- sqrt(mean((testsetdf$actual_test-testsetdf$predicted_test)^2))
rmse_test

# logistic regression
mean = median(df$Weekly_Sales)
mean
df$high_sales=ifelse(df$Weekly_Sales >= mean, 1, 0)
log1 = glm(high_sales ~ CPI, data = df)
log2 = glm(high_sales ~ CPI + Store, data = df)
log3 = glm(high_sales ~ CPI + Store + Unemployment , data = df)
log4 = glm(high_sales ~ CPI + Store + Unemployment + Temperature, data = df)
log5 = glm(high_sales ~ CPI + Store + Unemployment + Temperature + Holiday_Flag, data = df)
log6 = glm(high_sales ~ CPI + Store + Unemployment + Temperature + Holiday_Flag + Fuel_Price, data = df)
stargazer(log1, log2, log3, log4, log5,log6, se=list(cse(log1), cse(log2), cse(log3), cse(log4), cse(log5), cse(log6)), title="Weekly Sales", type="text", df=FALSE, digits=3)
mylogit<-glm(high_sales ~ CPI + Store + Unemployment + Temperature + Holiday_Flag, family = 'binomial', data = df)
summary(mylogit)

exp(coef(mylogit))


#Calculate the mean value of columns group by store_id
df_sales<-aggregate(df$Weekly_Sales,by = list(store=df$Store),mean)
df_CPI<-aggregate(df$CPI,by = list(store=df$Store),mean)
df_Unemployment<-aggregate(df$Unemployment,by = list(store=df$Store),mean)
df_Fuel_Price<-aggregate(df$Fuel_Price,by = list(store=df$Store),mean)


df_sales_Cpi<-bind_cols(df_sales, df_CPI)
df_unem<-bind_cols(df_sales_Cpi, df_Unemployment)
df_stores<-bind_cols(df_unem, df_Fuel_Price)

df_stores<-df_stores[ , -which(colnames(df_stores) %in% c("store...3","store...5","store...7"))]

names(df_stores)[1] <-"Store_ID"
names(df_stores)[2] <-"Average_Sales"
names(df_stores)[3] <-"Average_CPI"
names(df_stores)[4] <-"Average_Unemployment"
names(df_stores)[5] <-"Average_Fuel_Price"

#dummy variables
df_stores$Good_Stores = ifelse(df_stores$Average_Sales >= mean, 1, 0)

#logistics regression
Store_log1 = glm(Good_Stores ~ Store_ID, data = df_stores)
Store_log2 = glm(Good_Stores ~ Average_CPI, data = df_stores)
Store_log3 = glm(Good_Stores ~ Average_CPI + Average_Unemployment, data = df_stores)
Store_log4 = glm(Good_Stores ~ Average_CPI + Average_Unemployment+ Average_Fuel_Price, data = df_stores)

stargazer(Store_log1, Store_log2, Store_log3, Store_log4, se=list(cse(Store_log1), cse(Store_log2), cse(Store_log3), cse(Store_log4)), title="Good Stores", type="text", df=FALSE, digits=3)

####discover whether temperature affects weekly sales
#filter out store_id=1 and store_id from 1 to 10
df_store1<-df[(df$Store < 2),]
df_store10<-df[(df$Store < 11),]
#to explore relationship bwt temperature and weekly sales
ggplot(df,aes(x=Temperature,y=Weekly_Sales))+geom_point()
ggplot(df_store1,aes(x=Temperature,y=Weekly_Sales))+geom_point()
ggplot(df_store10,aes(x=Temperature,y=Weekly_Sales))+geom_point()
ggplot(df_store1,aes(x=Temperature,y=Weekly_Sales,color=Holiday_Flag))+geom_point()


# topic modeling and sentiment analysis
library(dplyr)
install.packages(c('mnormt','psych','Snowballc','hunspell','broom','tokenizers','janeaustenr'))
install.packages('textdata')
install.packages('tidytext')
library(tidyr)
library(topicmodels)
library(tm)
library(tidytext)
# Installing package to read txt file
install.packages("readr")
# Loading
library("readr")
mydata<- read_tsv("Desktop/Marketing/Walmart.txt")
mydf<- mydata

df_words<-textdf%>% count(line,word,sort=TRUE)
View(df_words)

#to remove stopwords doesn't work rn, might need to install another package
data('stop_words')


textdf2<- textdf%>%anti_join(stop_words)
View(textdf2)
#count words
textdf3<-textdf%>%count(word,sort=TRUE)
View(textdf3)
#visualize
library(ggplot2)
textdf %>% count(word,sort=TRUE) %>% filter (n>1) %>% ggplot(aes(n,word)) + geom_col()
textdf %>% count(word,sort=TRUE) %>% filter (n>10) %>% ggplot(aes(n,word)) + geom_col()
textdf2 %>% count(word,sort=TRUE) %>% filter (n>1) %>% ggplot(aes(n,word)) + geom_col()
textdf2 %>% count(word,sort=TRUE) %>% filter (n>2) %>% ggplot(aes(n,word)) + geom_col()

#sentiment analysis
# get package 
get_sentiments(lexicon='bing')

#using bing
sent_bing<-textdf2%>%inner_join(get_sentiments('bing'))%>%count(word,sentiment)
View(sent_bing)
sent_bing<-textdf2%>%inner_join(get_sentiments('bing'))%>%count(word,sentiment)%>% pivot_wider(names_from=sentiment,values_from=n,values_fill=0)
View(sent_bing)
sent_bing<-textdf2%>%inner_join(get_sentiments('bing'))%>%count(word,sentiment)%>% pivot_wider(names_from=sentiment,values_from=n,values_fill=0)%>% mutate(sentiment=positive-negative)
View(sent_bing)

ggplot(sent_bing,aes(word,sentiment,fill=word)) +geom_col()







