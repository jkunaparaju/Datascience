
# Steps 
# Took Indicators
# Subset data with indicators , Calender date and price
# Calucalte return price
# Remove Outliers
# Remove multi collinearity
# Remove NA'S
# Normalize indicator data
# Split data into 75% train and 25 test 
# Create Linear model for train data
# Find Coefficients with higher P value
# Build model for train data.
# Predict return price for test data 
# Forecast results. 
# How do we forecast ? what package ,  should date be unbroken 
#REVENUEUSD 		Revenues (USD)
#GP 	       		Gross Profit
#OPEX 	        Operating Expenses
#EBITUSD 		    Earning Before Interest & Taxes (USD)
#NETINC 	      Net Income
#EPSUSD 	      Earnings per Basic Share (USD)
#NCFO 	        Net Cash Flow from Operations
#NCFI 				  Net Cash Flow from Investing
#NCFF 				  Net Cash Flow from Financing
#NCF 			      Net Cash Flow / Change in Cash & Cash Equivalents
#ASSETS 			  Total Assets
#CASHNEQUSD 		Cash and Equivalents (USD)
#LIABILITIES 		Total Liabilities
#INVESTMENTS 		Investments
#EQUITYUSD 		 	hareholders Equity (USD
#DE 				    Debt to Equity Ratio 		
#EBITDA 		  	Earnings Before Interest, Taxes & Depreciation Amortization 									
#FCF 				    Free Cash Flow 		
#PE1 				    Price to Earnings Ratio 	
#PB 				    Price to Book Value 			nam



#Defining the Variables(Indicators Selected) from the list

indicator_list<-c(3,7,15,18, 25,28,32,35, 39,78,42,49,57,58, 59, 61,68,70, 86,90,93,94)



#Reading the file into a data frame
stock_data <- read.csv('/Users/jyothi/Downloads/data_file_ARQ.csv', header = T, sep =',')

#initializing the returns vector and loading values into it
return_price<-vector();
### Computing  Return price
#log of the returns for the price
for(i in 2:length(stock_data[,1])){
  if (identical(stock_data[i,1],stock_data[i-1,1])){
    return_price[i] = (stock_data[i,72]/stock_data[i-1,72]);
  }else{
    return_price[i] = 0;
  }
}
return_price[1]=0;

#combining return price to Stock Data
stock_data<-cbind(stock_data,return_price)



#Selecting only the indiactors chosen

stock_factors <- na.omit(stock_data[indicator_list])


#------------------------------------Outlier Removal-----------------
#plot(stock_factors$assets)
stock_factors<-subset(stock_factors, stock_factors$assets < 40000000000)
#plot(stock_factors$assets)
stock_factors<-subset(stock_factors, stock_factors$cashnequsd < 6000000000)
#plot(stock_factors$cashnequsd)
stock_factors<-subset(stock_factors, stock_factors$de > -100)
stock_factors<-subset(stock_factors, stock_factors$de < 100)
#plot(stock_factors$de)
#plot(stock_factors$ebitda)
stock_factors<-subset(stock_factors, stock_factors$ebitda > -500000000)
stock_factors<-subset(stock_factors, stock_factors$ebitda < 2000000000)
#plot(stock_factors$ebitda)
#plot(stock_factors$ebitusd)
stock_factors<-subset(stock_factors, stock_factors$ebitusd  > -400000000)
stock_factors<-subset(stock_factors, stock_factors$ebitusd < 1500000000)
#plot(stock_factors$ebitusd)
#View(stock_factors)
stock_factors<-subset(stock_factors, stock_factors$epsusd > -10)
#plot(stock_factors$epsusd)
stock_factors<-subset(stock_factors, stock_factors$epsusd < 10)
#plot(stock_factors$epsusd)
stock_factors<-subset(stock_factors, stock_factors$equityusd >  -1000000000)
#plot(stock_factors$equityusd)
stock_factors<-subset(stock_factors, stock_factors$equityusd <  20000000000)
#plot(stock_factors$equityusd)
stock_factors_new <- stock_factors
#stock_factors <- stock_factors_new
#plot(stock_factors$fcf)
stock_factors<-subset(stock_factors, stock_factors$fcf <   1500000000)
stock_factors<-subset(stock_factors, stock_factors$fcf  >   -1000000000)
#plot(stock_factors$fcf)
#plot(stock_factors$revenueusd)
stock_factors<-subset(stock_factors, stock_factors$revenueusd  <   8000000000)
#plot(stock_factors$revenueusd)
stock_factors<-subset(stock_factors, stock_factors$revenueusd  >   -100000)
#plot(stock_factors$revenueusd)
#plot(stock_factors$gp)
stock_factors<-subset(stock_factors, stock_factors$gp  <    3000000000)
stock_factors<-subset(stock_factors, stock_factors$gp  >    -4000000)
#plot(stock_factors$gp)
stock_factors<-subset(stock_factors, stock_factors$liabilities  <    25000000000)
stock_factors<-subset(stock_factors, stock_factors$liabilities  >    -80000)
#plot(stock_factors$liabilities)
stock_factors_new <- stock_factors
#plot(stock_factors$ncff)
stock_factors<-subset(stock_factors, stock_factors$ncff   < 2000000000)
#plot(stock_factors$ncff)
stock_factors<-subset(stock_factors, stock_factors$ncff   > -1500000000)
#plot(stock_factors$ncff)
#View(stock_factors_new)
stock_factors<-subset(stock_factors, stock_factors$ncfi   < 1000000000)
stock_factors<-subset(stock_factors, stock_factors$ncfi   < 1000000000)
#plot(stock_factors$ncfi)
stock_factors<-subset(stock_factors, stock_factors$ncfi   > - 2000000000)
#plot(stock_factors$ncfi)
stock_factors<-subset(stock_factors, stock_factors$ncfo   < 1500000000)
#plot(stock_factors$ncfo)
stock_factors<-subset(stock_factors, stock_factors$ncfo   > -500000000)
#plot(stock_factors$ncfo)
stock_factors_new <- stock_factors
#plot(stock_factors$netinc)
stock_factors<-subset(stock_factors, stock_factors$netinc   > -500000000)
#plot(stock_factors$netinc)
#plot(stock_factors$netinc)
stock_factors<-subset(stock_factors, stock_factors$netinc   < 1000000000)
#plot(stock_factors$netinc)
#plot(stock_factors$pb)
stock_factors<-subset(stock_factors, stock_factors$pb   < 10000)
stock_factors<-subset(stock_factors, stock_factors$pb   <200)
stock_factors<-subset(stock_factors, stock_factors$pb   > -200)
#plot(stock_factors$pb)
#plot(stock_factors$pe1)
stock_factors<-subset(stock_factors, stock_factors$pe1 > -1000)
stock_factors<-subset(stock_factors, stock_factors$pe1 < 1000)
#plot(stock_factors$pe1)
stock_factors_new <- stock_factors
#plot(stock_factors$sharesbas)
stock_factors<-subset(stock_factors, stock_factors$sharesbas < 800000000)
#plot(stock_factors$sharesbas)
#plot(stock_factors$tangibles)
stock_factors<-subset(stock_factors, stock_factors$tangibles < 20000000000)
#plot(stock_factors$tangibles)
#plot(stock_factors$workingcapital)
stock_factors<-subset(stock_factors, stock_factors$workingcapital >  -1500000000)
#plot(stock_factors$workingcapital)
stock_factors<-subset(stock_factors, stock_factors$workingcapital < 6000000000)
#plot(stock_factors$workingcapital)
stock_factors_new <- stock_factors

#------------------------------------end removal outliers-----------------

#View (stock_factors_new)

#####################Multicollinearity Removal#########



stock_factors_new <- stock_factors_new[ ,-c(1,22)]
sink("/Users/jyothi/Desktop/cor.txt")
cor(stock_factors_new)

stock_factors <- stock_factors[  , -c(2,6,20)]

### Unique Dates
## -- Train 
#2011-03-31 
#2011-06-30 
#2011-09-30 
#2011-12-31 
#2012-03-31 
#2012-06-30
#2012-09-30
#2012-12-31 
#2013-03-31 
#2013-06-30 
#2013-09-30 
#2013-12-31 
#2014-03-31
#2014-06-30 
#2014-09-30 
# --- Test 
#2014-12-31 
#2015-03-31 
#2015-06-30 
#2015-09-30 
#2015-12-31 
sink()

summary(stock_factors)
#View(stock_factors)
#stock_factors <- na.omit(stock_factors)

#Finding the unique dates 

stock_factors<-unique(stock_factors)
#unique(stock_factors$calendardate)
#View(cal_date)
#View(stock_factors)


## remove Calender Return price  to normalise rest of the factors.
data_norm<-stock_factors[  , -c(19)]


for (k in 2:18) {
  #normalizing each independent var
  x<-(data_norm[,k]-mean(data_norm[,k]))/sd(data_norm[,k])
  #merging the normalized var to the original dataset
  data_norm<-cbind(data_norm,x)
  #naming the normalized variable
  names(data_norm)[NCOL(data_norm)]<-paste(colnames(data_norm[k]),"_n",sep="")
  
}

### Select normalised data , calenderdate and Return price.
stock_factors_date <- data_norm[c(1,19:35)]
stock_factors_date["return_price"]<-stock_factors[,19]

### Subset data by calendar Date 
stock_factor_2011_06_30  <- subset(stock_factors_date , calendardate =='2011-06-30')
stock_factor_2011_06_30 <- stock_factor_2011_06_30[,-(1)]
stock_factor_2011_09_30 <- subset(stock_factors_date , calendardate =='2011-09-30' )
stock_factor_2011_09_30 <- stock_factor_2011_09_30[,-1]
stock_factor_2011_12_31 <- subset(stock_factors_date , calendardate =='2011-12-31' )
stock_factor_2011_12_31 <- stock_factor_2011_12_31[,-1]
stock_factor_2012_03_31 <- subset(stock_factors_date , calendardate =='2012-03-31' )
stock_factor_2012_03_31 <- stock_factor_2012_03_31[,-1]
stock_factor_2012_06_30 <- subset(stock_factors_date , calendardate =='2012-06-30' )
stock_factor_2012_06_30 <- stock_factor_2012_06_30[,-1]
stock_factor_2012_09_30 <- subset(stock_factors_date , calendardate =='2012-09-30' )
stock_factor_2012_09_30 <- stock_factor_2012_09_30[,-1]
stock_factor_2012_12_31  <- subset(stock_factors_date , calendardate =='2012-12-31' )
stock_factor_2012_12_31 <- stock_factor_2012_12_31[,-1]
stock_factor_2013_03_31 <- subset(stock_factors_date , calendardate =='2013-03-31' )
stock_factor_2013_03_31  <- stock_factor_2013_03_31 [,-1]
stock_factor_2013_06_30 <- subset(stock_factors_date , calendardate =='2013-06-30' )
stock_factor_2013_06_30 <- stock_factor_2013_06_30[,-1]
stock_factor_2013_09_30 <- subset(stock_factors_date , calendardate =='2013-09-30' )
stock_factor_2013_09_30 <- stock_factor_2013_09_30[,-1]
stock_factor_2013_12_31 <- subset(stock_factors_date , calendardate =='2013-12-31' )
stock_factor_2013_12_31 <- stock_factor_2013_12_31[,-1]
stock_factor_2014_03_31 <- subset(stock_factors_date , calendardate =='2014-03-31' )
stock_factor_2014_03_31 <- stock_factor_2014_03_31[,-1]
stock_factor_2014_06_30 <- subset(stock_factors_date , calendardate =='2014-06-30' )
stock_factor_2014_06_30 <- stock_factor_2014_06_30[,-1]
stock_factor_2014_09_30 <- subset(stock_factors_date , calendardate =='2014-09-30' )
stock_factor_2014_09_30 <- stock_factor_2014_09_30[,-1]

# Creates List of 15 stock_factors for each calender date
my.list <- list( stock_factor_2011_06_30, stock_factor_2011_09_30,
                stock_factor_2011_12_31,stock_factor_2012_03_31,stock_factor_2012_06_30,stock_factor_2012_09_30,stock_factor_2012_12_31,stock_factor_2013_03_31,
                stock_factor_2013_06_30,stock_factor_2013_09_30,stock_factor_2013_12_31,stock_factor_2014_03_31,
                stock_factor_2014_06_30,stock_factor_2014_09_30)
#View(my.list[1])
for (i in 1:length(my.list)) {
  obj <- as.data.frame(my.list[i])
  lm_model <- lm(return_price ~ . , data = obj)
  print(summary(lm_model))
  #View(my.list[i])
}







