
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
library('ggplot2')
library('forecast')
library('tseries')
library('astsa')
library('forecast')

########################################################
#################Functions ----- Functions ------####### 
#########################################################
### Function ---- Computing  Return price
compute_return_price<-function(stock_data){
  return_price<-vector();
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
}

#################################################################
remove_outlier <- function(stock_factors){
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
  stock_factors
}

##################################################################################
norm_x_var <- function(data_norm){
  for (k in 2:18) {
    #normalizing each independent var
    x<-(data_norm[,k]-mean(data_norm[,k]))/sd(data_norm[,k])
    #merging the normalized var to the original dataset
    data_norm<-cbind(data_norm,x)
    #naming the normalized variable
    names(data_norm)[NCOL(data_norm)]<-paste(colnames(data_norm[k]),"_n",sep="")
  }
  data_norm <- data_norm[c(1,19:35)]
 data_norm
}

######################################################################################
##### subset data and create a list ####

subset_stockdata <- function(stock_factors_date){
  
  unique(stock_factors_date$calendardate)
  ##Sort by staock factors date
  stock_factors_date <- dplyr::arrange(stock_factors_date, calendardate)
  #View(stock_factors_date)
  #caldates <- unique(stock_factors_date$calendardate)
  ### Subset data by calendar Date 
  stock_factor_2011_03_31  <- subset(stock_factors_date , calendardate =='2011-06-30')
  stock_factor_2011_03_31 <- stock_factor_2011_03_31[,-(1)]
  #View(stock_factor_2011_03_31)
  stock_factor_2011_06_30  <- subset(stock_factors_date , calendardate =='2011-06-30')
  stock_factor_2011_06_30 <- stock_factor_2011_06_30[,-(1)]
  #View(stock_factor_2011_06_30)
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
  
  ######
  stock_factor_2014_12_31 <- subset(stock_factors_date , calendardate =='2014-12-31' )
  stock_factor_2014_12_31 <- stock_factor_2014_12_31[,-1]
  #View(stock_factor_2014_12_31)
  is.na(stock_factor_2014_12_31) <- sapply(stock_factor_2014_12_31, is.infinite)
  stock_factor_2014_12_31 <- na.omit(stock_factor_2014_12_31)
  #2015-03-31 
  stock_factor_2015_03_31 <- subset(stock_factors_date , calendardate =='2015-03-31' )
  stock_factor_2015_03_31 <- stock_factor_2015_03_31[,-1]
  is.na(stock_factor_2015_03_31) <- sapply(stock_factor_2015_03_31, is.infinite)
  stock_factor_2015_03_31 <- na.omit(stock_factor_2015_03_31)
  #View(stock_factor_2015_03_31)
  #2015-06-30 
  stock_factor_2015_06_30 <- subset(stock_factors_date , calendardate =='2015-06-30' )
  stock_factor_2015_06_30 <- stock_factor_2015_06_30[,-1]
  is.na(stock_factor_2015_06_30) <- sapply(stock_factor_2015_06_30, is.infinite)
  stock_factor_2015_06_30 <- na.omit(stock_factor_2015_06_30)
  #View(stock_factor_2015_06_30)
  #2015-09-30 
  stock_factor_2015_09_30 <- subset(stock_factors_date , calendardate =='2015-09-30' )
  stock_factor_2015_09_30 <- stock_factor_2015_09_30[,-1]
  is.na(stock_factor_2015_09_30) <- sapply(stock_factor_2015_09_30, is.infinite)
  stock_factor_2015_09_30 <- na.omit(stock_factor_2015_09_30)
  #View(stock_factor_2015_09_30)
  #2015-12-31
  stock_factor_2015_12_31 <- subset(stock_factors_date , calendardate =='2015-12-31' )
  stock_factor_2015_12_31 <- stock_factor_2015_12_31[,-1]
  is.na(stock_factor_2015_12_31) <- sapply(stock_factor_2015_12_31, is.infinite)
  stock_factor_2015_06_30 <- na.omit(stock_factor_2015_12_31)
  #View(stock_factor_2015_12_31)
  
  my.list <- list( stock_factor_2011_03_31, stock_factor_2011_06_30, stock_factor_2011_09_30,
                   stock_factor_2011_12_31,stock_factor_2012_03_31,stock_factor_2012_06_30,
                   stock_factor_2012_09_30,stock_factor_2012_12_31,stock_factor_2013_03_31,
                   stock_factor_2013_06_30,stock_factor_2013_09_30,stock_factor_2013_12_31,
                   stock_factor_2014_03_31,stock_factor_2014_06_30,stock_factor_2014_09_30,
   stock_factor_2014_12_31, stock_factor_2015_03_31, stock_factor_2015_06_30,stock_factor_2015_09_30,
   stock_factor_2015_12_31)
  
}

#####################################Prediction Dates ####################################
subset_pred_dates <- function(stock_factors_date) {
  
  stock_factor_2014_12_31 <- subset(stock_factors_date , calendardate =='2014-12-31' )
  stock_factor_2014_12_31 <- stock_factor_2014_12_31[,-1]
  #View(stock_factor_2014_12_31)
  is.na(stock_factor_2014_12_31) <- sapply(stock_factor_2014_12_31, is.infinite)
  stock_factor_2014_12_31 <- na.omit(stock_factor_2014_12_31)
  #2015-03-31 
  stock_factor_2015_03_31 <- subset(stock_factors_date , calendardate =='2015-03-31' )
  stock_factor_2015_03_31 <- stock_factor_2015_03_31[,-1]
  is.na(stock_factor_2015_03_31) <- sapply(stock_factor_2015_03_31, is.infinite)
  stock_factor_2015_03_31 <- na.omit(stock_factor_2015_03_31)
  #View(stock_factor_2015_03_31)
  #2015-06-30 
  stock_factor_2015_06_30 <- subset(stock_factors_date , calendardate =='2015-06-30' )
  stock_factor_2015_06_30 <- stock_factor_2015_06_30[,-1]
  is.na(stock_factor_2015_06_30) <- sapply(stock_factor_2015_06_30, is.infinite)
  stock_factor_2015_06_30 <- na.omit(stock_factor_2015_06_30)
  #View(stock_factor_2015_06_30)
  #2015-09-30 
  stock_factor_2015_09_30 <- subset(stock_factors_date , calendardate =='2015-09-30' )
  stock_factor_2015_09_30 <- stock_factor_2015_09_30[,-1]
  is.na(stock_factor_2015_09_30) <- sapply(stock_factor_2015_09_30, is.infinite)
  stock_factor_2015_09_30 <- na.omit(stock_factor_2015_09_30)
  #View(stock_factor_2015_09_30)
  #2015-12-31
  stock_factor_2015_12_31 <- subset(stock_factors_date , calendardate =='2015-12-31' )
  stock_factor_2015_12_31 <- stock_factor_2015_12_31[,-1]
  is.na(stock_factor_2015_12_31) <- sapply(stock_factor_2015_12_31, is.infinite)
  stock_factor_2015_06_30 <- na.omit(stock_factor_2015_12_31)
  #View(stock_factor_2015_12_31)
 
  my.pred.list <- list(stock_factor_2014_12_31, stock_factor_2015_03_31, stock_factor_2015_06_30, stock_factor_2015_09_30,
                       stock_factor_2015_12_31)
}
###########################

###############################linear model for 20 pred set of beta ##############################################
create_beta_matrix <- function(my.list){
  beta_matrix <- matrix()
  List <- list()
  for (i in 1:length(my.list)) {
    obj <- as.data.frame(my.list[i])
    lm_model <- lm(return_price~. , data = obj)
    print(summary(lm_model))
    betas <- lm_model$coefficients
    print(betas)
    List[[i]] <- betas
  }
  beta_matrix = do.call(rbind, List)
}

#################################Predicting future date  betas #############################################
find_future_beta <- function(beta_matrix){
  end_Date_matrix <- matrix( c(2014 , 2015 , 2015 , 2015, 2015, 4, 1, 2, 3,4),  nrow=5, ncol=2)
  for (k in (15:19) ){
    j = k-14
    pred_beta <- list()
    new_beta_matrix <- beta_matrix[1:k,]
    for (i in names(new_beta_matrix)) {
      if(i != 'dt'){
        beta = new_beta_matrix[i]
        model <- ts(beta, start=c(2011, 1), end=c(end_Date_matrix[j,1], end_Date_matrix[j,2]), frequency=4) 
        #par(mfrow = c(3, 1))
        #plot(model)
        #plot(acf(model))
        #plot(pacf(model))
        arima_model2 <-  arima(model,  order=c(2,0,0))
        #tsdiag(arima_model2)
        #par(mfrow = c(1,1))
        pred = predict(arima_model2, n.ahead = 1)
        pred_beta_list[j,i] <-pred$pred
        #fcast <- forecast(model, h=1)
        #plot(fcast)
      }
    }
  }
  pred_beta_list
}
#############################################################################################################
###------------------Calculate Predicted return price 
pred_stock_returns <- function(future_stock_factor_data, pred_beta) {
  pred_return_price_row  <- 0 
  x <- data.frame("pred_returnPrice" =1)
  stock_16 <- future_stock_factor_data[, -18]
for ( i in 1:nrow(stock_16) ){ 
  row <- stock_16[i,]
  #print(i)
  for (k in 1:length(pred_beta)) {
    if(k==1){
      #intercept
      pred_return_price_row <- pred_beta[k]
      #print(pred_return_price_row )
    }else {
      #print("in k")
      pred_return_price_row <- pred_return_price_row + pred_beta[k]*row[k-1]
      
    }
  }
  x[i,] <- pred_return_price_row
  pred_return_price_row  <- 0
  
}
  future_stock_factor_data["pred_returnPrice"] <- x$pred_returnPrice
  future_stock_factor_data
}

######################################################################################
# ------------- Make Quantiles
make_quantiles <- function(sorted_data){
  start = 600
  quantile_list = list()
  quantile_pred_list = list()
  for (i in 1:5){
    #name_pred= paste("QQuantile_pred_" , i, sep="")
    #name= paste("QQuantile_" , i, sep="")
    start = 1+600*(i-1)
    #print(start)
    end <- i*600
   
    
    Quantile <- sorted_data[start:end,]
    #assign(name_pred,   Quantile_pred)
    #assign(name, Quantile)
    quantile_list[i] <- mean(Quantile$return_price)
    quantile_pred_list[i] <- mean(Quantile$pred_returnPrice)
  }
  
  list (quantile_list ,  quantile_pred_list) 
}


######################################################
#######  Code starts here  ###########################
#######################################################
indicator_list<-c(3,7,15,18, 25,28,32,35, 39,78,42,49,57,58, 59, 61,68,70, 86,90,93,94)
stock_data <- read.csv('/Users/jyothi/Desktop/predictive/data_file_ARQ.csv', header = T, sep =',')
#colnames(stock_data)
#---------------Compute log Return price -----------------------
stock_data <- compute_return_price(stock_data)
#---------------- Remove ourliers
stock_factors <- na.omit(stock_data[indicator_list])
stock_factors <- remove_outlier(stock_factors)
#------------------------------------end removal outliers-----------------
#-----------------------Multicollinearity Removal--------------------#
stock_factors_new <- stock_factors
stock_factors_new <- stock_factors_new[ ,-c(1,22)]
sink("/Users/jyothi/Desktop/cor.txt")
cor(stock_factors_new)
stock_factors <- stock_factors[  , -c(2,6,20)]
#####-------------------------------------------###


stock_factors<-unique(stock_factors)
## remove Calender Return price to normalise rest of the factors.
data_norm<-stock_factors[  , -c(19)]

##------------- Normalize X variables
stock_factors_date <- norm_x_var(data_norm)
caldates <- unique(stock_factors_date$calendardate)
stock_factors_date["return_price"]<-stock_factors[,19]
##----------
stock_factors_date <- dplyr::arrange(stock_factors_date, calendardate)

#-----------------create a list DF 20 dates 
my.list <- subset_stockdata(stock_factors_date )
#---------------beta matrix---------------------
beta_matrix <- create_beta_matrix(my.list)


print(beta_matrix)

#######--Attach Calender date to Beta Matrix ------------
df_date  <- unique(stock_factors_date$calendardate)
df_date <- data.frame(matrix(unlist(caldates), nrow=20, byrow=T))
colnames(df_date)[1] <- "dt"
beta_matrix <- cbind(beta_matrix, df_date)
colnames(beta_matrix)[1] <- "intercept"
new_beta_matrix <- beta_matrix[1,]
#View(new_beta_matrix)

##### ----------------
pred_beta_list <- new_beta_matrix[1,]
#### Function compute future date betas  --------


pred_beta_list <- find_future_beta(beta_matrix)
#View(pred_beta_list)
## --------------------------------------

###### Compute   return_price with predicted Betas ############# 

date16_pred_beta.list <- as.list(pred_beta_list[1,-19])
date17_pred_beta.list <- as.list(pred_beta_list[2,-19])
date18_pred_beta.list <- as.list(pred_beta_list[3,-19])
date19_pred_beta.list <- as.list(pred_beta_list[4,-19])
 

##----------------Calculate return_price from forcast betas -----------
stock_factor_2014_12_31 <- as.data.frame(my.list[16])

stock_factor_2014_12_31<- pred_stock_returns(stock_factor_2014_12_31,date16_pred_beta.list)

stock_factor_2015_03_31 <- as.data.frame(my.list[17])
stock_factor_2015_03_31<- pred_stock_returns(stock_factor_2015_03_31,date17_pred_beta.list)
stock_factor_2014_06_30 <- as.data.frame(my.list[18])
stock_factor_2014_06_30<- pred_stock_returns(stock_factor_2014_06_30,date18_pred_beta.list)

stock_factor_2014_09_30 <- as.data.frame(my.list[19])
stock_factor_2014_09_30<- pred_stock_returns(stock_factor_2014_09_30,date19_pred_beta.list)


#View(stock_factor_2014_12_31)
#View(stock_factor_2015_03_31)
#View(stock_factor_2014_06_30)
#View(stock_factor_2014_09_30)


sorted_stock_factor_2014_12_31 <-stock_factor_2014_12_31[order(-stock_factor_2014_12_31$return_price),] 
sorted_stock_factor_2015_03_31 <-stock_factor_2015_03_31[order(-stock_factor_2015_03_31$return_price),] 
sorted_stock_factor_2014_06_30 <-stock_factor_2014_06_30[order(-stock_factor_2014_06_30$return_price),] 
sorted_stock_factor_2014_09_30 <-stock_factor_2014_09_30[order(-stock_factor_2014_09_30$return_price),] 



qlist1   <- make_quantiles(sorted_stock_factor_2014_12_31)
qlist2  <- make_quantiles(sorted_stock_factor_2015_03_31)
qlist3  <- make_quantiles(sorted_stock_factor_2014_06_30)
qlist4   <- make_quantiles(sorted_stock_factor_2014_09_30 )


x1<- unlist(qlist1[1])
y1<- unlist(qlist1[2])
x2 <- unlist(qlist2[1])
y2 <- unlist(qlist2[2])
x3 <- unlist(qlist3[1])
y3 <- unlist(qlist3[2])
x4 <- unlist(qlist4[1])
y4 <- unlist(qlist4[2])



x1_name <- "quantile_1"
y1_name <- "pred_quantile_1"
x2_name <- "quantile_2"
y2_name <- "pred_quantile_2"
x3_name <- "quantile_3"
y3_name <- "pred_quantile_3"
x4_name <- "quantile_4"
y4_name <- "pred_quantile_4"




final_quantile<- data.frame(x1,y1,x2,y2,x3,y3,x4,y4)
names(final_quantile) <- c(x1_name,y1_name,x2_name,y2_name,x3_name,y3_name,x4_name,y4_name)
print(final_quantile)























