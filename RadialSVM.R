#Procedure Fallowed for SVM: 
#1.	Load Dataset 
#2.	Assigned parameters to compute average of  50 iterations
#3.	Iterated 50 times.
#4.	Split Dataset into 80:20 ratios for train and test 
#5.	Ran SVM with radial kernel  gamma =0.5 and Cost =1
#6.	Use tune function to find best model with range of Gamma and Cost
#7.	Manually compared best model with Computed model. 
#8.	We observed computed best model performed better. 
#9.	Adapted best model to predict with test data
#11.Repeat this procedure for 50 times with randomly assigned Test and Train datasets.
#12.Computed the average Matrix and Printed the confusion matrix.
#Repeated this procedure again with Kernel = "linear"
#Out of these two kernel Linear performed better. 
 
Heart<- read.csv("/Users/jyothi/Downloads/Heart.csv")
library(e1071)
#Remove NA's
hdata <- na.omit(Heart)
#Taking sample
hrt_smp_size <-floor(0.80* nrow(hdata))
# Declaring variables to store values for each 
# iteration to find average values. 
x1 <- 0
x2 <- 0
x3 <- 0  
x4 <- 0 
# Number of Iterations
factor <- 50

for(i in 1:factor){ # Iterating factor times
  train_ind <-sample(seq_len(nrow(hdata)), size = hrt_smp_size)
  heart_train <- Heart[train_ind,]
  heart_test  <-Heart[-train_ind,]
  heart_test <- na.omit(heart_test)
  heart_train <- na.omit(heart_train)
  heartsvm <- svm(AHD~ . , data=heart_train  ,method = "C-classification" ,kernel ="radial", cost=1 ,gamma=0.5)
  tune.out=tune(svm, AHD~., data=heart_train ,kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4) ))
 
  
   trainpred=predict(tune.out$best.model ,heart_test)

  x = table(predict=trainpred, truth=heart_test$AHD)
  x1 = x1 + x[1]
  x2=  x2 +  x[2]
  x3 = x3 +  x[3]
  x4= x4 +  x[4]
}

svm.out  <- matrix(c(x1/factor,x2/factor,x3/factor,x4/factor) , nrow=2,ncol=2)
colnames(svm.out) <- c("no", "yes")
rownames(svm.out) <- c("no", "yes")
svm.out
ctable1 <- as.table(matrix(svm.out, nrow = 2, byrow = TRUE))
fourfoldplot(ctable1, color = c("#CC6666", "#99CC49"),
             conf.level = 0, margin = 1, main = "Confusion Matrix SVM")
# Comparing with Radial, linear kernel showed better accuracy 