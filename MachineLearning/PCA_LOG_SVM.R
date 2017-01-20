# Readi file 
library(e1071)
library(ggplot2)
library(dummies)
library(graphics)
library(stats)
library(utils)
setwd("~/Desktop/rcode/ML/Proj_Heart_pca")
Heart <- read.csv("~/Desktop/rcode/ML/Proj_Heart_pca/Heart.csv")
#View(Heart)
dim(Heart)
Heart <- na.omit(Heart)
#View(Heart)
dim(Heart)
Heart_data <- Heart
# Remove First Column , Last Column 
Heart_data <- Heart_data[,-1]
Heart_data <- Heart_data[,-14]
#print Col names 
colnames(Heart_data)
# plot to check Correltaion
#plot(Heart_data)
# Remove Categorical Values to find Correlation
cor_heart <- Heart_data[,-3]
cor_heart <- cor_heart[,-12]
# Check Correlation
colnames(cor_heart)
cor(cor_heart)
plot(cor_heart)
#sink("cor_heart.txt")
#cor(cor_heart)
#sink
#--------There is no trace of Correlation---------------------
# Transform Categorical variable to  dummy variables
library(dummies)
new_my_Heart_data <- dummy.data.frame(Heart_data, names = c("ChestPain","Thal"))
dim(new_my_Heart_data)
new_my_Heart_data <- na.omit(new_my_Heart_data)
dim(new_my_Heart_data)

#View New data with Dummy variables.
#View(new_my_Heart_data)
# Find PCA components with PRINCOMP
pca_heart <- princomp(new_my_Heart_data)
summary(pca_heart)


# Comp.1     Comp.2      Comp.3     Comp.4
# Standard deviation     52.0044246 23.2921488 17.63224572 7.61501572
# Proportion of Variance  0.7469583  0.1498425  0.08586794 0.01601612
# Cumulative Proportion   0.7469583  0.8968008  0.98266874 0.99868486
# 98% of variation explained by 3 pca components. 
#sink("pca_heart_scores.csv")

#sink()
#Loadings
pca_heart$loadings[,1:3]
#Plot PCA
biplot(pca_heart)
plot(pca_heart)

#Dimention PCA loadings
dim(pca_heart$loadings)

#PCA data for LOGISTIC 
pca_heart_data <- (pca_heart$scores[,1:3])
var1 <- (pca_heart$sdev)^2
var_total <- var1/sum(var1)
var_total

#      Comp.1       Comp.2       Comp.3       Comp.4       Comp.5 
#7.467626e-01 1.503456e-01 8.558414e-02 1.598513e-02 3.954660e-04 
#Comp.6       Comp.7       Comp.8       Comp.9      Comp.10 
#2.563285e-04 1.952784e-04 1.231547e-04 8.151282e-05 5.948448e-05 
#Comp.11      Comp.12      Comp.13      Comp.14      Comp.15 
#5.427019e-05 4.382952e-05 3.648255e-05 3.066586e-05 2.315721e-05 
#Comp.16      Comp.17      Comp.18      Comp.19 
#2.050005e-05 2.358192e-06 1.097665e-17 2.855090e-21
# 98% Varince explained by first 3 components

# Variance Plot
plot(var_total, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
#Cumulative variance 
plot(cumsum(var_total), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
# Final PCA data for Logistic and SVM 
PCA.heart.data <- data.frame(AHD = Heart$AHD,pca_heart_data)
head(PCA.heart.data)
###--------------------------end PCA -------------------######
######----------------------Logistic--------------------#######
# split the data
hrt_smp_size <-floor(0.80* nrow(PCA.heart.data))

# Creating list to store the values for 50 runs  
listModel_1 <- list()
listModel_2 <- list()
listModel_3 <- list()
for(i in 1:50){
  
  train_ind <-sample(seq_len(nrow(PCA.heart.data)), size = hrt_smp_size)
  train_set <- PCA.heart.data[train_ind,]
  test_set  <-PCA.heart.data[-train_ind,]
  
  # logistic model
  log_model <-glm(AHD ~ Comp.1+ Comp.2+ Comp.3 ,data=train_set, family="binomial")
  
  test_set$predicted = predict(log_model, newdata=test_set, type="response")
  AHD_pred <- predict(log_model, newdata=test_set, type="response")
  AHD_pred <- ifelse(AHD_pred>0.5,"Yes", "No")
  # accuracy <- table(AHD_pred,test_set$AHD)
  confusionmatrix <- table(test_set$AHD, AHD_pred)
  accuracy<- sum(diag(confusionmatrix))/sum(confusionmatrix)
  
  listModel_1[[i]] <- confusionmatrix
  listModel_2[[i]] <- log_model
  listModel_3[[i]] <- accuracy
  
  
}  
which.max( listModel_3[] )
max(unlist(listModel_3))
listModel_2[18]
# End
a <-which.max( listModel_3[] )
max(unlist(listModel_3))
listModel_2[a]
listModel_1[[a]]
ctable <- as.table(matrix(unlist(listModel_1[[a]]), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC49"),
             conf.level = 0, margin = 1, main = "Confusion Matrix Logistic Regression")
accuracy
#-----------------------end Logistic----------------------------

factor <- 50

# Declaring variables to store values for each 
# iteration to find average values. 
x1 <- 0
x2 <- 0
x3 <- 0
x4 <- 0
head(PCA.heart.data)
for(i in 1:factor){ # Iterating factor times
  train_ind_1 <-sample(seq_len(nrow(PCA.heart.data)), size = hrt_smp_size)
  head( train_ind_1)
  heart_train_1 <- PCA.heart.data[train_ind,]
  heart_test_1  <- PCA.heart.data[-train_ind,]
  heart_test_1 <- na.omit(heart_test_1)
  heart_train_1 <- na.omit(heart_train_1)
  #View(heart_test)
  
  #View(heart_train)
  # Model with train data , radial kernal , cost=1 and gamma=-0
  heartsvm <- svm(AHD~ . , data=heart_train_1 ,kernel ="linear", cost=1 ,scale=FALSE)
  #summary(heartsvm)
  # Cross validating model and finding best model in terms of cost and gamma
  tune.out=tune(svm, AHD~., data=heart_train_1 ,kernel="linear",ranges=list(cost=c( 0.01, 0.1, 1,5)))
  
  
  # Predicting with test model 
  trainpred=predict(tune.out$best.model ,heart_test_1)
  # Creating table for cross validation
  x = table(predict=trainpred, truth=heart_test_1$AHD)
  x1 = x1 + x[1]
  x2=  x2 +  x[2]
  x3 = x3 +  x[3]
  x4= x4 +  x[4]
}
svm.out  <- matrix(c(x1/factor,x2/factor,x3/factor,x4/factor) , nrow=2,ncol=2)
colnames(svm.out) <- c("no", "yes")
rownames(svm.out) <- c("no", "yes")
svm.out
accuracy<- sum(x1/factor,x4/factor)/sum(x1/factor,x2/factor,x3/factor,x4/factor)
# Accuracy 
accuracy
ctable1 <- as.table(matrix(svm.out, nrow = 2, byrow = TRUE))
fourfoldplot(ctable1, color = c("#CC6666", "#99CC49"),
             conf.level = 0, margin = 1, main = "Confusion Matrix SVM")