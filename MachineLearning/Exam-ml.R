# Readi file 
setwd("~/Desktop/rcode/ML/Proj_Heart_pca")
Heart <- read.csv("~/Desktop/rcode/ML/Proj_Heart_pca/Heart.csv")
View(Heart)
dim(Heart)
Heart <- na.omit(Heart)
View(Heart)
dim(Heart)
Heart_data <- Heart
# Remove First Column : its an index , Last Column -Response Variable
Heart_data <- Heart_data[,-1]
Heart_data <- Heart_data[,-14]
#print Col names 
colnames(Heart_data)
# plot to check Correltaion
plot(Heart_data)
# Remove Categorical Values to find Correlation
cor_heart <- Heart_data[,-3]
cor_heart <- cor_heart[,-12]
# Check Correlation
colnames(cor_heart)
cor(cor_heart)
plot(cor_heart)
sink("cor_heart.txt")
cor(cor_heart)
sink
#There isn o trace of Correlation
# Transform Categorical variable to  dummy variables
new_my_Heart_data <- dummy.data.frame(Heart_data, names = c("ChestPain","Thal"))
dim(new_my_Heart_data)
new_my_Heart_data <- na.omit(new_my_Heart_data)
dim(new_my_Heart_data)
#View New data with Dummy variables.
View(new_my_Heart_data)
# Remove NA

pca_heart <- princomp(new_my_Heart_data)
summary(pca_heart)

# Importance of components:
# Comp.1     Comp.2      Comp.3     Comp.4
# Standard deviation     52.0044246 23.2921488 17.63224572 7.61501572
# Proportion of Variance  0.7469583  0.1498425  0.08586794 0.01601612
# Cumulative Proportion   0.7469583  0.8968008  0.98266874 0.99868486
# 98% of variation explained by 3 pca components. 


sink("pca_heart_scores.csv")
sink()

pca_heart$loadings[,1:3]
# Dimention Pca Scores

#Dimention PCA loadings
dim(pca_heart$loadings)
#PCA data for LOGISTIC 
pca_heart_data <- (pca_heart$scores[,1:3])
#Plot PCA
biplot(pca_heart)
plot(pca_heart)
#The parameter scale = 0 ensures that arrows are scaled to
#represent the loadings. To make inference from image above,
var1 <- (pca_heart$sdev)^2

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
# Get data from PCA 
heart = na.omit(Heart)
dim(Heart)
PCA.heart.data <- data.frame(AHD = Heart$AHD, prin_comp$x)


