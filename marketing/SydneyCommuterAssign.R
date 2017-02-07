
load("/Users/jyothi/Downloads/MDS_Chapter_2/correlation_heat_map.RData")  # from R utility programs
# read data from comma-delimited text file... create data frame object
sydney <- read.csv("/Users/jyothi/Downloads/MDS_Chapter_2/sydney.csv")
names(sydney) <- 
  c("Car_Time", "Car_Cost", "Train_Time", "Train_Cost", "Choice")
plotting_data_frame <- sydney[, 1:4]
# scatter plot matrix with simple linear regression
# models and lowess smooth fits for variable pairs
pdf(file = "fig_predicting_choice_scatter_plot_matrix.pdf", 
    width = 8.5, height = 8.5)
pairs(plotting_data_frame,
      panel = function(x, y) {
        points(x, y, cex = 0.5)
        abline(lm(y ~ x), lty = "solid", col = "red")
        lines(lowess(x, y))
      }
)
dev.off()  

# correlation heat map for the explanatory variables
pdf(file = "fig_predicting_choice_correlation_heat_map.pdf",
    width = 8.5, height = 8.5)
sydney_cormat <- 
  cor(sydney[, c("Car_Time", "Car_Cost", "Train_Time", "Train_Cost")])
correlation_heat_map(sydney_cormat)
dev.off()

# specify and fit logistic regression model
sydney_model <- {Choice ~ Car_Time + Car_Cost + Train_Time + Train_Cost}
sydney_fit <- glm(sydney_model, family=binomial, data=sydney)
print(summary(sydney_fit))
print(anova(sydney_fit, test="Chisq"))

# compute predicted probability of taking the train 
sydney$Predict_Prob_TRAIN <- predict.glm(sydney_fit, type = "response") 

pdf(file = "fig_predicting_choice_density_evaluation.pdf", 
    width = 8.5, height = 8.5)
plotting_object <- densityplot( ~ Predict_Prob_TRAIN | Choice, 
                                data = sydney, 
                                layout = c(1,2), aspect=1, col = "darkblue", 
                                plot.points = "rug",
                                strip=function(...) strip.default(..., style=1),
                                xlab="Predicted Probability of Taking Train") 
print(plotting_object) 
dev.off()

# predicted car-or-train choice using 0.5 cut-off
sydney$Predict_Choice <- ifelse((sydney$Predict_Prob_TRAIN > 0.5), 2, 1)
sydney$Predict_Choice <- factor(sydney$Predict_Choice,
                                levels = c(1, 2), labels = c("CAR", "TRAIN"))

confusion_matrix <- table(sydney$Predict_Choice, sydney$Choice)
cat("\nConfusion Matrix (rows = Predicted Choice, columns = Actual Choice\n")
print(confusion_matrix)
predictive_accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2])/
  sum(confusion_matrix)                                              
cat("\nPercent Accuracy: ", round(predictive_accuracy * 100, digits = 1))
Car_cost_vector <- seq(min(sydney$Car_Cost), max(sydney$Car_Cost), length=1000)
car_probability_vector <- numeric(1000)
for (i in 1:1000) {
Y.vector <- c(1, mean(sydney$Car_Time), mean(sydney$Train_Time),
Car_cost_vector[i], mean(sydney$Train_Cost))
car_probability_vector[i] <-
exp(Y.vector %*% beta.vector)/
(1 + exp(Y.vector %*% beta.vector))
}
#car_probability_vector
index <- 1  # beginning index for search
while (car_probability_vector[index] < 0.5) index <- index + 1
Solution_Price <- Car_cost_vector[index]
cat("\nSolution Price: ", Solution_Price)
Current_Mean_Price <- mean(sydney$Car_Cost)
Current_Mean_Price
Cents_Lower <- ceiling(Current_Mean_Price - Solution_Price)
Cents_Lower
cat("\nLower prices by ", Cents_Lower, "cents\n")

pdf(file = "fig_predicting_choice_ticket_price_solution.pdf", 
    width = 8.5, height = 8.5) 
plot(train_cost_vector, train_probability_vector,
     type="l",ylim=c(0,1.0), las = 1, 
     xlab="Cost of Taking the Train (in cents)",
     ylab="Estimated Probability of Taking the Train")
# plot current average train ticket price as vertical line     
abline(v = Current_Mean_Price, col = "red", lty = "solid", lwd = 2)    
abline(v = Solution_Price, col = "blue", lty = "dashed", lwd = 2)
legend("topright", legend = c("Current Mean Train Ticket Price", 
paste("Solution Price (", Cents_Lower, " Cents Lower)", sep = "")), 
col = c("red", "blue"), pch = c(NA, NA), lwd = c(2, 2),
border = "black", lty = c("solid", "dashed"), cex = 1.25)
dev.off()   
