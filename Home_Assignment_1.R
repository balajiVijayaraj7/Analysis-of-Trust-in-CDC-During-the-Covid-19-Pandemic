#--------------------------------Question 1 ------------####

# Libraries ####
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(corrplot)
library(leaps)
library(caret)
library(naivebayes)
library(MASS)
library(class)
library(nnet)

# read the Excel file into R ####
data <- read_excel("/Users/balajivijayaraj/Desktop/Statistical_Learning/HomeAssignment1/Conspiracy.xlsx", sheet = "3824426")

head(data)

data <- as.data.frame(data)
summary(data)

c_data <- subset(data, select = -c(cons_biowpn, cons_covax, cons_biowpn_dummy, cons_covax_dummy, weight))
summary(c_data)

# Add new column with the 1 as distrust and 0 as trust ####
c_data_1 <- c_data %>% 
  mutate(distrust = ifelse(trust_1 %in% c(1, 2), 1, 0))
#remove trust_1 column
c_data_1 <- c_data_1[-1]
colnames(c_data_1)

# Calculating p value and log odds for each predictor individually ####
for (predictor in c("populism_1", "populism_2", "populism_3", "populism_4", "populism_5", "age", "gender", "hhi", "hispanic", "cov_beh_sum", "white", "idlg", "highered", "pid3", "pid2", "md_radio", "md_national", "md_broadcast", "md_localtv", "md_localpap", "md_fox", "md_con", "md_agg", "rw_news", "ms_news")) {
  logit_model <- glm(distrust ~ ., data = na.omit(c_data_1[,c("distrust", predictor)]), family = binomial(link = "logit"))
  estimate <- coef(logit_model)[2]
  p_value <- summary(logit_model)$coefficients[2,4]
  if (estimate > 0) {
    log_odds <- log(estimate)
  } else {
    log_odds <- log(1/abs(estimate))
  }
  cat(sprintf("%-15s estimate: %-10f p-value: %-10f log odds: %-10f\n", predictor, estimate, p_value, log_odds))
}




#-------------------------------Question 2 --------------####

# Load the data ####
data <- read_excel("/Users/balajivijayaraj/Desktop/Statistical_Learning/HomeAssignment1/Conspiracy.xlsx", sheet = "3824426")
head(data)

data <- as.data.frame(data)
summary(data)

c_data <- subset(data, select = -c(cons_biowpn, cons_covax, cons_biowpn_dummy, cons_covax_dummy, weight))
summary(c_data)
# New column with 1 as trust and 0 as distrust ####
c_data_3 <- c_data %>% 
  mutate(trust = ifelse(trust_1 == 4, 1, 0))

#remove trust_1 column
c_data_3 <- c_data_3[-1]

# compute correlation matrix ####
cor_matrix <- cor(c_data_3, use = "complete.obs") #removing NA

# plot correlation matrix
corrplot(cor_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, col = colorRampPalette(c("red", "white", "blue"))(100), 
         cl.pos = "n", addCoef.col = "black", 
         number.cex = 0.7, number.round = 2)

summary(c_data_3)

# > backward$coefficients
# (Intercept)   populism_1   populism_2   populism_5          age 
# -0.380303010  0.071981856  0.060317557 -0.029158795  0.001662601 
# hhi  cov_beh_sum        white         pid3     md_radio 
# 0.007682389  0.009155182  0.088208891 -0.045834386  0.029660454 
# md_broadcast       md_con       md_agg 
# 0.064232600 -0.034252393  0.040762083 

# Backward Selection - 1  ####
# create the full model
full_model <- glm(trust ~ ., data = na.omit(c_data_3))
# perform backward selection
backward_model <- step(full_model, direction = "backward")

# Backward Selection - 2  ####
# create the full model
full_model <- glm(trust ~ ., data = na.omit(c_data_3), family = "binomial")

# perform backward selection
backward_model <- step(full_model, direction = "backward")


#to check if the data is balanced or not
table(c_data_3$trust)

c_data_3$trust <- as.factor(c_data_3$trust)
c_data_new$trust <- as.factor(c_data_new$trust)

# Creating the dataframe with the selected predictors ####
c_data_new <- data.frame(c_data_3$trust, c_data_3$populism_1, c_data$populism_2, c_data$age, c_data_3$hhi, c_data_3$cov_beh_sum, c_data_3$white, c_data_3$pid2, c_data_3$md_radio, c_data_3$md_broadcast, c_data_3$md_con, c_data_3$md_agg)
colnames(c_data_new) <- c("trust", "populism_1", "populism_2", "age", "hhi", "cov_beh_sum", "white", "pid2", "md_radio", "md_broadcast", "md_con", "md_agg")
nrow(na.omit(c_data_new))
nrow(na.omit(c_data_3))
nrow(c_data_na)

c_data_na <- na.omit(c_data_new)
c_data_na$trust <- as.factor(c_data_na$trust)
table(c_data_na$trust)
summary(c_data_new)

# Split the data into training and test sets ####
set.seed(123)
train_index <- createDataPartition(c_data_na$trust, p = 0.7, list = FALSE)
train_data <- c_data_na[train_index, ]
test_data <- c_data_na[-train_index, ]



# Naive Bayes model ####
nb_model <- naive_bayes(trust ~ populism_1 + populism_2 + white + age + hhi + cov_beh_sum + pid2 + md_broadcast + md_radio + md_agg + md_con , data = train_data)
summary(nb_model)

# Make predictions on the train set
train_predictions <- predict(nb_model, newdata = train_data)

# Create confusion matrix for train set
train_conf_matrix <- table(factor(train_predictions), factor(train_data$trust))

# Calculate training accuracy
train_accuracy <- sum(diag(train_conf_matrix)) / sum(train_conf_matrix)
train_accuracy

# Make predictions on the test set
test_predictions <- predict(nb_model, newdata = test_data)

# Create confusion matrix for test set
test_conf_matrix <- table(factor(test_predictions), factor(test_data$trust))

# Calculate test accuracy
test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_accuracy

# Extract TP, FP, TN, FN from confusion matrix
TP <- test_conf_matrix[2,2]
FP <- test_conf_matrix[1,2]
TN <- test_conf_matrix[1,1]
FN <- test_conf_matrix[2,1]

# Calculate sensitivity (true positive rate)
sensitivity <- TP / (TP + FN)

# Calculate specificity (true negative rate)
specificity <- TN / (TN + FP)

# Calculate precision and recall
precision <- test_conf_matrix[2,2] / sum(test_conf_matrix[2,])
recall <- test_conf_matrix[2,2] / sum(test_conf_matrix[,2])

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
print("Naive Bayes model")
print(test_conf_matrix)
print(paste0("Sensitivity: ", sensitivity))
print(paste0("Specificity: ", specificity))
print(paste0("Train Accuracy: ", train_accuracy))
print(paste0("Test Accuracy: ", test_accuracy))
print(paste0("Precision: ",precision))
print(paste0("Recall: ",recall))
print(paste0("F1 score: ", f1_score))



# Logistic regression model ####
logit_model <- glm(trust ~ populism_1 + populism_2 + white + age + hhi + cov_beh_sum + pid2 + md_broadcast + md_radio + md_agg + md_con , data = train_data, family = binomial(link = "logit"))

# View model summary
summary(logit_model)

# Make predictions on the train set
train_predictions <- predict(logit_model, train_data, type = "response")

# Create confusion matrix for train set
train_conf_matrix <- table(Predicted = round(train_predictions), Actual = train_data$trust)

# Calculate train accuracy
train_accuracy <- sum(diag(train_conf_matrix)) / sum(train_conf_matrix)
train_accuracy

# Make predictions on the test set
predictions <- predict(logit_model, test_data, type = "response")

# Create confusion matrix
conf_matrix <- table(Predicted = round(predictions), Actual = test_data$trust)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Extract TP, FP, TN, FN from confusion matrix
TP <- conf_matrix[2,2]
FP <- conf_matrix[1,2]
TN <- conf_matrix[1,1]
FN <- conf_matrix[2,1]

# Calculate sensitivity (true positive rate)
sensitivity <- TP / (TP + FN)

# Calculate specificity (true negative rate)
specificity <- TN / (TN + FP)

# Calculate precision and recall
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
print("Logistic regression model")
print(conf_matrix)
print(paste0("Sensitivity: ", sensitivity))
print(paste0("Specificity: ", specificity))
print(paste0("Training Accuracy: ", train_accuracy))
print(paste0("Test Accuracy: ", accuracy))
print(paste0("Precision: ",precision))
print(paste0("Recall: ",recall))
print(paste0("F1 score: ", f1_score))





# LDA model ####
lda_model <- lda(trust ~ populism_1 + populism_2 + white + age + hhi + cov_beh_sum + pid2 + md_broadcast + md_radio + md_agg + md_con , data = train_data)
summary(lda_model)

# Make predictions on the training set
train_predictions <- predict(lda_model,newdata = train_data )

# Create confusion matrix
train_conf_matrix <- table(factor(train_predictions$class), factor(train_data$trust))

# Calculate accuracy
train_accuracy <- sum(diag(train_conf_matrix)) / sum(train_conf_matrix)
train_accuracy

# Make predictions on the test set
predictions <- predict(lda_model, newdata = test_data)

# Create confusion matrix
conf_matrix <- table(factor(predictions$class), factor(test_data$trust))

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

# Extract TP, FP, TN, FN from confusion matrix
TP <- conf_matrix[2,2]
FP <- conf_matrix[1,2]
TN <- conf_matrix[1,1]
FN <- conf_matrix[2,1]

# Calculate sensitivity (true positive rate)
sensitivity <- TP / (TP + FN)

# Calculate specificity (true negative rate)
specificity <- TN / (TN + FP)

# Calculate precision and recall
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
print("LDA model")
print(conf_matrix)
print(paste0("Sensitivity: ", sensitivity))
print(paste0("Specificity: ", specificity))
print(paste0("Training Accuracy: ", train_accuracy))
print(paste0("Test Accuracy: ", accuracy))
print(paste0("Precision: ",precision))
print(paste0("Recall: ",recall))
print(paste0("F1 score: ", f1_score))



# QDA model ####
qda_model <- qda(trust ~ populism_1 + populism_2 + white + age + hhi + cov_beh_sum + pid2 + md_broadcast + md_radio + md_agg + md_con, data = train_data)

# Make predictions on the training set
predictions_train <- predict(qda_model, newdata = train_data)

# Create confusion matrix for training set
conf_matrix_train <- table(factor(predictions_train$class), factor(train_data$trust))

# Calculate accuracy for training set
accuracy_train <- sum(diag(conf_matrix_train)) / sum(conf_matrix_train)
accuracy_train

# Make predictions on the test set
predictions <- predict(qda_model, newdata = test_data)

# Create confusion matrix
conf_matrix <- table(factor(predictions$class), factor(test_data$trust))

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Extract TP, FP, TN, FN from confusion matrix
TP <- conf_matrix[2,2]
FP <- conf_matrix[1,2]
TN <- conf_matrix[1,1]
FN <- conf_matrix[2,1]

# Calculate sensitivity (true positive rate)
sensitivity <- TP / (TP + FN)

# Calculate specificity (true negative rate)
specificity <- TN / (TN + FP)

# Calculate precision and recall
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
print("QDA model")
print(conf_matrix)
print(paste0("Sensitivity: ", sensitivity))
print(paste0("Specificity: ", specificity))
print(paste0("Training Accuracy: ", accuracy_train))
print(paste0("Test Accuracy: ", accuracy))
print(paste0("Precision: ",precision))
print(paste0("Recall: ",recall))
print(paste0("F1 score: ", f1_score))




# KNN model ####
# Define the number of neighbors to consider
k <- 5

knn_model <- knn(train = train_data[, c("populism_1", "populism_2",  "age", "hhi", "cov_beh_sum","white", "pid2", "md_radio","md_broadcast", "md_con" , "md_agg")], 
                 test = test_data[, c("populism_1", "populism_2",  "age", "hhi", "cov_beh_sum","white", "pid2", "md_radio","md_broadcast",  "md_con", "md_agg")], 
                 cl = train_data$trust, k = k)

# Make predictions on the test set
predictions <- as.numeric(knn_model)

# Create confusion matrix
conf_matrix <- table(predictions, test_data$trust)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Extract TP, FP, TN, FN from confusion matrix
TP <- conf_matrix[2,2]
FP <- conf_matrix[1,2]
TN <- conf_matrix[1,1]
FN <- conf_matrix[2,1]

# Calculate sensitivity (true positive rate)
sensitivity <- TP / (TP + FN)

# Calculate specificity (true negative rate)
specificity <- TN / (TN + FP)

# Calculate precision and recall
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
print("KNN model")
print(conf_matrix)
print(paste0("Sensitivity: ", sensitivity))
print(paste0("Specificity: ", specificity))
print(paste0("Accuracy: ", accuracy))
print(paste0("Precision: ",precision))
print(paste0("Recall: ",recall))
print(paste0("F1 score: ", f1_score))

