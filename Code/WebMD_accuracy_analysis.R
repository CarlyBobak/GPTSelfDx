# Load required libraries
library(caret)
library(e1071)
library(pROC)
library(ggplot2)
library(tidyverse)
library(openxlsx)

# Read the data from CSV file
data <- read.csv("blinded_patient_data_with_adjusted_noise_and_id.csv")
data[data$Official_DX=="Cold","Official_DX"]<-"Common Cold"

# Define column names
official_col <- "Official_DX"
rater_col <- "Rater_DX_Clean"
chatgpt_cols <- c("ChatGPT_Clean_1", "ChatGPT_Clean_2", "ChatGPT_Clean_3")

# Confusion matrix function
confusionMatrixTable <- function(actual, predicted) {
  tab <- table(predicted, actual)
  tab <- addmargins(tab)
  tab <- cbind(tab, "Total" = rowSums(tab))
  tab <- rbind(tab, "Total" = colSums(tab))
  return(tab)
}

calculateMetrics <- function(actual, predicted) {
  levels_all <- union(levels(as.factor(actual)), levels(as.factor(predicted)))
  actual <- factor(as.factor(actual), levels = levels_all)
  predicted <- factor(as.factor(predicted), levels = levels_all)
  
  metrics <- list()
  for (disease in actual) {
    actual_bin <- (as.factor(ifelse(actual == disease, 1, 0)))
    predicted_bin <- (as.factor(ifelse(predicted == disease, 1, 0)))
    
    if(var(as.numeric(predicted_bin))==0){
      accuracy <- 0
      precision <- 0
      recall <- 0
      f1_score <- 0
    } else {
    
    confusion_mat <- confusionMatrix(predicted_bin, actual_bin, positive = "1")
    
    accuracy <- sum(actual_bin == predicted_bin) / length(actual_bin) * 100
    precision <- confusion_mat$byClass["Pos Pred Value"] * 100
    recall <- confusion_mat$byClass["Sensitivity"] * 100
    f1_score <- 2 * (precision * recall) / (precision + recall)
    
    }
    
    metrics[[disease]] <- list(accuracy = accuracy, precision = precision, 
                               recall = recall, f1_score = f1_score)
  }
  
  return(metrics)
}

# Calculate F1 Score
F1_Score <- function(actual, predicted) {
  precision <- posPredValue(actual, predicted)
  recall <- sensitivity(actual, predicted)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  return(f1_score)
}


# Store results
results <- list()

# Calculate metrics for human rater
results$rater <- calculateMetrics(data$Official_DX, data$Rater_DX_Clean)

# Calculate metrics for ChatGPT
for (col in chatgpt_cols) {
  results[[col]] <- calculateMetrics(data$Official_DX, data[[col]])
}

# Print results
for (col in c("rater", chatgpt_cols)) {
  cat("Metrics for", col, "\n")
  cat("Accuracy:", results[[col]]$accuracy, "%\n")
  cat("Precision:", results[[col]]$precision, "%\n")
  cat("Recall:", results[[col]]$recall, "%\n")
  cat("F1 Score:", results[[col]]$f1_score, "\n\n")
}

# Create confusion matrix for human rater
rater_confusion <- confusionMatrixTable(data$Official_DX, data$Rater_DX_Clean)
cat("Confusion Matrix for Rater:\n")
print(rater_confusion)

# Create confusion matrix for ChatGPT
for (col in chatgpt_cols) {
  chatgpt_confusion <- confusionMatrixTable(data$Official_DX, data[[col]])
  cat("\nConfusion Matrix for", col, ":\n")
  print(chatgpt_confusion)
}

# Calculate metrics for ChatGPT
for (col in chatgpt_cols) {
  results[[col]] <- calculateMetrics(data$Official_DX, data[[col]])
}

# Create a data frame to store the accuracy results for all diseases
accuracy_results <- data.frame(Disease = levels(as.factor(data$Official_DX)))

# Add accuracy values for the human rater
accuracy_results$Human_Rater_Accuracy <- sapply(accuracy_results$Disease, function(disease) {
  results$rater[[disease]]$accuracy
})

# Add accuracy values for ChatGPT Clean 1
accuracy_results$ChatGPT_Clean_1_Accuracy <- sapply(accuracy_results$Disease, function(disease) {
  results$ChatGPT_Clean_1[[disease]]$accuracy
})

# Add accuracy values for ChatGPT Clean 2
accuracy_results$ChatGPT_Clean_2_Accuracy <- sapply(accuracy_results$Disease, function(disease) {
  results$ChatGPT_Clean_2[[disease]]$accuracy
})

# Add accuracy values for ChatGPT Clean 3
accuracy_results$ChatGPT_Clean_3_Accuracy <- sapply(accuracy_results$Disease, function(disease) {
  results$ChatGPT_Clean_3[[disease]]$accuracy
})

# Reshape the data frame to have the required structure
accuracy_df <- reshape2::melt(accuracy_results, id.vars = "Disease", variable.name = "Predictor", value.name = "Accuracy")

# Print the accuracy results data frame
print(accuracy_df)

# Visualization of accuracy results
predictors <- c("Human Rater", chatgpt_cols)

# Filter the rows corresponding to Heart Disease, IBD, Prostate Cancer, and Breast Cancer
target_diseases <- c("Heart Disease", "IBD", "Prostate Cancer", "Breast Cancer")
filtered_rows <- accuracy_df[accuracy_df$Disease %in% target_diseases, ]

# Print the filtered rows
print(filtered_rows)

# Create a data frame to store the accuracy results for all diseases
accuracy_results <- data.frame(Disease = levels(as.factor(data$Official_DX)))

# Add accuracy values for the human rater
accuracy_results$Human_Rater_Accuracy <- sapply(accuracy_results$Disease, function(disease) {
  results$rater[[disease]]$accuracy
})

# Add accuracy values for ChatGPT Clean 1
accuracy_results$ChatGPT_Clean_1_Accuracy <- sapply(accuracy_results$Disease, function(disease) {
  results$ChatGPT_Clean_1[[disease]]$accuracy
})

# Add accuracy values for ChatGPT Clean 2
accuracy_results$ChatGPT_Clean_2_Accuracy <- sapply(accuracy_results$Disease, function(disease) {
  results$ChatGPT_Clean_2[[disease]]$accuracy
})

# Add accuracy values for ChatGPT Clean 3
accuracy_results$ChatGPT_Clean_3_Accuracy <- sapply(accuracy_results$Disease, function(disease) {
  results$ChatGPT_Clean_3[[disease]]$accuracy
})

# Create a new data frame with Disease, Human Accuracy, and Median ChatGPT Accuracy
accuracy_table <- data.frame(
  Disease = accuracy_results$Disease,
  Human = accuracy_results$Human_Rater_Accuracy,
  ChatGPT = apply(accuracy_results[, -1], 1, median)
)

# Print the accuracy table
print(accuracy_table)

# Prepare colors and labels based on your requirement
color_vector = c("skyblue", "red")
label_vector = c("Human", "ChatGPT")
names(color_vector) <- label_vector

# Combine the human accuracy and median ChatGPT accuracy into a single data frame
accuracy_plot_data <- accuracy_table %>%
  pivot_longer(cols = c(Human, ChatGPT), names_to = "Rater", values_to = "Accuracy")


# Plot the accuracy comparison using ggplot2
accuracy_bar_chart <- ggplot(accuracy_plot_data, aes(x = Disease, y = Accuracy, fill = Rater)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Accuracy Comparison", x = "Disease", y = "Accuracy (%)") +
  theme_classic() +  # Add classic theme
  scale_fill_manual(values = color_vector, labels = label_vector) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Display the bar chart
print(accuracy_bar_chart)

# Extracting the diseases
diseases <- levels(as.factor(data$Official_DX))

# Initialize a data frame to store the F1 scores
f1_scores_table <- data.frame(Disease = diseases)

# Add F1 scores for the human rater
f1_scores_table$Human_Rater_F1 <- sapply(f1_scores_table$Disease, function(disease) {
  results$rater[[disease]]$f1_score
})

# Add F1 scores for ChatGPT Clean 1
f1_scores_table$ChatGPT_Clean_1_F1 <- sapply(f1_scores_table$Disease, function(disease) {
  results$ChatGPT_Clean_1[[disease]]$f1_score
})

# Add F1 scores for ChatGPT Clean 2
f1_scores_table$ChatGPT_Clean_2_F1 <- sapply(f1_scores_table$Disease, function(disease) {
  results$ChatGPT_Clean_2[[disease]]$f1_score
})

# Add F1 scores for ChatGPT Clean 3
f1_scores_table$ChatGPT_Clean_3_F1 <- sapply(f1_scores_table$Disease, function(disease) {
  results$ChatGPT_Clean_3[[disease]]$f1_score
})

# Print the F1 scores table
print(f1_scores_table)

# ADDITIONAL CODE FOR ACCURACY TABLE (NOT INCLUDED IN PAPER)

# Create a new data frame to store the accuracy results for all diseases
accuracy_table <- data.frame(Disease = levels(as.factor(data$Official_DX)))

# Add accuracy values for the human rater
accuracy_table$Human_Rater_Accuracy <- sapply(accuracy_table$Disease, function(disease) {
  results$rater[[disease]]$accuracy
})

# Add accuracy values for ChatGPT Clean 1
accuracy_table$ChatGPT_Clean_1_Accuracy <- sapply(accuracy_table$Disease, function(disease) {
  results$ChatGPT_Clean_1[[disease]]$accuracy
})

# Add accuracy values for ChatGPT Clean 2
accuracy_table$ChatGPT_Clean_2_Accuracy <- sapply(accuracy_table$Disease, function(disease) {
  results$ChatGPT_Clean_2[[disease]]$accuracy
})

# Add accuracy values for ChatGPT Clean 3
accuracy_table$ChatGPT_Clean_3_Accuracy <- sapply(accuracy_table$Disease, function(disease) {
  results$ChatGPT_Clean_3[[disease]]$accuracy
})

# Print the accuracy table
print(accuracy_table)


