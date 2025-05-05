
#------RF------------

# Evaluate the model performance
set.seed(123)
conf_matrix_rf_cv <- confusionMatrix(rf_pred_cv, test_data$Group)
print(conf_matrix_rf_cv)
# Extract confusion matrix elements 
confusion_matrix_rf <- as.data.frame(conf_matrix_rf_cv$table)
confusion_matrix_rf
# Calculating MCC
TP <- as.double(confusion_matrix_rf[4, "Freq"])  # True Positives
TN <- as.double(confusion_matrix_rf[1, "Freq"])  # True Negatives
FP <- as.double(confusion_matrix_rf[2, "Freq"])  # False Positives
FN <- as.double(confusion_matrix_rf[3, "Freq"])  # False Negatives
TP
TN
FP
FN
# Calculating MCC
MCC_rf <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
# Printing the MCC
print(MCC_rf)
print(paste("Matthews Correlation Coefficient (MCC):", MCC_rf))



# ROC curve and AUC calculation
# Predict probabilities of the outcome for the test data
set.seed(123)
rf_probs <- predict(final_rf_model, newdata = test_data, type = "prob")
# Calculate the AUC value and its 95% confidence interval
roc_rf <- roc(test_data$T2D, rf_probs[, "Yes"], main="ROC Curves", percent=TRUE,
               # arguments for ci
               ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
               # arguments for plot
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE , legacy.axes = TRUE, col="blue" )
# Extract AUC and Confidence Interval
auc_rf <- roc_rf$auc
auc_rf
ci_auc_rf <- ci.auc(roc_rf)
ci_auc_rf
cat("AUC value:", auc_rf, "\n")
cat("95% Confidence Interval:", ci_auc_rf, "\n")

# Plot the ROC curve
plot(roc_rf, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")



#-----svm-------------
# Evaluate the model performance
set.seed(123)
conf_matrix_svm_cv <- confusionMatrix(svm_pred_cv, test_data$T2D)
print(conf_matrix_svm_cv)
confusion_matrix_svm <- as.data.frame(conf_matrix_svm_cv$table)
confusion_matrix_svm

TP <- as.double(confusion_matrix_svm[4, "Freq"])  # True Positives
TN <- as.double(confusion_matrix_svm[1, "Freq"])  # True Negatives
FP <- as.double(confusion_matrix_svm[2, "Freq"])  # False Positives
FN <- as.double(confusion_matrix_svm[3, "Freq"])  # False Negatives
TP
TN
FP
FN
# Calculating MCC
MCC_svm <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
# Printing the MCC
print(MCC_svm)
print(paste("Matthews Correlation Coefficient (MCC):", MCC_svm))


#ROC
# Predict probabilities of the outcome for the test data
set.seed(123)
svm_probs <- predict(final_svm_model, newdata = test_data, type = "prob", probability = TRUE)
# Calculate the AUC value and its 95% confidence interval
roc_svm <- roc(test_data$T2D, attr(svm_probs, "probabilities")[,"Yes"], main="ROC Curves", percent=TRUE,
               # arguments for ci
               ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
               # arguments for plot
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE , legacy.axes = TRUE, col="blue" )
roc_svm
auc_svm <- roc_svm$auc
ci_auc_svm <- ci.auc(roc_svm)
auc_svm
ci_auc_svm
cat("AUC value:", auc_svm, "\n")
cat("95% Confidence Interval:", ci_auc_svm, "\n")
# Plot the ROC curve
plot(roc_svm, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")



# -------------------LDA----------------------
# Evaluate the model performance

set.seed(123)
conf_matrix_lda_cv <- confusionMatrix(lda_pred_cv$class, test_data$T2D)
print(conf_matrix_lda_cv)

confusion_matrix_lda <- as.data.frame(conf_matrix_lda_cv$table)
confusion_matrix_lda

#MCC
TP <- as.double(confusion_matrix_lda[4, "Freq"])  # True Positives
TN <- as.double(confusion_matrix_lda[1, "Freq"])  # True Negatives
FP <- as.double(confusion_matrix_lda[2, "Freq"])  # False Positives
FN <- as.double(confusion_matrix_lda[3, "Freq"])  # False Negatives
TP
TN
FP
FN
# Calculating MCC
MCC_lda <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
# Printing the MCC
print(MCC_lda)
print(paste("Matthews Correlation Coefficient (MCC):", MCC_lda))




#ROC Curve
# Predict probabilities of the outcome for the test data
set.seed(123)
lda_probs <- predict(final_lda_model, newdata = test_data, type = "prob")
# Calculate the AUC value and its 95% confidence interval
roc_lda <- roc(test_data$T2D, lda_probs$posterior[,2], main="ROC Curves", percent=TRUE,
               # arguments for ci
               ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
               # arguments for plot
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE , legacy.axes = TRUE, col="blue" )
roc_lda
auc_lda <- roc_lda$auc
auc_lda
ci_auc_lda <- ci.auc(roc_lda)
ci_auc_lda
cat("AUC value:", auc_lda, "\n")
cat("95% Confidence Interval:", ci_auc_lda, "\n")
# Plot the ROC curve
plot(roc_lda, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")



# ------------------GLM------------------

# Evaluate the model performance

conf_matrix <- confusionMatrix(glm_predictions, test_data$T2D)
# Ensure consistency in factor levels
#test_data$T2D <- factor(test_data$T2D, levels = levels(factor(glm_predictions)))
conf_matrix_glm <- confusionMatrix(glm_predictions, test_data$T2D)
conf_matrix_glm
conf_matrix_glm$overall
conf_matrix_glm$byClass
conf_matrix_glm$table
confusion_matrix_glm <- as.data.frame(conf_matrix_glm$table)
confusion_matrix_glm

#MCC
TP <- as.double(confusion_matrix_glm[4, "Freq"])  # True Positives
TN <- as.double(confusion_matrix_glm[1, "Freq"])  # True Negatives
FP <- as.double(confusion_matrix_glm[2, "Freq"])  # False Positives
FN <- as.double(confusion_matrix_glm[3, "Freq"])  # False Negatives
TP
TN
FP
FN
# Calculating MCC
MCC_glm <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
# Printing the MCC
print(MCC_glm)
print(paste("Matthews Correlation Coefficient (MCC):", MCC_glm))




#ROC curve
# Predict probabilities of the outcome for the test data
set.seed(123)
lr_probs <- predict(glm_model, newdata = test_data, type = "prob")
# Calculate the AUC value and its 95% confidence interval
roc_lr <- roc(test_data$T2D, lr_probs[, "Yes"], main="ROC Curves", percent=TRUE,
               # arguments for ci
               ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
               # arguments for plot
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE , legacy.axes = TRUE, col="blue" )
auc_lr <- roc_lr$auc
ci_auc_lr <- ci.auc(roc_lr)
auc_lr
ci_auc_lr
cat("AUC value:", auc_lr, "\n")
cat("95% Confidence Interval:", ci_auc_lr, "\n")

# Plot the ROC curve
plot(roc_lr, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")



# -------------GBM -------------------------

# Evaluate the model performance

set.seed(123)

predictions <- final_gbm_model %>% predict(test_data)
gbm_pred_binary <- as.factor(ifelse(predictions> 0.5, "Yes", "No"))
#gbm_pred_binary
table(gbm_pred_binary)

# Convert test_data$T2D to a factor with the same levels as gbm_pred_binary
test_data$T2D <- factor(test_data$T2D, levels = c("No", "Yes"))
train_data$T2D <- factor(train_data$T2D, levels = c("No", "Yes"))

# Now, apply confusion matrix
set.seed(123)
conf_matrix_gbm_cv <- confusionMatrix(gbm_pred_binary, test_data$T2D)
print(conf_matrix_gbm_cv)
confusion_matrix_gbm <- as.data.frame(conf_matrix_gbm_cv$table)
confusion_matrix_gbm

#MCC
TP <- as.double(confusion_matrix_gbm[4, "Freq"])  # True Positives
TN <- as.double(confusion_matrix_gbm[1, "Freq"])  # True Negatives
FP <- as.double(confusion_matrix_gbm[2, "Freq"])  # False Positives
FN <- as.double(confusion_matrix_gbm[3, "Freq"])  # False Negatives
TP
TN
FP
FN
# Calculating MCC
MCC_gbm <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
# Printing the MCC
print(MCC_gbm)
print(paste("Matthews Correlation Coefficient (MCC):", MCC_gbm))




#ROC
set.seed(123)
gbm_probs <- predict(final_gbm_model, newdata = test_data, n.trees = final_gbm_model$n.trees, type = "response")
# Create a ROC curve object
roc_gbm <- roc(test_data$T2D, gbm_probs, main="ROC Curves", percent=TRUE,
               # arguments for ci
               ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
               # arguments for plot
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE , legacy.axes = TRUE, col="blue" )

roc_gbm
auc_gbm <- roc_gbm$auc
ci_auc_gbm <- ci.auc(roc_gbm)
auc_gbm
ci_auc_gbm
cat("AUC value:", auc_gbm, "\n")
cat("95% Confidence Interval:", ci_auc_gbm, "\n")
# Plot the ROC curve
plot(roc_gbm, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")



#-------DT-------------------


# 
conf_matrix_dt_cv <- confusionMatrix(predict_model_dt_cv, test_data$T2D)
print(conf_matrix_dt_cv)

confusion_matrix_dt <- as.data.frame(conf_matrix_dt_cv$table)
confusion_matrix_dt

#MCC
TP <- as.double(confusion_matrix_dt[4, "Freq"])  # True Positives
TN <- as.double(confusion_matrix_dt[1, "Freq"])  # True Negatives
FP <- as.double(confusion_matrix_dt[2, "Freq"])  # False Positives
FN <- as.double(confusion_matrix_dt[3, "Freq"])  # False Negatives
TP
TN
FP
FN
# Calculating MCC
MCC_dt <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
# Printing the MCC
print(MCC_dt)
print(paste("Matthews Correlation Coefficient (MCC):", MCC_dt))




#ROC
# Predict probabilities of the outcome for the test data
set.seed(123)
test_probs <- predict(final_dt_model_cv, newdata = test_data, type = "prob")
# Calculate the AUC value and its 95% confidence interval
roc_dt <- roc(test_data$T2D, test_probs[, "Yes"], main="ROC Curves", percent=TRUE,
               # arguments for ci
               ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
               # arguments for plot
               plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE , legacy.axes = TRUE, col="blue" )
auc_dt <- roc_dt$auc
ci_auc_dt <- ci.auc(roc_dt)
auc_dt
ci_auc_dt
cat("AUC value:", auc_dt, "\n")
cat("95% Confidence Interval:", ci_auc_dt, "\n")
# Plot the ROC curve
plot(roc_dt, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")




#-------table of performance metrics-------------------


#table of performance metrics
# Extract performance metrics for each model
metrics <- data.frame(
  Model = c("RF", "LR", "GBM", "SVM", "DT", "LDA"),
  Accuracy = c(conf_matrix_rf_cv$overall["Accuracy"],
               conf_matrix_glm$overall["Accuracy"],
               conf_matrix_gbm_cv$overall["Accuracy"],
               conf_matrix_svm_cv$overall["Accuracy"],
               conf_matrix_dt_cv$overall["Accuracy"],
               conf_matrix_lda_cv$overall["Accuracy"]),
  Precision = c(conf_matrix_rf_cv$byClass["Pos Pred Value"],
                conf_matrix_glm$byClass["Pos Pred Value"],
                conf_matrix_gbm_cv$byClass["Pos Pred Value"],
                conf_matrix_svm_cv$byClass["Pos Pred Value"],
                conf_matrix_dt_cv$byClass["Pos Pred Value"],
                conf_matrix_lda_cv$byClass["Pos Pred Value"]),
  Recall = c(conf_matrix_rf_cv$byClass["Sensitivity"],
             conf_matrix_glm$byClass["Sensitivity"],
             conf_matrix_gbm_cv$byClass["Sensitivity"],
             conf_matrix_svm_cv$byClass["Sensitivity"],
             conf_matrix_dt_cv$byClass["Sensitivity"],
             conf_matrix_lda_cv$byClass["Sensitivity"]),
  F1_score = c(conf_matrix_rf_cv$byClass["F1"],
               conf_matrix_glm$byClass["F1"],
               conf_matrix_gbm_cv$byClass["F1"],
               conf_matrix_svm_cv$byClass["F1"],
               conf_matrix_dt_cv$byClass["F1"],
               conf_matrix_lda_cv$byClass["F1"]),
MCC =   c(MCC_rf,
          MCC_glm,
          MCC_gbm, 
          MCC_svm,
          MCC_dt,
          MCC_lda),
AUC = c(auc_rf, auc_lr, auc_gbm, auc_svm, auc_dt, auc_lda),
  
AUC_CI_Lower = c(ci_auc_rf[1], ci_auc_lr[1], ci_auc_gbm[1], ci_auc_svm[1], ci_auc_dt[1], ci_auc_lda[1]),
  
AUC_CI_Upper = c(ci_auc_rf[2], ci_auc_lr[2], ci_auc_gbm[2], ci_auc_svm[2], ci_auc_dt[2], ci_auc_lda[2]),
  
Kappa = c(conf_matrix_rf_cv$overall["Kappa"],
            conf_matrix_glm$overall["Kappa"],
            conf_matrix_gbm_cv$overall["Kappa"],
            conf_matrix_svm_cv$overall["Kappa"],
            conf_matrix_dt_cv$overall["Kappa"],
            conf_matrix_lda_cv$overall["Kappa"])
)



# Print the table
print(metrics)




## -----------Save Predicted Probabilities  and all the models-------------------

write.csv(rf_probs, "rf_predicted_probs.csv", row.names = FALSE)
write.csv(svm_probs, "svm_predicted_probs.csv", row.names = FALSE)
write.csv(gbm_probs, "gbm_predicted_probs.csv", row.names = FALSE)
write.csv(lr_probs, "logreg_predicted_probs.csv", row.names = FALSE)
write.csv(dt_probs, "dt_predicted_probs.csv", row.names = FALSE)
write.csv(lda_probs, "lda_predicted_probs.csv", row.names = FALSE)



#save final models
# Save Random Forest model
saveRDS(final_rf_model, file = "final_rf_model.rds")

# Save SVM model
saveRDS(final_svm_model, file = "final_svm_model.rds")

# Save Logistic Regression model
saveRDS(final_glm_model, file = "final_glm_model.rds")

# Save LDA model
saveRDS(final_lda_model, file = "final_lda_model.rds")

# Save GBM model (if you have one)
saveRDS(final_gbm_model, file = "final_gbm_model.rds")

# Save Decision Tree model
saveRDS(final_dt_model_cv, file = "final_dt_model_cv.rds")



# Save the models

# Save Random Forest model
saveRDS(rf_model, file = "rf_model.rds")

# Save SVM model
saveRDS(svm_model, file = "svm_model.rds")

# Save Logistic Regression model
saveRDS(glm_model, file = "glm_model.rds")

# Save LDA model
saveRDS(lda_model, file = "lda_model.rds")

# Save GBM model (if you have one)
saveRDS(gbm_model, file = "gbm_model.rds")

# Save Decision Tree model
saveRDS(dt_model_cv, file = "dt_model_cv.rds")



model_list <- list(RF = rf_model, SVM = svm_model, GBM = gbm_model, LDA= lda_model, DT= dt_model_cv, GLM= glm_model  )
saveRDS(model_list, "all_models.rds")






# --------------Plot Calibration and PR Curves for All Models----------------



# Convert actual test labels to numeric (1 = T2D, 0 = No T2D)
actual_labels <- ifelse(test_data$T2D == "Yes", 1, 0)

# Create a dataframe for all models' probabilities
calibration_data <- data.frame(
  Observed = actual_labels,
  RF = rf_probs,
  SVM = svm_probs,
  GBM = gbm_probs,
  LDA = lda_probs,
  LR = lr_probs,
  DT = dt_probs
)

# Reshape data for ggplot
calibration_long <- calibration_data %>%
  pivot_longer(cols = -Observed, names_to = "Model", values_to = "Predicted")

# Bin predicted probabilities
calibration_long$Binned <- cut(calibration_long$Predicted, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)

# Compute observed vs predicted probability per bin
calibration_summary <- calibration_long %>%
  group_by(Model, Binned) %>%
  summarise(Observed_Mean = mean(Observed), Predicted_Mean = mean(Predicted), .groups = 'drop')

# Plot calibration curves for all models
ggplot(calibration_summary, aes(x = Predicted_Mean, y = Observed_Mean, color = Model)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Calibration Curves for All Models",
       x = "Predicted Probability",
       y = "Observed Probability") +
  theme_minimal()

# Compute Brier Scores for all models
brier_scores <- data.frame(
  Model = c("Random Forest", "SVM", "GBM", "LDA", "Logistic Regression", "Decision Tree"),
  Brier_Score = c(
    mean((rf_probs - actual_labels)^2),
    mean((svm_probs - actual_labels)^2),
    mean((gbm_probs - actual_labels)^2),
    mean((lda_probs - actual_labels)^2),
    mean((lr_probs - actual_labels)^2),
    mean((dt_probs - actual_labels)^2)
  )
)

print(brier_scores)

# Compute PR Curves and PR-AUC for all models
pr_curves <- list(
  "Random Forest" = pr.curve(scores.class0 = rf_probs, weights.class0 = actual_labels, curve = TRUE),
  "SVM" = pr.curve(scores.class0 = svm_probs, weights.class0 = actual_labels, curve = TRUE),
  "GBM" = pr.curve(scores.class0 = gbm_probs, weights.class0 = actual_labels, curve = TRUE),
  "LDA" = pr.curve(scores.class0 = lda_probs, weights.class0 = actual_labels, curve = TRUE),
  "Logistic Regression" = pr.curve(scores.class0 = lr_probs, weights.class0 = actual_labels, curve = TRUE),
  "Decision Tree" = pr.curve(scores.class0 = dt_probs, weights.class0 = actual_labels, curve = TRUE)
)

# Create a combined PR Curve plot
plot(pr_curves[["Random Forest"]], col = "blue", main = "Precision-Recall Curves for All Models")
for (model in names(pr_curves)[-1]) {
  plot(pr_curves[[model]], add = TRUE, col = sample(colors(), 1))
}
legend("bottomright", legend = names(pr_curves), col = c("blue", "red", "green", "purple", "orange", "brown"), lwd = 2)

# Compute PR-AUC values
pr_auc_values <- data.frame(
  Model = names(pr_curves),
  PR_AUC = sapply(pr_curves, function(pr) pr$auc.integral)
)

print(pr_auc_values)


