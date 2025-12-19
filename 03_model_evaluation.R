
#------RF------------

# Evaluate the model performance
set.seed(123)
conf_matrix_rf_cv <- confusionMatrix(rf_pred_cv, test_data$Group)
print(conf_matrix_rf_cv)
# Extract confusion matrix elements 
confusion_matrix_rf <- as.data.frame(conf_matrix_rf_cv$table)
confusion_matrix_rf



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



#-----svm-------------
# Evaluate the model performance
set.seed(123)
conf_matrix_svm_cv <- confusionMatrix(svm_pred_cv, test_data$T2D)
print(conf_matrix_svm_cv)
confusion_matrix_svm <- as.data.frame(conf_matrix_svm_cv$table)
confusion_matrix_svm


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


# -------------------LDA----------------------
# Evaluate the model performance

set.seed(123)
conf_matrix_lda_cv <- confusionMatrix(lda_pred_cv$class, test_data$T2D)
print(conf_matrix_lda_cv)

confusion_matrix_lda <- as.data.frame(conf_matrix_lda_cv$table)
confusion_matrix_lda



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




#-------DT-------------------


# 
conf_matrix_dt_cv <- confusionMatrix(predict_model_dt_cv, test_data$T2D)
print(conf_matrix_dt_cv)

confusion_matrix_dt <- as.data.frame(conf_matrix_dt_cv$table)
confusion_matrix_dt



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







