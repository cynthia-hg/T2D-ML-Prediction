
#----Random Forest 10-fold repeated 3 times  _ Accuracy-------------------

#Resampling: Cross-Validated (10 fold, repeated 3 times) 
control <- trainControl(method= "repeatedcv", number=10, repeats= 3,  savePredictions = T)
#Both 10-fold cross-validation and 3 repeats slows down the search process, but is intended to limit and reduce overfitting on the training set. 
set.seed(123)
rf_model <- train(T2D ~., data=train_data, method="rf", metric= "Accuracy",  trControl=control, weights = weights_vector)
print(rf_model)
plot(rf_model)
rf_model$results
# Use the mtry value that gave the highest accuracy during cross-validation
optimal_mtry <- rf_model$bestTune$mtry
optimal_mtry

#Accuracy was used to select the optimal model using the largest value.
# Train the final Random Forest model on the entire training dataset
set.seed(123)
final_rf_model <- randomForest(T2D ~ ., data = train_data, mtry = optimal_mtry)
final_rf_model

# Create a variable importance plot
set.seed(123)
var_importance_rf <- vip::vip(final_rf_model , num_features = 50)
print(var_importance_rf)
importance(final_rf_model)

# Make predictions on the test set
set.seed(123)
rf_pred_cv <- predict(final_rf_model, newdata = test_data)


#----Support Vector Machines SVM 10-fold repeated 3 times  _ Accuracy-------------------

set.seed(123)
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
control <- trainControl(method= "repeatedcv", number=10, repeats= 3,  savePredictions = T)
#Both 10-fold cross-validation and 3 repeats slows down the search process, but is intended to limit and reduce overfitting on the training set. 
set.seed(123)
svm_model <- train(T2D ~., data=train_data, method="svmLinear", metric= "Accuracy", trControl=control,weights = weights_vector, tuneLength = 10)
print(svm_model)
svm_model$results

# Use the c value that gave the highest accuracy during cross-validation
best_cost <- svm_model$bestTune$C
best_cost
set.seed(123)
final_svm_model <- svm(T2D ~ ., data = train_data, kernel="linear", cost = best_cost )  #, sigma = best_sigma
final_svm_model

# VIP plot
# r - how to find important factors in support vector machine - Stack Overflow
w <- t(final_svm_model$coefs) %*% final_svm_model$SV                 # weight vectors
w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight
w <- sort(w, decreasing = T)
print(w)

library(ggplot2)
vi_data <- data.frame(variable = names(w), importance = w)
vi_data
# Plot the variable importance
ggplot(vi_data, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# Subset the top 50 variables
top_50 <- head(vi_data[order(vi_data$importance, decreasing = TRUE), ], 50)

# Plot the top 50 variable importance
ggplot(top_50, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Make predictions on the test set
set.seed(123)
svm_pred_cv <- predict(final_svm_model, newdata = test_data)


#----Linear discriminant analysis LDA 10-fold repeated 3 times _  Accuracy-------------------

set.seed(123)
control <- trainControl(method= "repeatedcv", number=10, repeats= 3,  savePredictions = T)#Resampling: Cross-Validated (10 fold, repeated 3 times) 

lda_model <- train(T2D ~., data=train_data, method="lda", metric= "Accuracy",  trControl=control, weights = weights_vector, tuneLength = 10)
print(lda_model)
#plot(lda_model)
lda_model$results

library(klaR)
set.seed(123)
final_lda_model <- lda(T2D ~ ., data = train_data)
final_lda_model
plot(final_lda_model)

#final_lda_model$scaling
coef(final_lda_model)


imp_scores <- abs(coef(final_lda_model)[, "LD1"])
imp_scores
variable_names <- names(imp_scores)
variable_names
imp_names <- data.frame(Variable = variable_names, Importance = imp_scores)
imp_names <- imp_names[order(-imp_names$Importance), ]
print(imp_names)
attach(imp_names)

ggplot(imp_names, aes(x = reorder(Variable, Importance) , y = Importance)) +
  geom_bar(stat = "identity") +
  labs(x = "Variable", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# Subset the top 50 variables
top_50 <- head(imp_names, 50)
top_50
# Plot the top 50 variable importance
ggplot(top_50, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  labs(x = "Variable", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()



# Make predictions on the test set
set.seed(123)
lda_pred_cv <- predict(final_lda_model, newdata = test_data)



#----Logistic regression 10-fold repeated 3 times  _ Accuracy-------------------


set.seed(123)
control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        savePredictions = T
)

# train the glm_model on training set
glm_model <- train(T2D ~ .,
               data = train_data,
               trControl = control,
               method = "glm",
               family=binomial(),
               metric = 'Accuracy', 
               tuneLength = 10,
               weights = weights_vector
               )
# print cv scores
summary(glm_model)
print(glm_model)

# Print the best hyperparameter
print(glm_model$bestTune)
varImp(glm_model)
# Create a variable importance plot
set.seed(123)
var_importance_glm <- vip::vip(glm_model , num_features = 50)
print(var_importance_glm)

# Train final model without CV
final_glm_model <- glm(T2D ~ ., data = train_data, family = binomial())
# Feature Importance for final model
varImp(final_glm_model)

# Predict outcome using glm_model from training data based on testing data
set.seed(123)
glm_predictions <- predict(glm_model, newdata=test_data)
glm_predictions


#-------------- GBM Gradient Boosting  10-fold repeated 3 times  _ Accuracy-------------


set.seed(123)
control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        savePredictions = TRUE)

# Train the GBM model
gbm_model <- train(T2D ~ .,
                   data = train_data,
                   trControl = control,
                   method = "gbm",
                   metric = "Accuracy",
                   tuneLength = 10,
                   verbose = FALSE,
                   weights = weights_vector)  # verbose = FALSE   to prevent printing teh training

# Print the trained model
print(gbm_model)
#gbm_model$results
summary(gbm_model)

best_hyperparameters <- gbm_model$bestTune
best_hyperparameters
# Train the final GBM model with the best hyperparameters
set.seed(123)

#distribution = "bernoulli for binary
# Convert T2D to numeric (0 for "No", 1 for "Yes")
train_data$T2D <- ifelse(train_data$T2D == "Yes", 1, 0)
test_data$T2D <- ifelse(test_data$T2D == "Yes", 1, 0)

# Train the GBM model with bernoulli
final_gbm_model <- gbm(T2D ~ ., 
                       data = train_data, 
                       n.trees = best_hyperparameters$n.trees,
                       interaction.depth = best_hyperparameters$interaction.depth,
                       shrinkage = best_hyperparameters$shrinkage,
                       n.minobsinnode = best_hyperparameters$n.minobsinnode,
                       distribution = "bernoulli")  

#final_gbm_model_cal
summary(final_gbm_model)
final_gbm_model

set.seed(123)
vip::vip(final_gbm_model , num_features = 50)

# Make predictions on the test set
set.seed(123)
gbm_pred_cv <- predict(final_gbm_model, n.trees = final_gbm_model$n.trees, newdata = test_data, type = "response")

# Convert T2D to factors
train_data$T2D <- ifelse(train_data$T2D == "1", "Yes", "No")
test_data$T2D <- ifelse(test_data$T2D == "1", "Yes", "No")


#--------------# decision tree  10-fold repeated 3 times  _ Accuracy-------------------

ctrl <- trainControl(method = "repeatedcv", number=10, repeats= 3,  savePredictions = T)

dt_model_cv <- train(T2D ~ .,
               train_data,
               method = 'rpart',
               tuneLength = 20,
               metric= "Accuracy",
               trControl = ctrl, weights = weights_vector)
dt_model_cv


# Use the cp value that gave the highest accuracy during cross-validation
optimal_cp <- dt_model_cv$bestTune$cp
optimal_cp
#Accuracy was used to select the optimal model using the largest value.
control <- rpart.control( cp = optimal_cp)
set.seed(123)
final_dt_model_cv <- rpart(T2D ~ ., data = train_data, method = 'class', control = control)
# Visualize the final decision tree
prp(final_dt_model_cv, type = 4, extra = 1)
rpart.plot(final_dt_model_cv, type = 4)
rpart.rules(final_dt_model_cv)

# Create a variable importance plot
set.seed(123)
var_importance_dt <- vip::vip(final_dt_model_cv , num_features = 50)
print(var_importance_dt)

# Make predictions on the test set
set.seed(123)
predict_model_dt_cv <- predict(final_dt_model_cv, newdata = test_data, type = "class")
#predict_model_dt_cv
