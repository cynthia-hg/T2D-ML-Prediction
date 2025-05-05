
# Data preprocessing and Spliting into Train and test data


# Load required libraries
library(rpart)
library(readr)
library(caTools)
library(dplyr)
library(party)
library(partykit)
library(tidyverse)
library(rpart.plot)
library(party)
library(vip)
library(data.table)
library(dplyr)
library(tree)
library(randomForest)
library(caret)
library(pROC)
library(data.table)
library(glmnet)
library(caret)
library(data.table)
library(e1071)
library(gbm)
library(caret)
library(MLmetrics)
library(caret)
library(ggplot2)
library(scales)
library(ggpubr)
library(dplyr)
library(verification)
library(PRROC)


#Clinical and genetic features included in the dataset:
#Age, Gender, Weight, BMI, Height, Triglyceride, Total.Cholesterol, HDL, LDL, CVD, T2D, Hyperlipidemia, Hypertension, Fx.Hypertension, Fx.T2D, Fx.Hyperlipidemia, Smoking., Fx.CVD, 
#rs12137794, rs587404, rs1260326, rs2943641, rs2943645, rs2972143, rs2877716, rs10804976, rs7756992, rs3130501, rs3132946, rs2395163, rs2395182, rs6903608, rs2395185, rs9275595, rs987237, 
#rs9388489, rs4273712, rs2246012, rs10244051, rs4607517, rs13266634, rs4977756, rs10965243, rs11257655, rs10906115, rs6583826, rs5015480, rs7923837, rs7903146, rs5215, rs11555762, rs1387153, 
#rs2166706, rs8756, rs3764002, rs10146997, rs4886707, rs7177055, rs8042680, rs8050136, rs7202877, rs13342692, rs12970134, rs2075650

# Set working directory and read dataset
setwd("") 
merged<-fread("") #read the csv file
view(merged)
#attach(merged)


# Convert categorical variables to factors
merged2 <- merged %>%
  mutate(
    T2D = ifelse(T2D == 0, "No", "Yes"),
    Hypertension = ifelse(Hypertension == 0, "No", "Yes"),
    Hyperlipidemia = ifelse(Hyperlipidemia == 0, "No", "Yes"),
    Gender = ifelse(Gender == 1, "Male", "Female"), 
    Fx.Hyperlipidemia = ifelse(Fx.Hyperlipidemia == 0, "No", "Yes"),
    Fx.Hypertension = ifelse(Fx.Hypertension == 0, "No", "Yes"),
    Fx.T2D = ifelse(Fx.T2D == 0, "No", "Yes"),
    Smoking. = ifelse(Smoking. == 0, "No", "Yes"),
    Fx.CVD = ifelse(Fx.CVD == 0, "No", "Yes"),
    CVD = ifelse(CVD == 0, "No", "Yes")
      )

print(colnames(merged))

# Drop unnecessary columns and inspect the data
view(merged)
print(colnames(merged))
merged2<- merged2 %>% select(-c( ID.sample, FID, IID))
view(merged2)
print(colnames(merged2))


#remove rows with empty values
# Check for missing values
any_missing <- any(is.na(merged2))  #this one only covers T2D column
any_missing 
if (any_missing) {
  merged3 <- na.omit(merged2)  #or we can remove rows with empty values
}
#merged2
nrow(merged3)
table(merged3$T2D)

# Check for missing values
merged4 <- merged3 %>%
  filter_all(all_vars(. != ""))  #this one covers the SNPs columns 

# Check number of rows and T2D distribution
nrow(merged4)
table(merged4$T2D)


str(merged4)

#convert into factors  
merged4$T2D <- as.factor(merged4$T2D)
merged4$Gender <- as.factor(merged4$Gender)
merged4$CVD <- as.factor(merged4$CVD)
merged4$Hyperlipidemia <- as.factor(merged4$Hyperlipidemia)
merged4$Hypertension <- as.factor(merged4$Hypertension)
merged4$Fx.Hypertension <- as.factor(merged4$Fx.Hypertension)
merged4$Fx.Hyperlipidemia <- as.factor(merged4$Fx.Hyperlipidemia)
merged4$Fx.T2D <- as.factor(merged4$Fx.T2D)
merged4$Smoking. <- as.factor(merged4$Smoking.)
merged4$Fx.CVD <- as.factor(merged4$Fx.CVD)

merged4$Age <- as.numeric(merged4$Age)


# Ensure merged4 is a data.table
setDT(merged4)
# Get column indices of SNPs (starting with "rs")
snp_cols <- grep("^rs", colnames(merged4), value = TRUE)  # Get names, not indices
# Convert selected columns to factors
merged4[, (snp_cols) := lapply(.SD, as.factor), .SDcols = snp_cols]



#----data splitting into train and test data------

library(glmnet)
library(caret)
# Create a train/test split
set.seed(123)  # For reproducibility
split_index <- createDataPartition(merged4$T2D, p = 0.75, list = FALSE)
train_data <- merged4[split_index, ]
test_data <- merged4[-split_index, ]
dim(train_data)
dim(test_data)
table(train_data$T2D)
table(test_data$T2D)



write.csv(train_data, "train_data.csv", row.names = FALSE)
write.csv(test_data, "test_data.csv", row.names = FALSE)





# Calculate Class Weights (Inverse of Class Proportions)
class_counts <- table(train_data$T2D)
total_samples <- sum(class_counts)
class_weights <- total_samples / (length(class_counts) * class_counts)

# Assign Weights
weights_vector <- ifelse(train_data$T2D == "Yes", class_weights["Yes"], class_weights["No"])


