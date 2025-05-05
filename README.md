# T2D-ML-Prediction
Machine learning-based prediction of T2D using clinical and SNP data

This repository contains R scripts to train and evaluate machine learning models for predicting Type 2 Diabetes (T2D) using clinical and genetic (SNP) data.

## 📁 Repository Structure

- `01_preprocessing.R`  
  Loads the dataset, transforms clinical and genetic features, drops unnecessary columns, remove rows with missing data, split the data into train and test datasets and Assign Weights for train dataset.

- `02_model_training.R`  
  Trains multiple machine learning models (e.g., Random Forest, SVM) using the preprocessed dataset.

- `03_model_evaluation.R`  
  Evaluates the trained models using metrics such as AUC, ROC curves, and confusion matrices.

## 📦 Required R Packages

Make sure the following R packages are installed:

```r
install.packages(c(
"MLmetrics",
"PRROC",
"caTools",
"caret",
"data.table",
"dplyr",
"e1071",
"gbm",
"ggplot2",
"ggpubr",
"glmnet",
"pROC",
"party",
"partykit",
"randomForest",
"readr",
"rpart",
"rpart.plot",
"scales",
"tidyverse",
"tree",
"verification",
"vip"
))
```



## 🧬 Features Used in the Model

**Clinical and genetic features included in the dataset:**
Age, Gender, Weight, BMI, Height, Triglyceride, Total.Cholesterol, HDL, LDL, CVD, T2D, Hyperlipidemia, Hypertension, Fx.Hypertension, Fx.T2D, Fx.Hyperlipidemia, Smoking., Fx.CVD, rs12137794, rs587404, rs1260326, rs2943641, rs2943645, rs2972143, rs2877716, rs10804976, rs7756992, rs3130501, rs3132946, rs2395163, rs2395182, rs6903608, rs2395185, rs9275595, rs987237, rs9388489, rs4273712, rs2246012, rs10244051, rs4607517, rs13266634, rs4977756, rs10965243, rs11257655, rs10906115, rs6583826, rs5015480, rs7923837, rs7903146, rs5215, rs11555762, rs1387153, rs2166706, rs8756, rs3764002, rs10146997, rs4886707, rs7177055, rs8042680, rs8050136, rs7202877, rs13342692, rs12970134, rs2075650


## 🚀 How to Use

1. Clone or download this repository.
2. read your file (make sure you have the same features).
3. Run the scripts in sequence:

```r
source("01_preprocessing.R")
source("02_model_training.R")
source("03_model_evaluation.R")
```

## 📌 Notes

- This project supports the manuscript submission to the *Computational and Structural Biotechnology Journal*.
- The dataset is not included in this repository but the feature names are listed above.

## 📄 License

MIT License

