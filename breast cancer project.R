# Libraries
library(tidyverse)
# install.packages("Hmisc")
library(Hmisc)
# install.packages("corrplot")
library(corrplot)
library(caret)
library(MASS)
library(randomForest)
library(rpart)
library(nnet)
library(klaR)
library(PRP)
library(tree)
library(readr)
library(caTools)
library(dplyr)
library(party)
library(partykit)
library(rpart.plot)
library(e1071)
library(ggplot2)
library(ggpubr)
library(broom)
library(AICcmodavg)
library(datarium)
# library(rpart.plot)

# Import Dataset
biomarkers <- read.csv(file = "C://Users//pgand//OneDrive//Documents//HCA 743 Predictive Analytics in Healthcare//dataR1.csv")

head(biomarkers, n = 5)
tail(biomarkers, n = 5)
dim(biomarkers)
str(biomarkers)
levels(biomarkers$Classification)
summary(biomarkers)


# check for missing values
missingcols <- sapply(biomarkers, function(x) {
  any(is.na(x))
})  # no missing values

sum(missingcols)

# Correlation Matrix without P-Values
biomarkers.cor = cor(biomarkers, method = c("spearman"))
biomarkers.cor

# Correlation Matrix with P-Values
biomarkers.rcorr = rcorr(as.matrix(biomarkers))
biomarkers.rcorr

# Correlation Matrix with P-Values
biomarkers.coeff = biomarkers.rcorr$r
biomarkers.p = biomarkers.rcorr$P
biomarkers.p

biomarkers.p = biomarkers.rcorr$P
biomarkers.p



# Visualizing the correlation matrix
corrplot(biomarkers.cor)
# Exploratory Data Analysis
# Visualizing distribution


# ANOVA
model <- aov(Classification ~ Glucose + Insulin + HOMA + Resistin, data = biomarkers)
summary(model)


# Normality Glucose
res_aov_glucose <- aov(Glucose ~ Classification,
                       data = biomarkers)
par(mfrow = c(1, 2)) # combine plots

# Histogram Glucose
hist(res_aov_glucose$residuals)

# QQ-plot Glucose
#qqPlot(res_aov_glucose$residuals,
       #id = FALSE)

# Shapiro-Wilk normality test_Glucose
shapiro.test(res_aov_glucose$residuals)


# Normality Insulin
res_aov_insulin <- aov(Insulin ~ Classification,
                       data = biomarkers)
par(mfrow = c(1, 2)) # combine plots

# Histogram Insulin
hist(res_aov_insulin$residuals)

# QQ-plot Insulin
# qqPlot(res_aov_insulin$residuals,
       # id = FALSE)

# Shapiro-Wilk normality test_Insulin
shapiro.test(res_aov_insulin$residuals)


# Normality HOMA
res_aov_homa <- aov(HOMA ~ Classification,
                    data = biomarkers)
par(mfrow = c(1, 2)) # combine plots

# Histogram HOMA
hist(res_aov_homa$residuals)

# QQ-plot HOMA
#qqPlot(res_aov_homa$residuals,
       #id = FALSE)

# Shapiro-Wilk normality test_HOMA
shapiro.test(res_aov_homa$residuals)


# Normality Resistin
res_aov_res <- aov(Resistin ~ Classification,
                   data = biomarkers)
par(mfrow = c(1, 2)) # combine plots

# Histogram Resistin
hist(res_aov_res$residuals)

# QQ-plot Resistin
#qqPlot(res_aov_res$residuals,
       #id = FALSE)

# Shapiro-Wilk normality test_Resistin
shapiro.test(res_aov_res$residuals)

# Do a post-hoc test
# tukey.model<-TukeyHSD(model)

# tukey.model

# Changing Classification from 1's and 2's to Healthy Controls and Patients, respectively
biomarkers["Classification"][biomarkers["Classification"] == 1] <- "Healthy_Controls"
biomarkers["Classification"][biomarkers["Classification"] == 2] <- "Patients"

# Split dataset into train, validate, and test data

splitSample <- sample(1:2, size = nrow(biomarkers), prob = c(0.7, 0.3), 
                      replace = T)

# training set
train_set <- biomarkers[splitSample == 1, ]
dim(train_set)

intrain <- sample(1:2, size = nrow(train_set), prob = c(0.7, 0.3), replace = T)

trainset1 <- train_set[intrain == 1, ]
dim(trainset1)

# validation set
validset <- train_set[intrain == 2, ]
dim(validset)


# Cross Validation 1st Round
#tcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "grid")
#set.seed(33612)
# Cross Validation 1st Round
set.seed(33612)
tcontrol <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"
# Model Construction
# Train Data

# Linear Discriminant Analysis
modelLDA2<- train(Classification ~ ., data = trainset1, method = "lda", metric = metric, trControl = tcontrol)
print.train(modelLDA2)
# summary(modelLDA)

# CART
modelCART2 <- train(Classification ~ ., data = trainset1, method = "rpart", metric = metric, trControl = tcontrol)
print.train(modelCART2)
# summary(modelNB)

# Decision Tree
modelDT2 <- train(Classification ~ ., data = trainset1, method = "rpart", metric = metric, trControl = tcontrol)
print.train(modelDT2)
# summary(modelDT)

rtree <- rpart(Classification ~ ., trainset1)
rpart.plot(rtree)

# Random Forest
modelRF2 <- train(Classification ~ ., data = trainset1, method = "rf", ntree = 100, importance = T, metric = metric, trControl = tcontrol)
print.train(modelRF2)
# summary(modelRF)


# Neural Network
modelNN2 <- train(Classification ~ ., data = trainset1, size=10, maxit=500, trace=FALSE, metric = metric, trControl = tcontrol)  # data is normalised using Preprocess
print.train(modelNN2)
#summary(modelNN)

# Support Vector Machine
modelSVM2 <- train(Classification~., data = trainset1, method="svmRadial", metric=metric, trControl = tcontrol)
print.train(modelSVM2)
#summary(modelSVM)

# Select Best Model
results2 <- resamples(list(lda=modelLDA2, cart=modelCART2, dt=modelDT2, rf=modelRF2, nn=modelNN2, svm=modelSVM2))
summary(results2)
dotplot(results2)



# Prediction on Validation Set
# Linear Discriminant Analysis
pLDA2 <- predict(modelLDA2, validset)

# CART
pCART2 <- predict(modelCART2, validset)

# Decision Tree
pDT2 <- predict(modelDT2, validset)

# Random Forest
pRF2 <- predict(modelRF2, validset)

# Neural Network
pNN2 <- predict(modelNN2, validset)

# Support Vector Machine
psvm2 <- predict(modelSVM2, validset)


# Confusion Matrix
# Linear Discriminant Analysis
cmLDA2 <- confusionMatrix(as.factor(validset$Classification), pLDA2)
cmLDA2

# CART
cmCART2 <- confusionMatrix(as.factor(validset$Classification), pCART2)
cmCART2

# Decision Tree
cmDT2 <- confusionMatrix(as.factor(validset$Classification), pDT2)
cmDT2

# Random Forest
cmRF2 <- confusionMatrix(as.factor(validset$Classification), pRF2)
cmRF2

# Neural Network
cmNN2 <- confusionMatrix(as.factor(validset$Classification), pNN2)
cmNN2

# Support Vector Machine
cmSVM2 <- confusionMatrix(as.factor(validset$Classification), psvm2)
cmSVM2



#MACHINE LEARNING MODELING ALL VARIABLES
breast_cancer <- read.csv(file = "C://Users//pgand//OneDrive//Documents//HCA 743 Predictive Analytics in Healthcare//dataR2.csv")

head(breast_cancer, n = 5)
tail(breast_cancer, n = 5)
dim(breast_cancer)
str(breast_cancer)
levels(breast_cancer$Classification)
summary(breast_cancer)

# check for missing values
missingcols <- sapply(biomarkers, function(x) {
  any(is.na(x))
})  # no missing values

sum(missingcols)

# Visualizing the correlation matrix


# Correlation Matrix without P-Values
breast_cancer.cor = cor(breast_cancer, method = c("spearman"))
breast_cancer.cor

# Correlation Matrix with P-Values
breast_cancer.rcorr = rcorr(as.matrix(breast_cancer))
breast_cancer.rcorr

# Correlation Matrix with P-Values
breast_cancer.coeff = breast_cancer.rcorr$r
breast_cancer.p = breast_cancer.rcorr$P
breast_cancer.p

corrplot(breast_cancer.cor)


# ANOVA
model <- aov(Classification ~ Age + BMI + Glucose + Insulin + HOMA + Leptin + Adiponectin + Resistin + MCP.1, data = breast_cancer)
summary(model)

# Interaction
interaction <- aov(Classification ~ Age*BMI*Glucose*Insulin*HOMA*Leptin*Adiponectin*Resistin*MCP.1, data = breast_cancer)
summary(interaction)

# Changing Classification from 1's and 2's to Healthy Controls and Patients, respectively
breast_cancer["Classification"][breast_cancer["Classification"] == 1] <- "Healthy_Controls"
breast_cancer["Classification"][breast_cancer["Classification"] == 2] <- "Patients"
#breast_cancer


# Exploratory Data Analysis
# Visualizing distribution
# Label Distribution
percentage <- prop.table(table(breast_cancer$Classification)) * 100
cbind(freq=table(breast_cancer$Classification), percentage=percentage)

# Categorical Variables
ggplot(data = breast_cancer) +
  geom_bar(mapping = aes(x = Classification))

# Continuous Variables
# Age
ggplot(data = breast_cancer) +
  geom_histogram(mapping = aes(x = Age), binwidth = .5)

ggplot(data = breast_cancer, mapping = aes(x = Age, colour = Classification)) +
  geom_freqpoly(binwidth = 1)

# BMI
ggplot(data = breast_cancer) +
  geom_histogram(mapping = aes(x = BMI), binwidth = .5)

ggplot(data = breast_cancer, mapping = aes(x = BMI, colour = Classification)) +
  geom_freqpoly(binwidth = 1)

# Glucose
ggplot(data = breast_cancer) +
  geom_histogram(mapping = aes(x = Glucose), binwidth = .5)

ggplot(data = breast_cancer, mapping = aes(x = Glucose, colour = Classification)) +
  geom_freqpoly(binwidth = 1)

# Insulin
ggplot(data = breast_cancer) +
  geom_histogram(mapping = aes(x = Insulin), binwidth = .5)

ggplot(data = breast_cancer, mapping = aes(x = Insulin, colour = Classification)) +
  geom_freqpoly(binwidth = 1)

# HOMA
ggplot(data = breast_cancer) +
  geom_histogram(mapping = aes(x = HOMA), binwidth = .5)

ggplot(data = breast_cancer, mapping = aes(x = HOMA, colour = Classification)) +
  geom_freqpoly(binwidth = .1)

# Leptin
ggplot(data = breast_cancer) +
  geom_histogram(mapping = aes(x = Leptin), binwidth = .5)

ggplot(data = breast_cancer, mapping = aes(x = Leptin, colour = Classification)) +
  geom_freqpoly(binwidth = 1)

# Adiponectin
ggplot(data = breast_cancer) +
  geom_histogram(mapping = aes(x = Adiponectin), binwidth = .5)

ggplot(data = breast_cancer, mapping = aes(x = Adiponectin, colour = Classification)) +
  geom_freqpoly(binwidth = .5)

# Resistin
ggplot(data = breast_cancer) +
  geom_histogram(mapping = aes(x = Resistin), binwidth = .5)

ggplot(data = breast_cancer, mapping = aes(x = Resistin, colour = Classification)) +
  geom_freqpoly(binwidth = 1)

# MCP.1
ggplot(data = breast_cancer) +
  geom_histogram(mapping = aes(x = MCP.1), binwidth = .5)

ggplot(data = breast_cancer, mapping = aes(x = MCP.1, colour = Classification)) +
  geom_freqpoly(binwidth = 1.5)

# Split dataset into train, validate, and test data
splitSample <- sample(1:2, size = nrow(breast_cancer), prob = c(0.7, 0.3), 
                      replace = T)

# training set
train_set <- breast_cancer[splitSample == 1, ]
dim(train_set)

intrain <- sample(1:2, size = nrow(train_set), prob = c(0.7, 0.3), replace = T)

trainset <- train_set[intrain == 1, ]
dim(trainset)

# validation set
validset <- train_set[intrain == 2, ]
dim(validset)

# test set
# testset <- breast_cancer[splitSample == 2, ]  # 40 observations
# dim(testset)

# Cross Validation 1st Round
set.seed(33612)
tcontrol <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"
# Model Construction

# Train Data

# Linear Discriminant Analysis
modelLDA<- train(Classification ~ ., data = trainset, method = "lda", metric = metric, trControl = tcontrol)
print.train(modelLDA)
# summary(modelLDA)

# CART
modelCART <- train(Classification ~ ., data = trainset, method = "rpart", metric = metric, trControl = tcontrol)
print.train(modelCART)
# summary(modelNB)

# Decision Tree
modelDT <- train(Classification ~ ., data = trainset, method = "rpart", metric = metric, trControl = tcontrol)
print.train(modelDT)
# summary(modelDT)

rtree <- rpart(Classification ~ ., trainset)
rpart.plot(rtree)

# Random Forest
modelRF <- train(Classification ~ ., data = trainset, method = "rf", ntree = 100, importance = T, metric = metric, trControl = tcontrol)
print.train(modelRF)
# summary(modelRF)


# Neural Network
modelNN <- train(Classification ~ ., data = trainset, size=10, maxit=500, trace=FALSE, metric = metric, trControl = tcontrol)  # data is normalised using Preprocess
print.train(modelNN)
#summary(modelNN)

# Support Vector Machine
modelSVM <- train(Classification~., data = trainset, method="svmRadial", metric=metric, trControl = tcontrol)
print.train(modelSVM)
#summary(modelSVM)

# Select Best Model
results <- resamples(list(lda=modelLDA, cart=modelCART, dt=modelDT, rf=modelRF, nn=modelNN, svm=modelSVM))
summary(results)
dotplot(results)



# Prediction on Validation Set
# Linear Discriminant Analysis
pLDA <- predict(modelLDA, validset)

# CART
pCART <- predict(modelCART, validset)

# Decision Tree
pDT <- predict(modelDT, validset)

# Random Forest
pRF <- predict(modelRF, validset)

# Neural Network
pNN <- predict(modelNN, validset)

# Support Vector Machine
psvm <- predict(modelSVM, validset)


# Confusion Matrix
# Linear Discriminant Analysis
cmLDA <- confusionMatrix(as.factor(validset$Classification), pLDA)
cmLDA

# CART
cmCART <- confusionMatrix(as.factor(validset$Classification), pCART)
cmCART

# Decision Tree
cmDT <- confusionMatrix(as.factor(validset$Classification), pDT)
cmDT

# Random Forest
cmRF <- confusionMatrix(as.factor(validset$Classification), pRF)
cmRF

# Neural Network
cmNN <- confusionMatrix(as.factor(validset$Classification), pNN)
cmNN

# Support Vector Machine
cmSVM <- confusionMatrix(as.factor(validset$Classification), psvm)
cmSVM


# ModelType <- c("Linear Discriminant Analysis", "CART","Decision Tree", "Random forest", "Neural Net", "Support Vector Machine")  # vector containing names of models

# Training classification accuracy
# TrainAccuracy <- c(max(modelLDA$results$Accuracy), max(modelCART$results$Accuracy), max(modelDT$results$Accuracy), 
                   # max(modelRF$results$Accuracy), max(modelNN$results$Accuracy), max(modelSVM$results$Accuracy))
# TrainAccuracy

# Training missclassification error
# Train_missclass_Error <- 1 - TrainAccuracy

# Train_missclass_Error


# validation classification accuracy
# ValidationAccuracy <- c(cmLDA$overall[1], cmCART$overall[1], cmDT$overall[1], cmRF$overall[1], 
                        # cmNN$overall[1], cmSVM$overall[1])
# ValidationAccuracy

# Validation misclassification error or out-of-sample-error
# Validation_missclass_Error <- 1 - ValidationAccuracy

# Validation_missclass_Error

# metrics <- data.frame(ModelType, TrainAccuracy, Train_missclass_Error, ValidationAccuracy, 
                      # Validation_missclass_Error)  # data frame with above metrics
# summary(metrics)
# dotplot(metrics)

# knitr::kable(metrics, digits = 5)  # print table using kable() from knitr package

# Predicting Test Values
# Logistic Regression
# pTestingLDA <- predict(modelLDA, testset)
# print(pTestingLDA)
# CART
# pTestingCART <- predict(modelCART, testset)
# pTestingCART
# summary(pTestingCART)

# Decision Tree
# pTestingDT <- predict(modelDT, testset)
# pTestingDT
# summary(pTestingDT)

# Random Forest
#pTestingRF <- predict(modelRF, testset)
# pTestingRF
# summary(pTestingRF)

# Neural Net
# pTestingNN<- predict(modelNN, testset)
# pTestingNN
# summary(pTestingNN)

# Support Vector Machine
# pTestingSVM <- predict(modelSVM, testset)
# pTestingSVM
# summary(pTestingSVM)
