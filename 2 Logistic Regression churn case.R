#STEP 0. LOAD NO CODE VIRTUAL LAB
source('/Users/vatsalmandalia/DataScienceBootcamp/NCVLabLogReg.R')
setwd('/Users/vatsalmandalia/DataScienceBootcamp')

#STEP 1. LOAD ALL REQUIRED PACKAGES
sp.load.packages()

#STEP 2. LOAD DATA
df <- sp.load.data("/Users/vatsalmandalia/DataScienceBootcamp","telecom_customer_churn.csv")
sp.load.data.detail(df)
summary(df)


for (i in colnames(df)){ 
  
  print (i)
  print(sum(is.na(df[,i])))
  }


#STEP 3. REMOVE MISSING VALUES
df <- sp.remove.missing(df)

#STEP 4. DEFINE VARIABLE TYPE
categ_columns <- c("gender","SeniorCitizen", "Partner", "Dependents", "PhoneService" ,
                   "MultipleLines" , "InternetService" ,"OnlineSecurity","OnlineBackup",
                   "DeviceProtection", "TechSupport","StreamingTV" ,"StreamingMovies" ,
                   "Contract","PaperlessBilling","Churn" )

cont_columns <- c("tenure","MonthlyCharges","TotalCharges" )

#STEP 5. PERFORM UNIVARIATE ANALYSIS
#STEP 5.1. CATEGORICAL VARIABLES
sp.univariate.analysis.categ(df,categ_columns)

#STEP 5.2. CONTINOUS VARIABLES
sp.univariate.analysis.cont(df,cont_columns)

#STEP 6. DUMMY VARIABLE CREATION
df$D_Gender_M <- ifelse(df$gender=="Male" ,1,0)
df$D_Partner <- ifelse(df$Partner=="No" ,1,0)
df$D_Dependents_No <- ifelse(df$Dependents=="No" ,1,0)
df$D_MultipleLines_No <- ifelse(df$MultipleLines=="No" ,1,0)
df$D_MultipleLines_Yes <- ifelse(df$MultipleLines=="Yes" ,1,0)
df$D_InternetService_FO <- ifelse(df$InternetService=="Fiber optic" ,1,0)
df$D_InternetService_No <- ifelse(df$InternetService=="No" ,1,0)
df$D_OnlineSecurity <- ifelse(df$OnlineSecurity=="No" ,1,0)
df$D_OnlineBackup_No <- ifelse(df$OnlineBackup=="No" ,1,0)
df$D_DeviceProtection_No <- ifelse(df$DeviceProtection=="No" ,1,0)
df$D_TechSupport_No <- ifelse(df$TechSupport=="No" ,1,0)
df$D_StreamingMovies_No <- ifelse(df$StreamingMovies=="No" ,1,0)
df$D_StreamingTV_No <- ifelse(df$StreamingTV=="No" ,1,0)
df$D_Contract_monthly <- ifelse(df$Contract=="Month-to-month" ,1,0)
df$D_Contract_biannual <- ifelse(df$Contract=="Two year" ,1,0)
df$D_PaperlessBilling <- ifelse(df$PaperlessBilling=="Yes" ,1,0)


#SELECT THE REQUIRED VARS
colnames(df)
df2 <- df[,-c(1,3:4,6:16)]
colnames(df2)

#STEP7 PCA
#pcaData <- df2[, -c(5)]
#fit <- princomp(pcaData, cor=TRUE)
#summary(fit)
#loadings(fit)
#plot(fit,type="lines") #scree polt

#STEP8 VIF
fit <- lm(Churn ~., data= df2)
sort(vif(fit)) 

df2 <- sp.remove.var(df2,"MonthlyCharges")
fit <- lm(Churn ~., data= df2)
sort(vif(fit))

df2 <- sp.remove.var(df2,"TD_InternetService_FO")
fit <- lm(Churn ~., data= df2)
sort(vif(fit))

df2 <- sp.remove.var(df2,"D_MultipleLines_No")
fit <- lm(Churn ~., data= df2)
sort(vif(fit))


#Training and Test df
df.train <- sp.get.training.data(df2,0.8)
df.test <- sp.get.testing.data(df2,0.2)

#Logistic Regression on full df
model <- glm(Churn ~ ., data = df.train, family = binomial())
summary(model)

#RandomForest
install.packages("randomForest")
library(randomForest)
model1 <- randomForest(Churn ~ ., data = df.train, mtry = 5, importance = TRUE)
model1

model <- glm(Churn ~ . -D_DeviceProtection_No - D_Gender_M - D_Partner - D_OnlineBackup_No - D_Dependents_No, data = df.train, family = binomial())
summary(model)

model <- glm(Churn ~ . -D_Dependents_No -D_InternetService_No -D_Gender_M-D_Partner-D_DeviceProtection_No, data = df.train, family = binomial())
summary(model)

# concordance
Concordance(model)


# get prob scores
df.train$Score <- predict(model,df.train,type="response")


df.test$Score <- predict(model,df.test,type="response")
df.test$Score2 <- predict(model1,df.test,type="response")
 
#To run on production
#df.test$Score <- predict(model,newdataframe,type="response")


#15 Measuring Accuracy of Model Predictions
#There are many ways to measure how well a statistical model predicts a binary outcome.  
#Three very common measures are  accuracy, sensitivity, and specificity.
# first let's understand the 4 possible outcomes of a model prediction - 
# True Negative (TN) - Actual is 0 and model also predicted 0
# True Positive (TP) - Actual is 1 and model also predicted 1
# False Positive (FP) - Actual is 0 and model predicted 1
# False Negative (FN) - Actual is 1 and model predicted 0
# When we put the above 4 possibilities in a matrix it is also called confusion matrix
# Sensitivity - % of 1's that we managed to capture i.e. TP / (TP+FN)
# Specificity - % of 0's that we managed to capture i.e. TN / (TN+FP) 
# Accuracy - % of correct classification i.e. (TP+TN)/(TP+TN+FP+FN)
# Now higher the all the 3 the better it is.
# Now everything depends on the threshold we use in our prob score to define 1 or 0. 
# But there is always a trade-off. 
# If the threshold is low it will increase sensitivity but decrease specificity and vice-versa
# It's important to understand this as you're choosing that threshold and evaluating a model.
# We should select a threshold that gives the best accuracy
# Best threshold can be obtained by using coords() function


# ROC curve
# ROC Curves are used to see how well your classifier can separate 
# positive and negative examples and to identify the best threshold for separating them.
# ROC curve is obtained by plotting sensitivity against 1-Specificity for many possible thresholds.
# Sensitivity is also called True Positive Rate. 1-Specificity is FP/(TN+FP) also called False Positive Rate i.e. how many i am wrongly classifying as 1 out of total possible wrongly classifiying 1
# So you want higher Sensitivity (or True Positive Rate) & Lower False Positive Rate (or 1-Specificity)
# Area under the ROC curve determines how good the model is. Higher the area the better it is.
install.packages("pROC")
library(pROC)
rocCurve   <- roc(response = df.train$Churn, predictor = df.train$Score, levels = c(0,1))
# Plot ROC curve
plot(rocCurve)
# Area under the ROC curve. Higher the better.
auc(rocCurve)

# Best threshold
coords(rocCurve,"best")
 
# Confusion metric
#predclass <-ifelse(df.train$Score>coords(rocCurve,"best")[1],1,0)
predclass <-ifelse(df.test$Score>0.5,1,0)
Confusion <- table(Predicted = predclass,Actual = df.test$Churn)
Confusion

predclass2 <-ifelse(df.test$Score2>0.5,1,0)
Confusion <- table(Predicted = predclass2,Actual = df.test$Churn)
Confusion

# Accuracy. Higher the better.
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
AccuracyRate
install.packages("ROCR")
library(ROCR)
pred <- prediction(predclass, df.train$Churn)

gain <- performance(pred, "tpr", "rpp")
plot(gain, main = "Gain Chart")





#CHECKING ON TEST DATA
rocCurve   <- roc(response = df.test$Churn, predictor = df.test$Score, levels = c(0,1))
# Plot ROC curve
plot(rocCurve)

# Best threshold
coords(rocCurve,"best")

# Confusion metric
#predclass <-ifelse(df.test$Score>coords(rocCurve,"best")[1],1,0)
predclass <-ifelse(df.test$Score>0.5,1,0)
Confusion <- table(Predicted = predclass,Actual = df.test$Churn)
Confusion

# Accuracy. Higher the better.
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
AccuracyRate






