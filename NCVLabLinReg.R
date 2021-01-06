sp.load.packages <- function(){
  if (!require("lubridate")) install.packages("lubridate")
  if (!require("ggplot2")) install.packages("ggplot2")
  if (!require("corrplot")) install.packages("corrplot")
  if (!require("car")) install.packages("car")
  
}

sp.load.data <- function(path = getwd(),filename){
  setwd(path)
  df <- read.csv(filename)
  df <- df[,-c(17)]
  return(df)
}

sp.load.data.detail <- function(LinRegData){
  cat("No. of columns - ",ncol(LinRegData))
  cat("\nTop 6 rows - \n")
  print(head(LinRegData))
  cat("\nBottom 6 rows - \n")
  print(tail(LinRegData))
  cat("\nStructure of the data - \n")
  str(LinRegData)
  cat("\nData has been opened for view")
  View(LinRegData)
}

sp.date.format <- function(df,var){
  df[,var] <- as.Date(substr(df[,var],1,8), format='%Y%m%d')
  View(df)
  str(df)
  return(df)
}

sp.explore.dv <- function(df,var){
  options(scipen = 20)
  options(par = c(1,2))
  plot(df[,var])
  readline(prompt="Press [enter] to continue")
  hist(df[,var])
  readline(prompt="Press [enter] to continue")
  hist(log10(df[,var]))
}

sp.check.lower.outliers <- function(df,var){
  options(scipen = 20)
  options(par = c(1,2))

  x_data <- c(.001, .005, .01, .05, 0.1, 0.2, 0.3)
  y_data <- round(quantile(df[,var], c(.001, .005, .01, .05, 0.1, 0.2, 0.3)),0)
  qplot(x = x_data, y = y_data,label = round(y_data/1000000,1), geom=c("text","point"), hjust=-0.25)
  
}

sp.check.upper.outliers <- function(df,var){
  options(scipen = 20)
  options(par = c(1,2))

  x_data <- c(.90, .95, .96, .97, .98, .99, 1)
  y_data <- round(quantile(df[,var], c(.90, .95, .96, .97, .98, .99, 1)),0)
  qplot(x = x_data, y = y_data,label = round(y_data/1000000,1), geom=c("text","point"), hjust=-0.25)
  
}



sp.reduce.df <- function(df,var,lo.amt,hi.amt){
  temp1 <- df[,var]<=hi.amt
  temp2 <- df[,var]>=lo.amt
  temp <- temp1 & temp2
  df2 <- df[temp,]
  return(df2)
}

sp.log <- function(df,var){
  df[,var] <- log10(df[,var])
  return(df)
}

sp.explore.dates <- function(df){
  y <- year(df$date)
  m <- month(df$date)
  d <- ifelse(m<10,paste(y,m,sep="|0"),paste(y,m,sep="|"))  
  plot(table(d))
}

sp.explore.discrete <- function(df,var){
  plot(table(df[,var]))
}

sp.explore.continous <- function(df,var){
  plot(df[,var])
  readline(prompt="Press [enter] to continue")
  hist(df[,var])
  readline(prompt="Press [enter] to continue")
  hist(log10(df[,var]))
}

sp.correlation.matrix <- function(df){
  cor.mat <- cor(df,df)
  corrplot(cor.mat, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
}

sp.vif <- function(df,dv){
  f <- paste(dv,"~.")
  print(f)
  #alias((as.formula(f)))
  vif_data <- lm(as.formula(f),data=df)
  sort(vif(vif_data))
}

sp.remove.var <- function(df,var){
  df2 <- df[,-which(names(df) %in% c(var))]
  return(df2)
}

sp.get.training.data <- function(df,x){
  set.seed(3)
  test <-   sample(1:nrow(df),nrow(df)*(1-x))
  train <-  -test
  training_data <-  df[train,]
  testing_data <- df[test,]  
  return(training_data)
}

sp.get.testing.data <- function(df,x){
  set.seed(3)
  test <-   sample(1:nrow(df),nrow(df)*x)
  train <-  -test
  training_data <-  df[train,]
  testing_data <- df[test,]  
  return(testing_data)
}

model.residual.metrics <- function(tr,ts,mo,dv){
  pr.tr <- predict(mo,newdata=tr)
  pr.ts <- predict(mo,newdata=ts)
  ac.tr <- tr[,dv]
  ac.ts <- ts[,dv]
  pr.res.tr <- pr.tr - ac.tr
  pr.res.ts <- pr.ts - ac.ts
  mape.tr <- mean(abs(pr.res.tr*100/ac.tr))
  mape.ts <- mean(abs(pr.res.ts*100/ac.ts))
  rmse.tr <- sqrt(mean(pr.res.tr^2))
  rmse.ts <- sqrt(mean(pr.res.ts^2))
  cat("MAPE Training Data - ",mape.tr,"\nMAPE Testing Data - ", mape.ts)
  cat("\nRMSE Training Data - ",rmse.tr,"\nRMSE Testing Data - ", rmse.ts)
}

model.residual.metrics.unlog <- function(tr,ts,mo,dv){
  pr.tr <- exp(predict(mo,newdata=tr))
  pr.ts <- exp(predict(mo,newdata=ts))
  ac.tr <- exp(tr[,dv])
  ac.ts <- exp(ts[,dv])
  pr.res.tr <- pr.tr - ac.tr
  pr.res.ts <- pr.ts - ac.ts
  mape.tr <- mean(abs(pr.res.tr*100/ac.tr))
  mape.ts <- mean(abs(pr.res.ts*100/ac.ts))
  rmse.tr <- sqrt(mean(pr.res.tr^2))
  rmse.ts <- sqrt(mean(pr.res.ts^2))
  cat("MAPE Training Data - ",mape.tr,"\nMAPE Testing Data - ", mape.ts)
  cat("\nRMSE Training Data - ",rmse.tr,"\nRMSE Testing Data - ", rmse.ts)
}


model.assumption.testing <- function(tr,ts,mo,dv){
  pr.tr <- predict(mo,newdata=tr)
  pr.ts <- predict(mo,newdata=ts)
  ac.tr <- tr[,dv]
  ac.ts <- ts[,dv]
  pr.res.tr <- pr.tr - ac.tr
  pr.res.ts <- pr.ts - ac.ts
  
  cat("Mean of residual in training data - ",mean(pr.res.tr),"\nMean of residual in testing data - ", mean(pr.res.ts))
  readline(prompt="Press [enter] to continue to histogram of training residual")
  hist(pr.res.tr)
  readline(prompt="Press [enter] to continue to histogram of testing residual")
  hist(pr.res.ts)
  readline(prompt="Press [enter] to continue to compare residual with predicted values on training data")
  plot(pr.res.tr,pr.tr)
  readline(prompt="Press [enter] to continue to compare residual with predicted values on testing data")
  plot(pr.res.ts,pr.ts)
}