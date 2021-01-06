sp.load.packages <- function(){
  if (!require("lubridate")) install.packages("lubridate")
  if (!require("ggplot2")) install.packages("ggplot2")
  if (!require("corrplot")) install.packages("corrplot")
  if (!require("car")) install.packages("car")
  
}

sp.load.data <- function(path,filename){
  setwd(path)
  df <- read.csv(filename, stringsAsFactors = FALSE)
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
  cat("\nSummary of the data - \n")
  summary(LinRegData)
  cat("\nData has been opened for view")
  View(LinRegData)
}

sp.remove.missing <- function(df){
  df2 <- na.omit(df)
  return(df2)
}

sp.explore.dv <- function(df,var){
  plot(table(df[,var]))
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

uniAnalysisCateg<-function(df, var)
{
  Freq_tbl<-data.frame(table(df[,var]))
  Freq_tbl$var_name <-var
  colnames(Freq_tbl)<-c("values","freq","variable")
  return(Freq_tbl)
}

sp.univariate.analysis.categ <- function(df,cols){
  uniDataCateg <- data.frame(values = character(),freq = numeric(),variable = character())
  for(i in 1 : length(cols)){
    uniDataCateg<-rbind(uniDataCateg,uniAnalysisCateg(df,cols[i]))
  }
  
  tot_custs<-nrow(df)
  uniDataCateg$perc<-round(100*uniDataCateg$freq/tot_custs,0)
  ggplot(uniDataCateg, aes(x = values, y = perc)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")
  
}

uniAnalysisCont<-function(data,var)
{
  Pctl_tbl<-as.vector(quantile(data[,var], probs=c(.01, .10, .20, .50, .80, .90, .99, 1.0)))
  Pctl_tbl<-data.frame(c("P001","P010","P020","P050","P080","P090","P099","P100"),Pctl_tbl)
  Pctl_tbl<-data.frame(c(var,var,var,var),Pctl_tbl)
  colnames(Pctl_tbl)<-c("variable","quantiles","values")
  return(Pctl_tbl)
}

sp.univariate.analysis.cont <- function(df,cont_columns){
  uniDataCont <- data.frame(variable = character(), quantiles = character(), values = numeric())
  for(i in 1 : length(cont_columns)){
    uniDataCont<-rbind(uniDataCont,uniAnalysisCont(df,cont_columns[i]))
  }
  ggplot(uniDataCont, aes(x = quantiles, y = values)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")  
}

sp.remove.var <- function(df,var){
  df2 <- df[,-which(names(df) %in% c(var))]
  return(df2)
}

Concordance = function(GLM.binomial) {
  outcome_and_fitted_col = cbind(GLM.binomial$y, GLM.binomial$fitted.values)
  # get a subset of outcomes where the event actually happened
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  # get a subset of outcomes where the event didn't actually happen
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  # Equate the length of the event and non-event tables
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  else {zeros = zeros[1:length(ones[,1]),]}
  # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
  ones_and_zeros = data.frame(ones, zeros)
  # initiate columns to store concordant, discordant, and tie pair evaluations
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  for (i in 1:length(ones_and_zeros[,1])) {
    # This tests for concordance
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
    {conc[i] = 1
    disc[i] = 0
    ties[i] = 0}
    # This tests for a tie
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
    }
    # This should catch discordant pairs.
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0
    }
  }
  # Here we save the various rates
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  return(list(concordance=conc_rate))
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
