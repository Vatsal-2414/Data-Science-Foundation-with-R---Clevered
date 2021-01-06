#STEP 0. LOAD NO CODE VIRTUAL LAB
source('/Users/vatsalmandalia/DataScienceBootcamp/NCVLabLinReg.R')
setwd('/Users/vatsalmandalia/DataScienceBootcamp')

#STEP 1. LOAD ALL REQUIRED PACKAGES
sp.load.packages()

#STEP 2. LOAD DATA
df <- sp.load.data(,"kansas_city_house_sales.csv")
sp.load.data.detail(df)
summary(df)

#STEP 3. RECTIFY DATA
df <- sp.date.format(df,"date")

#STEP 4. EXPLORE DEPENDENT VARIABLE
sp.explore.dv(df,"price")
sp.check.lower.outliers(df,"price")
sp.check.upper.outliers(df,"price")
df <- sp.reduce.df(df,"price",150000,3000000)
df <- sp.log(df,"price")

#STEP 5. EXPLORE INDEPENDENT VARIABLES

#STEP 5.1. EXPLORE DATES
sp.explore.dates(df)

#STEP 5.2. EXPLORE DISCRETE VARIABLES
sp.explore.discrete(df,"bedrooms")
df[df$bedrooms==33,c(4)] <- 3 

sp.explore.discrete(df,"bathrooms")
sp.explore.discrete(df,"floors")

summary(df$waterfront)
sp.explore.discrete(df,"waterfront")
sp.explore.discrete(df,"view")
sp.explore.discrete(df,"condition")
sp.explore.discrete(df,"grade")

#STEP 5.2. EXPLORE CONTINOUS VARIABLES
sp.explore.continous(df,"sqft_living")
sp.check.lower.outliers(df,"sqft_living")
sp.check.upper.outliers(df,"sqft_living")
df <- sp.reduce.df(df,"sqft_living",700,7000)
df <- sp.log(df,"sqft_living")

sp.explore.continous(df,"sqft_lot")
sp.check.lower.outliers(df,"sqft_lot")
sp.check.upper.outliers(df,"sqft_lot")
df <- sp.reduce.df(df,"sqft_lot",1000,500000)
df <- sp.log(df,"sqft_lot")

sp.explore.continous(df,"sqft_above")
sp.check.lower.outliers(df,"sqft_above")
sp.check.upper.outliers(df,"sqft_above")
df <- sp.reduce.df(df,"sqft_above",700,5500)
df <- sp.log(df,"sqft_above")

sp.explore.continous(df,"sqft_basement")
sp.check.lower.outliers(df,"sqft_basement")
sp.check.upper.outliers(df,"sqft_basement")
df <- sp.reduce.df(df,"sqft_basement",0,2000)

sp.explore.continous(df,"yr_built")
sp.explore.continous(df,"yr_renovated")


#STEP 6. CREATE DERIVED VARIABLES
df$Sales_year <- year(df$date)
df$Sales_month <- month(df$date)
df$per_living <- (df$sqft_living*100)/df$sqft_lot
df$per_above <- (df$sqft_above*100)/df$sqft_lot
df$age <- df$Sales_year - df$yr_built
df$age_renovated <- ifelse(df$yr_renovated>0,df$Sales_year-df$yr_renovated,df$age)

#STEP 7. CREATE BACKUP
df_backup <- df

#STEP 8. DEFINE MODELING DATASET
colnames(df)
df <- df[,-c(1,2,14,15,16)]

#STEP 9. BI VARIATE ANALYSIS

#STEP 9.1. CORRELATION MATRIX
sp.correlation.matrix(df)
fit <- lm("price~.",data=df)


f <- paste("price","~.")
fit <- lm(as.formula(f),data=df)
summary(fit)


ld.vars <- attributes(alias(fit)$Complete)$dimnames[[1]]

#STEP 9.2. VARIANCE INFLATION FACTOR
sp.vif(df,"price")
df <- sp.remove.var(df,"per_above")
View(df)
sp.vif(df,"price")
df <- sp.remove.var(df,"sqft_living")
View(df)
sp.vif(df,"price")
df <- sp.remove.var(df,"per_living")

sp.vif(df,"price")
df <- sp.remove.var(df,"age_renovated")

sp.vif(df,"price")
df <- sp.remove.var(df,"sqft_above")
View(df)

#STEP 10. SPLIT DATA INTO TRAINING & TESTING
df.train <- sp.get.training.data(df,0.8)
df.test <- sp.get.testing.data(df,0.2)

#STEP 11. RUN MODEL ITERATIONS
model.output <- lm(price~., data = df.train)
summary(model.output)
length(y)

model.output <- lm(price~.-Sales_month, data = df.train)
summary(model.output)

#STEP 12. CHECK RESIDUAL METRICS
model.residual.metrics(df.train,df.test,model.output,"price")

#STEP 13. CHECK LINEAR REGRESSION MODEL ASSUMPTIONS
model.assumption.testing(df.train,df.test,model.output,"price")


#heteroskedasticity
lmtest::bptest(model.output)  # Breusch-Pagan test
