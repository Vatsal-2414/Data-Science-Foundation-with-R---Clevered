#create a vector
a <- 2
vec_num <- c(1,2,3,4,5)
vec_char <- c("red","blue","green")
num <- c(1,20,40,40:45)
num
number <- 10:15
number
vec <- c(1,2,3,"abc")
vec
vec2 <- c(1,2,3,TRUE)
VEC2
vec2
vec3 <- c("abc",TRUE)
vec3



a <- c(1,3,5,7)
a+5
b <- c(1,2,4,8)
a+b
b <- c(1,2)
a+b
d <- c(1,2,4,6,8)
a+c


a*5
a8b
a*b


c(a,b)
a <- c(21,25,13,54,32)
a[3]

a[c(2,3,4)]
a[2:4]
a[-3]

a[6]

b <- c(1,2,4,6,8)

a[c(FALSE,TRUE,TRUE,FALSE,FALSE)]
length(a)

vec <- seq(10)
vec <- seq(1,10)
vec
vec <- seq(1,10,by=2)
vec
vec <- seq(1,10,by=0.5)
vec

vec <- c(5,20,25,30,1)
sort(vec)
sort(vec,decreasing=TRUE)

list_data <- list("abc",c(13,14,15),TRUE,13.14)
list_data
list_data[1]

list_data <- list(months=c("Jan","Feb","March"),num=matrix(c(1,3,5,6,7,6),nrow=2))
list_data$months
list_data$num

list_data[[1]][[3]]
list_data[[1]][3]
list_data$months[3]


data <- c("East","West","North","South")
factor_data <- factor(data)
factor_data
print(data)
print(factor_data)
factor_data[4]


#employee <- data.frame(emp_id = c(1:5), emp_name = c("Rick","Dan","Michelle","Ryan","Gary"), salary = c(623.30,515.20,611.0,729.00,843.25), start_data = c())

head(iris)
tail(iris,10)

mtcars
head(mtcars)

str(iris)

summary(iris)
nrow(iris)
ncol(iris)


iris[1:10,]
iris[3,4]
iris[3,]
iris$Sepal.Length
iris$Sepal.Length[1]


mtcars$newcol <- 10
mtcars
mtcars[,"newcol3"] <- "test"
mtcars

vec1 <- c(1,2,3)
vec2 <- c(4,5,6)
cbind(vec1,vec2)


mtcars$hp <- NULL
mtcars

x <- c("apples","oranges","bananas","berries")
for (i in x) {
  print(i)
}
for (j in 1:length(x)) {
  print(x[j])
}
}

count = 0
for (j in 1:length(x)) {
  if(x[j]=="bananas"){
    count = count+1
  }
}
print(count)


getwd()

pow <- function(x,y){
  result <- x^y
  print(paste(x,"raised to the power",y,"is",result))
  
  return(result)
}

a = pow(3,2)
print(a)

pow <- function(x,y=2){
  result <- x^y
  print(paste(x,"raised to the power",y,"is",result))
  
}
pow(8,3)

mtcars$newcol3 <- NULL
mtcars[order(-mtcars$mpg),]

head(mtcars)

summary(mtcars)

mtcar2 <- mtcars
mtcar2$gear <- as.factor(mtcar2$gear)
summary(mtcar2)

head(mtcars)
aggregate(hp~gear,data=mtcars,mean)
#to get the avg hp for each gear level of the cars

#correlation between the cyl and hp
aggregate(hp~cyl,data=mtcars,mean)


aggregate(hp~cyl+gear,data=mtcars,mean)

aggregate(.~cyl,data=mtcars,mean)
#for all the columns specified by the dot.



 



aggregate(cbind(hp,disp)~cyl,data=mtcars,mean) #getting means of more



#data visualisation
plot(cyl~gear, data=mtcar2)  #box and whisker plot
plot(y=mtcars$gear, x=mtcars$cyl)
plot(y=mtcars$wt, x=mtcars$hp)

hist(mtcars$hp)
hist(mtcars$wt)



