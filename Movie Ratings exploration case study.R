
getwd()

setwd("/Users/vatsalmandalia")
 
movies <- read.csv("Movie Ratings.csv")

summary(movies)
str(movies)

nrow(movies)
ncol(movies)
a <- 2
a.a <- 2

head(movies,10)  # top 6 rows
tail(movies)  # last 6 rows

colnames(movies)

colnames(movies)<-c('name','genre','criticsratings','audienceratings','budgetmillions','releaseyear')

levels(movies$genre)

levels(movies$releaseyear)

# changing the numeric column to categorical
movies$releaseyear<-as.factor(movies$releaseyear)
levels(movies$releaseyear)


subdf <- movies[movies$genre=="Horror",]


# install ggplot2 package
library(ggplot2)
ggplot(data=movies[movies$genre=="Horror",],aes(x=criticsratings,y=audienceratings)) + geom_point()

#adding a new column of rating difference
movies$diff_rating <- (movies$criticsratings - movies$audienceratings)

movies$diff_rating <- ifelse(movies$diff_rating>0,1,0)

# color and size setting 
#ggplot(data=movies,aes(x=criticsratings,y=audienceratings,color=genre,size=budgetmillions))
r<-ggplot(data=movies,aes(x=criticsratings,y=audienceratings))+geom_point(color='red',size=3)
r

r<-ggplot(data=movies[movies$genre=="Horror",],aes(x=criticsratings,y=audienceratings))+geom_point(color='red',size=3)
r

r<-ggplot(data=movies[movies$genre=="Adventure",],aes(x=criticsratings,y=audienceratings))+geom_point(color='red',size=3)
r
movies[movies$audienceratings==0,]

#Color and Size mapping
r+geom_point(aes(color=genre))
r+geom_point(aes(size=budgetmillions))

# changing the graph type
ggplot(data=movies,aes(x=budgetmillions))+
  geom_histogram(binwidth=100,color='blue',fill='green')



# genre mappig
ggplot(data=movies,aes(x=budgetmillions))+
  geom_histogram(binwidth=50,color='black',aes(fill=genre))

summary(movies)

# Adding new layer for limiting the yscale
ggplot(data=movies,aes(x=budgetmillions))+
  geom_histogram(binwidth=50,color='black',aes(fill=genre))+
  coord_cartesian(ylim=c(0,50))

 

#Using facet grid to divide the graph according to genre
ggplot(data=movies,aes(x=criticsratings,y=audienceratings))+geom_point(color='red',size=3)+
  facet_grid(.~genre)

#Area plot
d <- ggplot(data=movies,aes(x=budgetmillions))+
  geom_density(color='black',stack=10,aes(fill=genre))
d

# histograms with facet grid
ggplot(data=movies,aes(x=budgetmillions))+
  geom_histogram(binwidth=10,color='black',aes(fill=genre))+
  facet_grid(genre~.,scales='free')

plot(criticsratings~audienceratings, data=movies)
colnames(movies)