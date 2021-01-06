# Load libraries
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(corrplot)) install.packages("corrplot")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(GGally)) install.packages("GGally")
if (!require(knitr)) install.packages("knitr")

# Load libraries
library(tidyverse)
library(corrplot)
library(gridExtra)
library(GGally)
library(knitr)

setwd('/Users/vatsalmandalia/DataScienceBootcamp')

cust_data<-read.csv("Insurance_Dataset_Clustering_Analysis.csv")

View(cust_data)
colnames(cust_data)
### Select the requried columns for clustering
cust_data<-cust_data[,c(2,4,5,7,9,10,13,14)]

# Histogram for each Attribute
cust_data %>%
  gather(Attributes, value, 1:7) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title=" Attributes - Histograms") +
  theme_bw()

# Boxplot for each Attribute  
cust_data %>%
  gather(Attributes, values, c(3:6)) %>%
  ggplot(aes(x=reorder(Attributes, values, FUN=median), y=values, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title=" Attributes - Boxplots") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  ylim(0, 5) +
  coord_flip()

#Check correlation between variables
corrplot(cor(cust_data,cust_data), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#removing age
cust_data <- cust_data[,-c(1)]

#Now normalize the data
cust_dataNorm <- as.data.frame(scale(cust_data))

# Execution of k-means with k=2
set.seed(1234)
cust_dataK2 <- kmeans(cust_dataNorm, centers=2)

#Find the centers
cust_dataK2$centers

# Cluster size
cust_dataK2$size

#How many clusters?
  
bss <- numeric()
wss <- numeric()

# Run the algorithm for different values of k 
set.seed(1234)

for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(cust_dataNorm, centers=i)$betweenss
  wss[i] <- kmeans(cust_dataNorm, centers=i)$tot.withinss
  
}

# Between-cluster sum of squares vs Choice of k
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Total within-cluster sum of squares vs Choice of k
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Subplot
grid.arrange(p3, p4, ncol=2)




#Final results
# Execution of k-means with k=3
set.seed(1234)

cust_dataK4 <- kmeans(cust_dataNorm, centers=4)


cust_data$Cluster <- cust_dataK4$cluster

#Export to excel
write.csv(cust_data,"cluster.csv")
# Visualizing the Clusters
ggpairs(cbind(cust_data, Cluster=as.factor(cust_dataK4$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()

# Mean values of each cluster
aggregate(cust_data, by=list(cust_dataK4$cluster), mean)
