library(ggplot2)
library(reshape2)
library(plyr)
library(ClustOfVar)
# Importing the data
aprildata <- read.csv("C:\\Users\\V.Babuvenkatesh\\Desktop\\Files\\06-19\\December 2016\\December2016.csv") 
plotdata <- aprildata
aprildata[is.na(aprildata)] <- 0
scaleddata <- cbind(aprildata["Property.Name"],scale(aprildata[,-1]))
#scaleddata<-aprildata
# Finding optimal number of clusters

optimal <- sapply(1:10,function(k) {kmeans(x = scaleddata[,-1],centers = k,nstart = 20,iter.max = 15)$tot.withinss})

plot(1:10,optimal,pch = 18, xlab = "Number of Clusters",ylab = "Variance Explained",type = "b",main = "Optimal Clusters")

set.seed(123)
# Performing K means Clustering
clusters <- kmeans(x = scaleddata[,-1],centers = 6,nstart = 20,iter.max = 15)$cluster
segments1 <- cbind(aprildata,clusters)
set.seed(123)
kmeans(x = scaleddata[,-1],centers = 6,nstart = 20,iter.max = 15)$size
write.csv(segments1,"C:\\Users\\V.Babuvenkatesh\\Desktop\\Files\\06-19\\December 2016\\Dec2016_Cluster6.csv")

# Data Preparation for BarChart
products <- colnames(segments1[,c(-1,-24)]) 
clustermeans = aggregate(segments1[, 2:24], list(segments1$clusters), median)
clustermeans <- clustermeans[,-1]
clustermeans_long<-melt(clustermeans,id.vars = "clusters")

# Plotting Barchar using GGplot Libraray

ggplot(clustermeans_long,aes(x=variable,y=value,fill=factor(clusters)))+
  geom_bar(stat="identity",position= position_dodge(width = 0.8))+
  scale_fill_discrete(name="Clusters",
                      breaks=c(1, 2,3,4,5,6),
                      labels=c("Cluster1", "Cluster2","Cluster3","Cluster4","Cluster5","Cluster 6"))+
  xlab("Products")+ylab("Median Spends") + theme(axis.text.x = element_text(face="bold", 
                                                                            size=8, angle=90))



# Anova Test (Finding Variables with Significant Contribution)
anova = aov(data = segments1[,-1],formula = clusters~.)
summary(anova)

# Box plot
ggplot(stack(plotdata[,-1]),aes(x = ind, y= values)) +geom_boxplot() + theme(axis.text.x = element_text(face="bold",size=8, angle=90)) + scale_y_continuous(limits=c(0,40))+ xlab("Products")+ylab("Spend Per Room") 




# Line charts
#linedata <- read.csv("C:\\Users\\V.Babuvenkatesh\\Desktop\\Files\\06-15\\2017-06-14_Data_FS_Luxury_Properties.csv") 

