#Title : RFM AnalysisK-means clustering 

#loading libraries for visualization
library("ggplot2")
library("tidyverse")
library(factoextra)
library("clValid")


#I. Data Preprocessing

#load dataset, put into dataframe
df <- read.csv("https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/online_retail_clean.csv")

#inspect 6 first row
head(df)

#inspect each variable quartile
summary(df)

#take only R,F and M variable into new dataframe
df_rfm<- df[,2:4]

#inspect 6 first row of dataframe
head(df_rfm)


#inspect recency, frequency and monetary data distribution
ggplot(df, aes(y=recency)) + 
  geom_boxplot()

ggplot(df, aes(y=frequency)) + 
  geom_boxplot(outlier.color="red")

ggplot(df, aes(y=monetary)) + 
  geom_boxplot(ourlier.color="red")

#count number of rows of less than 0 monetary, then remove
nrow(df[df$monetary < 0,])
#25 rows

nrow(df[df$monetary < 0,])/dim(df)[1]
#0.01 percent

#keep row with monetary bigger than or equal to 0
df_sub <- subset(df[df$monetary >= 0, ])

#check dimension after subsetting
dim(df_sub)[1]
#2350

#2. Data Modelling

#find the best cluster by calculating the wss (within sum of squares), distance of each datapoint to its centroid.
wss <-0
for (i in 1:15) {
  clust <- kmeans(df_sub, centers = i, nstart = 10)
  # Save total within sum of squares to wss variable
  wss[i] <- clust$tot.withinss
}

#plot into scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

#"elbow" of this plot is indicated as the appropriate number of cluster. In this plot, the elbow located in k-3
#model with 3 clusters
clust3 <-kmeans(df_sub, 3, nstart=10)

#visualize cluster
fviz_cluster(clust3, df_sub)

#print size of every cluster
print(clust3$size)

#inspect center of every cluster
clust3$centers

#model with 4 clusters
clust4 <-kmeans(df_sub, 4, nstart=10)

#visualize cluster
fviz_cluster(clust4, df_sub)

#print size of every cluster
print(clust4$size)

#inspect center of every cluster
clust4$centers


#3.Evaluation
#Calculate Dunn Index of 3 cluster
dunn_km = dunn(clusters = clust3$cluster, Data=df_sub)
dunn_km
#0.00238

#count Dunn Index of 4 cluster
dunn_km = dunn(clusters = clust4$cluster, Data=df_sub)
dunn_km
#0.00177



#let's assume that 3 clusters is the best cluster we'd use. Let's observe every cluster characters

#make new dataframe by labelling each customer data with its cluster
df_clust <-cbind(df_sub, cluster=clust3$cluster)
head(df_clust)

#encode R,F, and M variable
df_rfm_encode <- df_clust3 %>% mutate(
  recency = ifelse(df_clust3$recency <= median(df_clust3$recency),0,1),
  monetary = ifelse(df_clust3$monetary <= median(df_clust3$monetary),0,1),
  frequency = ifelse(df_clust3$frequency <= median(df_clust3$frequency),0,1)
)

df_clust1 <- df_rfm_encode[df_rfm_encode$clust == 1,]
df_clust2 <- df_rfm_encode[df_rfm_encode$clust == 2,]
df_clust3 <- df_rfm_encode[df_rfm_encode$clust == 3,]

head(df_clust3)

g1 <- ggplot(data=df_clust1, aes(x=recency, y=CustomerID)) +
  geom_bar(stat="identity")

g2<- ggplot(data=df_clust1, aes(x=frequency, y=CustomerID)) +
  geom_bar(stat="identity")

g3<-ggplot(data=df_clust1, aes(x=monetary, y=CustomerID)) +
  geom_bar(stat="identity")

g4<-ggplot(data=df_clust2, aes(x=recency, y=CustomerID)) +
  geom_bar(stat="identity")

g5<-ggplot(data=df_clust2, aes(x=frequency, y=CustomerID)) +
  geom_bar(stat="identity")

g6<-ggplot(data=df_clust2, aes(x=monetary, y=CustomerID)) +
  geom_bar(stat="identity")

g7<- ggplot(data=df_clust3, aes(x=recency, y=CustomerID)) +
  geom_bar(stat="identity")

g8<-ggplot(data=df_clust3, aes(x=frequency, y=CustomerID)) +
  geom_bar(stat="identity")

g9<-ggplot(data=df_clust3, aes(x=monetary, y=CustomerID)) +
  geom_bar(stat="identity")

layout(matrix(1:9, ncol = 3))
figure <- ggarrange(g1, g2, g3,g4,g5,g6,g7,g8,g9,
                    ncol = 3, nrow = 3)

figure



#Hiearchy Clustering

cluster_dist <-  dist(df_sub)
hcluster <- hclust(cluster_dist)
plot(hcluster)

abline(h=3.5, col="red")
cutree(hcluster, h=6)

hclust_complete <- hclust(cluster_dist, method = "complete")
plot(hclust_complete)

