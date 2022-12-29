library(tidyr) 
library(janitor)
library(dplyr) 

customer=read.csv("CustomerSegmentation.csv")

#encoding binary values of Gender Column to 1 and 0 

#cleaning and preprocessing data
View(customer)
names(customer)
summary(customer)
customer1 <- drop_na(customer)
#str(customer)
#str(customer1)
customer2 <- remove_empty(customer1, which = c("rows" , "cols") , quiet = FALSE)

#removing duplicated values
customer2<- customer2[-1]
duplicated(customer2)
customer2 <- customer2[!duplicated(customer2),]
View(customer2)




#statistics
#customer age
var(customer2$Age)
sd(customer2$Age)
range(customer2$Age)
mean(customer2$Age)
median(customer2$Age)
mode(customer2$Age)
max(customer2$Age)
min(customer2$Age)

#customer Annual income
summary(customer$Annual.Income..k..)
var(customer2$Annual.Income..k..)
sd(customer2$Annual.Income..k..)

#customer Spending score
summary(customer2$Spending.Score..1.100.)
var(customer2$Spending.Score..1.100.)
sd(customer2$Spending.Score..1.100.)

#correlation between income and spending score 
cor(customer2$Annual.Income..k.., customer2$Spending.Score..1.100.)

#end statistics


#Data Visualizations
library(MASS)
#Age Visualization
hist(customer2$Age,main = "Distribution of customers Age" ,col = "yellow",xlab = "Age")

#Annual Income Visualization
hist(customer$Annual.Income..k..,main = "Histogram of Customers Annual income" ,Freq = FALSE,col = "yellow",labels = TRUE,xlab = "Annual Income in Thousands")


#spending score visualization
hist(customer2$Spending.Score..1.100.,main = "Histogram of Customers Spending Score" ,Freq = FALSE,col = "yellow",labels = TRUE,xlab = "Spending Score")

#Gender Visualization
defaultCustomersDS <- read.csv("CustomerSegmentation.csv")
a=table(defaultCustomersDS$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision", ylab="Count",xlab="Gender",col=rainbow(2),legend=rownames(a))

#Analysis algorithms
#k-means clustering

customer_data <- customer2
customer_data.labels = customer_data$Gender
customer_data <- customer_data[2:4]
customer_data

table(customer_data.labels)

#calculate the optimal number of clusters 
#using elbow method
library(factoextra)

fviz_nbclust(customer_data,kmeans,method = "wss")+ title(main = "Elbow method")
#4 is the optimal number of clusters

#k-means function

km.out<- kmeans(customer_data,centers = 4,nstart = 50,iter.max = 100,algorithm = "Lloyd")
km.out


#visualize k-means clustering
km.clusters <- km.out$cluster
rownames(customer_data)<- paste(customer2$Gender,1:dim(customer2)[1])
customer_data
fviz_cluster(list(data = customer_data,cluster = km.clusters))

#what each clusters contain of 
clustersDetails <-table(km.clusters,customer2$Gender)
clustersDetails


#visualizing clustering with the 2 features : annual income and spending score 
ggplot(customer_data,aes(x= customer_data$Annual.Income..k..,y =customer_data$Spending.Score..1.100.))+
  geom_point(stat = "identity",aes(color = as.factor(km.clusters)))+
  ggtitle("Customers Segmentation", subtitle = "Using K-means Algorithm")+
  xlab(label = "Annual Income")  + 
  ylab(label = "Spending score") + 
  
  scale_color_discrete(labels = (c(paste((clustersDetails[1,1]+clustersDetails[1,2]),"customers"),
                                   paste((clustersDetails[2,1]+clustersDetails[2,2]),"customers"),
                                   paste((clustersDetails[3,1]+clustersDetails[3,2]),"customers"),                
                                   paste((clustersDetails[4,1]+clustersDetails[4,2]),"customers"))),
                       name = "Clusters Details")


#Hierarchial clustering

#preparing data
customer_data2 <- customer2[2:4]

#calculate the distance matrix to use it in the hierarchial clustering
customer_data2.dist <- dist(customer_data2, method = 'euclidean')
customer_data2.dist

#applying hierarchial clustering algorithm
hc.out <- hclust(customer_data2.dist,method = "average") 
hc.out

#plotting the dendrogram
plot(hc.out)

#dividing the data into 5 clusters
rect.hclust(hc.out,k = 5,border = 1:6)

#cutting the tree into 5 clusters 
hcclusters <- cutree(hc.out , k = 5)



#Visualizing Hierarchial Clustering

rownames(customer_data2)<- paste(customer2$Gender,1:dim(customer2)[1])
customer_data2

fviz_cluster(list(data = customer_data2,cluster = hcclusters))


#what each cluster contains of
clustersDetails2 <- table(hcclusters,customer2$Gender)

#plotting the clusters according to the annual income and spending score 

ggplot(customer_data2,aes(x= customer_data2$Annual.Income..k..,y =customer_data2$Spending.Score..1.100.))+
  geom_point(stat = "identity",aes(color = as.factor(hcclusters)))+
  ggtitle("Clusters of Customers")+
  xlab(label = "Annual Income")  + 
  ylab(label = "Spending score")  +
  ggtitle("Customers Segmentation" , subtitle = "Using Hierarchial Clustering")+ 
  
  scale_color_discrete(labels = c(paste((clustersDetails2[1,1]+clustersDetails2[1,2]),"customers"),
                                  paste((clustersDetails2[2,1]+clustersDetails2[2,2]),"customers"),
                                  paste((clustersDetails2[3,1]+clustersDetails2[3,2]),"customers"),                
                                  paste((clustersDetails2[4,1]+clustersDetails2[4,2]),"customers"),
                                  paste((clustersDetails2[5,1]+clustersDetails2[5,2]),"customers")),
                       name = "Clusters Details")

