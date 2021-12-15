## WholeSale Custoemr Data Set.
df<-read.csv("F:\\Aegis\\Machine Learning\\Topic 5\\Wholesale Customer dataset.csv")
summary(df)
head(df)
table(df$Channel)
View(df)

library(dplyr)
library(ggplot2)
library(factoextra)

## Part 1. Data Visualization
df$Channel=factor(df$Channel)
df$Region=factor(df$Region)
ggplot(df,aes(Region))+geom_bar(aes(fill=Channel))

## Total spending for each category, region and channel wise.
total<-df %>% group_by(Region,Channel) %>% summarise_all(funs(sum))
total<-as.data.frame(total)
total


## Visualizing the nature of the distribution of each columns in the dataset.
ggplot(df,aes(Fresh))+geom_histogram(fill="red",color="blue",bins = 50)
ggplot(df,aes(Milk))+geom_histogram(fill="red",color="blue",bins = 50)
ggplot(df,aes(Grocery))+geom_histogram(fill="red",color="blue",bins = 50)
ggplot(df,aes(Frozen))+geom_histogram(fill="red",color="blue",bins = 50)
ggplot(df,aes(Detergents_Paper))+geom_histogram(fill="red",color="blue",bins = 50)
ggplot(df,aes(Delicassen))+geom_histogram(fill="red",color="blue",bins = 50)


## Part 2. Nominal Attributes Removal.
colnames(df)
df_with_outlier<-df[,-c(1,2)]
df_no_outlier<-df[,-c(1,2)] 

## Part 3.

### With outliers.

##K-Means
## Normalisation without removing the outliers.

df1=sapply(df_with_outlier,function(x){(x-mean(x))/sd(x)})
df1  ## normalized data 

## k-means without outlier removal

i<-1
k1<-vector(mode = "integer")
error1<-vector(mode = "integer")
between_error1<-vector(mode = "integer")
for(i in 1:15)
{
  model1<-kmeans(df1,i,nstart = 20)
  k1<-rbind(k1,i)
  error1<-rbind(error1,model1$tot.withinss)
  between_error1<-rbind(between_error1,model1$betweenss)
}

error1
between_error1
plot(k1,error1)  
lines(k1,error1)  ## ELbow plot with total within ss vs number of clusters.

## Plotting the kmeans model having outlier, considering 4 clusters, from the elbow plot.
set.seed(1)
model1<-kmeans(df1,4,nstart = 20)
model1$cluster                            
df_final_with_outlier<-df
df_final_with_outlier$Clusters<-model1$cluster
View(df_final_with_outlier)
fviz_cluster(model1,data = df1,geom ="point",stand = FALSE,ellipse.type ='norm')

d.1<-df_final_with_outlier[,-c(1,2)] %>% group_by(Clusters) %>% summarise_all(funs(sum))
View(d.1) ## This gives the column wise total spending for each cluster.

d11<-(apply(df_final_with_outlier[,-c(1,2,9)],1,sum))
dtt<-cbind("Clusters"=df_final_with_outlier$Clusters,"Sums"=d11)
head(dtt)
dtt<-as.data.frame(dtt)
summary(dtt)
dtt$Clusters<-factor(dtt$Clusters)
ggplot(dtt, aes(x=Clusters, y=Sums, fill=Clusters)) +  geom_boxplot()
## This gives the visualizations of the range of the customer total spending for each clusters.

## H-Clust
set.seed(1)
hclust1=hclust(dist(df1))
hclust1$height
plot(hclust1)
summary(hclust1)
fviz_nbclust(df1,hcut,method = 'wss')
## From this optimal no of clusters graph, although it is not a steep curve like an elbow plot, still we take
## no. of clusters as 5, because beyond that the rate of decrease in error is less.

hclust_with_outlier<-cutree(hclust1,k=5)

## Without Outliers
##K-Means
#Removing the Outliers
n<-as.numeric(ncol(df_no_outlier))
i<-1
for(i in 1:n)
  {
  if(is.numeric(df_no_outlier[,i])==T)
    {
    if(length(boxplot(df_no_outlier[,i],plot = F)$out) !=0)
      {
        replace_out<- min(boxplot(df_no_outlier[,i],plot = F)$out)
        df_no_outlier[,i]<-ifelse(df_no_outlier[,i]>replace_out,replace_out,df_no_outlier[,i])
      }
    }
  }
    
summary(df_no_outlier)   


## Normalisation after removing the outliers.

df2=sapply(df_no_outlier,function(x){(x-mean(x))/sd(x)})
df2 ## Normalized data




## K.-means with outlier removal and normalization
i<-1
k<-vector(mode = "integer")
error<-vector(mode = "integer")
between_error<-vector(mode = "integer")
for(i in 1:15)
{
  model<-kmeans(df2,i,nstart = 20)
  k<-rbind(k,i)
  error<-rbind(error,model$tot.withinss)
  between_error<-rbind(between_error,model$betweenss)
}
model$betweenss/model$totss
model$tot.withinss/model$totss
error
between_error
plot(k,error)  
lines(k,error)  ## Elbow plot 


## Plotting the kmeans model without outlier, considering 6 clusters, from the elbow plot.
set.seed(1)
model<-kmeans(df2,6,nstart = 20)
model$cluster
df_final_no_outlier<-df
df_final_no_outlier$Clusters<-model$cluster
View(df_final_no_outlier)
fviz_cluster(model,data = df2,geom ="point",stand = FALSE,ellipse.type ='norm')


d<-df_final_no_outlier[,-c(1,2)] %>% group_by(Clusters) %>% summarise_all(funs(sum))
View(d) ## This gives the column wise total spending for each cluster.
d1<-(apply(df_final_no_outlier[,-c(1,2,9)],1,sum))
dt<-cbind("Clusters"=df_final_no_outlier$Clusters,"Sums"=d1)
head(dt)
dt<-as.data.frame(dt)
summary(dt)
dt$Clusters<-factor(dt$Clusters)
ggplot(dt, aes(x=Clusters, y=Sums, fill=Clusters)) +  geom_boxplot()
## This gives the visualizations of the range of the customer total spending for each clusters.

## Inference:
## We have considered kmeans algorithm model, for number of clusters from 1 to 15.And we
# plot the values of the total of the within ss error for each kmeans model, and from the elbow plot
#obtained, we can visualize that the error drastically reduces from clusters 2 and 3 , and keeps
# on significantly reducing till 6 clusters, and beyond that the drop in error is not much significant and drastic.
## So we select the kmeans model with 6 clusters after removal of outliers.


## H-Clust
set.seed(1)
hclust2=hclust(dist(df2))
plot(hclust2)
summary(hclust2)
fviz_nbclust(df2,hcut,method = 'wss')
## From this optimal no of clusters graph,similar to elbow plot, so we take
## no. of clusters as 3, because beyond that the rate of decrease in error is less.

hclust_no_outlier<-cutree(hclust2,k=3)



df<-cbind(df,"K-means Cluster with Outliers"=model1$cluster,"HClust Cluster with Outliers"=hclust_with_outlier,
"K-means Cluster removing Outliers"=model$cluster, "HClust Cluster removing Outliers"=hclust_no_outlier)

View(df) ## Final dataset, containing the clusters by both the k-means and hclust,
## with and without the outliers.

