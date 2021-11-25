library(tidyverse)

# importing data

df <- read_csv("Mall_Customers.csv")

summary(df)

# Gender Distribution
df %>% group_by(Gender) %>% summarise(count = n()) %>% 
  mutate(perc = count/sum(count)*100) %>% 
  ggplot(aes(Gender, count, fill = Gender)) + geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = paste0(perc, "%")),
          vjust    = -0.5,
          color    = "black",
          position = position_dodge(0.9),
          size     = 3.5) +
  ggtitle("Gender Distribution") +
  xlab("Gender") + ylab("Count")

# Age Distribution
df %>%  ggplot(aes(Age, fill = Gender)) + geom_density(alpha = 0.4) + 
  ggtitle("Age Distribution")

# Annual Income Distribution
df %>%  ggplot(aes(`Annual Income (k$)`)) + geom_boxplot(fill='#00BFC4') +
  ggtitle("Income Distribution") +
  coord_flip()

# Spending Score Distribution
df %>%  ggplot(aes(`Spending Score (1-100)`)) + geom_boxplot( fill ='#F8766D') +
  ggtitle("Spending Score Distribution") +
  coord_flip() 

# Selecting Data for Kmean
kdata <- df[,c(4,5)]

# wss plot

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(kdata)

# Kmean
km <- kmeans(kdata,5)

# Cluster Plot 
km

# Visualize the Cluster
kdata %>% ggplot(aes(`Annual Income (k$)`,`Spending Score (1-100)`)) + 
  geom_point(stat = "identity", aes(color = as.factor(km$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
  ggtitle("Mall Customer Segmens", subtitle = "K-means Clustering")


## take a look at the center of these clusters
km$centers

# conclusion 
# Cluster 1. Customers with medium annual income and medium annual spend
# Cluster 2. Customers with high annual income and high annual spend
# Cluster 3. Customers with low annual income and low annual spend
# Cluster 4. Customers with high annual income but low annual spend
# Cluster 5. Customers low annual income but high annual spend


