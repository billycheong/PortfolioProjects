library(tidyverse)
library(readr)
library(factoextra)
library(cluster)
library(Rtsne)


## Importing Data
df <- read_delim("marketing_campaign.csv", 
                                 delim = "\t", escape_double = FALSE, 
                                 trim_ws = TRUE)

##
str(df)

## Looking for missing data
sum(is.na(df))

## There are 24 NA in the data, I will remove the missing data 
df <- na.omit(df)

## Changing Dt_customer to Date format
df1$Dt_Customer <- as.Date(df1$Dt_Customer, format =  "%d-%m-%Y")

## Calculate Age
df$age <- 2021 - df$Year_Birth

## Calculate Number of Child
df$child <- df$Kidhome + df$Teenhome

## Combine all  the spending together
df$totalspend <- df$MntWines + df$MntFruits + df$MntMeatProducts + df$MntFishProducts + df$MntSweetProducts + df$MntGoldProds

## Combine all the campaign result
df$accepted <- df$AcceptedCmp1 + df$AcceptedCmp2 + df$AcceptedCmp3 + df$AcceptedCmp4 + df$AcceptedCmp5

## Remove all the column that I had combined
df1 <- df[c(-1,-2,-6,-7,-10,-11,-12,-13,-14,-15,-21,-22,-23,-24,-25,-27,-28)]

## Customer Education Distribution
df1 %>% ggplot(aes(Education, fill = Education)) + 
  geom_bar(stat = "count") +
  ggtitle("Customer Education Distribion") +
  theme_bw()

## Convert Marital Status to Single or Couple
df1$Marital_Status[df1$Marital_Status == "Divorced"] = "Single"
df1$Marital_Status[df1$Marital_Status == "Together"] = "Couple"
df1$Marital_Status[df1$Marital_Status == "Married"] = "Couple"
df1$Marital_Status[df1$Marital_Status == "Widow"] = "Single"
df1$Marital_Status[df1$Marital_Status == "Alone"] = "Single"
df1$Marital_Status[df1$Marital_Status == "Absurd"] = "Single"
df1$Marital_Status[df1$Marital_Status == "YOLO"] = "Single"

## Now Lets take a look at the distribution of the Marital Status
df1 %>% ggplot(aes(Marital_Status, fill = Marital_Status)) +
  geom_bar(stat = "count") +
  ggtitle("Marital Status Distribution") +
  theme_bw()

## Income Distribution
df1 %>% filter(Income < 600000) %>%
  ggplot(aes(Income)) +
  geom_histogram(fill = "blue", color = "white", bins = 30) +
  ggtitle("Income Distribution") +
  theme_bw()

## Income Boxplot
df1 %>%
  filter(Income < 600000) %>%  
  ggplot(aes(Income)) + 
  geom_boxplot() +
  ggtitle("Income Boxplot") +
  theme_bw()

## Age Distribution
df1 %>% ggplot(aes(age)) + 
  geom_histogram(color = "white", fill = "blue") +
  ggtitle("Age Distribution") +
  theme_bw()

## Number of Child Distribution
df1 %>% ggplot(aes(child)) + 
  geom_bar(stat = "count") +
  ggtitle("Number of Children") + 
  theme_bw()

## Total Spend Distribution
df1 %>% ggplot(aes(totalspend)) + 
  geom_histogram(color = "white", fill = "blue") +
  ggtitle("Customer Total Spending Distribution") +
  theme_bw()

## Income and Total Spend
df1 %>% 
  filter(Income < 600000) %>% 
  ggplot(aes(Income, totalspend)) +
  geom_point() + 
  geom_smooth() +
  ggtitle("Income vs Total Spend") +
  theme_bw()

## Education and Income
df1 %>% 
  filter(Income <600000) %>% 
  ggplot(aes(Education, Income)) +
  geom_boxplot() +
  ggtitle("Education and Income") +
  theme_bw()

## Customer Recency and Total Spend
df1 %>% ggplot(aes(Dt_Customer, totalspend)) +
  geom_point(color = "blue") +
  ggtitle("Customer Seniority vs Total Spend") + 
  theme_bw()

### No clear pattern between new and old customer with how much they Spend

### Picking the column for clustering
df2 <- df1[c(1,2,3,6,7,8,9,10,11,12,13,14,15,16)]


### Changing the Education and Marital Status to factor
df2$Education <- factor(
  x = df2$Education,
  levels = c("2n Cycle", "Basic", "Graduation", "Master", "PhD")
)

df2$Marital_Status <- factor(
  x = df2$Marital_Status,
  levels = c("Single", "Couple")
)

### In R, the Gower distance can be calculated using the ‘daisy’ package. Before we use the ‘daisy’ function,
### we observe the distribution of the numerical variables to understand if any of them need transformations. 
### We find that the variable Total Spending needs a log transformation due to the positive skew in its distribution.

gower_df <- daisy(df2,
                  metric = "gower" ,
                  type = list(logratio = 13))

summary(gower_df)

## Silhouette Width to select the optimal number of clusters

silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette)

### As can be seen, the value is maximized at 3. 
### Hence, we can conclude that clustering the data into 3 clusters gives us the best segmentation possible.

pam_df = pam(gower_df, diss = TRUE, k = 3)
df2[pam_df$medoids, ]

pam_summary <- df2 %>%
  mutate(cluster = pam_df$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))
pam_summary$cluster_summary[[1]]


pam_summary <- df2 %>%
  mutate(cluster = pam_df$clustering) %>%
  group_by(cluster)

###
pam_summary$cluster <- as.factor(pam_summary$cluster)

## Visualization
### Finally, let us wrap this article up with some fancy visualizations. 
### For this, we use the t-SNE or the t-Distributed Stochastic Neighbor Embedding technique. 
### This technique provides a great way to visualize a multi-dimensional data set such as the one we are working on.


tsne_object <- Rtsne(gower_df, is_distance = TRUE)
tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_df$clustering))
ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster))



###########

pam_summary %>%  ggplot(aes(cluster, fill = Education)) + geom_bar(stat = "count")

pam_summary %>%  ggplot(aes(cluster, fill = Marital_Status)) + geom_bar(stat = "count")

pam_summary %>%  ggplot(aes(child, fill = cluster)) + geom_bar(stat = "count")

pam_summary %>% filter(Income < 600000) %>% ggplot(aes(cluster, Income, fill = cluster)) + geom_boxplot()

pam_summary %>% ggplot(aes(cluster, totalspend, fill = cluster)) + geom_boxplot()

pam_summary %>% ggplot(aes(cluster, accepted, fill = cluster)) + geom_boxplot()

pam_summary %>% ggplot(aes(cluster, age, fill = cluster)) + geom_boxplot()

pam_summary %>% ggplot(aes(cluster, NumWebPurchases, fill = cluster)) + geom_boxplot()

pam_summary %>% ggplot(aes(cluster, NumCatalogPurchases, fill = cluster)) + geom_boxplot()

pam_summary %>% ggplot(aes(cluster, NumStorePurchases, fill = cluster)) + geom_boxplot()

pam_summary %>% ggplot(aes(cluster, NumWebVisitsMonth, fill = cluster)) + geom_boxplot()

# Conclusion

### All the above plotted features have a clear separation between clusters. 
### Cluster 1 are evenly distribute in education, Cluster 2 is mostly Graduate and Cluster 3 are highly educated.
### cluster 1 are Single,  Cluster 2 are Couple and Cluster 3 consist of both.
### Number of Child are evenly distributed between all cluster
### Cluster 2 have the lowest income, cluster 1 have medium income and cluster 3 have the highest income
### Cluster 2 are low spender, cluster 1 are medium spender and cluster 3 are high spender
### Cluster 3 accepted the most number of promotion
### Cluster 3 are slightly oldest and cluster 1 and 2 are about the same age range



