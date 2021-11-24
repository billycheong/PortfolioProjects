library(tidyverse)
library(DT)

# importing data
vgsales <- read_csv("vgsales.csv")
View(vgsales)

# take a look at the data
head(vgsales)

# summary of data
summary(vgsales)

###

### filter Year NA, 2017, 2020

vgsales <- vgsales %>% filter(!Year %in% c('N/A', 2017, 2020))


vgsalesRegion <- vgsales %>% gather(Region, Revenue, 7:10)
vgsalesRegion$Region <- as.factor(vgsalesRegion$Region)


### setting theme

mytheme_1 <- function() {
  
  return(theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4), 
               plot.title = element_text(size = 15, vjust = 2),
               axis.title.x = element_text(size = 12, vjust = -0.35)))
  
}

mytheme_2 <- function() {
  
  return(theme(axis.text.x = element_text(size = 10, vjust = 0.4), 
               plot.title = element_text(size = 15, vjust = 2),
               axis.title.x = element_text(size = 12, vjust = -0.35)))
  
}

###=========================================================================================================

### Most frequent year in the database
  
vgsales %>% ggplot(aes(Year)) + geom_bar(fill = "blue") + 
  ggtitle("Video Game Release by Year") +
  mytheme_1()

# 2002 to 2011 have the highest number of game released ########################################################################

###=========================================================================================================

### Revenue by Year

revenue_by_year <- vgsales %>%  group_by(Year) %>% summarise(Revenue =sum(Global_Sales))  

revenue_by_year %>% ggplot(aes(Year,Revenue)) + geom_bar(stat = "identity", fill = "#FF5733") +
  ggtitle("Revenue by Year") +  xlab("Year") +  ylab("Revenue") +
  mytheme_1()

# There is huge spike in the number of releases after 2000 and it peaked during 2008 and 2009.

###========================================================================================================

### Top Game by Revenue each Year

top_game_year <- vgsales %>% group_by(Year, Name) %>% summarise(Revenue = sum(Global_Sales)) %>% top_n(1)

datatable(top_game_year)

top_game_year %>% ggplot(aes(Year, Revenue, fill = Name)) +
  geom_histogram(stat = "identity") + 
  ggtitle("Top Game by Revenue each Year") +
  theme(legend.position = "top") +
  mytheme_1()

# 2006 was the best year with Wii Sports generated huge revenue compared to all other years.

### Top Platform by Revenue each Year

top_platform_year <- vgsales %>% group_by(Year, Platform) %>% summarise(Revenue = sum(Global_Sales)) %>% top_n(1)

datatable(top_platform_year)

top_platform_year %>% ggplot(aes(Year, Revenue, fill = Platform)) +
  geom_histogram(stat = "identity") + 
  ggtitle("Top Platform by Revenue each Year") +
  theme(legend.position = "top") +
  mytheme_1()

# Once a platform clicks in the market, it goes on to rule for few years.
# Playstation platform is the most famous in the market for nearly 20 years.

### Top Genre by Revenue each Year

top_genre_year <- vgsales %>% group_by(Year, Genre) %>% summarise(Revenue = sum(Global_Sales)) %>% top_n(1)

datatable(top_genre_year)

top_genre_year %>% ggplot(aes(Year, Revenue, fill = Genre)) +
  geom_histogram(stat = "identity") + 
  ggtitle("Top Genre by Revenue each Year") +
  theme(legend.position = "top") +
  mytheme_1()

# Last 15 Years Action has been the most top revenue generating genre except for once Sports in 2006.


### Top Publisher by Revenue each Year

top_publisher_year <- vgsales %>% group_by(Year, Publisher) %>% summarise(Revenue = sum(Global_Sales)) %>% top_n(1)

datatable(top_publisher_year)

top_publisher_year %>% ggplot(aes(Year, Revenue, fill = Publisher)) +
  geom_histogram(stat = "identity") + 
  ggtitle("Top Publisher by Revenue each Year") +
  theme(legend.position = "top") +
  mytheme_1()

# Nintendo is by far the most dominating company in Video Game Industry
# Nintendo and EA are dominating for more than 3 decades now.

###========================================================================================================

### Distribution of Sales

vgsalesRegion %>% group_by(Region) %>% summarise(TotalRevenue = sum(Revenue)) %>%
  ggplot(aes(reorder(Region, TotalRevenue), TotalRevenue, fill = Region)) + geom_bar(stat = "Identity") +
  mytheme_2() +
  ggtitle("Sales Region Distribution") +
  xlab("Region") +
  ylab("Revenue in Millions") +
  theme(legend.position = "none")

# The highest sales region is NA follow by EU, JP, Others ########################################################

###=========================================================================================================

### NA_Sales

vgsales %>% ggplot(aes(NA_Sales)) + 
  geom_histogram(bins = 50, fill = "blue") +
  xlab("NA Sales (in millions)") +
  ylab("Frequency") +
  ggtitle("NA Sales Histogram") +
  theme_minimal()

### EU Sales

vgsales %>% ggplot(aes(EU_Sales)) + 
  geom_histogram(bins = 50, fill = "blue") +
  xlab("Sales in North America (in millions)") +
  ylab("Frequency") +
  ggtitle("EU Sales Histogram") +
  theme_minimal()


### JP Sales

vgsales %>% ggplot(aes(JP_Sales)) + 
  geom_histogram(bins = 50, fill = "blue") +
  xlab("Sales in Japan (in millions)") +
  ylab("Frequency") +
  ggtitle("Japan Sales Histogram") +
  theme_minimal()

### Other Sales

vgsales %>% ggplot(aes(Other_Sales)) + 
  geom_histogram(bins = 50, fill = "blue") +
  xlab("Other Sales (in millions)") +
  ylab("Frequency") +
  ggtitle("Other Sales Histogram") +
  theme_minimal()

### Global Sales

vgsales %>% ggplot(aes(Global_Sales)) + 
  geom_histogram(bins = 50, fill = "blue") +
  xlab("Global Sales (in millions)") +
  ylab("Frequency") +
  ggtitle("Global Sales Histogram") +
  theme_minimal()

# majority of the video game sales is below 2 millions ###############################################

###=========================================================================================================

### Top 10 video game by Number of Release

vgsales %>% group_by(Name) %>% summarise(Total = n(), Percentage = Total/dim(vgsales)[1] * 100) %>%
  arrange(desc(Total)) %>% head(10) %>% 
  ggplot(aes(reorder(Name,Total), Total, fill = Name)) + geom_bar(stat='identity') +
  theme_minimal() +
  mytheme_2() +
  ggtitle("Most Popular Game by Number of Release") +
  xlab("Game") +
  ylab("Count") +
  coord_flip() +
  theme(legend.position = "none")

# Need for Speed have the highest number of release, LEGO and FIFA Series are very popular too.
  
### Top platform by Number of Release

vgsales %>% group_by(Platform) %>% summarise(Total = n(), Percentage = Total/dim(vgsales)[1] * 100) %>%
  arrange(desc(Total)) %>% head(5) %>% 
  ggplot(aes(reorder(Platform, Total), Total, fill = Platform)) + geom_bar(stat='identity') +
  mytheme_2() +
  ggtitle("Most Popular Platform by Number of Release") +
  xlab("Platform") +
  ylab("Count") +
  coord_flip() +
  theme(legend.position = "none")

# DS release the highest number of game and follow closely by PS2
  
### Top Genre by Number of Release

vgsales %>% distinct(Genre)
unique(vgsales$Genre)

vgsales %>% group_by(Genre) %>% summarise(Total = n(), Percentage = Total/dim(vgsales)[1] * 100) %>%
  arrange(desc(Total)) %>% 
  ggplot(aes(reorder(Genre, Total), Total, fill = Genre)) + geom_bar(stat='identity') +
  mytheme_2()+
  ggtitle("Most Popular Genre by Number of Release") +
  xlab("Genre") +
  ylab("Count") +
  coord_flip() +
  theme(legend.position = "none")

# Action game contribute to almost 20% of all the game released

### Top Publisher by Number of Release

vgsales %>% group_by(Publisher) %>% summarise(Total = n(), Percentage = Total/dim(vgsales)[1] * 100) %>%
  arrange(desc(Total)) %>% head(10) %>% 
  ggplot(aes(reorder(Publisher, Total), Total, fill = Publisher)) + 
  geom_bar(stat='identity') + 
  mytheme_2() +
  ggtitle("Top Publisher by Number of Release") +
  xlab("Publisher") +
  ylab("Count") +
  coord_flip() +
  theme(legend.position = "none")

# There are 577 publishers
# EA is Top publisher with Activision coming in second.
# Top 10 publishers have published nearly 35% of all the games.

###=========================================================================================================

### Top Revenue Generating Game
vgsales %>% group_by(Name) %>% 
  summarise(Revenue = sum(Global_Sales), Percentage = Revenue/sum(vgsales$Global_Sales)*100) %>% 
  arrange(desc(Revenue)) %>% 
  head(10) %>% 
  ggplot(aes(reorder(Name, Revenue), Revenue, fill = Name)) + geom_bar(stat='identity') +
  ggtitle("Highest Revenue Generating Game") +
  xlab("Game") +
  ylab("Revenue in Millions") +
  mytheme_2() +
  coord_flip() +
  theme(legend.position = "none")

# Wii Sports game has 1% of the total revenue with 82.74 million
# Grand Theft Auto V coming second with 55.92 million

### Top Revenue Generating Platform
vgsales %>% group_by(Platform) %>%
  summarise(Revenue = sum(Global_Sales), Percentage = Revenue/sum(vgsales$Global_Sales)*100) %>% 
  arrange(desc(Revenue)) %>% 
  head(10) %>% 
  ggplot(aes(reorder(Platform, Revenue), Revenue, fill = Platform)) + geom_bar(stat='identity') +
  theme_minimal() +
  ggtitle("Highest Revenue Generating Platform") +
  xlab("Platform") +
  ylab("Revenue in Millions") +
  mytheme_2() +
  coord_flip() +
  theme(legend.position = "none")

# PS2 generated 14% of the total revenue with 1233 millions
# X360 generated 11% of the total revenue with 970 millions

### Top Revenue Generating Genre
vgsales %>% group_by(Genre) %>%
  summarise(Revenue = sum(Global_Sales), Percentage = Revenue/sum(vgsales$Global_Sales)*100) %>% 
  arrange(desc(Revenue)) %>% 
  head(10) %>% 
  ggplot(aes(reorder(Genre, Revenue), Revenue, fill = Genre)) + geom_bar(stat='identity') +
  ggtitle("Highest Revenue Generating Genre") +
  xlab("Genre") +
  ylab("Revenue in Millions") +
  mytheme_2() +
  coord_flip() +
  theme(legend.position = "none")

# Action game accounted for almost 20% of the total revenue with 1723 millions
# Sports game come in second with almost 15% with 1309 millions

### Top Revenue Generating Publisher
vgsales %>% group_by(Publisher) %>%
  summarise(Revenue = sum(Global_Sales), Percentage = Revenue/sum(vgsales$Global_Sales)*100) %>% 
  arrange(desc(Revenue)) %>% 
  head(10) %>% 
  ggplot(aes(reorder(Publisher, Revenue), Revenue, fill = Publisher )) + geom_bar(stat='identity') +
  ggtitle("Highest Revenue Generating Publisher") +
  xlab("Publisher") +
  ylab("Revenue in Millions") +
  mytheme_2() +
  theme(legend.position = "none") +
  coord_flip()

# There is change in the positions from the list by number of releases.
# Nintendo is Top 1 with almost 21% of the overall revenue
# EA being Top 2 with nearly half the revenue of the Nintendo.
# Nearly staggering 70% of the overall revenue is generated by the Top 10 publishers

###=================================================================================================================

## Key Take aways from the Analysis:

# Over the last few years there is decrease in the games which have crossed 100K copies sales.
# In fact last 5-6 years has seen decrease in the revenue.
# Nintendo is the top most publisher by revenue.
# Action is the top most genre by revenue.
# Action is ruling the market from 2001
# Play Station is the most popular platform
# EA is the top most publisher by number of releases.
# Wii Sports is the top most game by revenue.
# North America and Europe contribute 75% of the market share by revenue.