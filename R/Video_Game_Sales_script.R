install.packages("tidyverse")
install.packages("skimr")
install.packages("dplyr")
install.packages("rmarkdown")
install.packages("knitr")
library("tidyverse")
library("readr")
library("skimr")
library(ggplot2)
library(dplyr)
library(rmarkdown)
library(knitr)

vgsales <- read_csv("vgsales.csv")

str(vgsales)
View(vgsales)
skim_without_charts(vgsales)


yearly_growth <- vgsales %>% 
  group_by(Year) %>% 
  summarise(Total_Global_Sales = sum(Global_Sales)) %>% 
  drop_na()

View(yearly_growth)
ggplot(data = yearly_growth,mapping = aes(x = Year,y=Total_Global_Sales))+geom_point()



region_trend_NA <- vgsales %>% 
  group_by(Year) %>% 
  summarise(Total_NA_Sales = sum(NA_Sales)) %>% 
  drop_na()

View(region_trend_NA)
ggplot(data = region_trend_NA,mapping = aes(x = Year,y=Total_NA_Sales))+geom_point()



region_trend_EU <- vgsales %>% 
  group_by(Year) %>% 
  summarise(Total_EU_Sales = sum(EU_Sales)) %>% 
  drop_na()

View(region_trend_EU)
ggplot(data = region_trend_EU,mapping = aes(x = Year,y=Total_EU_Sales))+geom_point()+ theme(axis.text.x = element_text(angle = 90))



region_trend_JP <- vgsales %>% 
  group_by(Year) %>% 
  summarise(Total_JP_Sales = sum(JP_Sales)) %>% 
  drop_na()

View(region_trend_JP)
ggplot(data = region_trend_JP,mapping = aes(x = Year,y=Total_JP_Sales))+geom_point()



brand_sales <- vgsales %>% 
  group_by(Publisher) %>% 
  summarise(Total_brand_Sales = sum(Global_Sales)) %>% 
  drop_na()

View(brand_sales)
ggplot(data = brand_sales,mapping = aes(x=Publisher,y=Total_brand_Sales))+geom_point()


Genre_sales <- vgsales %>% 
  group_by(Genre) %>% 
  summarise(Total_Sales = sum(Global_Sales)) %>% 
  drop_na()

View(Genre_sales)
ggplot(data = Genre_sales, aes(x = Genre, y = Total_Sales)) +
  geom_bar(stat = "identity") +
  labs(x = "Genre", y = "Total Sales", title = "Total Genre Sales")
