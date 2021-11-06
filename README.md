# portofolio
# Global Terrorism Database
More than 180,000 terrorist attacks worldwide,1970-2017

This study aims to discover which country has the most terrorist attack for the past 10 years based on the global terrorism database (GTD) produced by the National Consortium for the Study of Terrorism and Responses to Terrorism (START).

Definition of terrorism:
* "The threatened or actual use of illegal force and violence by a non-state actor to attain a political, economic, religious, or social goal through fear, coercion, or intimidation."
 
Data Content:
* Geography: Worldwide
* Time period: 1970-2017, except 1993
* Unit of analysis: Attack
* Variables: >100 variables on location, tactics, perpetrators, targets, and outcomes

The analysis is perfomed using R

# Importing Library
Before analyzing the file, we first import all the library needed for the analysis

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
#importing library
library(tidyverse)
library(patchwork)
library(dplyr)
library(tidyr)
library(ggrepel)
```
# Importing the file
Then the file is imported to be investigated
```{r}
#accesing and viewing the data 
global_terroris_data <- read.csv('globalterrorismdb_0718dist.csv')
global_terroris_data
```
from this file there are several columns that have been removed such as:
* event id which is combination of dates and unique number 
* latitude and longitude also been removed since we are not going to investigate based on that
* prov state and city also been removed because in this analysis, we are going to focus on investigating the attacks that happen on country based as well as region

# Filtering data
Since we are investigating terrorist attack that happen in the past 10 years so we only take the data that starts from 2007 and above
```{r}
global_terroris_data <-global_terroris_data %>% select(iyear,imonth,iday,country_txt,region_txt)
global_terroris_data <-global_terroris_data %>% filter(iyear>2007)
global_terroris_data
```
![image](https://user-images.githubusercontent.com/90851787/140603243-4635414b-9f70-4e63-960b-cea63d6f39de.png)

# Data checking
```{r}
#checking data info
glimpse(global_terroris_data)
```
![image](https://user-images.githubusercontent.com/90851787/140603289-7c18a7d5-f3b7-40d6-9533-b3c4f0a46cbb.png)

bsaed on the information, there is no change needed on the type of data in each column

# Checking for missing values
```{r}
#checking for missing data
global_terroris_data %>% sapply(function(x) sum(is.na(x)))
```
![image](https://user-images.githubusercontent.com/90851787/140603680-26ec9774-c372-4cae-84b2-a1f85267a7d2.png)

There is no missing value detected 

# Analysis
# Exploring Cases by Region and Country 

```{r}
#step 1
top_10_countries <- global_terroris_data %>%
  group_by(country_txt) %>%
  summarise(Number_of_Attacks = n()) %>%
  arrange(desc(Number_of_Attacks)) %>%
  head(10)

top_10_region <- global_terroris_data %>%
  group_by(region_txt) %>%
  summarise(Number_of_Attacks = n()) %>%
  arrange(desc(Number_of_Attacks)) %>%
  head(10)
```
```{r}
#step 2
#creating the graphs
top_10_region %>%
  ggplot(aes(x=Number_of_Attacks, y=reorder(region_txt, Number_of_Attacks))) +
  geom_bar(stat="identity", fill = "#6C88C4") +
  theme_bw() + 
  geom_text(aes(label=Number_of_Attacks), size = 3, position=position_dodge(width=0.8), hjust= -0.2) +
  xlim(0, 37000)+
  labs(
    title = "Top 10 Region by Number of Terrorist Attacks",
    x = "Number of Attacks",
    y = "Region") +
  theme(
    axis.title = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 8)
  ) -> P1
top_10_countries %>%
  ggplot(aes(x=Number_of_Attacks, y=reorder(country_txt, Number_of_Attacks))) +
  geom_bar(stat="identity", fill = "#E77577") +
  theme_bw() + 
  geom_text(aes(label=Number_of_Attacks), size = 3, position=position_dodge(width=0.8), hjust= -0.2) +
  xlim(0, 25000) +  
  labs(
    title = "Top 10 Countries by Number of Terrorist Attacks",
    x = "Number of Attacks",
    y = "Country") +
  theme(
    axis.title = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 8)
  ) -> P2
```
```{r}
#arranging the graph using patchwork
P1/P2
```


Reference:
National Consortium for the Study of Terrorism and Responses to Terrorism (START), University of Maryland. (2018). The Global Terrorism Database (GTD) [Data file]. Retrieved from https://www.start.umd.edu/gtd
