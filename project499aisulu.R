library(readr)
### animation
data1 <- read_csv("data1.csv")
head(data1)

set.seed(123)
options(scipen=999)

library(plotly)
library(modelr)
library(tidyverse)
library(ggplot2)

visualization <- data1 %>%
  plot_ly(
    x = ~ population, 
    y = ~consumption, 
    size = ~population,
    color = ~province, 
    frame = ~year, 
    text = ~province, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    ),
    title = "Natural gas consumption vs population by province"
  )


visualization
############### This animation shows the change of pouplation vs natural gas consumption by province during 5 years.

#### regression between population and natural consumption 
library(ggplot2)
ggplot(data1, aes(population, consumption, province)) + geom_point(aes(color = province), size = 3) + 
  geom_smooth(method = "lm", se = FALSE) + ggtitle("Natural gas consumtion vs population by regions")
## correlation between population and natural gas consumption with outlier (Kadikoy)
cor(data1$population,data1$consumption,use="complete.obs")
## fitted linear model with outlier (Kadikoy)
lm(consumption~population, data = data1)

##### finding and removing the outlier which was found to be KADIKOY
install.packages('sqldf')
library(sqldf)
data2 <- sqldf("select * from data1 where province != 'KADIKÖY'")
ggplot(data2, aes(population, consumption, province)) + geom_point(aes(color = province), size = 3) + 
  geom_smooth(method = "lm", se = FALSE) + ggtitle("Natural gas consumtion vs population by regions")

## correlation between population and natural gas consumption without outlier (Kadikoy)
cor(data2$population,data2$consumption,use="complete.obs")
# Here, correlation coefficient tells us that nearly 83,7% of the variation in the natural gas consumption can be attributed to the population. 
## fitted linear model without outlier (Kadikoy)
lm(consumption~population, data = data2)
# The linear model is estimated as ŷ =  37462148.2 + 288.6x. This implies that on average, for every 1 person increase in population, natural gas consumption is estimated to increase by 288.6 m³ /year.

### To sum up, we could observe the effect of outlier to the model. Regression coefficients of the data with outlier was higher than without. The reason can be due to higher consumption of natural gas than the average.
### Correlation coefficient of data with oulier was less (0.77) than without (0.84). This change means that correlation of population and natural gas consumption from data with outlier is weaker or more spreaded than other.
### Thus, simple regression model without outlier gives more reliable estimation of natural gas consumption for regions in Istanbul. 
### There can be many reasons of the oulier. Since there are more rich people live in Kadikoy, the rate of price increase can be higher or people heat their whole house more than ordinary people do. 
### Also, there can be a problem in their natural gas delivery pipe system. For these reasons, it will be better if further researchs are conducted in Kadikoy.
### Reference []

data3 <- data.frame("Year" = c(2016,2017,2018,2019), "Price" = c(0.992, 0.947, 1.059, 1.291))
plot_ly(data3, x = ~Year, y = ~Price, name = 'Price', type = 'scatter', mode = 'lines')










