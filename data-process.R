#### packages ----

library(tidyverse)
library(magrittr)
library(MASS)


#### load data ---- 

data <- read.csv("C:/Users/kdinh/Desktop/kc_house_data.csv")


#### select variables we want to explore ---- 


data %<>% dplyr::select(price, 
                        bathrooms,  
                        sqft_living, 
                        floors, 
                        view, 
                        condition, 
                        grade)

data %<>% 
  mutate(view = case_when(
    .$view == 0 ~ 0, 
    TRUE > 0 ~ 1))

data$logprice <- log(data$price)

data$sqft_living1000 <- data$sqft_living/1000

#### Correlation Matrix ---- 

cor(data)


#### plot for variables #### 

AvgPrice <- function(group){
  avg <- data %>% 
    group_by(.dots = group) %>% 
    summarise(avg_price = mean(price))
  
  p <- ggplot(avg, aes_string(x = group, y = "avg_price", group = 1)) + 
    geom_point() 
  
  return(p)
}



AvgPrice("bathrooms")            # bathroom 

AvgPrice("bedrooms")

AvgPrice("floors")

AvgPrice("view")

AvgPrice("condition")

AvgPrice("grade")


#### Stepwise Regression

fit <- lm(price ~ ., data = data)

fit2 <- lm(price ~ (.)^2, data = data)

step <- stepAIC(fit, direction="both")

step$anova # display results


#### save csv ---- 

write.csv(data, "C:/Users/kdinh/Desktop/kc_house_data1.csv")






