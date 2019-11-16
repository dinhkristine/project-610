
#### packages ----

library(tidyverse)
library(magrittr)
library(zipcode)
library(lubridate)
library(leaps)



#### load data ---- 

data <- read.csv("data/house_data.csv")

data <- quivR::make_names(data)

data(zipcode)


#### select variables we want to explore ---- 

data %<>% dplyr::select(price, 
                 bathrooms, 
                 bedrooms, 
                 sqft_living, 
                 floors, 
                 view, 
                 condition, 
                 grade, 
                 yr_built, 
                 zipcode)


#### Summary table of continuous variables ####

data_cont <- data %>% dplyr::select(-zipcode)

tmp <- do.call(data.frame, 
               list(mean = apply(data_cont, 2, mean),
                    sd = apply(data_cont, 2, sd),
                    median = apply(data_cont, 2, median),
                    min = apply(data_cont, 2, min),
                    max = apply(data_cont, 2, max),
                    n = apply(data_cont, 2, length)))

tmp %<>% round(.,3)


#### Clean up data ---- 

data %<>% 
  filter(bedrooms <= 10)

data$zipcode %<>% as.character()

real_zip <- as.vector(zipcode$zip)

data %<>% 
  mutate(view = case_when(
    .$view == 0 ~ 0, 
    TRUE > 0 ~ 1),
    zip3 = str_sub(.$zipcode, 1, 3))

data$yr_built <- cut(data$yr_built, breaks = seq(1900,2020, 10), include.lowest = TRUE, dig.lab = 4)

data %<>% dplyr::select(-zipcode)

data$price <- log(data$price)

data_cont <- data %>% dplyr::select(-zip3, -yr_built, -id)



#### Correlation Matrix ---- 

cor(data_cont)


#### Assign class ---- 

data$yr_built %<>% as.factor()

data$zip3 %<>% as.factor()



#### plot for variables #### 

AvgPrice <- function(group){
  avg <- data %>% 
    group_by(.dots = group) %>% 
    summarise(avg_price = mean(price))
  
  p <- ggplot(avg, aes_string(x = group, y = "avg_price", group = 1)) + 
    geom_point() + 
    geom_line()
  
  return(p)
}



AvgPrice("bathrooms")            # bathroom 

AvgPrice("bedrooms")

AvgPrice("floors")

AvgPrice("view")

AvgPrice("condition")

AvgPrice("grade")

AvgPrice("yr_built")

AvgPrice("zip3")


#### Stepwise Regression

library(MASS)
fit <- lm(price ~ . + zip3*condition*grade, data = data)

fit2 <- lm(price ~ (.)^2, data = data)

step <- stepAIC(fit, direction="both")

step$anova # display results

#### save csv ---- 

write.csv(data, "data/house_data1.csv")








