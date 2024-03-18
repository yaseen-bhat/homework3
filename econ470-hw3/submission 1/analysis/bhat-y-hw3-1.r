
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata,fixest)
cig.data <- read_rds("data/output/TaxBurden_Data.rds")

cig.data <- cig.data %>% group_by(state) %>% arrange(state,Year) %>%
    mutate(tax_change= tax_state - lag(tax_state),
    tax_change_d = ifelse(tax_change==0,0,1),
    price_cpi_2022 = cost_per_pack*(cpi_2012/index),
    total_tax_cpi_2022=tax_dollar*(cpi_2012/index),
    ln_tax_2012=log(total_tax_cpi_2022),
    ln_sales=log(sales_per_capita),
    ln_price_2012 = log(price_cpi_2022))

#Question 1
# calc n plot
tax_changes_by_year <- cig.data %>%
  filter(Year <= 1985) %>%
  group_by(Year) %>%
  summarise(TaxChangeCount = sum(tax_change_d, na.rm = TRUE))

  tax_change_plot <- ggplot(tax_changes_by_year, aes(x = Year, y = TaxChangeCount)) +
  geom_col(fill = "cornflowerblue") +
  labs(title = "Annual Count of States with Cigarette Tax Changes (up to 1985)",
       x = "Year",
       y = "Count of States with Tax Changes") +
  theme_minimal()


print(tax_change_plot)

# Question 2
data.2018 <- cig.data %>% 
filter(Year <= 2018)%>%
group_by(Year)%>%
summarise(mean_price = mean(price_cpi_2022, na.rm = TRUE), mean_tax = mean(total_tax_cpi_2022, na.rm = TRUE))

data.2018_plot <- ggplot(data.2018, aes(x = Year))+
geom_line(aes(y = mean_price), color = "black")+
geom_line(aes(y = mean_tax), color = "yellow")
labs(title = "Average Tax and Price of Cigarettes from 1970 to 2018)",
       x = "Year", y = "Value in 2012 dollars") +
  theme_classic()

print(data.2018)

#Q3
price_changes <- cig.data %>%
  filter(Year %in% c(1970, 2018)) %>%
  spread(key = Year, value = price_cpi_2022) %>%
  mutate(PriceIncrease = `2018` - `1970`) %>%
  select(state, PriceIncrease) %>%
  arrange(desc(PriceIncrease)) %>%
  slice_head(n = 5)

top_states <- price_changes$state

# calc n plot
avg_sales_top_states <- cig.data %>%
  filter(state %in% top_states) %>%
  group_by(Year) %>%
  summarise(MeanSales = mean(sales_per_capita, na.rm = TRUE))

avg_sales_plot <- ggplot(avg_sales_top_states, aes(x = Year, y = MeanSales)) +
  geom_line() +
  labs(title = "Average Number of Packs Sold Per Capita (Top 5 States by Price Increase)", 
       x = "Year", 
       y = "Average Packs Sold Per Capita")


print(avg_sales_plot)

#q4
bottom_states <- price_changes %>%
  arrange(PriceIncrease) %>%
  slice_head(n = 5) %>%
  .$state
# calc n plot
avg_sales_bottom_states <- cig.data %>%
  filter(state %in% bottom_states, Year <= 2018) %>%
  group_by(Year) %>%
  summarise(MeanSales = mean(sales_per_capita, na.rm = TRUE))

avg_sales_bottom_plot <- ggplot(avg_sales_bottom_states, aes(x = Year, y = MeanSales)) +
  geom_line() +
  labs(title = "Average Number of Packs Sold Per Capita (States with Lowest Price Increase)", 
       x = "Year", 
       y = "Average Packs Sold Per Capita")

print(avg_sales_bottom_plot)

#q6

price_elasticity_model <- lm(ln_sales ~ ln_price_2012, data = filter(cig.data, Year >= 1970 & Year <= 1990))

model_summary <- summary(price_elasticity_model)
print(model_summary)

#q7
regdata <- cig.data %>%
  filter(Year >= 1970 & Year <= 1990)
summary(feols(ln_sales ~ 1 | ln_price_2012 ~ ln_tax_2012, 
             data=regdata))
#q8
price_elasticity_model <- cig.data %>%
  filter(Year >= 1970 & Year <= 1990)

first_stage_model <- lm(ln_price_2012 ~ ln_tax_2012, data=price_elasticity_model)

price_elasticity_model$pricehat <- predict(first_stage_model)

second_stage_model <- lm(ln_sales ~ pricehat, data=price_elasticity_model)

summary(first_stage_model)
summary(second_stage_model)

reduced_form_model <- lm(ln_sales ~ ln_tax_2012, data=price_elasticity_model)
summary(reduced_form_model)
#q9
regdata2 <- cig.data %>%
filter(Year >= 1991 & Year <= 2015)

first_stage_model2 <- lm(ln_sales ~ ln_price_2012, data = regdata2)
summary(first_stage_model2)

summary(feols(ln_sales ~ 1 | ln_tax_2012, 
             data= regdata2))

first_stage2 <- lm(ln_price_2012 ~ ln_tax_2012, data = regdata2)
summary(first_stage2)

reduced_form_model2 <- lm(ln_sales ~ ln_tax_2012, data = regdata2)
summary(reduced_form_model2)
