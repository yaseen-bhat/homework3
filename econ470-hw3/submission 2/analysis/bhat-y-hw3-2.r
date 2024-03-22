
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
yearlytaxchanges <- cig.data %>%
  filter(Year <= 1985) %>%
  group_by(Year) %>%
  summarise(TaxChange = mean(tax_change_d, na.rm = TRUE))

  taxchangeplot <- ggplot(yearlytaxchanges, aes(x = Year, y = TaxChange)) +
  geom_col(fill = "brown") +
  labs(title = "Annual Proportion of States with Cigarette Tax Changes (up to 1985)",
       x = "Year",
       y = "Proportion of States with Tax Changes") +
  theme_minimal()

print(taxchangeplot)

# Question 2
b.1970x2018 <- cig.data %>% 
  filter(Year >= 1970 & Year <= 2018) %>%
  group_by(Year) %>%
  summarise(mean_price = mean(price_cpi_2022, na.rm = TRUE), mean_tax = mean(total_tax_cpi_2022, na.rm = TRUE))

b.1970x2018plot <- ggplot(b.1970x2018, aes(x = Year)) +
  geom_line(aes(y = mean_price), color = "black", size = 1) +
  geom_line(aes(y = mean_tax), color = "red", size = 1) +
  labs(title = "Average Tax and Price of Cigarettes from 1970 to 2018",
       x = "Year", y = "Value in 2012 dollars") +
  theme_classic()

print(b.1970x2018plot)
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
cigpack <- cig.data %>%
  filter(Year %in% c(1970, 2018)) %>%
  pivot_wider(names_from = Year, values_from = price_cpi_2022) %>%
  mutate(PriceIncrease = `2018` - `1970`) %>%
  arrange(desc(PriceIncrease)) %>%
  slice_head(n = 5)

top_states <- cigpack$state

print(top_states)

avg_sales_per_state <- cig.data %>%
  filter(state %in% top_states) %>%
  group_by(state, Year) %>%
  summarise(MeanSales = mean(sales_per_capita, na.rm = TRUE), .groups = 'drop')

avg_sales_plot <- ggplot(avg_sales_per_state, aes(x = Year, y = MeanSales, color = state)) +
  geom_line() +
  labs(title = "Average Number of Packs Sold Per Capita in Top 5 States by Price Increase from 1970 to 2018", 
       x = "Year", 
       y = "Average Packs Sold Per Capita",
       color = "State") +
  theme_classic()

print(avg_sales_plot)

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