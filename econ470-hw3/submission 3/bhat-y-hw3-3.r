
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
group_by(Year) %>% filter(Year<1986, Year>1970) %>%
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
price_diff <- cig.data %>%
  group_by(state) %>%
  summarise(price_increase = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE))
top_states <- price_diff %>%
  arrange(desc(price_increase)) %>% 
  slice(1:5)

top5data <- cig.data %>%
  semi_join(top_states, by = "state") %>%
  filter(Year >= 1970 & Year <= 2018)

average_sales <- top5data %>%
  group_by(state, Year) %>%
  summarise(Avg_Sales = mean(sales_per_capita, na.rm = TRUE), .groups = 'drop')  # Use .groups = 'drop' to avoid the grouping message

sales_plot <- ggplot(average_sales, aes(x = Year, y = Avg_Sales, color = state)) +
  geom_line() +
  labs(x = "Year", y = "Packs Per Capita", title = "Average Packs Sold Per Capita (1970-2018)", color = "State") +
  theme_minimal()

print(sales_plot)
#Q4
price_diff <- cig.data %>%
  group_by(state) %>%
  summarise(price_increase = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE), .groups = 'drop')

bottom_states <- price_diff %>%
  arrange(price_increase) %>%
  slice(1:5)

bottom_states <- as.data.frame(bottom_states)

bottom5data <- cig.data %>%
  semi_join(bottom_states, by = "state") %>%
  filter(Year >= 1970 & Year <= 2018)

average_sales_bottom <- bottom5data %>%
  group_by(state, Year) %>%
  summarise(Avg_Sales = mean(sales_per_capita, na.rm = TRUE))

sales_plot_bottom <- ggplot(average_sales_bottom, aes(x = Year, y = Avg_Sales, color = state)) +
  geom_line() +
  labs(x = "Year", 
       y = "Packs Per Capita",
       title = "Average Packs Sold Per Capita (1970-2018) for Bottom 5 States",
       color = "State") +
  theme_minimal()

print(sales_plot_bottom)
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
price_elasticity_model8 <- cig.data %>%
  filter(Year >= 1970 & Year <= 1990)
first_stage_model <- lm(ln_price_2012 ~ ln_tax_2012, data=price_elasticity_model8)
price_elasticity_model8$predicted_price <- predict(first_stage_model)
second_stage_model <- lm(ln_sales ~ predicted_price, data=price_elasticity_model8)
coef(first_stage_model)
coef(second_stage_model)
summary(first_stage_model)
summary(second_stage_model)
#q9
regdata2 <- cig.data %>%
filter(Year >= 1991 & Year <= 2015)

pem_2 <- lm(ln_sales ~ ln_price_2012, data = regdata2)
summary(pem_2)

summary(feols(ln_sales ~ 1 | ln_tax_2012, 
             data= regdata2))

first_stage2 <- lm(ln_price_2012 ~ ln_tax_2012, data = regdata2)
summary(first_stage2)
regdata2$predicted_price2 <- predict(first_stage2)
second_stage2 <- lm(ln_sales ~ predicted_price2, data=regdata2)
summary(second_stage2)

save.image("/Users/yaseenbhat/Downloads/econ470-hw3/submission 3/hwk3_workspace.Rdata")