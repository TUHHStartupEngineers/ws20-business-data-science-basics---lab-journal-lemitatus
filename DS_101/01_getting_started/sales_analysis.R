# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
#  library(tibble)    --> is a modern re-imagining of the data frame/ data objects
#  library(readr)     --> provides a fast and friendly way to read rectangular data like csv
#  library(dplyr)     --> provides a grammar of data manipulation
#  library(magrittr)  --> offers a set of operators which make your code more readable (pipe operator)
#  library(tidyr)     --> provides a set of functions that help you get to tidy data
#  library(stringr)   --> provides a cohesive set of functions designed to make working with strings as easy as possible
#  library(ggplot2)   --> graphics
library(readxl)

# 2.0 Importing Files ----
bikes_tbl <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")


# 3.0 Examining Data ----
glimpse(bikes_tbl)
glimpse(orderlines_tbl)

# 4.0 Joining Data ----
tbl1 <- orderlines_tbl %>% 
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
glimpse(tbl1)

# 5.0 Wrangling Data ----
tbl1$category # look at the category column
tbl1 %>%
  select(category) %>%
  filter(str_detect(category,"^Mountain")) %>%
  unique()   # returns vector/dataframe with duplicate elements removed

tbl2 <- tbl1 %>%
  separate(col = category,
           into = c("cat.1", "cat.2", "cat.3"),
           sep = " - ") %>%
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%     #removes all columns with .id
  bind_cols(tbl1 %>% select(order.id)) %>%      # reloads the column order.id
  mutate(total.price = price*quantity) %>%
  select(order.id, contains("order"), contains("model"), price, quantity, total.price, everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))
glimpse(tbl2)

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate
library(lubridate)
tbl3 <- tbl2 %>%
  select(order_date, price, quantity, total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " Euro"))
tbl3

# Step 2 - Visualize
tbl3 %>%
  ggplot(aes(x = year, y = sales)) +
  # Geometry
  geom_col(fill = "#2DC6D6") +  #geom_col is a bar plot
  geom_label(aes(label = sales_text)) +
  geom_smooth(method = "lm", se = FALSE) +  #adding trend line
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " Euro")) +
  labs(
    title = "Revenue by year",
    subtitle = "Upward Trend",
    x = "",
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
tbl4 <- tbl2 %>%
  select(order_date, price, quantity, total_price, cat_1:cat_3) %>%
  mutate(year = year(order_date)) %>%
  group_by(year, cat_1) %>%
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " Euro"))
glimpse(tbl4)
tbl4
  

# Step 2 - Visualize



# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----