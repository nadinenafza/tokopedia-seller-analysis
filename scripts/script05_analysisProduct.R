library(tidyverse)
options(scipen = 999)   

MIN_PRODUCTS <- 50      

products <- readRDS("data/processed/products.rds") %>%
  filter(!is.na(main_category), main_category != "")

dir.create("images", showWarnings = FALSE)

products <- products %>%
  mutate(
    on_discount = !is.na(discount_pct),
    store_type  = if_else(is_official, "Official Store", "Non-official")
  )


#Summary per category
category_summary <- products %>%
  group_by(main_category) %>%
  summarise(
    n_products   = n(),
    median_price = median(price, na.rm = TRUE),
    median_sold  = median(count_sold, na.rm = TRUE),
    mean_rating  = mean(rating_average, na.rm = TRUE),
    pct_official = mean(is_official, na.rm = TRUE),
    pct_discount = mean(on_discount)
  ) %>%
  filter(n_products >= MIN_PRODUCTS) %>%
  arrange(desc(median_sold))
print(category_summary, n = 25)


# Official store vs non-official
products %>%
  group_by(store_type) %>%
  summarise(
    n           = n(),
    median_sold = median(count_sold, na.rm = TRUE),
    mean_rating = mean(rating_average, na.rm = TRUE)
  )


# Product Discount vs Not discount
products %>%
  group_by(on_discount) %>%
  summarise(
    n           = n(),
    median_sold = median(count_sold, na.rm = TRUE),
    mean_rating = mean(rating_average, na.rm = TRUE)
  )


#Graphic median per category
p <- category_summary %>%
  ggplot(aes(reorder(main_category, median_sold), median_sold)) +
  geom_col() +
  coord_flip() +
  labs(title = "Median units sold per product, by category",
       x = NULL, y = "Median units sold")

ggsave("images/median_sold_by_category.png", p, width = 8, height = 6)