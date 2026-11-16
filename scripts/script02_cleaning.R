#Cleaning the data product and broke down review to be one line per review

library(tidyverse)
library(reticulate)

ast <- import("ast")

df <- read_csv("data/raw/tokopedia_products_with_review.csv", n_max = 1000)


# The table product
products <- df %>%
  select(-c(review_id,variant_name,message, review_rating, review_time,
            review_timestamp, review_response, review_like, bad_rating_reason))%>%

  mutate(
    original_price = as.numeric(na_if(discounted_price, "None")),
    discount_pct = (original_price - price)/ original_price
  ) %>%
  
  separate(category, into = c("main_category","sub_category"),
           sep = "\\|", extra = "merge", fill = "right")

  
# Table review (one line per review)
parse_col <- function(x) {
  lapply(x, function(s) if (is.na(s)) NULL else unlist(ast$literal_eval(s)))
}

reviews <- df %>%
  select(product_id, review_id, review_rating, review_time, review_like, 
         bad_rating_reason,review_response, message)%>%
  mutate(across(-product_id, parse_col)) %>%
  unnest(cols= c(review_id,review_rating,review_time, review_like,
                 bad_rating_reason, review_response, message)) %>%
  mutate(review_date = as_datetime(as.numeric(review_time)))

# save the results
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(products, "data/processed/products.rds")
saveRDS(reviews,  "data/processed/reviews.rds")

# checking
summary(products$discount_pct)
nrow(reviews)
count(reviews, bad_rating_reason, sort = TRUE)
