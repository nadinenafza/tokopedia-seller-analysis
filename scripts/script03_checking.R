# check is reason only appear in low rating
reviews %>%
  mutate(has_reason= bad_rating_reason != "") %>%
  count(review_rating, has_reason)

#how many review per product (is there maximum threshold)
reviews %>% count(product_id) %>% summary()

#range review date
range(reviews$review_date, na.rm= TRUE)

#seperate reason multi-lable into one line per line 
reasons <- reviews %>%
  filter(bad_rating_reason != "") %>%
  mutate(reason = str_remove(bad_rating_reason, "^Kendala:\\s*")) %>%
  separate_rows(reason, sep = ",\\s*")%>%
  count(reason, sort = TRUE)
reasons
