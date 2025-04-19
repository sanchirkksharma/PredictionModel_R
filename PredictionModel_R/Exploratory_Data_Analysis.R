
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)
library(reshape2)

str(airbnb_full)
summary(airbnb_full)

colSums(is.na(airbnb_full))

table(airbnb_full$Deal)
ggplot(airbnb_full, aes(x = Deal, fill = Deal)) +
  geom_bar() +
  theme_minimal() +
  ggtitle("Distribution of Target Variable: Deal")


numeric_vars <- c("price", "footage", "avgreview", "floor", "value_score", 
                  "log_price", "log_footage", "price_sqft", "review_weight")

for (var in numeric_vars) {
  print(
    ggplot(airbnb_full, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      theme_minimal() +
      ggtitle(paste("Histogram of", var))
  )
}

for (var in numeric_vars) {
  print(
    ggplot(airbnb_full, aes_string(x = "Deal", y = var, fill = "Deal")) +
      geom_boxplot() +
      theme_minimal() +
      ggtitle(paste("Boxplot of", var, "by Deal"))
  )
}



cor_data <- airbnb_full[, numeric_vars]
cor_matrix <- cor(cor_data, use = "complete.obs")

corrplot(cor_matrix, method = "color", tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7)


airbnb_full %>%
  group_by(borough) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    avg_footage = mean(footage, na.rm = TRUE),
    avg_review = mean(avgreview, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(avg_price))

ggplot(airbnb_full, aes(x = footage, y = price, color = Deal)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  ggtitle("Price vs. Footage colored by Deal")

ggplot(airbnb_full, aes(x = avgreview, y = price, color = Deal)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  ggtitle("Price vs. Review colored by Deal")


ggsave("price_vs_review.png")

