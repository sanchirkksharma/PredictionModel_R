# Load necessary library

library(rpart)

# Read the data
airbnb_data <- read.csv("airbnbTrain.csv")
boroughs_data <- read.csv("Boroughs.csv")


head(airbnb_data)
head(boroughs_data)
# Merge data with borough information
airbnb_full <- merge(airbnb_data, boroughs_data, by.x="neighbourhood", by.y="neighborhood", all.x=TRUE)

# Create New Features from Raw Data
airbnb_full$price_per_foot <- airbnb_full$price / airbnb_full$footage
airbnb_full$value_score <- airbnb_full$footage * airbnb_full$avgreview / airbnb_full$price
airbnb_full$is_manhattan <- airbnb_full$borough == "Manhattan"
airbnb_full$high_floor <- airbnb_full$floor > 10
airbnb_full$price_review_norm <- airbnb_full$price / (airbnb_full$avgreview + 1)
airbnb_full$log_price <- log(airbnb_full$price)
airbnb_full$log_footage <- log(airbnb_full$footage)
airbnb_full$price_sqft <- airbnb_full$price / sqrt(airbnb_full$footage)
airbnb_full$review_weight <- (airbnb_full$avgreview)^2  

# Borough-specific metrics
borough_avg_price <- tapply(airbnb_full$price, airbnb_full$borough, mean)
borough_avg_footage <- tapply(airbnb_full$footage, airbnb_full$borough, mean)
borough_avg_review <- tapply(airbnb_full$avgreview, airbnb_full$borough, mean)

# Add relative metrics
airbnb_full$price_vs_borough <- airbnb_full$price / borough_avg_price[airbnb_full$borough]
airbnb_full$footage_vs_borough <- airbnb_full$footage / borough_avg_footage[airbnb_full$borough]
airbnb_full$review_vs_borough <- airbnb_full$avgreview / borough_avg_review[airbnb_full$borough]

# More advanced price categories with more detailed breakpoints
airbnb_full$price_category <- cut(airbnb_full$price, 
                                  breaks=c(0, 75, 100, 125, 150, 175, 200, 250, 300, 400, 1000),
                                  labels=c("ultra_budget", "very_budget", "budget", "economy", "moderate", 
                                           "mid_range", "premium", "luxury", "ultra_luxury", "super_luxury"))

# More detailed footage categories
airbnb_full$footage_category <- cut(airbnb_full$footage, 
                                    breaks=c(0, 150, 200, 250, 300, 350, 400, 500, 600, Inf),
                                    labels=c("tiny", "very_small", "small", "medium", "mid_large", 
                                             "large", "very_large", "huge", "massive"))

# Enhanced review score categories
airbnb_full$review_category <- cut(airbnb_full$avgreview,
                                   breaks=c(0, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
                                   labels=c("terrible", "very_poor", "poor", "below_average", 
                                            "average", "good", "very_good", "excellent"))

# More detailed floor categories
airbnb_full$floor_category <- cut(airbnb_full$floor,
                                  breaks=c(-Inf, 1, 3, 5, 8, 10, 15, 20, Inf),
                                  labels=c("ground", "very_low", "low", "mid_low", "mid", 
                                           "mid_high", "high", "penthouse"))

# First tree model with enhanced original features
tree1 <- rpart(Deal ~ price + footage + avgreview + floor + borough + 
                 price_per_foot + is_manhattan + high_floor + value_score +
                 log_price + log_footage + price_sqft + review_weight +
                 price_vs_borough + footage_vs_borough + review_vs_borough,
               data=airbnb_full, 
               method="class",
               control=rpart.control(minsplit=5, cp=0.0008, maxdepth=30))

# Create sophisticated rule-based predictions
rule_decision <- rep('Neutral', nrow(airbnb_full))

# Calculate additional metrics for rules
value_quantiles <- quantile(airbnb_full$value_score, probs=c(0.25, 0.75))
price_quantiles <- quantile(airbnb_full$price, probs=c(0.1, 0.25, 0.75, 0.9))
footage_quantiles <- quantile(airbnb_full$footage, probs=c(0.1, 0.25, 0.75, 0.9))
review_quantiles <- quantile(airbnb_full$avgreview, probs=c(0.25, 0.75))


# Rules
# Rule 1: Exceptional value properties
rule_decision[airbnb_full$value_score > value_quantiles[2] & 
                airbnb_full$price_vs_borough < 0.9 &
                airbnb_full$footage_vs_borough > 1.1 &
                airbnb_full$review_vs_borough > 1.1] <- 'Good'

# Rule 2: Poor value luxury properties
rule_decision[airbnb_full$price > price_quantiles[4] & 
                airbnb_full$value_score < value_quantiles[1] &
                airbnb_full$review_weight < median(airbnb_full$review_weight)] <- 'Bad'

# Rule 3: Good value in prime locations
rule_decision[airbnb_full$is_manhattan == TRUE &
                airbnb_full$price < price_quantiles[2] &
                airbnb_full$footage > footage_quantiles[3] &
                airbnb_full$avgreview > review_quantiles[2]] <- 'Good'

# Rule 4: Overpriced small units
rule_decision[airbnb_full$footage < footage_quantiles[1] &
                airbnb_full$price > price_quantiles[3] &
                airbnb_full$review_vs_borough < 0.9] <- 'Bad'

# Rule 5: High value outer borough properties
rule_decision[airbnb_full$is_manhattan == FALSE &
                airbnb_full$value_score > value_quantiles[2] &
                airbnb_full$review_weight > median(airbnb_full$review_weight) &
                airbnb_full$footage_vs_borough > 1.2] <- 'Good'

# Rule 6: Poor performing expensive units
rule_decision[airbnb_full$price_vs_borough > 1.3 &
                airbnb_full$review_vs_borough < 0.8 &
                airbnb_full$footage_vs_borough < 1.1] <- 'Bad'

# Second tree model with enhanced categorical features
tree2 <- rpart(Deal ~ price_category + footage_category + review_category + 
                 floor_category + borough + price_vs_borough + footage_vs_borough + 
                 review_vs_borough + value_score,
               data=airbnb_full,
               method="class",
               control=rpart.control(minsplit=5, cp=0.0008, maxdepth=30))

# Add model predictions to dataset
airbnb_full$model1 <- predict(tree1, airbnb_full, type="class")
airbnb_full$model2 <- predict(tree2, airbnb_full, type="class")
airbnb_full$rule_model <- rule_decision

# Final combined model with weighted features
combined_tree <- rpart(Deal ~ model1 + model2 + rule_model + 
                         price + footage + avgreview + floor + borough +
                         price_per_foot + is_manhattan + high_floor + value_score +
                         price_category + footage_category + review_category + 
                         floor_category + price_review_norm + log_price + log_footage +
                         price_sqft + review_weight + price_vs_borough + 
                         footage_vs_borough + review_vs_borough,
                       data=airbnb_full,
                       method="class",
                       control=rpart.control(minsplit=4, cp=0.0004, maxdepth=30))




# Split 1
train_size <- floor(0.7 * nrow(airbnb_full))
train_indices1 <- sample(1:nrow(airbnb_full), train_size)
cv_train1 <- airbnb_full[train_indices1,]
cv_test1 <- airbnb_full[-train_indices1,]

cv_tree1_1 <- rpart(Deal ~ price + footage + avgreview + floor + borough + 
                      price_per_foot + is_manhattan + high_floor + value_score +
                      log_price + log_footage + price_sqft + review_weight +
                      price_vs_borough + footage_vs_borough + review_vs_borough,
                    data=cv_train1, method="class",
                    control=rpart.control(minsplit=5, cp=0.0008, maxdepth=30))

cv_tree2_1 <- rpart(Deal ~ price_category + footage_category + review_category + 
                      floor_category + borough + price_vs_borough + footage_vs_borough + 
                      review_vs_borough + value_score,
                    data=cv_train1, method="class",
                    control=rpart.control(minsplit=5, cp=0.0008, maxdepth=30))

cv_rule1 <- rule_decision[train_indices1]
cv_test_rule1 <- rule_decision[-train_indices1]

cv_train1$model1 <- predict(cv_tree1_1, cv_train1, type="class")
cv_train1$model2 <- predict(cv_tree2_1, cv_train1, type="class")
cv_train1$rule_model <- cv_rule1

cv_test1$model1 <- predict(cv_tree1_1, cv_test1, type="class")
cv_test1$model2 <- predict(cv_tree2_1, cv_test1, type="class")
cv_test1$rule_model <- cv_test_rule1

cv_combined1 <- rpart(Deal ~ model1 + model2 + rule_model + 
                        price + footage + avgreview + floor + borough +
                        price_per_foot + is_manhattan + high_floor + value_score +
                        price_category + footage_category + review_category + 
                        floor_category + price_review_norm + log_price + log_footage +
                        price_sqft + review_weight + price_vs_borough + 
                        footage_vs_borough + review_vs_borough,
                      data=cv_train1,
                      method="class",
                      control=rpart.control(minsplit=4, cp=0.0004, maxdepth=30))

cv_pred1 <- predict(cv_combined1, cv_test1, type="class")
cv_accuracy1 <- mean(cv_test1$Deal == cv_pred1)

# Split 2 
train_indices2 <- sample(1:nrow(airbnb_full), train_size)
cv_train2 <- airbnb_full[train_indices2,]
cv_test2 <- airbnb_full[-train_indices2,]

cv_tree1_2 <- rpart(Deal ~ price + footage + avgreview + floor + borough + 
                      price_per_foot + is_manhattan + high_floor + value_score +
                      log_price + log_footage + price_sqft + review_weight +
                      price_vs_borough + footage_vs_borough + review_vs_borough,
                    data=cv_train2, method="class",
                    control=rpart.control(minsplit=5, cp=0.0008, maxdepth=30))

cv_tree2_2 <- rpart(Deal ~ price_category + footage_category + review_category + 
                      floor_category + borough + price_vs_borough + footage_vs_borough + 
                      review_vs_borough + value_score,
                    data=cv_train2, method="class",
                    control=rpart.control(minsplit=5, cp=0.0008, maxdepth=30))

cv_rule2 <- rule_decision[train_indices2]
cv_test_rule2 <- rule_decision[-train_indices2]

cv_train2$model1 <- predict(cv_tree1_2, cv_train2, type="class")
cv_train2$model2 <- predict(cv_tree2_2, cv_train2, type="class")
cv_train2$rule_model <- cv_rule2

cv_test2$model1 <- predict(cv_tree1_2, cv_test2, type="class")
cv_test2$model2 <- predict(cv_tree2_2, cv_test2, type="class")
cv_test2$rule_model <- cv_test_rule2

cv_combined2 <- rpart(Deal ~ model1 + model2 + rule_model + 
                        price + footage + avgreview + floor + borough +
                        price_per_foot + is_manhattan + high_floor + value_score +
                        price_category + footage_category + review_category + 
                        floor_category + price_review_norm + log_price + log_footage +
                        price_sqft + review_weight + price_vs_borough + 
                        footage_vs_borough + review_vs_borough,
                      data=cv_train2,
                      method="class",
                      control=rpart.control(minsplit=4, cp=0.0004, maxdepth=30))

cv_pred2 <- predict(cv_combined2, cv_test2, type="class")
cv_accuracy2 <- mean(cv_test2$Deal == cv_pred2)

# Print cross-validation results
cat('\nCross-validation accuracies:\n')
cv_accuracies <- c(cv_accuracy1, cv_accuracy2)
print(cv_accuracies)
cat('\nMean cross-validation accuracy:', mean(cv_accuracies), '\n')
cat('Standard deviation of cross-validation accuracy:', sd(cv_accuracies), '\n')


# Read the test data and submission template
test_data <- read.csv("airbnbTestStudents.csv")
submission <- read.csv("submission_airbnb.csv")

# Merge test data with borough information
test_full <- merge(test_data, boroughs_data, by.x="neighbourhood", by.y="neighborhood", all.x=TRUE)

# Apply same new features from raw data to test data
test_full$price_per_foot <- test_full$price / test_full$footage
test_full$value_score <- test_full$footage * test_full$avgreview / test_full$price
test_full$is_manhattan <- test_full$borough == "Manhattan"
test_full$high_floor <- test_full$floor > 10
test_full$price_review_norm <- test_full$price / (test_full$avgreview + 1)
test_full$log_price <- log(test_full$price)
test_full$log_footage <- log(test_full$footage)
test_full$price_sqft <- test_full$price / sqrt(test_full$footage)
test_full$review_weight <- (test_full$avgreview)^2

# Apply borough-specific metrics
test_full$price_vs_borough <- test_full$price / borough_avg_price[test_full$borough]
test_full$footage_vs_borough <- test_full$footage / borough_avg_footage[test_full$borough]
test_full$review_vs_borough <- test_full$avgreview / borough_avg_review[test_full$borough]

# Add categorical features to test data
test_full$price_category <- cut(test_full$price, 
                                breaks=c(0, 75, 100, 125, 150, 175, 200, 250, 300, 400, 1000),
                                labels=c("ultra_budget", "very_budget", "budget", "economy", "moderate", 
                                         "mid_range", "premium", "luxury", "ultra_luxury", "super_luxury"))

test_full$footage_category <- cut(test_full$footage, 
                                  breaks=c(0, 150, 200, 250, 300, 350, 400, 500, 600, Inf),
                                  labels=c("tiny", "very_small", "small", "medium", "mid_large", 
                                           "large", "very_large", "huge", "massive"))

test_full$review_category <- cut(test_full$avgreview,
                                 breaks=c(0, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
                                 labels=c("terrible", "very_poor", "poor", "below_average", 
                                          "average", "good", "very_good", "excellent"))

test_full$floor_category <- cut(test_full$floor,
                                breaks=c(-Inf, 1, 3, 5, 8, 10, 15, 20, Inf),
                                labels=c("ground", "very_low", "low", "mid_low", "mid", 
                                         "mid_high", "high", "penthouse"))

# Apply rules to test data
test_rule_decision <- rep('Neutral', nrow(test_full))

# Apply the rules
test_rule_decision[test_full$value_score > value_quantiles[2] & 
                     test_full$price_vs_borough < 0.9 &
                     test_full$footage_vs_borough > 1.1 &
                     test_full$review_vs_borough > 1.1] <- 'Good'

test_rule_decision[test_full$price > price_quantiles[4] & 
                     test_full$value_score < value_quantiles[1] &
                     test_full$review_weight < median(airbnb_full$review_weight)] <- 'Bad'

test_rule_decision[test_full$is_manhattan == TRUE &
                     test_full$price < price_quantiles[2] &
                     test_full$footage > footage_quantiles[3] &
                     test_full$avgreview > review_quantiles[2]] <- 'Good'

test_rule_decision[test_full$footage < footage_quantiles[1] &
                     test_full$price > price_quantiles[3] &
                     test_full$review_vs_borough < 0.9] <- 'Bad'

test_rule_decision[test_full$is_manhattan == FALSE &
                     test_full$value_score > value_quantiles[2] &
                     test_full$review_weight > median(airbnb_full$review_weight) &
                     test_full$footage_vs_borough > 1.2] <- 'Good'

test_rule_decision[test_full$price_vs_borough > 1.3 &
                     test_full$review_vs_borough < 0.8 &
                     test_full$footage_vs_borough < 1.1] <- 'Bad'

# Add model predictions to test data
test_full$model1 <- predict(tree1, test_full, type="class")
test_full$model2 <- predict(tree2, test_full, type="class")
test_full$rule_model <- test_rule_decision


# Make final predictions using combined model
final_predictions <- predict(combined_tree, test_full, type="class")


# Update the Deal column in the submission file
submission$Deal <- final_predictions



# Save updated submission file
write.csv(submission, "submission_airbnb.csv", row.names = FALSE)


saveRDS(borough_avg_price, "borough_avg_price.rds")
saveRDS(borough_avg_footage, "borough_avg_footage.rds")
saveRDS(borough_avg_review, "borough_avg_review.rds")

saveRDS(value_quantiles, "value_quantiles.rds")
saveRDS(price_quantiles, "price_quantiles.rds")
saveRDS(footage_quantiles, "footage_quantiles.rds")
saveRDS(review_quantiles, "review_quantiles.rds")
saveRDS(median(airbnb_full$review_weight), "review_weight_median.rds")


# Saving the trained model
saveRDS(tree1, "tree1.rds")
saveRDS(tree2, "tree2.rds")
saveRDS(combined_tree, "combined_tree.rds")



