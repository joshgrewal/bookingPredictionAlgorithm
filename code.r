#code

library(dplyr)
library(tidyverse)

data <- read.csv("projectAirbnbDataset.csv")
df <- as.tibble(data)

air <- df %>% select(-id, -access, -amenities, -bed_type, -cancellation_policy, -description, -extra_people, -host_about, -host_location, -host_neighbourhood, -host_response_rate, -host_response_time, -host_since, -host_verifications, -house_rules, -interaction, -latitude, -longitude, -market, -neighborhood_overview, -neighbourhood, -notes, -property_type, -space, -room_type, -transit, -host_acceptance_rate, -monthly_price)

#CLEANING DATA we can see that theres typically 3 types on lines here, lines that fill in N/A, lines that alter the stings with $ and lines that convert values to numeric. 
air$accommodates[is.na(air$accommodates)] <- mean(air$accommodates, na.rm = TRUE) 

air$availability_30[is.na(air$availability_30)] <- mean(air$availability_30, na.rm = TRUE)
 
 air$availability_365[is.na(air$availability_365)] <- mean(air$availability_365, na.rm = TRUE)
 
 air$availability_60[is.na(air$availability_60)] <- mean(air$availability_60, na.rm = TRUE)
 
 air$availability_90[is.na(air$availability_90)] <- mean(air$availability_90, na.rm = TRUE)
 
 air$bathrooms[is.na(air$bathrooms)] <- mean(air$bathrooms, na.rm = TRUE)
 
 air$bedrooms[is.na(air$bedrooms)] <- mean(air$bedrooms, na.rm = TRUE)
 
 air$beds[is.na(air$beds)] <- mean(air$beds, na.rm = TRUE)
 
 air$city <- as.numeric(factor(air$city))
 
 View(air)
air$cleaning_fee <- gsub("\\$", "", air$cleaning_fee)
 
air$cleaning_fee[is.na(air$cleaning_fee)] <- mean(air$cleaning_fee, na.rm = TRUE)

air$cleaning_fee <- as.numeric(air$cleaning_fee)
 
 air$cleaning_fee[is.na(air$cleaning_fee)] <- mean(air$cleaning_fee, na.rm = TRUE)
 
 air$host_has_profile_pic <- as.numeric(air$host_has_profile_pic)
 
 air$host_identity_verified <- as.numeric(air$host_identity_verified)
 
 air$host_is_superhost <- as.numeric(air$host_is_superhost)
 
 
 air$guests_included[is.na(air$guests_included)] <- mean(air$guests_included, na.rm = TRUE)

 air$host_has_profile_pic[is.na(air$host_has_profile_pic)] <- mean(air$host_has_profile_pic, na.rm = TRUE)
 
 air$host_identity_verified[is.na(air$host_identity_verified)] <- mean(air$host_identity_verified, na.rm = TRUE)
 
 air$host_is_superhost[is.na(air$host_is_superhost)] <- mean(air$host_is_superhost, na.rm = TRUE)
 
 air$host_listings_count[is.na(air$host_listings_count)] <- mean(air$host_listings_count, na.rm = TRUE)
 
 air$instant_bookable <- as.numeric(air$instant_bookable)
 
 
 air$instant_bookable[is.na(air$instant_bookable)] <- mean(air$instant_bookable, na.rm = TRUE)
 
 air$is_business_travel_ready <- as.numeric(air$is_business_travel_ready)
 
 
 air$is_business_travel_ready[is.na(air$is_business_travel_ready)] <- mean(air$is_business_travel_ready, na.rm = TRUE)
 
 air$is_location_exact <- as.numeric(air$is_location_exact)
 
 
 air$is_location_exact[is.na(air$is_location_exact)] <- mean(air$is_location_exact, na.rm = TRUE)
 
 air$maximum_nights[is.na(air$maximum_nights)] <- mean(air$maximum_nights, na.rm = TRUE)
 
 air$minimum_nights[is.na(air$minimum_nights)] <- mean(air$minimum_nights, na.rm = TRUE)
 
 air$price <- gsub("\\$", "", air$price)

 air$price <- as.numeric(air$price)

 air$price[is.na(air$price)] <- mean(air$price, na.rm = TRUE)
 
air$require_guest_phone_verification <- as.numeric(air$require_guest_phone_verification)
 
 
 air$require_guest_phone_verification[is.na(air$require_guest_phone_verification)] <- mean(air$require_guest_phone_verification, na.rm = TRUE)
 
 air$require_guest_profile_picture <- as.numeric(air$require_guest_profile_picture)
 
 
 air$require_guest_profile_picture[is.na(air$require_guest_profile_picture)] <- mean(air$require_guest_profile_picture, na.rm = TRUE)
 
 air$requires_license <- as.numeric(air$requires_license)
 
 
 air$requires_license[is.na(air$requires_license)] <- mean(air$requires_license, na.rm = TRUE)
 
 air$review_scores_accuracy[is.na(air$review_scores_accuracy)] <- mean(air$review_scores_accuracy, na.rm = TRUE)
 
 air$review_scores_checkin[is.na(air$review_scores_checkin)] <- mean(air$review_scores_checkin, na.rm = TRUE)
 
 air$review_scores_cleanliness[is.na(air$review_scores_cleanliness)] <- mean(air$review_scores_cleanliness, na.rm = TRUE)
 
 air$review_scores_communication[is.na(air$review_scores_communication)] <- mean(air$review_scores_communication, na.rm = TRUE)
 
 air$review_scores_location[is.na(air$review_scores_location)] <- mean(air$review_scores_location, na.rm = TRUE)
 
 air$review_scores_rating[is.na(air$review_scores_rating)] <- mean(air$review_scores_rating, na.rm = TRUE)
 
 air$review_scores_value[is.na(air$review_scores_value)] <- mean(air$review_scores_value, na.rm = TRUE)
 
 air$security_deposit <- gsub("\\$", "", air$security_deposit)

 air$security_deposit <- as.numeric(air$security_deposit)

 air$security_deposit[is.na(air$security_deposit)] <- mean(air$security_deposit, na.rm = TRUE)
 
 air$square_feet[is.na(air$square_feet)] <- mean(air$square_feet, na.rm = TRUE)
 
 air$state <- as.numeric(factor(air$state))
 
 
 air$weekly_price <- gsub("\\$", "", air$weekly_price)

 air$weekly_price <- as.numeric(air$weekly_price)

 air$weekly_price[is.na(air$weekly_price)] <- mean(air$weekly_price, na.rm = TRUE)
 
 air$zipcode[is.na(air$zipcode)] <- mean(air$zipcode, na.rm = TRUE)

 air$zipcode <- as.numeric(air$zipcode)

air$zipcode[is.na(air$zipcode)] <- mean(air$zipcode, na.rm = TRUE)
 
 air$X.randomControl.[is.na(air$X.randomControl.)] <- mean(air$X.randomControl., na.rm = TRUE)

#DECISION TREE AND ANALYSIS PART WITH 70 30 SPLIT
air$high_booking_rate <- factor(air$high_booking_rate, levels = c("0", "1"), labels = c("0", "1"))
set.seed(1234)
train <- sample(nrow(air), 0.7 * nrow(air))
df.train <- air[train, ]
df.validate <- air[-train, ]
print(table(df.train$high_booking_rate))
print(table(df.validate$high_booking_rate))

library(rpart)
dtree <- rpart(high_booking_rate ~ ., data = df.train, method = "class")
print(dtree$cptable)

dtree.pruned <- prune(dtree, cp = 0.001)
library(rpart.plot)
print(prp(dtree.pruned, type = 2, extra = 104, main = "Decision Tree"))
dtree.pred <- predict(dtree.pruned, df.validate, type = "class")
dtree.perf <- table(df.validate$high_booking_rate, dtree.pred, dnn = c("Actual", "Predicted"))
print(dtree.perf)
tn <- dtree.perf[1, 1]
fp <- dtree.perf[1, 2]
fn <- dtree.perf[2, 1]
tp <- dtree.perf[2, 2]
accuracy <- (tp + tn) / (tp + tn + fp + fn)
error_rate <- (fp + fn) / (tp + tn + fp + fn)
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f_measure <- (2 * precision * recall) / (precision + recall)
print(accuracy)
print(error_rate)
print(sensitivity)
print(specificity)
print(precision)
print(recall)
print(f_measure)
