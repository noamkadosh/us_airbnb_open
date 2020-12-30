### Noam Kadosh
### HarvardX: PH125.9x - Capstone Project
### US Airbnb Open Data
### https://github.com/noamkadosh

################################################
# US Airbnb Open Data Price Prediction Project #
################################################

### Loading libraries ###

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(fastDummies)) install.packages("fastDummies", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) install.packages("wordcloud", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")

# Loading the dataset
airbnb <- read_csv("Data/AB_US_2020.csv", col_types = cols(
  neighbourhood = col_character(),
  neighbourhood_group = col_character()
))

# Changing NAs in neighbourhood_group column to "other".
airbnb$neighbourhood_group[is.na(airbnb$neighbourhood_group)] <- "Other neighborhoods"
airbnb$neighbourhood_group[airbnb$neighbourhood_group == "Other Cities"] <- "Other neighborhoods"
# Changing NAs in reviews_per_month column to 0.
airbnb$reviews_per_month[is.na(airbnb$reviews_per_month)] <- 0
# Converting last_review column to numeric
airbnb <- airbnb %>%
  mutate(last_review = as.numeric(as.Date(airbnb$last_review, "%d/%m/%y")))
airbnb$last_review[is.na(airbnb$last_review)] <- 0
airbnb <- airbnb %>%
  filter(minimum_nights < 365)
# Removing all NAs
airbnb <- na.omit(airbnb)

# Adding the age column
airbnb <- airbnb %>%
  mutate(age = ifelse(number_of_reviews > 0, round(number_of_reviews / reviews_per_month / 12, digits = 2), NA))

### Data Exploration ###
head(airbnb)
summary(airbnb)
length(airbnb$price)
airbnb %>% summarize(num_properties = n_distinct(id), num_hosts = n_distinct(host_id))

# Price Distribution
airbnb %>%
  ggplot(aes(price)) +
  geom_histogram(binwidth = 0.25, color = "black", fill = "#2e4057") +
  scale_x_continuous(name = "Price",trans = "log", breaks = c(1, 10, 100, 1000, 10000)) +
  ylab("Count") +
  ggtitle("Price Distribution")

# Get rid of extreme outliers. 
# Price Distribution
airbnb <- airbnb %>%
  filter(price > 10 & price < 1000)
airbnb %>%
  ggplot(aes(price)) +
  geom_histogram(binwidth = 0.25, color = "black", fill = "#2e4057") +
  scale_x_continuous(name = "Price",trans = "log", breaks = c(15, 100, 1000)) +
  ylab("Count") +
  ggtitle("Price Distribution")  

# Room Type Distribution
airbnb$room_type <- fct_infreq(airbnb$room_type)
airbnb %>%
  ggplot(aes(room_type, label = percent(prop.table(stat(count))))) +
  geom_bar(color = "black", fill = "#2e4057") +
  geom_text(stat = "count", position = position_dodge(width = .9), vjust = -0.5, size = 3) +
  xlab("Room Type") +
  scale_y_continuous(name = "Count", trans = "log", labels = comma, breaks = c(10, 100, 1000, 10000, 100000)) +
  ggtitle("Room Type Distribution")

# Average Price by Room Type
airbnb %>%
  group_by(room_type) %>%
  summarize(mean = mean(price), .groups = "drop") %>%
  ggplot(aes(x = reorder(room_type, mean), mean, label = dollar(round(mean, digits = 2)))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
  xlab("Room Type") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Price by Room Type")

# Number of Review Distribution
airbnb %>%
  ggplot(aes(number_of_reviews)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "#2e4057") +
  scale_x_continuous(name = "Number of Reviews",trans = pseudo_log_trans(base = 10), breaks = c(1, 10, 100, 1000)) +
  ylab("Count") +
  ggtitle("Number of Reviews Distribution") 

# Minimum Nights Distribution
airbnb %>%
  ggplot(aes(minimum_nights)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "#2e4057") +
  scale_x_continuous(name = "Minimum Nights",trans = pseudo_log_trans(base = 10), breaks = c(1, 10, 100)) +
  ylab("Count") +
  ggtitle("Minimum Nights Distribution")

# Availability 365 Distribution
airbnb %>%
  ggplot(aes(availability_365)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "#2e4057") +
  scale_x_continuous(name = "Availability 365",trans = pseudo_log_trans(base = 10), breaks = c(1, 10, 100, 365)) +
  ylab("Count") +
  ggtitle("Availability 365 Distribution") 

# Calculated Host Listings Count Distribution
airbnb %>%
  ggplot(aes(calculated_host_listings_count)) +
  geom_histogram(binwidth = 0.25, color = "black", fill = "#2e4057") +
  scale_x_continuous(name = "Calculated Host Listings Count",trans = "log10", breaks = c(1, 10, 100, 1000)) +
  ylab("Count") +
  ggtitle("Calculated Host Listings Count Distribution") 

# Listings' earth coordinates
airbnb %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(color = "black", fill = "#2e4057") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Listing Coordinates")

# Neighbourhood_group distribution
airbnb$neighbourhood_group <- fct_rev(fct_infreq(airbnb$neighbourhood_group))
airbnb %>%
  ggplot(aes(neighbourhood_group, label = percent(prop.table(stat(count))))) +
  geom_bar(color = "black", fill = "#2e4057") +
  geom_text(stat = "count", position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("Neighbourhood Group") +
  ggtitle("Neighbourhood Group Distribution") +
  coord_flip()

# Average Price by Top 10 Neighborhood Group
airbnb %>%
  group_by(neighbourhood_group) %>%
  summarize(mean = mean(price), .groups = "drop") %>%
  slice_max(mean, n = 10) %>%
  ggplot(aes(x = reorder(neighbourhood_group, mean), mean, label = dollar(round(mean, digits = 2)))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("Neighborhood Group") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Price by Neighborhood Group") +
  coord_flip()

# Cities Distribution
airbnb %>%
  group_by(city) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(city, count), label = percent(prop.table(stat(count))))) +
  geom_bar(color = "black", fill = "#2e4057") +
  geom_text(stat = "count", position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("City") +
  scale_y_continuous(name = "Count", labels = comma, breaks = c(1000, 10000, 25000, 40000)) +
  ggtitle("Cities Distribution") +
  coord_flip()

# Average Price by City
airbnb %>%
  group_by(city) %>%
  summarize(mean = mean(price), .groups = "drop") %>%
  ggplot(aes(x = reorder(city, mean), mean, label = dollar(round(mean, digits = 2)))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("City") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Price by City") +
  coord_flip()

# Adding the State column
temp <- tibble(city = c("Asheville", "Austin", "Boston", "Broward County", "Cambridge", "Chicago", "Clark County", "Columbus", "Denver", "Hawaii",
                        "Jersey City", "Los Angeles", "Nashville", "New Orleans", "New York City", "Oakland", "Pacific Grove", "Portland", "Rhode Island",
                        "Salem", "San Clara Country", "Santa Cruz County", "San Diego", "San Francisco", "San Mateo County", "Seattle", "Twin Cities MSA",
                        "Washington D.C."),
               state = c("NC", "TX", "MA", "FL", "MA", "IL", "NV", "OH", "CO", "HI", "NJ", "CA", "TN", "LA", "NY", "CA", "CA", "OR", "RI", "MA", "CA",
                         "CA", "CA", "CA", "CA", "WA", "MN", "DC"))
airbnb_states <- sapply(airbnb$city, function(x) {
  temp$state[which(temp$city == x)]
})
airbnb <- airbnb %>%
  mutate(state = airbnb_states)

# State Distribution
airbnb %>%
  group_by(state) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(state, count), label = percent(prop.table(stat(count))))) +
  geom_bar(color = "black", fill = "#2e4057") +
  geom_text(stat = "count", position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("State") +
  scale_y_continuous(name = "Count", labels = comma, breaks = c(1000, 10000, 25000, 40000, 55000)) +
  ggtitle("States Distribution") +
  coord_flip()

# Average Price by State
airbnb %>%
  group_by(state) %>%
  summarize(mean = mean(price), .groups = "drop") %>%
  ggplot(aes(x = reorder(state, mean), mean, label = dollar(round(mean, digits = 2)))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("State") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Price by State") +
  coord_flip()

# Text analysis
words <- airbnb %>%
  unnest_tokens(word, name, drop = FALSE) %>%
  filter(!word %in% stop_words$word)

words$word <- str_replace(words$word, pattern = "[^A-Za-z]+", replacement = "") # Removing non alphabet characters

words <- words[!words$word == "",]

# Top Words
wordcloud <- words %>%
  group_by(word) %>%
  summarize(count = n(), .groups = "drop") %>%
  slice_max(count, n = 150)
wordcloud(wordcloud$word, wordcloud$count)

# Sentiment Analysis
afinn <- get_sentiments("afinn")
words_sentiment <- words %>% 
  inner_join(afinn, by = "word") %>%
  rename(sentiment = value)

# Top 10 Words
words_sentiment %>%
  group_by(word, sentiment) %>%
  summarize(count = n(), .groups = "drop") %>%
  slice_max(count, n = 10)

# Bottom 10 Words
words_sentiment %>%
  group_by(word, sentiment) %>%
  summarize(count = n(), .groups = "drop") %>%
  slice_min(count, n = 10)

# Creating a new column for each word in the top 10
top_words <- words %>%
  group_by(word) %>%
  summarize(count = n(), .groups = "drop") %>%
  slice_max(count, n = 10) %>%
  pull(word)

words_cp <- words
words_cp$word[-which(words_cp$word %in% top_words)] <- 0

airbnb <- airbnb %>% mutate(name = tolower(name))

# Create a new column for each of the most common words
for (word in top_words){
  regex_word <- word
  col_name <- paste("word", word, sep="_")
  airbnb <- airbnb %>% mutate(!!col_name := ifelse(grepl(regex_word, name, fixed = TRUE), 1, 0))
}

words <- words %>% 
  left_join(afinn, by = "word") %>%
  rename(sentiment = value)

id_sentiment <- words %>% 
  group_by(id) %>% 
  summarize(sentiment = sum(sentiment, na.rm = TRUE), .groups = "drop")

airbnb <- airbnb %>%
  inner_join(id_sentiment, by = "id")
airbnb <- dummy_cols(airbnb, select_columns = c("neighbourhood_group", "room_type", "state", "city"))
airbnb <- airbnb %>%
  select(!(any_of(c("id", "name", "host_id", "host_name", "neighbourhood", "neighbourhood_group", "room_type", "state", "region", "city",
                    "age", "reviews_per_month", "last_review"))))
names(airbnb) <- str_replace_all(names(airbnb), c(" " = "_", "/" = "_"))

# Normalize
airbnb <- BBmisc::normalize(airbnb, method = "range", range = c(0, 1))

# Turn character columns to factor columns
airbnb <- airbnb %>% mutate_if(is.character, as.factor)

# Removing column with near zero variance since they are not very useful for prediction and can prolong running time.
nzv <- nearZeroVar(airbnb, )
if (length(nzv) > 0) {
  airbnb <- airbnb[, -nzv]
}

# Taking 10% as test test.
set.seed(1, sample.kind="Rounding")
test_indices <- createDataPartition(y = airbnb$price, times = 1, p = 0.1, list = FALSE)
train <- airbnb[-test_indices,]
test <- airbnb[test_indices,]

train %>%
  select_if(is.numeric) %>%
  cor %>%
  corrplot(type = "upper", order = "hclust", tl.col = "black", tl.cex = 0.6)

rm(words_cp, afinn, temp, col_name, regex_word, word, id_sentiment, test_indices, words, words_sentiment, airbnb_regions, airbnb_states, i, nzv, top_words)

### Modeling Approach ###
## Base Model - Mean only ##
# Mean calculation
mu <- mean(train$price)
mu
# Testing results
mu_rmse <- RMSE(test$price, mu)
mu_rmse

# Initializing a RMSE table to save the results
rmse_results <- tibble(method = "Average Price Model", RMSE = mu_rmse)
rmse_results %>% knitr::kable()

## Linear Regression ##
control <- trainControl(method = "cv", number = 10)
fit_lm <- train(price ~ ., method = "lm", data = train, trControl = control)
predictions_lm <- predict(fit_lm, test)
lm_rmse <- RMSE(test$price, predictions_lm)

actual_vs_pred <- data.frame(x = test$price, y = predictions_lm)
ggplot(actual_vs_pred ,aes(x, y)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method='lm') +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Linear Regression") +
  xlab("Actual Prices") +
  ylab("Predicted Prices")

rmse_results <- rmse_results %>%
  add_row(method = "Linear Regression", RMSE = lm_rmse)
rmse_results %>% knitr::kable()

## KNN ##
tune <- expand.grid(k = c(9, 15, 21, 29, 41, 55, 71))
train_small_subset <- train %>% sample_n(10000)
train_knn <- train(price ~ ., method = "knn", data = train_small_subset, trControl = control, tuneGrid = tune)
ggplot(train_knn)
train_knn$bestTune
fit_knn <- knnreg(price ~ ., data = train, k = train_knn$bestTune)
predictions_knn <- predict(fit_knn, test)
knn_rmse <- RMSE(test$price, predictions_knn)

actual_vs_pred <- data.frame(x = test$price, y = predictions_knn)
ggplot(actual_vs_pred ,aes(x, y)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method='glm') +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Knn") +
  xlab("Actual Prices") +
  ylab("Predicted Prices")

rmse_results <- rmse_results %>%
  add_row(method = "Knn", RMSE = knn_rmse)
rmse_results %>% knitr::kable()

## Regression Tree ##
tune <- expand.grid(cp = seq(0, 0.0002, len = 7))
train_rt <- train(price ~ ., method = "rpart", data = train, trControl = control, tuneGrid = tune)
train_rt$bestTune
ggplot(train_rt)
fit_rt <- rpart(price ~ ., data = train, control = rpart.control(cp = train_rt$bestTune))
predictions_rt <- predict(fit_rt, test)
rt_rmse <- RMSE(test$price, predictions_rt)
rpart.plot::prp(fit_rt, faclen = 0)

actual_vs_pred <- data.frame(x = test$price, y = predictions_rt)
ggplot(actual_vs_pred ,aes(x, y)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method='glm') +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Regression Tree") +
  xlab("Actual Prices") +
  ylab("Predicted Prices")

rmse_results <- rmse_results %>%
  add_row(method = "Regression Tree", RMSE = rt_rmse)
rmse_results %>% knitr::kable()

## Random Forest ##
# First tune the mtry parameter by training on a small subset using a few values.
mtry <- round(ncol(train) / 3)
tune <- expand.grid(mtry = seq(mtry - 3, mtry + 3, len = 7))
train_small_subset <- train %>% sample_n(10000)
train_rf <- train(price ~ ., method = "rf", data = train_small_subset, ntree = 100, trControl = control, tuneGrid = tune)
ggplot(train_rf)
train_rf$bestTune
fit_rf <- randomForest(price ~ ., data = train, minNode = fit_rf$bestTune$mtry, ntree = 150)
predictions_rf <- predict(fit_rf, test)
rf_rmse <- RMSE(test$price, predictions_rf)

actual_vs_pred <- data.frame(x = test$price, y = predictions_rf)
ggplot(actual_vs_pred ,aes(x, y)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method='glm') +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Random Forest") +
  xlab("Actual Prices") +
  ylab("Predicted Prices")

rmse_results <- rmse_results %>%
  add_row(method = "Random Forest", RMSE = rf_rmse)
rmse_results %>% knitr::kable()

# Average Ensemble
predictions_ensemble <- (predictions_lm + predictions_knn + predictions_rt + predictions_rf) / 4
ensemble_rmse <- RMSE(test$price, predictions_ensemble)

actual_vs_pred <- data.frame(x = test$price, y = predictions_ensemble)
ggplot(actual_vs_pred ,aes(x, y)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method='glm') +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Average Ensemble") +
  xlab("Actual Prices") +
  ylab("Predicted Prices")

rmse_results <- rmse_results %>%
  add_row(method = "Average Ensemble", RMSE = ensemble_rmse)
rmse_results %>% knitr::kable()

# A look at the models' predictions using rows with price less than $350.
low_prices_test <- test %>%
  filter(price < 0.35)

# Linear Regression
predictions_lm_low_prices <- predict(fit_lm, low_prices_test)
lm_rmse_low_prices <- RMSE(low_prices_test$price, predictions_lm_low_prices)

actual_vs_pred <- data.frame(x = low_prices_test$price, y = predictions_lm_low_prices)
ggplot(actual_vs_pred ,aes(x, y)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method='glm') +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Linear Regression") +
  xlab("Actual Prices") +
  ylab("Predicted Prices")

rmse_results <- rmse_results %>%
  add_row(method = "Linear Regression, low prices", RMSE = lm_rmse_low_prices)
rmse_results %>% knitr::kable()

# KNN
predictions_knn_low_prices <- predict(fit_knn, low_prices_test)
knn_rmse_low_prices <- RMSE(low_prices_test$price, predictions_knn_low_prices)

actual_vs_pred <- data.frame(x = low_prices_test$price, y = predictions_knn_low_prices)
ggplot(actual_vs_pred ,aes(x, y)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method='glm') +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Knn") +
  xlab("Actual Prices") +
  ylab("Predicted Prices")

rmse_results <- rmse_results %>%
  add_row(method = "Knn, low prices", RMSE = knn_rmse_low_prices)
rmse_results %>% knitr::kable()

# Regression Tree
predictions_rt_low_prices <- predict(fit_rt, low_prices_test)
rt_rmse_low_prices <- RMSE(low_prices_test$price, predictions_rt_low_prices)

actual_vs_pred <- data.frame(x = low_prices_test$price, y = predictions_rt_low_prices)
ggplot(actual_vs_pred ,aes(x, y)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method='glm') +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Regression Tree") +
  xlab("Actual Prices") +
  ylab("Predicted Prices")

rmse_results <- rmse_results %>%
  add_row(method = "Regression Tree, low prices", RMSE = rt_rmse_low_prices)
rmse_results %>% knitr::kable()

# Random Forest
predictions_rf_low_prices <- predict(fit_rf, low_prices_test)
rf_rmse_low_prices <- RMSE(low_prices_test$price, predictions_rf_low_prices)

actual_vs_pred <- data.frame(x = low_prices_test$price, y = predictions_rf_low_prices)
ggplot(actual_vs_pred ,aes(x, y)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method='glm') +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Random Forest") +
  xlab("Actual Prices") +
  ylab("Predicted Prices")

rmse_results <- rmse_results %>%
  add_row(method = "Random Forest, low prices", RMSE = rf_rmse_low_prices)
rmse_results %>% knitr::kable()

# Average Ensemble
predictions_ensemble_low_prices <- (predictions_lm_low_prices + predictions_knn_low_prices +
                                      predictions_rt_low_prices + predictions_rf_low_prices) / 4
ensemble_rmse_low_prices <- RMSE(low_prices_test$price, predictions_ensemble_low_prices)

actual_vs_pred <- data.frame(x = low_prices_test$price, y = predictions_ensemble_low_prices)
ggplot(actual_vs_pred ,aes(x, y)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method='glm') +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Average Ensemble") +
  xlab("Actual Prices") +
  ylab("Predicted Prices")

rmse_results <- rmse_results %>%
  add_row(method = "Average Ensemble, low prices", RMSE = ensemble_rmse_low_prices)
rmse_results %>% knitr::kable()

# # Reviews Per Month Distribution
# airbnb %>%
#   ggplot(aes(reviews_per_month)) +
#   geom_histogram(binwidth = 0.1, color = "black", fill = "#2e4057") +
#   scale_x_continuous(name = "Reviews Per Month",trans = pseudo_log_trans(base = 10), breaks = c(1, 10)) +
#   ylab("Count") +
#   ggtitle("Reviews Per Month Distribution") 

# # Last Review Distribution
# airbnb %>%
#   ggplot(aes(last_review)) +
#   geom_histogram(bins = 25, color = "black", fill = "#2e4057") +
#   scale_x_continuous(name = "Days") +
#   ylab("Count") +
#   ggtitle("Last Review Distribution") 

# # Last Review Distribution - without 0's.
# airbnb %>%
#   filter(last_review > 0) %>%
#   ggplot(aes(last_review)) +
#   geom_histogram(bins = 20, color = "black", fill = "#2e4057") +
#   scale_x_continuous(name = "Day") +
#   ylab("Count") +
#   ggtitle("Last Review Distribution") 

# # Age Distribution
# airbnb %>%
#   ggplot(aes(age)) +
#   geom_histogram(bins = 15, color = "black", fill = "#2e4057", na.rm= TRUE) +
#   xlab("Age") +
#   ylab("Count") +
#   ggtitle("Listing Age Distribution") 

# # Average Number of Reviews by Room Type
# airbnb %>%
#   group_by(room_type) %>%
#   summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
#   ggplot(aes(x = reorder(room_type, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
#   xlab("Room Type") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Number of Reviews by Room Type")
# 
# # Average Reviews per Month by Room Type
# airbnb %>%
#   group_by(room_type) %>%
#   summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
#   ggplot(aes(x = reorder(room_type, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
#   xlab("Room Type") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Reviews per Month by Room Type")
# 
# # Average Listing Availability by Room Type
# airbnb %>%
#   group_by(room_type) %>%
#   summarize(mean = mean(availability_365), .groups = "drop") %>%
#   ggplot(aes(x = reorder(room_type, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
#   xlab("Room Type") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Listing Availability by Room Type")

# # Average Price by Top 10 Neighborhoods
# airbnb %>%
#   group_by(neighbourhood) %>%
#   summarize(mean = mean(price), .groups = "drop") %>%
#   slice_max(mean, n = 10) %>%
#   ggplot(aes(x = reorder(neighbourhood, mean), mean, label = dollar(round(mean, digits = 2)))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("Neighborhood") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Price by Neighborhood") +
#   coord_flip()

# # Average Number of Reviews by Top 10 Neighborhoods
# airbnb %>%
#   group_by(neighbourhood) %>%
#   summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
#   slice_max(mean, n = 10) %>%
#   ggplot(aes(x = reorder(neighbourhood, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("Neighbourhood") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Number of Reviews by Neighbourhood") +
#   coord_flip()

# # Average Reviews per Month by Top 10 Neighborhoods
# airbnb %>%
#   group_by(neighbourhood) %>%
#   summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
#   slice_max(mean, n = 10) %>%
#   ggplot(aes(x = reorder(neighbourhood, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("Neighbourhood") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Reviews per Month by Neighbourhood") +
#   coord_flip()

# # Average Listing Availability by Top 10 Neighborhoods
# airbnb %>%
#   group_by(neighbourhood) %>%
#   summarize(mean = mean(availability_365), .groups = "drop") %>%
#   slice_max(mean, n = 10) %>%
#   ggplot(aes(x = reorder(neighbourhood, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("Neighbourhood") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Listing Availability by Neighbourhood") +
#   coord_flip()

# # Average Number of Reviews by Top 10 Neighborhood Group
# airbnb %>%
#   group_by(neighbourhood_group) %>%
#   summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
#   slice_max(mean, n = 10) %>%
#   ggplot(aes(x = reorder(neighbourhood_group, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("Neighbourhood Group") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Number of Reviews by Neighbourhood Group") +
#   coord_flip()
# 
# # Average Reviews per Month by Top 10 Neighborhood Group
# airbnb %>%
#   group_by(neighbourhood_group) %>%
#   summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
#   slice_max(mean, n = 10) %>%
#   ggplot(aes(x = reorder(neighbourhood_group, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("Neighbourhood Group") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Reviews per Month by Neighbourhood Group") +
#   coord_flip()
# 
# # Average Listing Availability by Top 10 Neighborhood Group
# airbnb %>%
#   group_by(neighbourhood_group) %>%
#   summarize(mean = mean(availability_365), .groups = "drop") %>%
#   slice_max(mean, n = 10) %>%
#   ggplot(aes(x = reorder(neighbourhood_group, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("Neighbourhood Group") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Listing Availability by Neighbourhood Group") +
#   coord_flip()

# # Average Number of Reviews by City
# airbnb %>%
#   group_by(city) %>%
#   summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
#   ggplot(aes(x = reorder(city, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("City") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Number of Reviews by City") +
#   coord_flip()
# 
# # Average Reviews per Month by City
# airbnb %>%
#   group_by(city) %>%
#   summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
#   ggplot(aes(x = reorder(city, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("City") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Reviews per Month by City") +
#   coord_flip()
# 
# # Average Listing Availability by City
# airbnb %>%
#   group_by(city) %>%
#   summarize(mean = mean(availability_365), .groups = "drop") %>%
#   ggplot(aes(x = reorder(city, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("City") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Listing Availability by City") +
#   coord_flip()
# 
# # Average Number of Reviews by State
# airbnb %>%
#   group_by(state) %>%
#   summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
#   ggplot(aes(x = reorder(state, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("State") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Number of Reviews by State") +
#   coord_flip()
# 
# # Average Reviews per Month by State
# airbnb %>%
#   group_by(state) %>%
#   summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
#   ggplot(aes(x = reorder(state, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("State") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Reviews per Month by State") +
#   coord_flip()
# 
# # Average Listing Availability by State
# airbnb %>%
#   group_by(state) %>%
#   summarize(mean = mean(availability_365), .groups = "drop") %>%
#   ggplot(aes(x = reorder(state, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
#   xlab("State") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Listing Availability by State") +
#   coord_flip()

# # Adding the Region column
# temp <- tibble(state = c("NC", "TX", "MA", "FL", "IL", "NV", "OH", "CO", "HI",
#                          "NJ", "CA", "TN", "LA", "NY","OR", "RI","WA", "MN", "DC"),
#                region = c("Southeast", "Southwest", "Northeast", "Southeast",
#                           "Midwest", "West", "Midwest", "West", "West", "Northeast",
#                           "West","Southeast", "Southeast", "Northeast", "West",
#                           "Northeast", "West", "Midwest", "Southeast"))
# airbnb_regions <- sapply(airbnb$state, function(x) {
#   temp$region[which(temp$state == x)]
# })
# airbnb <- airbnb %>%
#   mutate(region = airbnb_regions)

# # Region Distribution
# airbnb %>%
#   group_by(region) %>%
#   mutate(count = n()) %>%
#   ggplot(aes(x = reorder(region, count), label = percent(prop.table(stat(count))))) +
#   geom_bar(color = "black", fill = "#2e4057") +
#   geom_text(stat = "count", position = position_dodge(width = .9), vjust = -0.5, size = 3) +
#   xlab("Region") +
#   scale_y_continuous(name = "Count", labels = comma, breaks = c(10000, 25000, 50000,75000, 100000)) +
#   ggtitle("Region Distribution")

# # Average Price by Region
# airbnb %>%
#   group_by(region) %>%
#   summarize(mean = mean(price), .groups = "drop") %>%
#   ggplot(aes(x = reorder(region, mean), mean, label = dollar(round(mean, digits = 2)))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
#   xlab("Region") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Price by Region")

# # Average Number of Reviews by Region
# airbnb %>%
#   group_by(region) %>%
#   summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
#   ggplot(aes(x = reorder(region, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
#   xlab("Region") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Number of Reviews by Region")

# # Average Reviews per Month by Region
# airbnb %>%
#   group_by(region) %>%
#   summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
#   ggplot(aes(x = reorder(region, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
#   xlab("Region") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Reviews per Month by Region")

# # Average Listing Availability by Region
# airbnb %>%
#   group_by(region) %>%
#   summarize(mean = mean(availability_365), .groups = "drop") %>%
#   ggplot(aes(x = reorder(region, mean), mean, label = round(mean, digits = 2))) +
#   geom_col(color = "black", fill = "#2e4057") +
#   geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
#   xlab("Region") +
#   scale_y_continuous(name = "Count", labels = comma) +
#   ggtitle("Average Listing Availability by Region")

# # The relation between id and age
# airbnb %>%
#   ggplot(aes(id, age)) +
#   geom_point(na.rm = TRUE) +
#   geom_smooth(formula = "y ~ x", method='glm', na.rm = TRUE)

# # The relation between host_id and age
# airbnb %>%
#   ggplot(aes(host_id, age)) +
#   geom_point(na.rm = TRUE) +
#   geom_smooth(formula = "y ~ x", method='glm', na.rm = TRUE)

# # The relation between host_id and calculated_host_listings_count
# airbnb %>%
#   ggplot(aes(host_id, calculated_host_listings_count)) +
#   geom_point() +
#   geom_smooth(formula = "y ~ x", method='glm')

# # Top 10 Words by Room Type
# for(i in na.omit(unique(words$room_type))) {
#   print(i)
#   words %>%
#     filter(room_type == i) %>%
#     group_by(word) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
# }

# # Top 10 Words by Region
# for(i in na.omit(unique(words$region))) {
#   print(i)
#   words %>%
#     filter(region == i) %>%
#     group_by(word) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
# }

# # Top 10 Words by Number of Reviews
# words <- words %>%
#   mutate(categorized_num_reviews = cut(number_of_reviews, breaks=c(-Inf, quantile(number_of_reviews, 0.33), quantile(number_of_reviews, 0.67), Inf),
#                                        labels=c("Weak","Medium","Strong")))
# for(i in levels(words$categorized_num_reviews)) {
#   print(i)
#   words %>%
#     filter(categorized_num_reviews == i) %>%
#     group_by(word) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
# }

# # Top 10 Words by Price
# words <- words %>%
#   mutate(categorized_price = cut(price, breaks=c(-Inf, quantile(price, 0.33, na.rm =), quantile(price, 0.67), Inf), labels=c("Low","Medium","High")))
# for(i in levels(words$categorized_price)) {
#   print(i)
#   words %>%
#     filter(categorized_price == i) %>%
#     group_by(word) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
# }

# # Top 10 Words by Age
# words <- words %>%
#   mutate(categorized_age = cut(age, breaks=c(-Inf, quantile(age, 0.33, na.rm = TRUE), quantile(age, 0.67, na.rm = TRUE), Inf), labels=c("Young","Medium","Old")))
# for(i in levels(words$categorized_age)) {
#   print(i)
#   words %>%
#     filter(categorized_age == i) %>%
#     group_by(word) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
# }

# # Top 10 Words by Last Review
# words <- words %>%
#   mutate(categorized_days = cut(last_review, breaks=c(-Inf, 1, quantile(last_review, 0.33), quantile(last_review, 0.67), Inf),
#                                 labels=c("No reviews", "Recently","Medium","Long Ago")))
# for(i in levels(words$categorized_days)) {
#   print(i)
#   words %>%
#     filter(categorized_days == i) %>%
#     group_by(word) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
# }

# # Top 10 Sentiment Words by Room Type
# for(i in na.omit(unique(words_sentiment$room_type))) {
#   print(i)
#   words_sentiment %>%
#     filter(room_type == i) %>%
#     group_by(word, sentiment) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
#   words_sentiment %>% 
#     filter(room_type == i) %>%
#     summarize(mean = mean(sentiment)) %>%
#     print
# }

# # Top 10 Sentiment Words by Region
# for(i in na.omit(unique(words_sentiment$region))) {
#   print(i)
#   words_sentiment %>%
#     filter(region == i) %>%
#     group_by(word, sentiment) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
#   words_sentiment %>% 
#     filter(region == i) %>%
#     summarize(mean = mean(sentiment)) %>%
#     print
# }

# # Top 10 Sentiment Words by Number of Reviews
# words_sentiment <- words_sentiment %>%
#   mutate(categorized_num_reviews = cut(number_of_reviews, breaks=c(-Inf, quantile(number_of_reviews, 0.33), quantile(number_of_reviews, 0.67), Inf),
#                                        labels=c("Weak","Medium","Strong")))
# for(i in levels(words_sentiment$categorized_num_reviews)) {
#   print(i)
#   words_sentiment %>%
#     filter(categorized_num_reviews == i) %>%
#     group_by(word,sentiment) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
#   words_sentiment %>% 
#     filter(categorized_num_reviews == i) %>%
#     summarize(mean = mean(sentiment)) %>%
#     print
# }

# # Top 10 Sentiment Words by Price
# words_sentiment <- words_sentiment %>%
#   mutate(categorized_price = cut(price, breaks=c(-Inf, quantile(price, 0.33), quantile(price, 0.67), Inf), labels=c("Low","Medium","High")))
# for(i in levels(words_sentiment$categorized_price)) {
#   print(i)
#   words_sentiment %>%
#     filter(categorized_price == i) %>%
#     group_by(word, sentiment) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
#   words_sentiment %>% 
#     filter(categorized_price == i) %>%
#     summarize(mean = mean(sentiment)) %>%
#     print
# }

# # Top 10 Sentiment Words by Age
# words_sentiment <- words_sentiment %>%
#   mutate(categorized_age = cut(age, breaks=c(-Inf, quantile(age, 0.33, na.rm = TRUE), quantile(age, 0.67, na.rm = TRUE), Inf), labels=c("Young","Medium","Old")))
# for(i in levels(words_sentiment$categorized_age)) {
#   print(i)
#   words_sentiment %>%
#     filter(categorized_age == i) %>%
#     group_by(word, sentiment) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
#   words_sentiment %>% 
#     filter(categorized_age == i) %>%
#     summarize(mean = mean(sentiment)) %>%
#     print
# }

# # Top 10 Sentiment Words by Last Review
# words_sentiment <- words_sentiment %>%
#   mutate(categorized_days = cut(last_review, breaks=c(-Inf, 1, quantile(last_review, 0.33), quantile(last_review, 0.67), Inf), 
#                                 labels=c("No reviews", "Recently","Medium","Long Ago")))
# for(i in levels(words_sentiment$categorized_days)) {
#   print(i)
#   words_sentiment %>%
#     filter(categorized_days == i) %>%
#     group_by(word, sentiment) %>%
#     summarize(count = n(), .groups = "drop") %>%
#     slice_max(count, n = 10) %>%
#     print
#   words_sentiment %>% 
#     filter(categorized_days == i) %>%
#     summarize(mean = mean(sentiment)) %>%
#     print
# }

# # Weighted Average Ensemble
# train_ensemble <- tibble(linear_regression = predict(fit_lm, train), knn = predict(fit_knn, train), regression_tree = predict(fit_rt, train),
#                     random_forest = predict(fit_rf, train), price = train$price)
# test_ensemble <- tibble(linear_regression = predictions_lm, knn = predictions_knn, regression_tree = predictions_rt, random_forest = predictions_rf, price = test$price)
# 
# fit_weighted_ensemble <- train(price ~ ., method = "lm", data = train_ensemble, trControl = control)
# predictions_weighted_ensemble <- predict(fit_weighted_ensemble, test_ensemble)
# weighted_ensemble_rmse <- RMSE(test_ensemble$price, predictions_weighted_ensemble)
# 
# actual_vs_pred <- data.frame(x = test$price, y = predictions_lm)
# ggplot(actual_vs_pred ,aes(x, y)) +
#   geom_point() +
#   geom_smooth(formula = "y ~ x", method='lm') +
#   geom_abline(slope = 1, intercept = 0, color = "red") +
#   ggtitle("Weighted Average Ensemble") +
#   xlab("Actual Prices") +
#   ylab("Predicted Prices")
# 
# rmse_results <- rmse_results %>%
#   add_row(method = "Weighted Average Ensemble", RMSE = weighted_ensemble_rmse)
# rmse_results %>% knitr::kable()

# # Weighted Average Ensemble
# low_prices_test_ensemble <- test_ensemble %>% filter(price < 0.35)
# predictions_weighted_ensemble_low_prices <- predict(fit_weighted_ensemble, low_prices_test_ensemble)
# weighted_ensemble_rmse_low_prices <- RMSE(low_prices_test_ensemble$price, predictions_weighted_ensemble_low_prices)
# 
# actual_vs_pred <- data.frame(x = low_prices_test_ensemble$price, y = predictions_weighted_ensemble_low_prices)
# ggplot(actual_vs_pred ,aes(x, y)) +
#   geom_point() +
#   geom_smooth(formula = "y ~ x", method='lm') +
#   geom_abline(slope = 1, intercept = 0, color = "red") +
#   ggtitle("Weighted Average Ensemble") +
#   xlab("Actual Prices") +
#   ylab("Predicted Prices")
# 
# rmse_results <- rmse_results %>%
#   add_row(method = "Weighted Average Ensemble, low prices", RMSE = weighted_ensemble_rmse_low_prices)
# rmse_results %>% knitr::kable()
