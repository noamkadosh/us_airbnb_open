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
if(!require(fastDummies)) install.packages("fastDummies", repos = "http://cran.us.r-project.org")

# Loading the dataset
airbnb <- read_csv("Data/AB_US_2020.csv", col_types = cols(
  neighbourhood = col_character(),
  neighbourhood_group = col_character()
))

# Removing all NAs
airbnb <- na.omit(airbnb)

# Removing locations with price set at 0 since those are probably typos. Prices above 1500 will also be removed, very high prices can be treated as outliers.
indices <- which(airbnb$price == 0)
airbnb <- airbnb[-indices,]
airbnb <- airbnb %>%
  filter(price <= 1500)

# Converting last_review column to days since last review
airbnb <- airbnb %>%
  mutate(last_review = as.numeric(make_date(2020, 10, 20)) - as.numeric(as.Date(airbnb$last_review, "%d/%m/%y")))

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

# Number of Review Distribution
airbnb %>%
  ggplot(aes(number_of_reviews)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "#2e4057") +
  scale_x_continuous(name = "Number of Reviews",trans = pseudo_log_trans(base = 10), breaks = c(1, 10, 100, 1000)) +
  ylab("Count") +
  ggtitle("Number of Reviews Distribution") 

# Reviews Per Month Distribution
airbnb %>%
  ggplot(aes(reviews_per_month)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "#2e4057") +
  scale_x_continuous(name = "Reviews Per Month",trans = pseudo_log_trans(base = 10), breaks = c(1, 10)) +
  ylab("Count") +
  ggtitle("Reviews Per Month Distribution") 

# Days Since last Review Distribution
airbnb %>%
  ggplot(aes(last_review)) +
  geom_histogram(bins = 20, color = "black", fill = "#2e4057") +
  scale_x_continuous(name = "Days",trans = pseudo_log_trans(base = 10), breaks = c(25, 100, 250, 1000, 5000)) +
  ylab("Count") +
  ggtitle("Number of Days Since Last Reviews Distribution") 

# Age Distribution
airbnb %>%
  ggplot(aes(age)) +
  geom_histogram(bins = 15, color = "black", fill = "#2e4057") +
  xlab("Age") +
  ylab("Count") +
  ggtitle("Listing Age Distribution") 

# Calculated Host Listings Count Distribution
airbnb %>%
  ggplot(aes(calculated_host_listings_count)) +
  geom_histogram(binwidth = 0.25, color = "black", fill = "#2e4057") +
  scale_x_continuous(name = "Calculated Host Listings Count",trans = "log10", breaks = c(1, 10, 100, 1000)) +
  ylab("Count") +
  ggtitle("Calculated Host Listings Count Distribution") 

# Availability 365 Distribution
airbnb %>%
  ggplot(aes(availability_365)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "#2e4057") +
  scale_x_continuous(name = "Availability 365",trans = pseudo_log_trans(base = 10), breaks = c(1, 10, 100, 365)) +
  ylab("Count") +
  ggtitle("Availability 365 Distribution") 

# Room Type Distribution
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

# Average Number of Reviews by Room Type
airbnb %>%
  group_by(room_type) %>%
  summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
  ggplot(aes(x = reorder(room_type, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
  xlab("Room Type") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Number of Reviews by Room Type")

# Average Reviews per Month by Room Type
airbnb %>%
  group_by(room_type) %>%
  summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
  ggplot(aes(x = reorder(room_type, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
  xlab("Room Type") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Reviews per Month by Room Type")

# Average Listing Availability by Room Type
airbnb %>%
  group_by(room_type) %>%
  summarize(mean = mean(availability_365), .groups = "drop") %>%
  ggplot(aes(x = reorder(room_type, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
  xlab("Room Type") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Listing Availability by Room Type")

# Average Price by Top 10 Neighborhoods
airbnb %>%
  group_by(neighbourhood) %>%
  summarize(mean = mean(price), .groups = "drop") %>%
  slice_max(mean, n = 10) %>%
  ggplot(aes(x = reorder(neighbourhood, mean), mean, label = dollar(round(mean, digits = 2)))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("Neighborhood") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Price by Neighborhood") +
  coord_flip()

# Average Number of Reviews by Top 10 Neighborhoods
airbnb %>%
  group_by(neighbourhood) %>%
  summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
  slice_max(mean, n = 10) %>%
  ggplot(aes(x = reorder(neighbourhood, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("Neighbourhood") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Number of Reviews by Neighbourhood") +
  coord_flip()

# Average Reviews per Month by Top 10 Neighborhoods
airbnb %>%
  group_by(neighbourhood) %>%
  summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
  slice_max(mean, n = 10) %>%
  ggplot(aes(x = reorder(neighbourhood, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("Neighbourhood") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Reviews per Month by Neighbourhood") +
  coord_flip()

# Average Listing Availability by Top 10 Neighborhoods
airbnb %>%
  group_by(neighbourhood) %>%
  summarize(mean = mean(availability_365), .groups = "drop") %>%
  slice_max(mean, n = 10) %>%
  ggplot(aes(x = reorder(neighbourhood, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("Neighbourhood") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Listing Availability by Neighbourhood") +
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

# Average Number of Reviews by Top 10 Neighborhood Group
airbnb %>%
  group_by(neighbourhood_group) %>%
  summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
  slice_max(mean, n = 10) %>%
  ggplot(aes(x = reorder(neighbourhood_group, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("Neighbourhood Group") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Number of Reviews by Neighbourhood Group") +
  coord_flip()

# Average Reviews per Month by Top 10 Neighborhood Group
airbnb %>%
  group_by(neighbourhood_group) %>%
  summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
  slice_max(mean, n = 10) %>%
  ggplot(aes(x = reorder(neighbourhood_group, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("Neighbourhood Group") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Reviews per Month by Neighbourhood Group") +
  coord_flip()

# Average Listing Availability by Top 10 Neighborhood Group
airbnb %>%
  group_by(neighbourhood_group) %>%
  summarize(mean = mean(availability_365), .groups = "drop") %>%
  slice_max(mean, n = 10) %>%
  ggplot(aes(x = reorder(neighbourhood_group, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("Neighbourhood Group") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Listing Availability by Neighbourhood Group") +
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

# Average Number of Reviews by City
airbnb %>%
  group_by(city) %>%
  summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
  ggplot(aes(x = reorder(city, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("City") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Number of Reviews by City") +
  coord_flip()

# Average Reviews per Month by City
airbnb %>%
  group_by(city) %>%
  summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
  ggplot(aes(x = reorder(city, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("City") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Reviews per Month by City") +
  coord_flip()

# Average Listing Availability by City
airbnb %>%
  group_by(city) %>%
  summarize(mean = mean(availability_365), .groups = "drop") %>%
  ggplot(aes(x = reorder(city, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("City") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Listing Availability by City") +
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

# Average Number of Reviews by State
airbnb %>%
  group_by(state) %>%
  summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
  ggplot(aes(x = reorder(state, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("State") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Number of Reviews by State") +
  coord_flip()

# Average Reviews per Month by State
airbnb %>%
  group_by(state) %>%
  summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
  ggplot(aes(x = reorder(state, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("State") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Reviews per Month by State") +
  coord_flip()

# Average Listing Availability by State
airbnb %>%
  group_by(state) %>%
  summarize(mean = mean(availability_365), .groups = "drop") %>%
  ggplot(aes(x = reorder(state, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), hjust = -0.05, size = 3) +
  xlab("State") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Listing Availability by State") +
  coord_flip()

# Adding the Region column
temp <- tibble(state = c("NC", "TX", "MA", "FL", "IL", "NV", "OH", "CO", "HI",
                         "NJ", "CA", "TN", "LA", "NY","OR", "RI","WA", "MN", "DC"),
               region = c("Southeast", "Southwest", "Northeast", "Southeast",
                          "Midwest", "West", "Midwest", "West", "West", "Northeast",
                          "West","Southeast", "Southeast", "Northeast", "West",
                          "Northeast", "West", "Midwest", "Southeast"))
airbnb_regions <- sapply(airbnb$state, function(x) {
  temp$region[which(temp$state == x)]
})
airbnb <- airbnb %>%
  mutate(region = airbnb_regions)

# Region Distribution
airbnb %>%
  group_by(region) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(region, count), label = percent(prop.table(stat(count))))) +
  geom_bar(color = "black", fill = "#2e4057") +
  geom_text(stat = "count", position = position_dodge(width = .9), vjust = -0.5, size = 3) +
  xlab("Region") +
  scale_y_continuous(name = "Count", labels = comma, breaks = c(10000, 25000, 50000,75000, 100000)) +
  ggtitle("Region Distribution")

# Average Price by Region
airbnb %>%
  group_by(region) %>%
  summarize(mean = mean(price), .groups = "drop") %>%
  ggplot(aes(x = reorder(region, mean), mean, label = dollar(round(mean, digits = 2)))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
  xlab("Region") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Price by Region")

# Average Number of Reviews by Region
airbnb %>%
  group_by(region) %>%
  summarize(mean = mean(number_of_reviews), .groups = "drop") %>%
  ggplot(aes(x = reorder(region, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
  xlab("Region") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Number of Reviews by Region")

# Average Reviews per Month by Region
airbnb %>%
  group_by(region) %>%
  summarize(mean = mean(reviews_per_month), .groups = "drop") %>%
  ggplot(aes(x = reorder(region, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
  xlab("Region") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Reviews per Month by Region")

# Average Listing Availability by Region
airbnb %>%
  group_by(region) %>%
  summarize(mean = mean(availability_365), .groups = "drop") %>%
  ggplot(aes(x = reorder(region, mean), mean, label = round(mean, digits = 2))) +
  geom_col(color = "black", fill = "#2e4057") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
  xlab("Region") +
  scale_y_continuous(name = "Count", labels = comma) +
  ggtitle("Average Listing Availability by Region")

# Text analysis
words <- airbnb %>%
  unnest_tokens(word, name) %>%
  filter(!word %in% stop_words$word)

words$word <- str_replace(words$word, pattern = "[^A-Za-z]+", replacement = "") # Removing non alphabet characters

words <- words[!words$word == "",]

# Top 10 Words
words %>%
  group_by(word) %>%
  summarize(count = n(), .groups = "drop") %>%
  slice_max(count, n = 10)

# Bottom 10 Words
words %>%
  group_by(word) %>%
  summarize(count = n(), .groups = "drop") %>%
  slice_min(count, n = 10)

# Top 10 Words by Room Type
for(i in na.omit(unique(words$room_type))) {
  print(i)
  words %>%
    filter(room_type == i) %>%
    group_by(word) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
}

# Top 10 Words by Region
for(i in na.omit(unique(words$region))) {
  print(i)
  words %>%
    filter(region == i) %>%
    group_by(word) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
}

# Top 10 Words by Number of Reviews
words <- words %>%
  mutate(categorized_num_reviews = cut(number_of_reviews, breaks=c(-Inf, quantile(number_of_reviews, 0.33), quantile(number_of_reviews, 0.67), Inf),
                                       labels=c("Weak","Medium","Strong")))
for(i in levels(words$categorized_num_reviews)) {
  print(i)
  words %>%
    filter(categorized_num_reviews == i) %>%
    group_by(word) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
}

# Top 10 Words by Price
words <- words %>%
  mutate(categorized_price = cut(price, breaks=c(-Inf, quantile(price, 0.33), quantile(price, 0.67), Inf), labels=c("Low","Medium","High")))
for(i in levels(words$categorized_price)) {
  print(i)
  words %>%
    filter(categorized_price == i) %>%
    group_by(word) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
}

# Top 10 Words by Age
words <- words %>%
  mutate(categorized_age = cut(age, breaks=c(-Inf, quantile(age, 0.33), quantile(age, 0.67), Inf), labels=c("Young","Medium","Old")))
for(i in levels(words$categorized_age)) {
  print(i)
  words %>%
    filter(categorized_age == i) %>%
    group_by(word) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
}

# Top 10 Words by Days Since Last Review
words <- words %>%
  mutate(categorized_days = cut(last_review, breaks=c(-Inf,  quantile(last_review, 0.33), quantile(last_review, 0.67), Inf),
                                labels=c("Recently","Medium","Long Ago")))
for(i in levels(words$categorized_days)) {
  print(i)
  words %>%
    filter(categorized_days == i) %>%
    group_by(word) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
}

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

# Top 10 Sentiment Words by Room Type
for(i in na.omit(unique(words_sentiment$room_type))) {
  print(i)
  words_sentiment %>%
    filter(room_type == i) %>%
    group_by(word, sentiment) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
  words_sentiment %>% 
    filter(room_type == i) %>%
    summarize(mean = mean(sentiment)) %>%
    print
}

# Top 10 Sentiment Words by Region
for(i in na.omit(unique(words_sentiment$region))) {
  print(i)
  words_sentiment %>%
    filter(region == i) %>%
    group_by(word, sentiment) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
  words_sentiment %>% 
    filter(region == i) %>%
    summarize(mean = mean(sentiment)) %>%
    print
}

# Top 10 Sentiment Words by Number of Reviews
words_sentiment <- words_sentiment %>%
  mutate(categorized_num_reviews = cut(number_of_reviews, breaks=c(-Inf, quantile(number_of_reviews, 0.33), quantile(number_of_reviews, 0.67), Inf),
                                       labels=c("Weak","Medium","Strong")))
for(i in levels(words_sentiment$categorized_num_reviews)) {
  print(i)
  words_sentiment %>%
    filter(categorized_num_reviews == i) %>%
    group_by(word,sentiment) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
  words_sentiment %>% 
    filter(categorized_num_reviews == i) %>%
    summarize(mean = mean(sentiment)) %>%
    print
}

# Top 10 Sentiment Words by Price
words_sentiment <- words_sentiment %>%
  mutate(categorized_price = cut(price, breaks=c(-Inf, quantile(price, 0.33), quantile(price, 0.67), Inf), labels=c("Low","Medium","High")))
for(i in levels(words_sentiment$categorized_price)) {
  print(i)
  words_sentiment %>%
    filter(categorized_price == i) %>%
    group_by(word, sentiment) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
  words_sentiment %>% 
    filter(categorized_price == i) %>%
    summarize(mean = mean(sentiment)) %>%
    print
}

# Top 10 Sentiment Words by Age
words_sentiment <- words_sentiment %>%
  mutate(categorized_age = cut(age, breaks=c(-Inf, quantile(age, 0.33), quantile(age, 0.67), Inf), labels=c("Young","Medium","Old")))
for(i in levels(words_sentiment$categorized_age)) {
  print(i)
  words_sentiment %>%
    filter(categorized_age == i) %>%
    group_by(word, sentiment) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
  words_sentiment %>% 
    filter(categorized_age == i) %>%
    summarize(mean = mean(sentiment)) %>%
    print
}

# Top 10 Sentiment Words by Days Since Last Review
words_sentiment <- words_sentiment %>%
  mutate(categorized_days = cut(last_review, breaks=c(-Inf, quantile(last_review, 0.33), quantile(last_review, 0.67), Inf), 
                                labels=c("Recently","Medium","Long Ago")))
for(i in levels(words_sentiment$categorized_days)) {
  print(i)
  words_sentiment %>%
    filter(categorized_days == i) %>%
    group_by(word, sentiment) %>%
    summarize(count = n(), .groups = "drop") %>%
    slice_max(count, n = 10) %>%
    print
  words_sentiment %>% 
    filter(categorized_days == i) %>%
    summarize(mean = mean(sentiment)) %>%
    print
}

top_words <- words %>%
  group_by(word) %>%
  summarize(count = n(), .groups = "drop") %>%
  slice_max(count, n = 20) %>%
  pull(word)

words_cp <- words
words_cp$word[-which(words_cp$word %in% top_words)] <- 0

words_dummy <- dummy_cols(words_cp, select_columns = c("word"))

words_dummy <- words_dummy %>% 
  group_by(id) %>% 
  summarize(word_apartment = sum(word_apartment, na.rm = TRUE),
            word_apt = sum(word_apt, na.rm = TRUE),
            word_bath = sum(word_bath, na.rm = TRUE),
            word_beach = sum(word_beach, na.rm = TRUE),
            word_beautiful = sum(word_beautiful, na.rm = TRUE),
            word_bed = sum(word_bed, na.rm = TRUE),
            word_bedroom = sum(word_bedroom, na.rm = TRUE),
            word_br = sum(word_br, na.rm = TRUE),
            word_brooklyn = sum(word_brooklyn, na.rm = TRUE),
            word_condo = sum(word_condo, na.rm = TRUE),
            word_cozy = sum(word_cozy, na.rm = TRUE),
            word_home = sum(word_home, na.rm = TRUE),
            word_house = sum(word_house, na.rm = TRUE),
            word_modern = sum(word_modern, na.rm = TRUE),
            word_ocean = sum(word_ocean, na.rm = TRUE),
            word_park = sum(word_park, na.rm = TRUE),
            word_private = sum(word_private, na.rm = TRUE),
            word_spacious = sum(word_spacious, na.rm = TRUE),
            word_studio = sum(word_studio, na.rm = TRUE),
            word_view = sum(word_view, na.rm = TRUE), .groups = "drop")

temp <- airbnb %>%
  inner_join(words_dummy, by = "id")

words <- words %>% 
  left_join(afinn, by = "word") %>%
  rename(sentiment = value)

id_sentiment <- words %>% 
  group_by(id) %>% 
  summarize(sentiment = sum(sentiment, na.rm = TRUE), .groups = "drop")

temp <- temp %>%
  inner_join(id_sentiment, by = "id") %>%
  select(!(any_of(c("id", "host_id", "name", "host_name"))))

# Normalize
temp <- BBmisc::normalize(temp, method = "range", range = c(0, 1))

# Turn character columns to factor columns
temp <- temp %>% mutate_if(is.character, as.factor)

# Taking 20% as test test.
set.seed(1, sample.kind="Rounding")
test_indices <- createDataPartition(y = temp$price, times = 1, p = 0.1, list = FALSE)
train <- temp[-test_indices,]
test <- temp[test_indices,]

rm(words_cp, words_dummy, afinn, id_sentiment, temp, test_indices, words, words_sentiment, airbnb_regions, airbnb_states, i, indices)

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
train %>%
  select_if(is.numeric) %>%
  cor

train_glm <- train %>%
  select(!(any_of(c("neighbourhood", "state", "region", "city", "age")))) # Removing columns that might cause overfitting
test_glm <- test %>%
  select(!(any_of(c("neighbourhood", "state", "region", "city", "age"))))
control <- trainControl(method = "cv", number = 10)
fit_glm <- train(price ~ ., method = "glm", data = train_glm, trControl = control)
predictions_glm <- predict(fit_glm, test_glm)
glm_rmse <- RMSE(test_glm$price, predictions_glm)

rmse_results <- rmse_results %>%
  add_row(method = "Linear Regression", RMSE = glm_rmse)
rmse_results %>% knitr::kable()

## Regression Tree ##
train_rt <- train %>%
  select(!(any_of(c("neighbourhood", "state", "region", "city", "age")))) # Removing columns that might cause overfitting
test_rt <- test %>%
  select(!(any_of(c("neighbourhood", "state", "region", "city", "age"))))
tune <- data.frame(cp = seq(0, 0.005, len = 25))
train_fit_rt <- train(price ~ ., method = "rpart", data = train_rt, tuneGrid = tune)
train_fit_rt$bestTune
ggplot(train_fit_rt)
fit_rt <- rpart(price ~ ., data = train_rt, control = rpart.control(cp = train_fit_rt$bestTune))
predictions_rt <- predict(fit_rt, test_rt)
rt_rmse <- RMSE(test_rt$price, predictions_rt)
rpart.plot::prp(fit_rt, faclen = 0)

rmse_results <- rmse_results %>%
  add_row(method = "Regression Tree", RMSE = rt_rmse)
rmse_results %>% knitr::kable()

## Random Forest ##
# Removing columns that might cause overfitting.
# The column neighbourhood_groups helped the last 2 algorithms acheive better RMSE scores. 
# In this case, due to the many values this column has it will cause the rf algorithm to take a very long time.
train_rf <- train %>%
  select(!(any_of(c("neighbourhood_group", "neighbourhood", "state", "region", "city", "age"))))
test_rf <- test %>%
  select(!(any_of(c("neighbourhood_group", "neighbourhood", "state", "region", "city", "age"))))
# First tune the mtry parameter by training on a small subset using a few values.
control <- trainControl(method = "cv", number = 5)
mtry <- round(ncol(train_rf) / 3)
tune <- data.frame(mtry = seq(mtry - 2, mtry + 2, len = 5))
train_small_subset <- train_rf %>% sample_n(5000)
train_fit_rf <- train(price ~ ., method = "rf", data = train_small_subset, ntree = 150,trControl = control, tuneGrid = tune)
ggplot(train_fit_rf)
train_fit_rf$bestTune
# Train the real rf model using the best mtry parameter found.
fit_rf <- randomForest(price ~ ., data = train_rf, minNode = fit_rf$bestTune$mtry)
predictions_rf <- predict(fit_rf, test_rf)
rf_rmse <- RMSE(test_rf$price, predictions_rf)

rmse_results <- rmse_results %>%
  add_row(method = "Random Forest", RMSE = rf_rmse)
rmse_results %>% knitr::kable()


## Random Forest ##
# With neighbourhood_group and city #
# Removing columns that might cause overfitting.
train_rf <- train %>%
  select(!(any_of(c("neighbourhood", "state", "region", "age"))))
test_rf <- test %>%
  select(!(any_of(c("neighbourhood", "state", "region", "age"))))
# First tune the mtry parameter by training on a small subset using a few values.
control <- trainControl(method = "cv", number = 5)
mtry <- round(ncol(train_rf) / 3)
tune <- data.frame(mtry = seq(mtry - 2, mtry + 2, len = 5))
train_small_subset <- train_rf %>% sample_n(5000)
train_fit_rf <- train(price ~ ., method = "rf", data = train_small_subset, ntree = 150, trControl = control, tuneGrid = tune)
ggplot(train_fit_rf)
train_fit_rf$bestTune
# Train the real rf model using the best mtry parameter found.
fit_rf <- randomForest(price ~ ., data = train_rf, minNode = fit_rf$bestTune$mtry)
predictions_rf <- predict(fit_rf, test_rf)
rf_rmse <- RMSE(test_rf$price, predictions_rf)

rmse_results <- rmse_results %>%
  add_row(method = "Random Forest with neighbourhood_group and city", RMSE = rf_rmse)
rmse_results %>% knitr::kable()



# Show some more statistics about the models created.