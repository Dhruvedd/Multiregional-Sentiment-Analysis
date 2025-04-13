# 0. Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(tidytext)
library(tidyverse)
library(stringr)
library(quanteda)
library(tidyverse)
library(ggplot2)
library(textdata)
library(wordcloud2)

# 1. Load the dataset
dataset <- read_csv("D:\\Uni Resources\\CS Side Stuff\\RAISE\\Data.csv") %>% 
  mutate(date = ymd(date)) # format the date

# 2. Identify stop words -----------------------------------------------  
raw_word_counts <- dataset %>%
  unnest_tokens(word, title) %>% # tokenize by words
  count(word, sort = TRUE) # count words and sort by frequency

raw_word_counts %>%
  head(20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Words Before Cleaning") +
  theme_minimal()

# 3. Clean data once and review ------------------------------------------------
# Define stop words after viewing initial plot
custom_stopwords <- c("ai", "artificial", "intelligence", "future", "use", "the")

cleaned_data <- dataset %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%   # Remove default stopwords
  filter(!word %in% custom_stopwords,      # Remove custom stopwords
         !str_detect(word, "\\d"))         # Remove numbers

# 4. Validate categories with cleaned data -------------------------------------
category_keywords <- cleaned_data %>%
  count(category, word, sort = TRUE) %>%
  group_by(category) %>%
  slice_max(n, n = 5) %>%  # Top 5 words per category
  ungroup()

# TODO (5. Generate word clouds for each category)------------------------------
print(category_keywords)


# 6. Sentiment analysis with cleaned data --------------------------------------
sentiment_scores <- cleaned_data %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(date, category) %>%
  summarise(sentiment = mean(value, na.rm = TRUE), .groups = "drop") %>% 
  drop_na()  # Remove dates/categories with no sentiment words


# 7. visualizations of sentiment scores-----------------------------------------
ggplot(sentiment_scores, aes(x = date, y = sentiment, color = category)) +
  geom_line(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Sentiment Trends in AI Headlines by Category") +
  facet_wrap(~category) +
  theme_minimal()

# 8. Check "Other" category with CLEANED data ---------------------------------
other_keywords <- cleaned_data %>%
  filter(category == "Other") %>%
  count(word, sort = TRUE) %>%
  filter(n > 10)

# 9. Generate word cloud for other category -----------------------------------
other_keywords %>%
  wordcloud2(
    size = 0.65,
    color = "random-light", 
    backgroundColor = "grey10"
  )

distinct_publishers <- unique(dataset$source)
print(distinct_publishers)


# -------------------------------------------------------------------------
# 10. Country-Level Analysis (on 'Data_with_location.csv') ----------------
# -------------------------------------------------------------------------

# Load the dataset with 'location' column
geo_dataset <- read_csv("D:\\Uni Resources\\CS Side Stuff\\RAISE\\Data_with_location.csv") %>%
  mutate(date = ymd(date))

# Clean for token-level processing
geo_cleaned <- geo_dataset %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% custom_stopwords,
         !str_detect(word, "\\d"))

# ---- Top countries by number of articles ----
geo_dataset %>%
  count(location, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(location, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Countries Publishing AI News",
       x = "Country", y = "Number of Articles") +
  theme_minimal()

# ---- Sentiment by country over time ----
geo_sentiment <- geo_cleaned %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(location, date) %>%
  summarise(sentiment = mean(value, na.rm = TRUE), .groups = "drop") %>%
  drop_na()

# Identify top 5 countries by frequency
top_geo_countries <- geo_dataset %>%
  count(location, sort = TRUE) %>%
  top_n(5) %>%
  pull(location)

# Plot sentiment trend by country
geo_sentiment %>%
  filter(location %in% top_geo_countries) %>%
  ggplot(aes(x = date, y = sentiment, color = location)) +
  geom_line(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Sentiment Trends in AI Headlines by Country") +
  theme_minimal()

# ---- Distinctive words by country (TF-IDF) ----
geo_tfidf <- geo_cleaned %>%
  count(location, word, sort = TRUE) %>%
  bind_tf_idf(word, location, n) %>%
  group_by(location) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup()

ggplot(geo_tfidf, aes(x = reorder_within(word, tf_idf, location), y = tf_idf, fill = location)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~location, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top Distinctive Words per Country (TF-IDF)")

# Load if not already loaded
library(ggplot2)
library(dplyr)
library(forcats)

# Step 1: Top 5 countries by article count
top5_countries <- geo_dataset %>%
  count(location, sort = TRUE) %>%
  top_n(5) %>%
  pull(location)

# Step 2: Filter to top countries
top5_data <- geo_dataset %>%
  filter(location %in% top5_countries)

# Step 3: Create violin plot with red dots for means
ggplot(top5_data, aes(x = fct_reorder(location, number_of_words_title), y = number_of_words_title)) +
  geom_violin(fill = "white", color = "black") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 2) +
  labs(title = "Headline Word Count Distribution by Country (Top 5)",
       x = "Country", y = "Number of Words in Headline") +
  theme_minimal()


# 1. Identify top 5 countries by article count
top5_countries <- geo_dataset %>%
  count(location, sort = TRUE) %>%
  top_n(5) %>%
  pull(location)

# 2. Compute emotion frequencies for each (using NRC)
nrc_lexicon <- get_sentiments("nrc")

country_emotions <- geo_cleaned %>%
  inner_join(nrc_lexicon, by = "word") %>%
  group_by(location, sentiment) %>%
  summarise(freq = n(), .groups = "drop")

# 3. Define color palette (same order as NRC emotions)
nrc_colors <- c(
  "anger" = "#F8766D",        # reddish
  "anticipation" = "#E68613",# orange
  "disgust" = "#A3A500",      # olive
  "fear" = "#00BF7D",         # teal
  "joy" = "#00BFC4",          # light cyan
  "negative" = "#619CFF",     # soft blue
  "positive" = "#3F8ABF",     # richer blue
  "sadness" = "#C77CFF",      # purple
  "surprise" = "#FF61C3",     # pink
  "trust" = "#F564E3"         # light magenta
)

# 4. Loop through each country and plot separately
for (ctry in top5_countries) {
  ctry_data <- country_emotions %>%
    filter(location == ctry)
  
  p <- ggplot(ctry_data, aes(x = freq, y = fct_reorder(sentiment, freq), fill = sentiment)) +
    geom_col() +
    scale_fill_manual(values = nrc_colors) +
    labs(
      title = paste("Emotion Distribution in", ctry),
      x = "Frequency",
      y = "Emotion"
    ) +
    theme_minimal()
  
  print(p)
}



#top 5 words by country
top5_word_counts <- geo_cleaned %>%
  filter(location %in% top5_countries) %>%
  count(location, word, sort = TRUE) %>%
  group_by(location) %>%
  slice_max(n, n = 5, with_ties = FALSE) %>%
  ungroup()

# 3) Print or View the final data frame
View(top5_word_counts)


# -------------------------------------------------------------------------
# 11. Visualizations -------------------------------
# -------------------------------------------------------------------------

# A. Top 10 publishing countries
geo_dataset %>%
  count(location, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(location, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Countries Publishing AI News",
       x = "Country", y = "Number of Articles") +
  theme_minimal()

# B. Sentiment trend (top 5 countries)
geo_sentiment %>%
  filter(location %in% top_geo_countries) %>%
  ggplot(aes(x = date, y = sentiment, color = location)) +
  geom_line(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Sentiment Trends in AI Headlines by Country",
       x = "Date", y = "Average Sentiment") +
  theme_minimal()

# C. TF-IDF distinctive words by country
ggplot(geo_tfidf, aes(x = reorder_within(word, tf_idf, location), y = tf_idf, fill = location)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~location, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top Distinctive Words per Country (TF-IDF)",
       x = "Word", y = "TF-IDF") +
  theme_minimal()



























