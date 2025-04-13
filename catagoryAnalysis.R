# =========================
# Load required libraries
# =========================
library(readr)
library(dplyr)
library(lubridate)
library(tidytext)
library(tidyverse)
library(stringr)
library(quanteda)
library(ggplot2)
library(textdata)
library(wordcloud2)
library(spacyr)
library(xml2)
library(tm)
library(ggraph)
library(tidygraph)

# =========================
# Load dataset
# =========================
dataset <- read_csv("data.csv") %>% 
  mutate(date = ymd(date))

# =========================
# Clean & tokenize text
# =========================
raw_word_counts <- dataset %>%
  unnest_tokens(word, title) %>%
  count(word, sort = TRUE)

raw_word_counts %>%
  head(20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Words Before Cleaning") +
  theme_minimal()

custom_stopwords <- c("ai", "artificial", "intelligence", "future", "use", "the", "powered")

dataset <- dataset %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% custom_stopwords,
         !str_detect(word, "\\d"))

category_keywords <- dataset %>%
  count(category, word, sort = TRUE) %>%
  group_by(category) %>%
  slice_max(n, n = 5) %>%
  ungroup()

print(category_keywords)

other_keywords <- dataset %>%
  filter(category == "Other") %>%
  count(word, sort = TRUE) %>%
  filter(n > 10)

other_keywords %>%
  wordcloud2(size = 0.65, color = "random-light", backgroundColor = "grey10")

# =========================
# Sentiment analysis
# =========================
sentiment_scores <- dataset %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(date, category) %>%
  summarise(sentiment = mean(value, na.rm = TRUE), .groups = "drop") %>%
  drop_na()

ggplot(sentiment_scores, aes(x = date, y = sentiment, color = category)) +
  geom_line(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Sentiment Trends in AI Headlines by Category") +
  facet_wrap(~category) +
  theme_minimal()

# =========================
# Publisher bias
# =========================
dataset %>%
  count(source, category) %>%
  ggplot(aes(source, n, fill = category)) +
  geom_col(position = "fill") +
  coord_flip()

# =========================
# Headline length by category
# =========================
dataset %>%
  ggplot(aes(category, number_of_words_title)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", color = "red")

# =========================
# Article frequency in 2024
# =========================
dataset %>%
  filter(year == 2024) %>%
  count(date) %>%
  ggplot(aes(date, n)) +
  geom_line() +
  geom_vline(xintercept = ymd("2022-11-30"), color = "red", linetype = 2)

# =========================
# Document-term matrix
# =========================
dtm <- DocumentTermMatrix(dataset)
matrix <- as.matrix(dtm)
dim(matrix)

freq_terms <- findFreqTerms(dtm, lowfreq = 50)
print(freq_terms)

# =========================
# K-means clustering
# =========================
dtm_scaled <- scale(matrix)
set.seed(123)
km <- kmeans(dtm_scaled, centers = 3)
print(km$cluster)

# =========================
# Topic modeling (LDA)
# =========================
library(topicmodels)
lda_model <- LDA(dtm, k = 3, control = list(seed = 123))
terms(lda_model, 5)
