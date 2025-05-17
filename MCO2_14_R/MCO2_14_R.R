# ********************
# Last names: Cailao, Colobong, Domanais, Kaw
# Language: R
# Paradigm(s): Procedural, Functional, Declarative
# ********************

# REQUIRED LIBRARIES #
# use install.packages(<library>) to install
library(dplyr)
library(tidytext)
library(stringr)
library(wordcloud2)
library(lubridate)
library(ggplot2)
library(tidyr)
library(forcats)

# FILENAME OF CSV TO BE READ #
# Ensure the file is in your working directory. #
tweets <- read.csv("fake_tweets.csv")

##### WORD COUNT ##### 
# piping and appending new word count col
tweets <- tweets %>%
  mutate(word_count = str_count(text, "\\w+"))

total_word_count <- sum(str_count(tweets$text, "\\w+"))
###################### 

##### VOCABULARY SIZE ##### 
# extracts all distinct words, counts, and returns number of words
total_unique_words <- tweets %>%
  unnest_tokens(word, text) %>%
  distinct(word) %>%
  count() %>%
  pull(n)
########################### 

##### WORD FREQUENCY #####
# since count() is group_by and summarise(), this results in a word_frequency
# dataframe with word and n columns, sorted decreasingly
word_frequency <- tweets %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)
##########################

##### CHARACTER FREQUENCY #####
# remove all spaces, split into chars, place char and count in df
char_frequency <- tweets %>%
  mutate(text = gsub("\\s", "", text)) %>%  
  unnest_tokens(character, text, token = "characters") %>%
  count(character, sort = TRUE)
###############################

##### STOP WORD IDENTIFICATION #####
data("stop_words")  # use stop_words list from tidytext

# filter tweets df to words in stop_words and count
stop_words_in_tweets <- tweets %>%
  unnest_tokens(word, text) %>%
  filter(word %in% stop_words$word) %>%
  count(word, sort = TRUE)
####################################

###### CORPUS ANALYSIS ######
View(tweets)
View(char_frequency)
View(word_frequency)

cat("Word Count:", total_word_count, "\n", 
    "Vocabulary Size:", total_unique_words, "\n")

cat("Top 20 Frequent Words:\n")
print(head(word_frequency, 20))

cat("Top 10 common stop words:\n")
print(head(stop_words_in_tweets, 10))
#############################

##### WORD CLOUD OF TOP 20 FREQUENT WORDS #####
# get top 20 words based on n
# rename since wordcloud2 expects "freq"
top_20_words <- word_frequency %>%
  top_n(20, wt = n) %>%
  rename(freq = n)

wordcloud2(top_20_words, size = 0.5, color = "random-light", backgroundColor = "black")
###############################################

##### MONTHS HISTOGRAM ######
# convert to date format with ymd_hms format through lubridate
tweets$date_created <- ymd_hms(tweets$date_created)

# append to tweets df
tweets <- tweets %>%
  mutate(month_year = format(date_created, "%Y-%m"))

# count posts per month
posts_per_month <- tweets %>%
  count(month_year, sort = TRUE)

# making a plot with bars
ggplot(posts_per_month, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Posts Per Month", 
       x = "Month-Year", 
       y = "Number of Posts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#############################

##### PIE CHART SYMBOLS #####
# extract all alphanumerics, put what's left in symbols column, count (regex)
symbols_freq <- tweets %>%
  mutate(symbols = str_extract_all(text, "[^a-zA-Z0-9\\s]")) %>%
  unnest(symbols) %>%
  count(symbols, sort = TRUE)

# filter empty symbols
symbols_freq <- symbols_freq %>% 
  filter(symbols != "")

symbols_freq <- symbols_freq %>%
  mutate(symbols = fct_reorder(symbols, n, .desc = TRUE))

ggplot(symbols_freq, aes(x = "", y = n, fill = symbols)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # convert bar chart to pie chart
  labs(title = "Distribution of Different Symbols in Tweets",
       x = NULL, y = NULL) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) + # frequency
  theme_void() +  # remove axis labels and grid lines
  theme(legend.title = element_blank()) + # remove legend title
  guides(fill = guide_legend(reverse = TRUE)) # arrange legend
#############################


