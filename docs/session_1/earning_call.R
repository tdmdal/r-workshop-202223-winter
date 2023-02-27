# a simple sentiment analysis of MSFT Q2 2023 earning call transcript
# ref: 1) https://www.spglobal.com/marketintelligence/en/news-insights/blog/analyzing-sentiment-in-quarterly-earnings-calls-q2-2022
# 2) https://jagg19.github.io/2019/04/sentiment-analysis-conf-call/


# load libraries
library(tidyverse)
library(tidytext)
library(rvest)
library(ggplot2)
library(scales)
library(wordcloud2)

# 1) "import" data
# scrape Microsoft's q2 2023 earning call transcripts from seeking alpha
msft_q2_2023_url <- "https://seekingalpha.com/article/4572123-microsoft-corporation-msft-q2-2023-earnings-call-transcript"
raw_html <- read_html(msft_q2_2023_url)

# 2) transform data
# turn raw html into a data frame
transcript <- raw_html %>%
  html_elements("[data-test-id=article-content]") %>%
  html_elements("p") %>%
  html_text2()

# 3) build a "model" and plot results
# 3.1) check most frequent words
df_freq <- tibble(paragraph = 1:length(transcript), text = transcript) %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
  
df_freq %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Top Word Count")

df_freq %>%
  count(word, sort = TRUE, name = "freq") %>%
  top_n(50) %>%
  wordcloud2()

# 3.2) simple sentiment analysis using Loughran-McDonald dictionary
# ref: https://sraf.nd.edu/loughranmcdonald-master-dictionary/
loughran <- get_sentiments("loughran")
df_sentiment <- tibble(paragraph = 1:length(transcript), text = transcript) %>%
  unnest_tokens(word, text) %>%
  inner_join(loughran, by = "word")

df_pn <- df_sentiment %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  filter(!(word %in% c("question", "closing")))

# plot top positive word count
df_pn %>% 
  count(word, sentiment) %>%
  filter(sentiment == "positive") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    ggtitle("Top Positive Word Count")

# plot top negative word count
df_pn %>% 
  count(word, sentiment) %>%
  filter(sentiment == "negative") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    ggtitle("Top Negative Word Count")

# Plot percentage of total positive vs negative word count
df_pn %>% 
  count(sentiment) %>%
  mutate(perc = percent(n / sum(n))) %>%
  ggplot(aes(x="", y=n, fill=sentiment)) + 
    geom_col() +
    geom_text(aes(label = perc), position = position_stack(vjust = 0.5)) + 
    coord_polar(theta = "y") +
    ggtitle("Positive vs Negative Word Count") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))

  

