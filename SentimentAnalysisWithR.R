# Sentiment Analysis

install.packages('rtweet')
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("SentimentAnalysis")
install.packages("syuzhet")
install.packages("textreg")

library(syuzhet)
library(textreg)
library(tibble)
library(ggplot2)
library(SentimentAnalysis)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
library(tm)
library(rtweet)
library(dplyr)
source('utils.R')

# Authentication 
app_name = 'tt\_scraper\_dsa\_project'
consumer_key = ''
consumer_secret = ''
access_token = ''
access_secret = ''

token = create_token(app = app_name,
             consumer_key =  consumer_key,
             consumer_secret = consumer_secret,
             access_token = access_token,
             access_secret = access_secret)

################################################################################

# Export tweets using rtweet and saving tweets in a csv
tweets <- search_tweets(
  'Bolsonaro', 
  n = 1500, 
  include_rts = FALSE, 
  lang = 'en', 
  token = token)

# FYI: Some of the tweet's data frame columns are of type list. The data frame 
# is no longer 2-dimensional and can't be exported to a 2d csv-file directly. 
# It's necessary to format those columns before export. In here, the columns are
# being converted to char type. 
tweets_converted <- data.frame(
  lapply(tweets, as.character), 
  stringsAsFactors=FALSE)

write.csv(tweets_converted, 'BolsonaroTweetsEn.csv')

# Read csv 
# tweets <- read.csv('BolsonaroTweetsEn.csv', header = T, sep = ',')

################################################################################

# Pre-processing: text transformations

# Get only tweets text
tweets_text <- tweets$text

# Cleaning 
tweets_text_cleanned <- cleanTweets(tweets_text)

# Corpus
tweets_corpus <- VCorpus(VectorSource(tweets_text_cleanned))

# Remove stopwords
tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords('english'))

# Remove your own stop words
tweets_corpus <- tm_map(
  tweets_corpus, 
  removeWords, 
  c('jair', 'bolsonaro', 'lula')) 

################################################################################

# Analysis after pre-processing

# Word cloud
wordcloud(tweets_corpus, 
          min.freq = 2, 
          scale = c(5,1), 
          max.word = 100, 
          random.order = F,
          colors = brewer.pal(8,"Dark2"))

# Word frequencies
document_matrix <- as.matrix(TermDocumentMatrix(tweets_corpus))

words_frequency <- sort(rowSums(document_matrix), decreasing=TRUE)

words_frequency_df <- data.frame(
  word = names(words_frequency), 
  freq = words_frequency)

barplot(words_frequency_df[1:10,]$freq, 
        las = 2, 
        names.arg = words_frequency_df[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


################################################################################

# Sentiment Scores: Using SentimentAnalysis package

sentiment_analysis_v1_0 <- analyzeSentiment(tweets_corpus)

barplot(table(convertToBinaryResponse(sentiment_analysis_v1_0$SentimentLM)),
        col ="lightblue", 
        main ="Sentiment Analysis by SentimentAnalysis package ",
        ylab = "Frequency")

################################################################################

# Emotion Classification: Using Syuzhet package

# Transform corpus into char
tweets_text_pre_processed <- convert.tm.to.character(tweets_corpus)

# Get emotions
sentiment_analysis_v2_0 <- get_nrc_sentiment(tweets_text_pre_processed)

# Plot
sentiment_analysis_v2_df <- as.data.frame(colSums(sentiment_analysis_v2_0[1:8]))
sentiment_analysis_v2_df <- rownames_to_column(sentiment_analysis_v2_df) 
colnames(sentiment_analysis_v2_df) <- c("emotion", "count")

ggplot(sentiment_analysis_v2_df, aes(x = emotion, y = count, fill = emotion)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(legend.position="none", panel.grid.major = element_blank()) + 
  labs( x = "Emotion", y = "Total Count") + 
  ggtitle("Emotion classification of tweets with word 'Bolsonaro'") + 
  theme(plot.title = element_text(hjust=0.5))
