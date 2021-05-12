# Auxiliary functions

cleanTweets <- function(tweets){
  # Remove http links
  tweets = gsub("http(s?)\\S+", "", tweets)
  
  # Remove “#Hashtag”
  tweets = gsub("#\\w+", " ", tweets)
  
  # Remove “@usernames”
  tweets = gsub("@\\w+", " ", tweets)
  
  # Remove punctuation
  tweets = gsub("[[:punct:]]", " ", tweets)
  
  # Remove numbers
  tweets = gsub("[[:digit:]]", " ", tweets)
  
  # Remove unnecessary spaces
  tweets = gsub("[ \t]{2,}", " ", tweets)
  tweets = gsub("^\\s+|\\s+$", "", tweets)
  
  # Remove \n and \r
  tweets = gsub("[\n\r]", "", tweets)
  
  # Convert to low case
  tweets = tolower(tweets)
}