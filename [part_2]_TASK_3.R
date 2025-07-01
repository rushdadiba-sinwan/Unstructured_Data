install.packages("sentimentr")
library(sentimentr)
library(tidytext)
library(dplyr)
setwd("C:/Users/rushd/Desktop")
reviews <- read_csv("shopee_reviewss.csv")

text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))

data("stop_words")
from_data <- stop_words$word

cleaned_data <- docs %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords,from_data) %>%
  tm_map(removeWords,stopwords("english")) %>%
  tm_map(stemDocument)

dtm <- DocumentTermMatrix(docs)

#
m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing = T)
d <- data.frame(word=names(v), freq=v)
m <- d$word
head(d,5) #most Frequent Words

barplot(d[1:5,]$freq,
        las=2,
        names.arg = d[1:5,]$word,
        col = "lightgreen",
        main="Top 5 Most Frequent Words",
        ylab="Frequencies")

bing <- reviews %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)
