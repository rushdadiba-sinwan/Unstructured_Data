getwd()
setwd("C:/Users/rushd/Desktop/Lyrics")
install.packages("tidyverse")
install.packages("tm")
install.packages("SnowballC")
library(SnowballC)
library(tm)
library(tidyverse)

files <- list.files(pattern="*.csv")
read_file <- function(file) {
  lines <- read_lines(file)
  song_title <- lines[1]
  lyrics_lines <- lines[3:length(lines)]
  lyrics_text <- paste(lyrics_lines,collapse=" ")
  tibble(Title=song_title, Lyrics = lyrics_text)
}
#since in tidyverse library has map_dfr(), I'll just use that
lyrics_data <- map_dfr(files,read_file)

#store into a Corpus
corpus <- Corpus(VectorSource(lyrics_data$Lyrics))

cleaned_data <- corpus %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords,stopwords("english")) %>%
  tm_map(removeWords,c("hey","ooh","yeah")) %>%
  tm_map(stemDocument)
#Data is now clean and has been preprocessed 
install.packages("textstem")
library(textstem)
inspect(cleaned_data[1:5])

cleaned_lyrics <- tibble(
  Title = lyrics_data$Title,
  Cleaned_Lyrics = sapply(cleaned_data,as.character)
)

head(cleaned_lyrics)

#Document Term Matrix
dtm <- DocumentTermMatrix(cleaned_data)
inspect(dtm[1:5,10:100])




dtmm <- DocumentTermMatrix(cleaned_lyrics)

term_matrix <- colSums(as.matrix(dtm))
length(term_matrix)
ord <- order(term_matrix,decreasing = T)
head(ord)
term_matrix[head(ord)]

#dataframe
wf <- data.frame(names(term_matrix),term_matrix)
names(wf) <- c("TERM","FREQ")
head(wf)

findFreqTerms(dtm,lowfreq = 10)
findAssocs(dtm,"get",0.3)



tterm_matrix <- as.matrix(dtmm)
term_frequency <- colSums(term_matrix)
term_frequency <- sort(term_frequency,decreasing = T)
print(head(term_frequency,5))

#wordCloud
set.seed(1234)
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("readr")
library(wordcloud)
library(RColorBrewer)
library(readr)

wordcloud(names(term_matrix),term_matrix)
wordcloud(names(term_matrix),term_matrix,
          colors=brewer.pal(8,"Dark2"))

wordcloud(
  words = names(term_frequency),
  freq = term_frequency,
  min.freq =10,
  random.order = F,
  rot.per = 0.35,
  colors = brewer.pal(8,"Dark2")
)
