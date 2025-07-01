library(readr)
library(purrr)
library(tm)
library(tidytext)
library(tibble)
library(wordcloud)
library(RColorBrewer)

files <- list.files(pattern="*.csv")
files

#function to read the .csv file
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


inspect(cleaned_data[1:5])

#Document-Term Matrix
dtm <- DocumentTermMatrix(cleaned_data)
inspect(dtm[1:5,1:200])

term_freq <- colSums(as.matrix(dtm))
length(term_freq)

term_order <- order(term_freq,decreasing = T)
head(term_order)
term_freq[head(term_order)]

#Tabulate into a data.frame
term_df <- data.frame(names(term_freq),term_freq)
names(term_df) <- c("WORDS IN LYRICS","FREQUENCY")
head(term_df)
findFreqTerms(dtm,lowfreq = 10)
findAssocs(dtm,"like",0.5)

#WordCloud
wordcloud(
  words = names(term_freq),
  freq = term_freq,
  min.freq = 2,
  random.order=F,
  colors = brewer.pal(8,"Dark2")
)
