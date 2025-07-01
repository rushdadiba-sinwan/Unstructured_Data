library(readr)
library(tibble)
library(dplyr)
library(tm)
library(tidytext)
library(topicmodels)
library(broom)
library(tidyr)
library(ggplot2)

## DATA ACQUISITION
setwd("C:\\Users\\rushd\\Documents\\TASK 2 -\\news")

file_paths <- list.files(pattern="*.txt", full.names=TRUE)

read_file <- function(file) {
  lines <- read_lines(file)
  id <- basename(file)
  contents <- paste(lines,collapse=" ")
  tibble(id=id, Contents=contents)
}
file_list <- list()
for (file in file_paths) {
  file_list[[file]] <- read_file(file)
}

news <- bind_rows((file_list))
head(news)

newsCorpus <- VCorpus(VectorSource(news$Contents))

data("stop_words")
from_data <- stop_words$word

cleaned_data <- newsCorpus %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords,from_data) %>%
  tm_map(removeWords,stopwords("english")) %>%
  tm_map(stemDocument)

dtm <- DocumentTermMatrix(cleaned_data)

# 1. Using the dataset created, create 3 topics, k=3
lda_news <- LDA(dtm, k=3, control =list(seed=1234))
lda_news

# 2. Perform relevant analysis:
## (i) Extract per topic per word probabilities
news_topics<-tidy(lda_news,matrix="beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

## (i) Visualize at least 8 terms that are most common.
news_topics %>%
  mutate(term=reorder(term,beta))%>%
  ggplot(aes(term,beta,fill=factor(topic)))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~topic,scales="free")+
  coord_flip() + 
  labs(title = "Top 10 Terms Per Topic", x=NULL, y="Beta") #visualize the above

## (ii) Extract relevant beta spread. (between topic 1 and topic 2)
beta_spread <- news_topics %>% 
  mutate (topic=paste0("topic",topic)) %>% 
  spread(topic,beta) %>% 
  filter (topic1>0.001 | topic3 > 0.001) %>% 
  mutate(log_ratio = log2(topic3/topic1))

beta_spread%>%
  mutate(term=reorder(term,log_ratio))%>%
  ggplot(aes(term,log_ratio))+geom_col(show.legend=FALSE)+coord_flip()

## (iii) Perform per-document per topic probabilities
doc_topic <-tidy(lda_news,matrix="gamma") %>%
  group_by(topic) %>%
  top_n(10,gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

doc_topic %>%
  filter(document %in% 1:10) %>%
  ggplot(aes(x = factor(topic), y = gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ document)+
  labs(title= "Top 10 Docs Per Topic", x =NULL, y="Gamma")

ggplot(doc_topic, aes(factor(topic),gamma,fill=factor(topic))) +
  geom_boxplot() +
  labs(title="Document per Topic Probabilities", x="Topic", y="Gamma")

## (iv) Other relevant analysis.
tidy(dtm) %>%
  filter(document==25) %>%
  arrange(desc(count))

# 3. Comparison with Different Number of Topics (k)
k_value <- c(2,3,4,5)
LDA_model <- lapply(k_value, function(k) LDA(dtm, k=k, control=list(seed=123)))
names(LDA_model) <- paste0("k=", k_value)

perplexities <- sapply(LDA_model, perplexity)
data.frame(k=k_value, perplexity = perplexities)



















