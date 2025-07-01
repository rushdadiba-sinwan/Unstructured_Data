library(readr)
library(tibble)
library(tm)
library(tidytext)
library(dbscan)
library(cluster)
library(colorspace)

## DATA ACQUISITION
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

dtm <- DocumentTermMatrix(cleaned_data) #converted to DTM

dtm_tfidf <- weightTfIdf(dtm)
dtm_tfidf <- removeSparseTerms(dtm_tfidf, 0.999)
tfidf.matrix <-as.matrix(dtm_tfidf) #converted to Matrix

dist.matrix <- dist(tfidf.matrix, method = "cosine") #calculated Cosine-Distance Matrix

# 1. Construct data clustering by using k-means,
cluster_kmeans <- kmeans(tfidf.matrix, centers=5)

#----hierarchical,
cluster_hierarchical <- hclust(dist.matrix, method= "ward.D2")

#----and HDBScan algorithms
cluster_dbscan <- hdbscan(dist.matrix, minPts = 10)

# 2. Perform analysis(visualization)
clusplot(dist.matrix,
         cluster_kmeans$cluster,
         color=TRUE,
         shade=TRUE,
         labels=2,
         lines=0,
         main="K-Means Clustering",
         diss = TRUE)

plot(cluster_hierarchical) #Dendogram Cluster
rect.hclust(clustering.hierarchical,2)

plot(as.matrix(dist.matrix),
     col=cluster_dbscan$cluster+1L)

#Combine Results
master.cluster <- cluster_kmeans$cluster
slave.hierarchical <- cutree(clustering.hierarchical,k=2)
slave.dbscan <- clustering.dbscan$cluster

#plotting results
points <- cmdscale(dist.matrix, k=2)
palette <- diverge_hcl(2)

#-----K-Means Plot
plot(points, main="K-Means Clustering",
     col = as.factor(master.cluster),
     xlab="Dimension 1", ylab="Dimension 2", pch=19)

legend("topright",
       legend = unique(master.cluster),
       col = unique(as.factor(master.cluster)),
       pch = 19)

#-----Hierarchical Plot
plot(points, main="Hierarchical Clustering",
     col = as.factor(slave.hierarchical),
     xlab="s", ylab="", pch=19)

#-----Density-Based Plot
plot(points, main="Density-Based Clustering",
     col = as.factor(slave.dbscan),
     xlab="s", ylab="", pch=19)

table(master.cluster)
table(slave.hierarchical)
table(slave.dbscan)

# ELBOW PLOT
cost_df <- data.frame()
for (i in 1:20) {
  kmeans <- kmeans(x=tfidf.matrix, centers=i,
                   iter.max = 100)
}

cost_df <- rbind(cost_df, cbind(i, kmeans$tot.withinss))
names(cost_df) <- c("cluster","cost")
plot(cost_df$cluster, cost_df$cost)
lines(cost_df$cluster, cost_df$cost)
