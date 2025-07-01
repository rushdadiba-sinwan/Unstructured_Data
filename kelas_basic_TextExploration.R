mytext=DirSource("textfile")
docsVCorpus(mytext)

docs=tm_map(docs.content_transformer(tolower))
toSpace <- content_transformer(function(x, patte rn) { return (gsub(pattern, " ", x))}) 
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, ".")
docs <- tm_map(docs, toSpace, ".  ")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, removePunctuation) #remove punctuation
docs <- tm_map(docs, removeNumbers) #Strip digits

docs <- tm_map(docs, removeWords, stopwords ("english")) #remove stopwords
docs <- tm_map(docs, stripWhitespace) #remov e whitespace
tdm <- DocumentTermMatrix(docs) #Create doc ument term matrix
tdm

#Present text data numerically, weighted TF-IDF tdm.tfidf <- weightTfIdf(tdm)
tdm.tfidf <- removeSparseTerms(tdm.tfidf, 0.999)
tfidf.matrix <- as.matrix(tdm.tfidf) 
tfidf.matrix <- as.matrix(tdm.tfidf) 

# Cosine distance matrix (useful for specific clust ering algorithms) 
library(proxy)
dist.matrix <- dist(tfidf.matrix, method = "cosine" )