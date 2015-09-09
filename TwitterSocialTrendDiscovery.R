# loading the libraries
library("twitteR")
library("tm")
library("wordcloud")

# account info
consumer_key <- 'XXXXXXXX'
consumer_secret <- 'XXXXXXXXXXXXXX'
access_token <- 'XXXXXXXXXxx'
access_secret <- 'XXXXXXXXXXXXXX'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# search Twitter
bigdata <- searchTwitter("#bigdata", n = 1500) ### This is for data downloading

# preprocessing
big.list <- sapply(bigdata,function(x) x$getText())
big.corp <- Corpus(VectorSource(big.list))
big.corp <- sapply(big.corp, function(row) iconv(row, "latin1", "ASCII", sub=""))
big.corp <- Corpus(VectorSource(big.corp))
big.corp <- tm_map(big.corp,tolower)  
big.corp <- tm_map(big.corp,removePunctuation)  
big.corp <- tm_map(big.corp,function(x) removeWords(x,stopwords()))  

# visualize data
# big.corp <- tm_map(big.corp,function(x) removeWords(x,c("ada","akan","apa","bisa","aku","dan","mau","mberliu","sama"))) 
big.corp <- tm_map(big.corp, PlainTextDocument)
wordcloud(big.corp,max.words = 100)

# analyze data 1
big.tdm <- TermDocumentMatrix(big.corp)
big.tdm
findFreqTerms(big.tdm, lowfreq = 50)
findAssocs(bigdata.tdm, 'bigdata', 0.01)

# analyze data 2
big.tdm <- removeSparseTerms(big.tdm, sparse = 0.92)
big.df <- as.data.frame(as.matrix(big.tdm))
big.sc <- scale(big.df)
big.dist <- dist(big.sc, method = 'euclidean')
big.fit <- hclust(big.dist, method = 'ward.D')
plot(big.fit, main = "Cluster - Bigdata")
par(mar = c(6, 5, 5, 3))
par(mgp = c(3, 1, 0))

# visualize data
groups <- cutree(big.fit, k = 5)
rect.hclust(big.fit, k = 5, border = 'blue')




