# loading the libraries
library()
library("twitteR")
library("tm")
library("wordcloud")
library("SnowballC")


# account info
consumer_key <- 'XXXXXXXXX'
consumer_secret <- 'XXXXXXXXXXx'
access_token <- 'XXXXXXXXXXXx'
access_secret <- 'XXXXXXXXXXXXXX'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# search Twitter
prolif <- searchTwitter("#prolife", n = 3000) # This is for data downloading
procho <- searchTwitter("#prochoice", n = 3000) # This is for data downloading

# preprocessing data 1
abortion <- c(prolif, procho)
abortion <- twListToDF(abortion)
taglif <- rep(0, length(prolif))
tagcho <- rep(1, length(procho))
tag <- c(taglif, tagcho)
abortion$hash <- tag
keeps <- c("text", "id", "retweetCount", "isRetweet", "screenName", "hash")
twt <- abortion[,keeps]
# preprocessing data 2
list.vector.words <- list()
allwords <- NULL
names <- NULL
for (i in 1:dim(twt)[1]){
    each.vector <- strsplit(twt$text[i], split = "")
    names <- c(names, twt$screenName[i])
    allwords <- c(allwords, each.vector)
    list.vector.words[[i]] <- each.vector
}
outcome<- twt$hash


# make a corpus, change to lowercase, remove punctuation and stopwords white spaces, and stem documents
dat.tm <- Corpus(VectorSource(list.vector.words))
dat.tm <- sapply(dat.tm, function(row) iconv(row, "latin1", "ASCII", sub = ""))
dat.tm <- Corpus(VectorSource(dat.tm))
# convert all words to lowercase
dat.tm <- tm_map(dat.tm, tolower)
# remove punctuation
dat.tm <- tm_map(dat.tm, removePunctuation)
# remove the hashtags
dat.tm <- tm_map(dat.tm, removeWords, words=c("prochoice"))
dat.tm <- tm_map(dat.tm, removeWords, words=c("prolife"))
# remove extra white space
dat.tm <- tm_map(dat.tm, stripWhitespace)
# stem all words
dat.tm <- tm_map(dat.tm, stemDocument)


# create a bigram tokenizer using the RWeka package
Sys.setenv(JAVA_HOME='C:\\Java\\jre7') # for 32-bit version
require("rJava")
require("RWeka")
BigramTokenizer<- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# create the document-term matrix
dat.tm <- tm_map(dat.tm, PlainTextDocument)
datmat <- DocumentTermMatrix(dat.tm, control = list(tokenize = BigramTokenizer))
dat <- as.matrix(datmat)
# Add user names as rownames to matrix
rownames(dat) <- names
word.usage <- colSums(dat)
table(word.usage)

### first, set all values in the matrix that are greater than 1 to 1
dat[dat>1] <- 1
threshold <- 9 # set a threshold
tokeep <- which(word.usage>threshold)
# find which column sums are above the threshold
# keep all rows, and only columns with sums greater than the threshold
dat.out <- dat[,tokeep]

### Drop users with few words....
# find how many zeroes are in each row
num.zero <- rowSums(dat.out==0)
# explore data by making a table; can inform choice of cutoff
table(num.zero)

# the number of columns of the document bigram matrix
num_cols <- dim(dat.out)[2]
# users must have used this many bigrams to scale
cutoff <- 2
# create a list of authors to keep
authors_tokeep <- which(num.zero <(num_cols-cutoff))
# keep only users with 2 bigrams
dat.drop <- dat.out[authors_tokeep,]
# similarly, drop those users from the vector of hashtags
outcome <- outcome[authors_tokeep]

require(e1071)
# append the outcome to the first column of dat.drop
myDat <- cbind(outcome, dat.drop)
# turn the doc-term matrix into a data frame
myDat <- as.data.frame(myDat)
# turn the outcome variable (first column) into a factor
myDat[, 1] <- as.factor(myDat[, 1])


# run the model; save the results to an object
NBmod <- naiveBayes(myDat[, -1], myDat[, 1])

# generate a vector of predictions
# arguments: estimated model, predictors, outcome to predict
NBpredictions <- predict(NBmod, myDat[, -1])
# pull out the actual outcomes
actual<- myDat[, 1]
# make the confusion matrix
tab = table(NBpredictions, actual, dnn = list("predicted", "actual"))
train.error = sum(NBpredictions != actual)/(length(actual))
train.error



