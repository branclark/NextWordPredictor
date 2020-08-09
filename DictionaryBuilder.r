library(tm)
library(dplyr)
library(wordcloud)
library(textreg)
library(quanteda)
library(stringr)

curDir = 'C:/Users/bclar/OneDrive/Documents/Data Science Specialization/10 - Capstone Project'
setwd(curDir)

blogsFile <- file( paste0( curDir, '/datasets/en_US/en_US.blogs.txt' ), open = "rb")
en_US.blogs = readLines( blogsFile, encoding = "UTF-8", skipNul=TRUE )

twitterFile <- file( paste0( curDir, '/datasets/en_US/en_US.twitter.txt' ), open = "rb" )
en_US.twitter = readLines( twitterFile, encoding = "UTF-8", skipNul=TRUE )

newsFile <- file( paste0( curDir, '/datasets/en_US/en_US.news.txt' ), open = "rb")
en_US.news = readLines( newsFile, encoding = "UTF-8", skipNul=TRUE)

#remove non eneglish characters
en_US.blogs <- iconv(en_US.blogs, "latin1", "ASCII", sub="")
en_US.twitter<- iconv(en_US.twitter, "latin1", "ASCII", sub="")
en_US.news <- iconv(en_US.news, "latin1", "ASCII", sub="")

#take random sample of each file.  Full dataset is too large.  We will use binomial sampling to get 5% of lines
percent <- 0.05

sampleFunction <- function(data, percent)
{
  return(data[as.logical(rbinom(length(data),2,percent))])
}

en_US.blogs   <- sampleFunction(en_US.blogs, percent)
en_US.twitter   <- sampleFunction(en_US.twitter, percent)
en_US.news   <- sampleFunction(en_US.news, percent)

allSources<-c(en_US.blogs,en_US.twitter,en_US.news)

en_US.blogsCorp <- Corpus(VectorSource(en_US.blogs))
en_US.twitterCorp <- Corpus(VectorSource(en_US.twitter))
en_US.newsCorp <- Corpus(VectorSource(en_US.news))
allSourcesCorp <- Corpus(VectorSource(allSources))


#create a dataframe with one row for each word and totals by source.  

#clean data.  Remove punctuation, Numbers, whitespace.  Change to all lowercase.
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

allSourcesCorpCleaned <- allSourcesCorp
allSourcesCorpCleaned <- tm_map(allSourcesCorpCleaned, removePunctuation) 
allSourcesCorpCleaned <- tm_map(allSourcesCorpCleaned, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
allSourcesCorpCleaned <- tm_map(allSourcesCorpCleaned, toSpace, "@[^\\s]+")
allSourcesCorpCleaned <- tm_map(allSourcesCorpCleaned, removeNumbers)     
allSourcesCorpCleaned <- tm_map(allSourcesCorpCleaned, tolower)     
#allSourcesCorpCleaned <- tm_map(allSourcesCorpCleaned, removeWords, stopwords("english"))  
allSourcesCorpCleaned <- tm_map(allSourcesCorpCleaned, stripWhitespace)   

allSourcesDfm <- dfm(corpus(allSourcesCorpCleaned), verbose = FALSE)
allSourcesD <- textstat_frequency(allSourcesDfm)
colnames(allSourcesD) <- c('word','allSourcesFreq','allSourcesrank')
allSourcesD<-allSourcesD[,1:3]



#blogs
en_US.blogsCleaned <- en_US.blogsCorp
en_US.blogsCleaned <- tm_map(en_US.blogsCleaned, removePunctuation)  
en_US.blogsCleaned <- tm_map(en_US.blogsCleaned, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
en_US.blogsCleaned <- tm_map(en_US.blogsCleaned, toSpace, "@[^\\s]+")
en_US.blogsCleaned <- tm_map(en_US.blogsCleaned, removeNumbers)     
en_US.blogsCleaned <- tm_map(en_US.blogsCleaned, tolower)     
#en_US.blogsCleaned <- tm_map(en_US.blogsCleaned, removeWords, stopwords("english"))  
en_US.blogsCleaned <- tm_map(en_US.blogsCleaned, stripWhitespace)   

blogsDfm <- dfm(corpus(en_US.blogsCleaned), verbose = FALSE)
blogsD <- textstat_frequency(blogsDfm)
colnames(blogsD) <- c('word','blogsFreq','blogsRank')
blogsD<-blogsD[,1:3]

#twitter
en_US.twitterCleaned <- en_US.twitterCorp
en_US.twitterCleaned <- tm_map(en_US.twitterCleaned, removePunctuation)  
en_US.twitterCleaned <- tm_map(en_US.twitterCleaned, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
en_US.twitterCleaned <- tm_map(en_US.twitterCleaned, toSpace, "@[^\\s]+")
en_US.twitterCleaned <- tm_map(en_US.twitterCleaned, removeNumbers)     
en_US.twitterCleaned <- tm_map(en_US.twitterCleaned, tolower)     
#en_US.twitterCleaned <- tm_map(en_US.twitterCleaned, removeWords, stopwords("english"))  
en_US.twitterCleaned <- tm_map(en_US.twitterCleaned, stripWhitespace) 

twitterDfm <- dfm(corpus(en_US.twitterCleaned), verbose = FALSE)
twitterD <- textstat_frequency(twitterDfm)
colnames(twitterD) <- c('word','twitterFreq','twitterRank')
twitterD<-twitterD[,1:3]


#news
en_US.newsCleaned <- en_US.newsCorp
en_US.newsCleaned <- tm_map(en_US.newsCleaned, removePunctuation)  
en_US.newsCleaned <- tm_map(en_US.newsCleaned, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
en_US.newsCleaned <- tm_map(en_US.newsCleaned, toSpace, "@[^\\s]+")
en_US.newsCleaned <- tm_map(en_US.newsCleaned, removeNumbers)     
en_US.newsCleaned <- tm_map(en_US.newsCleaned, tolower)     
#en_US.newsCleaned <- tm_map(en_US.newsCleaned, removeWords, stopwords("english"))  
en_US.newsCleaned <- tm_map(en_US.newsCleaned, stripWhitespace)   

newsDfm <- dfm(corpus(en_US.newsCleaned), verbose = FALSE)
newsD <- textstat_frequency(newsDfm)
colnames(newsD) <- c('word','newsFreq','newsRank')
newsD<-newsD[,1:3]


allSourcesDF <- allSourcesD
allSourcesDF <- left_join(allSourcesD, blogsD, by = 'word')
allSourcesDF <- left_join(allSourcesDF, newsD, by = 'word')
allSourcesDF <- left_join(allSourcesDF, twitterD, by = 'word')


#############################################################################################
#3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
#############################################################################################
allSourcesDF$cumProportion <- cumsum( allSourcesDF$allSourcesFreq )/sum(allSourcesDF$allSourcesFreq )

#par(mfrow=c(2,2))
plot1 <-plot( seq.int(nrow(allSourcesDF)) , cumsum( allSourcesDF$allSourcesFreq )/sum(allSourcesDF$allSourcesFreq ),
              xlab = "Distinct Word Count", ylab = "Cumulative Proportion", 
              main = "All Data Sources: Ordered Word Count, Cumulative Sum/Total Word Count") +
  abline(h = .9, col = "red", lty = 2, cex =  2) +
  abline(h = .5, col = "blue", lty = 2, cex = 2) +
  abline(v=nrow(allSourcesDF) , col = "green", lty = 2, cex = 2) +
  abline(v = nrow(allSourcesDF[which(allSourcesDF$cumProportion < .5),]), col = "blue", lty = 2, cex = 2) +
  abline(v = nrow(allSourcesDF[which(allSourcesDF$cumProportion < .9),]), col = "red", lty = 2, cex = 2) +
  text(x=0,y=.53, "50%", col = "blue", cex = 1) +
  text(x=0,y=.93, "90%", col = "red", cex = 1) +
  text(x=nrow(allSourcesDF[which(allSourcesDF$cumProportion < .9),]),y=0, nrow(allSourcesDF[which(allSourcesDF$cumProportion < .9),]), col = "red", cex = 1) +
  text(x=nrow(allSourcesDF[which(allSourcesDF$cumProportion < .5),]),y=0, nrow(allSourcesDF[which(allSourcesDF$cumProportion < .5),]), col = "blue", cex = 1) +
  text(x=nrow(allSourcesDF),y=0, nrow(allSourcesDF), col = "green", cex = 1) 
plot1


# writeLines(as.character(allSourcesCorpCleanded), con="cleanedCorpAllSources.txt")
# writeLines(as.character(en_US.blogsCleaned, con="cleanedCorpBlogs.txt")
# writeLines(as.character(en_US.twitterCleaned), con="cleandedCorpTwitter.txt")
# writeLines(as.character(en_US.newsCleaned, con="cleanedCorpNews.txt")

#create ngrams for each datasource
#Blogs
source("Ngrams_tokenizer.R")
unigram.tokenizer <- ngram_tokenizer(1)
wordlist <- unigram.tokenizer(as.character(en_US.blogsCleaned))
unigram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
unigram.df <- unigram.df[which(unigram.df$V2 > 1),]
names(unigram.df) <- c("word","blogsFreq")
row.names(unigram.df) <- NULL
blogsUnigram.df <- unigram.df[with(unigram.df, order(-unigram.df$blogsFreq)),]


bigram.tokenizer <- ngram_tokenizer(2)
wordlist <- bigram.tokenizer(as.character(en_US.blogsCleaned))
bigram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
bigram.df <- bigram.df[which(bigram.df$V2 > 1),]
names(bigram.df) <- c("word","blogsFreq")
row.names(unigram.df) <- NULL
blogsBigram.df <- bigram.df[with(bigram.df, order(-bigram.df$blogsFreq)),]

trigram.tokenizer <- ngram_tokenizer(3)
wordlist <- trigram.tokenizer(as.character(en_US.blogsCleaned))
trigram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
trigram.df <- trigram.df[which(trigram.df$V2 > 1),]
names(trigram.df) <- c("word","blogsFreq")
row.names(trigram.df) <- NULL
blogsTrigram.df <- trigram.df[with(trigram.df, order(-trigram.df$blogsFreq)),]

quadgram.tokenizer <- ngram_tokenizer(4)
wordlist <- quadgram.tokenizer(as.character(en_US.blogsCleaned))
quadgram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
quadgram.df <- quadgram.df[which(quadgram.df$V2 > 1),]
names(quadgram.df) <- c("word","blogsFreq")
row.names(quadgram.df) <- NULL
blogsQuadgram.df <- quadgram.df[with(quadgram.df, order(-quadgram.df$blogsFreq)),]

#News
unigram.tokenizer <- ngram_tokenizer(1)
wordlist <- unigram.tokenizer(as.character(en_US.newsCleaned))
unigram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
unigram.df <- unigram.df[which(unigram.df$V2 > 1),]
names(unigram.df) <- c("word","newsFreq")
row.names(unigram.df) <- NULL
newsUnigram.df <- unigram.df[with(unigram.df, order(-unigram.df$newsFreq)),]


bigram.tokenizer <- ngram_tokenizer(2)
wordlist <- bigram.tokenizer(as.character(en_US.newsCleaned))
bigram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
bigram.df <- bigram.df[which(bigram.df$V2 > 1),]
names(bigram.df) <- c("word","newsFreq")
row.names(unigram.df) <- NULL
newsBigram.df <- bigram.df[with(bigram.df, order(-bigram.df$newsFreq)),]

trigram.tokenizer <- ngram_tokenizer(3)
wordlist <- trigram.tokenizer(as.character(en_US.newsCleaned))
trigram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
trigram.df <- trigram.df[which(trigram.df$V2 > 1),]
names(trigram.df) <- c("word","newsFreq")
row.names(trigram.df) <- NULL
newsTrigram.df <- trigram.df[with(trigram.df, order(-trigram.df$newsFreq)),]

quadgram.tokenizer <- ngram_tokenizer(4)
wordlist <- quadgram.tokenizer(as.character(en_US.newsCleaned))
quadgram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
quadgram.df <- quadgram.df[which(quadgram.df$V2 > 1),]
names(quadgram.df) <- c("word","newsFreq")
row.names(quadgram.df) <- NULL
newsQuadgram.df <- quadgram.df[with(quadgram.df, order(-quadgram.df$newsFreq)),]

#Twitter
unigram.tokenizer <- ngram_tokenizer(1)
wordlist <- unigram.tokenizer(as.character(en_US.twitterCleaned))
unigram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
unigram.df <- unigram.df[which(unigram.df$V2 > 1),]
names(unigram.df) <- c("word","twitterFreq")
row.names(unigram.df) <- NULL
twitterUnigram.df <- unigram.df[with(unigram.df, order(-unigram.df$twitterFreq)),]


bigram.tokenizer <- ngram_tokenizer(2)
wordlist <- bigram.tokenizer(as.character(en_US.twitterCleaned))
bigram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
bigram.df <- bigram.df[which(bigram.df$V2 > 1),]
names(bigram.df) <- c("word","twitterFreq")
row.names(unigram.df) <- NULL
twitterBigram.df <- bigram.df[with(bigram.df, order(-bigram.df$twitterFreq)),]

trigram.tokenizer <- ngram_tokenizer(3)
wordlist <- trigram.tokenizer(as.character(en_US.twitterCleaned))
trigram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
trigram.df <- trigram.df[which(trigram.df$V2 > 1),]
names(trigram.df) <- c("word","twitterFreq")
row.names(trigram.df) <- NULL
twitterTrigram.df <- trigram.df[with(trigram.df, order(-trigram.df$twitterFreq)),]

quadgram.tokenizer <- ngram_tokenizer(4)
wordlist <- quadgram.tokenizer(as.character(en_US.twitterCleaned))
quadgram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
quadgram.df <- quadgram.df[which(quadgram.df$V2 > 1),]
names(quadgram.df) <- c("word","twitterFreq")
row.names(quadgram.df) <- NULL
twitterQuadgram.df <- quadgram.df[with(quadgram.df, order(-quadgram.df$twitterFreq)),]

stopWords <- stopwords("english")

#create dataframe for each ngrams with frequencies

unigramDF <- merge(x = blogsUnigram.df, y = newsUnigram.df, by = "word", all = TRUE)
unigramDF <- merge(x = unigramDF, y = twitterUnigram.df, by = "word", all = TRUE)
blogsTot <- sum(unigramDF$blogsFreq,na.rm = TRUE)
unigramDF$blogsProp <- unigramDF$blogsFreq/blogsTot
newsTot <- sum(unigramDF$newsFreq,na.rm = TRUE)
unigramDF$newsProp <- unigramDF$newsFreq/newsTot
twitterTot <- sum(unigramDF$twitterFreq,na.rm = TRUE)
unigramDF$twitterProp <- unigramDF$twitterFreq/twitterTot
unigramDF$weightedProp <- rowSums(unigramDF[,c("blogsProp","newsProp","twitterProp")],na.rm=TRUE)/3
unigramDF <- unigramDF[order(-unigramDF$weightedProp),]
unigramDF$word <- trimws(unigramDF$word,which = 'both')
unigramDF$stopWord <- 'N'
index <- unigramDF$Word %in% stopWords
unigramDF$stopWord[index] <- "Y"

bigramDF <- merge(x = blogsBigram.df, y = newsBigram.df, by = "word", all = TRUE)
bigramDF <- merge(x = bigramDF, y = twitterBigram.df, by = "word", all = TRUE)
blogsTot <- sum(bigramDF$blogsFreq,na.rm = TRUE)
bigramDF$blogsProp <- bigramDF$blogsFreq/blogsTot
newsTot <- sum(bigramDF$newsFreq,na.rm = TRUE)
bigramDF$newsProp <- bigramDF$newsFreq/newsTot
twitterTot <- sum(bigramDF$twitterFreq,na.rm = TRUE)
bigramDF$twitterProp <- bigramDF$twitterFreq/twitterTot
bigramDF$weightedProp <- rowSums(bigramDF[,c("blogsProp","newsProp","twitterProp")],na.rm=TRUE)/3
bigramDF <- bigramDF[order(-bigramDF$weightedProp),]
bigramDF$word <- trimws(bigramDF$word,which = 'both')
bigramDF$ngram <- trimws(word(bigramDF$word,1,-2),which='both')
bigramDF$newWord <- trimws(word(bigramDF$word,-1),which='both')
bigramDF$stopWord <- 'N'
index <- bigramDF$newWord %in% stopWords
bigramDF$stopWord[index] <- "Y"


trigramDF <- merge(x = blogsTrigram.df, y = newsTrigram.df, by = "word", all = TRUE)
trigramDF <- merge(x = trigramDF, y = twitterTrigram.df, by = "word", all = TRUE)
blogsTot <- sum(trigramDF$blogsFreq,na.rm = TRUE)
trigramDF$blogsProp <- trigramDF$blogsFreq/blogsTot
newsTot <- sum(trigramDF$newsFreq,na.rm = TRUE)
trigramDF$newsProp <- trigramDF$newsFreq/newsTot
twitterTot <- sum(trigramDF$twitterFreq,na.rm = TRUE)
trigramDF$twitterProp <- trigramDF$twitterFreq/twitterTot
trigramDF$weightedProp <- rowSums(trigramDF[,c("blogsProp","newsProp","twitterProp")],na.rm=TRUE)/3
trigramDF <- trigramDF[order(-trigramDF$weightedProp),]
trigramDF$word <- trimws(trigramDF$word,which = 'both')
trigramDF$ngram <- trimws(word(trigramDF$word,1,-2),which='both')
trigramDF$newWord <- trimws(word(trigramDF$word,-1),which = 'both')
trigramDF$stopWord <- 'N'
index <- trigramDF$newWord %in% stopWords
trigramDF$stopWord[index] <- "Y"


quadgramDF <- merge(x = blogsQuadgram.df, y = newsQuadgram.df, by = "word", all = TRUE)
quadgramDF <- merge(x = quadgramDF, y = twitterQuadgram.df, by = "word", all = TRUE)
blogsTot <- sum(quadgramDF$blogsFreq,na.rm = TRUE)
quadgramDF$blogsProp <- quadgramDF$blogsFreq/blogsTot
newsTot <- sum(quadgramDF$newsFreq,na.rm = TRUE)
quadgramDF$newsProp <- quadgramDF$newsFreq/newsTot
twitterTot <- sum(quadgramDF$twitterFreq,na.rm = TRUE)
quadgramDF$twitterProp <- quadgramDF$twitterFreq/twitterTot
quadgramDF$weightedProp <- rowSums(quadgramDF[,c("blogsProp","newsProp","twitterProp")],na.rm=TRUE)/3
quadgramDF <- quadgramDF[order(-quadgramDF$weightedProp),]
quadgramDF$word <- trimws(quadgramDF$word,which = 'both')
quadgramDF$ngram <- trimws(word(quadgramDF$word,1,-2),which = 'both')
quadgramDF$newWord <- trimws(word(quadgramDF$word,-1),which = 'both')
quadgramDF$stopWord <- 'N'
index <- quadgramDF$newWord %in% stopWords
quadgramDF$stopWord[index] <- "Y"



write.csv(unigramDF,'dictionaryData/unigram.csv')
write.csv(bigramDF,'dictionaryData/bigram.csv')
write.csv(trigramDF,'dictionaryData/trigram.csv')
write.csv(quadgramDF,'dictionaryData/quadgram.csv')

