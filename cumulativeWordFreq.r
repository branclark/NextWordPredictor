library(tm)
library(dplyr)
library(wordcloud)
library(textreg)
library(quanteda)
library(stringr)

curDir = getwd()
#curDir = 'C:/Users/bclar/OneDrive/Documents/Data Science Specialization/10 - Capstone Project'
#setwd(curDir)

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

#take random sample of each file.  Full dataset is too large.  We will binomial sampling to get 5% of lines
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
#How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
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