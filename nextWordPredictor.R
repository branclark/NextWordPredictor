
library(varhandle)
library(stringr)
library(tidyr)

#setwd("C:/Users/bclar/OneDrive/Documents/Data Science Specialization/10 - Capstone Project")

#read in ngram datasets

unigram <- read.csv('dictionaryData/unigram.csv')
allWords <- unigram$word #create vector of all words
allWords <- unfactor(allWords)
unigram <- unigram[c('word','weightedProp')]
colnames(unigram) <- c('newWord','weightedProp')

#gets top words to use when no good prediction is available
TOTAL <- sum(unigram$count)
unigramFill <- data.frame(unigram[1:3,])
names(unigramFill) <- c('newWord', 'weightedProp')
unigramFill <- unfactor(unigramFill$newWord)

bigram <- read.csv('dictionaryData/bigram.csv')
bigram <- bigram[c('ngram','newWord','weightedProp')]
colnames(bigram) <- c('w1','newWord','weightedProp') #seperate ngram into one word for each column

trigram <- read.csv('dictionaryData/trigram.csv')
trigram <- trigram[c('ngram','newWord','weightedProp')]
trigram <- trigram %>%
  separate(ngram, c('w1','w2'), " ") #seperate ngram into one word for each column

quadgram <- read.csv('dictionaryData/quadgram.csv')
quadgram <- quadgram[c('ngram','newWord','weightedProp')]
quadgram <- quadgram %>%
  separate(ngram, c('w1','w2','w3'), " ") #seperate ngram into one word for each column


nextWordPredictor <- function(text, sourceWeights = NULL, ngramWeights = NULL){
  

  weights <- ngramWeights
  
  #default weights if none provided
  if (is.null(ngramWeights)) {
    weights <- c(0.05,0.3,0.55,0.1);
  }
  

  #clean input text
  regx <- '[^ \\.\'A-Za-z]'
  text <- gsub(regx, ' ', text)
  text <- gsub(" +", " ", text)
  text <- trimws(text,which = "both")
  text <- tolower(text)
  text <- strsplit(text," ")
  text <- unlist(text)[(unlist(text) %in% allWords)]
  
  strLength <- length(text)
  
  #create dataframe with potential candidates for each ngram
  if( strLength > 0 ){
    last1 <- text[length(text)]
    bigramNewWord <- bigram[bigram$w1 == last1,][,c('newWord', 'weightedProp')]
    bigramNewWord$weightedProp <- bigramNewWord$weightedProp / sum(bigramNewWord$weightedProp) #proportional occurrence of candidate word in set of candidate ngrams
    m <- merge(unigram[1:3,], bigramNewWord, by="newWord", all=T, suffixes=c("Unigram","Bigram"))
  } else{ #if there input string is not long enough to have ngrams
    last1 = NA
    m <- unigram
    m$weightedPropUnigram <- m$weightedProp / sum(m$weightedProp)
    m$weightedPropBigram <- NA
  }
  
  if( strLength > 1 ){
    last2 <- text[length(text)-1]
    trigramNewWord <- trigram[trigram$w1 == last2 & trigram$w2 == last1 ,][,c('newWord', 'weightedProp')]
    trigramNewWord$weightedProp <- trigramNewWord$weightedProp / sum(trigramNewWord$weightedProp) #proportional occurrence of candidate word in set of candidate ngrams
    colnames(trigramNewWord) <- c('newWord','weightedPropTrigram')
    m <- merge(m, trigramNewWord, by="newWord", all=T)
  } else{ #if there input string is not long enough to have ngrams
    last2 = NA
    m$weightedPropTrigram <- NA
  }
  
  if( strLength > 2 ){
    last3 <- text[length(text)-2]
    quadgramNewWord <- quadgram[quadgram$w1 == last3 & quadgram$w2 == last2 & quadgram$w3 == last1,][,c('newWord', 'weightedProp')]
    quadgramNewWord$weightedProp <- quadgramNewWord$weightedProp / sum(quadgramNewWord$weightedProp) #proportional occurrence of candidate word in set of candidate ngrams
    colnames(quadgramNewWord) <- c('newWord','weightedPropQuadgram')
    m <- merge(m, quadgramNewWord, by="newWord", all=T)
    
  } else{ #if there input string is not long enough to have ngrams
    last3 = NA
    m$weightedPropQuadgram <- NA
  }
  
  words <- m$newWord #all candidate words
  m[is.na(m)] <- 0 #set NA = 0
  m <- m[,c("weightedPropUnigram","weightedPropBigram","weightedPropTrigram","weightedPropQuadgram")]
  m <- sweep(m, MARGIN=2, weights, '*') #multiply each occurrence proportion by the relevent weight for that ngram
  
  top3 <- words[order(rowSums(m), decreasing=T)][1:3] #order by sum of weights
  top3 <- unfactor(top3)
  top3 <- top3[!is.na(top3)] #get top 3 words
 
  if (length(top3) < 3) {
    #if there are less than 3 candidates, fill with top unigram options
    top3 <- c(top3, unigramFill)
    top3 <- top3[1:3]
  }
  
  return( top3 )
  
  
}
  

