Text Predictor Overview
================
Brandon Clark
8/9/2020

# Text Predictor Algorithm and Web App

Please find web app:
<https://bclark94.shinyapps.io/NextWordPredictorWebApp/>

Techniques used: Natural Language Processing, N grams, Markov Chains,
Optimization modeling, Cross Validation, machine learning

All Development completed in R. Web App created with R Shiny and hosted
on shinyapps.io

## Introduction

This project was initially created for the capstone project of the Data
Science Specialization offered through Coursera and Johns Hopkins
University. SwiftKey participated as a corporate partner for the course
capstone project

3 large corpora were provided with text from News articles, Blogs, and
Tweets. I constructed an alogithm to return a prediction of the next
word given input text from the user

## Development Overview

1.  Load and sample copora to workable sized dataset
2.  Preprocess and clean corpora
3.  Exploratory Analysis
4.  Build nGram dictionaries
5.  Build prediction algorithm
6.  Optimize and Test
7.  Build Web App

### 1a. Load Corpora

``` r
library(tm)
library(dplyr)
library(wordcloud)
library(textreg)
library(quanteda)

blogsFile <- file( './datasets/en_US/en_US.blogs.txt' , open = "rb")
en_US.blogs = readLines( blogsFile, encoding = "UTF-8", skipNul=TRUE )

twitterFile <- file( './datasets/en_US/en_US.twitter.txt' , open = "rb" )
en_US.twitter = readLines( twitterFile, encoding = "UTF-8", skipNul=TRUE )

newsFile <- file( './datasets/en_US/en_US.news.txt' , open = "rb")
en_US.news = readLines( newsFile, encoding = "UTF-8", skipNul=TRUE)

#remove non eneglish characters
en_US.blogs <- iconv(en_US.blogs, "latin1", "ASCII", sub="")
en_US.twitter<- iconv(en_US.twitter, "latin1", "ASCII", sub="")
en_US.news <- iconv(en_US.news, "latin1", "ASCII", sub="")
```

Lines and Word counts in each corpus before preprocessing and cleaining

``` r
f.word.count <- function(my.list) { sum(stringr::str_count(my.list, "\\S+")) }
list <- list(blog = en_US.blogs , twitter = en_US.twitter, news = en_US.news)

df <- data.frame(source = c("blog", "twitter", "news"), line.count = NA, word.count = NA)

df$line.count <- sapply(list, length)
df$word.count <- sapply(list, f.word.count)

par(mfrow=c(1,2))
barplot(height = df$line.count, names.arg = df$source , ylab = 'line count')
barplot(height = df$word.count, names.arg = df$source , ylab = 'word count')
```

![](textPredictorWriteUp_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### 1b. Sample Corpora

Use binomial sampling to get random 5% sample of data

``` r
percent <- 0.05

sampleFunction <- function(data, percent)
{
  return(data[as.logical(rbinom(length(data),2,percent))])
}

en_US.blogs   <- sampleFunction(en_US.blogs, percent)
en_US.twitter   <- sampleFunction(en_US.twitter, percent)
en_US.news   <- sampleFunction(en_US.news, percent)

allSources<-c(en_US.blogs,en_US.twitter,en_US.news)
```

### 2\. Preprocess and clean corpora

Each corpus will have punction, non ASCII characters, whitespace, and
numbers removed. All text will be converted to lower case

Example for news datasource:

``` r
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
```

### 3\. Exploratory Analysis

Top Words:

``` r
allSourcesDF <- allSourcesD
allSourcesDF <- left_join(allSourcesD, blogsD, by = 'word')
allSourcesDF <- left_join(allSourcesDF, newsD, by = 'word')
allSourcesDF <- left_join(allSourcesDF, twitterD, by = 'word')
head(allSourcesDF, n = 20)
```

    ##    word allSourcesFreq allSourcesrank blogsFreq blogsRank newsFreq newsRank
    ## 1   the         464609              1    180403         1   193213        1
    ## 2    to         267785              2    102874         3    87991        2
    ## 3   and         233200              3    104661         2    86584        3
    ## 4     a         231143              4     86277         4    85330        4
    ## 5    of         195505              5     84826         5    75752        5
    ## 6    in         159754              6     56957         7    65996        6
    ## 7     i         159162              7     74769         6    15537       18
    ## 8   for         107674              8     35524        11    34634        7
    ## 9    is         103877              9     42020         9    27507        9
    ## 10 that         101065             10     44557         8    33963        8
    ## 11  you          90971             11     28498        12     9494       36
    ## 12   it          88642             12     38845        10    21265       15
    ## 13   on          80016             13     27068        14    26100       10
    ## 14 with          69521             14     27519        13    24843       11
    ## 15  was          61053             15     26992        15    22642       13
    ## 16   my          58336             16     26382        16     4061       81
    ## 17   at          55749             17     16830        25    20991       16
    ## 18   be          53371             18     20298        20    14948       20
    ## 19 this          52851             19     25078        17    11892       28
    ## 20 have          51812             20     21410        19    13983       23
    ##    twitterFreq twitterRank
    ## 1        90993           1
    ## 2        76920           2
    ## 3        41955           6
    ## 4        59536           4
    ## 5        34927           9
    ## 6        36801           8
    ## 7        68856           3
    ## 8        37516           7
    ## 9        34350          10
    ## 10       22545          14
    ## 11       52979           5
    ## 12       28532          11
    ## 13       26848          13
    ## 14       17159          18
    ## 15       11419          32
    ## 16       27893          12
    ## 17       17928          17
    ## 18       18125          16
    ## 19       15881          21
    ## 20       16419          20

The below plot shows that there is an extremely long tail of words in
the corpus. This indicates that there can be relatively strong
prediction accuracy without having to be able predict distinct every
word.

This is good news - the memory requirements to be able to predict
hundreds of thousands of words is not practical

``` r
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
```

![](textPredictorWriteUp_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
plot1
```

    ## integer(0)

### 4\. Build nGram Dictionaries

nGram dictionaries will be created as the core of the prediction engine.
An nGram is commonly occuring string of n words. For example, a 3-gram
would be used to predict the 3rd word for a string of 2 words.

``` r
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
```

Example for news:

``` r
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
```

Create dataframes to be used in prediction algorithm. Example for
quadgrams:

``` r
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
```

``` r
head(quadgram.df)
```

    ##        X                  word blogsFreq newsFreq twitterFreq    blogsProp
    ## 1  99892        the end of the       327      306         140 0.0009237549
    ## 2 104101       the rest of the       342      200         132 0.0009661290
    ## 3  16291         at the end of       286      262         104 0.0008079324
    ## 4 241975 thanks for the follow        NA       NA         577           NA
    ## 5  32774    for the first time       187      253         167 0.0005282635
    ## 6  16461      at the same time       215      169         106 0.0006073618
    ##       newsProp  twitterProp weightedProp          ngram newWord stopWord
    ## 1 0.0011056311 0.0005095912 0.0008463258     the end of     the        Y
    ## 2 0.0007226347 0.0004804717 0.0007230785    the rest of     the        Y
    ## 3 0.0009466515 0.0003785535 0.0007110458     at the end      of        Y
    ## 4           NA 0.0021002439 0.0007000813 thanks for the  follow        N
    ## 5 0.0009141329 0.0006078695 0.0006834220  for the first    time        N
    ## 6 0.0006106263 0.0003858334 0.0005346072    at the same    time        N

### 5\. Build prediction algorithm

The full algorithm can be found in nextWordPredictor.R.

The algorithm takes input text and returns 3 candidates for next word.
It will take the input text and use the bigram dictionary to create
predictions using just the last 1 word, trigram predictions using last 2
words, and quadgram predictions using the last 3 words. A unigram
prediction is provided based on the top occuring words.

For each candidate and set of ngram predictions, a probability is
calculated using the occurences of the n gram for the prediction divided
by the sum of frequencies of all predictions in the ngram dictionary.
These probabilities are then multiplied by a set of weights (optimized
below). The weighted probabilities are then summed and the top 3
predictions are provided by the algorithm.

This can mathematically represented by:   
![ \\begin{align\*}
P(w\_n|w\_{n-1},w\_{n-2},w\_{n-3}) = & \\lambda\_0 P(w\_n) + \\\\
& \\lambda\_1 P(w\_n|w\_{n-1}) + \\\\
& \\lambda\_2 P(w\_n|w\_{n-1},w\_{n-2})+ \\\\
& \\lambda\_3 P(w\_n|w\_{n-1},w\_{n-2},w\_{n-3})
\\end{align\*}
](https://ibm.codecogs.com/png.latex?%20%5Cbegin%7Balign%2A%7D%0AP%28w_n%7Cw_%7Bn-1%7D%2Cw_%7Bn-2%7D%2Cw_%7Bn-3%7D%29%20%3D%20%26%20%5Clambda_0%20P%28w_n%29%20%2B%20%5C%5C%0A%26%20%5Clambda_1%20P%28w_n%7Cw_%7Bn-1%7D%29%20%2B%20%5C%5C%0A%26%20%5Clambda_2%20P%28w_n%7Cw_%7Bn-1%7D%2Cw_%7Bn-2%7D%29%2B%20%5C%5C%0A%26%20%5Clambda_3%20P%28w_n%7Cw_%7Bn-1%7D%2Cw_%7Bn-2%7D%2Cw_%7Bn-3%7D%29%0A%5Cend%7Balign%2A%7D%20
" \\begin{align*}
P(w_n|w_{n-1},w_{n-2},w_{n-3}) = & \\lambda_0 P(w_n) + \\\\
& \\lambda_1 P(w_n|w_{n-1}) + \\\\
& \\lambda_2 P(w_n|w_{n-1},w_{n-2})+ \\\\
& \\lambda_3 P(w_n|w_{n-1},w_{n-2},w_{n-3})
\\end{align*} ")  

Where the set of weights =   
![ \\begin{align\*} \\lambda\_0,\\lambda\_1,\\lambda\_2,\\lambda\_3
\\end{align\*}
](https://ibm.codecogs.com/png.latex?%20%5Cbegin%7Balign%2A%7D%20%5Clambda_0%2C%5Clambda_1%2C%5Clambda_2%2C%5Clambda_3%20%5Cend%7Balign%2A%7D%20
" \\begin{align*} \\lambda_0,\\lambda_1,\\lambda_2,\\lambda_3 \\end{align*} ")  

### 6\. Optimize and Test

I decided to optimize for prediction accuracy where predediction
accuracy = % of text inputs where one of the 3 predictions was the
actual next word.

I adapted the benchmarking function from
<https://github.com/hfoffani/dsci-benchmark>

The full benchmark and optimization code can be found in
benchmarkOptimize.R

The optimization engine is finding the top permutation of weights to be
used in the alogrithm. In order to reduce the number of tests to be
performed, I found all permutations of weights where unigram weight \<=
.1 and sum of weights = 1.

``` r
library(RcppAlgos)
weights <- permuteGeneral(20,4, repetition = TRUE, constraintFun = "sum",comparisonFun = "==", limitConstraints = 20)
weights <- data.frame(weights)
weights <- weights/20
colnames(weights) <- c('n1','n2','n3','n4')
weights <- weights[which(weights$n1<=.1), ]
head(weights)
```

    ##     n1   n2   n3   n4
    ## 1 0.05 0.05 0.05 0.85
    ## 2 0.05 0.05 0.85 0.05
    ## 3 0.05 0.85 0.05 0.05
    ## 5 0.05 0.05 0.10 0.80
    ## 6 0.05 0.05 0.80 0.10
    ## 7 0.05 0.10 0.05 0.80

Optimization mathematical representation:   
![ \\begin{align\*}
max(PredictionAccuracy) \\\\
s.t. \\lambda\_0 + \\lambda\_1 + \\lambda\_2 + \\lambda\_3 = 1 \\\\
\\lambda\_0,\\lambda\_1,\\lambda\_2,\\lambda\_3 \\ge .05 \\\\
\\lambda\_0 \\le .1 \\\\
\\end{align\*}](https://ibm.codecogs.com/png.latex?%20%5Cbegin%7Balign%2A%7D%0Amax%28PredictionAccuracy%29%20%5C%5C%0As.t.%20%5Clambda_0%20%2B%20%5Clambda_1%20%2B%20%5Clambda_2%20%2B%20%5Clambda_3%20%3D%201%20%5C%5C%0A%5Clambda_0%2C%5Clambda_1%2C%5Clambda_2%2C%5Clambda_3%20%5Cge%20.05%20%20%20%5C%5C%0A%5Clambda_0%20%5Cle%20.1%20%5C%5C%0A%5Cend%7Balign%2A%7D
" \\begin{align*}
max(PredictionAccuracy) \\\\
s.t. \\lambda_0 + \\lambda_1 + \\lambda_2 + \\lambda_3 = 1 \\\\
\\lambda_0,\\lambda_1,\\lambda_2,\\lambda_3 \\ge .05   \\\\
\\lambda_0 \\le .1 \\\\
\\end{align*}")  

Data is then divieded in test and training sets to be ran though the
benchmark function for each set of weights:

``` r
#create test and training datasets
set.seed(1017)
tweetSamp <- sample(length(tweets),100 )
blogsSamp <- sample(length(blogs),100 )

tweets <- tweets[tweetSamp]
blogs <- blogs[blogsSamp]

tweetsTrainInd <- sample(length(tweets),80 )
blogsTrainInd <- sample(length(blogs),80 )

tweetsTrain <- tweets[tweetsTrainInd]
blogsTrain <- blogs[blogsTrainInd]

tweetsTest <- tweets[-tweetsTrainInd]
blogsTest <- blogs[-blogsTrainInd]
```

The optimal set of weights after training and cross validation of the
test data:

  
![\\lambda\_0,\\lambda\_1,\\lambda\_2,\\lambda\_3 = (0.05,0.3,0.55,0.1)
](https://ibm.codecogs.com/png.latex?%5Clambda_0%2C%5Clambda_1%2C%5Clambda_2%2C%5Clambda_3%20%3D%20%280.05%2C0.3%2C0.55%2C0.1%29%20
"\\lambda_0,\\lambda_1,\\lambda_2,\\lambda_3 = (0.05,0.3,0.55,0.1) ")  

A graphical illustration (holding lambda0 = 0.5)
<https://bclark94.shinyapps.io/NextWordPredictorWebApp/>
<img src="./optimizeWeightsResults.png" width="100%" /> This 3d plot
shows that the prediction score peaks at 21.3% for the optimal weights.
The plot shows clear decline in accuracy where n2 (bigram weight) is
large.

### 7\. Build Web App

The model is deployed with an R Shiny application using shinyapps.io

<img src="./shinyApp.png" width="100%" />

Please try the app\!
<https://bclark94.shinyapps.io/NextWordPredictorWebApp/>
