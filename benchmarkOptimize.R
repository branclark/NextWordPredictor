
require(stringi)
require(data.table)
library(RcppAlgos)

#adapted from https://github.com/hfoffani/dsci-benchmark

#read in test datasets
tweets <- readLines('data/tweets.txt', encoding = 'UTF-8')
blogs <- readLines('data/blogs.txt', encoding = 'UTF-8')

#texts <- readLines('smsTrainingData/SMSSpamCollection.txt', encoding = 'UTF-8')
#texts <- file( 'smsTrainingData/SMSSpamCollection.txt', open = "rb" )
#texts <- readLines( texts, encoding = "UTF-8", skipNul=TRUE )


################################################################################################

# Pre-processing functions
# split.sentence
#  Returns a matrix containing in column i the part of the line before the ith word (sentence) 
#  and the ith word (nextWord).
#  The function is used in benchmark to generate and evaluate predictions for the partial lines.
split.sentence <- compiler::cmpfun(function(line) {
    require(stringi)
    # append a space to the sentence (to make sure we always create one result with only the 
    # last word missing)
    sent <- paste0(line, ' ')
    
    sep <- stri_locate_all_regex(line, 
                                 pattern = '[^\\w\'@#\u2018\u2019\u201b]+', 
                                 omit_empty=T, 
                                 case_insensitive=T)[[1]]
    sapply(seq_len(nrow(sep)), 
           function(i) {
               c(sentence=ifelse(i>1, substr(line, 1, sep[i-1,2]), ''), 
                 nextWord=tolower(substr(line, max(sep[i-1,2]+1, 1), min(nchar(line), sep[i,1]-1)))
               )
           })
}, options=list(optimize=3))


# Benchmarking function
################################################################################################

# benchmark
#  Evaluates the performance of a next word prediction algorithm based on the provided test data-
#  set(s).
#
#  Parameters
#   FUN         Function that produces the next word prediction. The function should take a single 
#               character value as first input and return a vector of character values represen-
#               ting the top-3 predictions (with the 1st value being the first prediction).
#   ...         Additional parameters to pass to FUN.
#   sent.list   Named list of character vectors containing the text lines used for the benchmark.
#   ext.output  If TRUE, return additional details about the R environment and loaded packages 
#               after completing the benchmark.


benchmark <- compiler::cmpfun(function(FUN, ngramWeights, ..., sent.list, ext.output=T) {
    require(stringi)
    require(digest)
    require(data.table)
    result <- rbindlist(lapply(names(sent.list), 
                               function(list.name) {  
                                   sentences <- sent.list[[list.name]]
                                   
                                   score <- 0
                                   max.score <-0
                                   hit.count.top3 <- 0
                                   hit.count.top1 <- 0
                                   total.count <- 0
                                   time <- system.time({
                                       for (sent in sentences) {
                                           split <- split.sentence(sent[1])
                                           max.score <- max.score + ncol(split)*3
                                           total.count <- total.count + ncol(split)
                                           
                                           rank <- sapply(seq_len(ncol(split)),
                                                          function(i) {
                                                
                                                              min(which(FUN(text = split[1,i],ngramWeights=ngramWeights, ...)==split[2,i]),4)
    
                                                          })
                                           score <- score + sum(4-rank)
                                           hit.count.top3 <- hit.count.top3 + sum(rank<4)
                                           hit.count.top1 <- hit.count.top1 + sum(rank==1)
                                       }
                                   })
                                   
                                   list('list.name' = list.name,
                                        'line.count' = length(sentences),
                                        'word.count' = sum(stri_count_words(sentences)),
                                        'hash' = digest(paste0(sentences, collapse = '||'), algo='sha256', serialize=F),
                                        'score' = score,
                                        'max.score' = max.score,
                                        'hit.count.top3' = hit.count.top3,
                                        'hit.count.top1' = hit.count.top1,
                                        'total.count' = total.count,
                                        'total.runtime' = time[3]
                                   )               
                               }), use.names=T)
    
    setkey(result, list.name)
    
    # The overall scores are calculated weighting each data set equally (independent of the 
    # number of lines in each dataset).
    overall.score.percent = 100 * result[,sum(score/max.score)/.N]
    overall.precision.top3 = 100 * result[,sum(hit.count.top3/total.count)/.N]
    print(overall.precision.top3)
    final <- c(ngramWeights,-overall.precision.top3)
    
    return(overall.precision.top3)
    overall.precision.top1 = 100 * result[,sum(hit.count.top1/total.count)/.N]
    average.runtime = 1000 * result[,sum(total.runtime)/sum(total.count)]
    total.mem.used = sum(unlist(lapply(ls(.GlobalEnv),
                                       function(x) {
                                           object.size(get(x,
                                                           envir = .GlobalEnv,
                                                           inherits = FALSE))
                                       })))/(1024^2)

    }, options=list(optimize =3))




################################################################################################
#
# Define the wrapper function to be called by benchmark
#
################################################################################################

source("nextWordPredictor.r")

################################################################################################
#
# Perform the benchmark
#
################################################################################################



#create df of weights to test.  Sum of weights must equal 1

weights <- permuteGeneral(20,4, repetition = TRUE, constraintFun = "sum",comparisonFun = "==", limitConstraints = 20)
weights <- data.frame(weights)
weights <- weights/20
colnames(weights) <- c('n1','n2','n3','n4')
weights <- weights[which(weights$n1<=.1), ]

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



#function to run each set of weights through the prediction function against test/train dataset and return accuracy score

optimizeWeightsFUN <-function(x) benchmark(nextWordPredictor, 
                                      # additional parameters to be passed to the prediction function can be inserted here
                                      #ngramWeights = c(0.001, 0.05, 0.14, 0.809),
                                      ngramWeights = x,
                                      sent.list = list(#'quizzes' = quizzes, 
                                          #'texts' = texts
                                          'tweets' = tweetsTrain,
                                          'blogs' = blogsTrain
                                      ), 
                                      ext.output = T)

dfResults <- data.frame() #dataframe to store results

#run each set of weights through test function and store results
for(row in 1:nrow(weights) ){
    score <- optimizeWeightsFUN( as.numeric( weights[row,] ) )
    results <- c(unlist( as.numeric( weights[row,] ) ),score)
    print(results)
    dfResults <- rbind(dfResults, results)
}

colnames(dfResults) <- c('n1','n2','n3','n4','score')

dfResults <- dfResults[order(-dfResults$score),]

write.csv(dfResults, 'optimizeWeights.csv') #save results


optimizeWeights <- read.csv('optimizeWeights.csv')

head(optimizeWeights)

winner <- optimizeWeights[1,] #test top result agaists test set
winner <- c(winner$n1,winner$n2, winner$n3, winner$n4)

testWeights <- function(x) benchmark(nextWordPredictor, 
                                    # additional parameters to be passed to the prediction function can be inserted here
                                    #ngramWeights = c(0.001, 0.05, 0.14, 0.809),
                                    ngramWeights = x,
                                    sent.list = list(#'quizzes' = quizzes, 
                                        #'texts' = texts
                                        'tweets' = tweetsTest,
                                        'blogs' = blogsTest
                                    ), 
                                    ext.output = T)

testWeights(winner)






