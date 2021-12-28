#################################
##        Text Analysis        ##
## R Learning Lunch June 16th  ##
#################################
##     Code Pulled from EdX    ##
######################################
## This next line of code will clear #
## your working environment to you   #
## can start "fresh"                 #
rm(list=ls())
######################################

########################################
# Install packaged you want to use first
install.packages("tm")
install.packages("SnowballC")
install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("wordcloud")
install.packages("wordcloud2")
# once it is installed on your computer, 
# you only need to "call" the library to access it.
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(wordcloud)
library(wordcloud2)
########################################


############################################################
install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Confirm that your WD is set to your source file location. 
getwd()
#############################################################

# Twitter
# Read in the data
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
  #e.g. tweets <- read.csv("C:/Users/<YOURNAME>/Downloads/tweets.csv")

str(tweets)

# Create dependent variable
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)
# true indicates a negative tweet is present| 
# Examples: "@APPLE YOU RUINED MY LIFE"


# Load Packages if you haven't already.
library(tm)
# tm is a text mining package for r.
library(SnowballC)
# Tools for stemming words. 

# Create corpus
corpus = Corpus(VectorSource(tweets$Tweet))
# Corpus - Usually large machine-readable samples of naturally 
# occurring languages. 
# Look at corpus
corpus
# you'll see 1181 "documents." this is the number of observations 
#in the dataset. 

corpus[[1]]$content
# Convert to lower-case
    corpus0 = tm_map(corpus, tolower)
      corpus0[[1]]$content
# Remove punctuation
    corpus1 = tm_map(corpus0, removePunctuation)
      corpus1[[1]]$content

# Look at stop words
# Stop words - Many words are frequently used but are only meaningful 
#in a sentence. Examples include the, is, at, which. Their presence is 
#unlikely to improve machine learning prediction quality. Removing them 
#also reduces the size of data.
  stopwords("english")[1:10]
# Remove stopwords and apple
  corpus2 = tm_map(corpus1, removeWords, c("apple", stopwords("english")))

corpus2[[1]]$content
# Stem document
  corpus3 = tm_map(corpus2, stemDocument)
  # Stemming - reduces irregularities in words to their morphological roots to 
  #identify recurring concepts. The basic R environment, for example, 
  #treats "eat," "eating," and "eaten" as separate words; stemming trims
  #each word to its root, "eat," and allows the researcher to calculate 
  #the recurrence of that root.
  corpus3[[1]]$content


# Create document term matrix
frequencies = DocumentTermMatrix(corpus3)
#Document Term Matrix (DTM) lists all occurrences of words in the corpus, 
#by document. In the DTM, the documents are represented by rows and the 
#terms (or words) by columns. If a word occurs in a particular document, 
#then the matrix entry for corresponding to that row and column is 1, 
#else it is 0 (multiple occurrences within a document are recorded - 
#that is, if a word occurs twice in a document, it is recorded as "2" 
#in the relevant matrix entry).
  
#As an example consider corpus of having two documents.
#Doc1: bananas are good
#Doc2: bananas are yellow
#DTM for the above corpus would look like

#       banana	are 	yellow 	good 
#Doc1 	1	       1 	  1     	0 
#Doc2 	1 	     1    0     	1
frequencies
# Typical output: <<DocumentTermMatrix (documents: 1181, terms: 3289)>> 
# Non-/sparse entries: 8980/3875329 
# Sparsity : 100% 
# Maximal term length: 115
# Weighting : term frequency (tf)

# The output signifies that DTM has 1181 (tweets/documents) entries which
# collectively have over 3,289 terms which have appeared at least once.
# This reads like 3,875,329 cells in frequencies are 0,
# 8,980 have non-zero values.
#~100% of all cells are zero (which is 3,875,329/(3,875,329+8,980))
# Sparsity = (1181 (document)*3289 (terms))-8980 (non-zero cells - so full)
                # = cells with zero entries (i.e., 3,875,329)

# low sparsity means that fewer words make up a larger percentage of the matrix.

frequencies

# Look at matrix
  inspect(frequencies[1000:1005,505:515])
  # example: document/tweet 1000 has the term "idea" in it. 
  corpus[[1000]]$content

# Check for sparsity
  findFreqTerms(frequencies, lowfreq=20)
    #lowfreq is an argument specifying the lower frequency bound
    #you can also specify a highfreq for the upper frequency bound

# Remove sparse terms
  sparse = removeSparseTerms(frequencies, 0.995)
  sparse
  # That leaves us with 309 terms.

#Now you are removing those terms which don't appear too often in your data. 
#We will remove any element that doesn't appear in at least .05% of the 
#entries (or documents). Relating to the above created DTM we are basically 
#removing those columns whose entries are 1 in least number of documents.
#removeSparseTerms(frequencies, 0.995)
#removes those terms in frequencies, for which at least 99.5% 
#of all cells are zero, 
#i.e. which are quite uncommon in the corpus. As a result, you get a new
#DocumentTermMatrix 
#with 309 terms and only 360260 zero entries

# Convert to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable

tweetsSparse$Negative = tweets$Negative

#############################################
# Word Cloud Example #
#############################################
install.packages("dplyr")
library(dplyr)
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
# Create dependent variable
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)
positive<-filter(tweets, Negative =="FALSE")
negative<-filter(tweets, Negative =="TRUE")


corpus = Corpus(VectorSource(positive$Tweet))
corpus =tm_map(corpus, tolower)
corpus=tm_map(corpus, removePunctuation)
corpus=tm_map(corpus, removeNumbers)
corpus=tm_map(corpus, removeWords, c("apple", stopwords("english")))

  
#wordcloud from data.frame
dtm <-DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.995)
m<-as.matrix(dtm)
v<-sort(colSums(m))
words<-names(v)
d<-data.frame(word=words, freq=v)
#write.csv(d, "OE Word Frequency Table.csv")
wordcloud(d$word, d$freq, min.freq=5,rot.per = 0, colors=brewer.pal(8,"Paired"), random.color = F )
# The order of words is completely random, but the size is directly proprotional to the frequency of occurence of the word in text.
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf 


frequencyT<-slam::row_sums(TermDocumentMatrix(corpus))
F<-as.data.frame(frequencyT)

write.csv(F, "Frequency Table.csv")
T<- read.csv("Frequency Table.csv")
T<-filter(T,frequencyT>4)

wordcloud2(T, figPath="C:\\Users\\acarroll\\Desktop\\R Learning Lunch - 5-19-17 Text Analysis\\apple.png", size=1, color ="black")
wordcloud2(T, figPath="C:\\Users\\acarroll\\Desktop\\R Learning Lunch - 5-19-17 Text Analysis\\cap.png", size=1, color ="black")
wordcloud2(T, fontFamily = "Calibri")
letterCloud(T, word = "Apple", size =1)


# Need to "unset" seed??
set.seed(100) 
for (i in 1:100){ 
  a <- rnorm(1000, mean=0, sd=1) 
} 

#############################################
# Decision Trees - Preview for Next Session #
#############################################


# Split the data
# A preview for next session.

library(caTools)
# used to split data.
set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
# Setting Seed - This is used when you want to create reproducible results. 
#In essence, random number generators are called Pseudo Random Number Generators
#because they are in fact fully algorithmic: given the same seed, you get the same sequence. 
#If you decide to randomly split data into a training set and a testing set (addressed below), 
#you want to be sure that the process is reproducible.

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)
#Training Data / Testing data: When generating models, one way to 
#test the split up a dataset into training and testing sets so I could 
#train models on one half and test them on unseen data. 
#After a model has been processed by using the training set,
#you test the model by making predictions against the test set. 
#Typically, when you separate a data set into a training set and testing set,
#most of the data is used for training, and a smaller portion of the data is used for testing.

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")

table(testSparse$Negative, predictCART)

# Compute accuracy percentage accurately classified.
(294+18)/(294+6+18+37)

# Baseline accuracy 
table(testSparse$Negative)
300/(300+55)

# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)

table(testSparse$Negative, predictRF)

# Accuracy:
(293+21)/(293+7+34+21)

