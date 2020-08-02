#Now lets build our DTM
#with tweets file
# Obj of this: to get all negative tweets


# Read in the data

tweets = read.csv("02_tweets.csv", stringsAsFactors=FALSE)



# Create dependent variable

tweets$Negative = as.factor(tweets$Avg <= -1)#any score below -1 is our negative tweet

table(tweets$Negative)#False: 999 are like neutral tweets
#182 Negative Tweets


#Now we are into creation stage of Corpus
# Install new packages

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


# Create corpus

corpus = Corpus(VectorSource(tweets$Tweet))

# Visualizing Corpus for Frequent Terms, without cleaning, i want to see

install.packages("wordcloud")

library(wordcloud)
wordcloud(corpus,colors=rainbow(7),max.words=50)#top 50 words

# Now we do cleaning
# 1. Convert to lower-case

corpus = tm_map(corpus, tolower)


# IMPORTANT NOTE: If you are using the latest version of the tm package, 
#you will need to run the following line before continuing (it converts corpus to a Plain Text Document). 
#This is a recent change having to do with the tolower function that occurred after this video was recorded.

# corpus = tm_map(corpus, PlainTextDocument)


# 2. Remove punctuation

corpus = tm_map(corpus, removePunctuation)



# 3. Look at stop words, 
stopwords("english")[1:10]#all 10 stop words and remove them

# 4. Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))



# 5. Stem document : go, gone, going are all different avatars, dont need them
      #oranges orange are same etc

corpus = tm_map(corpus, stemDocument)

#every single word becomes a coln, every single tweet becoems frequency: DTM(Document Term Matrix)
frequencies = DocumentTermMatrix(corpus)
#Upto here we have created our DTM with 1181 rows and 3281 colns



# 6. Look at matrix 

inspect(frequencies[1000:1005,505:515]) #among them, only idea is shown once
#this is called sparse matrix, with too many 0 and few numbers

# 7. Check for sparsity

findFreqTerms(frequencies, lowfreq=20)

# 8. Remove sparse terms

sparse = removeSparseTerms(frequencies, 0.995)#99.5% of words to be considered in further that atleast appear in 5 tweets
#or if a word is not appearing atleast 5itmes, remove them
#less frequency of words

# 9. Convert above sparse to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))

# 10. Make all variable names R-friendly

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# 11. Add dependent variable, Last Step, as we need Negative Tweets

tweetsSparse$Negative = tweets$Negative

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=tweetsSparse, method="class")

prp(tweetCART,extra=2)#these three words

# Evaluate the performance of the model
predictCART = predict(tweetCART, data=tweetsSparse, type="class")

table(tweetsSparse$Negative, predictCART)

# Compute accuracy



# Baseline accuracy 

table(tweetsSparse$Negative)



# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=tweetsSparse)
varImpPlot(tweetRF)

# Make predictions:
predictRF = predict(tweetRF, data=tweetsSparse)

table(tweetsSparse$Negative, predictRF)

