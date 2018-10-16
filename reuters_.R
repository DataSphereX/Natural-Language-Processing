setwd("E:/1greatla/Web and Social Media Analytics")
reuters <- read.csv("03_Reuters_Topic.csv", stringsAsFactors = FALSE)

#Dependent variable:
##only Negative tweets are considered:
### USe "if" statement in r-studio and create an dependent variable
View(reuters)
reuters$Feed <- as.factor(reuters$crude <= 1)
table(reuters$crude)

#Installing new Packages:
library(tm)
library(SnowballC)
# create Corpus: <Put in a basket of text from tweet column to clean the data>

corpus <- Corpus(VectorSource(reuters$Feed)) 

#Visualise the Model:
library(wordcloud)
wordcloud(corpus, colors=rainbow(7), max.words=50)

#Convert to lower-case:
corpus <- tm_map(corpus, tolower)

#Remove the Punctuation:
corpus <- tm_map(corpus, removePunctuation)

#Look at Stop words:
stopwords("english")[1:10]

#Remove stopwords and apple: (Remove certain words from this)
corpus <- tm_map(corpus, removeWords, c("the", stopwords("english")))

#Stem Document: (To compress a word from various tenses to a single basic word)
corpus <- tm_map(corpus, stemDocument)
frequencies <- DocumentTermMatrix(corpus)

#Look at matrix: 
inspect(frequencies[1000:1005, 505:515])
#########<The above matrix is a sparse matrix>#########

#Check for Sparsity:
findFreqTerms(frequencies, lowfreq = 20)
####<there are number of tweets of low frequency>

#Remove sparse terms:
sparse = removeSparseTerms(frequencies, 0.995)

#Convert to a data frame:
tweetssparse = as.data.frame(as.matrix(sparse))

#Make all variable names R-friendly:
colnames(tweetssparse) = make.names(colnames(tweetssparse))

#Add dependent variable:
reuters$crude <- reuters$Feed

#Build a CART Model:

tweetCART = rpart(crude~., data=tweetssparse, method="class")
rpart.plot(tweetCART)
prp(tweetCART, extra=2)

# Build RF:
reutersRF <- randomForest(crude~., data=tweetssparse)
varImpPlot(tweetRF)

