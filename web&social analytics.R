setwd("E:/1greatla/Web and Social Media Analytics")
mydata <- read.csv("01_Spambase.csv")

attach(mydata)
View(mydata)

#Dependent variable as a factor:
mydata$Spam <- as.factor(mydata$Spam)

#Find the number of Spam mails:
table(mydata$Spam)

#Build LR Model:
# Step 1: Base line Accuracy:
2788/ 4601

# Step 2: 
spamModelLR <- glm(Spam~., data=mydata, family="binomial")
View(spamModelLR)

#Step3: Finding Accuracy:
spamPred <- predict(spamModelLR, data=mydata, type = "response")

table(mydata$Spam, spamPred>0.5)
##Accuracy= 2666+1619/ 4601

#CART Model:
library(rpart)
library(rpart.plot)

spamCART <- rpart(Spam~., data = mydata, method = "class")
prp(spamCART, extra=2)
rpart.plot(spamCART)
predictCART <- predict(spamCART, data=mydata, type= "class")


##Random Forest:
library(randomForest)
spamRF <- randomForest(Spam~., data=mydata)
varImpPlot(spamRF)


######## PHASE 2:   Creating DTM: ###############
tweets <- read.csv("02_tweets.csv", stringsAsFactors = FALSE)

#Dependent variable:
##only Negative tweets are considered:

tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)

#Installing new Packages:
library(tm)
library(SnowballC)

# create Corpus: <Put in a basket of text from tweet column to clean the data>

corpus <- Corpus(VectorSource(tweets$Tweet)) 

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
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))

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
tweetssparse$Negative <- tweets$Negative

#Build a CART Model:

tweetCART = rpart(Negative~., data=tweetssparse, method="class")
rpart.plot(tweetCART)
prp(tweetCART, extra=2)

# Build RF:
tweetRF <- randomForest(Negative~., data=tweetssparse)
varImpPlot(tweetRF)

#Make of predictions:

