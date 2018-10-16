library(twitteR)
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)

consumer_key = "mvPRP2Ac2kZ1naFzutlMQWBNL"
consumer_secret = "gVJv1njh39HUTbnXhSUmpV0u78N4E26yutzGWgS6ikVjDbZbvH"
access_token = "145224949-5UcYTdet9kbCV0NoIaIsLQyAilsA5jTu7PKNc6T6"
access_secret ="N0cs27ZHO2AZjtY0vK0kqPSyn7FehuTOVBVmlUbpyeeRp"

setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)

tweets = searchTwitter("#sterlite", n = 5000, lang = "en")
tweets.df = twListToDF(tweets)






## CLEANING TWEETS

tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)

tweets.df$text <- iconv(tweets.df$text, "UTF-8","ASCII", sub="")

sent.value <- get_sentiment(tweets.df$text)
sent.value2 <- get_nrc_sentiment(tweets.df$text)

corpusRCB = Corpus(VectorSource(tweets.df$text))
wordcloud(corpusRCB,colors=rainbow(7),max.words=5000)

corpusRCB = tm_map(corpusRCB, tolower)


corpusRCB = tm_map(corpusRCB, removePunctuation)





corpusRCB = tm_map(corpusRCB, removeWords, c(stopwords("english")))


corpusRCB = tm_map(corpusRCB, stemDocument)

frequenciesRCB = DocumentTermMatrix(corpusRCB)

sparseRCB = removeSparseTerms(frequenciesRCB, 0.995)

RCBSparse = as.data.frame(as.matrix(sparseRCB))

colnames(RCBSparse) = make.names(colnames(RCBSparse))

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

RCBSparse$Polarity = category_senti

table(RCBSparse$Polarity)

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Polarity ~ ., data=RCBSparse, method="class")

prp(tweetCART,extra=2)


### Using syuzhet####

method = "custom"
custom_lexicon = data.frame(word=c("freak", "hate", "stuff"), value=c(-2,-1,-1))

AppTweets=read.csv("02_tweets.csv",stringsAsFactors = FALSE)

my_custom_values = get_sentiment(AppTweets$Tweet, method = method, lexicon = custom_lexicon)

AppTweets$Polarity=my_custom_values

write.csv(AppTweets,file)
simple_plot(sent.value)          


#######MKB file#########
setwd("E:/1greatla/Web and Social Media Analytics")
mkb=read.delim("Ex4_mkb.txt", header = FALSE, stringsAsFactors = FALSE)

mkb$V1 <- iconv(mkb$V1, "ASCII", "UTF-8", sub = "")

mkbcorpus = Corpus(VectorSource(mkb$V1))
wordcloud(mkbcorpus,colors=rainbow(7),max.words=50)

mkbcorpus = tm_map(mkbcorpus, tolower)
mkbcorpus = tm_map(mkbcorpus, removePunctuation)
mkbcorpus = tm_map(mkbcorpus, removeWords, c(stopwords("english")))
mkbcorpus = tm_map(mkbcorpus, removeNumbers)
wordcloud(mkbcorpus,colors=rainbow(7),max.words=50)

mkbFre = DocumentTermMatrix(mkbcorpus)

sentiment1=get_nrc_sentiment(mkb$V1)
sentiment2=get_sentiment(mkb$V1)
simple_plot(sentiment2)

write.csv(mkb,file = "mkb.txt")

#########################################################
#########################################################
#########Sherlock Holmes file ###########################

#NOT WORKING ##
setwd("C:/Users/sarveshwaran/Desktop/WORDIJ Try/New folder")
sherlock=read.delim("Ex3_cano.txt", header = FALSE, stringsAsFactors = FALSE)

sherlock$text <- iconv(sherlock$text, "ASCII", "UTF-8", sub = "")

sherlock$text = Corpus(VectorSource(sherlock$text))
wordcloud(mkbcorpus,colors=rainbow(7),max.words=50)

mkbcorpus = tm_map(mkbcorpus, tolower)
mkbcorpus = tm_map(mkbcorpus, removePunctuation)
mkbcorpus = tm_map(mkbcorpus, removeWords, c(stopwords("english")))
mkbcorpus = tm_map(mkbcorpus, removeNumbers)
wordcloud(mkbcorpus,colors=rainbow(7),max.words=50)

mkbFre = DocumentTermMatrix(mkbcorpus)

sentiment1=get_nrc_sentiment(mkb$V1)
sentiment2=get_sentiment(mkb$V1)
simple_plot(sentiment2)

write.csv(mkb,file = "mkb.txt")
##############################################
##########FOR KAALA###########################

tweets = searchTwitter("#kaala", n = 5000, lang = "en")
tweets.df = twListToDF(tweets)

## CLEANING TWEETS

tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)

tweets.df$text <- iconv(tweets.df$text, "UTF-8","ASCII", sub="")

sent.value <- get_sentiment(tweets.df$text)
sent.value2 <- get_nrc_sentiment(tweets.df$text)

corpusRCB = Corpus(VectorSource(tweets.df$text))
wordcloud(corpusRCB,colors=rainbow(7),max.words=5000)

corpusRCB = tm_map(corpusRCB, tolower)


corpusRCB = tm_map(corpusRCB, removePunctuation)





corpusRCB = tm_map(corpusRCB, removeWords, c(stopwords("english")))


corpusRCB = tm_map(corpusRCB, stemDocument)

frequenciesRCB = DocumentTermMatrix(corpusRCB)

sparseRCB = removeSparseTerms(frequenciesRCB, 0.995)

RCBSparse = as.data.frame(as.matrix(sparseRCB))

colnames(RCBSparse) = make.names(colnames(RCBSparse))

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

RCBSparse$Polarity = category_senti
table(RCBSparse$Polarity)

write.csv(tweets.df, file="kaala.csv")


###WORD CLOUD Google Search ##########

setwd("E:/1greatla/Web and Social Media Analytics")
google <- read.csv("google.csv", header = FALSE, stringsAsFactors = FALSE)

google$V1  <- iconv(google$V1, "ASCII", "UTF-8", sub = "")

googlecorpus = Corpus(VectorSource(google$V1))
wordcloud(googlecorpus,colors=rainbow(7),max.words=50)

googlecorpus = tm_map(googlecorpus, tolower)
googlecorpus = tm_map(googlecorpus, removePunctuation)
googlecorpus = tm_map(googlecorpus, removeWords, c(stopwords("english")))
googlecorpus = tm_map(googlecorpus, removeNumbers)
wordcloud(googlecorpus,colors=rainbow(7),max.words=50)

googleFre = DocumentTermMatrix(googlecorpus)

sentiment1=get_nrc_sentiment(google$V1)
sentiment2=get_sentiment(google$V1)
simple_plot(sentiment2)

table(RCBSparse$Polarity)

write.csv(mkb,file = "mkb.txt")