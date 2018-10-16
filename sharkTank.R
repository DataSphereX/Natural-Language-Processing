## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
shark <- read.csv("E:/1greatla/Web and Social Media Analytics/Assignment/Shark Tank Companies-1.csv")

## ------------------------------------------------------------------------
summary(shark)

## ------------------------------------------------------------------------
head(shark)
names(shark)

## ------------------------------------------------------------------------
library(tm)
library(SnowballC)
library(wordcloud)
corpus <- Corpus(VectorSource(shark$description))
wordcloud(corpus, max.words = 50, colors=rainbow(7))

## ------------------------------------------------------------------------
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
stopwords("english")[1:10]
corpus <- tm_map(corpus, removeWords, c(stopwords('english')))
corpus <- tm_map(corpus, stemDocument)
wordcloud(corpus, max.words = 50, colors=rainbow(7))

## ------------------------------------------------------------------------
frequencies = DocumentTermMatrix(corpus)
#inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies, lowfreq = 20)

## ------------------------------------------------------------------------
sparse <- removeSparseTerms(frequencies, 0.995)
sharkSparse <- as.data.frame(as.matrix(sparse))
colnames(sharkSparse) <- make.names(colnames(sharkSparse))
names(shark)

## ------------------------------------------------------------------------
sharkSparse$deal <- as.factor(shark$deal)

## ------------------------------------------------------------------------
library(rpart)
library(rpart.plot)
sharkCART <- rpart(deal ~ ., data=sharkSparse, method="class")
rpart.plot(sharkCART)

## ------------------------------------------------------------------------
sharkCARTPred <- predict(sharkCART, data=sharkSparse)
sharkCARTCM <- table("Actual" = sharkSparse$deal, "Prediction" = sharkCARTPred[,2] > 0.5)

## ------------------------------------------------------------------------
sharkLr <- glm(deal~., data = sharkSparse, family = "binomial")
sharkLrCM <- table("Actual" = sharkSparse$deal, "Prediction" = sharkLr$fitted.values > 0.5)

install.packages('glmnet')
library(glmnet)
?glmnet

names(sharkSparse[!names(sharkSparse) %in%c("deal", "ratio")  ])

sharkSparse$deal

sharkLr <- glmnet(x = as.matrix(sharkSparse[!names(sharkSparse) %in%c("deal", "ratio")  ]), y = sharkSparse$deal,  family = "binomial")

sharkLRPred <- predict(sharkLr, newx = as.matrix(sharkSparse[!names(sharkSparse) %in%c("deal", "ratio") ]))

head(sharkLRPred)

sharkLrCM <- table("Actual" = sharkSparse$deal, "Prediction" = sharkLRPred > 0.5)


## ------------------------------------------------------------------------
sharkRF <- randomForest(deal~., data = sharkSparse, ntree=100)
sharkRFCM <- table("Actual" = sharkSparse$deal, "Prediction" = sharkRF$predicted > 0.5)
varImpPlot(sharkRF)

## ------------------------------------------------------------------------
accuraryCART <- (sharkCARTCM[1]+sharkCARTCM[4])/sum(sharkCARTCM)
accuraryLr <- (sharkLrCM[1]+sharkLrCM[4])/sum(sharkLrCM)
accuraryRF <- (sharkRFCM[1]+sharkRFCM[4])/sum(sharkRFCM)

round(c("CART" = accuraryCART, "LR" = accuraryLr, "RF" = accuraryRF) * 100, 2)

## ------------------------------------------------------------------------
sharkSparse$ratio <- shark$askedFor/shark$valuation

## ------------------------------------------------------------------------
sharkCARTImp <- rpart(deal ~ ., data=sharkSparse, method="class")
rpart.plot(sharkCARTImp)

## ------------------------------------------------------------------------
sharkCARTPredImp <- predict(sharkCART, data=sharkSparse)
sharkCARTImpCM <- table("Actual" = sharkSparse$deal, "Prediction" = sharkCARTPredImp[,2] > 0.5)

## ------------------------------------------------------------------------
sharkImpLr <- glm(deal~., data = sharkSparse, family = "binomial")
sharkImpLrCM <- table("Actual" = sharkSparse$deal, "Prediction" = sharkImpLr$fitted.values > 0.5)

## ------------------------------------------------------------------------
sharkImpRF <- randomForest(deal~., data = sharkSparse, ntree=100)
sharkImpRFCM <- table("Actual" = sharkSparse$deal, "Prediction" = sharkImpRF$predicted > 0.5)
varImpPlot(sharkImpRF)

## ------------------------------------------------------------------------
accuraryCART <- (sharkCARTImpCM[1]+sharkCARTImpCM[4])/sum(sharkCARTImpCM)
accuraryLr <- (sharkImpLrCM[1]+sharkImpLrCM[4])/sum(sharkImpLrCM)
accuraryRF <- (sharkImpRFCM[1]+sharkImpRFCM[4])/sum(sharkImpRFCM)

round(c("CART" = accuraryCART, "LR" = accuraryLr, "RF" = accuraryRF) * 100, 2)

