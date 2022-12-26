# Loading libraries
library("tm")
library("NLP")
library("SnowballC")
library("caTools")
library("rpart")
library("rpart.plot")
library("randomForest")

# Reading The Data
tweets<-read.csv("C:/Users/Hp/Downloads/New folder/R/error/apple tweets.csv",stringsAsFactors = FALSE)
tweets$Negative<-as.factor(tweets$Avg<=-1)
table(tweets$Negative)
#Building Corpus
corpus<-VCorpus(VectorSource(tweets$Tweet))
corpus[[1]]$content
corpus[[10]]$content
# Cleaning And Preproccessing Of The Text
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,PlainTextDocument)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeWords,c("apple",stopwords("english")))
corpus<-tm_map(corpus,stemDocument)
#making a document term matrix
DTM<-DocumentTermMatrix(corpus)
DTM
inspect(DTM[1000:1005,505:515])
dtm2<-TermDocumentMatrix(corpus)
tweets_matrix<-as.matrix(dtm2)
#removing sparse
sparse_DTM<-removeSparseTerms(DTM,.995)
#Making Wordcloud
w<-sort(rowSums(tweets_matrix),decreasing = TRUE)
w
set.seed(222)
library("wordcloud")
library("RColorBrewer")
library("wordcloud2")
wordcloud(words = names(w),freq = w,max.words = 150,random.order = FALSE,colors = brewer.pal(8,"Dark2"),scale = c(.3,.3),rot.per = .3)
# Converting DTM To a DataFrame
tweet_sparse<-as.data.frame(as.matrix(sparse_DTM))
colnames(tweet_sparse)<-make.names(colnames(tweet_sparse))
tweet_sparse$Negative<-tweets$Negative
#Split Data Into Training And Test Set
set.seed(123)
Split<-sample.split(tweet_sparse$Negative,SplitRatio = .7)
trainSparse<-subset(tweet_sparse,Split==TRUE)
testSparse<-subset(tweet_sparse,Split==FALSE)
trainSparse$Negative=as.factor(trainSparse$Negative)
testSparse$Negative=as.factor(testSparse$Negative)
#Predicting Sentiment and Accuracy

#1. Using CART
tweetCART<-rpart(Negative~.,data = trainSparse,method = "class")
prp(tweetCART)
predictCART<-predict(tweetCART,newdata = testSparse,type = "class")
Cmat_CART<-table(testSparse$Negative,predictCART)
Cmat_CART
#Sensitivity
18/55
#Specificity
294/300
#Accuracy
(294+18)/355
#Baseline Mode
Cmat_baseline<-table(testSparse$Negative)
Cmat_baseline
#Baseline Acuuracy
300/355
#CART MODEL DOES BETTER THAN THE SIMPLE BASELINE MODEL

#2. Using Randomforest Model
set.seed(123)
tweetRF<-randomForest(Negative~.,data = trainSparse)
predictRF<-predict(tweetRF,newdata=testSparse)
cmat_RF<-table(testSparse$Negative,predictRF)
cmat_RF
#Accuracy 
(293+21)/355
#RANDOM MODEL LITTLE BETTER THAN THE CART


#3.Using Logistic Regression Model
tweetLog<-glm(Negative~.,data=trainSparse,family="binomial")
tweet_predict_test<-predict(tweetLog,type = "response",newdata = testSparse)
Cmat_logreg<-table(testSparse$Negative,tweet_predict_test>0.5)
Cmat_logreg
#Accuracy
(257+34)/355
#LOGISTIC PREDICTION IS WORSE THAN BASELINE

#PREDICTING SENTIMENT ANALYSIS FOR TWEETS USING RANDOMFORREST IS HIGHLY RECOMMENDED
