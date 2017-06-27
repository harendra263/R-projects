library(tm)
options(header=FALSE, stringsAsFactors = FALSE,FileEncoding="latin1")
setwd("C:/Users/brai/Desktop/RFiles")
text <- readLines("RAI2009.txt")
 
corpus <- Corpus(VectorSource(text))
 
#Clean-up
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
cleanset <- tm_map(corpus, removeWords, stopwords("english"))
cleanset <- tm_map(cleanset, stripWhitespace)
cleanset <- tm_map(cleanset ,removeWords,c("fig","per"))


#Build term document matrix
cleanset <- tm_map(cleanset, PlainTextDocument)
dtm <- TermDocumentMatrix(corpus, control = list(c(1,Inf)))

# inspect frequent words
findFreqTerms(dtm, lowfreq=10)

#Bar plot
termFrequency <- rowSums(as.matrix(dtm))
termFrequency <- subset(termFrequency, termFrequency>=10)
library(ggplot2)
barplot(termFrequency, las=2,col=rainbow(20))

#WORD CLOUD
install.packages("wordcloud")
library(wordcloud)
m<- as.matrix(dtm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )

wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=2, random.order=F, colors=grayLevels)
