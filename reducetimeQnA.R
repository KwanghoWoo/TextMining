install.packages("qgraph")
library("base64enc")
library("twitteR")
library("ROAuth")
library("RCurl")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("dplyr")
library("KoNLP")
useNIADic()

setwd("c:\\data")
filePath <- "reducetime.txt"
text <- readLines(filePath)


nDcos <- length(text)


df <- do.call("rbind", lapply(text, as.data.frame))
df <- rename(df,text=`X[[i]]`)

removeTwit <- function(x){
  gsub("@[[:graph:]]*","", x)
}

df$ptext <- sapply(df$text, removeTwit)

removeURL1 <- function(x){
  gsub("http://[[:graph:]]*", "", x)
}
df$ptext <- sapply(df$ptext, removeURL1)


removeURL2 <- function(x){
  gsub("https://[[:graph:]]*", "", x)
}


df$ptext <- sapply(df$ptext, removeURL2)

useNIADic()

df$ptext <- sapply(df$ptext, function(x) {
  paste(extractNoun(x), collapse = " ")
})


myCorpus <- Corpus(VectorSource(df$ptext))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, tolower)
myStopwords <- c(stopwords("english"), "rt")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myStopwords <- c("rt", "언급", "직원", "경우", "지급", "검토", "요청", "변경", "회사", "한국",
                 "관련", "추가", "요청", "시간", "관련", "근무", "당사")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)


myTdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(4, 10)))
Encoding(rownames(myTdm)) <- "UTF-8"

findFreqTerms(myTdm, lowfreq = 3)
mtNC <- as.matrix(myTdm)
mtrowNC <- rowSums(mtNC)
mtNC.order <- mtrowNC[order(mtrowNC, decreasing=T)]
freq.wordsNC <- sample(mtNC.order[mtNC.order>30], 25)
freq.wordsNC <- as.matrix(freq.wordsNC)

co.matrix <- freq.wordsNC %*% t(freq.wordsNC)

#library("qgraph")
#qgraph(co.matrix,
 #      labels=rownames(co.matrix),
  #     diag=FALSE,
   #    layout='spring',
#       vsize=log(diag(co.matrix)*2))

findAssocs(myTdm, "시간", 0.25)

myTdm2 <- removeSparseTerms(myTdm, sparse = 0.97)
m2 <- as.matrix(myTdm2)

distMatrix <- dist(scale(m2))

fit <- hclust(distMatrix, method = "single")

plot(fit)

rect.hclust(fit, k=10)

