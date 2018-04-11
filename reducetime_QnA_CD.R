setwd("c:\\pending\\work")

library(KoNLP)
library(stringr)
library(wordcloud2)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(dplyr)
useNIADic()

docs <-readLines('reducetime.txt')
word <- SimplePos09(doc)

extracted <- str_match(word, '([가-힣]+)/[NPM]')

keyword <- extracted[,2]
nouns <- keyword[!is.na(keyword)]
nouns <- unlist(nouns)
nouns <- gsub("당사", "", nouns)
nouns <- gsub("현재", "", nouns)
nouns <- gsub("회사", "", nouns)
nouns <- gsub("근로시간단축", "", nouns)
nouns <- gsub("오랜", "", nouns)
nouns <- gsub("관련", "", nouns)
nouns <- gsub("직원들", "", nouns)
nouns <- gsub("영문자료", "", nouns)
nouns <- gsub("근로시간단축", "", nouns)
nouns <- gsub("양산", "", nouns)
nouns <- gsub("예정된", "", nouns)
nouns <- gsub("환노위", "", nouns)
nouns <- gsub("기본적", "", nouns)
nouns <- gsub("기능", "", nouns)
nouns <- gsub("으로", "", nouns)
nouns <- gsub("기타", "", nouns)
nouns <- gsub("근로자", "", nouns)
nouns <- gsub("시간", "", nouns)
nouns <- gsub("상의", "", nouns)
nouns <- gsub("해당", "", nouns)
nouns <- gsub("사내", "", nouns)
nouns <- gsub("의", "", nouns)
nouns <- gsub("교대조", "교대제", nouns)
nouns <- gsub("불가피", "", nouns)
nouns <- gsub("유지", "", nouns)
nouns <- gsub("실제", "", nouns)
nouns <- gsub("퇴근", "", nouns)
nouns <- gsub("다양한", "", nouns)
nouns <- gsub("작년", "", nouns)
nouns <- gsub("년부터", "", nouns)
nouns <- gsub("언론사", "", nouns)
nouns <- gsub("이후", "", nouns)

nouns <- Filter(function(x){nchar(x)>1}, nouns)

wordcount <- table(unlist(nouns))
df_word <- as.data.frame(wordcount)
df_word <- rename(df_word, word=Var1, freq=Freq)


myCorpus <- Corpus(VectorSource(df_word$word))
myTdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(4, 10)))
Encoding(rownames(myTdm)) <- "UTF-8"

findFreqTerms(myTdm, lowfreq = 5)

findAssocs(myTdm, "", 0.25)

myTdm2 <- removeSparseTerms(myTdm, sparse = 0.95)
m2 <- as.matrix(myTdm2)

distMatrix <- dist(scale(m2))

fit <- hclust(distMatrix, method = "single")

plot(fit)

rect.hclust(fit, k=10)
