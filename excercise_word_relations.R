setwd("c:\\pending\\work")

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("dplyr")
library("KoNLP")

useNIADic()

filePath <- "issue_2017.txt"
text <- readLines(filePath)

df <- do.call("rbind", lapply(text, as.data.frame))

df <- rename(df,text=`X[[i]]`)

removeTwit <- function(x){
  gsub("@[[:graph:]]*","", x)
}

df$ptext <- sapply(df$text, removeTwit)

removeURL <- function(x){
  gsub("http://[[:graph:]]*", "", x)
}

df$ptext <- sapply(df$ptext, removeURL)

df$ptext <- gsub("COB", "", df$ptext)
df$ptext <- gsub("eng", "", df$ptext)
df$ptext <- gsub("Eng", "", df$ptext)
df$ptext <- gsub("kor", "", df$ptext)
df$ptext <- gsub("Kor", "", df$ptext)

useNIADic()

df$ptext <- sapply(df$ptext, function(x) {
  paste(extractNoun(x), collapse = " ")
})

df$ptext <- Filter(function(x){nchar(x)>=2}, df$ptext)

myCorpus <- Corpus(VectorSource(df$ptext))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, tolower)
myStopwords <- c(stopwords("english"), "rt", "언급", "직원", "경우", "지급", "검토", "요청", "변경", "회사", "한국",
                 "관련", "추가")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

myTdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(4, Inf)))
Encoding(rownames(myTdm)) <- "UTF-8"

findFreqTerms(myTdm, lowfreq = 20)

findAssocs(myTdm, "퇴직금", 0.20)

myTdm2 <- removeSparseTerms(myTdm, sparse = 0.98)
m2 <- as.matrix(myTdm2)

distMatrix <- dist(scale(m2))

fit <- hclust(distMatrix, method = "ward.D")

plot(fit)

rect.hclust(fit, k=10)



nouns.temp <- extractNoun(text)
nouns <- unlist(nouns.temp)
nouns <- str_replace_all(nouns, "\\W", "")


#불필요한 단어 제거 작업
nouns <- gsub("\\d+", "", nouns) ##숫자업애기
nouns <- str_replace_all(nouns,"[^[:alpha:]]","")
nouns <- gsub("tax", "Tax", nouns)
nouns <- gsub("Incentive", "incentive", nouns)
nouns <- gsub("Eng", "", nouns)
nouns <- gsub("관련", "", nouns)
nouns <- gsub("요청", "", nouns)
nouns <- gsub("직원", "", nouns)
nouns <- gsub("지급", "", nouns)
nouns <- gsub("검토", "", nouns)
nouns <- gsub("Kor", "", nouns)
nouns <- gsub("추가", "", nouns)
nouns <- gsub("질의", "", nouns)
nouns <- gsub("경우", "", nouns)
nouns <- gsub("회사", "", nouns)
nouns <- gsub("근로자", "", nouns)
nouns <- gsub("변경", "", nouns)
nouns <- gsub("해당", "", nouns)
nouns <- gsub("여부", "", nouns)
nouns <- gsub("적용", "", nouns)
nouns <- gsub("관련하", "", nouns)
nouns <- gsub("포함", "", nouns)
nouns <- gsub("답변", "", nouns)
nouns <- gsub("진행", "", nouns)
nouns <- gsub("가능", "", nouns)
nouns <- gsub("회신", "", nouns)
nouns <- gsub("업무", "", nouns)
nouns <- gsub("필요", "", nouns)
nouns <- gsub("한국", "", nouns)
nouns <- gsub("예정", "", nouns)
nouns <- gsub("이슈", "", nouns)
nouns <- gsub("바람", "", nouns)
nouns <- gsub("한지", "", nouns)
nouns <- gsub("내용", "", nouns)
nouns <- gsub("사항", "", nouns)
nouns <- gsub("확인", "", nouns)
nouns <- gsub("제도", "", nouns)
nouns <- gsub("첨부", "", nouns)
nouns <- gsub("산정", "", nouns)
nouns <- gsub("근무", "", nouns)
nouns <- gsub("도입", "", nouns)
nouns <- gsub("업데이트", "", nouns)
nouns <- gsub("가능", "", nouns)
nouns <- gsub("계산", "", nouns)
nouns <- gsub("제공", "", nouns)
nouns <- gsub("자료", "", nouns)
nouns <- gsub("영향", "", nouns)
nouns <- gsub("제공", "", nouns)
nouns <- gsub("letter", "", nouns)
nouns <- gsub("조언", "", nouns)
nouns <- gsub("기존", "", nouns)
nouns <- gsub("이전", "", nouns)
nouns <- gsub("법률", "", nouns)
nouns <- gsub("해도", "", nouns)
nouns <- gsub("Cc", "", nouns)
nouns <- gsub("call", "", nouns)
nouns <- gsub("대상", "", nouns)
nouns <- gsub("to", "", nouns)
nouns <- gsub("자문", "", nouns)
nouns <- gsub("판결", "", nouns)
nouns <- gsub("설명", "", nouns)
nouns <- gsub("부서", "", nouns)
nouns <- gsub("고려", "", nouns)
nouns <- gsub("US", "", nouns)
nouns <- gsub("동안", "", nouns)
nouns <- gsub("of", "", nouns)
nouns <- gsub("방식", "", nouns)
nouns <- gsub("기타", "", nouns)
nouns <- gsub("전달", "", nouns)
nouns <- gsub("본사", "", nouns)
nouns <- gsub("USD", "", nouns)
nouns <- gsub("기재", "", nouns)
nouns <- gsub("개월", "", nouns)
nouns <- gsub("fee", "", nouns)
nouns <- gsub("sck", "", nouns)
nouns <- gsub("Sck", "", nouns)
nouns <- gsub("의", "", nouns)
nouns <- gsub("미지", "", nouns)
nouns <- gsub("들이", "", nouns)
nouns <- gsub("에서", "", nouns)
nouns <- gsub("처리", "", nouns)
nouns <- gsub("legal", "", nouns)
nouns <- gsub("방법", "", nouns)
nouns <- gsub("agreement", "", nouns)
nouns <- gsub("중간", "", nouns)
nouns <- gsub("나감", "", nouns)
nouns <- gsub("방안", "", nouns)
nouns <- gsub("사회", "", nouns)
nouns <- gsub("은행", "", nouns)
nouns <- gsub("부분", "", nouns)
nouns <- gsub("반영", "", nouns)
nouns <- gsub("클라이언트", "", nouns)
nouns <- gsub("의견", "", nouns)
nouns <- gsub("조항", "", nouns)
nouns <- gsub("local", "", nouns)
nouns <- gsub("and", "", nouns)
nouns <- gsub("시행", "", nouns)
nouns <- gsub("영문", "", nouns)
nouns <- gsub("법상", "", nouns)
nouns <- gsub("문제", "", nouns)
nouns <- gsub("영업", "", nouns)
nouns <- gsub("사용", "", nouns)
nouns <- gsub("fee", "", nouns)
nouns <- gsub("ea", "", nouns)
nouns <- gsub("인상", "", nouns)
nouns <- gsub("share", "", nouns)
nouns <- gsub("confidentiality", "", nouns)
nouns <- gsub("confidential", "", nouns)
nouns <- gsub("day", "", nouns)
nouns <- gsub("program", "", nouns)
nouns <- gsub("redundancy", "", nouns)
nouns <- gsub("package", "", nouns)
nouns <- gsub("목요일", "", nouns)
nouns <- gsub("데이트", "", nouns)
nouns <- gsub("결정", "", nouns)
nouns <- gsub("수정", "", nouns)
nouns <- gsub("이상", "", nouns)
nouns <- gsub("이것", "", nouns)
nouns <- gsub("올해", "", nouns)
nouns <- gsub("이번", "", nouns)
nouns <- gsub("summary", "", nouns)
nouns <- gsub("데이트", "", nouns)
nouns <- gsub("Client", "", nouns)
nouns <- gsub("하지", "", nouns)
nouns <- gsub("기간", "", nouns)
nouns <- gsub("하고", "", nouns)
nouns <- gsub("Client", "", nouns)
nouns <- gsub("작성", "", nouns)
nouns <- gsub("이사", "", nouns)
nouns <- gsub("대표", "", nouns)
nouns <- gsub("금액", "", nouns)
nouns <- gsub("하기", "", nouns)
nouns <- gsub("요구", "", nouns)
nouns <- gsub("하려", "", nouns)
nouns <- gsub("plan", "", nouns)
nouns <- gsub("Plan", "", nouns)
nouns <- gsub("범위", "", nouns)
nouns <- gsub("기준", "", nouns)
nouns <- gsub("발생", "", nouns)
nouns <- gsub("이후", "", nouns)
nouns <- gsub("운영", "", nouns)
nouns <- gsub("일부", "", nouns)
nouns <- gsub("운영", "", nouns)
nouns <- gsub("소송", "", nouns)
nouns <- gsub("비용", "", nouns)
nouns <- gsub("국문", "", nouns)
nouns <- gsub("도움", "", nouns)
nouns <- gsub("요건", "", nouns)
nouns <- gsub("절차", "", nouns)
nouns <- gsub("시스템", "", nouns)
nouns <- gsub("비용", "", nouns)
nouns <- gsub("항목", "", nouns)
nouns <- gsub("위반", "", nouns)
nouns <- gsub("on", "", nouns)
nouns <- gsub("관계", "", nouns)
nouns <- gsub("가지", "", nouns)
nouns <- gsub("번역", "", nouns)
nouns <- gsub("unit", "", nouns)
nouns <- gsub("sale", "", nouns)
nouns <- gsub("실시", "", nouns)
nouns <- gsub("차트", "", nouns)
nouns <- gsub("법정", "", nouns)
nouns <- gsub("결과", "", nouns)
nouns <- gsub("the", "", nouns)
nouns <- gsub("프로그램", "", nouns)
nouns <- gsub("아래", "", nouns)
nouns <- gsub("도움", "", nouns)
nouns <- gsub("설계", "", nouns)
nouns <- gsub("주장", "", nouns)
nouns <- gsub("제기", "", nouns)
nouns <- gsub("하나", "", nouns)
nouns <- gsub("초안", "", nouns)
nouns <- gsub("파일", "", nouns)
nouns <- gsub("제안", "", nouns)
nouns <- gsub("일반", "", nouns)
nouns <- gsub("해주", "", nouns)
nouns <- gsub("까지", "", nouns)
nouns <- gsub("버전", "", nouns)
nouns <- gsub("준비", "", nouns)
nouns <- gsub("관계", "", nouns)
nouns <- gsub("당사", "", nouns)
nouns <- gsub("실시", "", nouns)
nouns <- gsub("상담", "", nouns)
nouns <- gsub("완료", "", nouns)
nouns <- gsub("코멘트", "", nouns)
nouns <- gsub("assign", "", nouns)
nouns <- gsub("scheme", "", nouns)
nouns <- gsub("fer", "", nouns)
nouns <- gsub("pm", "", nouns)
nouns <- gsub("실시", "", nouns)
nouns <- gsub("document", "", nouns)
nouns <- gsub("fisibility", "", nouns)
nouns <- gsub("draft", "", nouns)
nouns <- gsub("법적", "", nouns)
nouns <- gsub("이익", "", nouns)
nouns <- gsub("supplement", "", nouns)
nouns <- gsub("제출", "", nouns)
nouns <- gsub("국내", "", nouns)
nouns <- gsub("개인", "", nouns)
nouns <- gsub("부담", "", nouns)
nouns <- gsub("일정", "", nouns)
nouns <- gsub("별도", "", nouns)
nouns <- gsub("인출", "", nouns)
nouns <- gsub("메일", "", nouns)
nouns <- gsub("문구", "", nouns)
nouns <- gsub("Compesation", "compensation", nouns)
nouns <- gsub("[A-z]","",nouns) ## delete all english word
nouns <- Filter(function(x){nchar(x)>=2},nouns)


wordcount <- table(unlist(nouns))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word, word=Var1, freq=Freq)


top_20 <- df_word %>% 
  arrange(desc(freq)) %>%
  head(20)

top_20

docs <- Corpus(VectorSource(df_word))


inspect(docs)

#Text transformattion

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "kor")
docs <- tm_map(docs, toSpace, "eng")
docs <- tm_map(docs, toSpace, "Eng")
docs <- tm_map(docs, toSpace, "Kor")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("관련", "검토")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
docs <- tm_map(docs, stemDocument)


#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
Encoding(rownames(m)) <- "UTF-8"
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = df_word$word, freq = df_word$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

m
rownames(m)[1:100]
word.count <-rowSums(m)
word.order <- order(word.count, decreasing =T)
freq.word <- m[word.order[1:20],]
co.matrix <- freq.word %*% t(freq.word)
co.matrix

library(qgraph)
qgraph(co.matrix, 
       label=rownames(co.matrix), 
       diag=F,
       layout = 'spring',
       edge.color='blue'
       )
