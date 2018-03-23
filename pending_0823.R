rm(list = ls())
setwd("C:\\pending\\work")


library(KoNLP)
library(dplyr)

useNIADic()

txt <- readLines("issue1.txt")
head(txt)

library(stringr)
txt <- str_replace_all(txt, "\\W", "")

nouns <- extractNoun(txt)

wordcount <- table(unlist(nouns))

df_word <- as.data.frame(wordcount, stringsAsFactors=F)

df_word <- rename(df_word, word = Var1, freq = Freq)
df_word <- filter(df_word, nchar(word) >=2)


top_30 <- df_word %>% arrange(desc(freq)) %>% head(30)

set.seed(1234)

pal <- brewer.pal(8, "Dark2")
wordcloud(words = df_word$word, freq = df_word$freq, min.freq = 2, max.words = 200, random.order = F, rot.per = 0.1, scale = c(4,0.3), colors = pal)
