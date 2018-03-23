rm(list = ls())
setwd("c:\\pending\\work")



library(RColorBrewer)
library(wordcloud)
library(stringr)
library(dplyr)
library(KoNLP)

useNIADic()
#mergeUserDic(data.frame("동의절차", "ncn"))
#mergeUserDic(data.frame("근로시간", "ncn"))
#mergeUserDic(data.frame("취업규칙", "ncn"))


for (i in 2013:2017){

  issue.y <- paste0("issue_",i,".txt")

## Aalysis by year
pend1 <- readLines(issue.y)
pend2 <- sapply(pend1, 
                extractNoun, 
                USE.NAMES = F
                )

head(unlist(pend2),30)

pend3 <- unlist(pend2)
head(pend3)

#특수문자 없애기
pend3 <- str_replace_all(pend3, "\\W", "")


#불필요한 단어 제거 작업
pend3 <- gsub("\\d+", "", pend3) ##숫자업애기
pend3 <- str_replace_all(pend3,"[^[:alpha:]]","")
pend3 <- gsub("tax", "Tax", pend3)
pend3 <- gsub("Incentive", "incentive", pend3)
pend3 <- gsub("Eng", "", pend3)
pend3 <- gsub("관련", "", pend3)
pend3 <- gsub("요청", "", pend3)
pend3 <- gsub("직원", "", pend3)
pend3 <- gsub("지급", "", pend3)
pend3 <- gsub("검토", "", pend3)
pend3 <- gsub("Kor", "", pend3)
pend3 <- gsub("추가", "", pend3)
pend3 <- gsub("질의", "", pend3)
pend3 <- gsub("경우", "", pend3)
pend3 <- gsub("회사", "", pend3)
pend3 <- gsub("근로자", "", pend3)
pend3 <- gsub("변경", "", pend3)
pend3 <- gsub("해당", "", pend3)
pend3 <- gsub("여부", "", pend3)
pend3 <- gsub("적용", "", pend3)
pend3 <- gsub("관련하", "", pend3)
pend3 <- gsub("포함", "", pend3)
pend3 <- gsub("답변", "", pend3)
pend3 <- gsub("진행", "", pend3)
pend3 <- gsub("가능", "", pend3)
pend3 <- gsub("회신", "", pend3)
pend3 <- gsub("업무", "", pend3)
pend3 <- gsub("필요", "", pend3)
pend3 <- gsub("한국", "", pend3)
pend3 <- gsub("예정", "", pend3)
pend3 <- gsub("이슈", "", pend3)
pend3 <- gsub("바람", "", pend3)
pend3 <- gsub("한지", "", pend3)
pend3 <- gsub("내용", "", pend3)
pend3 <- gsub("사항", "", pend3)
pend3 <- gsub("확인", "", pend3)
pend3 <- gsub("제도", "", pend3)
pend3 <- gsub("첨부", "", pend3)
pend3 <- gsub("산정", "", pend3)
pend3 <- gsub("근무", "", pend3)
pend3 <- gsub("도입", "", pend3)
pend3 <- gsub("업데이트", "", pend3)
pend3 <- gsub("가능", "", pend3)
pend3 <- gsub("계산", "", pend3)
pend3 <- gsub("제공", "", pend3)
pend3 <- gsub("자료", "", pend3)
pend3 <- gsub("영향", "", pend3)
pend3 <- gsub("제공", "", pend3)
pend3 <- gsub("letter", "", pend3)
pend3 <- gsub("조언", "", pend3)
pend3 <- gsub("기존", "", pend3)
pend3 <- gsub("이전", "", pend3)
pend3 <- gsub("법률", "", pend3)
pend3 <- gsub("해도", "", pend3)
pend3 <- gsub("Cc", "", pend3)
pend3 <- gsub("call", "", pend3)
pend3 <- gsub("대상", "", pend3)
pend3 <- gsub("to", "", pend3)
pend3 <- gsub("자문", "", pend3)
pend3 <- gsub("판결", "", pend3)
pend3 <- gsub("설명", "", pend3)
pend3 <- gsub("부서", "", pend3)
pend3 <- gsub("고려", "", pend3)
pend3 <- gsub("US", "", pend3)
pend3 <- gsub("동안", "", pend3)
pend3 <- gsub("of", "", pend3)
pend3 <- gsub("방식", "", pend3)
pend3 <- gsub("기타", "", pend3)
pend3 <- gsub("전달", "", pend3)
pend3 <- gsub("본사", "", pend3)
pend3 <- gsub("USD", "", pend3)
pend3 <- gsub("기재", "", pend3)
pend3 <- gsub("개월", "", pend3)
pend3 <- gsub("fee", "", pend3)
pend3 <- gsub("sck", "", pend3)
pend3 <- gsub("Sck", "", pend3)
pend3 <- gsub("의", "", pend3)
pend3 <- gsub("미지", "", pend3)
pend3 <- gsub("들이", "", pend3)
pend3 <- gsub("에서", "", pend3)
pend3 <- gsub("처리", "", pend3)
pend3 <- gsub("legal", "", pend3)
pend3 <- gsub("방법", "", pend3)
pend3 <- gsub("agreement", "", pend3)
pend3 <- gsub("중간", "", pend3)
pend3 <- gsub("나감", "", pend3)
pend3 <- gsub("방안", "", pend3)
pend3 <- gsub("사회", "", pend3)
pend3 <- gsub("은행", "", pend3)
pend3 <- gsub("부분", "", pend3)
pend3 <- gsub("반영", "", pend3)
pend3 <- gsub("클라이언트", "", pend3)
pend3 <- gsub("의견", "", pend3)
pend3 <- gsub("조항", "", pend3)
pend3 <- gsub("local", "", pend3)
pend3 <- gsub("and", "", pend3)
pend3 <- gsub("시행", "", pend3)
pend3 <- gsub("영문", "", pend3)
pend3 <- gsub("법상", "", pend3)
pend3 <- gsub("문제", "", pend3)
pend3 <- gsub("영업", "", pend3)
pend3 <- gsub("사용", "", pend3)
pend3 <- gsub("fee", "", pend3)
pend3 <- gsub("ea", "", pend3)
pend3 <- gsub("인상", "", pend3)
pend3 <- gsub("share", "", pend3)
pend3 <- gsub("confidentiality", "", pend3)
pend3 <- gsub("confidential", "", pend3)
pend3 <- gsub("day", "", pend3)
pend3 <- gsub("program", "", pend3)
pend3 <- gsub("redundancy", "", pend3)
pend3 <- gsub("package", "", pend3)
pend3 <- gsub("목요일", "", pend3)
pend3 <- gsub("데이트", "", pend3)
pend3 <- gsub("결정", "", pend3)
pend3 <- gsub("수정", "", pend3)
pend3 <- gsub("이상", "", pend3)
pend3 <- gsub("이것", "", pend3)
pend3 <- gsub("올해", "", pend3)
pend3 <- gsub("이번", "", pend3)
pend3 <- gsub("summary", "", pend3)
pend3 <- gsub("데이트", "", pend3)
pend3 <- gsub("Client", "", pend3)
pend3 <- gsub("하지", "", pend3)
pend3 <- gsub("기간", "", pend3)
pend3 <- gsub("하고", "", pend3)
pend3 <- gsub("Client", "", pend3)
pend3 <- gsub("작성", "", pend3)
pend3 <- gsub("이사", "", pend3)
pend3 <- gsub("대표", "", pend3)
pend3 <- gsub("금액", "", pend3)
pend3 <- gsub("하기", "", pend3)
pend3 <- gsub("요구", "", pend3)
pend3 <- gsub("하려", "", pend3)
pend3 <- gsub("plan", "", pend3)
pend3 <- gsub("Plan", "", pend3)
pend3 <- gsub("범위", "", pend3)
pend3 <- gsub("기준", "", pend3)
pend3 <- gsub("발생", "", pend3)
pend3 <- gsub("이후", "", pend3)
pend3 <- gsub("운영", "", pend3)
pend3 <- gsub("일부", "", pend3)
pend3 <- gsub("운영", "", pend3)
pend3 <- gsub("소송", "", pend3)
pend3 <- gsub("비용", "", pend3)
pend3 <- gsub("국문", "", pend3)
pend3 <- gsub("도움", "", pend3)
pend3 <- gsub("요건", "", pend3)
pend3 <- gsub("절차", "", pend3)
pend3 <- gsub("시스템", "", pend3)
pend3 <- gsub("비용", "", pend3)
pend3 <- gsub("항목", "", pend3)
pend3 <- gsub("위반", "", pend3)
pend3 <- gsub("on", "", pend3)
pend3 <- gsub("관계", "", pend3)
pend3 <- gsub("가지", "", pend3)
pend3 <- gsub("번역", "", pend3)
pend3 <- gsub("unit", "", pend3)
pend3 <- gsub("sale", "", pend3)
pend3 <- gsub("실시", "", pend3)
pend3 <- gsub("차트", "", pend3)
pend3 <- gsub("법정", "", pend3)
pend3 <- gsub("결과", "", pend3)
pend3 <- gsub("the", "", pend3)
pend3 <- gsub("프로그램", "", pend3)
pend3 <- gsub("아래", "", pend3)
pend3 <- gsub("도움", "", pend3)
pend3 <- gsub("설계", "", pend3)
pend3 <- gsub("주장", "", pend3)
pend3 <- gsub("제기", "", pend3)
pend3 <- gsub("하나", "", pend3)
pend3 <- gsub("초안", "", pend3)
pend3 <- gsub("파일", "", pend3)
pend3 <- gsub("제안", "", pend3)
pend3 <- gsub("일반", "", pend3)
pend3 <- gsub("해주", "", pend3)
pend3 <- gsub("까지", "", pend3)
pend3 <- gsub("버전", "", pend3)
pend3 <- gsub("준비", "", pend3)
pend3 <- gsub("관계", "", pend3)
pend3 <- gsub("당사", "", pend3)
pend3 <- gsub("실시", "", pend3)
pend3 <- gsub("상담", "", pend3)
pend3 <- gsub("완료", "", pend3)
pend3 <- gsub("코멘트", "", pend3)
pend3 <- gsub("assign", "", pend3)
pend3 <- gsub("scheme", "", pend3)
pend3 <- gsub("fer", "", pend3)
pend3 <- gsub("pm", "", pend3)
pend3 <- gsub("실시", "", pend3)
pend3 <- gsub("document", "", pend3)
pend3 <- gsub("fisibility", "", pend3)
pend3 <- gsub("draft", "", pend3)
pend3 <- gsub("법적", "", pend3)
pend3 <- gsub("이익", "", pend3)
pend3 <- gsub("supplement", "", pend3)
pend3 <- gsub("제출", "", pend3)
pend3 <- gsub("국내", "", pend3)
pend3 <- gsub("개인", "", pend3)
pend3 <- gsub("부담", "", pend3)
pend3 <- gsub("일정", "", pend3)
pend3 <- gsub("별도", "", pend3)
pend3 <- gsub("인출", "", pend3)
pend3 <- gsub("메일", "", pend3)
pend3 <- gsub("문구", "", pend3)
pend3 <- gsub("인센티브", "incentive", pend3)
pend3 <- gsub("Compesation", "compensation", pend3)
pend3 <- gsub("bus", "", pend3)
pend3 <- gsub("진정", "", pend3)
pend3 <- gsub("문구", "", pend3)
pend3 <- gsub("fee", "", pend3)
pend3 <- gsub("Fee", "", pend3)
pend3 <- gsub("payment", "", pend3)
pend3 <- gsub("정산", "", pend3)
pend3 <- gsub("COB", "", pend3)
pend3 <- Filter(function(x){nchar(x)>=2}, pend3)

#불필요한 단어 제거 후 재 count "
write(unlist(pend3),"2_issue.y")
pend4 <- read.table("2_issue.y")
nrow(pend4)
wordcount <- table(pend4)
head(sort(wordcount, 
          decreasing = T), 30
     )

library(RColorBrewer)
pal <- brewer.pal(9,"Blues")[5:9]



set.seed(1234)

wordcloud(names(wordcount),
          freq=wordcount,
          scale=c(5,0.3),
          rot.per=0.1,
          min.freq = 4,
          random.order = F,
          colors=pal)
legend.year <- paste0("Pending list for ", i)

legend(0.4, 1,
       legend.year,
       cex=0.8,
       fill=NA,
       border=NA,
       bg="white",
       text.col = "black",
       text.font = 4,
       box.col = "white"
       )



wordcount <- table(unlist(pend3))

df_word <- as.data.frame(wordcount, stringsAsFactors = F)

df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq
)
top_20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)
top_20

library(ggplot2)

order <- arrange(top_20, freq)$word
order

legend.yggplot <- paste0("Pending List for ", i)

ggplot(data = top_20, aes(x = word, y = freq)) +
  ylim(0,150) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) +
  geom_text(aes(label = freq), hjust = -0.5) +
  ggtitle(legend.yggplot)+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "black")
  )
graph.plot <-paste0("c:\\pending\\fig\\Top 20 word for ", i,".png")
ggsave(graph.plot)
}