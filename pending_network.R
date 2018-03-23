library(rvest)
library(httr)
library(stringr)
library(tm)
library(qgraph)
library('xml2')

all.pend <- c(pend1,pend3)

ko.words <-function(doc){
d <- as.character(doc)
pos <- paste(SimplePos09(d))
  extracted <- str_match(pos, '([가-힣]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}

options(mc.cores=1)
cps <- Corpus(VectorSource(pend3))
tdm <- TermDocumentMatrix(cps) 
                          #control=list(tokenzie=ko.words,
                           #            removePuctuation=T, 
                            #           removeNumbers=T, 
                             #          wordLengths=c(2,7), 
                              #         weighting=weightBin))
dim(tdm)
tdm.matrix <- as.matrix(tdm)
#Encoding(rownames(tdm.matrix)) <- "UTF-8"
rownames(tdm.matrix)[1:100]
tdm.matrix


word.count <- rowSums(tdm.matrix)
word.order <- order(word.count, decreasing = T)
freq.words <- tdm.matrix[word.order[1:20],]
co.matrix <- freq.words %*% t(freq.words)

co.matrix

qgraph(co.matrix, labels=rownames(co.matrix), diag=F, layout='spring', edge.color='blue', vsize=log(diag(co.matrix))*2)
