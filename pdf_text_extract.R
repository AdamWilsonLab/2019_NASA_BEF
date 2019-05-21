# process NASA questions
# 
library(tidyverse)
library(pdftools)
library(foreach)

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


f=data.frame(
  pdfs=list.files("/Users/adamw/Documents/Work/Workshops/2020_NASA_Biodiversity/NASA Biodiversity White Papers/",
                  pattern="pdf",recursive = T,full=T),stringsAsFactors = F)

f$file=basename(f$pdf)
f=f[!duplicated(f$file),] #drop duplicates

text=foreach(i=1:nrow(f)) %do%
  pdf_text(f$pdfs[i])

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

doc=text%>%
  VectorSource()%>%
  Corpus()%>%
  tm_map(toSpace, "/")%>%
  tm_map(toSpace, "@")%>%
  tm_map(toSpace, "\\|")%>%
  tm_map(toSpace, "")%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, stopwords("english"))%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)%>%
  tm_map(removeWords, c("question", "","use","-","answer","etc","per","answering","_")) 



dtm <- TermDocumentMatrix(doc)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 50)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=300, random.order=T, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

