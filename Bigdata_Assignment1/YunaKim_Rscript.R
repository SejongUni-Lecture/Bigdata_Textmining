library(pdftools)
library(stringr)

yuna_text<-pdf_text("Yuna_Kim.pdf")
yuna_tidy_word <- str_c(yuna_text, collapse = " ") %>%
  str_trunc(31828, side="right") %>%
  str_replace_all("[[:space:]]{1,}", " ") %>%
  str_replace_all("[^[:ascii:]]+"," ") %>%
  tolower() %>%
  str_replace_all("['][sS] "," ") %>%
  str_replace_all(" u\\.s\\. "," usa ") %>%
  str_replace_all(" r\\&b "," rnb ") %>%
  str_replace_all("\\[[[:digit:]]+\\]|\\([[:digit:]]+\\)","") %>%
  str_replace_all("[[:punct:]^]+","") %>%
  str_replace_all("[[:digit:]]+","") %>%
  str_replace_all("[[:space:]]{1,}"," ") %>%
  str_split(" ") %>%
  unlist()
yuna_tidy_word_freq<-yuna_tidy_word %>%
  table() %>%
  sort(decreasing=TRUE)

library(wordcloud)
pal<-brewer.pal(8,"Dark2")
set.seed(405)
wordcloud(words=names(yuna_tidy_word_freq),
          freq=yuna_tidy_word_freq,
          min.freq=5,
          max.words=500,
          random.order=FALSE,
          rot.per=0.1,
          scale=c(4,0.3),
          colors=pal)

#불용어 처리
library(tm)
stopwords("en")[1:10]
length(stopwords("en"))
stopwords("SMART")[1:10]
length(stopwords("SMART"))

yuna_words_nostop<-yuna_tidy_word[!yuna_tidy_word %in% stopwords("SMART")]
sort(table(yuna_words_nostop), decreasing = T)[1:50]