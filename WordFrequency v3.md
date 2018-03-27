---
title: "Capstone_toxicity"
author: "Teena"
date: '2018-03-26'
output:
  pdf_document: default
  word_document: default
  html_document:
    df_print: paged
---


# install packages

```r
# build ROC which isthe same as AUC , confusion matrix , graphing the AUC plot


#install.packages("data.table")
#install.packages("tm")
#update.packages("tm",  checkBuilt = TRUE)
#install.packages("SnowballC")
#install.packages("rsconnect")
#install.packages("dplyr")
#install.packages("tidytext")
#install.packages("mldr")
#install.packages("Hmisc")
#install.packages("ggplot2")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("stringr")
#install.packages("xgboost")
#install.packages("DT")
#install.packages("dplyr")
#install.packages("caret")
#library(caret)
#library(dplyr)
#library(DT)
#library(xgboost)
#library(stringr)
#library("wordcloud")
#library("RColorBrewer")
#library(ggplot2)
#library(Hmisc)
#library(mldr)
#library("data.table")
#library("tm")
#library(SnowballC)
#library(rsconnect)
#library(dplyr)
#library(tidytext)
```

importing of training and test data

```r
setwd("/Users/teenathampan/CapstoneProject/Project/")
traindata = fread("train.csv", header = "auto", sep="auto", nrows=-1L,blank.lines.skip=TRUE, encoding="UTF-8")
testdata = fread("test.csv", header = "auto", sep="auto", nrows=-1L,blank.lines.skip=TRUE, encoding="UTF-8")

# marking data as train and test data as well as creating dummy class columns in order to clean comments at the same time

traindata$type <- "train"

testdata$toxic<- 0
testdata$severe_toxic<- 0
testdata$obscene<- 0
testdata$threat<- 0
testdata$insult<- 0
testdata$identity_hate<- 0
testdata$type<- "test"

dataset <- rbind(traindata, testdata)

str(dataset)
```

```
## Classes 'data.table' and 'data.frame':	312735 obs. of  9 variables:
##  $ id           : chr  "0000997932d777bf" "000103f0d9cfb60f" "000113f07ec002fd" "0001b41b1c6bb37e" ...
##  $ comment_text : chr  "Explanation\nWhy the edits made under my username Hardcore Metallica Fan were reverted? They weren't vandalisms"| __truncated__ "D'aww! He matches this background colour I'm seemingly stuck with. Thanks.  (talk) 21:51, January 11, 2016 (UTC)" "Hey man, I'm really not trying to edit war. It's just that this guy is constantly removing relevant information"| __truncated__ "\"\"\nMore\nI can't make any real suggestions on improvement - I wondered if the section statistics should be l"| __truncated__ ...
##  $ toxic        : num  0 0 0 0 0 0 1 0 0 0 ...
##  $ severe_toxic : num  0 0 0 0 0 0 1 0 0 0 ...
##  $ obscene      : num  0 0 0 0 0 0 1 0 0 0 ...
##  $ threat       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ insult       : num  0 0 0 0 0 0 1 0 0 0 ...
##  $ identity_hate: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ type         : chr  "train" "train" "train" "train" ...
##  - attr(*, ".internal.selfref")=<externalptr>
```


```r
# function to clean comment field

clean_text<- function(text){
  
  # text to lower case
  text <- tolower(text)
  
  # remove linebreaks
  text<- gsub("\n", " ", text)
  
  # remove extra white spaces to one space
  text<- gsub("\\s+", " ", text)
  
  # transform short forms
  text<- gsub("'ll", " will", text)
  text<- gsub("i'm", "i am", text)
  text<- gsub("'re", " are", text)
  text<- gsub("'s", " is", text)
  text<- gsub("'ve", " have", text)
  text<- gsub("'d", " would", text)
  text<- gsub("can't", "can not", text)
  text<- gsub("don't", "do not", text)
  text<- gsub("doesn't", "does not", text)
  text<- gsub("isn't", "is not", text)
  text<- gsub("aren't", "are not", text)
  text<- gsub("couldn't", "could not", text)
  text<- gsub("mustn't", "must not", text)
  text<- gsub("didn't", "did not", text)
  text<- gsub("weren't", "were not", text)
  
  # remove incorrect text
  text<- gsub("f+u+c+k+\\b", "fuck", text)
  
  # remove graphics
  text<- gsub("[^[:graph:]]", " ", text)
  # remove punctuation
  text<- gsub("[[:punct:]]", " ", text)
  # remove digits
  text<- gsub("[[:digit:]]", " ", text)
  
  # strip multiple whitspace to one
  text<- gsub("\\s+", " ", text)
  
  # remove "shittext"
  text <- gsub("\\b(a|e)w+\\b", "AWWWW", text)
  text <- gsub("\\b(y)a+\\b", "YAAAA", text)
  text <- gsub("\\b(w)w+\\b", "WWWWW", text)
  text <- gsub("\\b((l+)(a+))+\\b", "LALALA", text)
  text <- gsub("(w+)(o+)(h+)(o+)", "WOHOO", text)
  text <- gsub("\\b(d?(u+)(n+)?(h+))\\b", "UUUHHH", text)
  text <- gsub("\\b(a+)(r+)(g+)(h+)\\b", "ARGH", text)
  text <- gsub("\\b(a+)(w+)(h+)\\b", "AAAWWHH", text)
  text <- gsub("\\b(p+)(s+)(h+)\\b", "SHHHHH", text)
  text <- gsub("\\b((s+)(e+)?(h+))+\\b", "SHHHHH", text)
  text <- gsub("\\b(s+)(o+)\\b", "", text)
  text <- gsub("\\b(h+)(m+)\\b", "HHMM", text)
  text <- gsub("\\b((b+)(l+)(a+)(h+)?)+\\b", "BLABLA", text)
  text <- gsub("\\b((y+)(e+)(a+)(h+)?)+\\b", "YEAH", text)
  text <- gsub("\\b((z+)?(o+)(m+)(f+)?(g+))+\\b", "OMG", text)
  text <- gsub("aa(a+)", "a", text)
  text <- gsub("ee(e+)", "e", text)
  text <- gsub("i(i+)", "i", text)
  text <- gsub("oo(o+)", "o", text)
  text <- gsub("uu(u+)", "u", text)
  text <- gsub("\\b(u(u+))\\b", "u", text)
  text <- gsub("y(y+)", "y", text)
  text <- gsub("hh(h+)", "h", text)
  text <- gsub("gg(g+)", "g", text)
  text <- gsub("tt(t+)\\b", "t", text)
  text <- gsub("(tt(t+))", "tt", text)
  text <- gsub("mm(m+)", "m", text)
  text <- gsub("ff(f+)", "f", text)
  text <- gsub("cc(c+)", "c", text)
  text <- gsub("\\b(kkk)\\b", "KKK", text)
  text <- gsub("\\b(pkk)\\b", "PKK", text)
  text <- gsub("kk(k+)", "kk", text)
  text <- gsub("fukk", "fuck", text)
  text <- gsub("k(k+)\\b", "k", text)
  text <- gsub("f+u+c+k+\\b", "fuck", text)
  text <- gsub("((a+)|(h+)){3,}", "HAHEHI", text)
  text <- gsub("mothjer", "mother", text)
  text <- gsub("wikipedia ", " ", text)
  text <- gsub("wiki ", " ", text)
  
  #remove non ascii words
  text <- gsub("[^\x20-\x7e]+", " ", text)
  
  # remove stopwords
otherstopwords<- c("can", "will", "don", "now", "just", "also", "may", "get", "well", "need", "say", "way", "want", "see", "read", "look", "stop", "like", "really", "however", "let", "ask", "used", "made", "much", "utc", "added", "didn", "sure", "put", "better", "using", "tell", "anything", "one", "two", "wiki", "wikipedia", "first", "second", "however", "hahehi", "peopl", "talk", "page", "edit", "articl", "user", "make", "put", "far", "bit", "well", "still", "much", "one", "two", "don", "now", "even", "article", "articles", "edit", "edits", "page", "pages","talk", "editor", "ax", "edu", "subject", "lines", "like", "likes", "line","uh", "oh", "also", "get", "just", "hi", "hello", "ok", "ja", "editing", "edited","dont", "wikipedia", "hey", "however", "id", "yeah", "yo", "use", "need", "take", "give", "say", "user", "day", "want", "tell", "even", "look", "one", "make", "come", "see", "said", "now", "know", "talk", "read", "time", "sentence", "ain't", "wow", "image", "jpg", "copyright","wikiproject", "background color", "align", "px", "pixel",
                      "org", "com", "en", "ip", "ip address", "http", "www", "html", "htm",
                      "wikimedia", "https", "httpimg", "url", "urls", "utc", "uhm",
                      "i", "me", "my", "myself", "we", "our", "ours", "ourselves",
                      "you", "your", "yours", "yourself", "yourselves", 
                      "he", "him", "his", "himself", 
                      "she", "her", "hers", "herself", 
                      "it", "its", "itself",    
                      "they", "them", "their", "theirs", "themselves",
                      "i'm", "you're", "he's", "i've", "you've", "we've", "we're",
                      "she's", "it's", "they're", "they've", 
                      "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", 
                      "i'll", "you'll", "he'll", "she'll", "we'll", "they'll",
                      "what", "which", "who", "whom", "this", "that", "these", "those",
                      "am", "can", "will", "not",
                      "is", "was", "were", "have", "has", "had", "having", "wasn't", "weren't", "hasn't",
                      "are", "cannot", "isn't", "aren't", "doesn't", "don't", "can't", "couldn't", "mustn't", "didn't",    
                      "haven't", "hadn't", "won't", "wouldn't",  
                      "do", "does", "did", "doing", "would", "should", "could",  
                      "be", "been", "being", "ought", "shan't", "shouldn't", "let's", "that's", "who's", "what's", "here's",
                      "there's", "when's", "where's", "why's", "how's", "a", "an", "the", "and", "but", "if",
                      "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against",
                      "between", "into", "through", "during", "before", "after", "above", "below", "to", "from",
                      "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once",
                      "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more",
                      "most", "other", "some", "such", "no", "nor", "only", "own", "same", "so", "than",
                      "too", "very")
text <- removeWords(text, stopwords("en"))
text <- removeWords(text, otherstopwords)
  
   return(unname(text))
}
```

cleaned dataset

```r
dataset$cleanedtext <- clean_text(dataset$comment_text)

# separate out training and test data

train <-subset(dataset, type=="train",select=c(-comment_text, -type))
test<-subset(dataset, type=="test",select=c(id, cleanedtext))
```

exploratory analysis - toxic, severe_toxic, obscene, threat, insult, identity_hate / toxic, obscene, insult are thee classes that go together

```r
summary(train)
```

```
##       id                toxic          severe_toxic         obscene       
##  Length:159571      Min.   :0.00000   Min.   :0.000000   Min.   :0.00000  
##  Class :character   1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:0.00000  
##  Mode  :character   Median :0.00000   Median :0.000000   Median :0.00000  
##                     Mean   :0.09584   Mean   :0.009996   Mean   :0.05295  
##                     3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:0.00000  
##                     Max.   :1.00000   Max.   :1.000000   Max.   :1.00000  
##      threat             insult        identity_hate     
##  Min.   :0.000000   Min.   :0.00000   Min.   :0.000000  
##  1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.000000  
##  Median :0.000000   Median :0.00000   Median :0.000000  
##  Mean   :0.002995   Mean   :0.04936   Mean   :0.008805  
##  3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.000000  
##  Max.   :1.000000   Max.   :1.00000   Max.   :1.000000  
##  cleanedtext       
##  Length:159571     
##  Class :character  
##  Mode  :character  
##                    
##                    
## 
```

```r
str(train)
```

```
## Classes 'data.table' and 'data.frame':	159571 obs. of  8 variables:
##  $ id           : chr  "0000997932d777bf" "000103f0d9cfb60f" "000113f07ec002fd" "0001b41b1c6bb37e" ...
##  $ toxic        : num  0 0 0 0 0 0 1 0 0 0 ...
##  $ severe_toxic : num  0 0 0 0 0 0 1 0 0 0 ...
##  $ obscene      : num  0 0 0 0 0 0 1 0 0 0 ...
##  $ threat       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ insult       : num  0 0 0 0 0 0 1 0 0 0 ...
##  $ identity_hate: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ cleanedtext  : chr  "explanation       username hardcore metallica fan  reverted    vandalisms  closure   gas   voted  new york doll"| __truncated__ "d AWWWW  matches  background colour   seemingly stuck  thanks  january  " " man     trying   war      guy  constantly removing relevant information  talking     instead      seems  care "| __truncated__ "       real suggestions  improvement  wondered   section statistics   later    subsection  types  accidents  th"| __truncated__ ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
dim(train)
```

```
## [1] 159571      8
```

```r
colSums(sapply(train, is.na))
```

```
##            id         toxic  severe_toxic       obscene        threat 
##             0             0             0             0             0 
##        insult identity_hate   cleanedtext 
##             0             0             0
```

```r
# plotting number of documents with each toxicity category
train2<-train[,c(2:7)]
x<-barplot(colSums(train2), ylim = c(0,20000), xlab="type of toxicity", ylab="frequency", main ="Frequency of each toxicity level")
y<-as.matrix(colSums(train2))
text(x, y, labels=as.character(y), pos = 3, cex=1)
```

![plot of chunk unnamed-chunk-99](figure/unnamed-chunk-99-1.png)

```r
train$toxcount<-rowSums(train[,2:7])

# plotting number of documents with the multiple labels per document
Num_Class<-table(train$toxcount)
tox_class<-as.data.frame(Num_Class, row.names = NULL, responseName = "Num_Doc", sep=" ")
colnames(tox_class)[colnames(tox_class)=="Var1"] <- "Num_of_classes"
c1<-ggplot(data = subset(tox_class, Num_of_classes!=0), aes(x=Num_of_classes, y=Num_Doc)) + geom_bar(stat="identity")+ggtitle("Document frequency for multi labels")+geom_text(aes(label = Num_Doc), vjust = 1.5, color = "red")
ggsave("Document frequency for multi labels.png", width=297, height =210, units = "mm")
```

TDM, words and wordcloud for label toxic test

```r
# load the data as a corpus
train_to <- train[toxic==1]$cleanedtext
train_to <- Corpus(VectorSource(train_to))  %>%
  tm_map(stemDocument)

## build  a term document matrix
tdm_to<-TermDocumentMatrix(train_to)
m_to<-as.matrix(tdm_to)
v_to<-sort(rowSums(m_to), decreasing=TRUE)
d_to<-data.frame(word = names(v_to), freq=v_to)

## generate wordcloud
set.seed(1234)
wordcloud(words=d_to$word, freq=d_to$freq, min.freq=1, max.words = 200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
```

![plot of chunk unnamed-chunk-100](figure/unnamed-chunk-100-1.png)

TDM, words and wordcloud for label severe_toxic test

```r
# load the data as a corpus
train_st <- train[severe_toxic==1]$cleanedtext
train_st <- Corpus(VectorSource(train_st))  %>%
  tm_map(stemDocument)

## build  a term document matrix
tdm_st<-TermDocumentMatrix(train_st)
m_st<-as.matrix(tdm_st)
v_st<-sort(rowSums(m_st), decreasing=TRUE)
d_st<-data.frame(word = names(v_st), freq=v_st)

## generate wordcloud
set.seed(1234)
wordcloud(words=d_st$word, freq=d_st$freq, min.freq=1, max.words = 200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
```

![plot of chunk unnamed-chunk-101](figure/unnamed-chunk-101-1.png)
TDM, words and wordcloud for label obscene test

```r
# load the data as a corpus
train_o <- train[obscene==1]$cleanedtext
train_o <- Corpus(VectorSource(train_o))  %>%
  tm_map(stemDocument)

## build  a term document matrix
tdm_o<-TermDocumentMatrix(train_o)
m_o<-as.matrix(tdm_o)
v_o<-sort(rowSums(m_o), decreasing=TRUE)
d_o<-data.frame(word = names(v_o), freq=v_o)

## generate wordcloud
set.seed(1234)
wordcloud(words=d_o$word, freq=d_o$freq, min.freq=1, max.words = 200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
```

![plot of chunk unnamed-chunk-102](figure/unnamed-chunk-102-1.png)

TDM, words and wordcloud for label threat test

```r
# load the data as a corpus
train_t <- train[threat==1]$cleanedtext
train_t <- Corpus(VectorSource(train_t))  %>%
  tm_map(stemDocument)

## build  a term document matrix
tdm_t<-TermDocumentMatrix(train_t)
m_t<-as.matrix(tdm_t)
v_t<-sort(rowSums(m_t), decreasing=TRUE)
d_t<-data.frame(word = names(v_t), freq=v_t)

## generate wordcloud
set.seed(1234)
wordcloud(words=d_t$word, freq=d_t$freq, min.freq=1, max.words = 200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
```

![plot of chunk unnamed-chunk-103](figure/unnamed-chunk-103-1.png)

TDM, words and wordcloud for label insult test

```r
# load the data as a corpus
train_i <- train[insult==1]$cleanedtext
train_i <- Corpus(VectorSource(train_i))  %>%
  tm_map(stemDocument)

## build  a term document matrix
tdm_i<-TermDocumentMatrix(train_i)
m_i<-as.matrix(tdm_i)
v_i<-sort(rowSums(m_i), decreasing=TRUE)
d_i<-data.frame(word = names(v_i), freq=v_i)

## generate wordcloud
set.seed(1234)
wordcloud(words=d_i$word, freq=d_i$freq, min.freq=1, max.words = 200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
```

![plot of chunk unnamed-chunk-104](figure/unnamed-chunk-104-1.png)

TDM, words and wordcloud for label identity_hate  test

```r
traindata_ih<-subset(traindata, identity_hate==1)
# load the data as a corpus
train_ih <- train[identity_hate==1]$cleanedtext
train_ih <- Corpus(VectorSource(train_ih))  %>%
  tm_map(stemDocument)

## build  a term document matrix
tdm_ih<-TermDocumentMatrix(train_ih)
m_ih<-as.matrix(tdm_ih)
v_ih<-sort(rowSums(m_ih), decreasing=TRUE)
d_ih<-data.frame(word = names(v_ih), freq=v_ih)

## generate wordcloud
set.seed(1234)
wordcloud(words=d_ih$word, freq=d_ih$freq, min.freq=1, max.words = 200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
```

![plot of chunk unnamed-chunk-105](figure/unnamed-chunk-105-1.png)


#Tokenisation of the sentences

The sentences are broken up into words.       


```r
trainWords <- train %>%
  unnest_tokens(word, cleanedtext) %>%
  count(toxic,severe_toxic,obscene,threat,insult,identity_hate,word) %>%
  ungroup()

datatable(head(trainWords,20), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

#Unique Categories of Text

The combinations of `toxic,severe toxic,obscene,threat,insult and identity hate` will create unique categories. We will display those categories here.           


```r
trainWords <- train %>%
  unnest_tokens(word, cleanedtext) %>%
  count(toxic,severe_toxic,obscene,threat,insult,identity_hate,word) %>%
  ungroup()

total_words <- trainWords %>% 
  group_by(toxic,severe_toxic,obscene,threat,insult,identity_hate) %>% 
  summarize(total = sum(n))

total_words
```

```
## # A tibble: 41 x 7
## # Groups:   toxic, severe_toxic, obscene, threat, insult [?]
##    toxic severe_toxic obscene threat insult identity_hate   total
##    <dbl>        <dbl>   <dbl>  <dbl>  <dbl>         <dbl>   <int>
##  1    0.           0.      0.     0.     0.            0. 4290726
##  2    0.           0.      0.     0.     0.            1.    2167
##  3    0.           0.      0.     0.     1.            0.    7422
##  4    0.           0.      0.     0.     1.            1.     813
##  5    0.           0.      0.     1.     0.            0.     286
##  6    0.           0.      0.     1.     1.            0.     563
##  7    0.           0.      1.     0.     0.            0.   12643
##  8    0.           0.      1.     0.     0.            1.      66
##  9    0.           0.      1.     0.     1.            0.    6192
## 10    0.           0.      1.     0.     1.            1.     563
## # ... with 31 more rows
```



## The Math
>  TF(t) = (Number of times term t appears in a document) / (Total number of terms in the document)         
IDF(t) = log_e(Total number of documents / Number of documents with term t in it).         
Value = TF * IDF


## Twenty Most Important words

Here using **TF-IDF** , we investigate the **Twenty Most Important words**                



```r
Category =1:41
fillColor = "#8db600"
fillColor2 = "#ffbf00"

total_words$Category = Category

trainWords <- left_join(trainWords, total_words)

#Now we are ready to use the bind_tf_idf which computes the tf-idf for each term. 
trainWords <- trainWords %>%
  bind_tf_idf(word, Category, n)


plot_trainWords <- trainWords %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_trainWords %>% 
  top_n(20) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(fill = fillColor) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme_bw()
```

![plot of chunk unnamed-chunk-108](figure/unnamed-chunk-108-1.png)



##Toxic TF-IDF

We plot the TF-IDF for the Toxic Comments



```r
plot_trainWords %>%
  filter(toxic == 1 ) %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(fill = fillColor2) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme_bw()
```

![plot of chunk unnamed-chunk-109](figure/unnamed-chunk-109-1.png)


##Severe Toxic TF-IDF

We plot the TF-IDF for the Severe Toxic Comments



```r
plot_trainWords %>%
  filter(severe_toxic == 1 ) %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(fill = fillColor2) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme_bw()
```

![plot of chunk unnamed-chunk-110](figure/unnamed-chunk-110-1.png)

##Obscene TF-IDF

We plot the TF-IDF for the Obscene Comments



```r
plot_trainWords %>%
  filter(obscene == 1 ) %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(fill = fillColor2) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme_bw()
```

![plot of chunk unnamed-chunk-111](figure/unnamed-chunk-111-1.png)

##Threat TF-IDF

We plot the TF-IDF for the Threat Comments



```r
plot_trainWords %>%
  filter(threat == 1 ) %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(fill = fillColor2) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme_bw()
```

![plot of chunk unnamed-chunk-112](figure/unnamed-chunk-112-1.png)


##Insult TF-IDF

We plot the TF-IDF for the Insult Comments



```r
plot_trainWords %>%
  filter(insult == 1 ) %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(fill = fillColor2) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme_bw()
```

![plot of chunk unnamed-chunk-113](figure/unnamed-chunk-113-1.png)


##Identity Hate TF-IDF

We plot the TF-IDF for the Identity hate Comments



```r
plot_trainWords %>%
  filter(identity_hate == 1 ) %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(fill = fillColor2) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme_bw()
```

![plot of chunk unnamed-chunk-114](figure/unnamed-chunk-114-1.png)

# calculating dtm for test and train data 


```r
corpus = VCorpus(VectorSource(train$cleanedtext))
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.99)
dataset = as.data.frame(as.matrix(dtm))


corpus = VCorpus(VectorSource(test$cleanedtext))%>%
  tm_map(stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.99)
datasetTest = as.data.frame(as.matrix(dtm))

## brining all the common words together from train and text

colnamesSame = intersect(colnames(dataset),colnames(datasetTest))

dataset = dataset[ , (colnames(dataset) %in% colnamesSame)]
datasetTest = datasetTest[ , (colnames(datasetTest) %in% colnamesSame)]
```


#Modelling using XGBoost


##Toxic Calculation

We calculate the various targets and predict the probablities


```r
# above to avoid package loading messages 

dataset2 = dataset
dataset2$toxic = train$toxic
dataset2$toxic = as.factor(dataset2$toxic)
levels(dataset2$toxic) = make.names(unique(dataset2$toxic))

formula = toxic ~ .

fitControl <- trainControl(method="none",classProbs=TRUE, summaryFunction=twoClassSummary)

# (nrounds = 500, #number of iterations
# eta = .05, #learning rate lies between 0.01 - 0.30
# max_depth = 3, #controls the depth of the tree default is 6 but range 0 - Inf
# gamma = 0, #controls regularization to prevent overfitting, default is 0
# lambda = 0, #controls L2 regularization on weight to prevent overfitting
# colsample_bytree = .8, #controls the number of features supplied to a tree lies between 0.5 - 0.9
# min_child_weight = 1, # leaf node has a min sum of instance wgt < min_child_wgt, the tree splitting stops.
# subsample = 1 # controls the number of samples supplied to the tree range 0 - 1 with default at 1

xgbGrid <- expand.grid(nrounds = 500, 
                       eta = .3,
                       max_depth = 6,
                       gamma = 0,
                       colsample_bytree = .8, 
                       min_child_weight = 1, 
                       subsample = 1) 


set.seed(13)

ToxicXGB = train(formula, data = dataset2,
                 method = "xgbTree",trControl = fitControl,
                 tuneGrid = xgbGrid,na.action = na.pass,metric="ROC", maximize=FALSE)

predToxic = predict(ToxicXGB,datasetTest,type ='prob')
plot(predToxic)
```

![plot of chunk unnamed-chunk-116](figure/unnamed-chunk-116-1.png)

```r
#####################################################################################################
```


##Severe Toxic Calculation

We calculate the various targets and predict the probablities


```r
dataset2 = dataset
dataset2$severe_toxic = train$severe_toxic
dataset2$severe_toxic = as.factor(dataset2$severe_toxic)
levels(dataset2$severe_toxic) = make.names(unique(dataset2$severe_toxic))

formula = severe_toxic ~ .

set.seed(13)

SToxicXGB = train(formula, data = dataset2,
                 method = "xgbTree",trControl = fitControl,
                 tuneGrid = xgbGrid,na.action = na.pass,metric="ROC", maximize=FALSE)

predSToxic = predict(SToxicXGB,datasetTest,type ='prob')

#####################################################################################################
```


##Obscene Toxic Calculation

We calculate the various targets and predict the probablities


```r
dataset2 = dataset
dataset2$obscene = train$obscene
dataset2$obscene = as.factor(dataset2$obscene)
levels(dataset2$obscene) = make.names(unique(dataset2$obscene))

formula = obscene ~ .

set.seed(13)

ObsceneXGB = train(formula, data = dataset2,
                 method = "xgbTree",trControl = fitControl,
                 tuneGrid = xgbGrid,na.action = na.pass,metric="ROC", maximize=FALSE)

predObscene = predict(ObsceneXGB,datasetTest,type ='prob')


#####################################################################################################
```



##Threat Toxic Calculation

We calculate the various targets and predict the probablities


```r
dataset2 = dataset
dataset2$threat = train$threat
dataset2$threat = as.factor(dataset2$threat)
levels(dataset2$threat) = make.names(unique(dataset2$threat))

formula = threat ~ .

set.seed(13)

ThreatXGB = train(formula, data = dataset2,
                 method = "xgbTree",trControl = fitControl,
                 tuneGrid = xgbGrid,na.action = na.pass,metric="ROC", maximize=FALSE)

predThreat = predict(ThreatXGB,datasetTest,type ='prob')


#####################################################################################################
```


##Insult Toxic Calculation

We calculate the various targets and predict the probablities


```r
dataset2 = dataset
dataset2$insult = train$insult
dataset2$insult = as.factor(dataset2$insult)
levels(dataset2$insult) = make.names(unique(dataset2$insult))

formula = insult ~ .

set.seed(13)

InsultXGB = train(formula, data = dataset2,
                 method = "xgbTree",trControl = fitControl,
                 tuneGrid = xgbGrid,na.action = na.pass,metric="ROC", maximize=FALSE)

predInsult = predict(InsultXGB,datasetTest,type ='prob')


#####################################################################################################
```


##Identity Hate Toxic Calculation

We calculate the various targets and predict the probablities


```r
dataset2 = dataset
dataset2$identity_hate = train$identity_hate
dataset2$identity_hate = as.factor(dataset2$identity_hate)
levels(dataset2$identity_hate) = make.names(unique(dataset2$identity_hate))

formula = identity_hate ~ .

set.seed(13)

IHXGB = train(formula, data = dataset2,
                 method = "xgbTree",trControl = fitControl,
                 tuneGrid = xgbGrid,na.action = na.pass,metric="ROC", maximize=FALSE)

predIH = predict(IHXGB,datasetTest,type ='prob')


#####################################################################################################
```


#submission

```r
submission =testdata = fread("sample_submission.csv", header = "auto", sep="auto", nrows=-1L,blank.lines.skip=TRUE, encoding="UTF-8")

submission$toxic = predToxic$X1
submission$severe_toxic = predSToxic$X1
submission$obscene = predObscene$X1
submission$threat = predThreat$X1
submission$insult = predInsult$X1
submission$identity_hate = predIH$X1

# Write it to file
write.csv(submission, 'ToxicCommentsMar262018.csv', row.names = F)
```





