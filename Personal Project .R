#text sementics 
library(stringr)
library(tidyverse)
library(tidytext)
#load csv file into R
setwd("Data science/Text Analytics with R/Personal Project/")
mychild <- read.csv("236_The Jungle Book.csv")
#check the first 20 observations 
head(mychild ,n=20)

#show the structure of my dataframe
str(mychild)

#change to tidytext format , this converts to lowercase

tidy_child <- mychild %>%
  unnest_tokens(word, text)#strip out the words

#remove stopwords
data(stop_words)

#custom stop words, add word thou into lexicon
custom_stop_words <- bind_rows(tibble(word = c("thou"),  
                                      lexicon = c("custom")), 
                               stop_words)
#remove stopwords and add tokens
tidy_child1 <- mychild %>%
  select(-gutenberg_id) %>%#remove id
  unnest_tokens(word, text)%>% #tokens
  anti_join(custom_stop_words) #remove stopwords


#count common words(PLOT THIS)
common_word_child <- tidy_child1 %>% count(word,sort=TRUE)%>%head(10)
cw <-common_word_child %>%ggplot(aes(reorder(x=word,n),y=n))+
  geom_bar(stat="identity",color="black",fill="blue")+coord_flip()
cw +labs(title= "Common words in The Jungle Book", y="count",x=NULL)
#sentiment analysis (word and sentiment)
get_sentiments("nrc")%>%count(sentiment)
#select all sentiment in  nrc 
nrc <- get_sentiments("nrc")%>%
  select(word,sentiment)
nrc
#child sentient analysis
child_senti <- tidy_child1 %>%
  inner_join(nrc)%>%
  count(sentiment,sort=TRUE)%>%
  mutate(sentiment =replace_na(sentiment,replace="none"))
#check for nrc composition
child_word_compo <- tidy_child1 %>%
  inner_join(nrc)%>%
  count(word,sentiment,sort=TRUE)
 ##plot

child_word_compo %>%
  inner_join(get_sentiments("nrc"))%>%
  group_by(sentiment)%>%
  top_n(10)%>% ungroup() %>%mutate(word = reorder(word, n))%>% 
  filter(sentiment %in% c("fear","anticipation","positive","anger", "negative","trust"))%>% 
  ggplot(aes(x=word,y=n ,fill=sentiment))+
  geom_bar(stat="identity",show.legend = FALSE)+coord_flip()+
  #show y
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(x= "Top six sentimentment word composition",y = NULL)

#compare different lexicons
nrc_neg_pos_child <-tidy_child1 %>%
  mutate(linenumber=row_number(word))%>%
  inner_join(get_sentiments("nrc"))%>%
  filter(sentiment %in% c("positive","negative"))%>%
  mutate (method="NRC")

bing_neg_pos_child <-tidy_child1 %>%
  mutate(linenumber=row_number(word))%>%
  inner_join(get_sentiments("bing"))%>%
  mutate (method="Bing")
#combine afinn and nrc  and count senti method per 80lines
# Title: Text mining with R
# *    Author: Julia Silge and David Robinson
# *    Date: 2021
# *    Availability: https://www.tidytextmining.com/sentiment.html

#put senti and count in diff columns
nrc_bing_child <- bind_rows(bing_neg_pos_child,nrc_neg_pos_child )%>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
#afinn
afinn_neg_pos_child <-tidy_child1%>%
  mutate(linenumber=row_number(word))%>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value))%>%
  mutate (method="AFINN")
#combine three lexicon for same novel 
bind_rows(afinn_neg_pos_child,nrc_bing_child ) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_bar(stat="identity",show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")



##BOOK2
  adult_book <- read.csv("1400_Great Expectations.csv")
  str(adult_book)
  adult_book1<- str_trim(adult_book)
  #str_subset("^(\\[)\\d+\\s+(\\w+\\])")%>%
  
  adult_book1 <- adult_book1 %>% str_remove_all("^(\\[)\\d+\\s+(\\w+\\])")
  adult_book1 <-adult_book[-1]
  #remove edition
  clean_ad_bk <- adult_book %>%
    filter(!str_detect(text, "^(\\[)\\d+\\s+(\\w+\\])"),!str_detect(text, "\\[A-Za-z]+\\/\\\w+\\."))
 
  
  c_stop_words <- bind_rows(tibble(word = c("found","word","looked","coming"),  
                                        lexicon = c("custom")), 
                                 stop_words)
  
  
  tidy_adult <-clean_ad_bk %>% 
  select(-gutenberg_id) %>%
  #detect chapter with non-digits/roman numerals and accumulate it
  mutate(chapter= cumsum(str_detect(text,("^Chapter[\\D+]"))))%>%
  ungroup()%>%
  unnest_tokens(word, text,)%>% #tokens
  mutate(word = str_trim(word)) %>%#remove any spacing before /after word
  filter(str_detect(word, "[a-z']$"),
         !word %in% c_stop_words$word)


#common words
common_word_adult <- tidy_adult %>%
  count(word,sort=TRUE)
library(wordcloud)

#plot common words in adult book
library(tidyverse)
common_word_adult%>%head(10)%>%
  ggplot(aes(reorder(n,x=word),y=n))+
  geom_bar(stat="identity",fill="aquamarine1",color="black")+
  coord_flip()+ 
  labs(title="Common Words in Great Expectations",y=NULL,x=NULL)
#sentiment analysis

  nrc_adult_sentiment <- tidy_adult %>%
  inner_join(get_sentiments("nrc"))%>%
    count(word,sentiment,sort=TRUE)%>%
    group_by(sentiment)%>%
    top_n(10) %>%
    ungroup()
  
  #plot 
  nrc_adult_sentiment %>%mutate(word = reorder(word, n)) %>%
    filter(sentiment %in% c("anticipation","disgust","positive", "negative","fear","trust"))%>% 
    ggplot(aes(x=n,y=word,fill=sentiment))+
    geom_bar(stat="identity",show.legend = FALSE)+
    facet_wrap(~sentiment, scales = "free_y") +
    labs(x = "Contribution to sentiment",
         y = NULL)

#compare sentiments analysis 
nrc_neg_pos_adult <-tidy_adult %>%
  mutate(linenumber=row_number(word))%>%
  inner_join(get_sentiments("nrc"))%>%
  filter(sentiment %in% c("positive","negative"))%>%
  mutate (method="NRC")
               
bing_neg_pos_adult <-tidy_adult %>%
  mutate(linenumber=row_number(word))%>%
  inner_join(get_sentiments("bing"))%>%
  mutate (method="Bing")
#combine afinn and nrc  and count senti method per 80lines
#put senti and count in diff columns
nrc_bing_adult <- bind_rows(bing_neg_pos_adult,nrc_neg_pos_adult)%>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


#
afinn_neg_pos_adult <-tidy_adult %>%
  mutate(linenumber=row_number(word))%>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value))%>%
  mutate (method="AFINN")
   
#combine three lexicon for same novel 
bind_rows(afinn_neg_pos_adult,nrc_bing_adult ) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_bar(stat="identity",show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

###compare ADULT and CHILD book
child.com <- tidy_child1 %>% 
 mutate(linenumber=row_number(word))%>%
  mutate (book="The Jungle Book")

adult.com <- tidy_adult %>% 
  select(-chapter)%>% 
 mutate(linenumber=row_number(word))%>%
  mutate (book="Great Expectations")
##sentiment comparison between books
adult_child <- bind_rows(child.com,adult.com)%>%
  inner_join(get_sentiments("bing"))%>%
  count(book,index = linenumber %/% 80, sentiment)%>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
#plot
adult_child %>%
  ggplot(aes(index, sentiment, fill = book)) +
  geom_bar(stat="identity",show.legend = FALSE) +
  facet_wrap(~book, nrow = 2, scales = "free_y")
