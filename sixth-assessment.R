# Part 1
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

data(brexit_polls)
april <- brexit_polls %>% filter(month(startdate)==4) 
sum(april)
week <- brexit_polls %>% mutate(round_week = round_date(enddate, unit="week"))
week <- week %>% filter(round_week == "2016-06-12")
week_day <- brexit_polls %>% mutate(weekday = weekdays(enddate)) %>% group_by(weekday) %>% summarise(n()) 

data(movielens)
movielens <- movielens %>% mutate(datetime = as_datetime(timestamp, tz="UTC"))
movielens <- movielens %>% mutate(year = year(datetime), hour = hour(datetime))
year <- movielens %>% group_by(year) %>% summarise(n())
hour <- movielens %>% group_by(hour) %>% summarise(n())

# Part 2
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
data(gutenberg_metadata)
pp <- gutenberg_metadata %>% filter(!is.na(title))
pp_2 <- pp %>% mutate(pride = str_detect(pp$title, "Pride and Prejudice")) %>% filter(pride==TRUE)
download <- gutenberg_download(1342)
words <- download %>% unnest_tokens(word, text)
library(janeaustenr)
pride_text <- austen_books() %>% filter(book=="Pride & Prejudice")
words_2 <- pride_text %>% unnest_tokens(word, text)
words_2 <- words_2 %>% filter(!word %in% stop_words$word)
words_2 <- words_2 %>% select(word) %>% mutate(digit=str_detect(words_2$word,"\\d"))
words_2 <- words_2 %>% filter(digit==FALSE) %>% select(word)
sum <- words_2 %>% group_by(word) %>% summarise(qtd = n())
sum_100 <- sum %>% filter(qtd>100)

afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(words_2, afinn)
mean(afinn_sentiments$value>0)
mean(afinn_sentiments$value==4)*6065
