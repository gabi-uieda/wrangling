# Part 1
Month <- c("January","February","March","April","May")
Sales <- c("$128,568","$109,523","$115,468","$122,274","$117,921")
Profit <- c("$16,234","$12,876","$17,920","$15,825","$15,437")
dat <- tibble(Month,Sales,Profit)
dat1 <- dat %>% mutate_at(2:3, parse_number)
dat2 <- dat %>% mutate_all(parse_number)
dat3 <- dat %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
  mutate_at(2:3, as.numeric)

# Part 2
not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)
pattern <- "[A-Z]$"
str_detect(animals, pattern)
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)

schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia", "U California", "California State University")
schools %>% str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% str_replace("^University of |^University ", "University of ")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)
converted <- s %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

# Part 3
day <- c("Monday", "Tuesday")
staff <- c("Mandy, Chris and Laura","Steve, Ruth and Frank")
schedule <- data.frame(day,staff)
str_split(schedule$staff,", | and ")
str_split(schedule$staff, ",\\s|\\sand\\s")
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest(cols=c(staff))

data("gapminder")
gapminder %>% 
  filter(region=="Middle Africa") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 
dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))

# Part 4
# Import raw Brexit referendum polling data from Wikipedia:
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
polls_2 <- polls %>% setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")) %>% filter(grepl("%", remain))
a <- as.numeric(str_replace(polls_2$remain, "%", ""))/100
f <- parse_number(polls_2$remain)/100
polls_2 <- polls_2 %>% mutate(remain = as.numeric(str_replace(polls_2$remain, "%", ""))/100, leave = as.numeric(str_replace(polls_2$leave, "%", ""))/100)
polls_2 <- polls_2 %>% mutate(undecided = str_replace(polls_2$undecided, "N/A","0%"))
polls_2 <- polls_2 %>% mutate(undecided = as.numeric(str_replace(polls_2$undecided, "%", ""))/100)

pattern_2 <- "\\d+\\s[a-zA-Z]+"
pattern_4 <- "[0-9]+\\s[a-zA-Z]+"
pattern_5 <- "\\d{1,2}\\s[a-zA-Z]+"
pattern_7 <- "\\d+\\s[a-zA-Z]{3,5}"

temp <- str_extract_all(polls_2$dates, pattern_2)
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
                              