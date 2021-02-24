# loading the data
library(dslabs)
data(reported_heights)

# parsing into numbers
class(reported_heights$height)
x <- as.numeric(reported_heights$height)
sum(is.na(x))

# keeping only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>% filter(is.na(new_height)) %>% head(n=10)

# function to automatically keep only entries that either result in NAs when applying as numeric or are outside a range of plausible heights.
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# apply the function and get number of problematic entries
problems <- reported_heights %>% filter(not_inches(height)) %>% .$height
length(problems)

# pattern 1: x' y or x' y" or x' y\"
pattern_1 <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern_1) %>% head(n=10) %>% cat

# pattern 2: x.y or x,y
pattern_2 <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern_2) %>% head(n=10) %>% cat

# pattern 3: reported in centimeters
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81))
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

# | or
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm|inches")

# \d any digit 0-9
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
str_detect(s, "\\d")

# [] character class
s <- 0:9
str_detect(s, "[56]")
str_detect(s, "[4-7]")
s <- c("a","c","f","D","B")
str_detect(s, "[a-d]")
str_detect(s, "[a-zA-Z]")

# ^ and $ anchors (define beginning and end of the string)
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_detect(s, "^\\d$")

# {} quantifiers (possible number of times the previous entry repeats)
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
s <- c(yes, no)
str_detect(s, "^\\d{1,2}$")

# replacing -> be careful when replacing
problems %>% str_replace("feet|ft|foot", "'") %>% str_replace("inches|in|''|\"", "")

# \s space
pattern_2 <- "^[4-7]'\\s\\d{1,2}$"

# * none or more of the previous character
# ? none or once of the previous character
# + once or more of the previous character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
s <- c(yes, no)
str_detect(s, "A1*B")
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"), none_or_more = str_detect(yes, "A1*B"), none_or_once = str_detect(yes, "A1?B"), once_or_more = str_detect(yes, "A1+B"))

# () Groups -> don't change the detection procedure
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <- "(^[4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes,no)
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

# but once we define groups, we can use a function str_match() to extract the values these groups define
str_match(s, pattern_with_groups)

# Replacing a comma by a single quote, but only if it is between two digits.
str_replace(s, pattern_with_groups, "\\1'\\2")

# So to cover , . and space:
pattern_with_groups <- "(^[4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# to test out this new pattern
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% str_replace(pattern_with_groups, "\\1'\\2") %>% head

# Function that captures all the entries that can't be converted into numbers, including ones that are centimeters
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) & ((inches >= smallest & inches <= tallest) | (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}
problems <- reported_heights %>% filter(not_inches_or_cm(height)) %>% .$height
length(problems)

# Reformatting problems
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)
converted[!index]

# 2 ways to extract feet and inches and store them separately
s <- c("5'10", "6'1")
tab <- data.frame(x=s)

tab %>% separate(x, c("feet", "inches"), sep="'")

tab %>% extract(x, c("feet", "inches"), regex="(\\d)'(\\d{1,2})")

# exactly 5 or 6 feet, just the number, for example 6
# We add a '0 to convert 6 to 6'0, using groups
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")

# exactly 5 or 6 without inches, for example 6'
str_replace(s, "^([56])'?$", "\\1'0")

# inches with decimal points
str_replace(s, "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$")
              
# spaces in the end
str_trim("5 ' 9 ")
              
# meters using comma
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

# 7. spelled out numbers -> To upper and to lower case
s <- c("Five feet eight inches")
str_to_lower(s)

# putting all together
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

# to see which problems still remain:
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

# putting it al together
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"
smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

# We can check all the entries we converted using the following code:
new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

# String splitting
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
x <- str_split(lines, ",")
col_names <- x[[1]]
x <- x[-1]

# converting the list into a data frame
library(purrr) # for the map function
dat <- data.frame(map_chr(x,1),map_chr(x,2),map_chr(x,3),map_chr(x,4),map_chr(x,5)) %>%
  mutate_all(parse_guess) %>% setNames(col_names)

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

# Case Study: Extracting a Table from a PDF
library(dslabs)
data("research_funding_rates")
research_funding_rates 

# Downloading the data:
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file, mode = "wb")
txt <- pdf_text(temp_file)
file.remove(temp_file)

# keeping the second page, the one with the data
raw_data_research_funding_rates <- txt[2]
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
the_names_1 <- tab[3]
the_names_2 <- tab[4]

# the_names_1 -> remove the leading space and everything following the comma. 
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)

# the_names_2 -> trim the leading space and then split by space as we did for the first line:
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)

# Join the_names_1 and the_names_2 to generate one name for each column:
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")

# Now the actual data. Lines 6-14. 
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

# recoding the names of categorical variables
library(dslabs)
data("gapminder")
gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# display long country names
gapminder %>% 
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 

# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
