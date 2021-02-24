# import a webpage into R, extract the table and convert the HTML table into a data frame
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
murders_raw <- read_html(url) %>% html_nodes("table") %>% html_table()
murders_raw <- murders_raw[[2]] %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))

# numbers with commas cannot be transformed into numbers by the usual process
murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3])

# We can use the str_detect() function to see that the columns have commas using this code.
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))

# We can then use the str_replace_all function to remove them
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

# To remove commas from all columns
remove_commas <- function(x) {
  rep <- str_replace_all(x,",","")
  as.numeric(rep)}
murders_test <- murders_raw %>% mutate_all(funs(remove_commas))

# Another way to remove the commas
test_2 <- parse_number(murders_raw$population)

# To remove commas from columns 2 and 3
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
