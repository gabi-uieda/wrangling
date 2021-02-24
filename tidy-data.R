# An example of tidy data
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>% filter(country %in% c("South Korea", "Germany")) %>% select(country, year, fertility)
head(tidy_data)

# with tidy data we can easily make the plot
tidy_data %>% ggplot(aes(year, fertility, color=country)) + geom_point()

# but the original data was not tidy
path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# creating a new tidy dataset object using the gather() function
new_tidy_data <- wide_data %>% gather(year, fertility,`1960`:`2015`)

# instead of defining the columns that will be gathered we can define that ones that won't
new_tidy_data <- wide_data %>% gather(year, fertility,-country)

# the year column was gathered as character, we nee to turn it into numbers
new_tidy_data <- wide_data %>% gather(year, fertility,-country, convert = TRUE)

# converting tidy data into wide data using the spread() function
new_wide_data <- new_tidy_data %>% spread(year, fertility)

# another tidying example with two variables
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_data <- read_csv(filename)

# gathering
dat <- raw_data %>% gather(key, value, -country)

# separate the key column into the year and the variable type
dat %>% separate(key, c("year", "variable_name"), "_") # or, simply
dat %>% separate(key, c("year", "variable_name"),)

# separate the key column, considering that the same separator (_) is also used in the middle of the variable's name
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

# Or we can use the argument extra in the separate function
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

# Now we need to create a column for each variable with the spread() function
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>% spread(variable_name, value)

# Join the murders table and US election results table and plot electoral votes versus population
tab <- left_join(murders, results_us_election_2016, by = "state")
library(ggrepel)
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

# creating 2 tables with different states in each to illustrate the uses of the join function
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)

# left join -> a table like tab 1, but adding information of tab 2 to whatever states we have available in tab 1
left_join(tab1, tab2) # or
tab1 %>% left_join(tab2)

# right join -> a table like tab 2
tab1 %>% right_join(tab2)

# inner join -> keep rows that have information from both tables, like intersection
inner_join(tab1, tab2)

# full join -> keep all the rows of both tables and fill in the missing information with NAs, like a union
full_join(tab1, tab2)

#semi join -> lets us keep the part of the first table for which we have information in the second. It does not add the columns of the second.
semi_join(tab1, tab2)

# anti join -> is the opposite of semi join, you keep only the part for which we don't have information in the second table. It also does not add the columns of the second.
anti_join(tab1, tab2)

# bind_cols() -> binds columns and creates a tibble
bind_cols(a = 1:3, b = 4:6)

# Bind_cols can also bind data frames.
tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)

# bind_rows() -> binds rows and creates a tibble
tab4 <- tab[1:2,]
tab5 <- tab[3:4,]
bind_rows(tab4, tab5)

# set operators
intersect(1:10, 6:15)
union(1:10, 6:15)
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
setequal(1:5, 1:6)
setequal(1:5, 5:1)

# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)

# extract the table and convert the HTML table into a data frame
tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab <- tab %>% html_table

# Let's change the names of the columns that are too long
tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))

# extracting information from a guacamole recipe
h <- read_html("https://www.tudogostoso.com.br/receita/49061-guacamole-do-chef.html")
recipe <- h %>% html_node("h1") %>% html_text()
prep_time <- h %>% html_node(".dt-duration") %>% html_text()
ingredients <- h %>% html_nodes(".p-ingredient") %>% html_text()
guacamole <- list(recipe, prep_time, ingredients)
guacamole

