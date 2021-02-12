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
