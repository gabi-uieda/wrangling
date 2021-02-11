# writing the path for the directory and listing the files in it
path <- system.file("extdata", package="dslabs")
list.files(path)

# Finding the full path for the file and copy it into my directory
fullpath <- file.path(path,"murders.csv")
file.copy(fullpath, getwd()) #getwd() gives the path of the working directory

# Looking at the first 3 lines of the file to find out what type it is and if there is a header or not
read_lines("murders.csv", n_max = 3)

# Reading in the data into R
dat <- read_csv("murders.csv")

# Importing with R functions instead of tidyverse
dat2 <- read.csv("murders.csv")

# Importing with R function and avoiding turning characters into factors
dat3 <- read.csv("murders.csv", stringsAsFactors = FALSE)

# Reading the files directly from an URL with tidyverse
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat4 <- read_csv(url)

# Downloading the file to keep a local copy
download.file(url, "murders.csv")

# Download a file, give it a temporary name, read it in, and then erase the file that we downloaded.
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat5 <- read_csv(tmp_filename)
file.remove(tmp_filename)
