library(tidyverse)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_lines(url, n_max = 3)
dat <- read_csv(url,col_names=FALSE)
