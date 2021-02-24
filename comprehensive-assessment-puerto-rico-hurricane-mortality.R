library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))
txt <- pdf_text(fn)
x <- txt[[9]] %>% str_split("\n")
class(x)
s <- x[[1]]
class(s)
s <- s %>% str_trim()
s[[1]]
header_index <- str_which(s,"2015")
header_index <- header_index[1]
header <- s[header_index]
header <- header %>% str_split("\\s+", simplify = TRUE)
tail_index <- str_which(s,"Total")
n <- str_count(s,"\\d+")
sum(n == 1)
s <- s[n!=1]
s <- s[-c(1:header_index,tail_index:40)]
s <- str_remove_all(s, "[^\\d\\s]")
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
tab <- s[1:30,] %>% data.frame(stringsAsFactors = FALSE) %>% setNames(c("day",header[2:5])) %>% mutate_all(as.numeric) %>% mutate(month="Sep")
mean(tab$'2015')
mean(tab$'2016')
mean(tab$'2017'[1:19])
mean(tab$'2017'[20:30])
tab <- tab %>% gather(year, deaths, -day, -month)
tab %>% filter(year!=2018) %>%
  ggplot(aes(day, deaths, color=year)) +
  geom_line() +
  geom_vline(xintercept=20, color="black") +
  ggtitle("Deaths per Day in September (2015-2017)")
