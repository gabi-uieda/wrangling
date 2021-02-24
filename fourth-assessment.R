library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[5]])
table1 <- html_table(nodes[[1]])
table2 <- html_table(nodes[[2]])
table3 <- html_table(nodes[[3]])
table4 <- html_table(nodes[[4]])
table19 <- html_table(nodes[[19]])
table20 <- html_table(nodes[[20]])
table21 <- html_table(nodes[[21]])

tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
library(janitor)
tab_1_clean <- tab_1 %>% row_to_names(row_number = 1)
tab_1_clean <- tab_1_clean %>% select(-No.)
tab_2_clean <- tab_2 %>% row_to_names(row_number = 1)
tab_full <- full_join(tab_1_clean,tab_2_clean,"Team")


library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
tab <- html_nodes(h, "table")
n <- vector(length=40)
for (i in 1:40){
  n[i] <- ncol(html_table(tab[[i]], fill=TRUE))
}
df <- data.frame(table = 1:40, ncolumns = n)
       