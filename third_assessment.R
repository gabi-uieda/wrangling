state <- c("Alabama", "Alaska", "Arizona", "Delaware", "District of Columbia")
population <- c(4779736,710231,6392017,897934,601723)
tab1 <- data.frame(state, population)

state <- c("Alabama", "Alaska", "Arizona", "California", "Colorado", "Connecticut")
electoral_votes <- c(9,3,11,55,9,7)
tab2 <- data.frame(state, electoral_votes)

dat <- left_join(tab1, tab2)
dat2 <- semi_join(tab1, tab2)

x <- c("a","b")
y <- c("a","a")
df1 <- data.frame(x,y)

x <- c("a","a")
y <- c("a","b")
df2 <- data.frame(x,y)

final <- setdiff(df1, df2)

install.packages("Lahman")
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()
top_names <- top %>% left_join(Master) %>% select(playerID, nameFirst, nameLast, HR)

Salaries %>% as_tibble()
top_salary <- Salaries %>% filter(yearID == 2016) %>% right_join(top_names) %>% select(nameFirst, nameLast, teamID, HR, salary)

AwardsPlayers %>% as_tibble()
awards_2016 <- AwardsPlayers %>% filter(yearID == 2016) %>% select(playerID)
top_10_names <- top_names %>% select(playerID)
intersect(awards_2016, top_10_names)
setdiff(awards_2016, top_10_names)
