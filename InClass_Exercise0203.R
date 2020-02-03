library(dplyr)
df_mtcars <- mtcars
head(df_mtcars)

#nesting
filter(df_mtcars,mpg>20)
#get 10 samples of that
sample_n(filter(df_mtcars,mpg>20),10)
#now to arrange the sample in descending order
arrange(sample_n(filter(df_mtcars,mpg>20),10),desc(mpg))
#assign this result to a variable called results_mpg
results_mpg <- arrange(sample_n(filter(df_mtcars,mpg>20),10),desc(mpg))
results_mpg

#do the same thing by multiple assignments
a1 <- filter(df_mtcars)
a2 <- sample_n(a1,5)
results_mpg_des <- arrange(a2,desc(mpg))
results_mpg_des

library(dplyr)
df_mtcars %>% filter(mpg>20) %>% sample_n(5) %>% arrange(desc(mpg))
