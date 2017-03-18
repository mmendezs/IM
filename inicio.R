a <- 'hola mundo'
library(dplyr)

summary(survey)

survey %>% select(starts_with('P')) %>% 
  mutate(replace(0, NA))

survey <- mutate_all(survey,funs(replace(., starts_with('P') ==0, NA)))
