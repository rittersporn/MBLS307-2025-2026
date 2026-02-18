#Excercise 5.2.1
library(tidyverse)
#tidying up table 2
new_table2<-table2 %>% 
  pivot_wider(
    id_cols = c(country,year),
    names_from = type,
    values_from = count
  )
#calculating the rate per 10000 in another column
new_table2 %>% 
  mutate(rate=cases/population*10000)

