library(dplyr)
library(nycflights13)
flights <- flights[1:5,]
flights

#1.Select_Function
select(flights, year, month) #Extracting the required row to our liking.
flights %>%  select(contains("_")) #Here, the "Pipe Symbo" <- %>% can be used to refer something. 
#The contains function returns everything within the quotes.
flights %>% select(ends_with("time"))
#This is similar to that of contains functions, but with a better controle. we shall now see a similar example with the starts with function.
flights %>% select(starts_with("arr"))

#2.Filter_Function
flights %>% filter(dep_delay <= 0)
#we can display a particular set of variables with conditionality.
flights %>% filter(!is.na(dep_delay))
#Displays all values of dep_delay the have no missing values.
flights %>% filter(dep_delay > 0) %>%  select(ends_with("time"))
#From flights, it first filters all the rows with dep_delay of greater than 0 and displays only the variables that end with "time".