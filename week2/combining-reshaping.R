library(tidyverse)

####################################################################################
# 12.2.1. Exercise 2
# Compute the rate for table2, and table 4a + table4b. You will need
# to perform four operations:
#   1. Extract the number of TB cases per country per year.
#   2. Extract the matching population per country per year.
#   3. Divide cases by population, and multiply by 10000.
#   4. Store back in the appropriate place.

# Which representation is easiest to work with? Which is hardest? Why?
# Add your answer as a comment.

# TABLE2
table2 %>%
  pivot_wider(names_from=type, values_from=count) %>%
  mutate(cases_by_pop=(cases/population * 10000))

# TABLE4A + TABLE4B
table4a_edit <- pivot_longer(table4a, names_to="year", values_to="cases",2:3)
table4b_edit <- pivot_longer(table4b, names_to="year",values_to="population",2:3)
inner_join(table4a_edit, table4b_edit) %>%
  mutate(cases_by_pop=(cases/population * 10000))

# table2 is easier to work with becuase it involves less manipulation, only requiring
# a pivot_wider, while the other representation requires pivot_longer and an inner join

####################################################################################
# 12.3.3 Exercise 1
# 1. Why are pivot_longer() and pivot_wider() not perfectly symmetrical?
# Carefully consider the following example:
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

# (Hint: look at the variable types and think about column names.)
# pivot_wider loses the column name (from the return column)
# pivot_longer loses the original variable type (year goes from dbl to chr)


# pivot_longer() has a names_ptypes argument, e.g.  names_ptypes = list(year = double()). 
# What does it do? Add your answer as a comment.
# It changes the variable type, seems like it casts the variable as a chosen type to 
# ensure type consistency between pivots.

####################################################################################
# 12.3.3 Exercise 3
# What would happen if you widen this table? Why? 
# How could you add a new column to uniquely identify each value?
#  Add your answers as a comment.
people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
) 


# attempting to widen this table would result in errors because one name corresponds to several age entires
# the following would add a new column to uniquely identify each value
# people %>% 
#   group_by(name,names) %>%
#   mutate(row_id=row_number()) %>% 
#   pivot_wider(id_cols = c(name, row_id), names_from=names, values_from=values)
