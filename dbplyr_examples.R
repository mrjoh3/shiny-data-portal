
library(dplyr)
library(dbplyr)

con <- DBI::dbConnect(RPostgres::Postgres(), 
                      host = 'localhost',
                      port = 5432,
                      db = 'gisdata',
                      user = 'gisuser',
                      password = 'kubexapa')

copy_to(con, nycflights13::flights, 'flights', 
        temporary = FALSE,
        indexes = list(
          c("year", 'month', 'day'),
          'carrier',
          'tailnum',
          'dest'
        ))

flights_db <- tbl(con, 'flights')

flt <- flights_db %>% 
  group_by(dest) %>%
  summarise(delay = mean(dep_time)) %>%
  collect()

dest <- flights_db %>% 
  select_('dest') %>%
  distinct() %>%
  collect()