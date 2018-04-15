
library(dplyr)
library(dbplyr)

con <- DBI::dbConnect(RPostgres::Postgres(), 
                      host = 'localhost',
                      port = 5432,
                      db = 'gisdata',
                      user = 'gisuser',
                      password = rstudioapi::askForPassword())

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

cl <- 'carrier'
vl <- 'DL'

cl2 <- enquo(cl)

fl = interp(c == v, c = cl, v = vl)
interp(~y == x, .values=list(y = as.name(col_name), x = value))

quo(
  tbl(con, 'flights') %>%
    filter(!!rlang::sym(cl) == vl)
    collect()
  )

query <- tbl(con, 'flights') %>%
  filter(!!rlang::sym(cl) %in% vl)

dbplyr::sql_render(query)
