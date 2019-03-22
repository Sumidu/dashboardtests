# Setup Script for the database

# TEST FILES

library(DBI)
library(pool)
library(tidyverse)
library(lubridate)

source("read_personal.R")
con <- dbConnect(RSQLite::SQLite(), "mydata.sqlite")

dbListTables(con)
con %>% tbl("personal")
con %>% db_drop_table("personal")
df <- read_personal()



dbWriteTable(con, "personal", df, overwrite=T )

con %>% tbl("personal") %>% as_tibble() -> test
test %>% mutate_at(.vars = c("Planstelle_Begin", "Planstelle_Ende", "Zuordnungsbeginn", "Zuordnungsende"), as_date)


dbDisconnect(con)
