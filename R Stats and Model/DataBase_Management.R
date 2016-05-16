

library(RMySQL)
con <- dbConnect(MySQL(),
                 user='root',
                 password='861117',
                 dbname='eledata', 
                 host="localhost") 
dbListTables(con)
data <- dbGetQuery(con, "select * from test_table")
data$name