#дёргать запрос из БД
SitGetQuery <- function(query) {
  con <- dbConnect(RPostgres::Postgres(),
                   dbname = 'frag_seq_db', 
                   host = '192.168.0.145',
                   port = 5432,
                   user = 'postgres',
                   password = Sys.getenv("frag_seq_db"))
  db_output <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(db_output)
}