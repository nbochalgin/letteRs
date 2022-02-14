#' DB connector to frag_seq_db
#'
#' @param query SQL query of character type
#'
#' @return Query result
#' @export
#'
#' @examples
#' query <- glue::glue(
#' "    SELECT *
#'        FROM frag_seq_results
#'       WHERE wgs_id = 'ch_ON_22';")
#' SitGetQuery(query)

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