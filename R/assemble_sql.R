#' Assemble results stored in SQLite data base to a tibble
#'
#' Analyses generated in CapRosion are often organized in chunks of tibbles
#' stored in SQLite data bases. This function helbs to load the tibble chunks
#' from a data base and to assemble them to one results table
#'
#' @param sql_path Path of the SQLite data base as a character string
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom dbplyr src_dbi
#' @importFrom dplyr bind_rows collect src_tbls tbl
#' @importFrom lubridate now
#' @importFrom RSQLite SQLite
#'
#' @export
#'
assemble_sql_tbls <- function(sql_path, sql_name) {
  sql_con <- dbConnect(SQLite(),sql_path%//%sql_name)
  sql_db <- src_dbi(sql_con)
  tbls <- src_tbls(sql_db) %>%
    .[!grepl("sqlite",.)] %>%
    sort_by_index(.)

  sql_data <- list()
  t0 <- now()
  i_prg <- 0
  cat("Compiling", length(tbls), "tables found in", sql_path%//%sql_name, ":\n")
  for(i_tbl in tbls) {
    sql_data[[i_tbl]] <- i_tbl %>%
      tbl(sql_db, .) %>%
      collect(.)
    i_prg <- i_prg + 1
    display_progress(n = i_prg, nmax = length(tbls), t0 = t0, word = "Table")
  }
  sql_data <- bind_rows(sql_data)
  dbDisconnect(sql_con)
  finish_progress(nmax = length(tbls), t0 = t0, word = "Table")
  return(sql_data)
}


#' Sort names by their suffix number
#'
#' @param lbls Vector of character strings with numbers in name that should be
#'   sorted
#' @keywords internal
#'
sort_by_index <- function(lbls) {
  idx <- gsub("[^[:digit:]]", "", lbls) %>% as.numeric()
  pos <- order(idx)
  lbls <- lbls[pos]

  return(lbls)
}
