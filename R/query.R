#' Function that queries a data table using a keyword/a list of keyword strings
#'
#' @param tbl A tibble. The data table to be filtered
#' @param kwds A string / list of strings. The query keywords
#' @param column A string. The data column to be filtered upon.
#' @param ignore_case A Boolean, default TRUE. Specify whether the query is case-insensitive
#' @param match_all A Boolean, default FALSE. Specify whether the query
#' requires the column value to match all the kwds or just one of them.
#' @return A tibble with filtered entries
#' @importFrom dplyr filter
#' @export
query_kwds <- function(tbl, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds <- paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query <- paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )

  tbl |> dplyr::filter(dplyr::sql(query))
}


#' Function that filters the studies data based on multiple queries,
#' including keyword matching and date range filtering
#' @param studies A tibble. The data table to be filtered
#' @param kws A string. The keyword string entered by the users
#' @param dates A list of two date values (start and end date)
#'
#' @return A tibble with filtered entries
#' @importFrom dplyr filter collect
#' @export
data_query_search = function(studies, kws, dates) {
  si = trimws(unlist(strsplit(kws, ",")))
  start <- as.Date(dates[1])
  end <- as.Date(dates[2])

  query_kwds(studies, si, "brief_title", match_all = TRUE) |>
    filter(start_date >= start & completion_date <= end) |>
    collect()
}


#' Function that filters the studies data based on brief title keywords queries
#' @param studies A tibble. The data table to be filtered
#' @param kw A string. The keyword string entered by the users
#'
#' @return A tibble with filtered entries
#' @importFrom dplyr filter collect
#' @export
title_kw_search = function(studies, kw) {
  query_kwds(studies, kw, "brief_title", match_all = TRUE) |>
    collect()
}
