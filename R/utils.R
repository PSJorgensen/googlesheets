#' Retrieve a worksheet-describing list from a googlesheet
#' 
#' From a googlesheet, retrieve a list (actually a row of a data.frame) giving
#' everything we know about a specific worksheet.
#'
#' @inheritParams get_via_lf
#' @param verbose logical, indicating whether to give a message re: title of the
#'   worksheet being accessed
#'
#' @keywords internal
get_ws <- function(ss, ws, verbose = TRUE) {

  stopifnot(inherits(ss, "googlesheet"),
            length(ws) == 1L,
            is.character(ws) || (is.numeric(ws) && ws > 0))

  if(is.character(ws)) {
    index <- match(ws, ss$ws$ws_title)
    if(is.na(index)) {
      stop(sprintf("Worksheet %s not found.", ws))
    } else {
      ws <- index %>% as.integer()
    }
  }
  if(ws > ss$n_ws) {
    stop(sprintf("Spreadsheet only contains %d worksheets.", ss$n_ws))
  }
  if(verbose) {
    message(sprintf("Accessing worksheet titled \"%s\"", ss$ws$ws_title[ws]))
  }
  ss$ws[ws, ]
}

#' List the worksheets in a googlesheet
#' 
#' Retrieve the titles of all the worksheets in a gpreadsheet.
#'
#' @inheritParams get_via_lf
#'
#' @examples
#' \dontrun{
#' gap_key <- "1HT5B8SgkKqHdqHJmn5xiuaC04Ngb7dG9Tv94004vezA"
#' gap_ss <- register_ss(gap_key)
#' list_ws(gap_ss)
#' }
#' @export
list_ws <- function(ss) {

  stopifnot(inherits(ss, "googlesheet"))
  
  ss$ws$ws_title
}

## functions for annoying book-keeping tasks with lists
## probably more naturally done via rlist or purrr
## see #12 for plan re: getting outside help for FP w/ lists

#' Filter a list by name
#'
#' @param x a list
#' @param name a regular expression
#' @param ... other parameters you might want to pass to grep
#'
#' @keywords internal
lfilt <- function(x, name, ...) {
  x[grep(name, names(x), ...)]
}

#' Pluck out elements from list components by name
#'
#' @param x a list
#' @param xpath a string giving the name of the component you want, XPath style
#'
#' @keywords internal
llpluck <- function(x, xpath) {
  x %>% plyr::llply("[[", xpath) %>% plyr::llply(unname)
}
lapluck <- function(x, xpath, .drop = TRUE) {
  x %>% plyr::laply("[[", xpath, .drop = .drop) %>% unname()
}

# OMG this is just here to use during development, i.e. after
# devtools::load_all(), when inspecting big hairy lists
#' @keywords internal
str1 <- function(...) str(..., max.level = 1)

#' Extract sheet key from its browser URL
#'
#' @param url URL seen in the browser when visiting the sheet
#'
#' @examples
#' \dontrun{
#' gap_url <- "https://docs.google.com/spreadsheets/d/1HT5B8SgkKqHdqHJmn5xiuaC04Ngb7dG9Tv94004vezA/"
#' gap_key <- extract_key_from_url(gap_url)
#' gap_ss <- register_ss(gap_key)
#' gap_ss
#' }
#'
#' @export
extract_key_from_url <- function(url) {
  url_start_list <-
    c(ws_feed_start = "https://spreadsheets.google.com/feeds/worksheets/",
      url_start_new = "https://docs.google.com/spreadsheets/d/",
      url_start_old = "https://docs.google.com/spreadsheet/ccc\\?key=",
      url_start_old2 = "https://docs.google.com/spreadsheet/pub\\?key=")
  url_start <- url_start_list %>% stringr::str_c(collapse = "|")
  url %>% stringr::str_replace(url_start, '') %>%
    stringr::str_split_fixed('[/&#]', n = 2) %>%
    `[`(1)
}

#' Construct a worksheets feed from a key
#'
#' @param key character, unique key for a spreadsheet
#' @param visibility character, either "private" (default) or "public",
#'   indicating whether further requests will be made with or without
#'   authentication, respectively
#'
#' @keywords internal
construct_ws_feed_from_key <- function(key, visibility = "private") {
  tmp <-
    "https://spreadsheets.google.com/feeds/worksheets/KEY/VISIBILITY/full"
  tmp %>%
    stringr::str_replace('KEY', key) %>%
    stringr::str_replace('VISIBILITY', visibility)
}
