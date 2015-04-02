## proposal: have consistent cell specification interface with readxl
## therefore, isolating these functions and writing w/ zero pkg dependencies
## see https://github.com/hadley/readxl/issues/8


#' Convert column IDs from letter representation to integer
#'
#' @param x character vector of letter-style column IDs (case insensitive)
#' 
#' @return vector of positive integers identifying columns numerically
#'
#' @examples
#' \dontrun{
#' letter_to_num('Z')
#' letter_to_num(c('AA', 'ZZ', 'ABD', 'ZZZ'))
#' }
#' 
#' @keywords internal
letter_to_num <- function(x) {
  
  x <- strsplit(toupper(x), '')
  x <- lapply(x, match, table = LETTERS)
  x <- lapply(x, function(z) sum((26 ^ rev(seq_along(z) - 1)) * z))
  unlist(as.integer(x))
  
}

#' Convert column IDs from numeric to letter representation
#'
#' @param x vector of numeric column IDs
#' 
#' @return character vector of letter-style column IDs
#'
#' @examples
#' \dontrun{
#' num_to_letter(28)
#' num_to_letter(900)
#' num_to_letter(18278)
#' num_to_letter(c(25, 52, 900, 18278))
#' }
#' 
#' @keywords internal
num_to_letter <- function(y) {
  # FYI Google spreadsheets have 300 columns max
  # Excel 2010 spreadsheets have up to 16,384 columns
  #  ZZ <-->    702
  # ZZZ <--> 18,278
  
  # fcn to express column number in this weird form of base 26
  jfun <- function(div) {
    ret <- integer()
    while(div > 0) {
      remainder <- ((div - 1) %% 26) + 1
      ret <- c(remainder, ret)
      div <- (div - remainder) %/% 26
    }
    ret
  }
  
  y <- lapply(y, jfun)
  y <- lapply(y, function(x) LETTERS[x])
  y <- lapply(y, paste, collapse = '')
  unlist(y)
  
}

#' Convert A1 positioning notation to R1C1 notation
#'
#' @param x vector of cell positions in A1 notation
#' 
#' @return vector of cell positions in R1C1 notation
#' 
#' @examples
#' \dontrun{
#' A1_to_RC("A1")
#' A1_to_RC("AZ10")
#' A1_to_RC(c("A1", "AZ10"))
#' }
#'
#' @keywords internal
A1_to_RC <- function(x) {
  
  m <- regexec("[[:digit:]]*$", x)
  row_part <- as.integer(unlist(regmatches(x, m)))
  
  m <- regexec("^[[:alpha:]]*", x)
  col_part <- letter_to_num(unlist(regmatches(x, m)))
  
  paste0("R", row_part, "C", col_part)
}

#' Convert R1C1 positioning notation to A1 notation
#'
#' @param x vector of cell positions in R1C1 notation
#' 
#' @return vector of cell positions in A1 notation
#' 
#' @examples
#' \dontrun{
#' A1_to_RC("A1")
#' A1_to_RC("AZ10")
#' A1_to_RC(c("A1", "AZ10"))
#' }
#' 
#' @keywords internal
RC_to_A1 <- function(x) {
  
  col_part <- sub("^R[0-9]+C([0-9]+)$", "\\1", x)
  col_part <- num_to_letter(as.integer(col_part))
  
  row_part <- sub("^R([0-9]+)C[0-9]+$", "\\1", x)
  
  paste0(col_part, row_part)
}

#' Convert a cell range into a limits list
#' 
#' @param x character vector of length one, representing a cell range, such as 
#'   "A1:D7" or "C3"
#' @param rc logical, indicating "R1C1" positioning notation
#'   
#' @return list with 4 positive integer elements named \code{min-row},
#'   \code{max-row}, \code{min-col}, \code{max-col}
#'   
#' @examples
#' \dontrun{
#' convert_range_to_limit_list("A1")
#' convert_range_to_limit_list("Q24")
#' convert_range_to_limit_list("A1:D8")
#' convert_range_to_limit_list("R5C11", rc = TRUE)
#' convert_range_to_limit_list("R2C3:R6C9", rc = TRUE)
#' }
#' 
#' @keywords internal
convert_range_to_limit_list <- function(x, rc = FALSE) {
  
  stopifnot(is.character(x), length(x) == 1L)
  
  x_orig <- x
  x <- unlist(strsplit(x, ":"))
  
  stopifnot(length(x) <= 2)
  
  x <- rep_len(x, 2)
  
  if (rc) {
    regex <- "^R([0-9]+)C([0-9]+$)"
  } else {
    regex <- "^[A-Za-z]{1,3}[0-9]+$"
  }
  
  if (!all(grepl(regex, x))) {
    stop(sprintf("Requested cell range is invalid:\n %s\n", x_orig))
  }
  
  if (!rc) {
    x <- A1_to_RC(x)
  }

  m <- regexec("^R([0-9]+)C([0-9]+$)", x)
  m <- regmatches(x, m)
  m <- unlist(lapply(m, `[`, 2:3))
  m <- as.list(as.integer(m))
  names(m) <- c("min-row", "min-col", "max-row", "max-col")

  m
  
}

#' Convert a cell range, special case wrapper for R1C1 positioning notation
#' 
#' @param range character vector, length one, such as "R1C1:R7C4" or "R3C3"
#' 
#' @return list with 4 positive integer elements named \code{min-row},
#'   \code{max-row}, \code{min-col}, \code{max-col}
#'   
#' @examples
#' \dontrun{
#' rc("R1C1:R7C4")
#' rc("R3C3")
#' }
#'
#' @keywords internal
rc <- function(range) convert_range_to_limit_list(range, rc = TRUE) 

#' Convert a limits list to a cell range
#' 
#' @param limits limits list
#' @param rc logical, indicating "R1C1" positioning notation
#'   
#' @return length one character vector holding a cell range, in either A1 or
#'   R1C1 positioning notation
#'   
#' @examples
#' \dontrun{
#' rgLL <- list(`min-row` = 1, `max-row` = 4, `min-col` = 1, `max-col` = 3)
#' convert_limit_list_to_range(rgLL)
#' convert_limit_list_to_range(rgLL, rc = TRUE)
#' }
#' 
#' @keywords internal
convert_limit_list_to_range <- function(limits, rc = FALSE) {
  
  range <- c(paste0("R", limits$`min-row`, "C", limits$`min-col`),
             paste0("R", limits$`max-row`, "C", limits$`max-col`))
  
  if(!rc) {
    range <- RC_to_A1(range) 
  }
  
  paste(range, collapse = ":")
}
