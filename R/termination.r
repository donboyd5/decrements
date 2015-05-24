#' Termination decrement tables.
#'
#' Selected decrement tables.
#'
#' @source Winklevoss Table 2-3, plus other sources (TO COME)\cr
#' @format Data frame with 1 row per age per entry age per tablename.
#' \describe{
#' \item{tablename}{Unique name identifying the termination table - ordinarily will select on this, character}
#' \item{age}{Attained age, integer}
#' \item{ea}{Entry age, integer}
#' \item{qxt}{Rate of termination at age x and given entry age, numeric}
#' }
#' @examples
#' library(dplyr)
#' glimpse(termination)
#' count(termination, tablename)
"termination"
