#' Mortality decrement tables.
#'
#' Selected decrement tables.
#'
#' @source Society of Actuaries (http://mort.soa.org/)\cr
#' @format Data frame with 1 row per age per tablename.
#' \describe{
#' \item{tablename}{Unique name identifying the mortality table - ordinarily will select on this, character}
#' \item{tid}{SOA unique table number if this table is on the SOA site (rather than constructed), integer}
#' \item{memtype}{Purpose of the table: c("employee", "annuitant", "disabled", "hybrid")}
#' \item{collar}{c("allcollars", "blue", "white")}
#' \item{sex}{Male or female, c("male", "female", "unisex", "female75"), character}
#'       unisex is 50% male, 50% female
#'       female75 is 75% female, 25% male
#' \item{age}{Attained age, integer}
#' \item{qxm}{Rate of mortality at age x, numeric}
#' }
#' @examples
#' library(dplyr)
#' glimpse(mortality)
#' count(mortality, tablename)
"mortality"
