#' Mortality decrement tables.
#'
#' Selected decrement tables.
#'
#' @source Society of Actuaries (http://mort.soa.org/)\cr
#' @format Data frame with 1 row per plan per year, with more than 200 variables. See ppdvars for documentation on variables.
#' \describe{
#' \item{tablename}{Unique name identifying the mortality table, character}
#' \item{series}{Level of government: 1, 2, or 3, for State-local, State, or local, numeric}
#' \item{memtype}{Purpose of the table: c("employee", "annuitant", "disabled", "hybrid")}
#' \item{sex}{Male or female, c("male", "female", "unisex", "female75"), character}
#'       Where female75 is 75% female, 25% male
#' \item{age}{Attained age, integer}
#' \item{qx.m}{Rate of mortality at age x, numeric}
#' }
#' @examples
#' library(dplyr)
#' glimpse(mortality)
#' count(mortality, tablename)
"mortality"

#   tablename, character
#   series, character:  rp2000,
#   usage, character: employee, annuitant, hybrid
#   sex, character: male, female, unisex, female75
