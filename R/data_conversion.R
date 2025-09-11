# R/data_conversion.R

#' Convert weight to length for fish
#'
#' Converts fish weight to length using allometric relationships
#'
#' @param weight_kg Fish weight in kilograms
#' @param a Allometric coefficient (default: 0.04478)
#' @param b Allometric exponent (default: 2.673)
#'
#' @return Length in centimetres
#' @export
#'
#' @examples
#' kg_to_cm(2.5)
#' kg_to_cm(c(1.2, 3.4, 0.8), a = 0.01, b = 3)
kg_to_cm <- function(weight_kg, a = 0.04478, b = 2.673) {
  ((weight_kg * 1000) / a)^(1 / b)
}
