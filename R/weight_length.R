#' Convert weight to length
#' Converts weight to length  using the standard allometric relationship weight = a*length^b
#' @param weight_kg Body weight in kilograms
#' @param a Allometric coefficient (default: 1)
#' @param b Allometric exponent (default: 3)
#' @returns Body length in centimetres
#' @examples
#' kg_to_cm(2.5)
#' kg_to_cm(c(1.2, 3.4, 0.8), a = 0.01, b = 3)
kg_to_cm <- function(weight_kg, a = 0.01, b = 3) {
  ((weight_kg * 1000) / a)^(1 / b)
}
