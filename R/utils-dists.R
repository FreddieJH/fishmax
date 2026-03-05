#' @noRd
dgev_v <- Vectorize(evd::dgev, vectorize.args = c("loc", "scale", "shape"))
#' @noRd
pgev_v <- Vectorize(evd::pgev, vectorize.args = c("loc", "scale", "shape"))
#' @noRd
qgev_v <- Vectorize(evd::qgev, vectorize.args = c("loc", "scale", "shape"))

#' @noRd
dgumbel_v <- Vectorize(evd::dgumbel, vectorize.args = c("loc", "scale"))
#' @noRd
pgumbel_v <- Vectorize(evd::pgumbel, vectorize.args = c("loc", "scale"))
#' @noRd
qgumbel_v <- Vectorize(evd::qgumbel, vectorize.args = c("loc", "scale"))
