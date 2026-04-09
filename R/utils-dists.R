#' @importFrom evd dgev
#' @noRd
dgev_v <- Vectorize(evd::dgev, vectorize.args = c("loc", "scale", "shape"))
#' @importFrom evd pgev
#' @noRd
pgev_v <- Vectorize(evd::pgev, vectorize.args = c("loc", "scale", "shape"))
#' @importFrom evd qgev
#' @noRd
qgev_v <- Vectorize(evd::qgev, vectorize.args = c("loc", "scale", "shape"))
#' @importFrom evd dgumbel
#' @noRd
dgumbel_v <- Vectorize(evd::dgumbel, vectorize.args = c("loc", "scale"))
#' @importFrom evd pgumbel
#' @noRd
pgumbel_v <- Vectorize(evd::pgumbel, vectorize.args = c("loc", "scale"))
#' @importFrom evd qgumbel
#' @noRd
qgumbel_v <- Vectorize(evd::qgumbel, vectorize.args = c("loc", "scale"))
