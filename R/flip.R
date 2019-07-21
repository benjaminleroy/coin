#' generic function for flipping an object
#'
#' @param x object to be flipped
#' @param ... additional parameters
#'
#' @return vector of observed elements
#' @export
flip <- function(x, ...) {
  UseMethod("flip")
}

#' flip a coin
#'
#' @param x coin object
#' @param times number of times to flip the coin
#'
#' @return a vector of flips (length: times)
#' @export
#'
#' @examples
#' coin1 <- coin()
#' flip(coin1, 5)
flip.coin <- function(x, times = 1) {
  sample(x, prob = attr(x, "prob"),
         size = times, replace = TRUE)
}
