#' create a coin object
#'
#' @param object vector of length (both sides of the coin)
#'
#' @return a coin object
#' @export
#'
#' @examples
coin <- function(object = c("heads", "tails")) {
  assertthat::assert_that(length(object) == 2,
                          msg = "'object' must be of length 2")
  class(object) <- "coin"
  object
}

