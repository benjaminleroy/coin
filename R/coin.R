#' create a coin object
#'
#' @param object vector of length (both sides of the coin)
#'
#' @return a coin object
#' @export
#'
#' @examples
#' coin()
coin <- function(object = c("heads", "tails"), prob = c(0.5, 0.5)) {
  assertthat::assert_that(length(object) == 2,
                          msg = "'object' must be of length 2")
  check_prob(prob)
  attr(object, "prob") <- prob
  class(object) <- "coin"
  object
}


#' Rare coin class
#'
#' @param name str, name of coin
#' @param year int, year coin was made
#' @param ... parameters to feed into \code{coin}
#' superclass
#'
#' @return rare_coin object
#' @export
#'
#' @examples
#' rare_coin(name = "Lincoln penny", year =  1972)
rare_coin <- function(name, year, ...){
  object <- coin(...)
  attr(object, "name") <- name
  attr(object, "year") <- as.numeric(year)
  class(object) <- c("rare_coin", "coin")
  object
}

