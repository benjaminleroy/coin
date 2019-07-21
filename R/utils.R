#' check if probability vector is as expected
#'
#' @param prob probability vector
#'
#' @return TRUE or errors
#' @export
check_prob <- function(prob) {
  assertthat::assert_that(is.numeric(prob),
                          msg = "'prob' must be a numeric vector")

  assertthat::assert_that(length(prob) == 2 & is.numeric(prob),
                          msg = "'prob' must be a numeric vector of length 2")

  assertthat::assert_that(all(prob >= 0) & all(any(prob <= 1)),
                          msg = "'prob' values must be between 0 and 1")

  assertthat::assert_that(sum(prob) == 1,
                         msg = "elements in 'prob' must add up to 1")
  TRUE
}
