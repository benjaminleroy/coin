test_that("coin creation works", {
  expect_error(coin(c("tick", "tac", "toe")))
  expect_true(inherits(coin(), "coin"))
})
