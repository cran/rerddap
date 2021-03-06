context("convert_time")

skip_on_cran()

test_that("convert_time works", {
  a <- convert_time(n = 473472000)
  b <- convert_time(isoTime = "1985-01-02T00:00:00Z")
  expect_is(a, "character")
  expect_is(b, "character")

  vcr::use_cassette("convert_time", {
    a_web <- convert_time(n = 473472000, method = "web")
    b_web <- convert_time(isoTime = "1985-01-02T00:00:00Z", method = "web")
  })

  expect_is(a_web, "character")
  expect_is(b_web, "character")
  expect_equal(b_web, "473472000")
})

test_that("convert_time fails well", {
  expect_error(convert_time(), "One of n or isoTime must be non-NULL")
  expect_error(convert_time(4, 5), "is not TRUE")
  expect_error(convert_time(473472000, "B"), "Supply only one of n or isoTime")
})
