test_that("the version shown in the app and reports matches DESCRIPTION", {
  expect_equal(ipmR:::IPMR_VERSION, as.character(utils::packageVersion("ipmR")))
})
