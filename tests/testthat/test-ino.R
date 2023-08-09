test_that("Warnings work", {
  expect_warning(
    ino_warn("warning message"),
    "warning message"
  )
})
