test_that("chi2_rows equals chisq.test (no continuity correction) per interviewer vs rest", {
  set.seed(1)
  for (r in 1:20) {
    A <- matrix(rpois(4 * 5, 6), 4, 5)
    p <- ipmR:::chi2_rows(A)
    for (i in 1:4) {
      tb <- rbind(A[i, ], colSums(A) - A[i, ])
      tb <- tb[, colSums(tb) > 0, drop = FALSE]
      expect_equal(p[i], suppressWarnings(chisq.test(tb, correct = FALSE)$p.value), tolerance = 1e-10)
    }
  }
})

test_that("ttest_rows equals pooled two-sample t.test", {
  set.seed(2)
  x <- rnorm(60, 10, 3); g <- rep(1:4, each = 15)
  st <- data.frame(n = tapply(x, g, length), s = tapply(x, g, sum), ss = tapply(x, g, function(z) sum(z^2)))
  p <- ipmR:::ttest_rows(st$n, st$s, st$ss, sum(st$n), sum(st$s), sum(st$ss))
  for (i in 1:4) expect_equal(p[i], t.test(x[g == i], x[g != i], var.equal = TRUE)$p.value, tolerance = 1e-10)
  expect_true(is.na(ipmR:::ttest_rows(5, 50, 500, 10, 100, 1000)))   # constant data: p undefined -> no flag
})

test_that("variable classification and recodes follow flaganal.do", {
  o <- ipm_defaults()
  expect_null(ipmR:::prep_variable(rep(NA_real_, 5), "", o))           # empty
  expect_null(ipmR:::prep_variable(rep(3, 5), "", o))                  # uniform
  expect_equal(ipmR:::prep_variable(c(1, 2, NA, 2), "", o)$cat, "CA")
  co <- ipmR:::prep_variable(c(1:40, 96, 97, 98, 99, NA), "", o)
  expect_equal(co$cat, "CO")
  expect_equal(tail(co$val, 5), c(-5, -4, -3, -2, -1))
  expect_equal(ipmR:::prep_variable(c("A", "", "B"), "A1", o)$cat, "SS")
  expect_null(ipmR:::prep_variable(c("ABC", "DE"), "A10", o))          # long strings not tested
  expect_equal(ipmR:::prep_variable(c(1.7, 2.2, 2.9), "", o)$val, c(1, 2, 2))  # int() truncation
})

test_that("unit-value pairing reproduces the DK/NR heuristic", {
  x <- data.table::data.table(A = 1:6, U = c(1, 2, 3, 8, 9, 1), V = c(5, 12, 3, 98, 99, 7), Z = 0)
  r <- ipmR:::make_unit_value_pairs(data.table::copy(x), "U", c(U = "unit"))
  expect_equal(names(r$x), c("A", "U_V_pair", "Z"))
  # levels of U: 1 2 3 8 9 -> 8 (DK) becomes 4, 9 (NR) becomes 5; multiplier 100 (V max = 99 -> 2 digits)
  expect_equal(r$x$U_V_pair, c(105, 212, 303, 498, 599, 107))
})

test_that("varlist expansion supports wildcards and ranges", {
  v <- c("HL1", "HL2", "HL3", "HL8", "ED1", "ED2")
  expect_equal(expand_varlist("HL1-HL3 ED*", v), c("HL1", "HL2", "HL3", "ED1", "ED2"))
  expect_equal(suggest_mics_modules(c("HH1", "HL4", "ED5A", "MWB3", "LN", "HAZ2"), exclude = "HH"), c("HL*", "ED*", "MWB*", "HAZ*"))
})

test_that("labels and formats stored as vectors are read as one string", {
  v <- structure(1:3, label = c("Interviewer", "number"), format.spss = c("F2.0", "F8.2"))
  expect_identical(var_attr1(v, "label"), "Interviewer number")
  expect_identical(var_attr1(v, "format.spss", alt = "format.stata"), "F2.0")
  expect_identical(var_attr1(1:3, "label"), "")
  expect_identical(var_attr1(structure(1, label = NA_character_), "label"), "")
  x <- data.frame(a = 1:2, b = 3:4); attr(x$a, "label") <- c("A", "B")
  expect_identical(vapply(x, var_attr1, character(1), which = "label"), c(a = "A B", b = ""))
})
