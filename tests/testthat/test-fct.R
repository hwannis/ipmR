# Field Check Tables (#83): MICS7 FCT syntaxes ported (by team / interviewer, targets, Excel)
fct_setup <- local({ x <- NULL; function() { if (is.null(x)) { dir <- add_synth_fct(make_synth()); cfg <- mics7_autoconfig(dir, "Synthetic")
  x <<- list(dir = dir, cfg = cfg, fd = ipm_fct_data(cfg)) }; x } })

test_that("the FCT data are found by their variables, and the tables are built or listed as not in the data", {
  s <- fct_setup()
  expect_true(all(c("hh", "hl", "wm", "bh") %in% names(s$fd$roles)))
  fc <- ipm_fct(s$fd)
  expect_setequal(names(fc$tables), ipm_fct_catalog()$id)
  av <- vapply(fc$tables, function(t) isTRUE(t$available), TRUE)
  expect_true(all(av[c("F1", "F2W", "F2UC", "F3W", "F4W", "F5W", "F5UC", "F6W", "F6W2", "F8E", "F10", "F11A", "F11B", "F11D", "F11E")]))
  expect_false(av[["F14"]]); expect_match(fc$tables$F14$why, "FE1")
  expect_equal(fc$tables$F1$data$.row[1], "Total"); expect_equal(nrow(fc$tables$F1$data), 4)      # total + 3 teams
})

test_that("F1 response rate and F2W eligible women match a direct count, and targets come from the parameters", {
  s <- fct_setup(); hh <- haven::read_sav(file.path(s$dir, "hh.sav")); r <- as.numeric(hh$HH46)
  fc <- ipm_fct(s$fd, list(t2w_u = 0.1, t2w_r = 0.1))
  t1 <- fc$tables$F1$data[.row == "Total"]
  expect_equal(t1$rr, 100 * sum(r == 1) / sum(r %in% c(1, 2, 4, 7)))
  expect_equal(t1$n, nrow(hh)); expect_equal(t1$c1, 100 * mean(r == 1))
  u <- as.numeric(hh$HH6) == 1 & r == 1
  t2 <- fc$tables$F2W$data[.row == "Total"]
  expect_equal(t2$u_hh, sum(u)); expect_equal(t2$u1_m, sum(hh$HH49[u]) / sum(u))
  expect_false(any(fc$tables$F2W$bad$u1_m))                                              # expected 0.1 woman: met everywhere
  fc2 <- ipm_fct(s$fd, list(t2w_u = 5, t2w_r = 5))
  expect_true(all(fc2$tables$F2W$bad$u1_m)); expect_equal(fc2$tables$F2W$data$u1_t, fc2$tables$F2W$data$u1_m)   # 5 expected: not met, value shown
})

test_that("F4W % interviewed by age group, F6W date of birth and F10 children ever born, with the targets not met", {
  s <- fct_setup(); fc <- ipm_fct(s$fd)
  wm <- haven::read_sav(file.path(s$dir, "wm.sav")); hl <- haven::read_sav(file.path(s$dir, "hl.sav"))
  w <- hl[as.numeric(hl$HL4) == 2 & hl$HL6 >= 15 & hl$HL6 <= 19, ]
  ok <- paste(w$HH1, w$HH2, w$HL1) %in% paste(wm$HH1, wm$HH2, wm$LN)[as.numeric(wm$WM17) == 1]
  f4 <- fc$tables$F4W$data[.row == "Total"]
  expect_equal(f4$n15, nrow(w)); expect_equal(f4$r15, 100 * mean(ok))
  expect_equal(f4$min, min(unlist(f4[, paste0("r", seq(15, 45, 5)), with = FALSE])))
  f6 <- fc$tables$F6W$data
  expect_equal(rowSums(f6[, .(myb, yab, yb, ab, nd)]), rep(100, nrow(f6)))
  t22 <- s$fd$team_info[grepl("^T2", team_label)]$team_label                                  # interviewer 22 (team 2) gives no month of birth
  expect_true(fc$tables$F6W$bad$myb[fc$tables$F6W$data$.row == t22])
  f10 <- fc$tables$F10$data; d1 <- as.numeric(wm$WM17) == 1
  expect_equal(f10[.row == "Total"]$mceb, sum(wm$CEB[d1]) / sum(d1))
  fc_low <- ipm_fct(s$fd, list(ceb_prev = 0.5)); expect_false(any(fc_low$tables$F10$bad$mceb))
})

test_that("F5UC age ratios, F11D 12-month heaping and F8E handwashing follow the MICS syntaxes", {
  s <- fct_setup(); fc <- ipm_fct(s$fd)
  hl <- haven::read_sav(file.path(s$dir, "hl.sav")); a <- as.numeric(hl$HL6)
  f5 <- fc$tables$F5UC$data[.row == "Total"]
  expect_equal(f5$a4, sum(a == 4)); expect_equal(f5$r1, sum(a == 4) / sum(a == 5)); expect_equal(f5$r2, sum(a == 7) / sum(a == 6))
  bh <- haven::read_sav(file.path(s$dir, "bh.sav")); dd <- as.numeric(bh$BH5) == 2 & bh$BH4Y >= 2026 - 15     # deaths in the last 15 years
  m <- ifelse(as.numeric(bh$BH9U) == 3, as.numeric(bh$BH9N) * 12, as.numeric(bh$BH9N))[dd]
  y1 <- sum(as.numeric(bh$BH9U)[dd] == 3 & as.numeric(bh$BH9N)[dd] == 1)
  f11 <- fc$tables$F11D$data[.row == "Total"]
  expect_equal(f11$y1, y1); expect_equal(f11$tot, sum(m >= 8 & m <= 16))
  expect_equal(f11$r12, (f11$m12 + f11$y1) * 9 / f11$tot)
  f8 <- fc$tables$F8E$data; t31 <- s$fd$team_info[grepl("^T3", team_label)]$team_label     # interviewer 31 never sees the place for handwashing
  expect_true(fc$tables$F8E$bad$h1[f8$.row == t31]); expect_gt(f8[.row == t31]$h2, 20)
})

test_that("rows by interviewer, team and date filters, summary, overview, graphs and Excel", {
  s <- fct_setup()
  fi <- ipm_fct(s$fd, rows = "interv", tables = c("F1", "F6W"))
  expect_equal(nrow(fi$tables$F1$data), 13); expect_equal(names(fi$tables), c("F1", "F6W"))
  ft <- ipm_fct(s$fd, teams = 2, from = as.Date("2026-01-12"), tables = "F1")
  expect_equal(nrow(ft$tables$F1$data), 2)
  hh <- haven::read_sav(file.path(s$dir, "hh.sav")); d <- as.Date(sprintf("%d-%d-%d", hh$HH5Y, hh$HH5M, hh$HH5D))
  expect_equal(ft$tables$F1$data[.row == "Total"]$n, sum(d >= as.Date("2026-01-12") & as.numeric(hh$HH3) %/% 10 == 2))
  fc <- ipm_fct(s$fd)
  expect_true(all(c("id", "row", "not_met") %in% names(fc$summary)))
  expect_true(any(grepl("age ratio", fc$overview$indicator)))
  for (t in fc$tables) for (i in seq_along(t$plots)) expect_s3_class(ipm_fct_plot(t, i), "ggplot")
  f <- tempfile(fileext = ".xlsx"); ipm_fct_xlsx(fc, f)
  sh <- openxlsx::getSheetNames(f)
  expect_true(all(c("Contents", "Targets", "Overview", "F1", "F2W", "Parameters") %in% sh)); expect_false("F14" %in% sh)
  x <- openxlsx::read.xlsx(f, "F1", startRow = 5); expect_equal(x[1, 1], "Total"); expect_equal(names(x)[1], "Team")
})
