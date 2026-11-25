# #86: what each uploaded file is, duplicates, files missing, only a linked file uploaded
test_that("the uploaded files are listed with what they are, and the usual MICS files not loaded", {
  cfg <- mics7_autoconfig(make_synth(), "Synthetic"); iv <- cfg$inventory
  expect_equal(vapply(iv$files, `[[`, "", "label"), c("Household questionnaire", "Household members (list)"))
  expect_equal(iv$files[[2]]$use, "linked to hh.sav")
  m <- vapply(iv$missing, `[[`, "", "file")
  expect_true(all(c("wm.sav", "bh.sav", "ch.sav") %in% m)); expect_false("hh.sav" %in% m)
  expect_length(iv$duplicates, 0)
  f <- tempfile(fileext = ".yml"); write_config(cfg, f); expect_null(read_config(f)$inventory)   # not a setting
})

test_that("a second copy of a questionnaire is ignored and said so; an older export loses to the latest", {
  d <- make_synth()
  file.copy(file.path(d, "hh.sav"), file.path(d, "hh (1).sav"))
  x <- haven::read_sav(file.path(d, "hh.sav")); haven::write_sav(x[seq_len(nrow(x) %/% 2), ], file.path(d, "hh_old.sav"))
  cfg <- mics7_autoconfig(d, "Synthetic"); iv <- cfg$inventory
  expect_equal(cfg$datasets$hh$file, "hh.sav"); expect_false(any(c("hh..1.", "hh_old") %in% names(cfg$datasets)))
  expect_setequal(vapply(iv$duplicates, `[[`, "", "file"), c("hh (1).sav", "hh_old.sav"))
  r <- vapply(iv$duplicates, `[[`, "", "reason")
  expect_match(r[vapply(iv$duplicates, `[[`, "", "file") == "hh (1).sav"], "second copy of hh.sav", fixed = TRUE)
  expect_match(r[vapply(iv$duplicates, `[[`, "", "file") == "hh_old.sav"], "hh_old.sav was ignored", fixed = TRUE)
})

test_that("only a linked file (hl.sav) gives a clear message on what it is and what to add", {
  d <- make_synth(); unlink(file.path(d, "hh.sav"))
  expect_error(mics7_autoconfig(d), "hl.sav is the household members [(]list[)].*Add hh.sav [(]household questionnaire[)]")
})
