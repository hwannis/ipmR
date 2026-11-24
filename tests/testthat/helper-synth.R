# Synthetic MICS-like survey: 3 teams x 4 interviewers, 4 weeks, planted interviewer bias
make_synth <- function(dir = tempfile("ipm_synth_"), lang = "en") {
  L <- if (lang == "fr") c(int = "Code enqu\u00eateur", sup = "Code chef d\u2019\u00e9quipe", day = "Jour interview", res = "R\u00e9sultat de l\u2019interview",
                            sh = "D\u00e9but interview - Heure", eh = "Fin de l\u2019interview - Heure")
       else c(int = "Interviewer number", sup = "Supervisor number", day = "Day of interview", res = "Result of HH interview",
              sh = "Start of interview - Hour", eh = "End of interview - Hour")
  dir.create(dir)
  set.seed(42)
  ints <- c(11:14, 21:24, 31:34)
  days <- seq(as.Date("2026-01-05"), by = "day", length.out = 28)
  n <- 1200
  iv <- sample(ints, n, TRUE); d <- sample(days, n, TRUE)
  hc1 <- sample(1:4, n, TRUE)
  hc2 <- ifelse(iv == 13, sample(1:2, n, TRUE, prob = c(.95, .05)), sample(1:2, n, TRUE))   # biased interviewer 13
  hc3 <- round(rnorm(n, 50 + 25 * (iv == 22), 10))                                          # biased continuous, int 22
  hc3[sample(n, 20)] <- 98
  lab <- function(x, l) haven::labelled(x, label = l)
  hh <- data.frame(
    HH1 = lab(rep(1:120, each = 10)[1:n], "Cluster number"), HH2 = lab(rep(1:10, 120)[1:n], "Household number"),
    HH3 = lab(iv, L[["int"]]), HH4 = lab(iv %/% 10 * 10, L[["sup"]]),
    HH5D = lab(as.integer(format(d, "%d")), L[["day"]]), HH5M = lab(as.integer(format(d, "%m")), "Month of interview"),
    HH5Y = lab(as.integer(format(d, "%Y")), "Year of interview"),
    HH11H = lab(sample(8:15, n, TRUE), L[["sh"]]), HH11M = lab(sample(0:59, n, TRUE), "Start of interview - Minutes"),
    HH46 = lab(sample(c(1, 1, 1, 1, 1, 1, 2, 4), n, TRUE), L[["res"]]),
    HH13H = NA, HH13M = NA,
    HC1 = haven::labelled(hc1, c(A = 1, B = 2, C = 3, D = 4), label = "Main material of floor"),
    HC2 = haven::labelled(hc2, c(YES = 1, NO = 2), label = "Has electricity"),
    HC3 = lab(hc3, "Distance in minutes"),
    HC4U = lab(sample(c(1, 2, 8), n, TRUE), "Duration unit"), HC4N = lab(sample(1:20, n, TRUE), "Duration number"),
    HC5 = lab(rep(1, n), "Uniform question"))
  hh$HH13H <- lab(pmin(hh$HH11H + sample(0:1, n, TRUE), 23), L[["eh"]])
  hh$HH13M <- lab(sample(0:59, n, TRUE), "End of interview - Minutes")
  hh <- hh[, c("HH1", "HH2", "HH3", "HH4", "HH5D", "HH5M", "HH5Y", "HH11H", "HH11M", "HH46", "HH13H", "HH13M", "HC1", "HC2", "HC3", "HC4U", "HC4N", "HC5")]
  haven::write_sav(hh, file.path(dir, "hh.sav"))
  # household members (linked to hh, no interviewer variable)
  k <- sample(1:7, n, TRUE)
  hl <- data.frame(HH1 = rep(hh$HH1, k), HH2 = rep(hh$HH2, k), HL1 = sequence(k))
  hl$HL4 <- haven::labelled(sample(1:2, nrow(hl), TRUE), c(MALE = 1, FEMALE = 2), label = "Sex")
  hl$HL6 <- lab(sample(0:80, nrow(hl), TRUE), "Age")
  hl$HL7 <- lab(sample(1:2, nrow(hl), TRUE), "Stayed last night")
  haven::write_sav(hl, file.path(dir, "hl.sav"))
  dir
}

# synthetic 5-17 questionnaire (fs.sav) with one interviewer (23) who stops children at the reading practice (#29)
add_synth_fs <- function(dir) {
  set.seed(7)
  lab <- function(x, l) haven::labelled(x, label = l)
  ints <- c(11:14, 21:24, 31:34); days <- seq(as.Date("2026-01-05"), by = "day", length.out = 28)
  n <- 900
  iv <- sample(ints, n, TRUE); d <- sample(days, n, TRUE); bad <- iv == 23
  res <- sample(c(1, 1, 1, 1, 1, 1, 1, 2, 4), n, TRUE)
  done <- res == 1
  agree <- ifelse(done, sample(c(1, 1, 1, 2), n, TRUE), NA)
  try_other <- ifelse(agree %in% 2, sample(c(11, 95), n, TRUE), NA)          # no at FL10A/B: offered the story in Somali
  fl14 <- ifelse(agree %in% 1, ifelse(bad, sample(1:2, n, TRUE, prob = c(.1, .9)), sample(1:2, n, TRUE, prob = c(.7, .3))), NA)
  story <- ifelse(fl14 %in% 1, 1, NA)
  fl23 <- ifelse(done, sample(1:3, n, TRUE, prob = c(.8, .1, .1)), NA)
  fl24 <- ifelse(fl23 %in% 1, 1, NA)
  sh <- sample(9:15, n, TRUE); sm <- sample(0:39, n, TRUE)
  dur <- ifelse(bad, sample(2:5, n, TRUE), sample(12:20, n, TRUE))
  fs <- data.frame(
    HH1 = lab(rep(1:90, each = 10)[1:n], "Cluster number"), HH2 = lab(rep(1:10, 90)[1:n], "Household number"), LN = lab(rep(3, n), "Line number"),
    FS5 = lab(iv, "Interviewer number"), FS6 = lab(iv %/% 10 * 10, "Supervisor number"),
    FS7D = lab(as.integer(format(d, "%d")), "Day of interview"), FS7M = lab(as.integer(format(d, "%m")), "Month of interview"),
    FS7Y = lab(as.integer(format(d, "%Y")), "Year of interview"),
    FS17 = lab(rep(1, n), "Result of interview for children age 5-17"),
    FS8H = lab(sh, "Start of interview - Hour"), FS8M = lab(sm, "Start of interview - Minutes"),
    FS11H = lab(sh + (sm + 10 + dur) %/% 60, "End of interview - Hour"), FS11M = lab((sm + 10 + dur) %% 60, "End of interview - Minutes"),
    FL2H = lab(sh + (sm + 10) %/% 60, "Start of interview with child - Hour"), FL2M = lab((sm + 10) %% 60, "Start of interview with child - Minutes"),
    FL10 = lab(agree, "Like to read story"), FL10C = lab(try_other, "Like to read story (other language)"), FL14 = lab(fl14, "Child read every word in the practice correctly"),
    FL19INTRO = lab(story, "FL19. INTRO"), FL23A = lab(fl23, "Child recognizes symbol: 9"), FL24A = lab(fl24, "Child identifies bigger of two numbers"),
    FL28 = lab(res, "Result of interview with child"))
  haven::write_sav(fs, file.path(dir, "fs.sav"))
  dir
}

# synthetic data for the Field Check Tables (#83): area and eligible members in hh.sav, women (wm.sav), under-5 (ch.sav)
# and birth history (bh.sav) linked to the household members, with known patterns
add_synth_fct <- function(dir) {
  set.seed(11)
  lab <- function(x, l) haven::labelled(x, label = l)
  hh <- haven::read_sav(file.path(dir, "hh.sav")); hl <- haven::read_sav(file.path(dir, "hl.sav"))
  done <- as.numeric(hh$HH46) == 1
  key <- paste(hh$HH1, hh$HH2); hk <- paste(hl$HH1, hl$HH2)
  w <- as.numeric(hl$HL4) == 2 & as.numeric(hl$HL6) >= 15 & as.numeric(hl$HL6) <= 49
  cnt <- function(f) { z <- tapply(f, factor(hk, levels = key), sum); z[is.na(z)] <- 0; as.integer(ifelse(done, z, NA)) }
  hh$HH6 <- haven::labelled(sample(1:2, nrow(hh), TRUE), c(URBAN = 1, RURAL = 2), label = "Area")
  hh$HH49 <- lab(cnt(w), "Number of women 15-49")
  hh$HH51 <- lab(cnt(as.numeric(hl$HL6) <= 4), "Number of children under 5")
  hh$HH52 <- lab(cnt(as.numeric(hl$HL6) >= 5 & as.numeric(hl$HL6) <= 17), "Number of children 5-17")
  hh$HW1 <- haven::labelled(ifelse(done, ifelse(as.numeric(hh$HH3) == 31, 4, sample(c(1, 1, 1, 1, 1, 2, 5), nrow(hh), TRUE)), NA),
                            c(`OBSERVED: FIXED` = 1, `OBSERVED: MOBILE` = 2, `NOT IN DWELLING` = 4, `NO PERMISSION` = 5), label = "Place for handwashing")
  haven::write_sav(hh, file.path(dir, "hh.sav"))
  # women: the women 15-49 of completed households; interviewer 22 records few months of birth
  m <- match(hk, key); wl <- hl[w & done[m], ]; mw <- match(paste(wl$HH1, wl$HH2), key); n <- nrow(wl)
  iv <- as.numeric(hh$HH3)[mw]; d <- as.Date(sprintf("%d-%02d-%02d", as.numeric(hh$HH5Y)[mw], as.numeric(hh$HH5M)[mw], as.numeric(hh$HH5D)[mw]))
  res <- sample(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 3), n, TRUE)
  ceb <- ifelse(res == 1, ifelse(iv == 12, 0, sample(0:5, n, TRUE)), NA)
  wm <- data.frame(HH1 = lab(as.numeric(wl$HH1), "Cluster number"), HH2 = lab(as.numeric(wl$HH2), "Household number"), LN = lab(as.numeric(wl$HL1), "Line number"),
                   WM3 = lab(as.numeric(wl$HL1), "Woman's line number"), WM5 = lab(iv, "Interviewer number"), WM4 = lab(iv %/% 10 * 10, "Supervisor number"),
                   WM6D = lab(as.integer(format(d, "%d")), "Day of interview"), WM6M = lab(as.integer(format(d, "%m")), "Month of interview"), WM6Y = lab(as.integer(format(d, "%Y")), "Year of interview"),
                   WM17 = haven::labelled(res, c(COMPLETED = 1, `NOT AT HOME` = 2, REFUSED = 3), label = "Result of woman's interview"),
                   WB3M = lab(ifelse(res == 1, ifelse(iv == 22, 98, sample(1:12, n, TRUE)), NA), "Month of birth"),
                   WB3Y = lab(ifelse(res == 1, 2026 - as.numeric(wl$HL6), NA), "Year of birth"), WB4 = lab(ifelse(res == 1, as.numeric(wl$HL6), NA), "Age"),
                   CEB = lab(ceb, "Children ever born"), CSURV = lab(ceb, "Children surviving"), CDEAD = lab(ifelse(res == 1, 0, NA), "Children dead"))
  haven::write_sav(wm, file.path(dir, "wm.sav"))
  # birth history: one record per child ever born, a few deaths at 12 months / '1 year'
  k <- ifelse(is.na(ceb), 0, ceb); b <- wm[rep(seq_len(n), k), c("HH1", "HH2", "LN")]; nb <- nrow(b)
  dead <- sample(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2), nb, TRUE)
  bh <- data.frame(HH1 = lab(as.numeric(b$HH1), "Cluster number"), HH2 = lab(as.numeric(b$HH2), "Household number"), LN = lab(as.numeric(b$LN), "Line number"),
                   BH0 = lab(sequence(k), "Birth number"), BH4M = lab(sample(c(1:12, 98), nb, TRUE), "Month of birth"), BH4Y = lab(sample(2005:2025, nb, TRUE), "Year of birth"),
                   BH5 = lab(dead, "Child alive"), BH6 = lab(ifelse(dead == 1, sample(0:20, nb, TRUE), NA), "Age"),
                   BH9U = lab(ifelse(dead == 2, sample(2:3, nb, TRUE), NA), "Age at death unit"), BH9N = lab(rep(NA_real_, nb), "Age at death number"))
  bh$BH9N <- lab(ifelse(dead == 2, ifelse(as.numeric(bh$BH9U) == 3, 1, sample(c(8:16, 12, 12), nb, TRUE)), NA), "Age at death number")
  haven::write_sav(bh, file.path(dir, "bh.sav"))
  dir
}
