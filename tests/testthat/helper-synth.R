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
