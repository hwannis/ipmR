# Build the stand-alone shinyapps.io / Posit Connect bundle (folder ipm_shinyapp/) from the package sources.
# Run from the repository root:   Rscript deploy/shinyapps/make_bundle.R [output_dir]
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "ipm_shinyapp"
unlink(out, recursive = TRUE); dir.create(file.path(out, "R"), recursive = TRUE)
stopifnot(file.copy("inst/shinyapps/app.R", file.path(out, "app.R")),
          file.copy("inst/shinyapps/deploy.R", file.path(out, "deploy.R")),
          file.copy("inst/app/app_core.R", file.path(out, "app_core.R")))
src <- setdiff(list.files("R", pattern = "\\.R$", full.names = TRUE), "R/app_launch.R")
stopifnot(all(file.copy(src, file.path(out, "R"))))
if (file.exists("deploy/shinyapps/README.txt")) file.copy("deploy/shinyapps/README.txt", file.path(out, "README.txt"))
cat("Bundle written to", normalizePath(out), "\n")
