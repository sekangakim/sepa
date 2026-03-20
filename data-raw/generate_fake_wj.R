## data-raw/generate_fake_wj.R
##
## Run this script ONCE from the package root to regenerate data/fake_wj.rda:
##
##   source("data-raw/generate_fake_wj.R")
##   # or equivalently:
##   devtools::load_all(); fake_wj <- simulate_sepa_fake_wj(); usethis::use_data(fake_wj, overwrite = TRUE)
##
## The script requires usethis (install.packages("usethis")).
## -----------------------------------------------------------------------

## Load the generator (works whether the package is installed or loaded)
if (!requireNamespace("usethis", quietly = TRUE))
  stop("Please install 'usethis': install.packages('usethis')")

devtools::load_all()   # loads simulate_sepa_fake_wj() from R/simulate.R

fake_wj <- simulate_sepa_fake_wj(n = 5127, seed = 20251127)

cat(sprintf("Generated fake_wj: %d rows x %d cols\n", nrow(fake_wj), ncol(fake_wj)))
print(sapply(fake_wj[, c("LT","ST","CP","AP","VP","CK","FR")], mean))
print(sapply(fake_wj[, c("LT","ST","CP","AP","VP","CK","FR")], sd))

usethis::use_data(fake_wj, overwrite = TRUE)
cat("Saved data/fake_wj.rda\n")
