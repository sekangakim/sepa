# SEPA — CRAN Submission Checklist & Workflow
# ================================================
# Run these commands in order from the package root.

# ── STEP 0 · Prerequisites ────────────────────────────────────────────────────
install.packages(c("devtools","usethis","roxygen2","testthat",
                   "knitr","rmarkdown","rcmdcheck"))

# ── STEP 1 · Generate data/fake_wj.rda ───────────────────────────────────────
# Run once only; commits the .rda to version control.
source("data-raw/generate_fake_wj.R")

# ── STEP 2 · Rebuild roxygen2 documentation ──────────────────────────────────
# Overwrites NAMESPACE and man/*.Rd from @-tags in R/*.R
devtools::document()

# ── STEP 3 · Run tests ───────────────────────────────────────────────────────
devtools::test()

# ── STEP 4 · Build vignette and check locally ────────────────────────────────
devtools::check(vignettes = TRUE)

# ── STEP 5 · Check as CRAN would ─────────────────────────────────────────────
rcmdcheck::rcmdcheck(args = "--as-cran", error_on = "warning")

# ── STEP 6 · win-builder check ───────────────────────────────────────────────
devtools::check_win_devel()    # R-devel on Windows
devtools::check_win_release()  # R-release on Windows

# ── STEP 7 · rhub checks (multiple platforms) ────────────────────────────────
# install.packages("rhub")
# rhub::check_for_cran()

# ── STEP 8 · Spell-check ─────────────────────────────────────────────────────
# install.packages("spelling")
# spelling::spell_check_package()

# ── STEP 9 · Finalise cran-comments.md ───────────────────────────────────────
# Fill in actual NOTE text (if any), test environment details, and
# downstream dependency count before submitting.

# ── STEP 10 · Submit ─────────────────────────────────────────────────────────
devtools::release()   # interactive wizard that calls R CMD check once more

# ── Common issues to resolve before submission ───────────────────────────────
#
# ERROR: "no visible binding for global variable 'X'"
#   → Add  utils::globalVariables(c("X"))  in R/utils.R, or prefix with pkg:::
#
# WARNING: "installed size is X Mb"
#   → Compress fake_wj: tools::resaveRdaFiles("data/")
#
# NOTE: "checking examples ... [XXs]"
#   → Wrap slow examples in \donttest{} (already done in run_sepa)
#
# NOTE: "no ORCID"
#   → Add your real ORCID in DESCRIPTION Authors@R comment = c(ORCID = "...")
#
# NOTE: "Suggests or Enhances not in mainstream repositories"
#   → All suggested packages (writexl, knitr, …) are on CRAN — no action needed
