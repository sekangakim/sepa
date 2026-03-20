# CRAN submission comments — SEPA 0.1.0

## Resubmission (Round 4)

Addressed recurring spelling NOTE from CRAN automated checker:
* Rewrote DESCRIPTION Title from "Subprofile Extraction via Pattern
  Analysis" to "Segment Profile Extraction via Pattern Analysis" —
  more precise and uses standard dictionary words only.
* Replaced "ipsatized" with "row-mean-centered" in Description.
* Replaced bare "BCa" with "bias-corrected and accelerated (BCa)"
  in Description.

## Summary of all changes across resubmissions

* Round 2: Vignette build time reduced by precomputing all bootstrap
  and permutation results (stored in inst/extdata/ as .rds files).
  The eval=FALSE chunks display the full workflow calls for
  reproducibility. Vignette now builds in under 2 minutes.
* Round 2: Removed non-standard top-level files.
* Round 4: Rewrote DESCRIPTION to eliminate aspell-flagged terms.

## Test environments
* macOS aarch64 (local): R 4.5.2
* Windows (win-builder): R-devel

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies

None — this is a new package.

## Notes for CRAN reviewers

* `fake_wj` is a fully synthetic dataset generated from `simulate_sepa_fake_wj()`.
  The original WJ-IV norming data are proprietary; no proprietary data are
  included in the package.
* The bootstrap functions can be slow with the default `B = 2000` replicates.
  Examples that call `run_sepa()` are wrapped in `\donttest{}` to avoid
  CRAN check timeouts.
* `save_pdf()` is an internal helper; it opens a graphics device and closes
  it with `on.exit(dev.off(), add = TRUE)` — no persistent device is left open.
* No internet access is required for any function.
* No compiled code is included; the package is pure R.
