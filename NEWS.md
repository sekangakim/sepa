# SEPA 0.1.0

## New features

* `simulate_sepa_fake_wj()`: generate a synthetic WJ-IV look-alike dataset.
* `sepa_stats_all()`: compute plane-fit rho and direction cosines.
* `parallel_analysis_ipsatized()`: permutation-based dimensionality selection
  for ipsatized data.
* `boot_cis_all()`: BCa (with percentile fallback) CIs for all bootstrap
  indices from a `boot` object.
* `percentile_ci_mat()`: percentile CIs from a matrix of bootstrap draws.
* `draw_sepa_biplot()`: base-R row-isometric SVD biplot with optional
  highlighting and PDF output.
* `write_long_to_wide()`, `write_matrix_wide()`: tidy data reshaping helpers.
* `run_sepa()`: end-to-end wrapper executing the full SEPA pipeline.
* `fake_wj`: bundled 5 127-case synthetic dataset.

## Bug fixes / clean-up (relative to development script)

* Loop variable renamed `pkg` (was `p`, conflicted with `ncol` variable).
* Duplicate `cos_index()` header comments removed.
* `get_domain_names()` defined once only.
* `sepa_stats_all()` defined once only.
* `out` object built once; redundant calls removed.
* `drop = TRUE` → `drop = FALSE` in all multi-column Procrustes slices.
* `planes` character vector renamed `plane_labels` (was overwriting list).
* `pid` initialised unconditionally.
* `<<-` superassignment removed from `save_pdf()`; restructured cleanly.
* `install.packages()` removed from body.
* All global-variable references in bootstrap statistics replaced with
  explicit arguments.
