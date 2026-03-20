# SEPA: Subprofile Extraction via Pattern Analysis

[![CRAN status](https://www.r-pkg.org/badges/version/SEPA)](https://CRAN.R-project.org/package=SEPA)

**SEPA** implements the Subprofile Extraction via Pattern Analysis method for ipsatized multivariate data. Given a matrix of domain scores (e.g., WJ-IV broad ability scores), SEPA removes the level-elevation component, fits a rank-*K* row-isometric SVD biplot, and quantifies each person's alignment with each domain via direction cosines, with bootstrap confidence intervals.

## Installation

```r
# Install from CRAN:
install.packages("SEPA")
```

## Quick start

```r
library(SEPA)

# Run the full pipeline on the bundled synthetic dataset
res <- run_sepa(
  data         = fake_wj,
  K            = 4,
  target_ids   = c(724, 944, 1080, 2117),
  B_dom        = 2000,
  B_cos        = 2000,
  seed         = 20251003
)

print(res)
# SEPA result
#   n = 5127  p = 7  K = 4
#   Domains: LT, ST, CP, AP, VP, CK, FL
#   PA significant dims: 1, 2, 3, 4
#   Marker domains: LT, ST, VP, CK

# Exemplar cosine table for Plane 1
head(res$cosine_tables[["12"]], 14)

# Biplot
draw_sepa_biplot(
  svd_fit      = list(U = res$ref_fit$U, d = res$ref_fit$d_all, V = res$ref_fit$V),
  id_vec       = res$pid,
  domain_names = res$domains,
  ids_highlight = c(724, 944, 1080, 2117)
)
```

## Key functions

| Function | Description |
|---|---|
| `run_sepa()` | Full pipeline wrapper |
| `sepa_stats_all()` | Plane-fit rho + direction cosines |
| `parallel_analysis_ipsatized()` | Permutation-based dimensionality selection |
| `boot_cis_all()` | BCa confidence intervals |
| `percentile_ci_mat()` | Percentile confidence intervals |
| `draw_sepa_biplot()` | Row-isometric SVD biplot |
| `simulate_sepa_fake_wj()` | Synthetic WJ-IV data generator |

## Data

`fake_wj` is a 5,127-case synthetic dataset that approximates the WJ-IV marginal distributions. The original norming data are proprietary; this object provides a fully reproducible substitute.

## Citation

If you use SEPA in your research, please cite:

> Kim, S.-K. (2025). *SEPA: Subprofile Extraction via Pattern Analysis*. R package version 0.1.0. https://CRAN.R-project.org/package=SEPA

## License

MIT © Se-Kang Kim
