test_that("run_sepa returns a sepa_result with correct class", {
  res <- run_sepa(
    data         = fake_wj[1:120, ],
    K            = 4L,
    target_ids   = c(1, 2),
    B_dom        = 30L,
    B_cos        = 30L,
    pa_B         = 20L,
    seed         = 99L,
    run_boot_dom = TRUE,
    run_boot_cos = TRUE,
    verbose      = FALSE
  )
  expect_s3_class(res, "sepa_result")
  expect_named(res, c("call","domains","pid","n","p","K",
                       "ref_fit","Xstar","sepa_stats","pa",
                       "boot_dom","dom_coords","len2","boot_cos",
                       "cosine_tables","dom_dom_cosines","norms","rho_exemplar"))
})

test_that("run_sepa print method runs without error", {
  res <- run_sepa(fake_wj[1:60, ], B_dom = 20L, pa_B = 10L,
                  run_boot_cos = FALSE, verbose = FALSE)
  expect_output(print(res), "SEPA result")
})

test_that("run_sepa works without bootstrap (run_boot_dom = FALSE)", {
  res <- run_sepa(fake_wj[1:50, ], run_boot_dom = FALSE,
                  run_pa = FALSE, verbose = FALSE)
  expect_null(res$boot_dom)
  expect_null(res$pa)
})
