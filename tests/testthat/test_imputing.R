library(testthat)
library(imputomics)
context("Testing imputation functions")

example_df <- data.frame(
  col1 = c(1, 2, 3, NA, 5),
  col2 = c(1, NA, NA, 4, NA)
)

test_that("test impute_constant()", {
  imputed <- imputomics:::impute_constant(example_df, -1)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, -1, 5),
                              col2 = c(1, -1, -1, 4, -1)),
                         row.names = c(NA, -5L),
                         class = "data.frame")
  )
  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_zero()", {

  imputed <- imputomics:::impute_zero(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 0, 5),
                              col2 = c(1, 0, 0, 4, 0)),
                         row.names = c(NA, -5L),
                         class = "data.frame"))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

#TODO: test `impute_per_column`
# I do not understand how impute_per_column() works
# imputomics:::impute_per_column(df, imputomics:::compute_col_mean)
# imputomics:::compute_col_halfmin(list(1:2, 2:5))
#imputomics:::compute_col_random(list(1:2, 2:5)) # this returns empty list
# imputomics:::compute_col_mean(list(1:2, 2:5))

test_that("test impute_min()", {

  imputed <- imputomics:::impute_min(example_df)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 1, 5),
                              col2 = c(1, 1, 1, 4, 1)),
                         row.names = c(NA, -5L),
                         class = "data.frame")
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_mean()", {

  imputed <- imputomics:::impute_mean(example_df)
  # Function returns column-wise means?
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.75, 5),
                              col2 = c(1, 2.5, 2.5, 4, 2.5)),
                         row.names = c(NA, -5L),
                         class = "data.frame")
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_halfmin()", {

  imputed <- imputomics:::impute_halfmin(example_df)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 0.5, 5),
                              col2 = c(1, 0.5, 0.5, 4, 0.5)),
                         row.names = c(NA, -5L),
                         class = "data.frame")
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_median()", {

  imputed <- imputomics:::impute_median(example_df)
  # one median or column-wise median?
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.75, 5),
                              col2 = c(1, 2.5, 2.5, 4, 2.5)),
                         row.names = c(NA, -5L),
                         class = "data.frame")
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test safe_impute()", {

  imputed <- imputomics:::safe_impute(impute_median, example_df)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.75, 5),
                              col2 = c(1, 2.5, 2.5, 4, 2.5)),
                         row.names = c(NA, -5L),
                         class = "data.frame")
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
  #TODO: test all scenarios
})

test_that("test impute_svd()", {

  imputed <- imputomics::impute_svd(example_df)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, -4.46864767411626e-15, 5),
                              col2 = c(1, -1.62370117351429e-15,
                                       6.93889390390723e-16,
                                       4,
                                       1.16573417585641e-15)),
                         row.names = c(NA, -5L),
                         class = "data.frame")
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_ppca()", {

  set.seed(42)
  imputed <- imputomics::impute_ppca(example_df)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 5.87726795082677e-05, 5),
                              col2 = c(1,
                                       2.93510006826869e-05,
                                       4.40265010237528e-05,
                                       4,
                                       7.33775017067728e-05
                              )),
                         class = "data.frame", row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_bpca()", {

  set.seed(42)
  imputed <- imputomics::impute_bpca(example_df)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 1.41140592392025e-34, 5),
                              col2 = c(1,
                                       -7.0571106725391e-35,
                                       2.3523702241797e-35,
                                       4,
                                       2.11713320176173e-34
                              )),
                         class = "data.frame", row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_nipals()", {

  set.seed(42)
  imputed <- imputomics::impute_nipals(example_df)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 0.17081245613347, 5),
                              col2 = c(1,
                                       0.0902627856409907,
                                       0.135394178461486,
                                       4,
                                       0.225656964102477
                              )),
                         class = "data.frame", row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

# TODO: impute_missmda_em() throws an error for example df
# test_that("test impute_missmda_em()", {
#
#   set.seed(42)
#   imputed <- imputomics::impute_missmda_em(example_df)
#
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0.17081245613347, 5),
#                               col2 = c(1,
#                                        0.0902627856409907,
#                                        0.135394178461486,
#                                        4,
#                                        0.225656964102477
#                               )),
#                          class = "data.frame", row.names = c(NA, -5L))
#   )
#
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# })

test_that("test impute_mice_pmm()", {

  set.seed(42)

  # imputed <- imputomics::impute_mice_pmm(example_df)
  # TODO: throws 113 warnings
  suppressWarnings(imputed <- imputomics::impute_mice_pmm(example_df))
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 1, 5),
                              col2 = c(1, 1, 1, 4, 1)),
                         class = "data.frame", row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_mice_cart()", {

  set.seed(42)

  # imputed <- imputomics::impute_mice_cart(example_df)
  # TODO: throws 113 warnings
  suppressWarnings(imputed <- imputomics::impute_mice_cart(example_df))
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2, 5),
                              col2 = c(1, 4, 1, 4, 4)),
                         class = "data.frame", row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

# TODO: ranger throws errors
# test_that("test impute_mice_rf()", {
#
#   set.seed(42)
#
#   # imputed <- imputomics::impute_mice_rf(example_df)
#   # TODO: throws 113 warnings
#   suppressWarnings(imputed <- imputomics::impute_mice_rf(example_df))
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2, 5),
#                               col2 = c(1, 4, 1, 4, 4)),
#                          class = "data.frame", row.names = c(NA, -5L))
#   )
#
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# })

test_that("test impute_amelia()", {

  set.seed(42)

  # imputed <- imputomics::impute_amelia(example_df)
  # TODO: throws 113 warnings
  suppressWarnings(imputed <- imputomics::impute_amelia(example_df))
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 4.54143757587711, 5),
                              col2 = c(1, 4.05951722548159, 3.95436085457519, 4, 4.00539397413402)),
                         class = "data.frame", row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

# TODO: impute_misforest does not work, example_df is too small
# "The response has five or fewer unique values.  Are you sure you want to do regression?"
# TODO: impute_areg throws an error

test_that("test impute_knn()", {
  imputed <- imputomics:::impute_knn(example_df)
  #TODO: maybe we would like to have `k` param
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 1.1, 5),
                              col2 = c(1, 4.375, 4.375, 4, 4.375)),
                         row.names = c(NA, -5L),
                         class = "data.frame")
  )
  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_qrilc()", {

  set.seed(42)

  imputed <- imputomics::impute_qrilc(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 0.734607230745018, 5),
                              col2 = c(1, 0.71116003879889, -1.36117681618997, 4, 0.631595231957346)),
                         class = "data.frame", row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_twlsa()", {

  imputed <- imputomics::impute_twlsa(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 4, 5),
                              col2 = c(1, 2, 3, 4, 5)),
                         class = "data.frame", row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_softimpute()", {

  set.seed(42)

  # imputed <- imputomics::impute_softimpute(example_df)
  # TODO: throws 113 warnings
  suppressWarnings(imputed <- imputomics::impute_softimpute(example_df))
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 4.72776714361837, 5),
                              col2 = c(1, 1.68911575301901, 2.53958464518727, 4, 4.23165957057264)),
                         class = "data.frame", row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

# TODO: impute_PEMM(example_df) throws an error
# TODO: impute_tknn(example_df) throws an error
# TODO: impute_corknn(example_df) throws an error
# TODO: impute_gsimp(example_df) throws an error

# TODO: below functions return a text row labels
test_that("test impute_MetabImpute_RF()", {

  set.seed(42)
  # TODO: prints notes
  imputed <- imputomics::impute_MetabImpute_RF(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 1, 5),
                              col2 = c(1, 2.365, 2.365, 4, 2.365)),
                         class = "data.frame", row.names = c("1", "2", "3", "4", "5")))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_MetabImpute_BPCA()", {

  set.seed(42)
  # TODO: prints notes
  imputed <- imputomics::impute_MetabImpute_BPCA(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.75, 5),
                              col2 = c(1, 2.5, 2.5, 4, 2.5)),
                         class = "data.frame", row.names = c("1", "2", "3", "4", "5")))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

# TODO: continue https://github.com/michbur/imputomics/blob/main/R/imputing.R#L1027
