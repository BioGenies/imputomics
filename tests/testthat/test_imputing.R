library(testthat)
library(imputomics)
context("Testing imputation functions")

example_df <- data.frame(
  col1 = c(1, 2, 3, NA, 5),
  col2 = c(1, NA, NA, 4, NA)
)

example_df_w_negatives <- data.frame(
  col1 = c(1, 2, -3, NA, 5),
  col2 = c(1, NA, NA, -4, NA)
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

  imputed_w_negatives <- imputomics::impute_bpca(example_df_w_negatives)
  expect_equal(imputed_w_negatives,
               structure(list(col1 = c(1, 2, -3, 1.75820888398392e-45, 5),
                              col2 = c(1, -5.35532745851259e-46, 3.0346855598238e-45, -4, -2.6776637292563e-45)),
                         class = "data.frame", row.names = c(NA, -5L)))

  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
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

  # TODO: throws warnings
  suppressWarnings(imputed <- imputomics::impute_mice_cart(example_df))
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2, 5),
                              col2 = c(1, 4, 1, 4, 4)),
                         class = "data.frame", row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))

  suppressWarnings(imputed_w_negatives <- imputomics::impute_mice_cart(example_df_w_negatives))
  expect_equal(imputed_w_negatives,
               structure(list(col1 = c(1, 2, -3, 1, 5),
                              col2 = c(1, -4, 1, -4, 1)),
                         row.names = c(NA, -5L), class = "data.frame"))

  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
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

  #TODO: If nrow < 4 * ncol throws an error
  suppressWarnings(imputed <- imputomics::impute_amelia(example_df))
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 4.54143757587711, 5),
                              col2 = c(1, 4.05951722548159, 3.95436085457519, 4, 4.00539397413402)),
                         class = "data.frame", row.names = c(NA, -5L))
  )

  suppressWarnings(imputed_w_negatives <- imputomics::impute_amelia(example_df_w_negatives))
  expect_equal(imputed_w_negatives,
               structure(list(col1 = c(1, 2, -3, -7.90213838452513, 5),
                              col2 = c(1, 1.64993242882426, -1.08944665319059, -4, 3.1504800119)),
                         row.names = c(NA,  -5L), class = "data.frame"))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))

  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
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

  imputed <- imputomics::impute_MetabImpute_BPCA(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.75, 5),
                              col2 = c(1, 2.5, 2.5, 4, 2.5)),
                         class = "data.frame", row.names = c("1", "2", "3", "4", "5")))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
  # TODO: transforms dataset if negative numbers are present
  # imputed_w_negatives <- imputomics::impute_MetabImpute_BPCA(example_df_w_negatives)
  # expect_equal(imputed_w_negatives,
  #              structure(list(col1 = c(1, 2, -3, -7.90213838452513, 5),
  #                             col2 = c(1, 1.64993242882426, -1.08944665319059, -4, 3.1504800119)),
  #                        row.names = c(NA,  -5L), class = "data.frame"))
  #
  # expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
  #              sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
})

test_that("test impute_MetabImpute_QRILC()", {

  set.seed(42)
  # TODO: prints notes
  imputed <- imputomics::impute_MetabImpute_QRILC(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 0.734607230745018, 5),
                              col2 = c(1, 0.71116003879889, 0, 4, 0.631595231957346)),
                         class = "data.frame",
                         row.names = c("1", "2", "3", "4", "5")))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

# test_that("test impute_MetabImpute_GSIMP()", {
#
#   set.seed(42)
#   # TODO: task 1 failed - "x should be a matrix with 2 or more columns"
#   # despite the fact that example_df does have 2 columns
#   imputed <- imputomics::impute_MetabImpute_GSIMP(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0.734607230745018, 5),
#                               col2 = c(1, 0.71116003879889, 0, 4, 0.631595231957346)),
#                          class = "data.frame",
#                          row.names = c("1", "2", "3", "4", "5")))
#
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# })

test_that("test impute_MetabImpute_min()", {

  imputed <- imputomics::impute_MetabImpute_min(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 1, 5),
                              col2 = c(1, 1, 1, 4, 1)),
                         class = "data.frame", row.names = c("1", "2", "3", "4", "5")))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})


test_that("test impute_MetabImpute_halfmin()", {

  imputed <- imputomics::impute_MetabImpute_halfmin(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 0.5, 5),
                              col2 = c(1, 0.5, 0.5, 4, 0.5)),
                         class = "data.frame", row.names = c("1", "2", "3", "4", "5")))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

# TODO:
# test_that("test impute_MetabImpute_rhalfmin()", {
#
#   imputed <- imputomics::impute_MetabImpute_rhalfmin(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0.5, 5),
#                               col2 = c(0, 0, 0, 0, 0)),
#                          class = "data.frame", row.names = c("1", "2", "3", "4", "5")))
#
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# })

test_that("test impute_MetabImpute_mean()", {

  imputed <- imputomics::impute_MetabImpute_mean(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.75, 5),
                              col2 = c(1, 2.5, 2.5, 4, 2.5)),
                         class = "data.frame", row.names = c("1", "2", "3", "4", "5")))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_MetabImpute_median()", {

  imputed <- imputomics::impute_MetabImpute_median(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.5, 5),
                              col2 = c(1, 2.5, 2.5, 4, 2.5)),
                         class = "data.frame", row.names = c("1", "2", "3", "4", "5")))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

test_that("test impute_MetabImpute_zero()", {

  imputed <- imputomics::impute_MetabImpute_zero(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 0, 5),
                              col2 = c(1, 0, 0, 4, 0)),
                         class = "data.frame", row.names = c(NA, -5L)))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

# test_that("test impute_MetabImpute_rmean()", {
#
#   imputed <- imputomics::impute_MetabImpute_rmean(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.75, 5),
#                               col2 = c(0, 0, 0, 0, 0)),
#                          class = "data.frame", row.names = c("1", "2", "3", "4", "5")))
#
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# })
# TODO:
# impute_MetabImpute_rmean() is not working correctly

# test_that("test impute_MetabImpute_rmedian()", {
#
#   imputed <- imputomics::impute_MetabImpute_rmedian(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0, 5),
#                               col2 = c(1, 0, 0, 4, 0)),
#                          class = "data.frame", row.names = c(NA, -5L)))
#
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# })
# TODO:
# impute_MetabImpute_rmedian() is not working correctly
# impute_MetabImpute_rmin() is not working correctly
# impute_MetabImpute_rzero() is not working correctly
# impute_MetabImpute_rrf() is not working correctly
# impute_MetabImpute_rrf() is not working correctly
# TODO: impute_MetabImpute_rGSIMP not working
#Error in { :
#    task 1 failed - "x should be a matrix with 2 or more columns"
# TODO:
# > imputomics:::impute_MetabImpute_rQRILC(example_df)
# Error in 1:nSamples : argument of length 0
# TODO: imputomics:::impute_MetabImpute_rBPCA(example_df) return column zero
# TODO: imputomics:::impute_MA(example_df)
# Not enough data to estimate the pattern of missingness!
# TODO: > imputomics:::impute_eucknn(example_df)
#Error in KNNEuc(as.matrix(missing_data_set), k = ceiling(nrow(missing_data_set) * :
#                                                           Fewer than K finite distances found
# TODO: imputomics:::impute_mice_mixed(example_df) is drunk

test_that("test impute_rmiMAE()", {

  imputed <- imputomics::impute_rmiMAE(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 4, 5),
                              col2 = c(1, 2, 3, 4, 5)),
                         class = "data.frame", row.names = c(NA, -5L)))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

# TODO: > imputomics:::impute_RegImpute(example_df)
# !!! This function ruins the entire session :D
# Error in filled[, -col][present_indices, ] :
# incorrect number of dimensions

# TODO: imputomics:::impute_bcv_svd(example_df) does not return anything
test_that("test impute_bcv_svd()", {

  imputed <- imputomics::impute_bcv_svd(example_df)
  expect_equal(imputed,
               structure(list(X1 = c(1, 2, 3, 2.75, 5),
                              X2 = c(1, 2.5, 2.5, 4, 2.5)),
                         class = "data.frame", row.names = c(NA, -5L)))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))
})

# TODO:imputomics:::impute_imputation_kNN(example_df)
#Error in imputation::kNNImpute(missing_data_set, k = 10) :
#  x should be a numeric data matrix

# TODO: imputomics:::impute_mNMF(example_df)
#Error in do.call(getGeneric("seed"), c(list(x = x, model = init, method = seed.method), :
#                                         'what' must be a function or character string

# TODO: imputomics:::impute_CM(example_df)
# Returns transposition

# TODO:
# > imputed <- imputomics:::impute_BayesMetab(example_df)
# Error in t(sample.x[, , i]) %*% SigInv %*% (as.matrix(Data.init[i, ])) :
#  non-conformable arguments

