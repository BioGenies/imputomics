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

example_df_bigger <- data.frame(
  col1 = c(1, 2, 3, NA, 5),
  col2 = c(1, NA, 4, 4, 0),
  col3 = c(1, 2, 3, NA, 5),
  col4 = c(1, NA, 2, 4, NA),
  col5 = c(1, 2, 3, NA, 5),
  col6 = c(1, NA, NA, 4, 0)
)

example_df_bigger_w_negatives <- data.frame(
  col1 = c(1, 2, 3, NA, 5),
  col2 = c(1, NA, 4, -4, 0),
  col3 = c(1, 2, 3, NA, 5),
  col4 = c(1, NA, -2, 4, NA),
  col5 = c(1, 2, 3, NA, 5),
  col6 = c(1, NA, NA, 4, 0)
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

  imputed <- imputomics:::impute_min(example_df_w_negatives)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, -3, -3, 5),
                              col2 = c(1, -4, -4, -4, -4)),
                         class = "data.frame", row.names = c(NA, -5L)))

  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed[!is.na(example_df_w_negatives)]))
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

  imputed <- imputomics:::impute_mean(example_df_w_negatives)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, -3, 1.25, 5),
                              col2 = c(1, -1.5, -1.5, -4, -1.5)),
                         class = "data.frame",
                         row.names = c(NA, -5L)))

  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed[!is.na(example_df_w_negatives)]))
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

  imputed <- imputomics:::impute_halfmin(example_df_w_negatives)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, -3, -1.5, 5),
                              col2 = c(1, -2, -2, -4, -2)),
                         class = "data.frame",
                         row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed[!is.na(example_df_w_negatives)]))
})

test_that("test impute_median()", {

  imputed <- imputomics:::impute_median(example_df)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.75, 5),
                              col2 = c(1, 2.5, 2.5, 4, 2.5)),
                         row.names = c(NA, -5L),
                         class = "data.frame")
  )

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))

  imputed <- imputomics:::impute_median(example_df_w_negatives)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, -3, 1.25, 5),
                              col2 = c(1, -1.5, -1.5, -4, -1.5)),
                         class = "data.frame",
                         row.names = c(NA, -5L))
  )

  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed[!is.na(example_df_w_negatives)]))
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

  imputed_w_negatives <- imputomics::impute_ppca(example_df_w_negatives)
  expect_equal(imputed_w_negatives,
               structure(list(col1 = c(1, 2, -3, -3.15535981398885e-05, 5),
                              col2 = c(1, 1.57667040923126e-05, -2.365005613858e-05, -4, 3.94167602308926e-05)),
                         class = "data.frame", row.names = c(NA, -5L)))

  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
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

  imputed_w_negatives <- imputomics::impute_nipals(example_df_w_negatives)
  expect_equal(imputed_w_negatives,
               structure(list(col1 = c(1, 2, -3, -0.17081245613347, 5),
                              col2 = c(1, 0.0902627856409907, -0.135394178461486, -4, 0.225656964102477)),
                         class = "data.frame",
                         row.names = c(NA, -5L)))

  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
})

test_that("test impute_missmda_em()", {

  set.seed(42)
  imputed <- imputomics::impute_missmda_em(example_df_bigger)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 5.45979878684297, 5),
                              col2 = c(1, 2.17030108964523, 4, 4, 0),
                              col3 = c(1, 2, 3, 5.45979878684297, 5),
                              col4 = c(1, 1.56858080182216, 2, 4, 4.45299044239315),
                              col5 = c(1, 2, 3, 5.45979878684297, 5),
                              col6 = c(1, 2.17089161524789, 4.00135503323105, 4, 0)),
                         class = "data.frame",
                         row.names = c(NA, -5L))
  )

  # expect_equal(sum(example_df[!is.na(example_df_bigger)]),
  #              sum(imputed[!is.na(example_df_bigger)]))

  imputed_w_negatives <- imputomics::impute_missmda_em(example_df_bigger_w_negatives)
  expect_equal(imputed_w_negatives,
               structure(list(col1 = c(1, 2, 3, 2.47735909982635, 5),
                              col2 = c(1, 0.101234839443088, 4, -4, 0),
                              col3 = c(1, 2, 3, 2.47735909982635, 5),
                              col4 = c(1, 1.23376735529684, -2, 4, 0.0535437684372712),
                              col5 = c(1, 2, 3, 2.47735909982635, 5),
                              col6 = c(1, 1.22299876433246, -2.04313682535228, 4, 0)),
                         class = "data.frame", row.names = c(NA, -5L)))

  # expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
  #              sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
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

  suppressWarnings(imputed_w_negatives <- imputomics::impute_mice_pmm(example_df_w_negatives))
  expect_equal(imputed_w_negatives,
               structure(list(col1 = c(1, 2, -3, 1, 5),
                              col2 = c(1, -4, -4, -4, -4)),
                         row.names = c(NA, -5L), class = "data.frame"))

  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
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

test_that("test impute_missforest()", {

  set.seed(42)
  suppressWarnings(imputed <- imputomics:::impute_missforest(example_df_bigger))

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.584, 5),
                              col2 = c(1, 3.238, 4, 4, 0),
                              col3 = c(1, 2, 3, 2.61, 5),
                              col4 = c(1, 2.844, 2, 4, 2.072),
                              col5 = c(1, 2, 3, 2.608, 5),
                              col6 = c(1, 2.762, 2.582, 4, 0)),
                         row.names = c(NA, -5L),
                         class = "data.frame")
  )
  expect_equal(sum(example_df_bigger[!is.na(example_df_bigger)]),
               sum(imputed[!is.na(example_df_bigger)]))

  suppressWarnings(imputed <- imputomics:::impute_missforest(example_df_bigger_w_negatives))
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.716, 5),
                              col2 = c(1, 0.176, 4, -4, 0),
                              col3 = c(1, 2, 3, 2.738, 5),
                              col4 = c(1, 1.774, -2, 4, -0.032),
                              col5 = c(1, 2, 3, 2.764, 5),
                              col6 = c(1, 1.746, 1.752, 4, 0)),
                         row.names = c(NA, -5L),
                         class = "data.frame")
  )
  expect_equal(sum(example_df_bigger_w_negatives[!is.na(example_df_bigger_w_negatives)]),
               sum(imputed[!is.na(example_df_bigger_w_negatives)]))
})

# TODO: impute_misforest does not work, example_df is too small
# "The response has five or fewer unique values.  Are you sure you want to do regression?"
# TODO: impute_areg throws an error

test_that("test impute_eucknn()", {

  imputed <- imputomics:::impute_eucknn(example_df_bigger)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.35924551796592, 5),
                              col2 = c(1, 2.5, 4, 4, 0),
                              col3 = c(1, 2, 3, 2.35924551796592, 5),
                              col4 = c(1, 1.5, 2, 4, 1.54446657821975),
                              col5 = c(1, 2, 3, 2.35924551796592, 5),
                              col6 = c(1, 0.75, 2.79190075645217, 4, 0)),
                         class = "data.frame",
                         row.names = c(NA, -5L))
  )
  expect_equal(sum(example_df_bigger[!is.na(example_df_bigger)]),
               sum(imputed[!is.na(example_df_bigger)]))

  imputed <- imputomics:::impute_eucknn(example_df_bigger_w_negatives)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.94501341312175, 5),
                              col2 = c(1, 2.5, 4, -4, 0),
                              col3 = c(1, 2, 3, 2.94501341312175, 5),
                              col4 = c(1, -0.5, -2, 4, -0.633399734659244),
                              col5 = c(1, 2, 3, 2.94501341312175, 5),
                              col6 = c(1, 0.75, 0.51925930159214, 4, 0)),
                         class = "data.frame",
                         row.names = c(NA, -5L))
  )
  expect_equal(sum(example_df_bigger_w_negatives[!is.na(example_df_bigger_w_negatives)]),
               sum(imputed[!is.na(example_df_bigger_w_negatives)]))
})

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

  imputed <- imputomics:::impute_knn(example_df_w_negatives)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, -3, 0.5, 5),
                              col2 = c(1, 3.375, 3.375, -4, 3.375)),
                         class = "data.frame",
                         row.names = c(NA, -5L))
  )
  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed[!is.na(example_df_w_negatives)]))
})

test_that("test impute_vim_knn()", {
  imputed <- imputomics:::impute_vim_knn(example_df)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 1, 5),
                              col2 = c(1, 1, 1, 4, 1)),
                         class = "data.frame",
                         row.names = c(NA, -5L))
  )
  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))

  imputed <- imputomics:::impute_vim_knn(example_df_w_negatives)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, -3, 1, 5),
                              col2 = c(1, 1, 1, -4, 1)),
                         class = "data.frame",
                         row.names = c(NA, -5L))
  )
  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed[!is.na(example_df_w_negatives)]))
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

  imputed <- imputomics:::impute_qrilc(example_df_w_negatives)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, -3, -3.90228834118572, 5),
                              col2 = c(1, -3.47292941046733, -8.1347427856126, -4, -5.24440453439759)),
                         row.names = c(NA, -5L), class = "data.frame"))

  expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
               sum(imputed[!is.na(example_df_w_negatives)]))
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


test_that("test impute_gsimp()", {

  set.seed(42)
  imputed <- imputomics::impute_gsimp(example_df_bigger)
  expect_equal(imputed,
               structure(c(1, 2, 3, 0.991618232928376, 5, 1, -0.00661838856317039,
                           4, 4, 0, 1, 2, 3, 0.990427351471755, 5, 1, -0.012484116059566,
                           2, 4, 0.0398736860894961, 1, 2, 3, 0.998376812881738, 5, 1, -0.101424194302568,
                           -0.0999069615915867, 4, 0), dim = 5:6, dimnames = list(NULL,
                                                                                  c("col1", "col2", "col3", "col4", "col5", "col6")))
  )
  # TODO: Fix output; now returns matrix
  # expect_equal(sum(example_df[!is.na(example_df_bigger)]),
  #              sum(imputed[!is.na(example_df_bigger)]))

  imputed <- imputomics::impute_gsimp(example_df_bigger_w_negatives)
  expect_equal(imputed,
               structure(c(1, 2, 3, 0.953770061032737, 5, 1, -4.80734571159551,
                           4, -4, 0, 1, 2, 3, 0.945704607146221, 5, 1, -2.01669659299205,
                           -2, 4, -4.31156621084011, 1, 2, 3, 0.938559342343023, 5, 1, -0.0093391921527519,
                           -0.147178897738088, 4, 0), dim = 5:6, dimnames = list(NULL, c("col1",
                                                                                         "col2", "col3", "col4", "col5", "col6")))
  )
  # TODO:
  # expect_equal(sum(example_df[!is.na(example_df_bigger_w_negatives)]),
  #              sum(imputed[!is.na(example_df_bigger_w_negatives)]))
})


# TODO: below functions return a text row labels
test_that("test impute_MetabImpute_RF()", {

  set.seed(42)

  imputed <- imputomics::impute_MetabImpute_RF(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 1, 5),
                              col2 = c(1, 2.365, 2.365, 4, 2.365)),
                         class = "data.frame", row.names = c("1", "2", "3", "4", "5")))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))

  imputed <- imputomics::impute_MetabImpute_RF(example_df_bigger_w_negatives)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 1, 5),
                              col2 = c(1, 0.1275, 4, 0, 0),
                              col3 = c(1, 2, 3, 2.795, 5),
                              col4 = c(1, 1.32, 0, 4, 1.32),
                              col5 = c(1, 2, 3, 2.785, 5),
                              col6 = c(1, 1.69333333333333, 1.69333333333333, 4, 0)),
                         class = "data.frame",
                         row.names = c("1", "2", "3", "4", "5")))

  # expect_equal(sum(example_df[!is.na(example_df_bigger_w_negatives)]),
  #              sum(imputed[!is.na(example_df_bigger_w_negatives)]))
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

  imputed <- imputomics:::impute_MetabImpute_QRILC(example_df_w_negatives)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 0, 0, 5),
                              col2 = c(1, 0, 0, 0, 0)),
                         class = "data.frame",
                         row.names = c("1", "2", "3", "4", "5")))
  # expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
  #              sum(imputed[!is.na(example_df_w_negatives)]))
})

# TODO:
# test_that("test impute_MetabImpute_GSIMP()", {
#
#   set.seed(42)
#   imputed <- imputomics::impute_MetabImpute_GSIMP(example_df_bigger)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0.998999002411531, 5), col2 = c(1,
#                                                                                 0.936543654286782, 4, 4, 1), col3 = c(1, 2, 3, 0.987659014108988,
#                                                                                                                       5), col4 = c(1, 0.309695122749475, 2, 4, 0.99758189965559), col5 = c(1,
#                                                                                                                                                                                            2, 3, 0.987627348352668, 5), col6 = c(1, 0.982447335027215, 0.994604787636155,
#                                                                                                                                                                                                                                  4, 1)), class = "data.frame", row.names = c("1", "2", "3", "4",
#                                                                                                                                                                                                                                                                              "5")))
#
#   # TODO: Does not work with zeros
#   # expect_equal(sum(example_df_bigger[!is.na(example_df_bigger)]),
#   #              sum(imputed[!is.na(example_df_bigger)]))
# })

test_that("test impute_MetabImpute_min()", {

  imputed <- imputomics::impute_MetabImpute_min(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 1, 5),
                              col2 = c(1, 1, 1, 4, 1)),
                         class = "data.frame", row.names = c("1", "2", "3", "4", "5")))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))

  imputed <- imputomics:::impute_MetabImpute_min(example_df_w_negatives)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 0, 0, 5),
                              col2 = c(1, 0, 0, 0, 0)),
                         class = "data.frame",
                         row.names = c("1", "2", "3", "4", "5"))
  )

  # expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
  #              sum(imputed[!is.na(example_df_w_negatives)]))


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

  imputed <- imputomics:::impute_MetabImpute_mean(example_df_w_negatives)

  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 0, 1.25, 5),
                              col2 = c(1, 0, 0, 0, 0)),
                         class = "data.frame",
                         row.names = c("1", "2", "3", "4", "5"))
  )

  # expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
  #              sum(imputed[!is.na(example_df_w_negatives)]))

})

test_that("test impute_MetabImpute_median()", {

  imputed <- imputomics::impute_MetabImpute_median(example_df)
  expect_equal(imputed,
               structure(list(col1 = c(1, 2, 3, 2.5, 5),
                              col2 = c(1, 2.5, 2.5, 4, 2.5)),
                         class = "data.frame", row.names = c("1", "2", "3", "4", "5")))

  expect_equal(sum(example_df[!is.na(example_df)]),
               sum(imputed[!is.na(example_df)]))

  imputed_w_negatives <- imputomics::impute_MetabImpute_median(example_df_w_negatives)
  expect_equal(imputed_w_negatives,
               structure(list(col1 = c(1, 2, 3, 2.5, 5),
                              col2 = c(1, 2.5, 2.5, 4, 2.5)),
                         class = "data.frame",
                         row.names = c("1", "2", "3", "4", "5")))

  # expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
  #              sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
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

# TODO:
# > imputed <- imputomics:::impute_BayesMetab(example_df)
# Error in t(sample.x[, , i]) %*% SigInv %*% (as.matrix(Data.init[i, ])) :
#  non-conformable arguments

test_that("test impute_CM()", {

  set.seed(42)

  # TODO: throws warnings
  imputed <- imputomics::impute_CM(example_df)
  expect_equal(imputed,
               structure(list(X1 = c(1, 1),
                              X2 = c(2, 1),
                              X3 = c(3, 1),
                              X4 = c(1, 4),
                              X5 = c(5, 1)),
                         class = "data.frame", row.names = c("col1", "col2")))

  #TODO:
  # expect_equal(sum(example_df[!is.na(example_df)]),
  #              sum(imputed[!is.na(example_df)]))
  imputed_w_negatives <- imputomics::impute_CM(example_df_w_negatives)
  expect_equal(imputed_w_negatives,
               structure(list(X1 = c(1, 1),
                              X2 = c(2, -4),
                              X3 = c(-3, -4),
                              X4 = c(-3, -4),
                              X5 = c(5, -4)),
                         class = "data.frame", row.names = c("col1", "col2")))

  # TODO:
  # expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
  #              sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
})
