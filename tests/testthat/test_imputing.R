# library(testthat)
# library(imputomics)
# context("Testing imputation functions")
# 
# example_df <- data.frame(
#   col1 = c(1, 2, 3, NA, 5),
#   col2 = c(1, NA, NA, 4, NA)
# )
# 
# example_df_w_negatives <- data.frame(
#   col1 = c(1, 2, -3, NA, 5),
#   col2 = c(1, NA, NA, -4, NA)
# )
# 
# example_df_bigger <- data.frame(
#   col1 = c(1, 2, 3, NA, 5),
#   col2 = c(1, NA, 4, 4, 0),
#   col3 = c(1, 2, 3, NA, 5),
#   col4 = c(1, NA, 2, 4, NA),
#   col5 = c(1, 2, 3, NA, 5),
#   col6 = c(1, NA, NA, 4, 0)
# )
# 
# example_df_bigger_w_negatives <- data.frame(
#   col1 = c(1, 2, 3, NA, 5),
#   col2 = c(1, NA, 4, -4, 0),
#   col3 = c(1, 2, 3, NA, 5),
#   col4 = c(1, NA, -2, 4, NA),
#   col5 = c(1, 2, 3, NA, 5),
#   col6 = c(1, NA, NA, 4, 0)
# )
# 
# 
# test_that("test impute_constant()", {
#   imputed <- imputomics:::impute_constant(example_df, -1)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, -1, 5),
#                               col2 = c(1, -1, -1, 4, -1)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame")
#   )
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# })
# 
# test_that("test impute_zero()", {
# 
#   imputed <- imputomics:::impute_zero(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0, 5),
#                               col2 = c(1, 0, 0, 4, 0)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame"))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
#   imputed <- imputomics:::impute_zero(example_df_w_negatives)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, -3, 0, 5),
#                               col2 = c(1, 0, 0, -4, 0)),
#                          row.names = c(NA, -5L), class = "data.frame"))
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed[!is.na(example_df_w_negatives)]))
# })
# 
# 
# test_that("test impute_min()", {
# 
#   imputed <- imputomics:::impute_min(example_df)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 1, 5),
#                               col2 = c(1, 1, 1, 4, 1)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame")
#   )
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics:::impute_min(example_df_w_negatives)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, -3, -3, 5),
#                               col2 = c(1, -4, -4, -4, -4)),
#                          class = "data.frame", row.names = c(NA, -5L)))
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_mean()", {
# 
#   imputed <- imputomics:::impute_mean(example_df)
#   # Function returns column-wise means?
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.75, 5),
#                               col2 = c(1, 2.5, 2.5, 4, 2.5)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame")
#   )
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics:::impute_mean(example_df_w_negatives)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, -3, 1.25, 5),
#                               col2 = c(1, -1.5, -1.5, -4, -1.5)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L)))
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_halfmin()", {
# 
#   imputed <- imputomics:::impute_halfmin(example_df)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0.5, 5),
#                               col2 = c(1, 0.5, 0.5, 4, 0.5)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame")
#   )
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics:::impute_halfmin(example_df_w_negatives)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, -3, -1.5, 5),
#                               col2 = c(1, -2, -2, -4, -2)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#   )
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_median()", {
# 
#   imputed <- imputomics:::impute_median(example_df)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.75, 5),
#                               col2 = c(1, 2.5, 2.5, 4, 2.5)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame")
#   )
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics:::impute_median(example_df_w_negatives)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, -3, 1.25, 5),
#                               col2 = c(1, -1.5, -1.5, -4, -1.5)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#   )
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test safe_impute()", {
# 
#   imputed <- imputomics:::safe_impute(impute_median, example_df)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.75, 5),
#                               col2 = c(1, 2.5, 2.5, 4, 2.5)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame")
#   )
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
#   #TODO: test all scenarios
# })
# 
# test_that("test impute_svd()", {
# 
#   imputed <- imputomics::impute_svd(example_df)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.75, 5),
#                               col2 = c(1, 2.5, 2.5, 4, 2.5)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#   )
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed_w_negatives <- imputomics::impute_svd(example_df_w_negatives)
#   expect_equal(imputed_w_negatives,
#                structure(list(col1 = c(1, 2, -3, 1.25, 5),
#                               col2 = c(1, -1.5, -1.50000000000001, -4, -1.49999999999999)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#                )
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_ppca()", {
# 
#   set.seed(42)
#   imputed <- imputomics::impute_ppca(example_df_bigger)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0.820137020221041, 5),
#                               col2 = c(1, 2.61453684220298, 4, 4, 0),
#                               col3 = c(1, 2, 3, 0.820137020221042, 5),
#                               col4 = c(1, 2.57700084488273, 2, 4, 1.01306079760239),
#                               col5 = c(1, 2, 3, 0.820137020221042, 5),
#                               col6 = c(1, 2.10232897180665, 2.38867272256023, 4, 0)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#   )
# 
#   imputed_w_negatives <- imputomics::impute_ppca(example_df_w_negatives)
#   expect_equal(imputed_w_negatives,
#                structure(list(col1 = c(1, 2, -3, 1.70957695683133, 5),
#                               col2 = c(1, -5.56800597754004, 21.5520338727766, -4, -21.8400298877205)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#                )
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_bpca()", {
# 
#   set.seed(42)
#   imputed <- imputomics::impute_bpca(example_df_bigger)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.75, 5),
#                               col2 = c(1, 2.25, 4, 4, 0),
#                               col3 = c(1, 2, 3, 2.75, 5),
#                               col4 = c(1, 2.33333333333333, 2, 4, 2.33333333333333),
#                               col5 = c(1, 2, 3, 2.75, 5),
#                               col6 = c(1, 1.66666666666667, 1.66666666666667, 4, 0)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#   )
# 
#   imputed_w_negatives <- imputomics::impute_bpca(example_df_w_negatives)
#   expect_equal(imputed_w_negatives,
#                structure(list(col1 = c(1, 2, -3, 1.25, 5),
#                               col2 = c(1, -1.5, -1.5, -4, -1.5)),
#                          class = "data.frame", row.names = c(NA, -5L))
#                )
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_nipals()", {
# 
#   set.seed(42)
#   imputed <- imputomics::impute_nipals(example_df)
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
# 
#   imputed_w_negatives <- imputomics::impute_nipals(example_df_w_negatives)
#   expect_equal(imputed_w_negatives,
#                structure(list(col1 = c(1, 2, -3, -0.17081245613347, 5),
#                               col2 = c(1, 0.0902627856409907, -0.135394178461486, -4, 0.225656964102477)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L)))
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_missmda_em()", {
# 
#   set.seed(42)
#   imputed <- imputomics::impute_missmda_em(example_df_bigger)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 6.50534706914778, 5),
#                               col2 = c(1, 2.42634073508925, 4, 4, 0),
#                               col3 = c(1, 2, 3, 6.50534706914778, 5),
#                               col4 = c(1, 1.50355563692155, 2, 4, 3.32142934388893),
#                               col5 = c(1, 2, 3, 6.50534706914778, 5),
#                               col6 = c(1, 2.42513105868879, 3.99808150929319, 4, 0)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#   )
# 
#   imputed_w_negatives <- imputomics::impute_missmda_em(example_df_bigger_w_negatives)
#   expect_equal(imputed_w_negatives,
#                structure(list(col1 = c(1, 2, 3, 2.55900876078867, 5),
#                               col2 = c(1, 0.0389093616470934, 4, -4, 0),
#                               col3 = c(1, 2, 3, 2.55900876078867, 5),
#                               col4 = c(1, 1.29008820103569, -2, 4, 0.0799409932605786),
#                               col5 = c(1, 2, 3, 2.55900876078867, 5),
#                               col6 = c(1, 1.27476144640184, -2.06546756580745, 4, 0)),
#                          class = "data.frame", row.names = c(NA, -5L))
#   )
# })
# 
# test_that("test impute_mice_pmm()", {
# 
#   set.seed(42)
# 
#   suppressWarnings(imputed <- imputomics::impute_mice_pmm(example_df))
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 1, 5),
#                               col2 = c(1, 1, 1, 4, 1)),
#                          class = "data.frame", row.names = c(NA, -5L))
#   )
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   suppressWarnings(imputed_w_negatives <- imputomics::impute_mice_pmm(example_df_w_negatives))
#   expect_equal(imputed_w_negatives,
#                structure(list(col1 = c(1, 2, -3, 1, 5),
#                               col2 = c(1, -4, -4, -4, -4)),
#                          row.names = c(NA, -5L), class = "data.frame"))
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_mice_cart()", {
# 
#   set.seed(42)
# 
#   # TODO: throws warnings
#   suppressWarnings(imputed <- imputomics::impute_mice_cart(example_df))
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2, 5),
#                               col2 = c(1, 4, 4, 4, 1)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame")
#   )
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   suppressWarnings(imputed_w_negatives <- imputomics::impute_mice_cart(example_df_w_negatives))
#   expect_equal(imputed_w_negatives,
#                structure(list(col1 = c(1, 2, -3, -3, 5),
#                               col2 = c(1, -4, -4, -4, 1)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame")
#   )
# 
#                expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_amelia()", {
# 
#   set.seed(42)
# 
#   #TODO: If nrow < 4 * ncol throws an error
#   suppressWarnings(imputed <- imputomics::impute_amelia(example_df))
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 4.54143757587711, 5),
#                               col2 = c(1, 4.05951722548159, 3.95436085457519, 4, 4.00539397413402)),
#                          class = "data.frame", row.names = c(NA, -5L))
#   )
# 
#   suppressWarnings(imputed_w_negatives <- imputomics::impute_amelia(example_df_w_negatives))
#   expect_equal(imputed_w_negatives,
#                structure(list(col1 = c(1, 2, -3, -7.90213838452513, 5),
#                               col2 = c(1, 1.64993242882426, -1.08944665319059, -4, 3.1504800119)),
#                          row.names = c(NA,  -5L), class = "data.frame"))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_missforest()", {
# 
#   set.seed(42)
#   suppressWarnings(imputed <- imputomics:::impute_missforest(example_df_bigger))
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.584, 5),
#                               col2 = c(1, 3.238, 4, 4, 0),
#                               col3 = c(1, 2, 3, 2.61, 5),
#                               col4 = c(1, 2.844, 2, 4, 2.072),
#                               col5 = c(1, 2, 3, 2.608, 5),
#                               col6 = c(1, 2.762, 2.582, 4, 0)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame")
#   )
#   expect_equal(sum(example_df_bigger[!is.na(example_df_bigger)]),
#                sum(imputed[!is.na(example_df_bigger)]))
# 
#   suppressWarnings(imputed <- imputomics:::impute_missforest(example_df_bigger_w_negatives))
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.716, 5),
#                               col2 = c(1, 0.176, 4, -4, 0),
#                               col3 = c(1, 2, 3, 2.738, 5),
#                               col4 = c(1, 1.774, -2, 4, -0.032),
#                               col5 = c(1, 2, 3, 2.764, 5),
#                               col6 = c(1, 1.746, 1.752, 4, 0)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame")
#   )
#   expect_equal(sum(example_df_bigger_w_negatives[!is.na(example_df_bigger_w_negatives)]),
#                sum(imputed[!is.na(example_df_bigger_w_negatives)]))
# })
# 
# 
# test_that("test impute_eucknn()", {
# 
#   imputed <- imputomics:::impute_eucknn(example_df_bigger)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.35924551796592, 5),
#                               col2 = c(1, 2.5, 4, 4, 0),
#                               col3 = c(1, 2, 3, 2.35924551796592, 5),
#                               col4 = c(1, 1.5, 2, 4, 1.54446657821975),
#                               col5 = c(1, 2, 3, 2.35924551796592, 5),
#                               col6 = c(1, 0.75, 2.79190075645217, 4, 0)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#   )
#   expect_equal(sum(example_df_bigger[!is.na(example_df_bigger)]),
#                sum(imputed[!is.na(example_df_bigger)]))
# 
#   imputed <- imputomics:::impute_eucknn(example_df_bigger_w_negatives)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.94501341312175, 5),
#                               col2 = c(1, 2.5, 4, -4, 0),
#                               col3 = c(1, 2, 3, 2.94501341312175, 5),
#                               col4 = c(1, -0.5, -2, 4, -0.633399734659244),
#                               col5 = c(1, 2, 3, 2.94501341312175, 5),
#                               col6 = c(1, 0.75, 0.51925930159214, 4, 0)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#   )
#   expect_equal(sum(example_df_bigger_w_negatives[!is.na(example_df_bigger_w_negatives)]),
#                sum(imputed[!is.na(example_df_bigger_w_negatives)]))
# })
# 
# test_that("test impute_knn()", {
#   imputed <- imputomics:::impute_knn(example_df)
#   #TODO: maybe we would like to have `k` param
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 1.1, 5),
#                               col2 = c(1, 4.375, 4.375, 4, 4.375)),
#                          row.names = c(NA, -5L),
#                          class = "data.frame")
#   )
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics:::impute_knn(example_df_w_negatives)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, -3, 0.5, 5),
#                               col2 = c(1, 3.375, 3.375, -4, 3.375)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#   )
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_vim_knn()", {
#   imputed <- imputomics:::impute_vim_knn(example_df)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2, 5),
#                               col2 = c(1, 1, 1, 4, 1)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L)))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics:::impute_vim_knn(example_df_w_negatives)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, -3, 1, 5),
#                               col2 = c(1, 1, 1, -4, 1)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#   )
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_qrilc()", {
# 
#   set.seed(42)
# 
#   imputed <- imputomics::impute_qrilc(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0.734607230745018, 5),
#                               col2 = c(1, 0.71116003879889, -1.36117681618997, 4, 0.631595231957346)),
#                          class = "data.frame", row.names = c(NA, -5L))
#   )
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics:::impute_qrilc(example_df_w_negatives)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, -3, -3.90228834118572, 5),
#                               col2 = c(1, -3.47292941046733, -8.1347427856126, -4, -5.24440453439759)),
#                          row.names = c(NA, -5L), class = "data.frame"))
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_softimpute()", {
# 
#   set.seed(42)
# 
#   suppressWarnings(imputed <- imputomics::impute_softimpute(example_df))
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 4.72776714361837, 5),
#                               col2 = c(1, 1.68911575301901, 2.53958464518727, 4, 4.23165957057264)),
#                          class = "data.frame", row.names = c(NA, -5L))
#   )
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics::impute_softimpute(example_df_bigger_w_negatives)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0.42085699464543, 5),
#                               col2 = c(1, -1.62751387206527, 4, -4, 0),
#                               col3 = c(1, 2, 3, 0.42085699464543, 5),
#                               col4 = c(1, 2.80512651687386, -2, 4, 2.92625185721165),
#                               col5 = c(1, 2, 3, 0.42085699464543, 5),
#                               col6 = c(1, 1.80306107334002, -3.81838789422431, 4, 0)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L)))
# 
#   expect_equal(sum(example_df_bigger_w_negatives[!is.na(example_df_bigger_w_negatives)]),
#                sum(imputed[!is.na(example_df_bigger_w_negatives)]))
# })
# 
# test_that("test impute_MetabImpute_RF()", {
# 
#   set.seed(42)
# 
#   imputed <- imputomics::impute_MetabImpute_RF(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 1, 5),
#                               col2 = c(1, 2.365, 2.365, 4, 2.365)),
#                          class = "data.frame", row.names = c("1", "2", "3", "4", "5")))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics::impute_MetabImpute_RF(example_df_bigger_w_negatives)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 1, 5),
#                               col2 = c(1, 0.1275, 4, 0, 0),
#                               col3 = c(1, 2, 3, 2.795, 5),
#                               col4 = c(1, 1.32, 0, 4, 1.32),
#                               col5 = c(1, 2, 3, 2.785, 5),
#                               col6 = c(1, 1.69333333333333, 1.69333333333333, 4, 0)),
#                          class = "data.frame",
#                          row.names = c("1", "2", "3", "4", "5")))
# 
# })
# 
# test_that("test impute_MetabImpute_BPCA()", {
# 
#   set.seed(42)
# 
#   imputed <- imputomics::impute_MetabImpute_BPCA(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.75, 5),
#                               col2 = c(1, 2.5, 2.5, 4, 2.5)),
#                          class = "data.frame", row.names = c("1", "2", "3", "4", "5")))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# })
# 
# test_that("test impute_MetabImpute_QRILC()", {
# 
#   set.seed(42)
# 
#   imputed <- imputomics::impute_MetabImpute_QRILC(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0.734607230745018, 5),
#                               col2 = c(1, 0.71116003879889, 0, 4, 0.631595231957346)),
#                          class = "data.frame",
#                          row.names = c("1", "2", "3", "4", "5")))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics:::impute_MetabImpute_QRILC(example_df_w_negatives)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 0, 0, 5),
#                               col2 = c(1, 0, 0, 0, 0)),
#                          class = "data.frame",
#                          row.names = c("1", "2", "3", "4", "5")))
# 
# })
# 
# test_that("test impute_MetabImpute_min()", {
# 
#   imputed <- imputomics::impute_MetabImpute_min(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 1, 5),
#                               col2 = c(1, 1, 1, 4, 1)),
#                          class = "data.frame", row.names = c("1", "2", "3", "4", "5")))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics:::impute_MetabImpute_min(example_df_w_negatives)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 0, 0, 5),
#                               col2 = c(1, 0, 0, 0, 0)),
#                          class = "data.frame",
#                          row.names = c("1", "2", "3", "4", "5"))
#   )
# 
#   # expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#   #              sum(imputed[!is.na(example_df_w_negatives)]))
# 
# 
# })
# 
# 
# test_that("test impute_MetabImpute_halfmin()", {
# 
#   imputed <- imputomics::impute_MetabImpute_halfmin(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0.5, 5),
#                               col2 = c(1, 0.5, 0.5, 4, 0.5)),
#                          class = "data.frame", row.names = c("1", "2", "3", "4", "5")))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# })
# 
# # test_that("test impute_MetabImpute_rhalfmin()", {
# #
# #   imputed <- imputomics::impute_MetabImpute_rhalfmin(example_df_bigger)
# #   expect_equal(imputed,
# #                structure(list(col1 = c(1, 2, 3, 0.5, 5),
# #                               col2 = c(1, 0.5, 4, 4, 0.5),
# #                               col3 = c(1, 2, 3, 0.5, 5),
# #                               col4 = c(1, 0.5, 2, 4, 0.5),
# #                               col5 = c(1, 2, 3, 0.5, 5),
# #                               col6 = c(0, 0, 0, 0, 0)),
# #                          class = "data.frame",
# #                          row.names = c("1", "2", "3", "4", "5")))
# #
# #   expect_equal(sum(example_df[!is.na(example_df_bigger)]),
# #                sum(imputed[!is.na(example_df_bigger)]))
# #
# #   imputed <- imputomics:::impute_MetabImpute_rhalfmin(example_df_w_negatives)
# #
# #   expect_equal(imputed,
# #                structure(list(col1 = c(1, 2, 0, 1.25, 5),
# #                               col2 = c(1, 0, 0, 0, 0)),
# #                          class = "data.frame",
# #                          row.names = c("1", "2", "3", "4", "5"))
# #   )
# #
# #   # expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
# #   #              sum(imputed[!is.na(example_df_w_negatives)]))
# #
# # })
# 
# 
# test_that("test impute_MetabImpute_mean()", {
# 
#   imputed <- imputomics::impute_MetabImpute_mean(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.75, 5),
#                               col2 = c(1, 2.5, 2.5, 4, 2.5)),
#                          class = "data.frame", row.names = c("1", "2", "3", "4", "5")))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed <- imputomics:::impute_MetabImpute_mean(example_df_w_negatives)
# 
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 0, 1.25, 5),
#                               col2 = c(1, 0, 0, 0, 0)),
#                          class = "data.frame",
#                          row.names = c("1", "2", "3", "4", "5"))
#   )
# 
#   # expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#   #              sum(imputed[!is.na(example_df_w_negatives)]))
# 
# })
# 
# test_that("test impute_MetabImpute_median()", {
# 
#   imputed <- imputomics::impute_MetabImpute_median(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 2.5, 5),
#                               col2 = c(1, 2.5, 2.5, 4, 2.5)),
#                          class = "data.frame", row.names = c("1", "2", "3", "4", "5")))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed_w_negatives <- imputomics::impute_MetabImpute_median(example_df_w_negatives)
#   expect_equal(imputed_w_negatives,
#                structure(list(col1 = c(1, 2, 0, 1.5, 5),
#                               col2 = c(1, 0, 0, 0, 0)),
#                          class = "data.frame",
#                          row.names = c("1", "2", "3", "4", "5"))
#                )
# 
#   # expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#   #              sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_MetabImpute_zero()", {
# 
#   imputed <- imputomics::impute_MetabImpute_zero(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 0, 5),
#                               col2 = c(1, 0, 0, 4, 0)),
#                          class = "data.frame", row.names = c(NA, -5L)))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed_w_negatives <- imputomics::impute_MetabImpute_zero(example_df_w_negatives)
#   expect_equal(imputed_w_negatives,
#                structure(list(col1 = c(1, 2, 0, 0, 5),
#                               col2 = c(1, 0, 0, 0, 0)),
#                          class = "data.frame", row.names = c(NA, -5L)))
# })
# 
# test_that("test impute_bcv_svd()", {
# 
#   imputed <- imputomics::impute_bcv_svd(example_df)
#   expect_equal(imputed,
#                structure(list(X1 = c(1, 2, 3, 2.75, 5),
#                               X2 = c(1, 2.5, 2.5, 4, 2.5)),
#                          class = "data.frame", row.names = c(NA, -5L)))
# 
#   expect_equal(sum(example_df[!is.na(example_df)]),
#                sum(imputed[!is.na(example_df)]))
# 
#   imputed_w_negatives <- imputomics::impute_bcv_svd(example_df_w_negatives)
#   expect_equal(imputed_w_negatives,
#                structure(list(X1 = c(1, 2, -3, 1.25, 5),
#                               X2 = c(1, -1.5, -1.5, -4, -1.5)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L)))
# 
#   expect_equal(sum(example_df_w_negatives[!is.na(example_df_w_negatives)]),
#                sum(imputed_w_negatives[!is.na(example_df_w_negatives)]))
# })
# 
# test_that("test impute_CM()", {
# 
#   set.seed(42)
# 
#   imputed <- imputomics::impute_CM(example_df)
#   expect_equal(imputed,
#                structure(list(col1 = c(1, 2, 3, 1, 5),
#                               col2 = c(1, 1, 1, 4, 1)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L))
#                )
# 
#   imputed_w_negatives <- imputomics::impute_CM(example_df_w_negatives)
#   expect_equal(imputed_w_negatives,
#                structure(list(col1 = c(1, 2, -3, -3, 5),
#                               col2 = c(1, -4, -4, -4, -4)),
#                          class = "data.frame",
#                          row.names = c(NA, -5L)))
# 
# })
