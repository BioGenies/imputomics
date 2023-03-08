context("Testing imputation functions")

test_that("impute_* functions do not work properly", {

  set.seed(2137)

  idf <- runif(100)
  idf[sample(1L:100, round(4, 0))] <- NA
  idf <- data.frame(matrix(idf, nrow = 10))
  na_row_no <- idf %>% is.na() %>% which()



    expect_equal(as.matrix(impute_zero(idf))[na_row_no][1], 0)
    expect_false(any(is.na(impute_zero(idf))))

    expect_equal(as.matrix(impute_random(idf))[na_row_no][1], 0.9235016, tolerance = 1e-3)
    expect_false(any(is.na(impute_random(idf))))

    expect_equal(as.matrix(impute_min(idf))[na_row_no][1], 0.1939139, tolerance = 1e-3)
    expect_false(any(is.na(impute_min(idf))))

    expect_equal(as.matrix(impute_mean(idf))[na_row_no][1], 0.5952122, tolerance = 1e-3)
    expect_false(any(is.na(impute_mean(idf))))

    expect_equal(as.matrix(impute_halfmin(idf))[na_row_no][1], 0.09695694, tolerance = 1e-3)
    expect_false(any(is.na(impute_halfmin(idf))))

    expect_equal(as.matrix(impute_median(idf))[na_row_no][1], 0.5952122, tolerance = 1e-3)
    expect_false(any(is.na(impute_median(idf))))

    # expect_equal(as.matrix(impute_svd(idf))[na_row_no][1], 0.6153299, tolerance = 1e-3)
    # expect_false(any(is.na(impute_svd(idf))))

    # expect_equal(as.matrix(impute_ppca(idf))[na_row_no][1], 0.623828, tolerance = 1e-2)
    # expect_false(any(is.na(impute_ppca(idf))))

    # expect_equal(as.matrix(impute_bpca(idf))[na_row_no][1], -9.139074e-26, tolerance = 1e-3)
    # expect_false(any(is.na(impute_bpca(idf))))


    # expect_equal(as.matrix(impute_nipals(idf))[na_row_no][1], 0.5029951, tolerance = 1e-3)
    # expect_false(any(is.na(impute_nipals(idf))))

    # expect_equal(as.matrix(impute_nlpca(idf))[na_row_no][1], -, tolerance = 1e-3) random
    # expect_false(any(is.na(impute_nlpca(idf))))

    # expect_equal(as.matrix(impute_nipals(idf))[na_row_no][1], 0.5029951, tolerance = 1e-3)
    # expect_false(any(is.na(impute_nipals(idf))))

    # expect_equal(as.matrix(impute_nipals(idf))[na_row_no][1], 0.5029951, tolerance = 1e-3)
    # expect_false(any(is.na(impute_nipals(idf))))

    # expect_equal(as.matrix(impute_mice(idf, method = "pmm"))[na_row_no][1], , tolerance = 1e-3) random
    # expect_false(any(is.na(impute_mice(idf, method = "pmm"))))

    # expect_equal(as.matrix(impute_mice(idf, method = "midastouch"))[na_row_no][1], , tolerance = 1e-3) random
    # expect_false(any(is.na(impute_mice(idf, method = "midastouch"))))

    # expect_equal(as.matrix(impute_mice(idf, method = "cart"))[na_row_no][1], , tolerance = 1e-3) random
    # expect_false(any(is.na(impute_mice(idf, method = "cart"))))

    # expect_equal(as.matrix(impute_mice(idf, method = "rf"))[na_row_no][1], , tolerance = 1e-3) random
    # expect_false(any(is.na(impute_mice(idf, method = "rf"))))

    # expect_equal(as.matrix(impute_mice(idf, method = "norm"))[na_row_no][1], , tolerance = 1e-3) random
    # expect_false(any(is.na(impute_mice(idf, method = "norm"))))

    # expect_equal(as.matrix(impute_mice(idf, method = "norm.nob"))[na_row_no][1], , tolerance = 1e-3) random
    # expect_false(any(is.na(impute_mice(idf, method = "norm.nob"))))

    # expect_equal(as.matrix(impute_mice(idf, method = "norm.boot"))[na_row_no][1], , tolerance = 1e-3) NA
    # expect_false(any(is.na(impute_mice(idf, method = "norm.boot"))))

    # expect_equal(as.matrix(impute_mice(idf, method = "norm.predict"))[na_row_no][1], , tolerance = 1e-3) random
    # expect_false(any(is.na(impute_mice(idf, method = "norm.predict"))))

    # expect_equal(as.matrix(impute_mice(idf, method = "lasso.norm"))[na_row_no][1], , tolerance = 1e-3) random
    # expect_false(any(is.na(impute_mice(idf, method = "lasso.norm"))))

    # expect_equal(as.matrix(impute_mice(idf, method = "lasso.select.norm"))[na_row_no][1], , tolerance = 1e-3) random
    # expect_false(any(is.na(impute_mice(idf, method = "lasso.select.norm"))))

    # expect_equal(as.matrix(impute_amelia(idf))[na_row_no][1], , tolerance = 1e-3) error
    expect_false(any(is.na(impute_amelia(idf))))

    # expect_equal(as.matrix(impute_missforest(idf))[na_row_no][1], , tolerance = 1e-3) random
    expect_false(any(is.na(impute_missforest(idf))))

    # expect_equal(as.matrix(impute_mi(idf))[na_row_no][1], , tolerance = 1e-3) error
    # expect_false(any(is.na(impute_mi(idf))))  error

    # expect_equal(as.matrix(impute_areg(idf))[na_row_no][1], , tolerance = 1e-3) error
    # expect_false(any(is.na(impute_areg(idf)))) error

    expect_equal(as.matrix(impute_knn(idf))[na_row_no][1], 0.5678735, tolerance = 1e-3)
    expect_false(any(is.na(impute_knn(idf))))

    # expect_equal(as.matrix(impute_qrilc(idf))[na_row_no][1], , tolerance = 1e-3) random
    expect_false(any(is.na(impute_qrilc(idf))))

    # expect_equal(as.matrix(impute_mle(idf))[na_row_no][1], , tolerance = 1e-3)random
    expect_false(any(is.na(impute_mle(idf))))

    # expect_equal(as.matrix(impute_twlsa(idf))[na_row_no][1], , tolerance = 1e-3) random
    # expect_false(any(is.na(impute_twlsa(idf))))

    # expect_equal(as.matrix(impute_softimpute(idf))[na_row_no][1], , tolerance = 1e-3) random
    expect_false(any(is.na(impute_softimpute(idf))))

    # expect_equal(as.matrix(impute_irmi(idf))[na_row_no][1], , tolerance = 1e-3) random
    # expect_false(any(is.na(impute_irmi(idf))))

    # expect_equal(as.matrix(impute_PEMM(idf))[na_row_no][1], , tolerance = 1e-3) error
    # expect_false(any(is.na(impute_PEMM(idf)))) error

    expect_equal(as.matrix(impute_tknn(idf))[na_row_no][1], 0.5376948, tolerance = 1e-3)
    expect_false(any(is.na(impute_tknn(idf))))

    # expect_equal(as.matrix(impute_gsimp(idf))[na_row_no][1], , tolerance = 1e-3) random
    expect_false(any(is.na(impute_gsimp(idf))))


})
