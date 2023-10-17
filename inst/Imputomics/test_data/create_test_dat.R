
set.seed(10)

dat <- matrix(rnorm(1000, mean = 1000, sd = 20), ncol = 10)

# set with zeroes
example_set_0 <- dat
example_set_0[runif(1000) < 0.3] <- 0
colnames(example_set_0) <- paste0("variable_", letters[1:10])
example_set_0 <- cbind(example_set_0, i_missed_the_amputation = runif(100))
write.csv(round(example_set_0, 4), "./inst/Imputomics/test_data/i_have_zeros.csv", row.names = FALSE)

# set with ones
example_set_1 <- dat
example_set_1[runif(1000) < 0.3] <- 1
colnames(example_set_1) <- paste0("variable_", letters[1:10])
example_set_1 <- cbind(example_set_1, i_missed_the_amputation = runif(100))
write.csv(round(example_set_1, 4), "./inst/Imputomics/test_data/i_have_ones.csv", row.names = FALSE)

dat <- matrix(rnorm(2600, mean = 1000, sd = 20), ncol = 26)

#set with NA
example_set_na <- dat
example_set_na[runif(2600) < 0.2] <- NA
colnames(example_set_na) <- paste0("variable_", letters[1:26])
example_set_na <- cbind(example_set_na, i_missed_the_amputation = runif(100))
write.csv(example_set_na, "./inst/Imputomics/test_data/im_normal.csv", row.names = FALSE)

#set with character column
example_set_na <- dat
example_set_na[runif(2600) < 0.2] <- NA
colnames(example_set_na) <- paste0("variable_", letters[1:26])
example_set_na <- cbind(data.frame(example_set_na), categorical = c("a", "b"))
example_set_na <- cbind(example_set_na, i_missed_the_amputation = runif(100))
write.csv(example_set_na, "./inst/Imputomics/test_data/i_have_some_letters.csv", row.names = FALSE)


#set with only character columns

dat_char <- matrix(sample(letters, 1000, replace = TRUE), ncol = 10)

example_set_na <- as.data.frame(dat_char)
write.csv(example_set_na, "./inst/Imputomics/test_data/im_character.csv", row.names = FALSE)


# set without columns
write.csv(data.frame(), "./inst/Imputomics/test_data/im_empty.csv", row.names = FALSE)

# set small
example_set_na <- dat
example_set_na[runif(2600) < 0.2] <- NA
example_set_na <- example_set_na[, 1:3]
write.csv(example_set_na, "./inst/Imputomics/test_data/im_smol.csv", row.names = FALSE)

example_set_na <- dat
example_set_na[runif(2600) < 0.2] <- NA
example_set_na <- example_set_na[1:3, 1:3]
write.csv(example_set_na, "./inst/Imputomics/test_data/im_very_smol.csv",
          row.names = FALSE)


# set small and character
example_set_na <- dat
example_set_na[runif(2600) < 0.2] <- NA
example_set_na <- example_set_na[, 1:3]
example_set_na <- cbind(data.frame(example_set_na), categorical = c("a", "b"))
example_set_na <- cbind(data.frame(example_set_na), categorical = c("a", "b"))
example_set_na <- cbind(data.frame(example_set_na), categorical = c("a", "b"))
write.csv(example_set_na, "./inst/Imputomics/test_data/im_smol_and_character.csv", row.names = FALSE)






