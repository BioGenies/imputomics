
set.seed(10)

dat <- matrix(rnorm(1000), ncol = 10)

example_set_0 <- dat
example_set_0[runif(1000) < 0.3] <- 0
colnames(example_set_0) <- letters[1:10]
example_set_0 <- cbind(example_set_0, i_missed_the_amputation = runif(100))
write.csv(round(example_set_0, 4), "example_set_0.csv", row.names = FALSE)

example_set_na <- dat
example_set_na[runif(1000) < 0.3] <- NA
colnames(example_set_na) <- letters[1:10]
example_set_na <- cbind(example_set_na, i_missed_the_amputation = runif(100))
write.csv(round(example_set_na, 4), "example_set_na.csv", row.names = FALSE)

