
################################################################################
## Creating B datasets of size N (samples) by p (Metabolites)
################################################################################


################################################################################
###### Create MissingValues Datasets.
################################################################################


################################################################
## This is used for MCMC sampling with cov matrix from factor model
################################################################


MCMC.Factor <- function(data, M, miss.pattern, K.max) {
  Samp <- nrow(data)
  Metab <- ncol(data)


  ##Initialize Data with missing values replaced with zeros
  Data.init <- data
  Data.init[is.na(Data.init)] <- 0

  ##Find the Truncation Point i.e the Minimum Value
  min.data <- min(data, na.rm = TRUE)

  p.alpha <- runif(1, min=0, max = 1)

  p.beta <- array(NA, dim = c(2*Metab, 1))

  p.beta[1:Metab] <-(colMeans(Data.init,na.rm = TRUE))
  for( i in (Metab+1):(2*Metab)){
    p.beta[i] <- rnorm(1,mean =0, sd=1)
  }

  sample.x <- array(NA, dim=c(Metab, Metab*2, Samp))
  for(i in 1:(Samp/2)) {
    sample.x[1:Metab,1:Metab,i] <- diag(1,Metab)
    sample.x[1:Metab,(Metab+1):(2*Metab),i]  <- diag(0,Metab)
  }
  for(i in ((Samp/2)+1):Samp) {
    sample.x[1:Metab,1:Metab,i] <- diag(1,Metab)
    sample.x[1:Metab,(Metab+1):(2*Metab),i] <- diag(1,Metab)
  }

  sample.mean <- array(NA , dim=c(Samp,Metab))

  for(i in 1:Samp) {
    sample.mean[i,] <- sample.x[,,i] %*% (p.beta)  #Xi x Beta  ## 181 by 10
  }

  Omega <- diag(100^2, 2*Metab)  # 362 by 362

  factor.eta <- array( rnorm(K.max*Samp), c(K.max, Samp))
  factor.lambda <- array( rnorm(Metab*K.max)*.1, c(Metab, K.max))
  factor.sigma <- rep(.1,Metab)
  Sig.inv <- diag(1/factor.sigma)
  factor.nu <- 3
  factor.phi <- array(rgamma(K.max*Metab, factor.nu/2, factor.nu/2),
                       c(Metab,K.max))
  factor.a <- c(2,2)
  factor.delta <- rep(2, K.max)
  factor.tau <- cumprod(factor.delta)
  SigmaStar <- factor.lambda %*% t(factor.lambda) + diag(factor.sigma)
  MH.a <- c(1.2,.5)
  MH.counter <- c(0,0)

  ind <- which(miss.pattern == 0, arr.ind = TRUE)

  for(m in 1:M) {  ## Begin the M iterations

    ##Conditional Parameters for p.alpha
    ##p.alpha <- rbeta(n, a.shape1, a.shape2)
    ##
    a.shape1 <- sum((1- miss.pattern)*(Data.init > min.data))+ 1
    a.shape2 <- sum((miss.pattern)*(Data.init > min.data))+ 1
    p.alpha <- rbeta(1, a.shape1, a.shape2)


    ##Conditional Parameters for p.beta
    ##p.beta <- rmvnorm(n, mean, sigma)
    ##
    temp <- array(NA, dim =c(2*Metab,2*Metab,Samp))
    temp2 <- array(NA, dim = c(2*Metab, 1, Samp))

    SigmaStar <- factor.lambda %*% t(factor.lambda) + diag(factor.sigma)
    SigInv <- solve(SigmaStar)

    for(i in 1:Samp) {
      temp[,,i] <- t(sample.x[,,i]) %*% SigInv %*% sample.x[,,i]
      temp2[,,i] <- t(sample.x[,,i]) %*% SigInv %*% (as.matrix(Data.init[i,]))
    }
    tempsum <- apply(temp, MARGIN=c(1, 2), sum)
    tempsum2 <- apply(temp2, MARGIN=c(1, 2), sum)

    b.mean <- solve(tempsum + solve(Omega)) %*% (tempsum2)
    b.sigma <- solve(tempsum + solve(Omega))

    p.beta <- SimDesign::rmvnorm(1, b.mean, b.sigma)

    for(i in 1:Samp) {
      sample.mean[i,] <- sample.x[,,i] %*% t(p.beta)  #Xi x Beta  ## 181 by 1
    }

    ###############
    # Sample covariance matrix
    ###############

    resid <- Data.init - sample.mean

    eta.var <- solve(diag(K.max)+t(factor.lambda)%*% Sig.inv %*% factor.lambda)
    for( i in 1:Samp ){
      eta.mean <- eta.var %*% t(factor.lambda) %*% Sig.inv %*% resid[i,]
      factor.eta[,i] <- SimDesign::rmvnorm(1, eta.mean, eta.var)
    }

    eta.eta <- factor.eta %*% t(factor.eta)
    for( j in 1:Metab ){
      lam.var <- solve(diag(factor.phi[j,]*factor.tau)+ eta.eta/factor.sigma[j])
      lam.mean <- lam.var %*% factor.eta %*% resid[,j] / factor.sigma[j]
      factor.lambda[j,] <- SimDesign::rmvnorm(1, lam.mean, lam.var)
    }

    factor.cont <- t(factor.lambda %*% factor.eta)
    factor.sigma <- 1/rgamma(Metab,
                             1 + Samp/2,
                             0.3 + .5*apply((resid-factor.cont)^2,2,sum))
    Sig.inv <- diag(1/factor.sigma)

    for( h in 1:K.max ){
      factor.phi[,h] <- rgamma(
        Metab,
        .5*(1+factor.nu),
        .5*(factor.nu + factor.tau[h]*factor.lambda[,h]^2)
      )
    }

    factor.delta[1] <- rgamma(
      1,
      factor.a[1] + Metab*K.max/2,
      1+.5*sum((factor.tau/factor.delta[1])*apply(factor.phi*factor.lambda^2,
                                                  2,
                                                  sum))
    )

    for( h in 2:K.max ){
      factor.delta[h] <- rgamma(
        1,
        factor.a[2] + Metab*(K.max-h+1)/2,
        1+.5*sum(((factor.tau/factor.delta[h])*apply(factor.phi*factor.lambda^2,
                                                     2,
                                                     sum))[h:K.max])
      )
    }

    factor.tau <- cumprod(factor.delta)

    a.cand <- rlnorm( 1, log(factor.a[1]), MH.a[1] )
    MH.prob <- dgamma(a.cand,2,1,log=T)-dgamma(factor.a[1],2,1,log=T) +
      dgamma(factor.delta[1],a.cand,1,log=T) -
      dgamma(factor.delta[1],factor.a[1],1,log=T) +
      dlnorm( factor.a[1], log(a.cand), MH.a[1], log=T) -
      dlnorm( a.cand, log(factor.a[1]), MH.a[1], log=T)
    if( runif(1) < exp(MH.prob) ){
      factor.a[1] <- a.cand
      MH.counter[1] <- MH.counter[1] + 1
    }

    a.cand <- rlnorm( 1, log(factor.a[2]), MH.a[2] )
    MH.prob <- dgamma(a.cand,2,1,log=T)-dgamma(factor.a[2],2,1,log=T) +
      sum(dgamma(factor.delta[-1],a.cand,1,log=T) -
            dgamma(factor.delta[-1],factor.a[2],1,log=T)) +
      dlnorm(factor.a[2], log(a.cand), MH.a[2], log=T) -
      dlnorm( a.cand, log(factor.a[2]), MH.a[2], log=T)

    if( runif(1) < exp(MH.prob) ){
      factor.a[2] <- a.cand
      MH.counter[2] <- MH.counter[2] + 1
    }


    ####################################################################
    ##P(Yij | Rij= 0, ...)                ##############################
    ##                                    ##############################
    ####################################################################


    for( i in 1:dim(ind)[[1]] ) {

      MuCalc <- sample.mean[ind[i,][[1]],ind[i,][[2]]] +
        factor.cont[ind[i,][[1]],ind[i,][[2]]]
      VarCalc <- factor.sigma[ ind[i,][[2]] ]

      lowprob <-  pnorm(min.data,
                        mean = MuCalc,
                        sd = sqrt(VarCalc),
                        lower.tail = T)
      highprob <- pnorm(min.data,
                        mean = MuCalc,
                        sd = sqrt(VarCalc),
                        lower.tail = F)

      prob <- (lowprob) / (lowprob + p.alpha*highprob)

      zij <- rbinom(1,1, prob)

      if(zij == 0){
        Data.init[ind[i,][[1]], ind[i,][[2]]] <-
          truncnorm::rtruncnorm(1, a=min.data, b=Inf,
                                mean = MuCalc, sd = sqrt(VarCalc))
      } else {
        Data.init[ind[i,][[1]], ind[i,][[2]]] <-
          truncnorm::rtruncnorm(1, a=-Inf, b=min.data,
                                mean = MuCalc, sd = sqrt(VarCalc))
      }
    }

  } # End of M Iterations

  stor.alpha <- p.alpha
  stor.beta <- p.beta
  DATA.STOR <- Data.init
  stor.sigma <- factor.sigma
  stor.lambda <- factor.lambda

  MH.counter <- MH.counter/M
  return(list(stor.alpha, stor.sigma, stor.beta,
              stor.lambda, DATA.STOR, MH.counter))
}
