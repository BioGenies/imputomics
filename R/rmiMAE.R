###############################################################################
###    Robust Missing imputation by minimizing two way mean absolute error  ###
###                               (RMIMAE) Function                         ###
###############################################################################
## URL: https://github.com/NishithPaul/missingImputation/blob/main/rmiMAE.R
## Author: @ NishithPaul


rmiMAE<- function (x,contRate=99){
  origdat<-x
  RegCoef <- function(x, a) {
    keep <- (a != 0) & (!is.na(x))
    a <- a[keep]
    return(matrixStats::weightedMedian(x[keep]/a, abs(a), na.rm = TRUE, interpolate = FALSE))
  }

  absReg <- function(x, a, b) {
    x <- as.vector(x)
    ab <- as.vector(outer(a, b))
    keep <- (ab != 0) & (!is.na(x))
    ab <- ab[keep]
    return(matrixStats::weightedMedian(x[keep]/ab, abs(ab), na.rm = TRUE, interpolate = FALSE))
  }
  metU <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
  samV <- matrix(NA, nrow = ncol(x), ncol = ncol(x))
  vard <- rep(NA, ncol(x))
  for (k in 1:ncol(x)) {
    ak <- apply(abs(x), 1, median, na.rm = TRUE)
    converged <- FALSE
    repe<-0
    while (!converged) {
      akprev <- ak
      c <- apply(x, 2, RegCoef, ak)
      bk <- c/sqrt(sum(c^2))

      d <- apply(x, 1, RegCoef, bk)
      ak <- d/sqrt(sum(d^2))
      if (k!=1){
        rk<-ak
        coef.proj <- c(crossprod(rk,metU[,1:(k-1)]))/diag(crossprod(metU[,1:(k-1)]))
        ak <- rk - matrix(metU[,1:(k-1)],nrow=nrow(x))%*%matrix(coef.proj,nrow=(k-1))
        #ak<-ak/sqrt(sum(ak^2))
        ck<-bk
        coef.proj <- c(crossprod(ck,samV[,1:(k-1)]))/diag(crossprod(samV[,1:(k-1)]))
        bk<- ck - matrix(samV[,1:(k-1)],nrow=ncol(x))%*%matrix(coef.proj,nrow=(k-1))
        #bk<-bk/sqrt(sum(bk^2))
      }
      repe<-repe+1
      if (sum((ak - akprev)^2) < 0.05)
        converged <- TRUE
      else if (repe > 4)
        converged <- TRUE
    }

    eigenk <- absReg(x, ak, bk)

    x <- x - eigenk * ak %*% t(bk)
    metU[, k] <- ak
    samV[, k] <- bk
    vard[k] <- eigenk
  }

  vard[is.na(vard)]<-0
  metU[is.na(metU)]<-0
  samV[is.na(samV)]<-0
  sm<-0
  tsum<-sum(vard*vard)
  expVar<-NULL
  ik<-0
  targetVar<-FALSE
  while(!targetVar){
    ik=ik+1
    sm=sm+vard[ik]*vard[ik]
    expVar[ik]=(sm/tsum)*100
    if (expVar[ik]>= contRate){
      targetVar <- TRUE
    }
  }
  if(ik<2){
    approX<-(vard[1:ik]*metU[,1:ik])%*%t(samV[,1:ik])
  }
  if(ik>1){
    approX<-metU[,1:ik]%*%diag(vard[1:ik])%*%t(samV[,1:ik])
  }

  q1<-NULL
  q3<-NULL
  iqR<-NULL
  for (k in 1:nrow(origdat)){
    q1[k]<-quantile(origdat[k,],na.rm=TRUE)[2][[1]]
    q3[k]<-quantile(origdat[k,],na.rm=TRUE)[4][[1]]
    iqR[k]<-quantile(origdat[k,],na.rm=TRUE)[4][[1]]-quantile(origdat[k,],na.rm=TRUE)[2][[1]]
  }

  recmat<-origdat
  # rmed<-rowMedians(origdat,na.rm=TRUE)
  # rvar<-RowVar(origdat)
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)) {if (is.na(recmat[i,j])|| recmat[i,j]> (q3[i]+iqR[i])|| recmat[i,j]< (q1[i]-iqR[i])) recmat[i,j]<-approX[i,j]
    }
  }
  imputeDat<-list()
  imputeDat$x<-recmat
  imputeDat$u<-metU
  imputeDat$v<-samV
  imputeDat$d<-vard
  return(imputeDat)
}


###############################################################
###                  THE END                                ###
###############################################################
