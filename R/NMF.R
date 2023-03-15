################################################################################
#### This function was copied from https://github.com/freeoliver-jing/NMF
#### and contains the GSimp algorithm and related functions developed by
#### Jingjing Xu (https://doi.org/10.3390/molecules26195787).
################################################################################


###################################################################
########                                                  #########
########             NMF-based  imputation                #########
########                                                  #########
###################################################################


#' Supplementary function for mNMF
#'
#' @importFrom NMF nmf
#'
#' @param IMPdata  data set to be imputed, where rows = features,
#' columns = samples
#' @param k factor of matrix decomposition
#' @param Method  methods used in NMF package
#' @param type pre-imputation type (zero, mean, median)
#'
#' @keywords internal

nmf_method <- function(IMPdata, k, Method, type){

  V <- IMPdata
  V <- as.matrix(V)
  m <- dim(V)[1]
  n <- dim(V)[2]

  V1 <- IMPdata   # a copy of data
  V1 <- as.matrix(V1)

  # pre-imputing NAs by zero
  if(type == "zero"){
    zero_imputed <- V
    zero_imputed[is.na(zero_imputed)] <- 0
    V <- zero_imputed
  }

  #pre-imputing NAs by mean of row
  if(type == "mean"){
    mean_imputed <- V
    meanValue <- rowMeans(V,na.rm=TRUE)
    meanMat <-  matrix(rep(meanValue, times=n),nrow = m,ncol = n)
    mean_imputed[is.na(mean_imputed)] <- meanMat[is.na(mean_imputed)]
    V <- mean_imputed
  }

  #pre-impting NAs by median of row
  if(type == "median"){
    median_imputed <- V
    medianValue <- rowMedians(V,na.rm=TRUE)
    medianMat <-  matrix(rep(medianValue, times=n),nrow = m,ncol = n)
    median_imputed[is.na(median_imputed)] <- medianMat[is.na(median_imputed)]
    V <- median_imputed
  }

  #performing log transformation
  V <- log10(V)

  # performing nmf for a given k
  z1 <- nmf(V,k,method = Method)
  Basic <- z1@fit@W    # Basic matrix
  Coeff <- z1@fit@H    # Coefficent matrix
  V_recon <- Basic %*% Coeff

  # performing inverse log transformation
  V1[is.na(V1)] <- 10^V_recon[is.na(V1)]
  V_recon <- 10^V_recon

  result <- list()
  result$FIT  <- V_recon    # FIT = reconstructed matrix
  result$OUT <- V1        # OUT = estimated value on the position of NAs ֵ
  class(result) <- "Imputation"
  return(result)
}


################################################################################
########                                                                    ####
########         Weighted average of multiple reconstructed matrics         ####
########                                                                    ####
################################################################################

#' Supplementary function for mNMF
#'
#' @param IMP  data set to be imputed, where rows = features,
#' columns = samples
#' @param kgroup  the range of k value
#' @param M  methods used in NMF package
#' @param initialType pre-imputaion type
#'
#' @keywords internal

nmf_opt <- function(IMP,
                    M,
                    kgroup,
                    initialType){

  V <- IMP
  m <- dim(V)[1]
  n <- dim(V)[2]

  V1 <- IMP
  V1 <- as.matrix(V1)

  Q <- min(m,n)
  V2 <- matrix(NA,m,n)

  ## preserve the observed value in V2
  V2[which(!is.na(IMP))] <- IMP[which(!is.na(IMP))]

  estim <- matrix(0,(m*n),length(kgroup)+1)
  imputeData <- matrix(0,m,n)
  weig <- matrix(NA,1,length(kgroup))

  #calculating weight of each reconstructed matrix
  for (k in kgroup[1]:kgroup[length(kgroup)]) {

    z <- nmf_method(V1,k,M,initialType)

    for (i in 1:(m*n)) {
      if(!is.na(IMP[i])){
        # observed values in first column of estim
        estim[i,1] <- IMP[i]
        estim[i,(which(kgroup == k)+1)] <- z$FIT[i]
      }
    }

    estim <- estim[complete.cases(estim),]

    d <- sum(abs(estim[,(which(kgroup == k)+1)] - estim[,1]))/
      (sum(!is.na(IMP)) * max(IMP, na.rm = TRUE))

    weig[1,which(kgroup == k)] <- 1/exp(2*d)  # weight calculation

    imputeData <- imputeData+(z$OUT*weig[1,which(kgroup == k)])

  }

  imputeData <- imputeData/sum(weig)


  return(imputeData)
}

###############################################################
###                  THE END                                ###
###############################################################
