  <!-- badges: start -->
[![R-CMD-check](https://github.com/michbur/imputomics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/michbur/imputomics/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# imputomics

Imputomics is a software package  and a web server designed to simulate and impute 
missing values in omics datasets. It is a comprehensive package that offers a range 
of methods for simulating and imputing missing values in different types of omics 
data such as genomics, transcriptomics, proteomics, and metabolomics. Imputomics 
provides a user-friendly interface that allows users to simulate missing values 
based on different distributions and impute missing values using state-of-the-art 
methods. 

## Key Features:

1. Simulation of missing values: Imputomics provides a variety of options for simulating 
missing values, including missing completely at random (MCAR), missing at random (MAR), 
and missing not at random (MNAR) mechanisms. Users can specify the percentage of 
missing values and the distribution from which the missing values are generated.

2. Imputation methods: Imputomics offers the biggest collection of imputation 
methods for different types of omics data, including k-nearest neighbors (KNN), 
random forests, expectation-maximization (EM) algorithm, and principal components
analysis (PCA) and many others. 

3. Performance evaluation: Imputomics facilitates evaluating the performance of 
imputation methods. Users can evaluate imputation accuracy and compare different 
methods using metrics such as root mean squared error (RMSE), mean absolute 
error (MAE), and coefficient of determination (R-squared).




The functions of imputomics can also be accessed through our [web server](http://) 
or via the R console for customized analysis pipelines.

## Installation

*imputomics* is available on [CRAN](https://cran.r-project.org/package=imputomics). 

```R
install.packages("imputomics")
```

However, you can also install the developmental version of imputomics directly from GitHub

```R
devtools::install_github("michbur/imputomics")
```

## Run imputomics

To run imputomics type the following command into an R console.

```R
imputomics::imputomics_gui()
```

## How to cite



```tex
@Article{,
  author = "",
  title = "",
  year = , 
  issn = {},
  journal = "",
  url = "",
}

@article{,
	title = {},
	volume = {},
	issn = {},
	shorttitle = {{imputomics}},
	url = {},
	doi = {},
	abstract = {},
	language = {en},
	number = {},
	urldate = },
	journal = {},
	author = {},
	month = ,
	year = {},
	note = {}
}
```
