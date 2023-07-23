
# imputomics

Imputomics is a software package and a web server designed to simulate
and impute missing values in omics datasets. It is a comprehensive
package that offers a range of methods for simulating and imputing
missing values in different types of omics data such as genomics,
transcriptomics, proteomics, and metabolomics. Imputomics provides a
user-friendly interface that allows users to simulate missing values
based on different distributions and impute missing values using
state-of-the-art methods.

## Key Features:

1.  Simulation of missing values: Imputomics provides a variety of
    options for simulating missing values, including missing completely
    at random (MCAR), missing at random (MAR), and missing not at random
    (MNAR) mechanisms. Users can specify the percentage of missing
    values and the distribution from which the missing values are
    generated.

2.  Imputation methods: Imputomics offers the biggest collection of
    imputation methods for different types of omics data, including
    k-nearest neighbors (KNN), random forests, expectation-maximization
    (EM) algorithm, and principal components analysis (PCA) and many
    others.

3.  Performance evaluation: Imputomics facilitates evaluating the
    performance of imputation methods. Users can evaluate imputation
    accuracy and compare different methods using metrics such as root
    mean squared error (RMSE), mean absolute error (MAE), and
    coefficient of determination (R-squared).

# Getting started

This repository contains the data and code necessary to reproduce the
results from the paper *Imputomics: comprehensive missing data
imputation for metabolomics data*. It uses
[renv](https://CRAN.R-project.org/package=renv) package to assure the
reproducibility. As *imputomics* implements lots of missing value
imputations methods from other R packages.

## Webserver

The *imputomics* can be accessed through our [web
server](http://imputomics.umb.edu.pl/).

## Installation

*imputomics* is available on
[GitHub](https://github.com/BioGenies/imputomics)

``` r
devtools::install_github("michbur/imputomics")
renv::restore()
```

## Run imputomics

To run *imputomics* type the following command into an R console.

``` r
imputomics::imputomics_gui()
```

<!-- # How to cite -->

## How to cite?

Jarosław Chilimoniuk, Krystyna Grzesiak, Jakub Kała, Dominik Nowakowski,
Małgorzata Bogdan, Michał Ciborowski, Adam Krętowski, Michał
Burdukiewicz (2023). Imputomics: comprehensive missing data imputation
for metabolomics data (submitted).

# Contact

If you have any questions, suggestions or comments, contact [Michal
Burdukiewicz](mailto:michalburdukiewicz@gmail.com).

## Funding and acknowledgements

We want to thank the Clinical Research Centre (Medical University of
Białystok) members for fruitful discussions. K.G. wants to acknowledge
grant no. 2021/43/O/ST6/02805 (National Science Centre). M.C.
acknowledges grant no. B.SUB.23.533 (Medical University of Białystok).
The study was supported by the Ministry of Education and Science funds
within the project ‘Excellence Initiative - Research University’. We
also acknowledge the Center for Artificial Intelligence at the Medical
University of Białystok (funded by the Ministry of Health of the
Republic of Poland).

<img src='https://raw.githubusercontent.com/BioGenies/imputomics/main/inst/umb_logo.jpg' style='width: 200px'>
