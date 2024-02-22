
# imputomics <img src='man/figures/logo.png' align="right" height="150"/>

*imputomics* is an R package and a shiny web server designed to simulate
and impute missing values. It offers 42 algorithms for imputing missing
values, especially in different types of ‘-omics’ data such as genomics,
transcriptomics, proteomics, and metabolomics. imputomics provides a
user-friendly interface that allows users to simulate missing values
based on different distributions and impute missing values using
state-of-the-art methods.

## Key Features:

1.  Imputation methods: imputomics offers the biggest collection of
    imputation methods for different types of omics data, including
    k-nearest neighbors (KNN), random forests, expectation-maximization
    (EM) algorithm, and principal components analysis (PCA) and many
    others.

2.  Performance evaluation: imputomics facilitates evaluating the
    performance of imputation methods. Users can evaluate imputation
    accuracy and compare different methods using metrics such as root
    mean squared error (RMSE), mean absolute error (MAE), and
    coefficient of determination (R-squared).

3.  Simulation of missing values: imputomics provides a variety of
    options for simulating missing values, including missing completely
    at random (MCAR), missing at random (MAR), and missing not at random
    (MNAR) mechanisms. Users can specify the percentage of missing
    values and the distribution from which the missing values are
    generated.

# Getting started

This repository contains the data and code necessary to reproduce the
results from the paper *imputomics: comprehensive missing data
imputation for metabolomics data*. It uses
[renv](https://CRAN.R-project.org/package=renv) package to assure the
reproducibility. As *imputomics* implements lots of missing value
imputations methods from other R packages.

## Webserver

The *imputomics* can be accessed through our [web
server](https://biogenies.info/imputomics-ws/).

## Installation

*imputomics* is available on
[GitHub](https://github.com/BioGenies/imputomics)

To install *imputomics* you need to have *R* version \>= 4.2.0.

``` r
devtools::install_github("BioGenies/imputomics")
```

Sometimes, not all packages can be installed on the first try. In this
case, consider re-running the *install_github* function.

### Docker

To enhance the reproducibility of *imputomics*, we share it also as a
[rocker-based](https://github.com/rocker-org) container. The docker
manifest is available in the *imputomics* repository:
<https://github.com/BioGenies/imputomics/blob/main/Dockerfile_imputomics>.

## Reproducibility

To reproduce our environment you need to git clone our repo and activate
renv.

``` bash
git clone https://github.com/BioGenies/imputomics.git
```

``` r
renv::activate()
renv::restore()
```

### Troubleshooting

Q: I am receiving the following error message: “Error: HTTP error 403.
API rate limit exceeded for \[my IP\]”. A: Due to its comprehensiveness,
*imputomics* downloads many packages from GitHub, which may lead to
exceeding the limit of GitHub API queries. Please consider setting the
GitHub API token with usethis::create_github_token().

## Run imputomics

To run *imputomics* type the following command into an R console.

``` r
imputomics::imputomics_gui()
```

<!-- # How to cite -->

#### How to cite?

Jarosław Chilimoniuk, Krystyna Grzesiak, Jakub Kała, Dominik Nowakowski,
Adam Krętowski, Rafał Kolenda, Michał Ciborowski, Michał Burdukiewicz
(2023). imputomics: web server and R package for missing values
imputation in metabolomics data, *Bioinformatics*,
[10.1093/bioinformatics/btae098](https://doi.org/10.1093/bioinformatics/btae098).

#### Contact

If you have any questions, suggestions or comments, contact [Michal
Burdukiewicz](mailto:michalburdukiewicz@gmail.com).

#### Funding and acknowledgements

We want to thank the Clinical Research Centre (Medical University of
Białystok) members for fruitful discussions. K.G. wants to acknowledge
grant no. 2021/43/O/ST6/02805 (National Science Centre). M.C.
acknowledges grant no. B.SUB.23.533 (Medical University of Białystok).
The study was supported by the Ministry of Education and Science funds
within the project ‘Excellence Initiative - Research University’. We
also acknowledge the Center for Artificial Intelligence at the Medical
University of Białystok (funded by the Ministry of Health of the
Republic of Poland).

<img src='man/figures/funding.png' style='height: 90px'>
