
### Citation

T. Dimitriadis, L. Dümbgen, A. Henzi, M. Puke, and J. Ziegel. Honest
calibration assessment for binary outcome predictions. *Preprint*,
[*arXiv:2203.04065*](https://arxiv.org/abs/2203.04065), 2022.

### Associated repository

The package `calibrationband` is available on [The Comprehensive R
Archive Network
(CRAN)](https://cran.r-project.org/web/packages/calibrationband/index.html)
and via [GitHub](https://github.com/marius-cp/calibrationband).

``` r
# CRAN version
install.packages("calibrationband")

# development version: 
#install.packages("devtools")
devtools::install_github("marius-cp/calibrationband")
library(calibrationband)
```

### Simulations

All graphs in chapter 5 can be replicated based on the code in the
folder `simulation`. The simulation results can be obtained by running
the script `simulation.R`. The simulation output necessary to replicate
the results in Figures 3 and 4 is provided in `Fig_3.RDS` and
`Fig_4.RDS`.

### Application

The code in the respective folder allows to replicate Figure 1 and 5.
The data preparation is carried out in `data_preperation.R` and requires
that the following data set has been downloaded in advance
(<https://data.nber.org/natality/2017/natl2017.dta.zip>). The user
manual is to be found here
<https://data.nber.org/natality/2017/natl2017.pdf>. A cleaned version of
the data (randomly split into a test and training set) is provided in
`bw_train.RDS` and `bw_test.RDS`.

<p align="middle">
<img src="application/Fig_readme.png" width="525" />
</p>

All plots were created using the `calibrationband` library.

### Supplement

The folder `supplement` holds the replication files for the supplement
material of our paper.
