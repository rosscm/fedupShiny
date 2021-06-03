
# fedupShiny

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`{fedupShiny}` is an R Shiny implementation of the Bioconductor
`{fedup}` package developed with the `{golem}` framework. Source code
can be found [here](https://github.com/rosscm/fedup).

Install and run `{fedupShiny}` in R as follows:

``` r
# install.packages("devtools")
devtools::install_github("rosscm/fedupShiny")
fedupShiny::run_app()
```

To deploy `{fedupShiny}` via Docker, first clone this repo locally:

    git clone https://github.com/rosscm/fedupShiny.git

Build the image by running this command in the app’s home directory
(where the Dockerfile is stored):

    docker build -t rosscm-fedupshiny .

And run the container:

    docker run -d -p 80:80 rosscm-fedupshiny
