install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")
devtools::install_github("r-lib/devtools")


library(devtools)
has_devel()


.onLoad <- function(libname, pkgname){
  packageStartupMessage("Welcome to a package that lets you visualise intersecting sets")
}