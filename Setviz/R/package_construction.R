install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")
devtools::install_github("r-lib/devtools")


library(devtools)
has_devel()
print(has_devel())

.onLoad <- function(libname, pkgname){
  packageStartupMessage("Welcome to a package that lets you visualise intersecting sets")
}

print(rtools_path() )
has_rtools()
build()
check()
test()
Sys.setenv(PATH = paste("c:\\Rtools\\bin\\", Sys.getenv("PATH"), sep=";"))
Sys.setenv(BINPREF = "C:\\Rtools\\mingw_$(WIN\\bin\\")


