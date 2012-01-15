#library(methods)
#library(devtools)
library(testthat)

if (interactive()) {
  load_all("skel")
} else {
  library(locClass)  
}
test_dir("skel/inst/tests")

