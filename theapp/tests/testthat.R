library(testthat)
library(assertthat)
library(lubridate)
library(purrr)
library(shiny)
library(shinyCleave)
library(stringr)
library(dplyr)
library(theapp)


#create("theapp")

test_check('theapp')


# #run this if you want to check the test coverage.
# devtools::install_github("r-lib/covr")
# library(covr)
# package_coverage('../theapp')
# zero_coverage(package_coverage("../theapp"))
#
