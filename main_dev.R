# 21-5-2021

# Script to keep track of package development

# DO NOT RUN EVERYTHING AGAIN

# If you want to add a function copy and paste a function block
# at the end of the script (before the install part) and run
# that section that you just pasted.

rm(list = ls())

library(devtools)
library(testthat)


# Create a package structure
create_package("D:/Documentos/mis_trabajos/Academic/BIRDIE/SABAP")

# Add license
use_mit_license("BIRDIE Development Team")

# Remember to edit the DESCRIPTION file



# Imports -----------------------------------------------------------------

# Import pipe
use_pipe()

# Import packages
use_package("dplyr")
use_package("magrittr")
use_package("lubridate")
use_package("stringr")
use_package("readr")
use_package("RCurl")
use_package("rjson")
use_package("data.table")


# Function listCwacSites --------------------------------------------------

# Add function
use_r("listCwacSites")

# test locally
load_all()

listCwacSites("Eastern Cape")

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function getCwacSiteInfo --------------------------------------------------

# Add function
use_r("getCwacSiteInfo")

# test locally
load_all()

getCwacSiteInfo(23312919)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function listCwacCards --------------------------------------------------

# Add function
use_r("listCwacCards")

# test locally
load_all()

listCwacCards(32481810)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()



# Function getCwacSurvey --------------------------------------------------

# Add function
use_r("getCwacSurvey")

# test locally
load_all()

getCwacSurvey(508082)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function jsonToTibble --------------------------------------------------

# Add function
use_r("jsonToTibble")

# test locally
load_all()


# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function getDicctionary --------------------------------------------------

# Add function
use_r("getCwacDictionary")

# test locally
load_all()

getCwacDictionary()

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function searchCwacTerm --------------------------------------------------

# Add function
use_r("searchCwacTerm")

# test locally
load_all()

searchCwacTerm("Season")
searchCwacTerm(option = "fields")
# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()

# Install -----------------------------------------------------------------

devtools::install()


