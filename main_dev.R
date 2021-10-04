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

# Create an Rmarkdown README
usethis::use_readme_rmd()

# Remember to run this line when README is modified
use_readme_rmd()

# Imports -----------------------------------------------------------------

# Import pipe
use_pipe()

# Import packages
use_package("dplyr")
use_package("data.table")
use_package("rjson")
use_package("RCurl")
use_package("readr")


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


# Function searchSabapSpecies --------------------------------------------------

# Add function
use_r("searchSabapSpecies")

# test locally
load_all()

searchSabapSpecies("Duck")

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function getSabapData --------------------------------------------------

# Add function
use_r("getSabapData")

# test locally
load_all()

getSabapData(123, region_type = "pentad", region = "2505_2850")

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function cleanSabapData --------------------------------------------------

# BETA

# Add function
use_r("cleanSabapData")

# test locally
load_all()

getSabapData(123, region_type = "pentad", region = "2505_2850") %>%
  cleanSabapData()

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Data pentads_sabap2 -------------------------------------------------------------

# Prepare pentads
# source("data_prep/barberspan_prep.R")

# Create an ROxygen2 file and document
document()

# Function getRegionPentads --------------------------------------------------

# Add function
use_r("getRegionPentads")

# test locally
load_all()

getRegionPentads(country = "South Africa", province = "North West")
getRegionPentads(country = "South Africa", province = "North West", path = "analysis/data")

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


