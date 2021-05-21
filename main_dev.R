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
use_package("data.table")
use_package("rjson")
use_package("RCurl")


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


# Install -----------------------------------------------------------------

devtools::install()


