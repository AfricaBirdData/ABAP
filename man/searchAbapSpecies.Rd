% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/searchAbapSpecies.R
\name{searchAbapSpecies}
\alias{searchAbapSpecies}
\title{Search species in ABAP data base}
\usage{
searchAbapSpecies(species)
}
\arguments{
\item{species}{A character string with any part of the common or taxonomic
name, or, the SAFRING code. Note that full names will
not work; choose a single word.}
}
\value{
A tibble with taxonomic information and codes of all species
matching the search. At the moment, if a species code is passed, the new API
is used. For convenience functionality for entering a common name is
maintained through the old API, but this might be discontinued.
}
\description{
Search species in ABAP data base
}
\examples{
searchAbapSpecies("Duck")
searchAbapSpecies(169)
searchAbapSpecies("Fulvous")
}
