% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combineGenoData.R
\name{combineGenoData}
\alias{combineGenoData}
\title{combine genotype data}
\usage{
combineGenoData(allGenoFiles = NULL)
}
\arguments{
\item{allGenoFiles}{A vector of the genotype data files. Genotype data file name must have a numeric trial id. e.g. file: genotype_data_141.txt.}
}
\value{
a data.frame of the combined genotype data, with an additional column ('trial') indicating population origin of the individual.
}
\description{
combines genotype data from multiple populations.
}
