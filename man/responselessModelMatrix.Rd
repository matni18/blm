% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/supportMethods.R
\name{responselessModelMatrix}
\alias{responselessModelMatrix}
\title{Responseless Model Matrix}
\usage{
responselessModelMatrix(model, dataframe)
}
\arguments{
\item{model}{A formula describing the model.}

\item{dataframe}{A dataframe with the data}
}
\value{
A model matrix without a response variable.
}
\description{
Builds a model matrix from data, excluding the response variable.
Support function for update() to build the phiX matrix.
}

