% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_survey_bathymetry.R
\name{get_survey_bathymetry}
\alias{get_survey_bathymetry}
\title{Function to get survey bathymetry layers}
\usage{
get_survey_bathymetry(select.region, set.crs)
}
\arguments{
\item{select.region}{Region to select}

\item{set.crs}{CRS to use}
}
\value{
Returns an sf multiline object with survey contours.
}
\description{
Retrieves bathymetric contours that have been used in tech memos and reports through 2019 for the EBS.
}
