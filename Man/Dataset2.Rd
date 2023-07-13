\name{Dataset2}
\alias{Dataset2}
\docType{data}
\title{
Dataset2
}
\description{
List of specimens utilized in the manuscript, where they are found, and where the data can be obtained online.
}
\usage{data("Dataset2")}
\format{
  A data frame with 11,227 observations on the following 8 variables.
  \describe{
  \item{\code{collection_code}}{the abbreviation of what collection the specimen belongs to}
  \item{\code{catalog_number}}{a unique number or combination of letters and numbers that refers to a specimen}
  \item{\code{data_source}}{where the specimen data was originally obtained from for this manuscript}
  \item{\code{location_data_pulled_from}}{the location where the data for the specimen originally came from for this manuscript}
  \item{\code{availability_notes}}{where data for this specimen can be found}
  \item{\code{collection_url}}{the url to where the data for specimens can be found if the specimen data was not originally collected from an online database such as BISON, GBIF, or SCAN}
  \item{\code{SCAN_catalogNumber}}{the catalogNumber for each specimen record in SCAN}
  \item{\code{date_data_pulled}}{the date when data was pulled from online databases such as BISON, GBIF, or SCAN if the specimen data originally came from an online database}
  }
}
\details{
Complete list of specimens used in analyses.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Dataset2)
str(Dataset2)
}
\keyword{datasets}
