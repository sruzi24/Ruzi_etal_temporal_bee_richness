\name{Dataset1}
\alias{Dataset1}
\docType{data}
\title{
Dataset1
}
\description{
Dataset of unique specimen records as defined by unique combinations of scientific name, collection date, collector, and location collected at. These records may be associated with multiple specimens of the same species collected by the same person on the same day at the same location though they will be referred to as specimens within the variable descriptions below.
}
\usage{data("Dataset1")}
\format{
  A data frame with 6,080 observations on the following 10 variables.
  \describe{
  \item{\code{scientific_name}}{the scientific name of the specimen}
  \item{\code{family}}{the taxonomic family the specimen belongs to}
  \item{\code{day}}{the day the specimen was collected on}
  \item{\code{month}}{the month the specimen was collected on}
  \item{\code{year}}{the year the specimen was collected on}
  \item{\code{recorded_by}}{who collected the specimen}
  \item{\code{decimal_latitude}}{the latitude where the specimen was collected in decimal format}
  \item{\code{decimal_longitude}}{the longitude where the specimen was collected in decimal format}
  \item{\code{where_obtained_coordinates}}{whether the decimal coordinates were in the original dataset the record was from or was obtained by using google earth and searching for the collection location}
  \item{\code{n}}{the number of specimens that the unique combination of \code{scientific_name}, \code{day}, \code{month}, \code{year}, \code{recorded_by}, \code{decimal_latitude}, \code{decimal_longitude} corresonds to}
  }
}
\details{
Used for estimating species richness trends and when species were collected over time.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Dataset1)
str(Dataset1)
}
\keyword{datasets}
