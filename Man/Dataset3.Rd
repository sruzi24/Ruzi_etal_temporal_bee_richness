\name{Dataset3}
\alias{Dataset3}
\docType{data}
\title{
Dataset3
}
\description{
List of the different collection coordinates and whether those coordinates were obtained from the original dataset or from Google Earth.
}
\usage{data("Dataset3")}
\format{
  A data frame with 1,760 observations on the following 5 variables.
  \describe{
  \item{\code{scientific_name}}{the scientific name of the specimen}
  \item{\code{decimal_latitude}}{the latitude where the specimen was collected in decimal format}
  \item{\code{decimal_longitude}}{the longitude where the specimen was collected in decimal format}
  \item{\code{where_obtained_coordinates}}{whether the decimal coordinates were in the original dataset the record was from or was obtained by using google earth and searching for the collection location}
  \item{\code{total_num_specimens}}{the number of specimens that correspond to this combination of \code{scientific_name}, \code{decimal_latitude}, \code{decimal_longitude}, \code{where_obtained_coordinates}}
  }
}
\details{
List of collection coordinates and where coordinates were obatined from.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Dataset3)
str(Dataset3)
}
\keyword{datasets}
