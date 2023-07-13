\name{Bee_trait_file}
\alias{Bee_trait_file, bee_trait_file}
\docType{data}
\title{
Bee_trait_file
}
\description{
List of species, their traits, and the sources for each of the traits.
}
\usage{data("Bee_trait_file")}
\format{
  A data frame with 328 observations on the following 20 variables.
  \describe{
  \item{\code{Scientific Name}}{the scientific name of the specimen}
  \item{\code{Family}}{the taxonomic family the species belongs to}
  \item{\code{Native Status}}{whether the species is native or not to North Carolina}
  \item{\code{Native Notes}}{notes about \code{Native Status}}
  \item{\code{Native Source}}{sources for where the information in \code{Native Status} came from}
  \item{\code{Nesting level}}{whether the species is known to nest above or belowground}
  \item{\code{Nesting Rent or Build}}{whether the species is known to rent or build their nests}
  \item{\code{Nest Substrate}}{what the species is known to make their nests out of}
  \item{\code{nesting Notes}}{notes about nesting information}
  \item{\code{Nesting Sources}}{sources for where the information about nesting comes from}
  \item{\code{Diet Categroy}}{whether the species is known to be a generalist or a speciailise}
  \item{\code{Diet Oligo Host}}{if the species is a specialist what hosts it is known to feed on}
  \item{\code{Diet Notes}}{notes on diet}
  \item{\code{Diet Source}}{sources for where the information about diet comes from}
  \item{\code{Social Category}}{what social category the species belongs to}
  \item{\code{Social Notes}}{notes about the \code{Social Category}}
  \item{\code{Social Sources}}{sources for where the information about \code{Social Category} came from}
  \item{\code{Parasite Host}}{if the species is parasitic what hosts they are known to parasitize}
  \item{\code{Parasite Host Notes}}{notes on \code{Parasite Host}}
  \item{\code{Parasite Host Sources}}{sources for where the information about \code{Parasite Host} comes from}
  }
}
\details{
Used for species trati categroizations and creating the heatmaps.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Bee_trait_file)
str(Bee_trait_file)
}
\keyword{datasets}
