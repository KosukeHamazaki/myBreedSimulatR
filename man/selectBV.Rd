% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mySelection.R
\name{selectBV}
\alias{selectBV}
\title{Selection according to breeding values}
\usage{
selectBV(pop, n, lociEffects)
}
\arguments{
\item{pop}{(population object) Population of individuals to select
(see:\link[breedSimulatR]{population})}

\item{n}{(integer) number of individuals to select}

\item{lociEffects}{(numeric vector) effect of the genetic markers}
}
\value{
character vector of the selected individuals' names
}
\description{
Selection according to breeding values
}
\examples{
mySpec <- specie$new(specName = "Statisticae exempli",
                     nChr = 10,
                     lChr = 1e6,
                     ploidy = 2,
                     recombRate = 3/1e6)
myLoci <- lociInfo$new(genoMap = exampleData$genoMap, specie = mySpec)
initPop <- createPop(geno = exampleData$genotypes,
                     lociInfo = myLoci,
                     popName = "Initial population")
selectBV(pop = initPop,
         lociEffects = exampleData$lociEffects,
         n = 10)
}
