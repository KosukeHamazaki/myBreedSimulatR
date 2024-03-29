% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myPopulation.R
\name{createPop}
\alias{createPop}
\title{Create population object from genotype data.frame}
\usage{
createPop(
  geno = NULL,
  haplo = NULL,
  lociInfo,
  traitInfo = NULL,
  founderIsInitPop = FALSE,
  seedSimHaplo = NA,
  seedSimRM = NA,
  seedSimMC = NA,
  indNames = NULL,
  popName = NULL,
  verbose = TRUE
)
}
\arguments{
\item{geno}{[data.frame / matrix] genotype of the individuals encoded in allele dose.
If you use real data, you must specify `geno` or `haplo` argument.}

\item{haplo}{[array] haplotype of the individuals scored with 0 and 1 (3-dimensional array).}

\item{lociInfo}{[lociInfo object] information about the individuals haplotypes'
SNPs (see:\link[breedSimulatR]{lociInfo})}

\item{traitInfo}{[traitInfo class] Specific information of traits
(see:\link[myBreedSimulatR]{traitInfo})}

\item{founderIsInitPop}{[logical] Founder haplotype will be regarded as first population or not.}

\item{seedSimHaplo}{[numeric] Random seed for selecting haplotype from founder haplotype}

\item{seedSimRM}{[numeric] Random seed for mate pairs}

\item{seedSimMC}{[numeric] Random seed for make crosses}

\item{indNames}{[character] NULL or character string vector specifying the individuals
names. If NULL, \code{rownames(geno)} will be used.}

\item{popName}{[character] population's name.}

\item{verbose}{[logical] display information}
}
\value{
population object (see:\link[breedSimulatR]{population})
}
\description{
Create population object from genotype data.frame
}
\examples{
### create simulation information
mySimInfo <- simInfo$new(simName = "Simulation Example",
                         simGeno = TRUE,
                         simPheno = TRUE,
                         nSimGeno = 1,
                         nSimPheno = 3,
                         nCoreMax = 4,
                         nCorePerGeno = 1,
                         nCorePerPheno = 3,
                         saveDataFileName = NULL)
### create specie information
mySpec <- specie$new(nChr = 3,
                     lChr = c(100, 150, 200),
                     specName = "Example 1",
                     ploidy = 2,
                     mutRate = 10^-8,
                     recombRate = 10^-6,
                     chrNames = c("C1", "C2", "C3"),
                     nLoci = 100,
                     recombRateOneVal = FALSE,
                     effPopSize = 100,
                     simInfo = mySimInfo,
                     verbose = TRUE)
### create lociInfo object
myLoci <- lociInfo$new(genoMap = NULL, specie = mySpec)
plot(myLoci, alpha = 0.1)
### create traitInfo object
myTrait <- traitInfo$new(lociInfo = myLoci,
                         nMarkers = 80,
                         nTraits = 3,
                         nQTLs = c(4, 8, 3),
                         actionTypeEpiSimple = TRUE,
                         qtlOverlap = TRUE,
                         nOverlap = c(2, 3, 0),
                         effCor = 0.1,
                         propDomi = 0.2,
                         interactionMean = c(4, 1, 2))
plot(myTrait)
### create simulated population
simulatedPop <- createPop(geno = NULL,
                          haplo = NULL,
                          lociInfo = myLoci,
                          traitInfo = myTrait,
                          founderIsInitPop = TRUE,
                          popName = "First Population",
                          verbose = FALSE)
simulatedPop$plot(plotTarget = "trueGV",
                  plotType = "violin")
simulatedPop$plot(plotTarget = "trueGV",
                  plotType = "scatter",
                  scatterAxes = 1:3)

}
