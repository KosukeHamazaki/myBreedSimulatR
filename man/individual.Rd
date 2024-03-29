% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myIndividual.R
\name{individual}
\alias{individual}
\title{R6 class representing an individual}
\description{
individual object store specific information about one individual
}
\examples{

## ------------------------------------------------
## Method `individual$new`
## ------------------------------------------------

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
                     nLoci = c(3, 4, 5),
                     effPopSize = 3,
                     simInfo = mySimInfo,
                     verbose = TRUE)

### create lociInfo object
myLoci <- lociInfo$new(genoMap = NULL, specie = mySpec)


### create traitInfo object
myTrait <- traitInfo$new(lociInfo = myLoci,
                         nMarkers = c(2, 4, 3),
                         nTraits = 2,
                         nQTLs = matrix(c(1, 0, 2,
                                          1, 0, 1),
                                        nrow = 2,
                                        byrow = TRUE),
                         actionTypeEpiSimple = TRUE,
                         qtlOverlap = TRUE,
                         nOverlap = c(1, 0, 1),
                         effCor = 0.6,
                         propDomi = 0.2,
                         interactionMean = c(1, 0))
plot(myTrait, alphaMarker = 0.4)
### simulate haplotype
rawHaplo <- matrix(sample(c(0, 1), (3 + 4 + 5) * 2, replace = TRUE),
                   nrow = 2)
colnames(rawHaplo) <- paste0("Locus_", 1:(3 + 4 + 5))

haplo <- myBreedSimulatR::haplotype$new(lociInfo = myLoci,
                                        haplo = rawHaplo)

### create individuals:
myInd <- individual$new(name = "Ind 1",
                        specie = myTrait$lociInfo$specie,
                        traitInfo = myTrait,
                        parent1 = "OkaaSan", parent2 = "OtouSan",
                        haplo = myHaplo, verbose = TRUE)

## ------------------------------------------------
## Method `individual$generateGametes`
## ------------------------------------------------

myInd$generateGametes()
myInd$generateGametes(2)
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{[string] Name of the individual}

\item{\code{specie}}{[specie class] Specie of the SNPs
(see:\link[myBreedSimulatR]{specie})}

\item{\code{traitInfo}}{[traitInfo class] Specific information of traits
(see:\link[myBreedSimulatR]{traitInfo})}

\item{\code{parent1}}{[string] Name of the individual's parent}

\item{\code{parent2}}{[string] Name of the individual's parent}

\item{\code{haplo}}{[haplotype class] Haplotype of the individual (see:
\link[myBreedSimulatR]{haplotype})}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{trueAGVs}}{[numeric] true additive genotypic values}

\item{\code{trueAGVETs}}{[numeric] true additive genotypic values specific to each trait}

\item{\code{trueAGVCTs}}{[numeric] true additive genotypic values common across traits}

\item{\code{trueDGVs}}{[numeric] true dominant genotypic values}

\item{\code{trueDGVETs}}{[numeric] true dominance genotypic values specific to each trait}

\item{\code{trueDGVCTs}}{[numeric] true dominant genotypic values common across traits}

\item{\code{trueEGVs}}{[numeric] true epistatic genotypic values}

\item{\code{trueEGVETs}}{[numeric] true epistatic genotypic values between effects specific to each trait}

\item{\code{trueEGVCTs}}{[numeric] true epistatic genotypic values between effects common across each trait}

\item{\code{trueGVs}}{[numeric] true genotypic values}

\item{\code{trueGVETs}}{[numeric] true genotypic values specific to each trait}

\item{\code{trueGVCTs}}{[numeric] true genotypic values common across trait}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{individual$new()}}
\item \href{#method-generateGametes}{\code{individual$generateGametes()}}
\item \href{#method-clone}{\code{individual$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a new individual object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{individual$new(
  name = "Unnamed",
  specie = specie$new(),
  traitInfo = NULL,
  parent1 = NA,
  parent2 = NA,
  haplo = NULL,
  verbose = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{[string] name of the individual}

\item{\code{specie}}{[specie class] Specie of the SNPs
(see:\link[myBreedSimulatR]{specie})}

\item{\code{traitInfo}}{[traitInfo class] Specific information of traits
(see:\link[myBreedSimulatR]{traitInfo})}

\item{\code{parent1}}{[string] Name of the individual's parent}

\item{\code{parent2}}{[string] Name of the individual's parent}

\item{\code{haplo}}{[haplotype class] Haplotype of the individual (see:
\link[myBreedSimulatR]{haplotype})}

\item{\code{verbose}}{[boolean] display information}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new `individual` object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{### create simulation information
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
                     nLoci = c(3, 4, 5),
                     effPopSize = 3,
                     simInfo = mySimInfo,
                     verbose = TRUE)

### create lociInfo object
myLoci <- lociInfo$new(genoMap = NULL, specie = mySpec)


### create traitInfo object
myTrait <- traitInfo$new(lociInfo = myLoci,
                         nMarkers = c(2, 4, 3),
                         nTraits = 2,
                         nQTLs = matrix(c(1, 0, 2,
                                          1, 0, 1),
                                        nrow = 2,
                                        byrow = TRUE),
                         actionTypeEpiSimple = TRUE,
                         qtlOverlap = TRUE,
                         nOverlap = c(1, 0, 1),
                         effCor = 0.6,
                         propDomi = 0.2,
                         interactionMean = c(1, 0))
plot(myTrait, alphaMarker = 0.4)
### simulate haplotype
rawHaplo <- matrix(sample(c(0, 1), (3 + 4 + 5) * 2, replace = TRUE),
                   nrow = 2)
colnames(rawHaplo) <- paste0("Locus_", 1:(3 + 4 + 5))

haplo <- myBreedSimulatR::haplotype$new(lociInfo = myLoci,
                                        haplo = rawHaplo)

### create individuals:
myInd <- individual$new(name = "Ind 1",
                        specie = myTrait$lociInfo$specie,
                        traitInfo = myTrait,
                        parent1 = "OkaaSan", parent2 = "OtouSan",
                        haplo = myHaplo, verbose = TRUE)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-generateGametes"></a>}}
\if{latex}{\out{\hypertarget{method-generateGametes}{}}}
\subsection{Method \code{generateGametes()}}{
Generate Gametes
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{individual$generateGametes(n = 1)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n}}{[float] number of gametes to create (default: 1)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
list of gametes. A gamete is a named vectors with value 0 or 1.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{myInd$generateGametes()
myInd$generateGametes(2)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{individual$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
