# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# Definition of selection functions



#' Selection according to breeding values
#'
#' @param pop (population object) Population of individuals to select
#'   (see:\link[breedSimulatR]{population})
#' @param n (integer) number of individuals to select
#' @param lociEffects (numeric vector) effect of the genetic markers
#'
#' @return character vector of the selected individuals' names
#' @export
#'
#' @examples
#' mySpec <- specie$new(specName = "Statisticae exempli",
#'                      nChr = 10,
#'                      lChr = 1e6,
#'                      ploidy = 2,
#'                      recombRate = 3/1e6)
#' myLoci <- lociInfo$new(genoMap = exampleData$genoMap, specie = mySpec)
#' initPop <- createPop(geno = exampleData$genotypes,
#'                      lociInfo = myLoci,
#'                      popName = "Initial population")
#' selectBV(pop = initPop,
#'          lociEffects = exampleData$lociEffects,
#'          n = 10)
selectBV <- function(pop, n, lociEffects){

  if (!all(colnames(lociEffects) == colnames(pop$genoMat))) {
    lociEffects <- lociEffects[colnames(pop$genoMat)]
  }

  BV <- as.numeric(pop$genoMat %*% lociEffects)
  names(pop$inds)[order(BV, decreasing = TRUE)][1:n]
}


#' Selection according to weighted breeding values
#'
#' @param pop (population object) Population of individuals to select
#'   (see:\link[breedSimulatR]{population})
#' @param n (integer) number of individuals to select
#' @param lociEffects (numeric vector) effect of the genetic markers
#'
#' @return character vector of the selected individuals' names
#' @references Jannink, Jean-Luc. “Dynamics of Long-Term Genomic Selection.”
#'   Genetics Selection Evolution 42, no. 1 (December 2010).
#'   https://doi.org/10.1186/1297-9686-42-35.
#' @export
#'
#' @examples
#' mySpec <- specie$new(specName = "Statisticae exempli",
#'                      nChr = 10,
#'                      lChr = 1e6,
#'                      ploidy = 2,
#'                      recombRate = 3/1e6)
#' myLoci <- lociInfo$new(genoMap = exampleData$genoMap, specie = mySpec)
#' initPop <- createPop(geno = exampleData$genotypes,
#'                      lociInfo = myLoci,
#'                      popName = "Initial population")
#' selectWBV(pop = initPop,
#'          lociEffects = exampleData$lociEffects,
#'          n = 10)
selectWBV <- function(pop, n, lociEffects){

  if (!all(colnames(lociEffects) == colnames(pop$genoMat))) {
    lociEffects <- lociEffects[colnames(pop$genoMat)]
  }

  favAllel <- as.numeric(lociEffects > 0)

  w <- pop$af
  w[favAllel == 0] <- 1 - w[favAllel == 0]
  w[w == 0] <- 1 # give weight 1 for fixed alleles
  w <- w ^ (-0.5)

  W_lociEffects <- lociEffects * w
  WBV <- as.numeric(pop$genoMat %*% W_lociEffects)
  names(pop$inds)[order(WBV, decreasing = TRUE)][1:n]
}
