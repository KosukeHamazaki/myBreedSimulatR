# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# Definition of haplotype class




#' R6 Class representing an haplotype
#'
#' @description
#' haplotype object store specific information of individuals haplotype
#'
#'
#' @export
#' @import R6
haplotype <- R6::R6Class(
  "haplotype",
  public = list(
    #' @field lociInfo [lociInfo class] information about the haplotype of the
    #'   SNPs (see:\link[myBreedSimulatR]{lociInfo})
    lociInfo = NULL,
    #' @field values [list] list haplotype values
    values = NULL,

    #' @description Create a new Haplotype object.
    #' @param lociInfo [lociInfo class] information about the haplotype's SNPs
    #'   (see:\link[myBreedSimulatR]{lociInfo})
    #' @param haplo [matrix] named matrix of the genotype for all markers
    #' @return A new `lociInfo` object.
    #' @examples
    #' ### create simulation information
    #' mySimInfo <- simInfo$new(simName = "Simulation Example",
    #'                          simGeno = TRUE,
    #'                          simPheno = TRUE,
    #'                          nSimGeno = 1,
    #'                          nSimPheno = 3,
    #'                          nCoreMax = 4,
    #'                          nCorePerGeno = 1,
    #'                          nCorePerPheno = 3,
    #'                          saveDataFileName = NULL)
    #'
    #' ### create specie information
    #' mySpec <- specie$new(nChr = 3,
    #'                      lChr = c(100, 150, 200),
    #'                      specName = "Example 1",
    #'                      ploidy = 2,
    #'                      mutRate = 10^-8,
    #'                      recombRate = 10^-6,
    #'                      recombRateOneVal = FALSE,
    #'                      chrNames = c("C1", "C2", "C3"),
    #'                      nLoci = c(3, 4, 5),
    #'                      effPopSize = 3,
    #'                      simInfo = mySimInfo,
    #'                      verbose = TRUE)
    #'
    #' ### create lociInfo object
    #' myLoci <- lociInfo$new(genoMap = NULL, specie = mySpec)
    #'
    #' ### simulate haplotype
    #' rawHaplo <- matrix(sample(c(0, 1), (3 + 4 + 5) * 3, replace = TRUE),
    #'                    nrow = 2)
    #' colnames(rawHaplo) <- paste0("Locus_", 1:(3 + 4 + 5))
    #'
    #' myHaplo <- myBreedSimulatR::haplotype$new(lociInfo = myLoci,
    #'                                           haplo = rawHaplo)
    #' myHaplo$values
    initialize = function(lociInfo, haplo) {

      # checks

      # lociInfo class
      if (class(lociInfo)[1] != "lociInfo") {
        stop(paste('class(lociInfo)[1] != "lociInfo"\n"lociInfo" must be a',
             'lociInfo object see: ?lociInfo'))
      }
      # haplo class
      if (!is(haplo,"matrix")) {
        stop('is(haplo,"matrix") is FALSE\n"haplo" must be a matrix')
      }
      # haplo ploidy
      if (nrow(haplo) != lociInfo$specie$ploidy) {
        stop(
          paste('nrow(haplo) != lociInfo$specie$ploidy\nnrow(haplo) must be',
             'equal to the specie ploidy'))
      }
      # number of markers
      if (ncol(haplo) != nrow(lociInfo$genoMap)) {
        stop(paste(
          'ncol(haplo) != nrow(lociInfo$genoMap)\nncol(haplo) must be equal to',
          'the number of markers in lociInfo'))
      }
      # markers names
      if (is.null(colnames(haplo))) {
        stop('colnames(haplo) is NULL\nhaplo must be a named matrix')
      }
      if (!all(colnames(haplo) %in% lociInfo$genoMap$lociNames)) {
        stop(paste(
          'all(colnames(haplo) %in% lociInfo$genoMap$lociNames)',
          'is false\ncolnames(haplo) must be the names of the markers'
        ))
      }

      self$lociInfo <- lociInfo

      # self$values <- list()
      # for (chr in lociInfo$specie$chrNames) {
      #   h <- haplo[, lociInfo$ids[[chr]]]
      #   mode(h) <- "integer"
      #   rownames(h) <- paste0("ploidy_", 1:lociInfo$specie$ploidy)
      #   self$values[[chr]] <- h
      # }

      self$values <- sapply (X = lociInfo$ids,
                        FUN = function(id) {
                          h <- haplo[, id]
                          mode(h) <- "integer"
                          rownames(h) <- .charSeq(paste0("Ploidy_"), seq(lociInfo$specie$ploidy))
                          return(h)
                        }, simplify = FALSE)

    }),

  active = list(
    #' @field allelDose [numeric] vector of haplotypes encoded in allele dose
    allelDose = function(){
      valuesBind <- do.call(cbind, self$values)
      allelDose <- Rfast::colsums(valuesBind)
      names(allelDose) <- colnames(valuesBind)

      return(allelDose)
    }

  )
)
