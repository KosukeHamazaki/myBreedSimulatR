# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# Definition of specie class




#' R6 Class Representing a Specie
#'
#' @description
#' Specie object store specific information of one specie.
#'
# @details
# Details: Specie object store specific information of one specie.
#'
#' @export
#' @import R6
specie <- R6::R6Class(
  "Specie",
  public = list(
    #' @field nChr [numeric] Number of chromosomes
    nChr = NA,
    #' @field lChr [numeric] Length of all chromosomes
    lChr = NA,
    #' @field specName [str] Specie's name
    specName = "Undefinded",
    #' @field ploidy [numeric] Number of possible alleles at one locus
    ploidy = NA,
    #' @field mutRate [numeric] Mutation rate at each base
    mutRate = NA,
    #' @field recombRate [numeric] Recombination rate at each base
    recombRate = NA,
    #' @field chrNames [str] Names of the chromosomes
    chrNames = NA,
    #' @field nLoci [numeric] Number of loci (including both markers (SNPs) & QTLs)
    nLoci = NA,
    #' @field effPopSize [numeric] Effective population size
    effPopSize = NA,
    #' @field recombRateOneVal [logical] Assume same recombination rate across genome
    recombRateOneVal = NA,
    #' @field simInfo [simInfo class] Simulation information
    #'   (see:\link[myBreedSimulatR]{simInfo})
    simInfo = NULL,

    #' @description Create a new specie object.
    #' @param nChr [numeric] Number of chromosomes
    #' @param lChr [numeric] length of all chromosomes
    #' @param specName [str] Specie's name (optional)
    #' @param ploidy [numeric] Number of possible alleles at one locus
    #'   (optional)
    #' @param mutRate [numeric] Mutation rate at each base (optional)
    #' @param recombRate [numeric] Recombination rate at each base (optional)
    #' @param chrNames [str] Names of the chromosomes (optional)
    #' @param nLoci [numeric] Number of loci (including both markers (SNPs) & QTLs)
    #' @param effPopSize [numeric] Effective population size
    #' @param recombRateOneVal [logical] Assume same recombination rate across genome or not
    #' @param simInfo [simInfo class] Simulation information (see:\link[myBreedSimulatR]{simInfo})
    #' @param verbose [logical] Display info (optional)
    #' @return A new `specie` object.
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
    #'                      recombRate = 10^-7,
    #'                      recombRateOneVal = FALSE,
    #'                      chrNames = c("C1", "C2", "C3"),
    #'                      nLoci = 100,
    #'                      effPopSize = 100,
    #'                      simInfo = mySimInfo,
    #'                      verbose = TRUE)
    #' print(mySpec)


    initialize = function(nChr,
                          lChr,
                          specName = "Undefinded",
                          ploidy = NA,
                          mutRate = NA,
                          recombRate = NA,
                          chrNames = NA,
                          nLoci = NA,
                          effPopSize = NA,
                          recombRateOneVal = TRUE,
                          simInfo = NULL,
                          verbose = TRUE) {
      if (!is.numeric(nChr)) stop("nChr must be numeric.")
      if (floor(nChr) - nChr != 0) stop("nChr must be integer.")
      if (!is.numeric(lChr)) stop("lChr must be numeric.")
      if (any(floor(lChr) - lChr != 0)) stop("lChr must be integers.")
      if (length(lChr) != 1 && length(lChr) != nChr) {
        stop(paste("length(lChr) must be equal to 1 (all chr have the same",
                   "size) or equal to nChr."))
      }

      stopifnot(!any(nChr < 0))
      stopifnot(!any(lChr < 0))

      if (is.na(ploidy)) {
        message('"ploidy" was not specified. The ploidy had been set to "2"')
        ploidy <- 2
      }

      stopifnot(!any(ploidy < 0))

      if (is.null(simInfo)) {
        message("You do not specify `simInfo` class. Here, the default setting will be used.")
        simInfo <- myBreedSimulatR::simInfo$new()
      } else {
        if (class(simInfo)[1] != "simInfo") {
          stop('"class(simInfo)" must be "simInfo"')
        }
      }

      if (simInfo$simGeno) {
        if (any(is.na(nLoci), is.na(effPopSize))) {
          stop("When you simulate marker genotype, you must specify both `nLoci` & `effPopSize`!")
        }

        if (is.na(ploidy)) {
          message('"mutRate" was not specified. The mutRate had been set to "1e-08"')
          mutRate <- 1e-08
        }

        if (is.na(ploidy)) {
          message('"recombRate" was not specified. The recombRate had been set to "1e-04"')
          recombRate <- 1e-04
        }
      } else {
        message("You should specify marker genotype information by `SNP` class.")
      }

      if (all(!is.na(nLoci))) {
        if (!is.numeric(nLoci)) stop("nLoci must be numeric.")
        if (any(floor(nLoci) - nLoci != 0)) stop("nLoci must be integers.")
        if (length(nLoci) != 1 && length(nLoci) != nChr) {
          stop(paste("length(nLoci) must be equal to 1 (all chr have the same",
                     "size) or equal to nChr."))
        }
      }

      stopifnot(!any(nLoci < 0))

      if (!is.na(effPopSize)) {
        if (!is.numeric(effPopSize)) stop("effPopSize must be numeric.")
        if (floor(effPopSize) - effPopSize != 0) stop("effPopSize must be integer.")
      }

      stopifnot(!any(effPopSize < 0))

      self$specName <- specName
      self$nChr <- nChr
      self$ploidy <- ploidy
      self$mutRate <- mutRate
      self$recombRate <- recombRate
      if (all(is.na(chrNames))) {
        self$chrNames <- .charSeq("Chr", 1:self$nChr)
      } else {
        self$chrNames <- chrNames
      }

      if (length(lChr) == 1) {
        self$lChr <- rep(lChr, nChr)
      } else {
        self$lChr <- lChr
      }
      names(self$lChr) <- self$chrNames

      if (length(nLoci) == 1) {
        self$nLoci <- rep(nLoci, nChr)
      } else {
        self$nLoci <- nLoci
      }
      names(self$nLoci) <- self$chrNames

      self$effPopSize <- effPopSize
      self$recombRateOneVal <- recombRateOneVal
      self$simInfo <- simInfo


      if (verbose) cat(paste("A new species has emerged:", self$specName,
                             "!\n\n"))
    },

    #' @description
    #' Display information about the object
    print = function() {
      cat(paste0(
        "Name: ", self$specName, "\n",
        "Number of Chromosomes: ", self$nChr, "\n",
        "Ploidy: ", self$ploidy, "\n",
        "Mutation rate: ", self$mutRate, "\n",
        "Recombination Rate: ", self$recombRate, "\n",
        "Effective population size: ", self$effPopSize, "\n",
        "Simulate marker genotyope: ", self$simInfo$simGeno, "\n",
        "Assume same recombination rate across genome: ", self$recombRateOneVal, "\n",
        "Chromosome length & Number of loci (including markers & QTLs):\n"
      ))
      print(data.frame(chrNames = self$chrNames,
                       chrLength = self$lChr,
                       nLoci = self$nLoci))
    },

    #' @description
    #' Get the chromosomes length
    #' @param chr [str or numeric] chromosome ids
    #' @examples
    #' mySpec$getChrLength()
    #' mySpec$getChrLength(2)
    #' mySpec$getChrLength("Chr3")
    getChrLength = function(chr = NA) {

      # quick return:
      if (is.na(chr)) {
        return(self$lChr)
      }

      stopifnot((is.character(chr) || is.numeric(chr)))

      if (is.character(chr)) {
        id <- as.numeric(regmatches(chr, gregexpr("[0-9]+", chr)))
      } else id <- chr
      self$lChr[id]

    }
  )
)
