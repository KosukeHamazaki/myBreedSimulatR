# Author: Kosuke Hamazaki hamazaki@ut-biomet.org
# 2020 The University of Tokyo
#
# Description:
# Definition of traitInfo class




#' R6 Class representing information of traits (especially simulated ones)
#'
#' @description traitInfo object store specific information of traits (especially simulated ones).
#'
#' @export
#' @import R6
traitInfo <- R6::R6Class(
  "traitInfo",
  public = list(
    #' @field lociInfo [lociInfo class] lociInfo of the specie
    lociInfo = NULL,
    #' @field nMarkers [numeric] Number of markers for each chromosome
    nMarkers = NULL,
    #' @field nTraits [numeric] Number of traits assumed in this breeding scheme
    nTraits = NULL,
    #' @field traitNames [character] Names of traits
    traitNames = NULL,
    #' @field nQTLs [matrix] Number of QTLs of each chromosome for each trait
    nQTLs = NULL,
    #' @field qtlVarList [list] A list of variances of QTLs for additive/dominance, pleiotropy, and epistasis
    #'
    #' \itemize{
    #'  \item{\code{$qtlPle}:} {A variance of QTLs for pleiotropy}
    #'  \item{\code{$qtlEach}:} {A variance of QTLs for each trait (except epistasis)}
    #'  \item{\code{$qtlEpi}:} {A variance of QTLs for each trait (epistasis)}
    #'  }
    qtlVarList = NULL,
    #' @field qtlOverlap [logical] If TRUE, pleiotropy across traits will be assumed
    qtlOverlap = NULL,
    #' @field nOverlap [numeric] Number of QTLs common across traits for each chromosome
    nOverlap = NULL,
    #' @field effCor [numeric] Correlation of pleiotropic QTL effects between traits
    effCor = NULL,
    #' @field propDomi [numeric] Proportion of dominant effects for each trait
    propDomi = NULL,
    #' @field interactionMean [numeric] The expected number that each QTL interacts with others for each trait
    interactionMean = NULL,
    #' @field actionTypeEpiSimple [logical] If TRUE, `actionTypeEpi` will be same as
    #' `actionType` for each QTL.
    actionTypeEpiSimple = NULL,
    #' @field seedSelectMarker [numeric] Random seed for selecting marker information
    seedSelectMarker = NA,
    #' @field seedSelectQTLEach [numeric] Random seed for selecting each QTL and its effect (it should have the same length as nTraits)
    seedSelectQTLEach = NA,
    #' @field seedSelectQTLPle [numeric] Random seed for selecting pleiotropic QTLs and their effects
    seedSelectQTLPle = NA,
    #' @field seedSelectQTLDom [numeric] Random seed for selecting QTL positions with dominant effects (it should have the same length as nTraits)
    seedSelectQTLDom = NA,
    #' @field seedSelectQTLEpi [numeric] Random seed for selecting epistasis and its effect (it should have the same length as nTraits)
    seedSelectQTLEpi = NA,
    #' @field mrkPos [numeric] Marker positions
    mrkPos = NULL,
    #' @field mrkPosEachChr [list] Marker positions for each chromosome
    mrkPosEachChr = NULL,
    #' @field qtlPos [list] QTL positions for each trait
    qtlPos = NULL,
    #' @field qtlEachPos [list] Positions of QTLs specific to one trait for each trait
    qtlEachPos = NULL,
    #' @field qtlPlePos [list] Pleiotropic QTL positions for each trait
    qtlPlePos = NULL,
    #' @field qtlEpiPos [list] Epistatic QTL positions for each trait
    qtlEpiPos = NULL,
    #' @field qtlPosEachChrList [list] QTL positions for each trait for each chromosome
    qtlPosEachChrList = NULL,
    #' @field qtlEachPosEachChrList [list] Positions of QTLs specific to one trait for each trait for each chromosome
    qtlEachPosEachChrList = NULL,
    #' @field qtlPlePosEachChrList [list] Pleiotropic QTL positions for each trait for each chromosome
    qtlPlePosEachChrList = NULL,
    #' @field qtlNamesList [list] QTL names for each trait
    qtlNamesList = NULL,
    #' @field qtlEachNamesList [list] Names of QTLs common to one trait for each trait
    qtlEachNamesList = NULL,
    #' @field qtlPleNamesList [list] Pleiotropic QTL names for each trait
    qtlPleNamesList = NULL,
    #' @field qtlEpiNamesList [list] Epistatic QTL names for each trait
    qtlEpiNamesList = NULL,
    #' @field qtlAllNamesList [list] All QTL names for each trait (including epistatic effects)
    qtlAllNamesList = NULL,
    #' @field actionType [list] A list of vectors representing additive by 0 and dominance by 1 for each trait
    actionType = NULL,
    #' @field actionTypeEpi [list] A list of matrix representing additive by 0 and dominance by 1 for epitasis for each trait
    actionTypeEpi = NULL,
    #' @field qtlEff [list] QTL effects for each trait
    qtlEff = NULL,
    #' @field qtlPleEff [list] Pleiotropic QTL effects for each trait
    qtlPleEff = NULL,
    #' @field qtlEachEff [list] Effects of QTLs specific to one trait for each trait
    qtlEachEff = NULL,
    #' @field qtlEpiEff [list] Epistatic QTL effects for each trait
    qtlEpiEff = NULL,
    #' @field qtlAllEff [list] All QTL effects for each trait (including epistasis)
    qtlAllEff = NULL,
    #' @field nQTLsEach [numeric] Number of QTLs specific to each trait
    nQTLsEach = NULL,
    #' @field nEpi [numeric] Number of epistatic effects
    nEpi = NULL,

    #' @description Create a new traitInfo object.
    #' @param lociInfo [lociInfo class] lociInfo of the specie
    #' @param nMarkers [numeric] Number of markers for each chromosome
    #' @param nTraits [numeric] Number of traits assumed in this breeding scheme
    #' @param traitNames [character] Names of traits
    #' @param nQTLs [matrix] Number of QTLs of each chromosome for each trait
    #' @param qtlVarList [list] A list of variances of QTLs for additive/dominance, pleiotropy, and epistasis
    #'
    #' \itemize{
    #'  \item{\code{$qtlPle}:} {A variance of QTLs for pleiotropy}
    #'  \item{\code{$qtlEach}:} {A variance of QTLs for each trait (except epistasis)}
    #'  \item{\code{$qtlEpi}:} {A variance of QTLs for each trait (epistasis)}
    #'  }
    #' @param qtlOverlap [logical] If TRUE, pleiotropy across traits will be assumed
    #' @param nOverlap [numeric] Number of QTLs common across traits for each chromosome
    #' @param effCor [numeric] Correlation of pleiotropic QTL effects between traits
    #' @param propDomi [numeric] Proportion of dominant effects for each trait
    #' @param interactionMean [numeric] The expected number that each QTL interacts with others for each trait
    #' @param actionTypeEpiSimple [logical] If TRUE, `actionTypeEpi` will be same as
    #' `actionType` for each QTL.
    #' @param qtlPleNamesList [list] Pleiotropic QTL names for each trait
    #' @param qtlEachNamesList [list] Names of QTLs common to one trait for each trait
    #' @param seedSelectMarker [numeric] Random seed for selecting marker information
    #' @param seedSelectQTLEach [numeric] Random seed for selecting each QTL and its effect (it should have the same length as nTraits)
    #' @param seedSelectQTLPle [numeric] Random seed for selecting pleiotropic QTLs and their effects
    #' @param seedSelectQTLDom [numeric] Random seed for selecting QTL positions with dominant effects (it should have the same length as nTraits)
    #' @param seedSelectQTLEpi [numeric] Random seed for selecting epistasis and its effect (it should have the same length as nTraits)
    #' @return A new `traitInfo` object.
    #' @examples
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
    #'
    #' ### create SNP information
    #' myLoci <- lociInfo$new(genoMap = NULL,
    #'                      specie = mySpec,
    #'                      seedSimGeno = NA,
    #'                      lociNames = NULL,
    #'                      founderNames = NULL)
    #'
    #'
    #' ### create traitInfo object
    #' myTrait <- traitInfo$new(lociInfo = myLoci,
    #'                          nMarkers = 80,
    #'                          nTraits = 3,
    #'                          nQTLs = c(4, 8, 3),
    #'                          actionTypeEpiSimple = TRUE,
    #'                          qtlOverlap = TRUE,
    #'                          nOverlap = c(2, 3, 0),
    #'                          effCor = 0.1,
    #'                          propDomi = 0.2,
    #'                          interactionMean = c(4, 1, 2))
    #' print(myTrait)
    #'
    initialize = function(lociInfo,
                          nMarkers,
                          nTraits = NULL,
                          traitNames = NULL,
                          nQTLs = NULL,
                          qtlVarList = NULL,
                          qtlOverlap = TRUE,
                          nOverlap = NULL,
                          effCor = NULL,
                          propDomi = NULL,
                          interactionMean = NULL,
                          actionTypeEpiSimple = TRUE,
                          qtlPleNamesList = NULL,
                          qtlEachNamesList = NULL,
                          seedSelectMarker = NA,
                          seedSelectQTLEach = NA,
                          seedSelectQTLPle = NA,
                          seedSelectQTLDom = NA,
                          seedSelectQTLEpi = NA
    ) {

      # CHECKS:
      # parameters classes
      if (class(lociInfo)[1] != "lociInfo") {
        stop('"class(lociInfo)" must be "lociInfo"')
      }
      nChr <- lociInfo$specie$nChr
      chrNames <- lociInfo$specie$chrNames
      nLoci <- lociInfo$specie$nLoci
      qtlPosCand <- sapply(1:nChr, function(chrNo) {
        qtlPosCandEachChr <- 1:nLoci[chrNo]
        names(qtlPosCandEachChr) <- lociInfo$ids[[chrNo]]

        return(qtlPosCandEachChr)
      },
      simplify = FALSE)
      names(qtlPosCand) <- chrNames


      if (!is.numeric(nMarkers)) stop("nMarkers must be numeric.")
      if (any(floor(nMarkers) - nMarkers != 0)) stop("nMarkers must be integers.")
      if (length(nMarkers) != 1 && length(nMarkers) != nChr) {
        stop(paste("length(nMarkers) must be equal to 1 (all chr have the same",
                   "size) or equal to nChr."))
      } else if (length(nMarkers) == 1) {
        nMarkers <- rep(nMarkers, nChr)
      }
      stopifnot(!any(nMarkers < 0))

      names(nMarkers) <- chrNames

      if (lociInfo$specie$simInfo$simPheno) {
        if (is.null(nTraits)) {
          message('"nTraits" was not specified. The nTraits had been set to "1"')
          nTraits <- 1
        }

        if (!is.numeric(nTraits)) stop("nTraits must be numeric.")
        if (any(floor(nTraits) - nTraits != 0)) stop("nTraits must be integers.")
        stopifnot(nTraits >= 1)

        if (is.null(traitNames)) {
          traitNames <- paste0("Trait_", 1:nTraits)
        }

        stopifnot(length(traitNames) == nTraits)


        if (is.null(propDomi)) {
          message('"propDomi" was not specified. The propDomi had been set to "0".')
          propDomi <- rep(0, nTraits)
        }

        if (!is.numeric(propDomi)) stop("propDomi must be numeric.")
        if (length(propDomi) != 1 && length(propDomi) != nTraits) {
          stop(paste("length(propDomi) must be equal to 1 (all traits have the same",
                     "value) or equal to nTraits."))
        } else if (length(propDomi) == 1) {
          propDomi <- rep(propDomi, nTraits)
        }
        if (any((propDomi > 1) | (propDomi < 0))) {
          stop("All elements in `propDomi` should be within the interval [0, 1].")
        }


        if (is.null(interactionMean)) {
          message('"interactionMean" was not specified. The interactionMean had been set to "0".')
          interactionMean <- rep(0, nTraits)
        }

        if (!is.numeric(interactionMean)) stop("interactionMean must be numeric.")
        if (any(floor(interactionMean) - interactionMean != 0)) stop("interactionMean must be integers.")
        if (length(interactionMean) != 1 && length(interactionMean) != nTraits) {
          stop(paste("length(interactionMean) must be equal to 1 (all traits have the same",
                     "value) or equal to nTraits."))
        } else if (length(interactionMean) == 1) {
          interactionMean <- rep(interactionMean, nTraits)
        }
        stopifnot(!any(propDomi < 0))
        stopifnot(!any(interactionMean < 0))

        names(propDomi) <- names(interactionMean) <- traitNames

        if (!is.null(seedSelectMarker)) {
          if (!is.na(seedSelectMarker)) {
            stopifnot(is.numeric(seedSelectMarker))
            seedSelectMarker <- floor(seedSelectMarker)
          } else {
            seedSelectMarker <- sample(x = 1e9, size = 1)
          }
        }

        if (!is.null(seedSelectQTLEach)) {
          if (!is.na(seedSelectQTLEach)) {
            stopifnot(is.numeric(seedSelectQTLEach))
            stopifnot(length(seedSelectQTLEach) == nTraits)
            seedSelectQTLEach <- floor(seedSelectQTLEach)
          } else {
            seedSelectQTLEach <- sample(x = 1e9, size = nTraits)
          }
        }

        if (!is.null(seedSelectQTLPle)) {
          if (!is.na(seedSelectQTLPle)) {
            stopifnot(is.numeric(seedSelectQTLPle))
            seedSelectQTLPle <- floor(seedSelectQTLPle)
          } else {
            seedSelectQTLPle <- sample(x = 1e9, size = 1)
          }
        }


        if (!is.null(seedSelectQTLDom)) {
          if (!is.na(seedSelectQTLDom)) {
            stopifnot(is.numeric(seedSelectQTLDom))
            stopifnot(length(seedSelectQTLDom) == nTraits)
            seedSelectQTLDom <- floor(seedSelectQTLDom)
          } else {
            seedSelectQTLDom <- sample(x = 1e9, size = nTraits)
          }
        }


        if (!is.null(seedSelectQTLEpi)) {
          if (!is.na(seedSelectQTLEpi)) {
            stopifnot(is.numeric(seedSelectQTLEpi))
            stopifnot(length(seedSelectQTLEpi) == nTraits)
            seedSelectQTLEpi <- floor(seedSelectQTLEpi)
          } else {
            seedSelectQTLEpi <- sample(x = 1e9, size = nTraits)
          }
        }


        if (nTraits == 1) {
          if (is.null(nQTLs)) {
            message('"nQTLs" was not specified. The nQTLs had been set to "10" for all chromosome & traits.')
            nQTLs <- rep(10, nChr)
          }

          if (!is.numeric(nQTLs)) stop("nQTLs must be numeric.")
          if (any(floor(nQTLs) - nQTLs != 0)) stop("nQTLs must be integers.")
          if (length(nQTLs) != 1 && length(nQTLs) != nChr) {
            stop(paste("length(nQTLs) must be equal to 1 (all chr have the same",
                       "size) or equal to nChr."))
          } else if (length(nQTLs) == 1) {
            nQTLs <- rep(nQTLs, nChr)
          }

          names(nQTLs) <- chrNames


          if (is.null(qtlVarList)) {
            qtlVarList <- list(qtlEach = 1,
                               qtlEpi = 1)
          }

          if (!all(c("qtlEach", "qtlEpi") %in% names(qtlVarList))) {
            stop("`qtlVarList` should contain objects names `qtlPle`, `qtlEach`, `qtlEpi`.")
          } else {
            effVarEach <- qtlVarList$qtlEach
            effVarEpi <- qtlVarList$qtlEpi

            if (!is.numeric(effVarEach)) stop("qtlEach must be numeric.")
            if (!is.numeric(effVarEpi)) stop("qtlEpi must be numeric.")
          }
          names(effVarEach) <- names(effVarEpi) <- traitNames
          qtlVarList <- list(qtlEach = effVarEach,
                             qtlEpi = effVarEpi)

          qtlOverlap <- FALSE
          nOverlap <- NULL
          effCor <- NULL

          nLociUsed <- nMarkers + nQTLs
          nQTLsEach <- nQTLs

          qtlPleNamesList <- list(NULL)

          if (is.null(qtlEachNamesList)) {
            qtlEachNamesList <- list(paste0("QTL_", 1:sum(nQTLsEach)))
          }
          stopifnot(length(qtlEachNamesList) == 1)
          stopifnot(length(qtlEachNamesList[[1]]) == sum(nQTLsEach))

          names(qtlPleNamesList) <- names(qtlEachNamesList) <- traitNames

          qtlPleEff <- list(NULL)
          qtlPlePos <- list(NULL)

          qtlEachNames <- qtlEachNamesList[[1]]

          set.seed(seed = seedSelectQTLEach)
          qtlEachPosEachChr <- sapply(1:nChr, function(chrNo) {
            sampleVec(x = qtlPosCand[[chrNo]], size = nQTLsEach[chrNo])
          }, simplify = FALSE)
          names(qtlEachPosEachChr) <- chrNames

          set.seed(seed = seedSelectQTLEach)
          qtlEachPos <- list(sampleVec(unlist(sapply(1:nChr, function(chrNo) {
            qtlEachPosEachChr[[chrNo]] + cumsum(c(0, nLoci))[chrNo]
          }, simplify = FALSE))))
          names(qtlEachPos[[1]]) <- qtlEachNames
          qtlPos <- qtlEachPos


          qtlPosCandNow <- sapply(1:nChr, function(chrNo) {
            qtlPosCandNowEachChr <- (qtlPosCand[[chrNo]])[!(names(qtlPosCand[[chrNo]]) %in%
                                                                 names(qtlEachPosEachChr[[chrNo]]))]
            return(qtlPosCandNowEachChr)
          }, simplify = FALSE)

          qtlEachPosEachChrList <- list(qtlEachPosEachChr)
          qtlPlePosEachChrList <- list(NULL)
          names(qtlEachPosEachChrList) <-
            names(qtlPlePosEachChrList) <- traitNames
          qtlPosEachChrList <- qtlEachPosEachChrList

          set.seed(seed = seedSelectQTLEach)
          qtlEachEff0 <- rnorm(n = sum(nQTLsEach), mean = 0,
                               sd = sqrt(effVarEach / sum(nQTLsEach)))
          qtlEachEff <- list(qtlEachEff0[order(abs(qtlEachEff0), decreasing = TRUE)])
          names(qtlEachEff[[1]]) <- qtlEachNames

          set.seed(seed = seedSelectQTLEpi)
          nEpi <- round(sum(rpois(n = sum(nQTLs), lambda = interactionMean)) / 2, 0)
          names(nEpi) <- traitNames
          if (nEpi > 0) {
            set.seed(seed = seedSelectQTLEpi)
            qtlEpiPos0 <- t(sapply(1:nEpi, function(x) sort(sampleVec(qtlPos[[1]], 2))))
            qtlEpiPos0 <- qtlEpiPos0[!duplicated(qtlEpiPos0), , drop = FALSE]
            nEpi <- nrow(qtlEpiPos0)
            qtlEpiNames <- paste0(qtlEachNames[match((qtlEpiPos0)[, 1], qtlPos[[1]])], " x ",
                                  qtlEachNames[match((qtlEpiPos0)[, 2], qtlPos[[1]])])
            rownames(qtlEpiPos0) <- qtlEpiNames
            colnames(qtlEpiPos0) <- paste0("Epi_", 1:2)
            qtlEpiPos <- list(qtlEpiPos0)

            qtlAllNames <- c(qtlEachNames, qtlEpiNames)

            set.seed(seed = seedSelectQTLDom)
            actionType <- list(rbinom(sum(nQTLs), 1, propDomi))
            names(actionType[[1]]) <- qtlEachNames

            if (!actionTypeEpiSimple) {
              set.seed(seed = seedSelectQTLDom)
              actionTypeEpi <- list(matrix(rbinom(2 * nEpi, 1, propDomi), nrow = nEpi))
            } else {
              actionTypeEpi <- list(matrix((actionType[[1]])[apply(qtlEpiPos0, 2, function(x) {
                match(x, qtlPos[[1]])
              })], nrow = nEpi))
            }
            rownames(actionTypeEpi[[1]]) <- qtlEpiNames
            colnames(actionTypeEpi[[1]]) <- paste0("Epi_", 1:2)

            set.seed(seed = seedSelectQTLEpi)
            qtlEpiEff0 <- rnorm(n = nEpi, mean = 0, sd = sqrt(effVarEpi / nEpi))
            qtlEpiEff <- list(qtlEpiEff0[order(abs(qtlEpiEff0), decreasing = TRUE)])
            names(qtlEpiEff[[1]]) <- qtlEpiNames

            qtlAllEff <- list(c(qtlEachEff[[1]], qtlEpiEff[[1]]))
          } else {
            qtlEpiPos <- list(NULL)
            qtlEpiNames <- NULL

            qtlAllNames <- qtlEachNames

            set.seed(seed = seedSelectQTLDom)
            actionType <- list(rbinom(sum(nQTLs), 1, propDomi))
            names(actionType[[1]]) <- qtlEachNames

            actionTypeEpi <- list(NULL)
            qtlVarList$qtlEpi <- 0

            qtlEpiEff <- list(NULL)
            qtlAllEff <- qtlEachEff
          }
          qtlNamesList <- qtlEachNamesList
          qtlEpiNamesList <- list(qtlEpiNames)
          qtlAllNamesList <- list(qtlAllNames)
          qtlEff <- qtlEachEff

          names(qtlPos) <- names(qtlEachPos) <- names(qtlPlePos) <- names(qtlEpiPos) <-
            names(qtlEpiNamesList) <- names(qtlAllNamesList) <-
            names(actionType) <- names(actionTypeEpi) <-
            names(qtlPleEff) <- names(qtlEachEff) <- names(qtlEff) <-
            names(qtlEpiEff) <- names(qtlAllEff) <- traitNames

        } else {
          if (is.null(nQTLs)) {
            message('"nQTLs" was not specified. The nQTLs had been set to "10" for all chromosome & traits.')
            nQTLs <- matrix(10, nrow = nTraits, ncol = nChr)
          }

          if (is.vector(nQTLs)) {
            if ((length(nQTLs) != 1) && (length(nQTLs) != nChr) &&
                (length(nQTLs) != nTraits)) {
              stop(paste("length(nQTLs) must be equal to 1 (all chr have the same",
                         "size) or equal to nChr."))
            } else if (length(nQTLs) == 1) {
              nQTLs <- matrix(nQTLs, nrow = nTraits, ncol = nChr)
            } else if (length(nQTLs) == nTraits) {
              nQTLs <- matrix(rep(nQTLs, nChr), nrow = nTraits, ncol = nChr)
            } else if (length(nQTLs) == nChr) {
              nQTLs <- matrix(rep(nQTLs, nTraits), nrow = nTraits,
                              ncol = nChr, byrow = TRUE)
            }
          } else if (is.matrix(nQTLs)) {
            if (length(dim(nQTLs)) == 2) {
              if (!all(dim(nQTLs) == c(nTraits, nChr))) {
                stop(paste0("The dimension of `nQTLs` must be equal to the number of traits ",
                            "and the number of chromosomes respectively."))
              }
            } else {
              stop("`nQTLs` must be a matrix (2-dimensional array) class object.")
            }
          } else {
            stop("`nQTLs` must be a matrix (2-dimensional array) class object.")
          }

          if (!is.numeric(nQTLs)) stop("nQTLs must be numeric.")
          if (any(floor(nQTLs) - nQTLs != 0)) stop("nQTLs must be integers.")
          stopifnot(!any(nQTLs < 0))

          rownames(nQTLs) <- traitNames
          colnames(nQTLs) <- chrNames



          if (is.null(qtlVarList)) {
            qtlVarList <- list(qtlPle = rep(1, nTraits),
                               qtlEach = rep(1, nTraits),
                               qtlEpi = rep(1, nTraits))
          }

          if (!all(c("qtlPle", "qtlEach", "qtlEpi") %in% names(qtlVarList))) {
            stop("`qtlVarList` should contain objects names `qtlPle`, `qtlEach`, `qtlEpi`.")
          } else {
            effVarPle <- qtlVarList$qtlPle
            effVarEach <- qtlVarList$qtlEach
            effVarEpi <- qtlVarList$qtlEpi

            if (!is.numeric(effVarPle)) stop("qtlPle must be numeric.")
            if (length(effVarPle) != 1 && length(effVarPle) != nTraits) {
              stop(paste("length(qtlPle) must be equal to 1 (all trait have the same",
                         "size) or equal to nTraits."))
            } else if (length(effVarPle) == 1) {
              effVarPle <- rep(effVarPle, nTraits)
            }

            if (!is.numeric(effVarEach)) stop("qtlEach must be numeric.")
            if (length(effVarEach) != 1 && length(effVarEach) != nTraits) {
              stop(paste("length(qtlEach) must be equal to 1 (all trait have the same",
                         "size) or equal to nTraits."))
            } else if (length(effVarEach) == 1) {
              effVarEach <- rep(effVarEach, nTraits)
            }

            if (!is.numeric(effVarEpi)) stop("qtlEpi must be numeric.")
            if (length(effVarEpi) != 1 && length(effVarEpi) != nTraits) {
              stop(paste("length(qtlEpi) must be equal to 1 (all trait have the same",
                         "size) or equal to nTraits."))
            } else if (length(effVarEpi) == 1) {
              effVarEpi <- rep(effVarEpi, nTraits)
            }
          }

          names(effVarPle) <- names(effVarEach) <- names(effVarEpi) <- traitNames
          qtlVarList <- list(qtlPle = effVarPle,
                             qtlEach = effVarEach,
                             qtlEpi = effVarEpi)


          if (qtlOverlap) {
            if (is.null(nOverlap)) {
              message('"nOverlap" was not specified. The nOverlap had been set to "2" for all chromosome & traits.')
              nOverlap <- rep(2, nChr)
            }

            if (!is.numeric(nOverlap)) stop("nOverlap must be numeric.")
            if (any(floor(nOverlap) - nOverlap != 0)) stop("nOverlap must be integers.")
            if (length(nOverlap) != 1 && length(nOverlap) != nChr) {
              stop(paste("length(nOverlap) must be equal to 1 (all chr have the same",
                         "size) or equal to nChr."))
            } else if (length(nOverlap) == 1) {
              nOverlap <- rep(nOverlap, nChr)
            }

            stopifnot(!any(nOverlap < 0))
            if (sum(nOverlap) == 0) {
              stop("If nOverlap = 0 for all chromosomes, you should set 'qtlOverlap = FALSE'!!!")
            }

            if (any(apply(nQTLs, 1, function(x) any(x < nOverlap)))) {
              stop("The number of overlapping QTLs across traits (`nOverlap`) should be equal to or less than nQTLs for any chromosome or any trait.")
            }

            if (is.null(effCor)) {
              message('"effCor" was not specified. The effCor had been set to "0.5" for all chromosome & traits.')
              effCor <- 0.5
            }
            if (!is.numeric(effCor)) stop("effCor must be numeric.")

            if (is.vector(effCor)) {
              if (length(effCor) != 1) {
                stop("`effCor` should be scalar or 2-dimensional matrix with `nTraits x nTraits` dimension.")
              }
              effCorMat <- diag(nTraits)
              effCorMat[upper.tri(x = effCorMat)] <- effCor
              effCorMat[lower.tri(x = effCorMat)] <- effCor
              effCor <- effCorMat
            } else if (is.matrix(effCor)) {
              if (length(dim(effCor)) != 2) {
                stop("`effCor` should be scalar or 2-dimensional matrix with `nTraits x nTraits` dimension.")
              } else {
                if (!all(dim(effCor) == c(nTraits, nTraits))) {
                  stop("`effCor` should be scalar or 2-dimensional matrix with `nTraits x nTraits` dimension.")
                }
                stopifnot(all(diag(effCor) == 1))
              }
            } else {
              stop("`effCor` should be scalar or 2-dimensional matrix with `nTraits x nTraits` dimension.")
            }

            if (any((effCor > 1) | (effCor < -1))) {
              stop("All elements in `effCor` should be within the interval [-1, 1].")
            }


            if (is.null(qtlPleNamesList)) {
              qtlPleNamesList <- sapply(X = rep(sum(nOverlap), nTraits), function(i) {
                paste0("QTLPle_", 1:i)
              }, simplify = FALSE)
            }

            stopifnot(length(qtlPleNamesList) == nTraits)
            stopifnot(all(unlist(lapply(qtlPleNamesList, length)) == rep(sum(nOverlap), nTraits)))


          } else {
            nOverlap <- rep(0, nChr)
            effCor <- diag(nTraits)

            qtlPleNamesList <- rep(list(NULL), nTraits)
          }

          names(nOverlap) <- chrNames
          rownames(effCor) <- colnames(effCor) <- traitNames


          nLociUsed <- nMarkers + (apply(nQTLs, 2, sum) - nOverlap)
          nQTLsEach <- nQTLs - matrix(rep(nOverlap, nTraits), nrow = nTraits, byrow = TRUE)


          if (is.null(qtlEachNamesList)) {
            qtlEachNamesList <- sapply(X = apply(nQTLsEach, 1, sum), function(i) {
              if (i >= 1) {
                return(paste0("QTL_", 1:i))
              } else {
                return(NULL)
              }
            }, simplify = FALSE)
          }

          stopifnot(length(qtlEachNamesList) == nTraits)
          stopifnot(all(unlist(lapply(qtlEachNamesList, length)) == apply(nQTLsEach, 1, sum)))

          names(qtlPleNamesList) <- names(qtlEachNamesList) <- traitNames



          nEpi <- rep(NA, nTraits)
          names(nEpi) <- traitNames
          qtlPos <- qtlEachPos <- qtlEpiPos <-
            qtlPosEachChrList <- qtlEachPosEachChrList <-
            qtlNamesList <- qtlEpiNamesList <- qtlAllNamesList <-
            actionType <- actionTypeEpi <- qtlEff <-
            qtlPleEff <- qtlEachEff <- qtlEpiEff <-
            qtlAllEff <- rep(list(NULL), nTraits)


          if (qtlOverlap) {
            set.seed(seed = seedSelectQTLPle)
            qtlPlePosEachChr <- sapply(1:nChr, function(chrNo) {
              sampleVec(x = qtlPosCand[[chrNo]], size = nOverlap[chrNo])
            }, simplify = FALSE)
            names(qtlPlePosEachChr) <- chrNames
            qtlPlePosEachChrList <- rep(list(qtlPlePosEachChr), nTraits)

            set.seed(seed = seedSelectQTLPle)
            qtlPlePos <- rep(list(sampleVec(unlist(sapply(1:nChr, function(chrNo) {
              qtlPlePosEachChr[[chrNo]] + cumsum(c(0, nLoci))[chrNo]
            }, simplify = FALSE)))), nTraits)
            qtlPlePos <- lapply(1:nTraits, function(traitNo) {
              names(qtlPlePos[[traitNo]]) <- qtlPleNamesList[[traitNo]]
              return(qtlPlePos[[traitNo]])
            })

            qtlPosCandExcPle <- sapply(1:nChr, function(chrNo) {
              qtlPosCandExcPleEachChr <- (qtlPosCand[[chrNo]])[!(names(qtlPosCand[[chrNo]]) %in%
                                                                   names(qtlPlePosEachChr[[chrNo]]))]
              return(qtlPosCandExcPleEachChr)
            }, simplify = FALSE)

            effVarPleMat <- diag(sqrt(effVarPle)) %*% effCor %*% diag(sqrt(effVarPle))
            rownames(effVarPleMat) <- colnames(effVarPleMat) <- traitNames

            set.seed(seed = seedSelectQTLPle)
            qtlPleEffs0 <- matrix(MASS::mvrnorm(n = sum(nOverlap), mu = rep(0, nTraits),
                                                Sigma = effVarPleMat / sum(nOverlap)),
                                  nrow = sum(nOverlap), ncol = nTraits)
            # qtlPleEffs0 <- matrix(mvrnorm(n = 1, mu = rep(0, nTrait * nOverlap),
            #                                Sigma = kronecker(diag(nOverlap), effVarAll)),
            #                        nrow = nOverlap, byrow = TRUE)

            qtlPleEffSize <- apply(qtlPleEffs0, 1, function(x) sum(x ^ 2))
            qtlPleEffs <- qtlPleEffs0[order(qtlPleEffSize, decreasing = TRUE), , drop = FALSE]
            rownames(qtlPleEffs) <- qtlPleNamesList[[1]]
          } else {
            qtlPlePosEachChr <- rep(list(NULL), nChr)
            names(qtlPlePosEachChr) <- chrNames
            qtlPlePosEachChrList <- rep(list(qtlPlePosEachChr), nTraits)

            qtlPlePos  <- rep(list(NULL), nTraits)
            qtlPosCandExcPle <- qtlPosCand

            qtlPleEffs <- NULL
          }


          for (traitNo in 1:nTraits) {
            qtlPleNames <- qtlPleNamesList[[traitNo]]
            qtlEachNames <- qtlEachNamesList[[traitNo]]
            qtlNames <- c(qtlPleNames, qtlEachNames)

            set.seed(seed = seedSelectQTLEpi[traitNo])
            nEpi[traitNo] <- round(sum(rpois(n = sum(nQTLs), lambda = interactionMean[traitNo])) / 2, 0)

            if (traitNo == 1) {
              qtlPosCandNow <- qtlPosCandExcPle
            } else {
              qtlPosCandNow <- sapply(1:nChr, function(chrNo) {
                qtlPosCandNowEachChr <- (qtlPosCandNow[[chrNo]])[!(names(qtlPosCandNow[[chrNo]]) %in%
                                                                     names(qtlEachPosEachChr[[chrNo]]))]
                return(qtlPosCandNowEachChr)
              }, simplify = FALSE)
            }

            set.seed(seed = seedSelectQTLEach[traitNo])
            qtlEachPosEachChr <- sapply(1:nChr, function(chrNo) {
              sampleVec(x = qtlPosCandNow[[chrNo]], size = nQTLsEach[traitNo, chrNo])
            }, simplify = FALSE)
            names(qtlEachPosEachChr) <- chrNames
            qtlEachPosEachChrList[[traitNo]] <- qtlEachPosEachChr

            set.seed(seed = seedSelectQTLEach[traitNo])
            qtlEachPos[[traitNo]] <- sampleVec(unlist(sapply(1:nChr, function(chrNo) {
              qtlEachPosEachChr[[chrNo]] + cumsum(c(0, nLoci))[chrNo]
            }, simplify = FALSE)))
            names(qtlEachPos[[traitNo]]) <- qtlEachNames


            qtlPos[[traitNo]] <- c(qtlPlePos[[traitNo]], qtlEachPos[[traitNo]])
            names(qtlPos[[traitNo]]) <- c(qtlPleNames, qtlEachNames)

            qtlPosEachChr <- sapply(1:nChr, function(chrNo) {
              c(qtlPlePosEachChr[[chrNo]], qtlEachPosEachChr[[chrNo]])
            }, simplify = FALSE)
            qtlPosEachChrList[[traitNo]] <- qtlPosEachChr


            if (qtlOverlap) {
              qtlPleEffNow <- qtlPleEffs[, traitNo]
              names(qtlPleEffNow) <- qtlPleNames
            } else {
              qtlPleEffNow <- NULL
            }

            set.seed(seed = seedSelectQTLEach[traitNo])
            qtlEachEffNow0 <- rnorm(n = sum(nQTLsEach[traitNo, ]), mean = 0,
                                    sd = sqrt(effVarEach[traitNo] / sum(nQTLsEach[traitNo, ])))
            qtlEachEffNow <-
              qtlEachEffNow0[order(abs(qtlEachEffNow0), decreasing = TRUE)]
            names(qtlEachEffNow) <- qtlEachNames

            qtlEffNow <- c(qtlPleEffNow, qtlEachEffNow)


            if (nEpi[traitNo] > 0) {
              set.seed(seed = seedSelectQTLEpi[traitNo])
              qtlEpiPos0 <- t(sapply(1:nEpi[traitNo], function(x) sort(sampleVec(qtlPos[[traitNo]], 2))))
              qtlEpiPos0 <- qtlEpiPos0[!duplicated(qtlEpiPos0), , drop = FALSE]
              nEpi[traitNo] <- nrow(qtlEpiPos0)
              qtlEpiNames <- paste0(qtlNames[match((qtlEpiPos0)[, 1], qtlPos[[traitNo]])], " x ",
                                    qtlNames[match((qtlEpiPos0)[, 2], qtlPos[[traitNo]])])
              rownames(qtlEpiPos0) <- qtlEpiNames
              colnames(qtlEpiPos0) <- paste0("Epi_", 1:2)
              qtlEpiNamesList[[traitNo]] <- qtlEpiNames
              qtlEpiPos[[traitNo]] <- qtlEpiPos0


              qtlAllNames <- c(qtlNames, qtlEpiNames)

              set.seed(seed = seedSelectQTLDom[traitNo])
              actionType[[traitNo]] <- rbinom(sum(nQTLs[traitNo, ]), 1, propDomi[traitNo])
              names(actionType[[traitNo]]) <- qtlNames

              set.seed(seed = seedSelectQTLDom[traitNo])
              if (!actionTypeEpiSimple) {
              actionTypeEpi[[traitNo]] <- matrix(rbinom(2 * nEpi[traitNo], 1, propDomi[traitNo]),
                                                 nrow = nEpi[traitNo])
              } else {
                actionTypeEpi[[traitNo]] <- matrix((actionType[[traitNo]])[apply(qtlEpiPos0, 2, function(x) {
                  match(x, qtlPos[[traitNo]])
                })], nrow = nEpi[traitNo])
              }
              rownames(actionTypeEpi[[traitNo]]) <- qtlEpiNames
              colnames(actionTypeEpi[[traitNo]]) <- paste0("Epi_", 1:2)

              set.seed(seed = seedSelectQTLEpi[traitNo])
              qtlEpiEff0 <- rnorm(n = nEpi[traitNo], mean = 0,
                                  sd = sqrt(effVarEpi[traitNo] / nEpi[traitNo]))
              qtlEpiEffNow <- qtlEpiEff0[order(abs(qtlEpiEff0), decreasing = TRUE)]
              names(qtlEpiEffNow) <- qtlEpiNames
            } else {
              qtlAllNames <- c(qtlPleNames, qtlEachNames)

              set.seed(seed = seedSelectQTLDom)
              actionType[[traitNo]] <- rbinom(sum(nQTLs[traitNo, ]), 1, propDomi[traitNo])
              names(actionType[[traitNo]]) <- qtlNames

              qtlVarList$qtlEpi[traitNo] <- 0

              qtlEpiEffNow <- NULL
            }
            qtlNamesList[[traitNo]] <- qtlNames
            qtlAllNamesList[[traitNo]] <- qtlAllNames

            qtlAllEffNow <- c(qtlPleEffNow, qtlEachEffNow, qtlEpiEffNow)
            if (!is.null(qtlPleEffNow)) {
              qtlPleEff[[traitNo]] <- qtlPleEffNow
            }
            qtlEachEff[[traitNo]] <- qtlEachEffNow
            qtlEff[[traitNo]] <- qtlEffNow

            if (!is.null(qtlEpiEffNow)) {
              qtlEpiEff[[traitNo]] <- qtlEpiEffNow
            }
            qtlAllEff[[traitNo]] <- qtlAllEffNow
          }   ### traitNo


          names(qtlPos) <- names(qtlEachPos) <- names(qtlPlePos) <-
            names(qtlEpiPos) <- names(qtlPosEachChrList) <-
            names(qtlEachPosEachChrList) <- names(qtlPlePosEachChrList) <-
            names(qtlNamesList) <- names(qtlEpiNamesList) <- names(qtlAllNamesList) <-
            names(actionType) <- names(actionTypeEpi) <-
            names(qtlPleEff) <- names(qtlEachEff) <- names(qtlEff) <-
            names(qtlEpiEff) <- names(qtlAllEff) <- traitNames
        }

        if (any(lociInfo$specie$nLoci < nLociUsed)) {
          stop("The number of loci used (sum of the number of markers & QTLs) must be lower than nLoci for any chromosome.")
        }


        mrkPosCand <- sapply(1:nChr, function(chrNo) {
          qtlPosCandNowEachChr <- (qtlPosCandNow[[chrNo]])[!(names(qtlPosCandNow[[chrNo]]) %in%
                                                               names(qtlEachPosEachChr[[chrNo]]))]
        }, simplify = FALSE)

      } else {
        nQTLs <- qtlVarList <- qtlOverlap <- nOverlap <- effCor <- propDomi <-
          interactionMean <- qtlPos <- qtlEachPos <- qtlPlePos <- qtlEpiPos <-
          qtlPosEachChrList <- qtlEachPosEachChrList <- qtlPlePosEachChrList <-
          qtlNamesList <- qtlEachNamesList <- qtlPleNamesList <- qtlEpiNamesList <-
          qtlAllNamesList <- actionType <- actionTypeEpi <- qtlPleEff <-
          qtlEachEff <- qtlEff <- qtlEpiEff <- qtlAllEff <- seedSelectQTLEach <-
          seedSelectQTLPle <- seedSelectQTLDom <- seedSelectQTLEpi <-
          nQTLsEach <- nEpi <- NULL

        mrkPosCand <- qtlPosCand
      }


      if (!is.null(seedSelectMarker)) {
        if (!is.na(seedSelectMarker)) {
          stopifnot(is.numeric(seedSelectMarker))
          seedSelectMarker <- floor(seedSelectMarker)
        } else {
          seedSelectMarker <- sample(x = 1e9, size = 1)
        }
      }

      set.seed(seed = seedSelectMarker)

      mrkPosEachChr <- sapply(1:nChr, function(chrNo) {
        sort(sampleVec(x = mrkPosCand[[chrNo]], size = nMarkers[chrNo], replace = FALSE))
      }, simplify = FALSE)
      names(mrkPosEachChr) <- chrNames

      mrkPos <- unlist(sapply(1:nChr, function(chrNo) {
        mrkPosEachChr[[chrNo]] + cumsum(c(0, nLoci))[chrNo]
      }, simplify = FALSE))



      self$lociInfo <- lociInfo
      self$nMarkers <- nMarkers
      self$nTraits <- nTraits
      self$traitNames <- traitNames
      self$nQTLs <- nQTLs
      self$qtlVarList <- qtlVarList
      self$qtlOverlap <- qtlOverlap
      self$nOverlap <- nOverlap
      self$effCor <- effCor
      self$propDomi <- propDomi
      self$interactionMean <- interactionMean
      self$actionTypeEpiSimple <- actionTypeEpiSimple
      self$seedSelectMarker <- seedSelectMarker
      self$seedSelectQTLEach <- seedSelectQTLEach
      self$seedSelectQTLPle <- seedSelectQTLPle
      self$seedSelectQTLDom <- seedSelectQTLDom
      self$seedSelectQTLEpi <- seedSelectQTLEpi

      self$mrkPos <- mrkPos
      self$mrkPosEachChr <- mrkPosEachChr
      self$qtlPos <- qtlPos
      self$qtlEachPos <- qtlEachPos
      self$qtlPlePos <- qtlPlePos
      self$qtlEpiPos <- qtlEpiPos
      self$qtlPosEachChrList <- qtlPosEachChrList
      self$qtlEachPosEachChrList <- qtlEachPosEachChrList
      self$qtlPlePosEachChrList <- qtlPlePosEachChrList
      self$qtlNamesList <- qtlNamesList
      self$qtlEachNamesList <- qtlEachNamesList
      self$qtlPleNamesList <- qtlPleNamesList
      self$qtlEpiNamesList <- qtlEpiNamesList
      self$qtlAllNamesList <- qtlAllNamesList
      self$actionType <- actionType
      self$actionTypeEpi <- actionTypeEpi
      self$qtlPleEff <- qtlPleEff
      self$qtlEachEff <- qtlEachEff
      self$qtlEff <- qtlEff
      self$qtlEpiEff <- qtlEpiEff
      self$qtlAllEff <- qtlAllEff
      self$nQTLsEach <- nQTLsEach
      self$nEpi <- nEpi

    },

    #' @description Detail information of map for marker genome
    #' @param lociNames [character] loci ids
    #' @examples
    #' myLoci$genoMapDetail()
    #' myLoci$genoMapDetail("Loci_6")
    genoMapDetail = function(lociNames = NULL) {
      genoMap <- self$lociInfo$genoMap

      if (self$lociInfo$specie$simInfo$simPheno) {
        for (traitNo in 1:self$nTraits) {
          traitNameNow <- self$traitNames[traitNo]
          qtlNamesNow <- self$qtlNamesList[[traitNo]]
          qtlPosNow <- self$qtlPos[[traitNo]]

          qtlNameForMapNow <- rep(NA, sum(self$lociInfo$specie$nLoci))
          qtlNameForMapNow[qtlPosNow] <- qtlNamesNow

          genoMap <- data.frame(genoMap, qtlNameForMapNow)
          colnames(genoMap)[ncol(genoMap)] <- traitNameNow
        }
      } else {
        genoMapNA <- matrix(NA, nrow = nrow(genoMap), ncol = self$nTraits)
        rownames(genoMapNA) <- rownames(genoMap)
        colnames(genoMapNA) <- self$traitNames
        genoMap <- cbind(genoMap, genoMapNA)
      }
      if (is.null(lociNames)) {
        return(genoMap)
      } else {
        genoMap[genoMap$lociNames %in% lociNames]
      }
    },

    #' @description Display summary information about the object: traitInfo
    print = function() {
      cat(paste0("Number of traits: ", self$nTraits, "\n",
                 "Number of markers: \n"))
      print(self$nMarkers)
      cat(paste0("Number of QTLs (except epistasis) in total: \n"))
      print(self$nQTLs)
      cat(paste0("----------------------------------------- \n"))
      cat(paste0("Number of QTLs specific to each trait: \n"))
      print(self$nQTLsEach)
      cat(paste0("Number of pleiotropic QTLs across traits: \n"))
      print(self$nOverlap)
      cat(paste0("Number of epistatic effects for each trait: \n"))
      print(self$nEpi)
    },

    #' @description
    #' plot QTL map using the \pkg{plotly} package
    #' @param alphaMarker [numeric] transparency for markers see \link[plotly]{plot_ly}
    #' @param alphaQTL [numeric] transparency for QTLs see \link[plotly]{plot_ly}
    #' @param sizeMarker [numeric] size for markers
    #' @param sizeQTLmin [numeric] size for QTL with minimum effect
    #' @param sizeQTLmax [numeric] size for QTL with maximum effect
    #'
    #' @import plotly
    #'
    #' @examples
    #' myLoci$plot(alphaMarker = 0.1, alphaQTL = 0.9,
    #'             sizeMarker = 5, sizeQTLmin = 6, sizeQTLmax = 12)
    plot = function(alphaMarker = 0.05,
                    alphaQTL = 0.8,
                    sizeMarker = 5,
                    sizeQTLmin = 6,
                    sizeQTLmax = 12) {
      ends <- self$lociInfo$specie$lChr
      chrNames <- self$lociInfo$specie$chrNames
      if (self$lociInfo$specie$simInfo$simPheno) {
        genoDetail <- self$genoMapDetail()
        genoDetail$rec <- round(genoDetail$rec, 3)
        genoDetailQTLs <- genoDetail[apply(genoDetail[, -c(1:4), drop = FALSE], 1, function(x) any(!is.na(x))), ]
        genoDetailMarkers <- genoDetail[self$mrkPos, ]

        effQTLs <- sapply(X = 1:self$nTraits, function(traitNo) {
          effQTLNow <- rep(NA, nrow(genoDetailQTLs))
          effQTLNow[match(names(self$qtlEff[[traitNo]]), genoDetailQTLs[, traitNo + 4])] <-
            self$qtlEff[[traitNo]]

          return(effQTLNow)
        })

        effQTLsWithNA <- effQTLs
        effQTLs[is.na(effQTLs)] <- 0
        rownames(effQTLs) <- genoDetailQTLs$lociNames
        colnames(effQTLs) <- self$traitNames
        effQTLSize <- apply(effQTLs, 1, function(x) sqrt(sum(x ^ 2)))

        effQTLSizeScaled <- sizeQTLmin + (effQTLSize - min(effQTLSize)) *
          (sizeQTLmax - sizeQTLmin) / diff(range(effQTLSize))

        for (traitNo in 1:self$nTraits) {
          nonNANow <- !is.na(genoDetailQTLs[, traitNo + 4])
          actionTypeNow <- self$actionType[[traitNo]]
          actionTypeNow <- factor(ifelse(actionTypeNow == 1, "Dom", "Add"), levels = c("Add", "Dom"))
          genoDetailQTLs[nonNANow, traitNo + 4] <-
            paste0(genoDetailQTLs[nonNANow, traitNo + 4], ", ",
                   round(effQTLsWithNA[nonNANow, traitNo], 3), ", ",
                   actionTypeNow[genoDetailQTLs[nonNANow, traitNo + 4]])
        }
      } else {
        genoDetailMarkers <- self$genoMapDetail()[self$mrkPos, ]
      }
      genoDetailMarkers$chr <- factor(genoDetailMarkers$chr, levels = chrNames)
      plt <- plotly::plot_ly(data = genoDetailMarkers,
                             x = ~ chr,
                             y = ~ pos,
                             type = "scatter",
                             mode = "markers",
                             alpha = alphaMarker,
                             marker = list(size = sizeMarker),
                             name = "Markers",
                             hoverinfo = 'text',
                             text = apply(genoDetailMarkers, 1, function(l) {
                               paste(names(l), ":", l, collapse = "\n")
                             })) %>%
        plotly::add_markers(x = rep(names(ends), 2),
                            y = c(ends, rep(0, length(ends))),
                            alpha = 1,
                            name = "Chromosome's edges",
                            hoverinfo = 'text',
                            text = paste(rep(names(ends), 2),
                                         ": length =",
                                         c(ends, rep(0, length(ends)))))

      if (self$lociInfo$specie$simInfo$simPheno) {
        genoDetailQTLs$chr <- factor(genoDetailQTLs$chr, levels = chrNames)
        plt <- plt %>%
          plotly::add_markers(data = genoDetailQTLs,
                              x = ~ chr,
                              y = ~ pos,
                              type = "scatter",
                              mode = "markers",
                              alpha = alphaQTL,
                              marker = list(size = effQTLSizeScaled),
                              name = "QTLs",
                              hoverinfo = 'text',
                              text = apply(genoDetailQTLs, 1, function(l) {
                                paste(names(l), ":", l, collapse = "\n")
                              }))
      }
      print(plt)
    }

  )
)
