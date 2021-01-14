# Author: Kosuke Hamazaki hamazaki@ut-biomet.org
# 2020 The University of Tokyo
#
# Description:
# Definition of trialInfo class




#' R6 Class representing information of field trial
#'
#' @description traitInfo object store specific information of field trial.
#'
#' @export
#' @import R6
trialInfo <- R6::R6Class(
  "trialInfo",
  public = list(
    #' @field population [population class] population class object
    population = NULL,
    #' @field herit [numeric] Heritability for each trait (plot-based/line-based)
    herit = NULL,
    #' @field nRep [numeric] Replication of the field trial (common to all traits)
    nRep = NULL,
    #' @field multiTraitsAsEnvs [logical] Treat multiple traits as multiple environments or not
    multiTraitsAsEnvs = NULL,
    #' @field envSpecificEffects [numeric] Effects specific to each environments / treatments.
    #' If `multiTraitsAsEnvs = FALSE`, envSpecificEffects will be 0 for all traits.
    envSpecificEffects = NULL,
    #' @field residCor [matrix] Residual correlation between traits
    residCor = NULL,
    #' @field seedResid [numeric] Random seed for selecting residuals
    seedResid = NULL,
    #' @field trueGeneticVar [numeric] True genetic variance
    trueGeneticVar = NULL,
    #' @field trueGeneticCov [matrix] True genetic covariance between traits
    trueGeneticCov = NULL,
    #' @field trueGeneticCor [matrix] True genetic correlation between traits
    trueGeneticCor = NULL,
    #' @field residVar [numeric] Residual variance for each trait
    residVar = NULL,
    #' @field residCov [matrix] Residual covariance between traits
    residCov = NULL,
    #' @field resid [array] individual x traits x replication (3-dimensional array) of residuals
    resid = NULL,
    #' @field trueResidVar [numeric] True residual variance for each trait
    trueResidVar = NULL,
    #' @field trueResidCov [matrix] True residual covariance between traits
    trueResidCov = NULL,
    #' @field trueResidCor [matrix] True residual correlation between traits
    trueResidCor = NULL,
    #' @field trueHeritLine [numeric] True heritability of each trait (plot-based/line-based)
    trueHeritLine = NULL,
    #' @field trueHeritInd [numeric] True heritability of each trait (individual-based)
    trueHeritInd = NULL,


    #' @description Create a new trialInfo object.
    #' @param population [population class] population class object
    #' @param herit [numeric] Heritability for each trait (plot-based/line-based)
    #' @param nRep [numeric] Replication of the field trial (common to all traits)
    #' @param multiTraitsAsEnvs [logical] Treat multiple traits as multiple environments or not
    #' @param envSpecificEffects [numeric] Effects specific to each environments / treatments.
    #' If `multiTraitsAsEnvs = FALSE`, envSpecificEffects will be 0 for all traits.
    #' @param residCor [matrix] Residual correlation between traits
    #' @param seedResid [numeric] Random seed for selecting residuals
    #' @return A new `trialInfo` object.
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
    #
    #' ### create specie information
    #' mySpec <- specie$new(nChr = 3,
    #'                      lChr = c(100, 150, 200),
    #'                      specName = "Example 1",
    #'                      ploidy = 2,
    #'                      mutRate = 10^-8,
    #'                      recombRate = 10^-6,
    #'                      chrNames = c("C1", "C2", "C3"),
    #'                      nLoci = 100,
    #'                      recombRateOneVal = FALSE,
    #'                      effPopSize = 100,
    #'                      simInfo = mySimInfo,
    #'                      verbose = TRUE)
    #
    #' ### create lociInfo object
    #' myLoci <- lociInfo$new(genoMap = NULL, specie = mySpec)
    #' plot(myLoci, alpha = 0.1)
    #
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
    #
    #' ### create simulated population
    #' simulatedPop <- createPop(geno = NULL,
    #'                           haplo = NULL,
    #'                           lociInfo = myLoci,
    #'                           traitInfo = myTrait,
    #'                           founderIsInitPop = TRUE,
    #'                           popName = "First Population",
    #'                           verbose = FALSE)
    #
    #
    #' ### create trialInfo object
    #' myTrial <- trialInfo$new(population = simulatedPop,
    #'                          herit = c(0.4, 0.7, 0.2),
    #'                          nRep = 2,
    #'                          multiTraitsAsEnvs = FALSE,
    #'                          residCor = 0.3)
    #
    #' simulatedPop$inputTrialInfo(trialInfoNow = myTrial)
    #'
    initialize = function(population,
                          herit = NULL,
                          nRep = NULL,
                          multiTraitsAsEnvs = FALSE,
                          envSpecificEffects = NULL,
                          residCor = NULL,
                          seedResid = NA
    ) {

      # CHECKS:
      # parameters classes
      if (class(population)[1] != "population") {
        stop('"class(population)" must be "population"')
      }

      traitInfo <- population$traitInfo

      if (is.null(traitInfo)) {
        stop("You cannot perform field trial if you do not have `traitInfo` onject in `population` object!")
      }


      if (class(traitInfo)[1] != "traitInfo") {
        stop('"class(traitInfo)" must be "traitInfo"')
      }

      if (is.null(nRep)) {
        message('"nRep" was not specified. The nRep had been set to "1"')
        nRep <- 1
      }

      if (!is.numeric(nRep)) stop("nRep must be numeric.")
      if (floor(nRep) - nRep != 0) stop("nRep must be integer.")
      stopifnot(nRep >= 1)

      simPheno <- traitInfo$lociInfo$specie$simInfo$simPheno

      nTraits <- traitInfo$nTraits
      traitNames <- traitInfo$traitNames

      if (simPheno) {
        if (is.null(herit)) {
          message('"herit" was not specified. The herit had been set to "0.5".')
          herit <- rep(0.5, nTraits)
        }

        if (!is.numeric(herit)) stop("herit must be numeric.")
        if (length(herit) != 1 && length(herit) != nTraits) {
          stop(paste("length(herit) must be equal to 1 (all traits have the same",
                     "value) or equal to nTraits."))
        } else if (length(herit) == 1) {
          herit <- rep(herit, nTraits)
        }
        if (any((herit > 1) | (herit < 0))) {
          stop("All elements in `herit` should be within the interval [0, 1].")
        }

        if (any(herit < 1e-05)) {
          message("`herit` should be more than 1e-05. `herit` will be reset to 1e-05.")
          herit[herit < 1e-05] <- 1e-05
        }


        if (nTraits >= 2) {
          if (is.null(residCor)) {
            message('"residCor" was not specified. The residCor had been set to "0".')
            residCor <- 0
          }
          if (!is.numeric(residCor)) stop("residCor must be numeric.")

          if (is.vector(residCor)) {
            if (length(residCor) != 1) {
              stop("`residCor` should be scalar or 2-dimensional matrix with `nTraits x nTraits` dimension.")
            }
            residCorMat <- diag(nTraits)
            residCorMat[upper.tri(x = residCorMat)] <- residCor
            residCorMat[lower.tri(x = residCorMat)] <- residCor
            residCor <- residCorMat
          } else if (is.matrix(residCor)) {
            if (length(dim(residCor)) != 2) {
              stop("`residCor` should be scalar or 2-dimensional matrix with `nTraits x nTraits` dimension.")
            } else {
              if (!all(dim(residCor) == c(nTraits, nTraits))) {
                stop("`residCor` should be scalar or 2-dimensional matrix with `nTraits x nTraits` dimension.")
              }
              stopifnot(all(diag(residCor) == 1))
            }
          } else {
            stop("`residCor` should be scalar or 2-dimensional matrix with `nTraits x nTraits` dimension.")
          }

          if (any((residCor > 1) | (residCor < -1))) {
            stop("All elements in `residCor` should be within the interval [-1, 1].")
          }

          rownames(residCor) <- colnames(residCor) <- traitNames


          if (multiTraitsAsEnvs) {
            if (is.null(envSpecificEffects)) {
              message('"envSpecificEffects" was not specified. The envSpecificEffects had been set to "0".')
              envSpecificEffects <- rep(0, nTraits)
            }

            if (!is.numeric(envSpecificEffects)) stop("envSpecificEffects must be numeric.")
            if (length(envSpecificEffects) != 1 && length(envSpecificEffects) != nTraits) {
              stop(paste("length(envSpecificEffects) must be equal to 1 (all traits have the same",
                         "value) or equal to nTraits."))
            } else if (length(envSpecificEffects) == 1) {
              envSpecificEffects <- rep(envSpecificEffects, nTraits)
            }
          } else {
            envSpecificEffects <- rep(0, nTraits)
          }

        } else {
          envSpecificEffects <- 0
          residCor <- NULL
          multiTraitsAsEnvs <- FALSE
        }

        if (!is.null(seedResid)) {
          if (!is.na(seedResid)) {
            stopifnot(is.numeric(seedResid))
            seedResid <- floor(seedResid)
          } else {
            seedResid <- sample(x = 1e9, size = 1)
          }
        }

        names(herit) <- names(envSpecificEffects) <- traitNames

        trueGeneticVar <- apply(population$trueGVMat, 2, var)
        trueGeneticCov <- cov(population$trueGVMat)
        trueGeneticCor <- cor(population$trueGVMat)

        residVar <- trueGeneticVar * (1 - herit) / herit * nRep

        if (nTraits >= 2) {
          residCov <- diag(sqrt(residVar)) %*% residCor %*% diag(sqrt(residVar))
          rownames(residCov) <- colnames(residCov) <- traitNames

          set.seed(seed = seedResid)
          resid <- array(replicate(n = nRep,
                                   expr = MASS::mvrnorm(n = population$nInd,
                                                        mu = rep(0, nTraits),
                                                        Sigma = residCov)),
                         dim = c(population$nInd, nTraits, nRep),
                         dimnames = list(indNames = names(population$inds),
                                         traitNames = traitNames,
                                         repNames = .charSeq(paste0("Rep_"), seq(nRep))))
        } else {
          residCov <- residVar
          resid <- array(replicate(n = nRep,
                                   expr = rnorm(n = population$nInd,
                                                mean = 0, sd = sqrt(residCov))),
                         dim = c(population$nInd, nTraits, nRep),
                         dimnames = list(indNames = names(population$inds),
                                         traitNames = traitNames,
                                         repNames = .charSeq(paste0("Rep_"), seq(nRep))))
        }

        resid2 <- aperm(resid, perm = c(1, 3, 2))
        resid3 <- array(resid2, dim = c(population$nInd * nRep, nTraits))
        trueResidVar <- apply(resid3, 2, var)
        trueResidCov <- cov(resid3)
        trueResidCor <- cor(resid3)

        names(trueResidVar) <- rownames(trueResidCov) <-
          colnames(trueResidCov) <- rownames(trueResidCor) <-
          colnames(trueResidCor) <- traitNames

        trueHeritLine <- trueGeneticVar / (trueGeneticVar + trueResidVar / nRep)
        trueHeritInd <- trueGeneticVar / (trueGeneticVar + trueResidVar)
      } else {
        herit <- envSpecificEffects <- residCor <-
          trueGeneticVar <- trueGeneticCov <-
          trueGeneticCor <- residVar <-
          residCov <- resid <- trueResidVar <-
          trueResidCov <- trueResidCor <-
          trueHeritLine <- trueHeritInd <- seedResid <- NULL
      }


      self$population <- population
      self$herit <- herit
      self$nRep <- nRep
      self$multiTraitsAsEnvs <- multiTraitsAsEnvs
      self$envSpecificEffects <- envSpecificEffects
      self$residCor <- residCor
      self$seedResid <- seedResid
      self$trueGeneticVar <- trueGeneticVar
      self$trueGeneticCov <- trueGeneticCov
      self$trueGeneticCor <- trueGeneticCor
      self$residVar <- residVar
      self$residCov <- residCov
      self$resid <- resid
      self$trueResidVar <- trueResidVar
      self$trueResidCov <- trueResidCov
      self$trueResidCor <- trueResidCor
      self$trueHeritLine <- trueHeritLine
      self$trueHeritInd <- trueHeritInd
    },


    #' @description Display summary information about the object: traitInfo
    print = function() {
        cat(paste0("Number of replication: ", self$nRep, "\n",
                   "Treat multiple traits as environments: ", self$multiTraitsAsEnvs, "\n",
                   "Heritability you set: \n"))
        print(self$herit)

        cat("Environmental specific effects: \n")
        print(self$envSpecificEffects)
        cat("Residual correlation you set: \n")
        print(self$residCor)
        cat("-------------------------------------\n")
        cat("True plot(line)-based heritability : \n")
        print(round(self$trueHeritLine, 4))
        cat("\n")
        cat("True individual-based heritability : \n")
        print(round(self$trueHeritInd, 4))
    }
  )
)
