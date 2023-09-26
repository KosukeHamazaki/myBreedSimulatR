# Author: Kosuke Hamazaki hamazaki@ut-biomet.org
# 2020 The University of Tokyo
#
# Description:
# Definition of breederInfo class




#' R6 Class Representing a Breeder
#'
#' @description
#' breederInfo object store specific information of one breeder.
#'
# @details
# Details: breederInfo object store specific information of one breeder.
#'
#' @export
#' @import R6
breederInfo <- R6::R6Class(
  "breederInfo",
  public = list(
    #' @field breederName [character] name of this breeder
    breederName = "Undefined",
    #' @field simInfo [simInfo class] Simulation information
    #' (see:\link[myBreedSimulatR]{simInfo})
    simInfo = NULL,
    #' @field specie [specie class] Specie of the SNPs
    #'   (see:\link[myBreedSimulatR]{specie})
    specie = NULL,
    #' @field lociInfoFB [list] information about the individuals haplotypes' SNPs available for breeder
    lociInfoFB = NULL,
    #' @field traitInfoFB [list] Specific information of traits available for breeder
    traitInfoFB = NULL,
    #' @field popNameBase [character] base of population's name.
    popNameBase = NULL,
    #' @field populationsFB [list] A list of populationFb objects available for breeders
    populationsFB = NULL,
    #' @field crossInfoList [list] A list of crossInfo objects
    crossInfoList = NULL,
    #' @field generation [list] current generation No. in the breeder
    generation = NULL,
    #' @field calculateGRMBase [logical] calculate genomic relationship matrix (GRM) for each population or not
    calculateGRMBase = NULL,
    #' @field methodsGRMBase [character] default methods to calculate GRM
    methodsGRMBase = NULL,
    #' @field calcEpistasisBase [logical] when additive / dominance GRM has already been calulated,
    #'  whether or not calculate epistatic GRM
    calcEpistasisBase = NULL,
    #' @field estimatedGVByGRMInfo [list] A list of information on estimated GVs using GRM
    estimatedGVByGRMInfo = NULL,
    #' @field estimatedMrkEffInfo [list] A list of information on estimated marker effects
    estimatedMrkEffInfo = NULL,
    #' @field estimatedGVByMLRInfo [list] A list of information on estimated GVs using MLR (multiple linear regression)
    estimatedGVByMLRInfo = NULL,
    #' @field multiTraitsAsEnvs [logical] Treat multiple traits as multiple environments or not
    multiTraitsAsEnvs = NULL,
    #' @field includeIntercept [logical] Include intercept information when estimating genotypic values by replication
    includeIntercept = NULL,
    #' @field verbose [boolean] display information
    verbose = NULL,

    #' @description Create a new breederInfo object.
    #' @param breederName [character] name of this breeder
    #' @param bsInfo [bsInfo class] breeding scheme info (whichever generation is OK,
    #' but it will use only 1st population)
    #'   (see:\link[myBreedSimulatR]{bsInfo})
    #' @param mrkNames [character] marker names
    #' @param initGenotyping [logical] obtain marker genotype for initial population or not
    #' @param initGenotypedIndNames [character] individual names that you want to genotype in initial population
    #' @param mafThres [numeric] threshold for removing markers with maf < mafThres
    #' @param heteroThres [numeric] threshold for removing markers with heteroRate >= heteroThres
    #' @param calculateGRMBase [logical] calculate genomic relationship matrix (GRM) for each population or not
    #' @param methodsGRMBase [character] default methods to calculate GRM
    #' @param calcEpistasisBase [logical] when additive / dominance GRM has already been calulated,
    #'  whether or not calculate epistatic GRM
    #' @param multiTraitsAsEnvs [logical] Treat multiple traits as multiple environments or not
    #' @param includeIntercept [logical] Include intercept information when estimating genotypic values by replication
    #' @param verbose [logical] Display info (optional)
    #' @return A new `breederInfo` object.
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


    initialize = function(breederName = "Undefined",
                          bsInfo,
                          mrkNames = NULL,
                          initGenotyping = TRUE,
                          initGenotypedIndNames = NULL,
                          mafThres = 0.05,
                          heteroThres = 1,
                          calculateGRMBase = TRUE,
                          methodsGRMBase = "addNOIA",
                          calcEpistasisBase = FALSE,
                          multiTraitsAsEnvs = FALSE,
                          includeIntercept = TRUE,
                          verbose = TRUE) {

      # bsInfo class
      if (class(bsInfo)[1] != "bsInfo") {
        stop(paste('class(bsInfo)[1] != "bsInfo"\n"bsInfo" must be a',
                   'bsInfo object see: ?bsInfo'))
      }

      simInfo <- bsInfo$simInfo
      specie <- bsInfo$specie

      lociInfo <- bsInfo$lociInfo
      genoMapAll <- lociInfo$genoMap

      traitInfo <- bsInfo$traitInfo
      nMarkers <- traitInfo$nMarkers
      nTraits <- traitInfo$nTraits
      traitNames <- traitInfo$traitNames
      traitInfoFB <- list(nTraits = nTraits,
                          traitNames = traitNames)

      genoMapFB <- genoMapAll[traitInfo$mrkPos, ]

      if (is.null(mrkNames)) {
        mrkNames <- paste0("Marker_", 1:sum(nMarkers))
      } else {
        stopifnot(length(mrkNames) == sum(nMarkers))
      }

      genoMapFB[, 1] <- mrkNames
      colnames(genoMapFB)[1] <- "mrkNames"
      lociInfoFB <- list(genoMapFB = genoMapFB,
                         mrkNames = mrkNames,
                         nMarkers = nMarkers,
                         mafThres = mafThres,
                         heteroThres = heteroThres)

      initPopulationFB <- self$populationFB$new(
        population = bsInfo$populations[[1]],
        genotyping = initGenotyping,
        genotypedIndNames = initGenotypedIndNames,
        mrkNames = mrkNames,
        mafThres = mafThres,
        heteroThres = heteroThres,
        calculateGRM = calculateGRMBase,
        methodsGRM = methodsGRMBase,
        calcEpistasis = calcEpistasisBase
      )

      populationsFB <- list()
      populationsFB[[initPopulationFB$name]] <- initPopulationFB

      popNameBase <- (stringr::str_split(string = initPopulationFB$name,
                                         pattern = "_")[[1]])[1]

      if (traitInfoFB$nTraits == 1) {
        multiTraitsAsEnvs <- FALSE
      }

      self$breederName <- breederName
      self$simInfo <- simInfo
      self$specie <- specie
      self$lociInfoFB <- lociInfoFB
      self$traitInfoFB <- traitInfoFB
      self$popNameBase <- popNameBase
      self$populationsFB <- populationsFB
      self$crossInfoList <- list()
      self$generation <- 1
      self$calculateGRMBase <- calculateGRMBase
      self$methodsGRMBase <- methodsGRMBase
      self$calcEpistasisBase <- calcEpistasisBase
      self$estimatedGVByGRMInfo <- list()
      self$multiTraitsAsEnvs <- multiTraitsAsEnvs
      self$includeIntercept <- includeIntercept
      self$verbose <- verbose
    },

    #' @description
    #' get information on new population
    #' @param bsInfo [bsInfo class]  a bsInfo class object
    #' @param generationNew [numeric] a generation of new population
    #' @param genotyping [logical] Whether or not you want to genotype
    #' @param genotypedIndNames [character] individual names that you want to genotype
    getNewPopulation = function(bsInfo,
                                generationNew = NULL,
                                genotyping = TRUE,
                                genotypedIndNames = NULL) {
      populationsFB <- self$populationsFB
      crossInfoList <- self$crossInfoList
      populations <- bsInfo$populations
      generation <- self$generation
      if (is.null(generationNew)) {
        generationNew <- generation + 1
      } else {
        stopifnot(generationNew >= 1)
      }

      if (bsInfo$generation < generationNew) {
        stop("bsInfo object doesn't contain the information on new population!")
      } else if (bsInfo$generation > generationNew) {
        message("New population for breeder is not the latest population in bsInfo. OK?")
      }

      newPopulation <- bsInfo$populations[[which(unlist(lapply(populations, function(pop) pop$generation)) %in% generationNew)]]
      newPopulationFB <- self$populationFB$new(
        population = newPopulation,
        genotyping = genotyping,
        genotypedIndNames = genotypedIndNames,
        mrkNames = self$lociInfoFB$mrkNames,
        mafThres = self$lociInfoFB$mafThres,
        heteroThres = self$lociInfoFB$heteroThres,
        calculateGRM = self$calculateGRMBase,
        methodsGRM = self$methodsGRMBase,
        calcEpistasis = self$calcEpistasisBase
      )

      crossInfoName <- paste0(generationNew - 1, "_to_", generationNew)
      crossInfoList[[crossInfoName]] <- newPopulation$crossInfo
      populationsFB[[newPopulationFB$name]] <- newPopulationFB

      self$generation <- generationNew
      self$populationsFB <- populationsFB
      self$crossInfoList <- crossInfoList
    },


    #' @description
    #' get phenotypic values on current population
    #' @param bsInfo [bsInfo class]  a bsInfo class object
    #' @param generationOfInterest a generation where you want to obtain phenotypic values from population
    #' @param nRep [numeric] Replication of the field trial (common to all traits)
    #' @param multiTraitsAsEnvs [logical] Treat multiple traits as multiple environments or not
    #' @param phenotypedIndNames [character] individual names that you want to phenotype
    #' @param estimateGV [logical] estimate genotypic values by replication or not
    #' @param estimatedGVMethod [character] We offer 'lme4' and 'mean'. 'lme4' is recommended.
    #'
    phenotyper = function(bsInfo,
                          generationOfInterest = NULL,
                          nRep = 1,
                          phenotypedIndNames = NULL,
                          estimateGV = TRUE,
                          estimatedGVMethod = "lme4") {
      populationsFB <- self$populationsFB
      populations <- bsInfo$populations

      if (is.null(generationOfInterest)) {
        generationOfInterest <- self$generation
      }

      if (is.null(nRep)) {
        nRep <- 1
      }
      stopifnot(is.numeric(nRep))
      stopifnot(nRep >= 1)


      maxGenFB <- max(sapply(X = populationsFB,
                             FUN = function(populationFB) {
                               populationFB$generation
                             },
                             simplify = TRUE))
      maxGen <- max(sapply(X = populations,
                           FUN = function(population) {
                             population$generation
                           },
                           simplify = TRUE))
      if (maxGenFB < generationOfInterest) {
        stop(paste0("You haven't obtained the information of population of interest yet!! ",
                    "Please perform `self$getNewPopulation` first!!"))
      } else if (maxGenFB > generationOfInterest) {
        message("Population of your interest is not the latest population in breederInfo. OK?")
      }

      if (maxGen < generationOfInterest) {
        stop("bsInfo object doesn't contain the information on population of your interest!")
      } else if (maxGen > generationOfInterest) {
        message("Population of your interest is not the latest population in bsInfo. OK?")
      }


      currentPopulationFB <- populationsFB[[generationOfInterest]]
      currentPopulation <- populations[[generationOfInterest]]

      currentTrialInfo <- trialInfo$new(population = currentPopulation,
                                        herit = bsInfo$herit,
                                        nRep = nRep,
                                        multiTraitsAsEnvs = self$multiTraitsAsEnvs,
                                        envSpecificEffects = bsInfo$envSpecificEffects,
                                        residCor = bsInfo$residCor)

      currentPopulationFB$phenotyper(trialInfo = currentTrialInfo,
                                     estimateGV = estimateGV,
                                     estimatedGVMethod = estimatedGVMethod,
                                     includeIntercept = self$includeIntercept,
                                     phenotypedIndNames = phenotypedIndNames)
      populationsFB[[currentPopulationFB$name]] <- currentPopulationFB

      self$populationsFB <- populationsFB
    },



    #' @description
    #' estimate genotypic values based on GBLUP
    #' @param trainingPop [character / numeric] training population names or No.s (not generations!!)
    #' @param testingPop [character / numeric] testing population names or No.s (not generations!!)
    #' @param testingIndNames [character] names of testing individuals
    #' @param methodsGRMFP [character] methods for calculating GRM for prediction
    #' @param bayesian [logical] use bayesian model (BGLR) or not (RAINBOWR) for solving mixed-effects model
    #' @param multiTrait [logical] use multiple-trait model for estimation of genotypic values
    #' @param nIter [numeric] the number of iterations
    #' @param burnIn [numeric] the number of burn-in
    #' @param thin [numeric] the number of thinning
    #'
    estimateGVByGRM = function(trainingPop = NULL,
                               testingPop = NULL,
                               testingIndNames = NULL,
                               methodsGRMFP = "addNOIA",
                               bayesian = FALSE,
                               multiTrait = FALSE,
                               nIter = 10000,
                               burnIn = 2000,
                               thin = 5) {
      populationsFB <- self$populationsFB
      generation <- self$generation
      estimatedGVByGRMInfo <- self$estimatedGVByGRMInfo
      nTraits <- self$traitInfoFB$nTraits
      if (nTraits == 1) {
        multiTrait <- FALSE
      }

      supportedMethodsGRMFP <- c("addNOIA", "domNOIA", "A.mat", "linear",
                                 "gaussian", "exponential", "correlation",
                                 "axaNOIA", "axdNOIA", "dxdNOIA")
      epistasisMethodsGRM <- c("axaNOIA", "axdNOIA", "dxdNOIA")
      methodsGRMFP <- methodsGRMFP[methodsGRMFP %in% supportedMethodsGRMFP]
      stopifnot(length(methodsGRMFP) >= 1)

      allPop <- 1:length(populationsFB)
      allPopNo <- unlist(lapply(populationsFB,
                                function(popFB) popFB$generation))
      allPopName <- names(populationsFB)
      allIndNames <- unlist(lapply(populationsFB,
                                   function(popFB) popFB$indNames))

      if (is.null(trainingPop)) {
        trainingPop <- allPop
        trainingPopNo <- allPopNo
        trainingPopName <- allPopName
      } else if (is.numeric(trainingPop)) {
        trainingPop <- trainingPop[trainingPop %in% allPop]
      } else if (is.character(trainingPop)) {
        trainingPop <- trainingPop[trainingPop %in% allPopName]
      } else {
        stop("A class of `trainingPop` should be numeric or character!!")
      }

      trainingPopulationsFB <- populationsFB[trainingPop]
      trainingPopName <- names(trainingPopulationsFB)
      trainingPopNo <- unlist(lapply(trainingPopulationsFB,
                                     function(popFB) popFB$generation))
      trainingIndNames <- unlist(lapply(trainingPopulationsFB,
                                        function(popFB) popFB$indNames))

      if (is.null(testingIndNames)) {
        if (is.null(testingPop)) {
          testingPop <- allPop
          testingPopNo <- allPopNo
          testingPopName <- allPopName
        } else if (is.numeric(testingPop)) {
          testingPop <- testingPop[testingPop %in% allPop]
        } else if (is.character(testingPop)) {
          testingPop <- testingPop[testingPop %in% allPopName]
        } else {
          stop("A class of `testingPop` should be numeric or character!!")
        }

        testingPopulationsFB <- populationsFB[testingPop]
        testingPopName <- names(testingPopulationsFB)
        testingPopNo <- unlist(lapply(testingPopulationsFB,
                                      function(popFB) popFB$generation))

        testingIndNames <- unlist(lapply(testingPopulationsFB,
                                         function(popFB) popFB$indNames))
      } else {
        stopifnot(is.character(testingIndNames))
        testingIndNames <- testingIndNames[testingIndNames %in% allIndNames]

        testingPop <- sapply(testingIndNames, function(indName) {
          whichPop <- which(!is.na(unlist(lapply(populationsFB, function(pop) {
            charmatch(x = indName,
                      table = pop$indNames)
          }))))

          return(whichPop)
        })

        testingPopulationsFB <- populationsFB[testingPop]
        testingPopName <- names(testingPopulationsFB)
        testingPopNo <- unlist(lapply(testingPopulationsFB,
                                      function(popFB) popFB$generation))

        if (is.character(trainingPop)) {
          testingPop <- testingPopName
        }
      }


      totalPop <- unique(c(trainingPop, testingPop))
      totalPopulationsFB <- populationsFB[totalPop]
      totalPopName <- names(totalPopulationsFB)
      totalPopNo <- unlist(lapply(totalPopulationsFB,
                                  function(popFB) popFB$generation))
      totalIndNames <- unique(c(trainingIndNames, testingIndNames))
      nTotalInds <- length(totalIndNames)

      if (length(totalPop) >= 2) {
        totalPopOG <- self$overGeneration(targetPop = totalPop)
      } else if (length(totalPop) == 1) {
        totalPopOG <- totalPopulationsFB[[1]]
      }

      trainingIndNamesWithPheno <- trainingIndNames[trainingIndNames %in% rownames(totalPopOG$estimatedGVByRep)]
      testingIndNamesWithPheno <- testingIndNames[testingIndNames %in% rownames(totalPopOG$estimatedGVByRep)]

      trainingEstimatedGVByRep <- totalPopOG$estimatedGVByRep[trainingIndNamesWithPheno, , drop = FALSE]
      totalEstimatedGVByRep <- matrix(data = NA,
                                      nrow = nTotalInds,
                                      ncol = ncol(trainingEstimatedGVByRep),
                                      dimnames = list(totalIndNames,
                                                      colnames(trainingEstimatedGVByRep)))
      totalEstimatedGVByRep[trainingIndNamesWithPheno, ] <- trainingEstimatedGVByRep
      totalEstimatedGVByRepForPlt <- totalEstimatedGVByRep

      if (length(testingIndNamesWithPheno) >= 1) {
        testingEstimatedGVByRep <- totalPopOG$estimatedGVByRep[testingIndNamesWithPheno, , drop = FALSE]
        totalEstimatedGVByRepForPlt[testingIndNamesWithPheno, ] <- testingEstimatedGVByRep
      }

      calcEpistasis <- any(methodsGRMFP %in% epistasisMethodsGRM)
      methodsGRMFPAdd <- methodsGRMFP
      if (any(methodsGRMFP %in% c("axaNOIA", "axdNOIA"))) {
        methodsGRMFPAdd <- unique(c(methodsGRMFP, "addNOIA"))
      }
      if (any(methodsGRMFP %in% c("axdNOIA", "dxdNOIA"))) {
        methodsGRMFPAdd <- unique(c(methodsGRMFP, "domNOIA"))
      }

      totalPopOG$calcGRMs(methodsGRM = methodsGRMFPAdd,
                          overWrite = FALSE,
                          calcEpistasis = calcEpistasis)

      if (!bayesian) {
        if (multiTrait) {
          message(paste0("Multi-trait model for frequentist is not available now...\n",
                         "We use `multiTrait = FALSE` instead."))
          multiTrait <- FALSE
        }

        ZETA <- list()
        for (methodGRMFP in methodsGRMFP) {
          totalGRM <- (totalPopOG$GRMs[[methodGRMFP]])[totalIndNames, totalIndNames]
          designMat <- RAINBOWR::design.Z(pheno.labels = rownames(totalEstimatedGVByRep),
                                          geno.names = rownames(totalGRM))
          ZETA[[methodGRMFP]] <- list(Z = designMat,
                                      K = totalGRM)
        }
        ETA <- NULL

        if (self$verbose) {
          EMMResList <- pbapply::pbsapply(X = colnames(totalEstimatedGVByRep),
                                          FUN = function(traitName) {
                                            EMMRes <- EM3.cpp(y = totalEstimatedGVByRep[, traitName],
                                                              X0 = NULL,
                                                              ZETA = ZETA)
                                          }, simplify = FALSE)
        } else {
          EMMResList <- sapply(X = colnames(totalEstimatedGVByRep),
                               FUN = function(traitName) {
                                 EMMRes <- EM3.cpp(y = totalEstimatedGVByRep[, traitName],
                                                   X0 = NULL,
                                                   ZETA = ZETA)
                               }, simplify = FALSE)
        }

        totalEstimatedGVByGRM <- do.call(
          what = cbind,
          args = lapply(X = EMMResList,
                        FUN = function(EMMRes) EMMRes$y.pred)
        )

      } else {

        ETA <- list()
        for (methodGRMFP in methodsGRMFP) {
          totalGRM <- (totalPopOG$GRMs[[methodGRMFP]])[totalIndNames, totalIndNames]
          designMat <- RAINBOWR::design.Z(pheno.labels = rownames(totalEstimatedGVByRep),
                                          geno.names = rownames(totalGRM))
          totalGRM <- tcrossprod(designMat %*% totalGRM, designMat)
          ETA[[methodGRMFP]] <- list(K = totalGRM,
                                     model = "RKHS")
        }
        ZETA <- NULL

        if (!multiTrait) {
          if (self$verbose) {
            EMMResList <- pbapply::pbsapply(X = colnames(totalEstimatedGVByRep),
                                            FUN = function(traitName) {
                                              BGLRRes <- BGLR::BGLR(y = totalEstimatedGVByRep[, traitName],
                                                                    response_type = "gaussian", ETA = ETA,
                                                                    nIter = nIter, burnIn = burnIn, thin = thin,
                                                                    saveAt = "", verbose = FALSE,
                                                                    rmExistingFiles = TRUE)
                                              listFiles <- list.files()
                                              listFilesRemove <- listFiles[grep(pattern = "*.dat", x = listFiles)]
                                              listFilesRemoveNoDat <- stringr::str_remove_all(string = listFilesRemove,
                                                                                              pattern = ".dat")
                                              traceInfo <- list()
                                              for (listFileNo in 1:length(listFilesRemove)) {
                                                datNow <- read.csv(file = listFilesRemove[listFileNo],
                                                                   header = FALSE)
                                                traceInfo[[listFilesRemoveNoDat[listFileNo]]] <- datNow
                                              }
                                              BGLRRes$traceInfo <- traceInfo
                                              file.remove(listFilesRemove)

                                              return(BGLRRes)
                                            }, simplify = FALSE)
          } else {
            EMMResList <- sapply(X = colnames(totalEstimatedGVByRep),
                                 FUN = function(traitName) {
                                   BGLRRes <- BGLR::BGLR(y = totalEstimatedGVByRep[, traitName],
                                                         response_type = "gaussian", ETA = ETA,
                                                         nIter = nIter, burnIn = burnIn, thin = thin,
                                                         saveAt = "", verbose = FALSE,
                                                         rmExistingFiles = TRUE)
                                   listFiles <- list.files()
                                   listFilesRemove <- listFiles[grep(pattern = "*.dat", x = listFiles)]
                                   listFilesRemoveNoDat <- stringr::str_remove_all(string = listFilesRemove,
                                                                                   pattern = ".dat")
                                   traceInfo <- list()
                                   for (listFileNo in 1:length(listFilesRemove)) {
                                     datNow <- read.table(file = listFilesRemove[listFileNo],
                                                          header = FALSE, sep = " ")
                                     traceInfo[[listFilesRemoveNoDat[listFileNo]]] <- datNow
                                   }
                                   BGLRRes$traceInfo <- traceInfo
                                   file.remove(listFilesRemove)

                                   return(BGLRRes)
                                 }, simplify = FALSE)
          }
          totalEstimatedGVByGRM <- do.call(what = cbind,
                                           args = lapply(X = EMMResList,
                                                         FUN = function(BGLRRes) {
                                                           totalEstimatedGVByGRMET <- BGLRRes$yHat

                                                           return(totalEstimatedGVByGRMET)
                                                         }))
        } else {
          EMMResList <- BGLR::Multitrait(y = totalEstimatedGVByRep,
                                         ETA = ETA, nIter = nIter,
                                         burnIn = burnIn, thin = thin,
                                         saveAt = "", verbose = FALSE)
          listFiles <- list.files()
          listFilesRemove <- listFiles[grep(pattern = "*.dat", x = listFiles)]
          listFilesRemoveNoDat <- stringr::str_remove_all(string = listFilesRemove,
                                                          pattern = ".dat")
          traceInfo <- list()
          for (listFileNo in 1:length(listFilesRemove)) {
            datNow <- read.table(file = listFilesRemove[listFileNo],
                                 header = FALSE, sep = " ")
            traceInfo[[listFilesRemoveNoDat[listFileNo]]] <- datNow
          }
          EMMResList$traceInfo <- traceInfo
          file.remove(listFilesRemove)

          totalEstimatedGVByGRM <- EMMResList$ETAHat
        }
      }

      rownames(totalEstimatedGVByGRM) <- totalIndNames
      colnames(totalEstimatedGVByGRM) <- colnames(trainingEstimatedGVByRep)
      testingEstimatedGVByGRM <- totalEstimatedGVByGRM[testingIndNames, , drop = FALSE]


      whichPops <- sapply(totalIndNames, function(indName) {
        whichPop <- which(!is.na(unlist(lapply(populationsFB, function(pop) {
          charmatch(x = indName,
                    table = pop$indNames)
        }))))

        return(whichPop)
      }, simplify = FALSE)

      totalIndNamesList <- list()
      for (indNo in 1:length(whichPops)) {
        whichPopList <- whichPops[indNo]
        indName <- names(whichPopList)

        for(popNameNow in names(whichPopList[[1]])) {
          totalIndNamesList[[popNameNow]] <- c(totalIndNamesList[[popNameNow]], indName)
        }
      }

      totalEstimatedGVByGRMList <- lapply(totalIndNamesList,
                                          function(totalIndNamesEach) {
                                            totalEstimatedGVByGRM[totalIndNamesEach, ]
                                          })

      for (totalPopEach in totalPopName) {
        populationsFB[[totalPopEach]]$estimatedGVByGRM <-
          totalEstimatedGVByGRMList[[totalPopEach]]
      }

      totalR2 <- sapply(X = 1:ncol(totalEstimatedGVByRep),
                        FUN = function(x) {
                          R2 <- try(cor(totalEstimatedGVByRepForPlt[, x],
                                        totalEstimatedGVByGRM[, x],
                                        use = "complete.obs") ^ 2)

                          if ("try-error" %in% class(R2)) {
                            R2 <- NA
                          }

                          return(R2)
                        })
      trainingR2 <- sapply(X = 1:ncol(totalEstimatedGVByRep),
                           FUN = function(x) {
                             R2 <- try(cor(totalEstimatedGVByRepForPlt[trainingIndNames, x],
                                           totalEstimatedGVByGRM[trainingIndNames, x],
                                           use = "complete.obs") ^ 2)

                             if ("try-error" %in% class(R2)) {
                               R2 <- NA
                             }

                             return(R2)
                           })
      testingR2 <- sapply(X = 1:ncol(totalEstimatedGVByRep),
                          FUN = function(x) {
                            R2 <- try(cor(totalEstimatedGVByRepForPlt[testingIndNames, x],
                                          totalEstimatedGVByGRM[testingIndNames, x],
                                          use = "complete.obs") ^ 2)

                            if ("try-error" %in% class(R2)) {
                              R2 <- NA
                            }

                            return(R2)
                          })
      R2 <- rbind(total = totalR2,
                  training = trainingR2,
                  testing = testingR2)
      colnames(R2) <- colnames(totalEstimatedGVByRep)

      trainingOrTesting <- rep("training", nTotalInds)
      names(trainingOrTesting) <- totalIndNames
      trainingOrTesting[testingIndNames] <- "testing"
      trainingOrTesting <- factor(trainingOrTesting,
                                  levels = c("training", "testing"))
      pltList <- sapply(X = 1:ncol(totalEstimatedGVByRep),
                        FUN = function(x) {
                          dataForPlot <- data.frame(Observed = totalEstimatedGVByRepForPlt[, x],
                                                    Predicted = totalEstimatedGVByGRM[, x],
                                                    TrainOrTest = trainingOrTesting)
                          pltRange <- range(dataForPlot[, 1:2])
                          plt <- plotly::plot_ly(
                            data = dataForPlot,
                            x = ~ Observed,
                            y = ~ Predicted,
                            split = ~ TrainOrTest,
                            type = "scatter",
                            mode = "markers",
                            hoverinfo = "text",
                            text = apply(data.frame(indNames = rownames(totalEstimatedGVByRepForPlt),
                                                    GVByRep = round(totalEstimatedGVByRepForPlt, 5),
                                                    GVByGRM = round(totalEstimatedGVByGRM, 5)),
                                         MARGIN = 1, FUN = function(l) {
                                           paste(names(l), ":", l, collapse = "\n")
                                         })
                          ) %>%
                            plotly::layout(title = colnames(totalEstimatedGVByRep)[x],
                                           xaxis = list(title = list(text = "Observed"),
                                                        range = pltRange),
                                           yaxis = list(title = list(text = "Predicted"),
                                                        range = pltRange))

                          return(plt)
                        }, simplify = FALSE)
      names(pltList) <- colnames(totalEstimatedGVByRep)

      if (self$multiTraitsAsEnvs) {
        totalEstimatedGVByGRM <- matrix(data = rep(totalEstimatedGVByGRM, self$traitInfoFB$nTraits),
                                        nrow = nrow(totalEstimatedGVByGRM), byrow = FALSE,
                                        dimnames = list(rownames(totalEstimatedGVByGRM),
                                                        self$traitInfoFB$traitNames))
        testingEstimatedGVByGRM <- matrix(data = rep(testingEstimatedGVByGRM, self$traitInfoFB$nTraits),
                                          nrow = nrow(testingEstimatedGVByGRM), byrow = FALSE,
                                          dimnames = list(rownames(testingEstimatedGVByGRM),
                                                          self$traitInfoFB$traitNames))
      }

      estimatedGVByGRMInfoNow <- list(trainingPop = trainingPop,
                                      trainingPopName = trainingPopName,
                                      trainingPopNo = trainingPopNo,
                                      trainingIndNames = trainingIndNames,
                                      testingPop = testingPop,
                                      testingPopName = testingPopName,
                                      testingPopNo = testingPopNo,
                                      testingIndNames = testingIndNames,
                                      totalPop = totalPop,
                                      totalPopName = totalPopName,
                                      totalPopNo = totalPopNo,
                                      totalIndNames = totalIndNames,
                                      methodsGRMFP = methodsGRMFP,
                                      multiTrait = multiTrait,
                                      bayesian = bayesian,
                                      ZETA = ZETA,
                                      ETA = ETA,
                                      EMMResList = EMMResList,
                                      totalEstimatedGVByRep = totalEstimatedGVByRep,
                                      totalEstimatedGVByGRM = totalEstimatedGVByGRM,
                                      testingEstimatedGVByGRM = testingEstimatedGVByGRM,
                                      R2 = R2,
                                      plot = pltList)

      self$populationsFB <- populationsFB
      self$estimatedGVByGRMInfo[[totalPopName[length(totalPopName)]]] <- estimatedGVByGRMInfoNow
    },


    #' @description
    #' estimate marker effects based on multiple linear regression (machine learning)
    #' @param trainingPop [character / numeric] training population names or No.s (not generations!!)
    #' @param trainingIndNames [character] names of training individuals
    #' @param methodMLR [character] methods for estimating marker effects.
    #' The following methods are offered:
    #'
    #' "Ridge", "LASSO", "ElasticNet", "RR-BLUP", "GBLUP", "BayesA" (uni-trait),
    #' "BayesB" (uni-trait), "BayesC" (uni-trait), "BRR", "BL" (uni-trait), "SpikeSlab" (multi-trait)
    #' @param multiTrait [logical] use multiple-trait model for estimation of marker effects or not
    #' @param alpha [numeric] the elastic net mixing parameter, with \eqn{0 \leq \alpha \leq 1}.
    #' The penalty is defined as
    #'
    #' \eqn{\frac {1 - \alpha} { 2 } || \beta || _ 2 ^ 2 + \alpha || \beta || _ 1}
    #'
    #' `alpha = 1` is the lasso penalty, and `alpha = 0` the ridge penalty.
    #' @param nIter [numeric] the number of iterations
    #' @param burnIn [numeric] the number of burn-in
    #' @param thin [numeric] the number of thinning
    #' @param bayesian [logical] use bayesian model (BGLR) or not (RAINBOWR) for solving mixed-effects model
    #' (only when `methodMLR = 'GBLUP'`, this argument is valid.)
    #' @param alphaMarker [numeric] for plot: transparency for markers, see \link[plotly]{plot_ly}
    #' @param sizeMrkMin [numeric] for plot: size for marker with minimum estimated effect
    #' @param sizeMrkMax [numeric] for plot: size for marker with maximum estimated effect
    #'

    estimateMrkEff = function(trainingPop = NULL,
                              trainingIndNames = NULL,
                              methodMLR = "Ridge",
                              multiTrait = FALSE,
                              alpha = 0.5,
                              nIter = 10000,
                              burnIn = 2000,
                              thin = 5,
                              bayesian = FALSE,
                              alphaMarker = 0.5,
                              sizeMrkMin = 2,
                              sizeMrkMax = 12) {
      populationsFB <- self$populationsFB
      generation <- self$generation
      nTraits <- self$traitInfoFB$nTraits
      if (nTraits == 1) {
        multiTrait <- FALSE
      }

      supportedMethodsMLR <- c("Ridge", "LASSO", "ElasticNet", "RR-BLUP", "GBLUP",
                               "BayesA", "BayesB", "BayesC", "BRR", "BL", "SpikeSlab")
      supportedMethodsGlmnet <- c("Ridge", "LASSO", "ElasticNet")
      supportedMethodsBGLR <- c("BayesA", "BayesB", "BayesC", "BRR", "BL", "SpikeSlab")


      methodMLR <- methodMLR[methodMLR %in% supportedMethodsMLR]
      stopifnot(length(methodMLR) >= 1)

      allPop <- 1:length(populationsFB)
      allPopNo <- unlist(lapply(populationsFB,
                                function(popFB) popFB$generation))
      allPopName <- names(populationsFB)
      allIndNames <- unlist(lapply(populationsFB,
                                   function(popFB) popFB$indNames))

      if (is.null(trainingPop)) {
        trainingPop <- allPop
        trainingPopNo <- allPopNo
        trainingPopName <- allPopName
      } else if (is.numeric(trainingPop)) {
        trainingPop <- trainingPop[trainingPop %in% allPop]
      } else if (is.character(trainingPop)) {
        trainingPop <- trainingPop[trainingPop %in% allPopName]
      } else {
        stop("A class of `trainingPop` should be numeric or character!!")
      }

      trainingPopulationsFB <- populationsFB[trainingPop]
      trainingPopName <- names(trainingPopulationsFB)
      trainingPopNo <- unlist(lapply(trainingPopulationsFB,
                                     function(popFB) popFB$generation))
      trainingIndNamesAll <- unlist(lapply(trainingPopulationsFB,
                                           function(popFB) popFB$indNames))
      if (is.null(trainingIndNames)) {
        trainingIndNames <- trainingIndNamesAll
      } else {
        trainingIndNames <- trainingIndNames[trainingIndNames %in% trainingIndNamesAll]
        stopifnot(is.character(trainingIndNames))
        stopifnot(length(trainingIndNames) >= 1)
      }
      nTrainingInds <- length(trainingIndNames)

      if (length(trainingPop) >= 2) {
        trainingPopOG <- self$overGeneration(targetPop = trainingPop)
      } else if (length(trainingPop) == 1) {
        trainingPopOG <- trainingPopulationsFB[[1]]
      }

      trainingIndNamesWithPheno <- trainingIndNames[trainingIndNames %in% rownames(trainingPopOG$estimatedGVByRep)]
      trainingEstimatedGVByRep <- trainingPopOG$estimatedGVByRep[trainingIndNamesWithPheno, , drop = FALSE]

      trainingGenoMat <- trainingPopOG$genoMat[trainingIndNamesWithPheno, ]


      if (methodMLR %in% supportedMethodsGlmnet) {
        if (methodMLR == "Ridge") {
          alpha <- 0
        } else if (methodMLR == "LASSO") {
          alpha <- 1
        } else if (methodMLR == "ElasticNet") {
          if (is.null(alpha)) {
            message("You don't set `alpha`. We will set `alpha = 0.5` for Elastic Net instead.")
            alpha <- 0.5
          }
          stopifnot(is.numeric(alpha))
          stopifnot(alpha > 0)
          stopifnot(alpha < 1)
        }


        if (!multiTrait) {
          if (self$verbose) {
            mrkEstRes <- pbapply::pbsapply(X = colnames(trainingEstimatedGVByRep),
                                           FUN = function(traitName) {
                                             glmnetRes <- glmnet::cv.glmnet(x = trainingGenoMat,
                                                                            y = trainingEstimatedGVByRep[, traitName],
                                                                            family = "gaussian", alpha = alpha,
                                                                            standardize = FALSE,
                                                                            standardize.response = TRUE)

                                             return(glmnetRes)
                                           }, simplify = FALSE)
          } else {
            mrkEstRes <- sapply(X = colnames(trainingEstimatedGVByRep),
                                FUN = function(traitName) {
                                  glmnetRes <- glmnet::cv.glmnet(x = trainingGenoMat,
                                                                 y = trainingEstimatedGVByRep[, traitName],
                                                                 family = "gaussian", alpha = alpha,
                                                                 standardize = FALSE,
                                                                 standardize.response = TRUE)

                                  return(glmnetRes)
                                }, simplify = FALSE)
          }

          mrkEffMat <- do.call(what = cbind,
                               args = lapply(X = mrkEstRes, FUN = function(glmnetRes) {
                                 mrkEffEst <- as.matrix(coef(glmnetRes, s = glmnetRes$lambda.min))

                                 return(mrkEffEst)
                               }))
        } else {
          mrkEstRes <- glmnet::cv.glmnet(x = trainingGenoMat,
                                         y = trainingEstimatedGVByRep,
                                         family = "mgaussian", alpha = alpha,
                                         standardize = FALSE,
                                         standardize.response = TRUE)
          mrkEffList <- coef(mrkEstRes, s = "lambda.min")
          mrkEffMat <- do.call(what = cbind,
                               args = lapply(X = mrkEffList, FUN = function(mrkEffEach) {
                                 mrkEffEst <- as.matrix(mrkEffEach)

                                 return(mrkEffEst)
                               }))
        }

        mrkEffSdMat <- NULL
      } else if (methodMLR %in% supportedMethodsBGLR) {


        if (!multiTrait) {
          if (methodMLR == "SpikeSlab") {
            message(paste0("For uni-trait model, `methodMLR = 'SpikeSlab'` is not offered.\n",
                           "We use `methodMLR = 'BayesC'` instead."))
            methodMLR <- "BayesC"
          }
          ETA <- list(G = list(X = trainingGenoMat, model = methodMLR))
          if (self$verbose) {
            mrkEstRes <- pbapply::pbsapply(X = colnames(trainingEstimatedGVByRep),
                                           FUN = function(traitName) {
                                             BGLRRes <- BGLR::BGLR(y = trainingEstimatedGVByRep[, traitName],
                                                                   response_type = "gaussian", ETA = ETA,
                                                                   nIter = nIter, burnIn = burnIn, thin = thin,
                                                                   saveAt = "", verbose = FALSE,
                                                                   rmExistingFiles = TRUE)
                                             listFiles <- list.files()
                                             listFilesRemove <- listFiles[grep(pattern = "*.dat", x = listFiles)]
                                             listFilesRemoveNoDat <- stringr::str_remove_all(string = listFilesRemove,
                                                                                             pattern = ".dat")
                                             traceInfo <- list()
                                             for (listFileNo in 1:length(listFilesRemove)) {
                                               datNow <- read.csv(file = listFilesRemove[listFileNo],
                                                                  header = FALSE)
                                               traceInfo[[listFilesRemoveNoDat[listFileNo]]] <- datNow
                                             }
                                             BGLRRes$traceInfo <- traceInfo
                                             file.remove(listFilesRemove)

                                             return(BGLRRes)
                                           }, simplify = FALSE)
          } else {
            mrkEstRes <- sapply(X = colnames(trainingEstimatedGVByRep),
                                FUN = function(traitName) {
                                  BGLRRes <- BGLR::BGLR(y = trainingEstimatedGVByRep[, traitName],
                                                        response_type = "gaussian", ETA = ETA,
                                                        nIter = nIter, burnIn = burnIn, thin = thin,
                                                        saveAt = "", verbose = FALSE,
                                                        rmExistingFiles = TRUE)
                                  listFiles <- list.files()
                                  listFilesRemove <- listFiles[grep(pattern = "*.dat", x = listFiles)]
                                  listFilesRemoveNoDat <- stringr::str_remove_all(string = listFilesRemove,
                                                                                  pattern = ".dat")
                                  traceInfo <- list()
                                  for (listFileNo in 1:length(listFilesRemove)) {
                                    datNow <- read.table(file = listFilesRemove[listFileNo],
                                                         header = FALSE, sep = " ")
                                    traceInfo[[listFilesRemoveNoDat[listFileNo]]] <- datNow
                                  }
                                  BGLRRes$traceInfo <- traceInfo
                                  file.remove(listFilesRemove)

                                  return(BGLRRes)
                                }, simplify = FALSE)
          }
          mrkEffMat <- do.call(what = cbind,
                               args = lapply(X = mrkEstRes,
                                             FUN = function(BGLRRes) {
                                               mrkEffEst <- c(Intercept = BGLRRes$mu,
                                                              BGLRRes$ETA$G$b)

                                               return(mrkEffEst)
                                             }))
          mrkEffSdMat <- do.call(what = cbind,
                                 args = lapply(X = mrkEstRes,
                                               FUN = function(BGLRRes) {
                                                 mrkEffSd <- c(Intercept = BGLRRes$SD.mu,
                                                               BGLRRes$ETA$G$SD.b)

                                                 return(mrkEffSd)
                                               }))
        } else {
          if (methodMLR %in% c("BL", "BayesB", "BayesC")) {
            message(paste0("For multi-trait model, `methodMLR = 'BL'`, `methodMLR = 'BayesB'`, `methodMLR = 'BayesC'` are not offered.\n",
                           "We use `methodMLR = 'SpikeSlab'` instead. This is equivalent to BayesC model."))
            methodMLR <- "SpikeSlab"
          } else if (methodMLR == "BayesA") {
            message(paste0("For multi-trait model, `methodMLR = 'BayesA'` is not offered.\n",
                           "We use `methodMLR = 'BRR'` instead."))
            methodMLR <- "BRR"
          }

          ETA <- list(G = list(X = trainingGenoMat, model = methodMLR))
          mrkEstRes <- BGLR::Multitrait(y = trainingEstimatedGVByRep,
                                        ETA = ETA, nIter = nIter,
                                        burnIn = burnIn, thin = thin,
                                        saveAt = "", verbose = FALSE)
          mrkEffMat <- rbind(mrkEstRes$mu,
                             mrkEstRes$ETA$G$beta)
          mrkEffSdMat <- rbind(mrkEstRes$SD.mu,
                               mrkEstRes$ETA$G$SD.beta)
          listFiles <- list.files()
          listFilesRemove <- listFiles[grep(pattern = "*.dat", x = listFiles)]
          listFilesRemoveNoDat <- stringr::str_remove_all(string = listFilesRemove,
                                                          pattern = ".dat")
          traceInfo <- list()
          for (listFileNo in 1:length(listFilesRemove)) {
            datNow <- read.table(file = listFilesRemove[listFileNo],
                                 header = FALSE, sep = " ")
            traceInfo[[listFilesRemoveNoDat[listFileNo]]] <- datNow
          }
          mrkEstRes$traceInfo <- traceInfo
          file.remove(listFilesRemove)
        }



      } else if (methodMLR == "RR-BLUP") {
        Z <- trainingGenoMat
        K <- diag(ncol(Z))
        rownames(K) <- colnames(K) <- colnames(Z)

        if (!multiTrait) {
          ZETA <- list(M = list(Z = Z, K = K))

          if (self$verbose) {
            mrkEstRes <- pbapply::pbsapply(X = colnames(trainingEstimatedGVByRep),
                                           FUN = function(traitName) {
                                             EMMRes <- RAINBOWR::EMM.cpp(y = trainingEstimatedGVByRep[, traitName],
                                                                         X = NULL, ZETA = ZETA, REML = TRUE)

                                             return(EMMRes)
                                           }, simplify = FALSE)
          } else {
            mrkEstRes <- sapply(X = colnames(trainingEstimatedGVByRep),
                                FUN = function(traitName) {
                                  EMMRes <- RAINBOWR::EMM.cpp(y = trainingEstimatedGVByRep[, traitName],
                                                              X = NULL, ZETA = ZETA, REML = TRUE)

                                  return(EMMRes)
                                }, simplify = FALSE)
          }

          mrkEffMat <- do.call(what = cbind,
                               args = lapply(X = mrkEstRes,
                                             FUN = function(EMMRes) {
                                               mrkEffEst <- c(Intercept = EMMRes$beta,
                                                              EMMRes$u)

                                               return(mrkEffEst)
                                             }))
        } else {
          X <- t(matrix(data = 1,
                        nrow = nrow(Z),
                        ncol = 1,
                        dimnames = list(rownames(Z),
                                        "Intercept")))
          mrkEstRes <- EMMREML::emmremlMultivariate(Y = t(trainingEstimatedGVByRep),
                                                    X = X,
                                                    Z = t(Z),
                                                    K = K)

          mrkEffMat <- t(cbind(mrkEstRes$Bhat, mrkEstRes$Gpred))
        }

        mrkEffSdMat <- NULL
      } else if (methodMLR == "GBLUP") {
        K <- tcrossprod(trainingGenoMat) / ncol(trainingGenoMat)
        KInv <- MASS::ginv(K)
        Z <- diag(nrow(K))
        rownames(Z) <- colnames(Z) <- rownames(K)

        if (!bayesian) {
          if (!multiTrait) {
            ZETA <- list(A = list(Z = Z, K = K))

            if (self$verbose) {
              mrkEstRes <- pbapply::pbsapply(X = colnames(trainingEstimatedGVByRep),
                                             FUN = function(traitName) {
                                               EMMRes <- RAINBOWR::EMM.cpp(y = trainingEstimatedGVByRep[, traitName],
                                                                           X = NULL, ZETA = ZETA, REML = TRUE)

                                               return(EMMRes)
                                             }, simplify = FALSE)
            } else {
              mrkEstRes <- sapply(X = colnames(trainingEstimatedGVByRep),
                                  FUN = function(traitName) {
                                    EMMRes <- RAINBOWR::EMM.cpp(y = trainingEstimatedGVByRep[, traitName],
                                                                X = NULL, ZETA = ZETA, REML = TRUE)

                                    return(EMMRes)
                                  }, simplify = FALSE)
            }

            mrkEffMat <- do.call(what = cbind,
                                 args = lapply(X = mrkEstRes,
                                               FUN = function(EMMRes) {
                                                 gvEst <- EMMRes$u
                                                 intercept <- EMMRes$beta
                                                 mrkEffEst <- (crossprod(trainingGenoMat / ncol(trainingGenoMat),
                                                                         KInv) %*% gvEst)[, 1]
                                                 mrkEffEst <- c(Intercept = intercept, mrkEffEst)

                                                 return(mrkEffEst)
                                               }))
          } else {
            X <- t(matrix(data = 1,
                          nrow = nrow(Z),
                          ncol = 1,
                          dimnames = list(rownames(Z),
                                          "Intercept")))
            mrkEstRes <- EMMREML::emmremlMultivariate(Y = t(trainingEstimatedGVByRep),
                                                      X = X,
                                                      Z = t(Z),
                                                      K = K)
            gvEst <- t(mrkEstRes$Gpred)
            intercept <- t(mrkEstRes$Bhat)
            mrkEffMat <- crossprod(trainingGenoMat / ncol(trainingGenoMat),
                                   KInv) %*% gvEst
            mrkEffMat <- rbind(Intercept = intercept, mrkEffEst)
          }
        } else {
          ETA <- list(A = list(K = K, model = "RKHS"))
          if (!multiTrait) {
            if (self$verbose) {
              mrkEstRes <- pbapply::pbsapply(X = colnames(trainingEstimatedGVByRep),
                                             FUN = function(traitName) {
                                               BGLRRes <- BGLR::BGLR(y = trainingEstimatedGVByRep[, traitName],
                                                                     response_type = "gaussian", ETA = ETA,
                                                                     nIter = nIter, burnIn = burnIn, thin = thin,
                                                                     saveAt = "", verbose = FALSE,
                                                                     rmExistingFiles = TRUE)
                                               listFiles <- list.files()
                                               listFilesRemove <- listFiles[grep(pattern = "*.dat", x = listFiles)]
                                               listFilesRemoveNoDat <- stringr::str_remove_all(string = listFilesRemove,
                                                                                               pattern = ".dat")
                                               traceInfo <- list()
                                               for (listFileNo in 1:length(listFilesRemove)) {
                                                 datNow <- read.csv(file = listFilesRemove[listFileNo],
                                                                    header = FALSE)
                                                 traceInfo[[listFilesRemoveNoDat[listFileNo]]] <- datNow
                                               }
                                               BGLRRes$traceInfo <- traceInfo
                                               file.remove(listFilesRemove)

                                               return(BGLRRes)
                                             }, simplify = FALSE)
            } else {
              mrkEstRes <- sapply(X = colnames(trainingEstimatedGVByRep),
                                  FUN = function(traitName) {
                                    BGLRRes <- BGLR::BGLR(y = trainingEstimatedGVByRep[, traitName],
                                                          response_type = "gaussian", ETA = ETA,
                                                          nIter = nIter, burnIn = burnIn, thin = thin,
                                                          saveAt = "", verbose = FALSE,
                                                          rmExistingFiles = TRUE)
                                    listFiles <- list.files()
                                    listFilesRemove <- listFiles[grep(pattern = "*.dat", x = listFiles)]
                                    listFilesRemoveNoDat <- stringr::str_remove_all(string = listFilesRemove,
                                                                                    pattern = ".dat")
                                    traceInfo <- list()
                                    for (listFileNo in 1:length(listFilesRemove)) {
                                      datNow <- read.table(file = listFilesRemove[listFileNo],
                                                           header = FALSE, sep = " ")
                                      traceInfo[[listFilesRemoveNoDat[listFileNo]]] <- datNow
                                    }
                                    BGLRRes$traceInfo <- traceInfo
                                    file.remove(listFilesRemove)

                                    return(BGLRRes)
                                  }, simplify = FALSE)
            }
            mrkEffMat <- do.call(what = cbind,
                                 args = lapply(X = mrkEstRes,
                                               FUN = function(BGLRRes) {
                                                 gvEst <- BGLRRes$ETA$A$u
                                                 intercept <- BGLRRes$mu
                                                 mrkEffEst <- (crossprod(trainingGenoMat / ncol(trainingGenoMat),
                                                                         KInv) %*% gvEst)[, 1]
                                                 mrkEffEst <- c(Intercept = intercept, mrkEffEst)

                                                 return(mrkEffEst)
                                               }))
          } else {
            mrkEstRes <- BGLR::Multitrait(y = trainingEstimatedGVByRep,
                                          ETA = ETA, nIter = nIter,
                                          burnIn = burnIn, thin = thin,
                                          saveAt = "", verbose = FALSE)
            listFiles <- list.files()
            listFilesRemove <- listFiles[grep(pattern = "*.dat", x = listFiles)]
            listFilesRemoveNoDat <- stringr::str_remove_all(string = listFilesRemove,
                                                            pattern = ".dat")
            traceInfo <- list()
            for (listFileNo in 1:length(listFilesRemove)) {
              datNow <- read.table(file = listFilesRemove[listFileNo],
                                   header = FALSE, sep = " ")
              traceInfo[[listFilesRemoveNoDat[listFileNo]]] <- datNow
            }
            mrkEstRes$traceInfo <- traceInfo
            file.remove(listFilesRemove)

            gvEst <- mrkEstRes$ETA$A$u
            intercept <- mrkEstRes$mu
            mrkEffMat <- crossprod(trainingGenoMat / ncol(trainingGenoMat),
                                   KInv) %*% gvEst
            mrkEffMat <- rbind(Intercept = intercept, mrkEffMat)
          }
        }

        mrkEffSdMat <- NULL
      }
      rownames(mrkEffMat) <- c("Intercept", colnames(trainingGenoMat))
      colnames(mrkEffMat) <- colnames(trainingEstimatedGVByRep)

      if (!is.null(mrkEffSdMat)) {
        dimnames(mrkEffSdMat) <- dimnames(mrkEffMat)
      }

      ends <- self$specie$lChr
      genoMap <- self$lociInfoFB$genoMapFB
      genoMap$rec <- round(genoMap$rec, 3)
      mrkEffSize <- apply(X = mrkEffMat[-1, , drop = FALSE], MARGIN = 1,
                          FUN = function(x) sqrt(sum(x ^ 2)))

      mrkEffSizeScaled <- sizeMrkMin + (mrkEffSize - min(mrkEffSize)) *
        (sizeMrkMax - sizeMrkMin) / diff(range(mrkEffSize))
      genoMapWithSize <- data.frame(genoMap, round(mrkEffMat[-1, , drop = FALSE], 3))

      plt <- plotly::plot_ly(data = genoMapWithSize,
                             x = ~ chr,
                             y = ~ pos,
                             type = "scatter",
                             mode = "markers",
                             alpha = alphaMarker,
                             marker = list(size = mrkEffSizeScaled),
                             name = "Markers",
                             hoverinfo = 'text',
                             text = apply(genoMapWithSize, 1, function(l) {
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

      if (self$multiTraitsAsEnvs) {
        mrkEffMat <- matrix(data = rep(mrkEffMat, self$traitInfoFB$nTraits),
                            nrow = nrow(mrkEffMat), byrow = FALSE,
                            dimnames = list(rownames(mrkEffMat),
                                            self$traitInfoFB$traitNames))
      }

      estimatedMrkEffInfoNow <- list(trainingPop = trainingPop,
                                     trainingPopName = trainingPopName,
                                     trainingPopNo = trainingPopNo,
                                     trainingIndNames = trainingIndNames,
                                     methodMLR = methodMLR,
                                     alpha = alpha,
                                     nIter = nIter,
                                     burnIn = burnIn,
                                     thin = thin,
                                     multiTrait = multiTrait,
                                     bayesian = bayesian,
                                     mrkEstRes = mrkEstRes,
                                     mrkEffMat = mrkEffMat,
                                     mrkEffSdMat = mrkEffSdMat,
                                     plot = plt)

      infoName <- paste0(trainingPopName[length(trainingPopName)], "_", methodMLR)
      self$estimatedMrkEffInfo[[infoName]] <- estimatedMrkEffInfoNow
    },



    #' @description
    #' estimate genotypic values based on GBLUP
    #' @param trainingPop [character / numeric] training population names or No.s (not generations!!)
    #' @param trainingIndNames [character] names of training individuals
    #' @param testingPop [character / numeric] testing population names or No.s (not generations!!)
    #' @param testingIndNames [character] names of testing individuals
    #' @param methodMLR [character] methods for estimating marker effects.
    #' The following methods are offered:
    #'
    #' "Ridge", "LASSO", "ElasticNet", "RR-BLUP", "GBLUP", "BayesA" (uni-trait),
    #' "BayesB" (uni-trait), "BayesC" (uni-trait), "BRR", "BL" (uni-trait), "SpikeSlab" (multi-trait)
    #' @param multiTrait [logical] use multiple-trait model for estimation of marker effects or not
    #' @param alpha [numeric] the elastic net mixing parameter, with \eqn{0 \leq \alpha \leq 1}.
    #' The penalty is defined as
    #'
    #' \eqn{\frac {1 - \alpha} { 2 } || \beta || _ 2 ^ 2 + \alpha || \beta || _ 1}
    #'
    #' `alpha = 1` is the lasso penalty, and `alpha = 0` the ridge penalty.
    #' @param nIter [numeric] the number of iterations
    #' @param burnIn [numeric] the number of burn-in
    #' @param thin [numeric] the number of thinning
    #' @param bayesian [logical] use bayesian model (BGLR) or not (RAINBOWR) for solving mixed-effects model
    #' (only when `methodMLR = 'GBLUP'`, this argument is valid.)
    #'
    estimateGVByMLR = function(trainingPop = NULL,
                               trainingIndNames = NULL,
                               testingPop = NULL,
                               testingIndNames = NULL,
                               methodMLR = "Ridge",
                               multiTrait = FALSE,
                               alpha = 0.5,
                               nIter = 10000,
                               burnIn = 2000,
                               thin = 5,
                               bayesian = FALSE) {
      populationsFB <- self$populationsFB
      generation <- self$generation
      nTraits <- self$traitInfoFB$nTraits
      if (nTraits == 1) {
        multiTrait <- FALSE
      }

      supportedMethodsMLR <- c("Ridge", "LASSO", "ElasticNet", "RR-BLUP", "GBLUP",
                               "BayesA", "BayesB", "BayesC", "BRR", "BL", "SpikeSlab")
      supportedMethodsGlmnet <- c("Ridge", "LASSO", "ElasticNet")
      supportedMethodsBGLR <- c("BayesA", "BayesB", "BayesC", "BRR", "BL", "SpikeSlab")

      methodMLR <- methodMLR[methodMLR %in% supportedMethodsMLR]
      stopifnot(length(methodMLR) >= 1)


      if (!multiTrait) {
        if (methodMLR == "SpikeSlab") {
          message(paste0("For uni-trait model, `methodMLR = 'SpikeSlab'` is not offered.\n",
                         "We use `methodMLR = 'BayesC'` instead."))
          methodMLR <- "BayesC"
        }
      } else {
        if (methodMLR %in% c("BL", "BayesB", "BayesC")) {
          message(paste0("For multi-trait model, `methodMLR = 'BL'`, `methodMLR = 'BayesB'`, `methodMLR = 'BayesC'` are not offered.\n",
                         "We use `methodMLR = 'SpikeSlab'` instead. This is equivalent to BayesC model."))
          methodMLR <- "SpikeSlab"
        } else if (methodMLR == "BayesA") {
          message(paste0("For multi-trait model, `methodMLR = 'BayesA'` is not offered.\n",
                         "We use `methodMLR = 'BRR'` instead."))
          methodMLR <- "BRR"
        }
      }

      allPop <- 1:length(populationsFB)
      allPopNo <- unlist(lapply(populationsFB,
                                function(popFB) popFB$generation))
      allPopName <- names(populationsFB)
      allIndNames <- unlist(lapply(populationsFB,
                                   function(popFB) popFB$indNames))

      if (is.null(trainingPop)) {
        trainingPop <- allPop
        trainingPopNo <- allPopNo
        trainingPopName <- allPopName
      } else if (is.numeric(trainingPop)) {
        trainingPop <- trainingPop[trainingPop %in% allPop]
      } else if (is.character(trainingPop)) {
        trainingPop <- trainingPop[trainingPop %in% allPopName]
      } else {
        stop("A class of `trainingPop` should be numeric or character!!")
      }

      trainingPopulationsFB <- populationsFB[trainingPop]
      trainingPopName <- names(trainingPopulationsFB)
      trainingPopNo <- unlist(lapply(trainingPopulationsFB,
                                     function(popFB) popFB$generation))
      trainingIndNamesAll <- unlist(lapply(trainingPopulationsFB,
                                           function(popFB) popFB$indNames))
      if (is.null(trainingIndNames)) {
        trainingIndNames <- trainingIndNamesAll
      } else {
        trainingIndNames <- trainingIndNames[trainingIndNames %in% trainingIndNamesAll]
        stopifnot(is.character(trainingIndNames))
        stopifnot(length(trainingIndNames) >= 1)
      }

      infoName <- paste0(trainingPopName[length(trainingPopName)], "_", methodMLR)

      if (!(infoName %in% names(self$estimatedMrkEffInfo))) {
        self$estimateMrkEff(trainingPop = trainingPop,
                            methodMLR = methodMLR,
                            multiTrait = multiTrait,
                            alpha = alpha, nIter = nIter,
                            burnIn = burnIn, thin = thin,
                            bayesian = bayesian)
      }

      mrkEffMat <- self$estimatedMrkEffInfo[[infoName]]$mrkEffMat

      if (is.null(testingIndNames)) {
        if (is.null(testingPop)) {
          testingPop <- allPop
          testingPopNo <- allPopNo
          testingPopName <- allPopName
        } else if (is.numeric(testingPop)) {
          testingPop <- testingPop[testingPop %in% allPop]
        } else if (is.character(testingPop)) {
          testingPop <- testingPop[testingPop %in% allPopName]
        } else {
          stop("A class of `testingPop` should be numeric or character!!")
        }

        testingPopulationsFB <- populationsFB[testingPop]
        testingPopName <- names(testingPopulationsFB)
        testingPopNo <- unlist(lapply(testingPopulationsFB,
                                      function(popFB) popFB$generation))

        testingIndNames <- unlist(lapply(testingPopulationsFB,
                                         function(popFB) popFB$indNames))
      } else {
        stopifnot(is.character(testingIndNames))
        testingIndNames <- testingIndNames[testingIndNames %in% allIndNames]

        testingPop <- sapply(testingIndNames, function(indName) {
          whichPop <- which(!is.na(unlist(lapply(populationsFB, function(pop) {
            charmatch(x = indName,
                      table = pop$indNames)
          }))))

          return(whichPop)
        })

        testingPopulationsFB <- populationsFB[testingPop]
        testingPopName <- names(testingPopulationsFB)
        testingPopNo <- unlist(lapply(testingPopulationsFB,
                                      function(popFB) popFB$generation))

        if (is.character(trainingPop)) {
          testingPop <- testingPopName
        }
      }


      totalPop <- unique(c(trainingPop, testingPop))
      totalPopulationsFB <- populationsFB[totalPop]
      totalPopName <- names(totalPopulationsFB)
      totalPopNo <- unlist(lapply(totalPopulationsFB,
                                  function(popFB) popFB$generation))
      totalIndNames <- unique(c(trainingIndNames, testingIndNames))
      nTotalInds <- length(totalIndNames)

      if (length(totalPopNo) >= 2) {
        totalPopOG <- self$overGeneration(targetPop = totalPop)
      } else if (length(totalPopNo) == 1) {
        totalPopOG <- totalPopulationsFB[[1]]
      }

      trainingIndNamesWithPheno <- trainingIndNames[trainingIndNames %in% rownames(totalPopOG$estimatedGVByRep)]
      testingIndNamesWithPheno <- testingIndNames[testingIndNames %in% rownames(totalPopOG$estimatedGVByRep)]

      trainingEstimatedGVByRep <- totalPopOG$estimatedGVByRep[trainingIndNamesWithPheno, , drop = FALSE]
      totalEstimatedGVByRep <- matrix(data = NA,
                                      nrow = nTotalInds,
                                      ncol = ncol(trainingEstimatedGVByRep),
                                      dimnames = list(totalIndNames,
                                                      colnames(trainingEstimatedGVByRep)))
      totalEstimatedGVByRep[trainingIndNamesWithPheno, ] <- trainingEstimatedGVByRep
      totalEstimatedGVByRepForPlt <- totalEstimatedGVByRep

      if (length(testingIndNamesWithPheno) >= 1) {
        testingEstimatedGVByRep <- totalPopOG$estimatedGVByRep[testingIndNamesWithPheno, , drop = FALSE]
        totalEstimatedGVByRepForPlt[testingIndNamesWithPheno, ] <- testingEstimatedGVByRep
      }

      totalPopOGGenoMat <- totalPopOG$genoMat
      totalPopOGGenoMat <- totalPopOGGenoMat[rownames(totalEstimatedGVByRepForPlt), ]
      totalEstimatedGVByMLR <- cbind(Intercept = rep(1, nrow(totalPopOGGenoMat)),
                                     totalPopOGGenoMat)[, rownames(mrkEffMat)] %*% mrkEffMat

      testingEstimatedGVByMLR <- totalEstimatedGVByMLR[testingIndNames, , drop = FALSE]


      whichPops <- sapply(totalIndNames, function(indName) {
        whichPop <- which(!is.na(unlist(lapply(populationsFB, function(pop) {
          charmatch(x = indName,
                    table = pop$indNames)
        }))))

        return(whichPop)
      }, simplify = FALSE)

      totalIndNamesList <- list()
      for (indNo in 1:length(whichPops)) {
        whichPopList <- whichPops[indNo]
        indName <- names(whichPopList)

        for(popNameNow in names(whichPopList[[1]])) {
          totalIndNamesList[[popNameNow]] <- c(totalIndNamesList[[popNameNow]], indName)
        }
      }

      totalEstimatedGVByMLRList <- lapply(totalIndNamesList,
                                          function(totalIndNamesEach) {
                                            totalEstimatedGVByMLR[totalIndNamesEach, ]
                                          })

      for (totalPopEach in totalPopName) {
        populationsFB[[totalPopEach]]$estimatedGVByMLR <-
          totalEstimatedGVByMLRList[[totalPopEach]]
      }

      totalR2 <- sapply(X = 1:ncol(totalEstimatedGVByRep),
                        FUN = function(x) {
                          R2 <- try(cor(totalEstimatedGVByRepForPlt[, x],
                                        totalEstimatedGVByMLR[, x],
                                        use = "complete.obs") ^ 2, silent = TRUE)

                          if ("try-error" %in% class(R2)) {
                            R2 <- NA
                          }

                          return(R2)
                        })
      trainingR2 <- sapply(X = 1:ncol(totalEstimatedGVByRep),
                           FUN = function(x) {
                             R2 <- try(cor(totalEstimatedGVByRepForPlt[trainingIndNames, x],
                                           totalEstimatedGVByMLR[trainingIndNames, x],
                                           use = "complete.obs") ^ 2, silent = TRUE)

                             if ("try-error" %in% class(R2)) {
                               R2 <- NA
                             }

                             return(R2)
                           })
      testingR2 <- sapply(X = 1:ncol(totalEstimatedGVByRep),
                          FUN = function(x) {
                            R2 <- try(cor(totalEstimatedGVByRepForPlt[testingIndNames, x],
                                          totalEstimatedGVByMLR[testingIndNames, x],
                                          use = "complete.obs") ^ 2, silent = TRUE)

                            if ("try-error" %in% class(R2)) {
                              R2 <- NA
                            }

                            return(R2)
                          })
      R2 <- rbind(total = totalR2,
                  training = trainingR2,
                  testing = testingR2)
      colnames(R2) <- colnames(totalEstimatedGVByRep)

      trainingOrTesting <- rep("training", nTotalInds)
      names(trainingOrTesting) <- totalIndNames
      trainingOrTesting[testingIndNames] <- "testing"
      trainingOrTesting <- factor(trainingOrTesting,
                                  levels = c("training", "testing"))
      pltList <- sapply(X = 1:ncol(totalEstimatedGVByRep),
                        FUN = function(x) {
                          dataForPlot <- data.frame(Observed = totalEstimatedGVByRepForPlt[, x],
                                                    Predicted = totalEstimatedGVByMLR[, x],
                                                    TrainOrTest = trainingOrTesting)
                          pltRange <- range(dataForPlot[, 1:2])
                          plt <- plotly::plot_ly(
                            data = dataForPlot,
                            x = ~ Observed,
                            y = ~ Predicted,
                            split = ~ TrainOrTest,
                            type = "scatter",
                            mode = "markers",
                            hoverinfo = "text",
                            text = apply(data.frame(indNames = rownames(totalEstimatedGVByRepForPlt),
                                                    GVByRep = round(totalEstimatedGVByRepForPlt, 5),
                                                    GVByMLR = round(totalEstimatedGVByMLR, 5)),
                                         MARGIN = 1, FUN = function(l) {
                                           paste(names(l), ":", l, collapse = "\n")
                                         })
                          ) %>%
                            plotly::layout(title = colnames(totalEstimatedGVByRep)[x],
                                           xaxis = list(title = list(text = "Observed"),
                                                        range = pltRange),
                                           yaxis = list(title = list(text = "Predicted"),
                                                        range = pltRange))

                          return(plt)
                        }, simplify = FALSE)
      names(pltList) <- colnames(totalEstimatedGVByRep)

      estimatedGVByMLRInfoNow <- list(trainingPop = trainingPop,
                                      trainingPopName = trainingPopName,
                                      trainingPopNo = trainingPopNo,
                                      trainingIndNames = trainingIndNames,
                                      testingPop = testingPop,
                                      testingPopName = testingPopName,
                                      testingPopNo = testingPopNo,
                                      testingIndNames = testingIndNames,
                                      totalPop = totalPop,
                                      totalPopName = totalPopName,
                                      totalPopNo = totalPopNo,
                                      totalIndNames = totalIndNames,
                                      methodMLR = methodMLR,
                                      alpha = alpha,
                                      nIter = nIter,
                                      burnIn = burnIn,
                                      thin = thin,
                                      multiTrait = multiTrait,
                                      bayesian = bayesian,
                                      mrkEffMat = mrkEffMat,
                                      totalEstimatedGVByRep = totalEstimatedGVByRep,
                                      totalEstimatedGVByMLR = totalEstimatedGVByMLR,
                                      testingEstimatedGVByMLR = testingEstimatedGVByMLR,
                                      R2 = R2,
                                      plot = pltList)

      self$populationsFB <- populationsFB
      self$estimatedGVByMLRInfo[[totalPopName[length(totalPopName)]]] <- estimatedGVByMLRInfoNow
    },





    #' @field populationFB [populationFB class] R6 class representing population for breeder
    populationFB = R6::R6Class(
      "populationFB",
      public = list(
        name = NULL,
        population = NULL,
        crossInfo = NULL,
        nInd = NULL,
        indNames = NULL,
        genotyping = NULL,
        genotypedIndNames = NULL,
        mrkNames = NULL,
        af = NULL,
        maf = NULL,
        mafThres = NULL,
        heteroRate = NULL,
        heteroThres = NULL,
        generation = NULL,
        genoMat = NULL,
        genoMatMC = NULL,
        genoMap = NULL,
        genoMapMC = NULL,
        haploArray = NULL,
        haploArrayMC = NULL,
        removedCond = NULL,
        removedMarkers = NULL,
        GRMs = NULL,
        methodsGRM = NULL,
        calcEpistasis = NULL,
        verbose = NULL,
        trialInfoFB = NULL,
        phenotypicValues = NULL,
        estimateGV = NULL,
        lmerResList = NULL,
        estimatedGVByRep = NULL,
        residualsByRep = NULL,
        VarHeritByRep = NULL,
        estimatedGVMethod = NULL,
        includeIntercept = NULL,
        estimatedEnvEffByRep = NULL,
        estimatedGVByGRM = NULL,
        estimatedGVByMLR = NULL,

        initialize = function(population,
                              genotyping = TRUE,
                              genotypedIndNames = NULL,
                              mrkNames = NULL,
                              mafThres = 0.05,
                              heteroThres = 1,
                              calculateGRM = TRUE,
                              methodsGRM = "addNOIA",
                              calcEpistasis = FALSE) {
          # population class
          if (class(population)[1] != "population") {
            stop(paste('class(population)[1] != "population"\n"population" must be a',
                       'population object see: ?population'))
          }

          generation <- population$generation
          crossInfo <- population$crossInfo
          name <- population$name
          verbose <- population$verbose


          self$population <- population
          self$nInd <- population$nInd
          self$indNames <- names(population$inds)
          self$generation <- generation
          self$crossInfo <- crossInfo
          self$name <- name
          self$verbose <- verbose
          self$genotyping <- genotyping
          self$crossInfo <- population$crossInfo
          self$GRMs <- list()

          if (genotyping) {
            self$genotyper(genotypedIndNames = genotypedIndNames,
                           mrkNames = mrkNames,
                           mafThres = mafThres,
                           heteroThres = heteroThres)
          }

          if (calculateGRM) {
            self$calcGRMs(methodsGRM = methodsGRM,
                          overWrite = FALSE,
                          calcEpistasis = calcEpistasis)
          }
        },

        genotyper = function(genotypedIndNames = NULL,
                             mrkNames = NULL,
                             mafThres = 0.05,
                             heteroThres = 1) {
          population <- self$population
          indNames <- self$indNames
          ploidy <- population$specie$ploidy
          mrkPos <- population$traitInfo$mrkPos
          nMarkers <- population$traitInfo$nMarkers

          if (!is.null(genotypedIndNames)) {
            if (is.numeric(genotypedIndNames)) {
              stopifnot(any(genotypedIndNames %in% 1:population$nInd))
              genotypedIndNames <- indNames[genotypedIndNames]
            } else {
              stopifnot(any(genotypedIndNames %in% indNames))
            }
          } else {
            genotypedIndNames <- indNames
          }

          if (is.null(mrkNames)) {
            mrkNames <- paste0("Marker_", 1:sum(nMarkers))
          } else {
            stopifnot(length(mrkNames) == sum(nMarkers))
          }


          genoMat <- population$genoMat[genotypedIndNames, mrkPos]
          haploArray <- population$haploArray[genotypedIndNames, mrkPos, ]
          colnames(genoMat) <- colnames(haploArray) <- mrkNames
          af <- Rfast::colsums(genoMat) / (nrow(genoMat) * ploidy)
          names(af) <- colnames(genoMat)
          maf <- pmin(af, 1 - af)
          heteroRate <- apply(genoMat, 2, function(mrk) {
            mean(!(mrk %in% c(0, ploidy)))
          })

          removedCond <- (maf < mafThres) | (heteroRate >= heteroThres)
          removedMarkers <- mrkPos[removedCond]

          genoMapAll <- population$traitInfo$lociInfo$genoMap
          genoMap <- genoMapAll[mrkPos, ]
          genoMap[, 1] <- mrkNames
          colnames(genoMap)[1] <- "mrkNames"

          genoMatMC <- genoMat[, !removedCond]
          haploArrayMC <- haploArray[, !removedCond, ]
          genoMapMC <- genoMap[!removedCond, ]

          self$genotypedIndNames <- genotypedIndNames
          self$mrkNames <- mrkNames
          self$af <- af
          self$maf <- maf
          self$mafThres <- mafThres
          self$heteroRate <- heteroRate
          self$heteroThres <- heteroThres
          self$removedCond <- removedCond
          self$removedMarkers <- removedMarkers
          self$genoMat <- genoMat
          self$genoMatMC <- genoMatMC
          self$genoMap <- genoMap
          self$genoMapMC <- genoMapMC
          self$haploArray <- haploArray
          self$haploArrayMC <- haploArrayMC
          self$genotyping <- TRUE
        },

        calcGRM = function(methodGRM = "addNOIA",
                           mafCutGRM = TRUE) {
          if (mafCutGRM) {
            genoMat <- self$genoMatMC
          } else {
            genoMat <- self$genoMat
          }
          nMarkers <- ncol(genoMat)
          mrkNames <- colnames(genoMat)

          methodNOIA <- stringr::str_detect(string = methodGRM,
                                            pattern = "NOIA")
          if (methodNOIA) {
            probaa <- apply(genoMat == 0, 2, mean)
            probAa <- apply(genoMat == 1, 2, mean)
            if (methodGRM == "addNOIA") {
              replaceaa <- - (2 - probAa - 2 * probaa)
              replaceAa <- - (1 - probAa - 2 * probaa)
              replaceAA <- - (- probAa - 2 * probaa)
            } else if (methodGRM == "domNOIA") {
              probAA <- 1 - probaa - probAa
              denominator <- probAA + probaa - (probAA - probaa) ^ 2
              replaceaa <- - 2 * probAA * probAa /denominator
              replaceAa <- 4 * probAA * probaa /denominator
              replaceAA <- - 2 * probaa * probAa /denominator
            }

            HMat <- sapply(1:nMarkers, function(mrkNo) {
              HMatEachMrk <- genoMat[, mrkNo]
              HMatEachMrk[HMatEachMrk == 0] <- replaceaa[mrkNo]
              HMatEachMrk[HMatEachMrk == 1] <- replaceAa[mrkNo]
              HMatEachMrk[HMatEachMrk == 2] <- replaceAA[mrkNo]

              return(HMatEachMrk)
            })
            colnames(HMat) <- mrkNames

            HHt <- tcrossprod(HMat)
            GRM <- HHt * self$nInd / sum(diag(HHt))
          } else {
            genoMat <- genoMat - 1

            if (methodGRM == "A.mat") {
              GRM <- rrBLUP::A.mat(X = genoMat)
            } else if (methodGRM == "linear") {
              HHt <- tcrossprod(genoMat)
              GRM <- HHt * self$nInd / sum(diag(HHt))
            } else if (methodGRM == "gaussian") {
              distMat <- Rfast::Dist(x = genoMat) / sqrt(ncol(genoMat))
              rownames(distMat) <- colnames(distMat) <- rownames(genoMat)
              hinv <- median((distMat ^ 2)[upper.tri(distMat ^ 2)])
              h <- 1 / hinv

              GRM <- exp(- h * distMat ^ 2)
            } else if (methodGRM == "exponential") {
              distMat <- Rfast::Dist(x = genoMat) / sqrt(ncol(genoMat))
              rownames(distMat) <- colnames(distMat) <- rownames(genoMat)
              hinv <- median(distMat[upper.tri(distMat)])
              h <- 1 / hinv

              GRM <- exp(- h * distMat)
            } else if (methodGRM == "correlation") {
              GRM <- cor(t(genoMat))
            }
          }

          return(GRM)
        },


        calcGRMs = function(methodsGRM = "addNOIA",
                            overWrite = TRUE,
                            calcEpistasis = FALSE) {
          GRMs <- self$GRMs
          methodsGRMTotal <- self$methodsGRM

          supportedMethodsGRM <- c("addNOIA", "domNOIA", "A.mat", "linear",
                                   "gaussian", "exponential", "correlation")

          methodsGRM <- methodsGRM[methodsGRM %in% supportedMethodsGRM]
          stopifnot(length(methodsGRM) >= 1)

          methodsGRMTotal <- unique(c(methodsGRMTotal, methodsGRM))

          for (methodGRM in methodsGRM) {
            existGRM <- !is.null(GRMs[[methodGRM]])
            if ((!existGRM) | overWrite) {
              GRMNow <- self$calcGRM(methodGRM = methodGRM,
                                     mafCutGRM = TRUE)
              GRMs[[methodGRM]] <- GRMNow
            }
          }

          if (calcEpistasis) {
            existGRMAddNOIA <- !is.null(GRMs[["addNOIA"]])
            existGRMDomNOIA <- !is.null(GRMs[["domNOIA"]])

            if (existGRMAddNOIA) {
              GRMAddNOIA <- GRMs[["addNOIA"]]
              GRMAxANOIANonScaled <- GRMAddNOIA * GRMAddNOIA
              GRMAxANOIA <- GRMAxANOIANonScaled * self$nInd / sum(diag(GRMAxANOIANonScaled))

              GRMs[["axaNOIA"]] <- GRMAxANOIA
            }

            if (existGRMDomNOIA) {
              GRMDomNOIA <- GRMs[["domNOIA"]]
              GRMDxDNOIANonScaled <- GRMDomNOIA * GRMDomNOIA
              GRMDxDNOIA <- GRMDxDNOIANonScaled * self$nInd / sum(diag(GRMDxDNOIANonScaled))

              GRMs[["dxdNOIA"]] <- GRMDxDNOIA
            }

            if (existGRMAddNOIA & existGRMDomNOIA) {
              GRMAxDNOIANonScaled <- GRMAddNOIA * GRMDomNOIA
              GRMAxDNOIA <- GRMAxDNOIANonScaled * self$nInd / sum(diag(GRMAxDNOIANonScaled))

              GRMs[["axdNOIA"]] <- GRMAxDNOIA
            }
          }

          self$GRMs <- GRMs
          self$methodsGRM <- methodsGRMTotal
          self$calcEpistasis <- calcEpistasis
        },



        phenotyper = function(trialInfo,
                              phenotypedIndNames = NULL,
                              estimateGV = TRUE,
                              estimatedGVMethod = "lme4",
                              includeIntercept = TRUE) {
          population <- self$population
          indNames <- self$indNames

          if (!is.null(phenotypedIndNames)) {
            if (is.numeric(phenotypedIndNames)) {
              stopifnot(any(phenotypedIndNames %in% 1:population$nInd))
              phenotypedIndNames <- indNames[phenotypedIndNames]
            } else {
              stopifnot(any(phenotypedIndNames %in% indNames))
            }
          } else {
            phenotypedIndNames <- indNames
          }

          if (population$specie$simInfo$simPheno) {
            population$trialInfo <- trialInfo
            population$inputPhenotypicValues()
          }

          phenotypicValues <- population$phenotypicValues[phenotypedIndNames, , , drop = FALSE]
          trialInfoFB <- list(nRep =  trialInfo$nRep,
                              multiTraitsAsEnvs = trialInfo$multiTraitsAsEnvs,
                              phenotypedIndNames = phenotypedIndNames)
          self$phenotypicValues <- phenotypicValues
          self$trialInfoFB <- trialInfoFB
          self$estimateGV <- estimateGV


          if (estimateGV) {
            self$estimateGVByRep(estimatedGVMethod = estimatedGVMethod,
                                 includeIntercept = includeIntercept)
          }
        },


        estimateGVByRep = function(estimatedGVMethod = "lme4",
                                   includeIntercept = TRUE) {
          trialInfoFB <- self$trialInfoFB
          phenotypicValues <- self$phenotypicValues
          phenotypedIndNames <- self$trialInfoFB$phenotypedIndNames
          if (is.null(phenotypicValues)) {
            stop("You should phenotype by `self$phenotyper()` before estimating GV!!")
          }

          if (trialInfoFB$nRep >= 2) {
            if (estimatedGVMethod == "mean") {
              if (!trialInfoFB$multiTraitsAsEnvs) {
                estimatedGVByRep <- apply(X = phenotypicValues,
                                          MARGIN = c(1, 2),
                                          FUN = mean, na.rm = TRUE)
                residualsByRep <- phenotypicValues -
                  array(data = rep(estimatedGVByRep, trialInfoFB$nRep),
                        dim = dim(phenotypicValues),
                        dimnames = dimnames(phenotypicValues))

                VuByRep <- apply(estimatedGVByRep, 2, var)
                VeByRep <- apply(X = residualsByRep, 2, function(x) var(c(x)))

                heritIndByRep <- VuByRep / (VuByRep + VeByRep)
                heritLineByRep <- VuByRep / (VuByRep + VeByRep / trialInfoFB$nRep)

                VarHeritByRep <- rbind(VuByRep, VeByRep,
                                       heritIndByRep, heritLineByRep)

                estimatedEnvEffByRep <- NULL
              } else {
                estimatedGVByRepEachEnv <- apply(X = phenotypicValues,
                                                 MARGIN = c(1, 2),
                                                 FUN = mean, na.rm = TRUE)
                residualsByRepEachEnv <- phenotypicValues -
                  array(data = rep(estimatedGVByRepEachEnv, trialInfoFB$nRep),
                        dim = dim(phenotypicValues),
                        dimnames = dimnames(phenotypicValues))
                estimatedGVByRepEachEnvDF <- data.frame(
                  Ind = rep(rownames(estimatedGVByRepEachEnv),
                            ncol(estimatedGVByRepEachEnv)),
                  Env = rep(colnames(estimatedGVByRepEachEnv),
                            each = nrow(estimatedGVByRepEachEnv)),
                  Value = c(estimatedGVByRepEachEnv)
                )
                lmRes <- lm(formula = Value ~ (Ind - 1) + (Env - 1),
                            data = estimatedGVByRepEachEnvDF)

                estimatedGVByRep0 <-
                  coefficients(lmRes)[paste0("Ind", rownames(estimatedGVByRepEachEnv))]
                names(estimatedGVByRep0) <- rownames(estimatedGVByRepEachEnv)
                estimatedEnvEffByRep0 <- c(0, coefficients(lmRes)[paste0("Env",
                                                                         colnames(estimatedGVByRepEachEnv)[-1])])
                names(estimatedEnvEffByRep0) <- colnames(estimatedGVByRepEachEnv)

                estimatedGVByRep <- as.matrix(estimatedGVByRep0 - mean(estimatedGVByRep0))
                estimatedEnvEffByRep <- estimatedEnvEffByRep0 + mean(estimatedGVByRep0)

                residualsByRep <- residualsByRepEachEnv +
                  array(data = rep(resid(lmRes), 2),
                        dim = dim(phenotypicValues),
                        dimnames = dimnames(phenotypicValues))

                VuByRep <- apply(estimatedGVByRep, 2, var)
                VeByRep <- var(c(residualsByRep))

                heritIndByRep <- VuByRep / (VuByRep + VeByRep)
                heritLineByRep <- VuByRep / (VuByRep + VeByRep / (trialInfoFB$nRep * ncol(phenotypicValues)))

                VarHeritByRep <- rbind(VuByRep, VeByRep,
                                       heritIndByRep, heritLineByRep)

                colnames(estimatedGVByRep) <- colnames(VarHeritByRep) <- "traitOfInterest"
              }


              lmerResList <- NULL
            } else if (estimatedGVMethod == "lme4") {
              if (!trialInfoFB$multiTraitsAsEnvs) {
                lmerResList <- apply(X = phenotypicValues,
                                     MARGIN = 2,
                                     FUN = function(phenotypicValuesEach) {
                                       phenotypicValuesDataFrame <- data.frame(
                                         Ind = rep(rownames(phenotypicValuesEach),
                                                   ncol(phenotypicValuesEach)),
                                         Rep = rep(colnames(phenotypicValuesEach),
                                                   each = nrow(phenotypicValuesEach)),
                                         Value = c(phenotypicValuesEach)
                                       )

                                       lmerRes <- lme4::lmer(formula = Value ~ (1 | Ind),
                                                             REML = TRUE,
                                                             data = phenotypicValuesDataFrame)

                                       return(lmerRes)
                                     })

                estimatedGVByRep <- do.call(what = cbind,
                                            args =
                                              lapply(lmerResList,
                                                     function(lmerRes) {
                                                       estimatedGVByRepEach <-
                                                         lme4::ranef(lmerRes)$Ind[phenotypedIndNames, ]
                                                       names(estimatedGVByRepEach) <-
                                                         phenotypedIndNames

                                                       return(estimatedGVByRepEach)
                                                     }))
                residualsByRep <- phenotypicValues -
                  array(data = rep(estimatedGVByRep, trialInfoFB$nRep),
                        dim = dim(phenotypicValues),
                        dimnames = dimnames(phenotypicValues))

                VarHeritByRep <- do.call(what = cbind,
                                         args =
                                           lapply(lmerResList,
                                                  function(lmerRes) {
                                                    VarInfo <- data.frame(lme4::VarCorr(lmerRes))
                                                    VuByRep <- VarInfo[1, 4]
                                                    VeByRep <- VarInfo[2, 4]

                                                    heritIndByRep <- VuByRep / (VuByRep + VeByRep)
                                                    heritLineByRep <- VuByRep / (VuByRep + VeByRep / trialInfoFB$nRep)

                                                    return(c(VuByRep = VuByRep,
                                                             VeByRep = VeByRep,
                                                             heritIndByRep = heritIndByRep,
                                                             heritLineByRep = heritLineByRep))
                                                  }))

                estimatedEnvEffByRep <- unlist(lapply(X = lmerResList,
                                                      FUN = function(lmerRes) lme4::fixef(lmerRes)),
                                               use.names = FALSE)
                names(estimatedEnvEffByRep) <- names(lmerResList)


                if (includeIntercept) {
                  estimatedGVByRep <- estimatedGVByRep +
                    matrix(data = rep(estimatedEnvEffByRep, nrow(estimatedGVByRep)),
                           nrow = nrow(estimatedGVByRep), byrow = TRUE)
                }
              } else {
                phenotypicValuesDataFrame <- data.frame(
                  Ind = rep(rownames(phenotypicValues),
                            prod(dim(phenotypicValues)[2:3])),
                  Env = rep(rep(colnames(phenotypicValues),
                                each = nrow(phenotypicValues)),
                            dim(phenotypicValues)[3]),
                  Rep = rep(dimnames(phenotypicValues)[[3]],
                            each = prod(dim(phenotypicValues)[1:2])),
                  Value = c(phenotypicValues)
                )

                lmerRes <- lme4::lmer(formula = Value ~ (1 | Ind) + Env,
                                      REML = TRUE,
                                      data = phenotypicValuesDataFrame)
                lmerResList <- list(traitOfInterest = lmerRes)

                estimatedGVByRep <- lme4::ranef(lmerRes)$Ind[phenotypedIndNames, ]
                names(estimatedGVByRep) <- phenotypedIndNames
                estimatedGVByRep <- as.matrix(estimatedGVByRep)

                estimatedEnvEffByRep <- lme4::fixef(lmerRes)
                estimatedEnvEffByRep[-1] <- estimatedEnvEffByRep[-1] + estimatedEnvEffByRep[1]
                names(estimatedEnvEffByRep) <- colnames(phenotypicValues)

                residualsByRep <- array(data = resid(lmerRes),
                                        dim = dim(phenotypicValues),
                                        dimnames = dimnames(phenotypicValues))


                VarInfo <- data.frame(lme4::VarCorr(lmerRes))
                VuByRep <- VarInfo[1, 4]
                VeByRep <- VarInfo[2, 4]

                heritIndByRep <- VuByRep / (VuByRep + VeByRep)
                heritLineByRep <- VuByRep / (VuByRep + VeByRep / (trialInfoFB$nRep * ncol(phenotypicValues)))

                VarHeritByRep <- rbind(VuByRep = VuByRep,
                                       VeByRep = VeByRep,
                                       heritIndByRep = heritIndByRep,
                                       heritLineByRep = heritLineByRep)

                colnames(estimatedGVByRep) <- colnames(VarHeritByRep) <- "traitOfInterest"
              }

            } else {
              stop("We only offer 'mean' and 'lme4' methods for `estimatedGVMethod`!!")
            }
          } else {
            estimatedGVByRep <- array(data = phenotypicValues,
                                      dim = dim(phenotypicValues)[1:2],
                                      dimnames = dimnames(phenotypicValues)[1:2])
            estimatedGVMethod <- NULL
            lmerResList <- NULL
            residualsByRep <- NULL
            VarHeritByRep <- NULL
            estimatedEnvEffByRep <- NULL
          }

          self$estimatedGVMethod <- estimatedGVMethod
          self$lmerResList <- lmerResList
          self$estimatedGVByRep <- estimatedGVByRep
          self$residualsByRep <- residualsByRep
          self$VarHeritByRep <- VarHeritByRep
          self$estimatedEnvEffByRep <- estimatedEnvEffByRep
          self$estimateGV <- TRUE
          self$includeIntercept <- includeIntercept
        },


        #' @description
        #' Display informations about the object
        print = function() {
          cat(paste0(
            "Population: ", self$name, "\n",
            "Parent Population: ", self$crossInfo$parentPopulation$name, "\n",
            "Generation: ", self$generation, "\n",
            "Species: ", self$specie$specName, "\n",
            "Number of individuals: ", self$nInd
          ))
        }
      )
    ),


    #' @description
    #' assemble populations over generation
    #' @param targetPop [character / numeric] population names or No.s you want to assemble.
    #' If NULL, assemble all the populations
    overGeneration = function(targetPop = NULL) {
      populationsFB <- self$populationsFB
      generation <- self$generation
      allPop <- 1:length(populationsFB)
      allPopNo <- unlist(lapply(populationsFB,
                                function(popFB) popFB$generation))
      allPopName <- names(populationsFB)

      if (is.null(targetPop)) {
        targetPop <- allPop
      } else if (is.numeric(targetPop)) {
        targetPop <- targetPop[targetPop %in% allPop]
      } else if (is.character(targetPop)) {
        targetPop <- targetPop[targetPop %in% allPopName]
      } else {
        stop("A class of `targetPop` should be numeric or character!!")
      }

      targetPopulationsFB <- populationsFB[targetPop]
      targetPopName <- names(targetPopulationsFB)
      targetPopNo <- unlist(lapply(targetPopulationsFB,
                                   function(popFB) popFB$generation))

      nTargetPop <- length(targetPop)
      indsOverGeneration <- NULL
      for (i in 1:nTargetPop) {
        indsNow <- targetPopulationsFB[[i]]$population$inds
        indsOverGeneration <- c(indsOverGeneration, indsNow)
      }


      popOGName <- paste0(self$popNameBase, "_",
                          paste(targetPopNo, collapse = "_"))

      traitInfo <- self$populationsFB[[1]]$population$traitInfo
      popOverGeneration <- population$new(name = popOGName,
                                          generation = NA,
                                          traitInfo = traitInfo,
                                          inds = indsOverGeneration,
                                          verbose = FALSE)
      genotypedIndNamesOG <- do.call(
        what = c,
        args = lapply(X = targetPopulationsFB,
                      FUN = function(popFB) {
                        if (popFB$genotyping) {
                          return(popFB$genotypedIndNames)
                        } else {
                          return(NULL)
                        }
                      })
      )
      if (is.null(genotypedIndNamesOG)) {
        genotypingOG <- FALSE
      } else {
        genotypingOG <- TRUE
      }
      names(genotypedIndNamesOG) <- NULL

      popOverGenerationFB <- self$populationFB$new(
        population = popOverGeneration,
        genotyping = genotypingOG,
        genotypedIndNames = genotypedIndNamesOG,
        mrkNames = targetPopulationsFB[[1]]$mrkNames,
        mafThres = self$lociInfoFB$mafThres,
        heteroThres = self$lociInfoFB$heteroThres,
        calculateGRM = self$calculateGRMBase,
        methodsGRM = self$methodsGRMBase
      )

      phenotypicValuesOG <- lapply(X = targetPopulationsFB,
                                   FUN = function(popFB) {
                                     popFB$phenotypicValues
                                   })
      estimatedGVByRepOG <- do.call(
        what = rbind,
        args = lapply(X = targetPopulationsFB,
                      FUN = function(popFB) {
                        popFB$estimatedGVByRep
                      })
      )

      popOverGenerationFB$phenotypicValues <- phenotypicValuesOG
      popOverGenerationFB$estimatedGVByRep <- estimatedGVByRepOG


      return(popOverGenerationFB)
    },


    #' @description
    #' remove latest population
    removeLatestPop = function() {
      populationsFB <- self$populationsFB
      generation <- self$generation
      crossInfoList <- self$crossInfoList

      if (length(populationsFB) == 1) {
        warning(paste0("We cannot remove all the populations. ",
                       "Please remain at least 1 population or redefine the initial population with `self$initialize`!"))
      } else {
        populationsFB[[length(populationsFB)]] <- NULL
        crossInfoList[[length(populationsFB) - 1]] <- NULL
        generation <- generation - 1
      }


      self$generation <- generation
      self$populationsFB <- populationsFB
      self$crossInfoList <- crossInfoList
    },


    #' @description
    #' remove initial population
    removeInitialPop = function() {
      populationsFB <- self$populationsFB
      crossInfoList <- self$crossInfoList

      if (length(populationsFB) == 1) {
        warning(paste0("We cannot remove all the populations. ",
                       "Please remain at least 1 population or redefine the initial population with `self$initialize`!"))
      } else {
        populationsFB[[1]] <- NULL
        crossInfoList[[1]] <- NULL
      }

      self$populationsFB <- populationsFB
      self$crossInfoList <- crossInfoList
    },




    #' @description
    #' search parents of an individual of interest
    #' @param indName [character] individual name of interest
    parentInd = function(indName) {
      populationsFB <- self$populationsFB
      whichPop <- which(!is.na(unlist(lapply(populationsFB, function(pop) {
        charmatch(x = indName,
                  table = pop$indNames)
      }))))
      if (length(whichPop) == 1) {
        indInfo <- populationsFB[[whichPop]]$population$inds[[indName]]
      } else if (length(whichPop) >= 2) {
        indInfo <- populationsFB[[whichPop[1]]]$population$inds[[indName]]
      } else if (length(whichPop) == 0) {
        stop("There is no individual named '", indName, " ' in this bredding scheme!!")
      }

      parent1 <- indInfo$parent1
      parent2 <- indInfo$parent2

      return(c(Parent_1 = parent1, Parent_2 = parent2))
    },


    #' @description
    #' search parents of an individual of interest
    #' @param bsInfo [bsInfo class] breeding scheme info (whichever generation is OK,
    #' but it will use only 1st population)
    #'   (see:\link[myBreedSimulatR]{bsInfo})
    #' @param trainingPop [character / numeric] training population names or No.s (not generations!!)
    #' @param methodMLR [character] methods for estimating marker effects.
    #' @param samplingMrkEff [logical] Whether or not sampling of marker effects from the distribution is conducted.
    #' This option can be used for the robust optimization when using estimated marker effects.
    #' This option can only be `TRUE` when using bayesian methods for `methodMLR`.
    #' @param seedMrkEffSampling [numeric] When `samplingMrkEff` is TRUE, you can set seed for sampling by setting `seedMrkEffSampling`.
    #' The following methods are offered:
    #'
    #' "Ridge", "LASSO", "ElasticNet", "RR-BLUP", "GBLUP", "BayesA" (uni-trait),
    #' "BayesB" (uni-trait), "BayesC" (uni-trait), "BRR", "BL" (uni-trait), "SpikeSlab" (multi-trait)
    #'
    lociEffects = function(bsInfo,
                           trainingPop = NULL,
                           methodMLR = NULL,
                           samplingMrkEff = FALSE,
                           seedMrkEffSampling = NA) {
      bayesianMLRMethods <- c("BayesA", "BayesB", "BayesC", "BRR", "BL", "SpikeSlab")

      lociInfo <- bsInfo$lociInfo
      traitInfo <- bsInfo$traitInfo
      nLoci <- lociInfo$nLoci()
      lociNames <- lociInfo$genoMap$lociNames
      lociNames <- stringr::str_sort(x = lociNames, numeric = TRUE)
      nTraits <- traitInfo$nTraits
      traitNames <- traitInfo$traitNames
      populationsFB <- self$populationsFB

      lociEffects <- matrix(data = 0,
                            nrow = nLoci,
                            ncol = nTraits,
                            dimnames = list(lociNames,
                                            traitNames))
      if (is.null(trainingPop) & is.null(methodMLR)) {
        mrkEffMat <- self$estimatedMrkEffInfo[[1]]$mrkEffMat
        mrkEffSdMat <- self$estimatedMrkEffInfo[[1]]$mrkEffSdMat
        trainingPop <- self$estimatedMrkEffInfo[[1]]$trainingPop
        trainingPopName <- self$estimatedMrkEffInfo[[1]]$trainingPopName
        methodMLR <- self$estimatedMrkEffInfo[[1]]$methodMLR
      } else {
        allPop <- 1:length(populationsFB)
        allPopNo <- unlist(lapply(populationsFB,
                                  function(popFB) popFB$generation))
        allPopName <- names(populationsFB)

        if (is.null(trainingPop)) {
          trainingPop <- allPop
          trainingPopNo <- allPopNo
          trainingPopName <- allPopName
        } else if (is.numeric(trainingPop)) {
          trainingPop <- trainingPop[trainingPop %in% allPop]
        } else if (is.character(trainingPop)) {
          trainingPop <- trainingPop[trainingPop %in% allPopName]
        } else {
          stop("A class of `trainingPop` should be numeric or character!!")
        }
        trainingPopulationsFB <- populationsFB[trainingPop]
        trainingPopName <- names(trainingPopulationsFB)

        if (is.null(methodMLR)) {
          methodMLR <- "Ridge"
        }

        infoName <- paste0(trainingPopName[length(trainingPopName)], "_", methodMLR)
        mrkEffMat <- self$estimatedMrkEffInfo[[infoName]]$mrkEffMat
        mrkEffSdMat <- self$estimatedMrkEffInfo[[infoName]]$mrkEffSdMat
      }

      if (!(methodMLR %in% bayesianMLRMethods)) {
        samplingMrkEff <- FALSE
      }


      if (samplingMrkEff) {
        if (is.na(seedMrkEffSampling)) {
          seedMrkEffSampling <- sample(x = 1:1e09, size = 1)
        }

        set.seed(seedMrkEffSampling)
        mrkEffMatSampled <- sapply(X = 1:nTraits,
                                   FUN = function(traitNo) {
                                     mrkEffVecSampled <- mapply(FUN = function(mean, sd) {
                                       rnorm(1, mean = mean, sd = sd)
                                     },
                                     mrkEffMat[, traitNo, drop = TRUE],
                                     mrkEffSdMat[, traitNo, drop = TRUE],
                                     SIMPLIFY = TRUE)

                                     return(mrkEffVecSampled)
                                   }, simplify = TRUE)
        dimnames(mrkEffMatSampled) <- dimnames(mrkEffMat)
        mrkEffMatReplaced <- mrkEffMatSampled
      } else {
        seedMrkEffSampling <- NA
        mrkEffMatReplaced <- mrkEffMat
      }


      lociEffects[traitInfo$mrkPos, ] <- mrkEffMatReplaced[-1, ]
      lociEffects <- rbind(Intercept = mrkEffMatReplaced[1, ],
                           lociEffects)

      return(lociEffects)
    },


    #' @description
    #' Display information about the object
    print = function() {
      cat(paste0(
        "Name of Breeder: ", self$breederName, "\n",
        "Name of Specie: ", self$specie$specName, "\n",
        "Number of Markers: ", sum(self$lociInfoFB$nMarkers), "\n",
        "Number of Traits: ", self$traitInfoFB$nTraits, "\n",
        "Current Generation No.: ", self$generation, "\n",
        "Number of individuals for each population: \n"
      ))
      print(unlist(lapply(self$populationsFB, function(x) x$nInd)))
    }
  )
)
