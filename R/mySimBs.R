# Author: Kosuke Hamazaki hamazaki@ut-biomet.org
# 2020 The University of Tokyo
#
# Description:
# Definition of simBs class




#' R6 Class Representing a Breeding Scheme
#'
#' @description
#' simBs object store specific information of simulation results of breeding scheme.
#'
# @details
# Details: simBs object store specific information of simulation results of breeding scheme.
#'
#' @export
#' @import R6
simBs <- R6::R6Class(
  "simBs",
  public = list(
    #' @field simBsName [character] Name of this simulation of breeding schemes
    simBsName = NULL,
    #' @field bsInfoInit [bsInfo] breeding scheme info
    #'   (see:\link[myBreedSimulatR]{bsInfo})
    bsInfoInit = NULL,
    #' @field breederInfoInit [breederInfo] breeder info
    #'   (see:\link[myBreedSimulatR]{breederInfo})
    breederInfoInit = NULL,
    #' @field lociEffMethod [character] How to compute loci effects (use true QTL effects ("true") or estimated marker effects ("estimated"))
    lociEffMethod = NULL,
    #' @field trainingPopType [character] Training population consists of all populations created in the breeding scheme for `trainingPopType = "all"`,
    #' or consists of the latest population in breeding scheme for `trainingPopType = "latest"`.
    trainingPopType = NULL,
    #' @field trainingPopInit [character / numeric] Training population names or No.s (not generations!!) for initial population
    trainingPopInit = NULL,
    #' @field trainingIndNamesInit [character] Names of training individuals for initia; population
    trainingIndNamesInit = NULL,
    #' @field methodMLRInit [character] Methods for estimating marker effects for initial population.
    #' The following methods are offered:
    #'
    #' "Ridge", "LASSO", "ElasticNet", "RR-BLUP", "GBLUP", "BayesA" (uni-trait),
    #' "BayesB" (uni-trait), "BayesC" (uni-trait), "BRR", "BL" (uni-trait), "SpikeSlab" (multi-trait)
    methodMLRInit = NULL,
    #' @field multiTraitInit [logical] Use multiple-trait model for estimation of marker effects or not for initial population
    multiTraitInit = NULL,
    #' @field samplingMrkEffInit [logical] Whether or not sampling of marker effects from the distribution is conducted for initial population.
    #' This option can be used for the robust optimization when using estimated marker effects.
    #' This option can only be `TRUE` when using bayesian methods for `methodMLRInit`.
    samplingMrkEffInit = NULL,
    #' @field seedMrkEffSamplingInit [numeric] When `samplingMrkEffInit` is TRUE, you can set seed for sampling by setting `seedMrkEffSampling`.
    seedMrkEffSamplingInit = NULL,
    #' @field nIterSimulation [numeric] Number of iterations for this simulation setting
    nIterSimulation = NULL,
    #' @field nGenerationProceed [numeric] Number of generations to be proceeded
    nGenerationProceed = NULL,
    #' @field nRefreshMemoryEvery [numeric] Every `nRefreshMemoryEvery` iterations, we refresh memory used for simulations by `gc(reset = TRUE)`.
    nRefreshMemoryEvery = NULL,
    #' @field updateBreederInfo [logical] Update breederInfo or not for each generation
    updateBreederInfo = NULL,
    #' @field phenotypingInds [logical] Phenotyping individuals or not for each generation
    phenotypingInds = NULL,
    #' @field nRepForPhenoInit [numeric] Number of replications to be phenotyped for initial population
    nRepForPhenoInit = NULL,
    #' @field nRepForPheno [numeric] Number of replications to be phenotyped for each generation
    nRepForPheno = NULL,
    #' @field updateModels [logical] Whether or not updating the model for each generation
    #' (If `phenotypingInds = FALSE` for generation of your interest, `updateModels` will be automatically `FALSE` for that generation.)
    updateModels = NULL,
    #' @field methodMLR [character] Methods for estimating marker effects.
    #' The following methods are offered:
    #'
    #' "Ridge", "LASSO", "ElasticNet", "RR-BLUP", "GBLUP", "BayesA" (uni-trait),
    #' "BayesB" (uni-trait), "BayesC" (uni-trait), "BRR", "BL" (uni-trait), "SpikeSlab" (multi-trait)
    methodMLR = NULL,
    #' @field multiTrait [logical] Use multiple-trait model for estimation of marker effects or not
    multiTrait = NULL,
    #' @field nSelectionWaysVec [numeric] Number of selection ways for each generation
    nSelectionWaysVec = NULL,
    #' @field selectionMethodList [list] (list of) Selection method for each generation
    selectionMethodList = NULL,
    #' @field traitNoSelList [list] (list of list of) Number of trait No of your interest for selection for each generation
    traitNoSelList = NULL,
    #' @field blockSplitMethod [character] How to determine the markers belonging to each block when computing OHV.
    #' You can define the number of markers in each block (`nMrkInBlock`) or the minimum length of each segment (`minimumSegmentLength`).
    blockSplitMethod = NULL,
    #' @field nMrkInBlock [numeric] Number of markers in each block. This will be used for the computation of OHV.
    nMrkInBlock = NULL,
    #' @field minimumSegmentLength [numeric] Minimum length of each segment [cM]. This will be used for the computation of OHV.
    minimumSegmentLength = NULL,
    #' @field nSelInitOPVList [list] (list of) Number of selected candiates for first screening before selecting parent candidates by OPV for each generation
    nSelInitOPVList = NULL,
    #' @field nIterOPV [numeric] Number of iterations for computation of OPV
    nIterOPV = NULL,
    #' @field nProgeniesEMBVVec [numeric] Number of progenies of double haploids produced when computing EMBV for each generation
    nProgeniesEMBVVec = NULL,
    #' @field nIterEMBV [numeric] Number of iterations to estimate EMBV
    nIterEMBV = NULL,
    #' @field nCoresEMBV [numeric] Number of cores used for EMBV estimation
    nCoresEMBV = NULL,
    #' @field clusteringForSelList [list] (list of) Apply clustering results of marker genotype to selection or not for each generation
    clusteringForSelList = NULL,
    #' @field nClusterList [list] (list of) Number of clusters for each generation
    nClusterList = NULL,
    #' @field nTopClusterList [list] (list of) Number of top clusters used for selection for each generation
    nTopClusterList = NULL,
    #' @field nTopEachList [list] (list of) Number of selected individuals in each cluster for each generation
    nTopEachList = NULL,
    #' @field nSelList [list] (list of) Number of selection candidates for each generation
    nSelList = NULL,
    #' @field multiTraitsEvalMethodList [list] (list of) When evaluating multiple traits, you can choose how to evaluate these traits simultaneously.
    #' One method is to take a weighted sum of these traits, and the other is to compute a product of these traits (adjusted to be positive values in advance.)
    multiTraitsEvalMethodList = NULL,
    #' @field hSelList [list] (list of) Hyperparameter which determines which trait is weighted when selecting parent candidates for multiple traits for each generation
    hSelList = NULL,
    #' @field matingMethodVec [character] Mating method for each generation
    matingMethodVec = NULL,
    #' @field allocateMethodVec [character] Allocation method for each generation
    allocateMethodVec = NULL,
    #' @field weightedAllocationMethodList [list] (list of) Which selection index will be used for weighted resource allocation for each generation
    weightedAllocationMethodList = NULL,
    #' @field traitNoRAList [list] (list of) Trait No of your interest for resource allocation for each generation
    traitNoRAList = NULL,
    #' @field hList [list] (list of) Hyperparameter which determines how parent pair with high BV is emphasized when producing progenies for each generation
    hList = NULL,
    #' @field minimumUnitAllocateVec [numeric] (vector of) Minimum number of allocated progenies for each pair.
    minimumUnitAllocateVec = NULL,
    #' @field includeGVPVec [logical] Whether or not to consider genetic variance of progenies of each pair when determining the number of progenies per each pair for each generation
    includeGVPVec = NULL,
    #' @field nNextPopVec [numeric] Number of progenies for the next generation  for each generation
    nNextPopVec = NULL,
    #' @field nameMethod [character] Method for naming individuals
    nameMethod = NULL,
    #' @field nCores [numeric] Number of cores used for simulations of breeding scheme
    nCores = NULL,
    #' @field overWriteRes [logical] Overwrite simulation results when the targeted results already exists
    overWriteRes = NULL,
    #' @field showProgress [logical] Show progress bar or not
    showProgress = NULL,
    #' @field returnMethod [character] Which type of results will be returned (saved) in the object
    returnMethod = NULL,
    #' @field saveAllResAt [character] If NULL, we won't save the simulation results. Else, we will save bsInfo and breederInfo for each iteration in the directory defined by `saveAllResAt` path.
    saveAllResAt = NULL,
    #' @field evaluateGVMethod [character] Which type of GV (true GV or estimated GV) will be used for evaluation of the simulation results
    evaluateGVMethod = NULL,
    #' @field nTopEval [numeric] Number of individuals to be evaluated when evaluating population max or population min
    nTopEval = NULL,
    #' @field traitNoEval [numeric] Trait No of your interest for evaluation of the method
    traitNoEval = NULL,
    #' @field hEval [numeric] Hyperparameter which determines which BV is emphasized when evaluating simulation results of each method
    hEval = NULL,
    #' @field summaryAllResAt [character] If NULL, we will summary the simulation results in `self$trueGVMatList` and `self$estimatedGVMatList`.
    #' Else, we will summary the simulation results in `summaryAllResAt` path.
    summaryAllResAt = NULL,
    #' @field verbose [logical] Display info (optional)
    verbose = NULL,


    #' @field lociEffectsInit [matrix] Marker and QTL effects used for crossInfo object for initial population
    lociEffectsInit = NULL,
    #' @field simBsRes [list] Simulation results of each method
    simBsRes = list(),
    #' @field trueGVMatInit [matrix] A true GV matrix for initial population
    trueGVMatInit = NULL,
    #' @field trueGVMatList [list] A list of true GV matrix of simulation results
    trueGVMatList = list(),
    #' @field trueGVSummaryArray [array] An array of summary statistics of true GVs for each population & each iteration
    trueGVSummaryArray = NULL,
    #' @field estimatedGVMatInit [matrix] An estimated GV matrix for initial population
    estimatedGVMatInit = NULL,
    #' @field estimatedGVMatList [list] A list of estimated GV matrix of simulation results
    estimatedGVMatList = list(),
    #' @field estimatedGVSummaryArray [array] An array of summary statistics of estimated GVs for each population & each iteration
    estimatedGVSummaryArray = NULL,



    #' @description Create a new simBs object.
    #' @param simBsName [character] Name of this simulation of breeding schemes
    #' @param bsInfoInit [bsInfo] breeding scheme info
    #'   (see:\link[myBreedSimulatR]{bsInfo})
    #' @param breederInfoInit [breederInfo] breeder info
    #'   (see:\link[myBreedSimulatR]{breederInfo})
    #' @param lociEffMethod [character] How to compute loci effects (use true QTL effects ("true") or estimated marker effects ("estimated"))
    #' @param trainingPopType [character] Training population consists of all populations created in the breeding scheme for `trainingPopType = "all"`,
    #' or consists of the latest population in breeding scheme for `trainingPopType = "latest"`.
    #' @param trainingPopInit [character / numeric] Training population names or No.s (not generations!!) for initial population
    #' @param trainingIndNamesInit [character] Names of training individuals for initia; population
    #' @param methodMLRInit [character] Methods for estimating marker effects for initial population.
    #' The following methods are offered:
    #'
    #' "Ridge", "LASSO", "ElasticNet", "RR-BLUP", "GBLUP", "BayesA" (uni-trait),
    #' "BayesB" (uni-trait), "BayesC" (uni-trait), "BRR", "BL" (uni-trait), "SpikeSlab" (multi-trait)
    #' @param multiTraitInit [logical] Use multiple-trait model for estimation of marker effects or not for initial population
    #' @param samplingMrkEffInit [logical] Whether or not sampling of marker effects from the distribution is conducted for initial population.
    #' This option can be used for the robust optimization when using estimated marker effects.
    #' This option can only be `TRUE` when using bayesian methods for `methodMLRInit`.
    #' @param seedMrkEffSamplingInit [numeric] When `samplingMrkEffInit` is TRUE, you can set seed for sampling by setting `seedMrkEffSampling`.
    #' @param nIterSimulation [numeric] Number of iterations for this simulation setting
    #' @param nGenerationProceed [numeric] Number of generations to be proceeded
    #' @param nRefreshMemoryEvery [numeric] Every `nRefreshMemoryEvery` iterations, we refresh memory used for simulations by `gc(reset = TRUE)`.
    #' @param updateBreederInfo [logical] Update breederInfo or not for each generation
    #' @param phenotypingInds [logical] Phenotyping individuals or not for each generation
    #' @param nRepForPhenoInit [numeric] Number of replications to be phenotyped for initial population
    #' @param nRepForPheno [numeric] Number of replications to be phenotyped for each generation
    #' @param updateModels [logical] Whether or not updating the model for each generation
    #' (If `phenotypingInds = FALSE` for generation of your interest, `updateModels` will be automatically `FALSE` for that generation.)
    #' @param methodMLR [character] Methods for estimating marker effects.
    #' The following methods are offered:
    #'
    #' "Ridge", "LASSO", "ElasticNet", "RR-BLUP", "GBLUP", "BayesA" (uni-trait),
    #' "BayesB" (uni-trait), "BayesC" (uni-trait), "BRR", "BL" (uni-trait), "SpikeSlab" (multi-trait)
    #' @param multiTrait [logical] Use multiple-trait model for estimation of marker effects or not
    #' @param nSelectionWaysVec [numeric] Number of selection ways for each generation
    #' @param selectionMethodList [list] (list of) Selection method for each generation
    #' @param traitNoSelList [list] (list of list of) Number of trait No of your interest for selection for each generation
    #' @param blockSplitMethod [character] How to determine the markers belonging to each block when computing OHV.
    #' You can define the number of markers in each block (`nMrkInBlock`) or the minimum length of each segment (`minimumSegmentLength`).
    #' @param nMrkInBlock [numeric] Number of markers in each block. This will be used for the computation of OHV.
    #' @param minimumSegmentLength [numeric] Minimum length of each segment [cM]. This will be used for the computation of OHV.
    #' @param nSelInitOPVList [list] (list of) Number of selected candiates for first screening before selecting parent candidates by OPV for each generation
    #' @param nIterOPV [numeric] Number of iterations for computation of OPV
    #' @param nProgeniesEMBVVec [numeric] Number of progenies of double haploids produced when computing EMBV for each generation
    #' @param nIterEMBV [numeric] Number of iterations to estimate EMBV
    #' @param nCoresEMBV [numeric] Number of cores used for EMBV estimation
    #' @param clusteringForSelList [list] (list of) Apply clustering results of marker genotype to selection or not for each generation
    #' @param nClusterList [list] (list of) Number of clusters for each generation
    #' @param nTopClusterList [list] (list of) Number of top clusters used for selection for each generation
    #' @param nTopEachList [list] (list of) Number of selected individuals in each cluster for each generation
    #' @param nSelList [list] (list of) Number of selection candidates for each generation
    #' @param multiTraitsEvalMethodList [list] (list of) When evaluating multiple traits, you can choose how to evaluate these traits simultaneously.
    #' One method is to take a weighted sum of these traits, and the other is to compute a product of these traits (adjusted to be positive values in advance.)
    #' @param hSelList [list] (list of) Hyperparameter which determines which trait is weighted when selecting parent candidates for multiple traits for each generation
    #' @param matingMethodVec [character] Mating method for each generation
    #' @param allocateMethodVec [character] Allocation method for each generation
    #' @param weightedAllocationMethodList [list] (list of) Which selection index will be used for weighted resource allocation for each generation
    #' @param traitNoRAList [list] (list of) Trait No of your interest for resource allocation for each generation
    #' @param hList [list] (list of) Hyperparameter which determines how parent pair with high BV is emphasized when producing progenies for each generation
    #' @param minimumUnitAllocateVec [numeric] (vector of) Minimum number of allocated progenies for each pair.
    #' @param includeGVPVec [logical] Whether or not to consider genetic variance of progenies of each pair when determining the number of progenies per each pair for each generation
    #' @param nNextPopVec [numeric] Number of progenies for the next generation  for each generation
    #' @param nameMethod [character] Method for naming individuals
    #' @param nCores [numeric] Number of cores used for simulations of breeding scheme
    #' @param overWriteRes [logical] Overwrite simulation results when the targeted results already exists
    #' @param showProgress [logical] Show progress bar or not
    #' @param returnMethod [character] Which type of results will be returned (saved) in the object
    #' @param saveAllResAt [character] If NULL, we won't save the simulation results. Else, we will save bsInfo and breederInfo for each iteration in the directory defined by `saveAllResAt` path.
    #' @param evaluateGVMethod [character] Which type of GV (true GV or estimated GV) will be used for evaluation of the simulation results
    #' @param nTopEval [numeric] Number of individuals to be evaluated when evaluating population max or population min
    #' @param traitNoEval [numeric] Trait No of your interest for evaluation of the method
    #' @param hEval [numeric] Hyperparameter which determines which BV is emphasized when evaluating simulation results of each method
    #' @param summaryAllResAt [character] If NULL, we will summary the simulation results in `self$trueGVMatList` and `self$estimatedGVMatList`.
    #' Else, we will summary the simulation results in `summaryAllResAt` path.
    #' @param verbose [logical] Display info (optional)
    #' @return A new `simBs` object.
    #' @examples
    #' ### create simulation information
    #' mySimInfo <- simInfo$new(simName = "Simulation Example",
    #'                          simGeno = TRUE,
    #'                          simPheno = TRUE,
    #'                          #' nSimGeno = 1,
    #'                          #' nSimPheno = 3,
    #'                          #' nCoreMax = 4,
    #'                          #' nCorePerGeno = 1,
    #'                          #' nCorePerPheno = 3,
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
    #' myTrait$plot(alphaMarker = 0.1)
    #
    #
    #
    #' ### create bsInfo object
    #' myBsInfo <- bsInfo$new(simInfo = mySimInfo,
    #'                        specie = mySpec,
    #'                        lociInfo = myLoci,
    #'                        traitInfo = myTrait,
    #'                        geno = NULL,
    #'                        haplo = NULL,
    #'                        founderIsInitPop = TRUE,
    #'                        popNameBase = "Population",
    #'                        initIndNames = NULL,
    #'                        verbose = TRUE)
    #
    #' ### create cross information object
    #' for (i in 1:10) {
    #'   myCrossInfo <- crossInfo$new(parentPopulation = myBsInfo$populations[[myBsInfo$generation]],
    #'                                method = "randomMate",
    #'                                nNextPop = 100)
    #
    #'   myBsInfo$nextGeneration(crossInfo = myCrossInfo)
    #' }
    #' geno <- myBsInfo$overGeneration()$genoMat
    #' myBsInfo$print()
    #' myBsInfo$plot(plotTarget = "trueAGV",
    #'               targetTrait = 1:3,
    #'               targetPopulation = 1:11,
    #'               plotType = "jitter")
    #'


    initialize = function(simBsName = "Undefined",
                          bsInfoInit,
                          breederInfoInit = NULL,
                          lociEffMethod = NULL,
                          trainingPopType = NULL,
                          trainingPopInit = NULL,
                          trainingIndNamesInit = NULL,
                          methodMLRInit = NULL,
                          multiTraitInit = FALSE,
                          samplingMrkEffInit = FALSE,
                          seedMrkEffSamplingInit = NA,
                          nIterSimulation = NULL,
                          nGenerationProceed = NULL,
                          nRefreshMemoryEvery = NULL,
                          updateBreederInfo = TRUE,
                          phenotypingInds = FALSE,
                          nRepForPhenoInit = NULL,
                          nRepForPheno = NULL,
                          updateModels = FALSE,
                          methodMLR = NULL,
                          multiTrait = FALSE,
                          nSelectionWaysVec = NULL,
                          selectionMethodList = NULL,
                          traitNoSelList = NULL,
                          blockSplitMethod = NULL,
                          nMrkInBlock = NULL,
                          minimumSegmentLength = NULL,
                          nSelInitOPVList = NULL,
                          nIterOPV = NULL,
                          nProgeniesEMBVVec = NULL,
                          nIterEMBV = NULL,
                          nCoresEMBV = NULL,
                          clusteringForSelList = FALSE,
                          nClusterList = NULL,
                          nTopClusterList = NULL,
                          nTopEachList = NULL,
                          nSelList = NULL,
                          multiTraitsEvalMethodList = NULL,
                          hSelList = NULL,
                          matingMethodVec = NULL,
                          allocateMethodVec = NULL,
                          weightedAllocationMethodList = NULL,
                          traitNoRAList = NULL,
                          hList = NULL,
                          minimumUnitAllocateVec = NULL,
                          includeGVPVec = FALSE,
                          nNextPopVec = NULL,
                          nameMethod = "pairBase",
                          nCores = NULL,
                          overWriteRes = FALSE,
                          showProgress = TRUE,
                          returnMethod = "all",
                          saveAllResAt = NULL,
                          evaluateGVMethod = "true",
                          nTopEval = NULL,
                          traitNoEval = NULL,
                          hEval = NULL,
                          summaryAllResAt = NULL,
                          verbose = TRUE) {

      # define some methods
      lociEffMethodsOffered <- c("true", "estimated")
      trainingPopTypesOffered <- c("all", "latest")
      supportedMethodsMLR <- c("Ridge", "LASSO", "ElasticNet", "RR-BLUP", "GBLUP",
                               "BayesA", "BayesB", "BayesC", "BRR", "BL", "SpikeSlab")
      supportedMethodsGlmnet <- c("Ridge", "LASSO", "ElasticNet")
      supportedMethodsBGLR <- c("BayesA", "BayesB", "BayesC", "BRR", "BL", "SpikeSlab")
      selectionMethodsOffered <- c("nonSelection", "selectBV", "selectWBV", "selectOHV",
                                   "selectEMBV", "selectOPV", "userSI", "userSpecific")
      selectionMethodsWithSelection <- c("selectBV", "selectWBV", "selectOHV",
                                         "selectEMBV", "selectOPV", "userSI", "userSpecific")
      selectionMethodsWithMrkEff <- c("selectBV", "selectWBV", "selectOHV", "selectEMBV", "selectOPV")
      matingMethodsOffered <- c("randomMate", "roundRobin", "diallel", "diallelWithSelfing",
                                "selfing", "maxGenDist", "makeDH", "nonCross", "userSpecific")
      allocateMethodsOffered <- c("equalAllocation", "weightedAllocation", "userSpecific")
      blockSplitMethodsOffered <- c("nMrkInBlock", "minimumSegmentLength")
      multiTraitsEvalMethodsOffered <- c("sum", "prod")
      nameMethodsOffered <- c("pairBase", "individualBase")
      returnMethodsOffered <- c("all", "summary", "max", "mean", "median", "min", "var")


      # simBsName
      if (is.null(simBsName)) {
        simBsName <- "Undefined"
      }
      stopifnot(is.character(simBsName))

      # bsInfoInit class
      if (class(bsInfoInit)[1] != "bsInfo") {
        stop(paste('class(bsInfoInit)[1] != "bsInfo"\n"bsInfo" must be a',
                   'bsInfo object see: ?bsInfo'))
      }



      # define some variables
      nIndNow <- bsInfoInit$populations[[length(bsInfoInit$populations)]]$nInd
      nTraits <- bsInfoInit$traitInfo$nTraits



      # nIterSimulation
      if (!is.null(nIterSimulation)) {
        stopifnot(is.numeric(nIterSimulation))
        nIterSimulation <- floor(nIterSimulation)
        stopifnot(nIterSimulation >= 1)
      } else {
        nIterSimulation <- 1
        message(paste0("`nIterSimulation` is not specified. We substitute `nIterSimulation = ",
                       nIterSimulation,"` instead."))
      }



      # nGenerationProceed
      if (!is.null(nGenerationProceed)) {
        stopifnot(is.numeric(nGenerationProceed))
        nGenerationProceed <- floor(nGenerationProceed)
        stopifnot(nGenerationProceed >= 1)
      } else {
        nGenerationProceed <- 1
        message(paste0("`nGenerationProceed` is not specified. We substitute `nGenerationProceed = ",
                       nGenerationProceed,"` instead."))
      }

      # nRefreshMemoryEvery
      if (!is.null(nRefreshMemoryEvery)) {
        stopifnot(is.numeric(nRefreshMemoryEvery))
        nRefreshMemoryEvery <- floor(nRefreshMemoryEvery)
      } else {
        nRefreshMemoryEvery <- 2
        message(paste0("`nRefreshMemoryEvery` is not specified. We substitute `nRefreshMemoryEvery = ",
                       nRefreshMemoryEvery,"` instead."))
      }

      # updateBreederInfo
      stopifnot(is.logical(updateBreederInfo))

      if (!(length(updateBreederInfo) %in% c(1, nGenerationProceed))) {
        stop(paste("length(updateBreederInfo) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(updateBreederInfo) == 1) {
        updateBreederInfo <- rep(updateBreederInfo, nGenerationProceed)
      }
      names(updateBreederInfo) <- 1:nGenerationProceed


      # phenotypingInds
      stopifnot(is.logical(phenotypingInds))

      if (!(length(phenotypingInds) %in% c(1, nGenerationProceed))) {
        stop(paste("length(phenotypingInds) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(phenotypingInds) == 1) {
        phenotypingInds <- rep(phenotypingInds, nGenerationProceed)
      }
      names(phenotypingInds) <- 1:nGenerationProceed
      phenotypingInds[!updateBreederInfo] <- FALSE


      # nRepForPhenoInit
      if (!is.null(nRepForPhenoInit)) {
        stopifnot(is.numeric(nRepForPhenoInit))
        nRepForPhenoInit <- floor(nRepForPhenoInit)
        stopifnot(nRepForPhenoInit >= 1)
      } else {
        nRepForPhenoInit <- 1
        message(paste0("`nRepForPhenoInit` is not specified. We substitute `nRepForPhenoInit = ",
                       nRepForPhenoInit,"` instead."))
      }


      # nRepForPheno
      if (!is.null(nRepForPheno)) {
        stopifnot(is.numeric(nRepForPheno))
        nRepForPheno <- floor(nRepForPheno)
        stopifnot(all(nRepForPheno >= 0))
      } else {
        nRepForPheno <- 1
        message(paste0("`nRepForPheno` is not specified. We substitute `nRepForPheno = ",
                       nRepForPheno,"` instead."))
      }

      if (!(length(nRepForPheno) %in% c(1, nGenerationProceed))) {
        stop(paste("length(nRepForPheno) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(nRepForPheno) == 1) {
        nRepForPheno <- rep(nRepForPheno, nGenerationProceed)
      }
      names(nRepForPheno) <- 1:nGenerationProceed

      nRepForPheno[!phenotypingInds] <- 0


      # lociEffMethod
      if (!is.null(lociEffMethod)) {
        if (!(lociEffMethod %in% lociEffMethodsOffered)) {
          stop(paste0("We only offer the following methods for naming individuals: ",
                      paste(lociEffMethodsOffered, collapse = "; ")))
        }
      } else {
        lociEffMethod <- "estimated"
        message(paste0("`lociEffMethod` is not specified. We substitute `lociEffMethod = ",
                       lociEffMethod,"` instead."))
      }


      # trainingPopType
      if (!is.null(trainingPopType)) {
        if (!(trainingPopType %in% trainingPopTypesOffered)) {
          stop(paste0("We only offer the following methods for naming individuals: ",
                      paste(trainingPopTypesOffered, collapse = "; ")))
        }
      } else {
        trainingPopType <- "latest"
        message(paste0("`trainingPopType` is not specified. We substitute `trainingPopType = ",
                       trainingPopType,"` instead."))
      }


      # multiTraitInit
      stopifnot(is.logical(multiTraitInit))

      # methodMLRInit
      if (!is.null(methodMLRInit)) {
        if (!(methodMLRInit %in% supportedMethodsMLR)) {
          stop(paste0("We only offer the following methods for naming individuals: ",
                      paste(supportedMethodsMLR, collapse = "; ")))
        }
      } else {
        if (multiTraitInit) {
          methodMLRInit <- "SpikeSlab"
        } else {
          methodMLRInit <- "BayesB"
        }
        message(paste0("`methodMLRInit` is not specified. We substitute `methodMLRInit = ",
                       methodMLRInit,"` instead."))
      }


      if (methodMLRInit %in% supportedMethodsBGLR) {
        if (!multiTraitInit) {
          if (methodMLRInit == "SpikeSlab") {
            message(paste0("For uni-trait model, `methodMLR = 'SpikeSlab'` is not offered.\n",
                           "We use `methodMLR = 'BayesC'` instead."))
            methodMLRInit <- "BayesC"
          }
        } else {
          if (methodMLRInit %in% c("BL", "BayesB", "BayesC")) {
            message(paste0("For multi-trait model, `methodMLR = 'BL'`, `methodMLR = 'BayesB'`, `methodMLR = 'BayesC'` are not offered.\n",
                           "We use `methodMLR = 'SpikeSlab'` instead. This is equivalent to BayesC model."))
            methodMLRInit <- "SpikeSlab"
          } else if (methodMLRInit == "BayesA") {
            message(paste0("For multi-trait model, `methodMLR = 'BayesA'` is not offered.\n",
                           "We use `methodMLR = 'BRR'` instead."))
            methodMLRInit <- "BRR"
          }
        }
      }


      # samplingMrkEffInit
      if (samplingMrkEffInit & (!(methodMLRInit %in% supportedMethodsBGLR))) {
        message(paste0("For non-bayesian model, `samplingMrkEffInit = TRUE` is not offered.\n",
                       "We use `samplingMrkEffInit = FALSE` instead."))
        samplingMrkEffInit <- FALSE
      }


      # seedMrkEffSamplingInit
      if (samplingMrkEffInit) {
        seedMrkEffSamplingInit <- NA
      } else {
        if (is.na(seedMrkEffSamplingInit)) {
          seedMrkEffSamplingInit <- sample(x = 1:1e09, size = 1)
        }
      }


      # updateModels
      stopifnot(is.logical(updateModels))

      if (!(length(updateModels) %in% c(1, nGenerationProceed))) {
        stop(paste("length(updateModels) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(updateModels) == 1) {
        updateModels <- rep(updateModels, nGenerationProceed)
      }
      names(updateModels) <- 1:nGenerationProceed

      updateModels[!phenotypingInds] <- FALSE


      # multiTrait
      stopifnot(is.logical(multiTrait))

      # methodMLR
      if (!is.null(methodMLR)) {
        if (!(methodMLR %in% supportedMethodsMLR)) {
          stop(paste0("We only offer the following methods for naming individuals: ",
                      paste(supportedMethodsMLR, collapse = "; ")))
        }
      } else {
        methodMLR <- "LASSO"
        message(paste0("`methodMLR` is not specified. We substitute `methodMLR = ",
                       methodMLR,"` instead."))
      }


      if (methodMLR %in% supportedMethodsBGLR) {
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
      }


      # breederInfoInit class
      if (!is.null(breederInfoInit)) {
        if (class(breederInfoInit)[1] != "breederInfo") {
          stop(paste('class(breederInfoInit)[1] != "breederInfo"\n"breederInfo" must be a',
                     'breederInfo object see: ?breederInfo'))
        }
      } else {
        breederInfoInit <- myBreedSimulatR::breederInfo$new(breederName = "Undefined",
                                                            bsInfo = bsInfoInit,
                                                            mrkNames = NULL,
                                                            initGenotyping = TRUE,
                                                            initGenotypedIndNames = NULL,
                                                            mafThres = 0.05,
                                                            heteroThres = 1,
                                                            calculateGRMBase = TRUE,
                                                            methodsGRMBase = "addNOIA",
                                                            calcEpistasisBase = FALSE,
                                                            verbose = verbose)
        breederInfoInit$phenotyper(bsInfo = bsInfoInit,
                                   generationOfInterest = NULL,
                                   nRep = nRepForPhenoInit,
                                   multiTraitsAsEnvs = FALSE,
                                   phenotypedIndNames = NULL,
                                   estimateGV = TRUE,
                                   estimatedGVMethod = "lme4")
      }


      # nSelectionWaysVec
      if (!is.null(nSelectionWaysVec)) {
        stopifnot(is.numeric(nSelectionWaysVec))
        nSelectionWaysVec <- floor(nSelectionWaysVec)
        stopifnot(all(nSelectionWaysVec >= 1))
      } else {
        nSelectionWaysVec <- 1
        message(paste0("`nSelectionWaysVec` is not specified. We substitute `nSelectionWaysVec = ",
                       nSelectionWaysVec,"` instead."))
      }

      if (!(length(nSelectionWaysVec) %in% c(1, nGenerationProceed))) {
        stop(paste("length(nSelectionWaysVec) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(nSelectionWaysVec) == 1) {
        nSelectionWaysVec <- rep(nSelectionWaysVec, nGenerationProceed)
      }
      names(nSelectionWaysVec) <- 1:nGenerationProceed


      # selectionMethodList
      if (!is.null(selectionMethodList)) {
        if (!is.list(selectionMethodList)) {
          selectionMethodList <- list(selectionMethodList)
        }
      } else {
        selectionMethodList <- "nonSelection"
        message(paste0("`selectionMethodList` is not specified. We substitute `selectionMethodList = list(",
                       selectionMethodList,")` instead."))
        selectionMethodList <- sapply(X = nSelectionWaysVec,
                                      FUN = function(nSelectionWays) {
                                        rep(selectionMethodList, nSelectionWays)
                                      }, simplify = FALSE)
      }

      if (!(length(selectionMethodList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(selectionMethodList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(selectionMethodList) == 1) {
        selectionMethodList <- rep(selectionMethodList, nGenerationProceed)
      }
      names(selectionMethodList) <- 1:nGenerationProceed
      stopifnot(all(unlist(lapply(selectionMethodList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(selectionMethodList, function(x) all(x %in% selectionMethodsOffered)))))

      whereSelectionList <- lapply(selectionMethodList, function(x) x %in% selectionMethodsWithSelection)


      # traitNoSelList
      if (!is.null(traitNoSelList)) {
        if (!is.list(traitNoSelList)) {
          traitNoSelList <- sapply(nSelectionWaysVec,
                                   function(nSelectionWays) {
                                     traitNoSelListNow <- rep(list(traitNoSelList), nSelectionWays)

                                     return(traitNoSelListNow)
                                   }, simplify = FALSE)
        } else if (!is.list(traitNoSelList[[1]])) {
          traitNoSelList <- sapply(nSelectionWaysVec,
                                   function(nSelectionWays) {
                                     traitNoSelListNow <- rep(traitNoSelList, nSelectionWays)

                                     return(traitNoSelListNow)
                                   }, simplify = FALSE)
        }
      } else {
        traitNoSelList <- 1
        message(paste0("`traitNoSelList` is not specified. We substitute `traitNoSelList = list(list(",
                       traitNoSelList,"))` instead."))
        traitNoSelList <- sapply(nSelectionWaysVec,
                                 function(nSelectionWays) {
                                   traitNoSelListNow <- rep(list(traitNoSelList), nSelectionWays)

                                   return(traitNoSelListNow)
                                 }, simplify = FALSE)
      }

      if (!(length(traitNoSelList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(traitNoSelList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(traitNoSelList) == 1) {
        traitNoSelList <- rep(traitNoSelList, nGenerationProceed)
      }
      stopifnot(all(unlist(lapply(traitNoSelList, is.list))))
      stopifnot(all(unlist(lapply(traitNoSelList, function(traitNoSel) all(unlist(lapply(traitNoSel, is.numeric)))))))
      stopifnot(all(sapply(traitNoSelList, function(traitNoSel) all(unlist(lapply(traitNoSel, function(x) all(x >= 1)))))))
      stopifnot(all(sapply(traitNoSelList, function(traitNoSel) all(unlist(lapply(traitNoSel, function(x) all(x <= nTraits)))))))
      stopifnot(all(unlist(lapply(traitNoSelList, length)) == nSelectionWaysVec))

      names(traitNoSelList) <- 1:nGenerationProceed


      # blockSplitMethod
      if (!is.null(blockSplitMethod)) {
        if (!(blockSplitMethod %in% blockSplitMethodsOffered)) {
          stop(paste0("We only offer the following block splitting methods: ",
                      paste(blockSplitMethodsOffered, collapse = "; ")))
        }
      } else {
        blockSplitMethod <- "minimumSegmentLength"
        message("You do not specify the block splitting method. We will define blocks by the minimum length of segements.")
      }


      # nMrkInBlock
      if (!is.null(nMrkInBlock)) {
        stopifnot(is.numeric(nMrkInBlock))
        nMrkInBlock <- floor(nMrkInBlock)
        stopifnot(nMrkInBlock >= 1)
        stopifnot(nMrkInBlock <= min(bsInfoInit$specie$nLoci))
      } else {
        nMrkInBlock <- min(bsInfoInit$specie$nLoci) %/% 10

        if (any(unlist(lapply(selectionMethodList, function(selectionMethod) c("selectOHV", "selectOPV") %in% selectionMethod)))) {
          message(paste0("`nMrkInBlock` is not specified even though you choose 'selectOHV' / 'selectOPV' method. We substitute `nMrkInBlock = ", nMrkInBlock,"` instead."))
        }
      }


      # minimumSegmentLength
      if (!is.null(minimumSegmentLength)) {
        stopifnot(is.numeric(minimumSegmentLength))
        minimumSegmentLength <- floor(minimumSegmentLength)
        stopifnot(minimumSegmentLength >= 1)
        stopifnot(minimumSegmentLength <= min(bsInfoInit$specie$lChr))
      } else {
        minimumSegmentLength <- 6.25
        if (any(unlist(lapply(selectionMethodList, function(selectionMethod) c("selectOHV", "selectOPV") %in% selectionMethod)))) {
          message(paste0("`nMrkInBlock` is not specified even though you choose 'selectOHV' / 'selectOPV' method. We substitute `nMrkInBlock = ", nMrkInBlock,"` instead."))
        }
      }



      # nIterOPV
      if (!is.null(nIterOPV)) {
        stopifnot(is.numeric(nIterOPV))
        nIterOPV <- floor(nIterOPV)
        stopifnot(nIterOPV >= 1)
      } else {
        nIterOPV <- 5000
        if (any(unlist(lapply(selectionMethodList, function(selectionMethod) "selectOPV" %in% selectionMethod)))) {
          message(paste0("`nIterOPV` is not specified even though you choose 'selectOPV' method. We substitute `nIterOPV = ", nIterOPV,"` instead."))
        }
      }



      # clusteringForSelList
      if (!is.null(clusteringForSelList)) {
        if (!is.list(clusteringForSelList)) {
          clusteringForSelList <- list(clusteringForSelList)
        }
      } else {
        clusteringForSelList <- FALSE
        message(paste0("`clusteringForSelList` is not specified. We substitute `clusteringForSelList = list(",
                       clusteringForSelList,")` instead."))
        clusteringForSelList <- sapply(X = nSelectionWaysVec,
                                       FUN = function(nSelectionWays) {
                                         rep(clusteringForSelList, nSelectionWays)
                                       }, simplify = FALSE)
      }

      if (!(length(clusteringForSelList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(clusteringForSelList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(clusteringForSelList) == 1) {
        clusteringForSelList <- rep(clusteringForSelList, nGenerationProceed)
      }
      names(clusteringForSelList) <- 1:nGenerationProceed
      stopifnot(all(unlist(lapply(clusteringForSelList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(clusteringForSelList, is.logical))))




      # nSelList
      if (!is.null(nSelList)) {
        if (!is.list(nSelList)) {
          nSelList <- list(nSelList)
        }
      } else {
        nSelList <- nIndNow %/% 10
        message(paste0("`nSelList` is not specified. We substitute `nSelList = list(",
                       nSelList,")` instead."))
        nSelList <- sapply(X = nSelectionWaysVec,
                           FUN = function(nSelectionWays) {
                             rep(nSelList, nSelectionWays)
                           }, simplify = FALSE)
      }

      if (!(length(nSelList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(nSelList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(nSelList) == 1) {
        nSelList <- rep(nSelList, nGenerationProceed)
      }
      stopifnot(all(unlist(lapply(nSelList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(nSelList, is.numeric))))

      nSelList <- sapply(X = 1:nGenerationProceed,
                         FUN = function(generationProceedNo) {
                           nSel <- nSelList[[generationProceedNo]]
                           whereSelection <- whereSelectionList[[generationProceedNo]]

                           nSel[!whereSelection] <- nIndNow

                           return(nSel)
                         }, simplify = FALSE)


      names(nSelList) <- 1:nGenerationProceed


      # nSelInitOPVList
      if (!is.null(nSelInitOPVList)) {
        if (!is.list(nSelInitOPVList)) {
          nSelInitOPVList <- list(nSelInitOPVList)
        }
      } else {
        nSelInitOPVList <- nIndNow %/% 2
        if (any(unlist(lapply(selectionMethodList, function(selectionMethod) "selectOPV" %in% selectionMethod)))) {
          message(paste0("`nSelInitOPVList` is not specified. We substitute `nSelInitOPVList = list(",
                         nSelInitOPVList,")` instead."))
        }
        nSelInitOPVList <- sapply(X = nSelectionWaysVec,
                                  FUN = function(nSelectionWays) {
                                    rep(nSelInitOPVList, nSelectionWays)
                                  }, simplify = FALSE)
      }

      if (!(length(nSelInitOPVList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(nSelInitOPVList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(nSelInitOPVList) == 1) {
        nSelInitOPVList <- rep(nSelInitOPVList, nGenerationProceed)
      }
      stopifnot(all(unlist(lapply(nSelInitOPVList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(nSelInitOPVList, is.numeric))))
      stopifnot(all(unlist(lapply(nSelInitOPVList, is.numeric))))

      whereNSelSatisfy <- unlist(mapply(FUN = function(nSelInitOPV, nSel) {
        all(nSelInitOPV >= nSel)
      },
      nSelInitOPVList,
      nSelList))

      if (any(!whereNSelSatisfy)) {
        if (any(lapply(selectionMethodList, function(selectionMethod) "selectOPV" %in% selectionMethod))) {
          message("`nSelInitOPV` should be larger than `nSel`. We substitute `nSelInitOPV` by `nSel` when `nSelInitOPV` is smaller than `nSel`.")
        }

        nSelInitOPVList[whereNSelSatisfy] <- sapply(X = (1:nGenerationProceed)[whereNSelSatisfy],
                                                    FUN = function(generationProceedNo) {
                                                      nSelInitOPV <- nSelInitOPVList[generationProceedNo]
                                                      nSel <- nSelList[generationProceedNo]

                                                      nSelInitOPV[nSelInitOPV < nSel] <- nSel[nSelInitOPV < nSel]

                                                      return(nSelInitOPV)
                                                    }, simplify = FALSE)
      }

      names(nSelInitOPVList) <- 1:nGenerationProceed




      # nClusterList
      if (!is.null(nClusterList)) {
        if (!is.list(nClusterList)) {
          nClusterList <- list(nClusterList)
        }
      } else {
        nClusterList <- 5
        message(paste0("`nClusterList` is not specified. We substitute `nClusterList = list(",
                       nClusterList,")` instead."))
        nClusterList <- sapply(X = nSelectionWaysVec,
                               FUN = function(nSelectionWays) {
                                 rep(nClusterList, nSelectionWays)
                               }, simplify = FALSE)
      }

      if (!(length(nClusterList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(nClusterList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(nClusterList) == 1) {
        nClusterList <- rep(nClusterList, nGenerationProceed)
      }
      stopifnot(all(unlist(lapply(nClusterList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(nClusterList, is.numeric))))

      nClusterList <- sapply(X = 1:nGenerationProceed,
                             FUN = function(generationProceedNo) {
                               nCluster <- nClusterList[[generationProceedNo]]
                               clusteringForSel <- clusteringForSelList[[generationProceedNo]]

                               nCluster[!clusteringForSel] <- 1

                               return(nCluster)
                             }, simplify = FALSE)


      names(nClusterList) <- 1:nGenerationProceed


      # nTopClusterList
      if (!is.null(nTopClusterList)) {
        if (!is.list(nTopClusterList)) {
          nTopClusterList <- list(nTopClusterList)
        }
      } else {
        nTopClusterList <- 5
        message(paste0("`nTopClusterList` is not specified. We substitute `nTopClusterList = list(",
                       nTopClusterList,")` instead."))
        nTopClusterList <- sapply(X = nSelectionWaysVec,
                                  FUN = function(nSelectionWays) {
                                    rep(nTopClusterList, nSelectionWays)
                                  }, simplify = FALSE)
      }

      if (!(length(nTopClusterList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(nTopClusterList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(nTopClusterList) == 1) {
        nTopClusterList <- rep(nTopClusterList, nGenerationProceed)
      }
      stopifnot(all(unlist(lapply(nTopClusterList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(nTopClusterList, is.numeric))))

      nTopClusterList <- sapply(X = 1:nGenerationProceed,
                                FUN = function(generationProceedNo) {
                                  nTopCluster <- nTopClusterList[[generationProceedNo]]
                                  clusteringForSel <- clusteringForSelList[[generationProceedNo]]

                                  nTopCluster[!clusteringForSel] <- 1

                                  return(nTopCluster)
                                }, simplify = FALSE)


      names(nTopClusterList) <- 1:nGenerationProceed



      # nTopEachList
      if (!is.null(nTopEachList)) {
        if (!is.list(nTopEachList)) {
          nTopEachList <- list(nTopEachList)
        }
      } else {
        nTopEachList <- sapply(X = 1:nGenerationProceed,
                               FUN = function(generationProceedNo) {
                                 nTopEach <- nTopEachList[[generationProceedNo]]
                                 nSel <- nSelList[[generationProceedNo]]
                                 nTopCluster <- nTopClusterList[[generationProceedNo]]

                                 nTopEach <- nSel %/% nTopCluster

                                 return(nTopEach)
                               }, simplify = FALSE)
        message(paste0("`nTopEachList` is not specified. We substitute `nTopEachList = list(",
                       nTopEachList,")` instead."))
      }

      if (!(length(nTopEachList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(nTopEachList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(nTopEachList) == 1) {
        nTopEachList <- rep(nTopEachList, nGenerationProceed)
      }
      stopifnot(all(unlist(lapply(nTopEachList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(nTopEachList, is.numeric))))

      names(nTopEachList) <- 1:nGenerationProceed


      # multiTraitsEvalMethodList
      if (!is.null(multiTraitsEvalMethodList)) {
        if (!is.list(multiTraitsEvalMethodList)) {
          multiTraitsEvalMethodList <- list(multiTraitsEvalMethodList)
        }
      } else {
        multiTraitsEvalMethodList <- "sum"
        message(paste0("`multiTraitsEvalMethodList` is not specified. We substitute `multiTraitsEvalMethodList = list(",
                       multiTraitsEvalMethodList,")` instead."))
        multiTraitsEvalMethodList <- sapply(X = nSelectionWaysVec,
                                            FUN = function(nSelectionWays) {
                                              rep(multiTraitsEvalMethodList, nSelectionWays)
                                            }, simplify = FALSE)
      }

      if (!(length(multiTraitsEvalMethodList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(multiTraitsEvalMethodList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(multiTraitsEvalMethodList) == 1) {
        multiTraitsEvalMethodList <- rep(multiTraitsEvalMethodList, nGenerationProceed)
      }
      names(multiTraitsEvalMethodList) <- 1:nGenerationProceed
      stopifnot(all(unlist(lapply(multiTraitsEvalMethodList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(multiTraitsEvalMethodList, function(x) all(x %in% multiTraitsEvalMethodsOffered)))))



      # hSelList
      if (!is.null(hSelList)) {
        if (!is.list(hSelList)) {
          hSelList <- sapply(nSelectionWaysVec,
                             function(nSelectionWays) {
                               hSelListNow <- rep(list(hSelList), nSelectionWays)

                               return(hSelListNow)
                             }, simplify = FALSE)
        } else if (!is.list(hSelList[[1]])) {
          hSelList <- sapply(nSelectionWaysVec,
                             function(nSelectionWays) {
                               hSelListNow <- rep(hSelList, nSelectionWays)

                               return(hSelListNow)
                             }, simplify = FALSE)
        }
      } else {
        hSelList <- 1
        message(paste0("`hSelList` is not specified. We substitute `hSelList = list(list(",
                       hSelList,"))` instead."))
        hSelList <- sapply(1:nGenerationProceed,
                           function(generationProceedNo) {

                             hSelListNow <- sapply(X = traitNoSelList[[generationProceedNo]],
                                                   FUN = function(traitNoSelNow) {
                                                     rep(hSelList, length(traitNoSelNow))
                                                   }, simplify = FALSE)

                             return(hSelListNow)
                           }, simplify = FALSE)
      }

      if (!(length(hSelList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(hSelList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(hSelList) == 1) {
        hSelList <- rep(hSelList, nGenerationProceed)
      }
      stopifnot(all(unlist(lapply(hSelList, is.list))))
      stopifnot(all(unlist(lapply(hSelList, function(hSel) all(unlist(lapply(hSel, is.numeric)))))))
      stopifnot(all(sapply(hSelList, function(hSel) all(unlist(lapply(hSel, function(x) all(x >= 0)))))))
      stopifnot(all(unlist(lapply(hSelList, length)) == nSelectionWaysVec))

      names(hSelList) <- 1:nGenerationProceed

      stopifnot(all(unlist(lapply(X = hSelList, FUN = function(x) lapply(x, length))) ==
                      unlist(lapply(X = traitNoSelList, FUN = function(x) lapply(x, length)))))


      # matingMethodVec
      if (!is.null(matingMethodVec)) {
        stopifnot(is.character(matingMethodVec))
        stopifnot(all(matingMethodVec %in% matingMethodsOffered))
      } else {
        matingMethodVec <- "randomMate"
        message(paste0("`matingMethodVec` is not specified. We substitute `matingMethodVec = ",
                       matingMethodVec,"` instead."))
      }

      if (!(length(matingMethodVec) %in% c(1, nGenerationProceed))) {
        stop(paste("length(matingMethodVec) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(matingMethodVec) == 1) {
        matingMethodVec <- rep(matingMethodVec, nGenerationProceed)
      }
      names(matingMethodVec) <- 1:nGenerationProceed


      # allocateMethodVec
      if (!is.null(allocateMethodVec)) {
        stopifnot(is.character(allocateMethodVec))
        stopifnot(all(allocateMethodVec %in% allocateMethodsOffered))
      } else {
        allocateMethodVec <- "equalAllocation"
        message(paste0("`allocateMethodVec` is not specified. We substitute `allocateMethodVec = ",
                       allocateMethodVec,"` instead."))
      }

      if (!(length(allocateMethodVec) %in% c(1, nGenerationProceed))) {
        stop(paste("length(allocateMethodVec) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(allocateMethodVec) == 1) {
        allocateMethodVec <- rep(allocateMethodVec, nGenerationProceed)
      }
      names(allocateMethodVec) <- 1:nGenerationProceed



      # weightedAllocationMethodList
      if (!is.null(weightedAllocationMethodList)) {
        if (!is.list(weightedAllocationMethodList)) {
          weightedAllocationMethodList <- list(weightedAllocationMethodList)
        }
      } else {
        weightedAllocationMethodList <- "selectBV"
        message(paste0("`weightedAllocationMethodList` is not specified. We substitute `weightedAllocationMethodList = list(",
                       weightedAllocationMethodList,")` instead."))
        weightedAllocationMethodList <- list(weightedAllocationMethodList)
      }

      if (!(length(weightedAllocationMethodList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(weightedAllocationMethodList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(weightedAllocationMethodList) == 1) {
        weightedAllocationMethodList <- rep(weightedAllocationMethodList, nGenerationProceed)
      }
      weightedAllocationMethodList <- lapply(weightedAllocationMethodList, function(x) x[x %in% selectionMethodsWithSelection])

      names(weightedAllocationMethodList) <- 1:nGenerationProceed
      stopifnot(all(unlist(lapply(weightedAllocationMethodList, is.character))))
      stopifnot(all(unlist(lapply(weightedAllocationMethodList, length)) >= 1))



      # traitNoRAList
      if (!is.null(traitNoRAList)) {
        if (!is.list(traitNoRAList)) {
          traitNoRAList <- list(traitNoRAList)
        }
      } else {
        traitNoRAList <- 1
        message(paste0("`traitNoRAList` is not specified. We substitute `traitNoRAList = list(",
                       traitNoRAList,")` instead."))
        traitNoRAList <- list(traitNoRAList)
      }

      if (!(length(traitNoRAList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(traitNoRAList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(traitNoRAList) == 1) {
        traitNoRAList <- rep(traitNoRAList, nGenerationProceed)
      }
      stopifnot(all(unlist(lapply(traitNoRAList, is.numeric))))
      stopifnot(all(sapply(traitNoRAList, function(traitNoRA) all(traitNoRA >= 1))))

      names(traitNoRAList) <- 1:nGenerationProceed


      # includeGVPVec
      stopifnot(is.logical(includeGVPVec))

      if (!(length(includeGVPVec) %in% c(1, nGenerationProceed))) {
        stop(paste("length(includeGVPVec) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(includeGVPVec) == 1) {
        includeGVPVec <- rep(includeGVPVec, nGenerationProceed)
      }
      names(includeGVPVec) <- 1:nGenerationProceed


      # hList
      if (!is.null(hList)) {
        if (!is.list(hList)) {
          hList <- list(hList)
        }
      } else {
        hList <- 0.1
        message(paste0("`hList` is not specified. We substitute `hList = list(",
                       hList,")` instead."))
        hList <- sapply(X = 1:nGenerationProceed,
                        FUN = function(generationProceedNo) {
                          hLenNow <- length(traitNoRAList[[generationProceedNo]]) *
                            (length(weightedAllocationMethodList[[generationProceedNo]]) +
                               includeGVPVec[generationProceedNo])

                          return(rep(hList, hLenNow))
                        }, simplify = FALSE)
      }

      if (!(length(hList) %in% c(1, nGenerationProceed))) {
        stop(paste("length(hList) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(hList) == 1) {
        hList <- rep(hList, nGenerationProceed)
      }
      stopifnot(all(unlist(lapply(hList, is.numeric))))
      stopifnot(all(sapply(hList, function(h) all(h >= 0))))
      stopifnot(all(sapply(hList, function(h) all(h <= 10))))
      stopifnot(all(unlist(lapply(hList, length)) == (unlist(lapply(traitNoRAList, length)) *
                                                        (unlist(lapply(weightedAllocationMethodList, length)) + includeGVPVec))))

      names(hList) <- 1:nGenerationProceed



      # minimumUnitAllocateVec
      if (!is.null(minimumUnitAllocateVec)) {
        stopifnot(is.numeric(minimumUnitAllocateVec))
        minimumUnitAllocateVec <- floor(minimumUnitAllocateVec)
        stopifnot(all(minimumUnitAllocateVec >= 1))
      } else {
        minimumUnitAllocateVec <- 1
        message(paste0("`minimumUnitAllocateVec` is not specified. We substitute `minimumUnitAllocateVec = ",
                       minimumUnitAllocateVec,"` instead."))
      }

      if (!(length(minimumUnitAllocateVec) %in% c(1, nGenerationProceed))) {
        stop(paste("length(minimumUnitAllocateVec) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(minimumUnitAllocateVec) == 1) {
        minimumUnitAllocateVec <- rep(minimumUnitAllocateVec, nGenerationProceed)
      }
      names(minimumUnitAllocateVec) <- 1:nGenerationProceed



      # nNextPopVec
      if (!is.null(nNextPopVec)) {
        stopifnot(is.numeric(nNextPopVec))
        nNextPopVec <- floor(nNextPopVec)
        stopifnot(all(nNextPopVec >= 1))
      } else {
        nNextPopVec <- nIndNow
        message(paste0("`nNextPopVec` is not specified. We substitute `nNextPopVec = ",
                       nNextPopVec,"` instead."))
      }

      if (!(length(nNextPopVec) %in% c(1, nGenerationProceed))) {
        stop(paste("length(nNextPopVec) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(nNextPopVec) == 1) {
        nNextPopVec <- rep(nNextPopVec, nGenerationProceed)
      }
      names(nNextPopVec) <- 1:nGenerationProceed



      # nProgeniesEMBV
      if (!is.null(nProgeniesEMBVVec)) {
        stopifnot(is.numeric(nProgeniesEMBVVec))
        nProgeniesEMBVVec <- floor(nProgeniesEMBVVec)
        stopifnot(all(nProgeniesEMBVVec >= 1))
      } else {
        nProgeniesEMBVVec <- round(2 * nNextPopVec / min(unlist(lapply(nSelList, sum)), nNextPopVec))
        if (any(unlist(lapply(selectionMethodList, function(selectionMethod) "selectEMBV" %in% selectionMethod)))) {
          message(paste0("`nProgeniesEMBVVec` is not specified. We substitute `nProgeniesEMBVVec = c(",
                         paste(nProgeniesEMBVVec, collapse = ", "),
                         ")` instead."))
        }
      }


      if (!(length(nProgeniesEMBVVec) %in% c(1, nGenerationProceed))) {
        stop(paste("length(nProgeniesEMBVVec) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(nProgeniesEMBVVec) == 1) {
        nProgeniesEMBVVec <- rep(nProgeniesEMBVVec, nGenerationProceed)
      }
      names(nProgeniesEMBVVec) <- 1:nGenerationProceed


      # nIterEMBV
      if (!is.null(nIterEMBV)) {
        stopifnot(is.numeric(nIterEMBV))
        nIterEMBV <- floor(nIterEMBV)
        stopifnot(nIterEMBV >= 1)
      } else {
        nIterEMBV <- 10
        if (any(unlist(lapply(selectionMethodList, function(selectionMethod) "selectEMBV" %in% selectionMethod)))) {
          message(paste0("`nIterEMBV` is not specified. We substitute `nIterEMBV = ", nIterEMBV,"` instead."))
        }
      }


      # nCoresEMBV
      if (!is.null(nCoresEMBV)) {
        stopifnot(is.numeric(nCoresEMBV))
        nCoresEMBV <- floor(nCoresEMBV)
        stopifnot(nCoresEMBV >= 1)
      } else {
        nCoresEMBV <- 1
        if (any(unlist(lapply(selectionMethodList, function(selectionMethod) "selectEMBV" %in% selectionMethod)))) {
          message(paste0("`nCoresEMBV` is not specified. We substitute `nCoresEMBV = ",
                         nCoresEMBV,"` instead."))
        }
      }

      if (nCoresEMBV >= parallel::detectCores()) {
        warning("You are going to assign the number of cores larger than that of your PC to `nCoresEMBV` ! Is it OK ?")
      }



      # nameMethod
      if (!is.null(nameMethod)) {
        if (!(nameMethod %in% nameMethodsOffered)) {
          stop(paste0("We only offer the following methods for naming individuals: ",
                      paste(nameMethodsOffered, collapse = "; ")))
        }
      }

      # returnMethod
      if (!is.null(returnMethod)) {
        if (!all(returnMethod %in% returnMethodsOffered)) {
          returnMethod <- returnMethod[returnMethod %in% returnMethodsOffered]
          message(paste0("We only offer the following methods for returining the simulation results: ",
                         paste(returnMethodsOffered, collapse = "; ")))

          stopifnot(length(returnMethod) >= 1)
        }
      } else {
        returnMethod <- "summary"
      }

      # evaluateGVMethod
      if (!is.null(evaluateGVMethod)) {
        if (!(evaluateGVMethod %in% lociEffMethodsOffered)) {
          stop(paste0("We only offer the following methods for evaluating the simulation results: ",
                      paste(lociEffMethodsOffered, collapse = "; ")))
        }
      } else {
        evaluateGVMethod <- "true"
      }


      # nTopEval
      if (!is.null(nTopEval)) {
        stopifnot(is.numeric(nTopEval))
        nTopEval <- floor(nTopEval)
        stopifnot(nTopEval >= 1)
        stopifnot(nTopEval <= min(nNextPopVec))
      } else {
        nTopEval <- 1
        message(paste0("`nTopEval` is not specified. We substitute `nTopEval = ",
                       nTopEval,"` instead."))
      }


      # traitNoEval
      if (!is.null(traitNoEval)) {
        stopifnot(is.numeric(traitNoEval))
        traitNoEval <- floor(traitNoEval)
        stopifnot(all(traitNoEval >= 1))
        stopifnot(all(traitNoEval <= nTraits))
      } else {
        traitNoEval <- 1
        message(paste0("`traitNoEval` is not specified. We substitute `traitNoEval = ", traitNoEval,"` instead."))
      }


      # hEval
      if (!is.null(hEval)) {
        stopifnot(is.numeric(hEval))
        stopifnot(all(hEval >= 0))
      } else {
        hEval <- 0.1
        message(paste0("`hEval` is not specified. We substitute `hEval = ", hEval, "` instead."))
      }

      hEvalLen <- length(traitNoEval)

      if (!(length(hEval) %in% c(1, hEvalLen))) {
        stop(paste("length(hEval) must be equal to 1 or equal to `length(traitNoEval)`"))
      } else if (length(hEval) == 1) {
        hEval <- rep(hEval, hEvalLen)
      }


      # nCores
      if (!is.null(nCores)) {
        stopifnot(is.numeric(nCores))
        nCores <- floor(nCores)
        stopifnot(nCores >= 1)
      } else {
        nCores <- 1
        message(paste0("`nCores` is not specified. We substitute `nCores = ",
                       nCores,"` instead."))
      }

      if (nCores >= parallel::detectCores()) {
        warning("You are going to assign the number of cores larger than that of your PC to `nCores` ! Is it OK ?")
      }


      estimatedGVInitExist <- !is.null(breederInfoInit$estimatedGVByMLRInfo[[names(bsInfoInit$populations[length(bsInfoInit$populations)])]])
      if (!estimatedGVInitExist) {
        # trainingPopInit
        if (is.null(trainingPopInit)) {
          if (trainingPopType == "all") {
            trainingPopInit <- 1:length(breederInfoInit$populationsFB)
          } else {
            trainingPopInit <- length(breederInfoInit$populationsFB)
          }
        } else {
          stopifnot(is.numeric(trainingPopInit))
          trainingPopInit <- floor(trainingPopInit)
          if (!all(trainingPopInit %in% (1:bsInfoInit$generation))) {
            message(paste0("The following population cannot be used for initial training population; ",
                           paste(trainingPopInit[!(trainingPopInit %in% (1:bsInfoInit$generation))], collapse = ", ")))
            trainingPopInit <- trainingPopInit[trainingPopInit %in% (1:bsInfoInit$generation)]
          }
        }

        # trainingIndNamesInit
        if (!is.null(trainingIndNamesInit)) {
          stopifnot(is.character(trainingIndNamesInit))
          stopifnot(length(trainingIndNamesInit) >= 1)
        }
      }


      # saveAllResAt
      if (!is.null(saveAllResAt)) {
        stopifnot(is.character(saveAllResAt))
        tailCheck <- TRUE

        while (tailCheck) {
          strLength <- stringr::str_length(string = saveAllResAt)
          tailCheck <- stringr::str_sub(string = saveAllResAt, start = strLength, end = strLength) == "/"
          if (tailCheck) {
            saveAllResAt <- stringr::str_sub(string = saveAllResAt, start = 1, end = strLength - 1)
          }
        }

        if (!dir.exists(paths = saveAllResAt)) {
          dir.create(path = saveAllResAt)
        }
      }


      # summaryAllResAt
      if (!is.null(summaryAllResAt)) {
        stopifnot(is.character(summaryAllResAt))
        tailCheck <- TRUE

        while (tailCheck) {
          strLength <- stringr::str_length(string = summaryAllResAt)
          tailCheck <- stringr::str_sub(string = summaryAllResAt, start = strLength, end = strLength) == "/"
          if (tailCheck) {
            summaryAllResAt <- stringr::str_sub(string = summaryAllResAt, start = 1, end = strLength - 1)
          }
        }


        if (!dir.exists(paths = summaryAllResAt)) {
          message(paste0("There is no directory named '", summaryAllResAt, "'. We will summarize the simulation results inside the `simBs` object."))
          summaryAllResAt <- NULL
        }
      }



      # Save arguments in `self`
      self$simBsName <- simBsName
      self$bsInfoInit <- bsInfoInit
      self$breederInfoInit <- breederInfoInit
      self$lociEffMethod <- lociEffMethod
      self$trainingPopType <- trainingPopType
      self$trainingPopInit <- trainingPopInit
      self$trainingIndNamesInit <- trainingIndNamesInit
      self$methodMLRInit <- methodMLRInit
      self$multiTraitInit <- multiTraitInit
      self$samplingMrkEffInit <- samplingMrkEffInit
      self$seedMrkEffSamplingInit <- seedMrkEffSamplingInit
      self$nIterSimulation <- nIterSimulation
      self$nGenerationProceed <- nGenerationProceed
      self$nRefreshMemoryEvery <- nRefreshMemoryEvery
      self$updateBreederInfo <- updateBreederInfo
      self$phenotypingInds <- phenotypingInds
      self$nRepForPhenoInit <- nRepForPhenoInit
      self$nRepForPheno <- nRepForPheno
      self$updateModels <- updateModels
      self$methodMLR <- methodMLR
      self$multiTrait <- multiTrait
      self$nSelectionWaysVec <- nSelectionWaysVec
      self$selectionMethodList <- selectionMethodList
      self$traitNoSelList <- traitNoSelList
      self$blockSplitMethod <- blockSplitMethod
      self$nMrkInBlock <- nMrkInBlock
      self$minimumSegmentLength <- minimumSegmentLength
      self$nSelInitOPVList <- nSelInitOPVList
      self$nIterOPV <- nIterOPV
      self$nProgeniesEMBVVec <- nProgeniesEMBVVec
      self$nIterEMBV <- nIterEMBV
      self$nCoresEMBV <- nCoresEMBV
      self$clusteringForSelList <- clusteringForSelList
      self$nClusterList <- nClusterList
      self$nTopClusterList <- nTopClusterList
      self$nTopEachList <- nTopEachList
      self$nSelList <- nSelList
      self$multiTraitsEvalMethodList <- multiTraitsEvalMethodList
      self$hSelList <- hSelList
      self$matingMethodVec <- matingMethodVec
      self$allocateMethodVec <- allocateMethodVec
      self$weightedAllocationMethodList <- weightedAllocationMethodList
      self$traitNoRAList <- traitNoRAList
      self$hList <- hList
      self$minimumUnitAllocateVec <- minimumUnitAllocateVec
      self$includeGVPVec <- includeGVPVec
      self$nNextPopVec <- nNextPopVec
      self$nameMethod <- nameMethod
      self$nCores <- nCores
      self$overWriteRes <- overWriteRes
      self$showProgress <- showProgress
      self$returnMethod <- returnMethod
      self$saveAllResAt <- saveAllResAt
      self$evaluateGVMethod <- evaluateGVMethod
      self$nTopEval <- nTopEval
      self$traitNoEval <- traitNoEval
      self$hEval <- hEval
      self$summaryAllResAt <- summaryAllResAt
      self$verbose <- verbose




      # trueGVMatList
      trueGVMatInit <- list(bsInfoInit$populations[[length(bsInfoInit$populations)]]$trueGVMat)
      names(trueGVMatInit) <- bsInfoInit$populations[[length(bsInfoInit$populations)]]$name
      trueGVMatInitList <- rep(list(trueGVMatInit), nIterSimulation)
      names(trueGVMatInitList) <- paste0("Iteration_", 1:nIterSimulation)

      self$trueGVMatInit <- trueGVMatInit[[1]]
      self$trueGVMatList <- trueGVMatInitList


      # estimatedGVMatList
      # if (!estimatedGVInitExist) {
      #   self$computeLociEffInit()
      #   breederInfoInit$estimateGVByMLR(trainingPop = trainingPopInit,
      #                                   trainingIndNames = trainingIndNamesInit,
      #                                   testingPop = length(breederInfoInit$populationsFB),
      #                                   methodMLR = methodMLRInit,
      #                                   multiTrait = multiTraitInit,
      #                                   alpha = 0.5,
      #                                   nIter = 12000,
      #                                   burnIn = 3000,
      #                                   thin = 5,
      #                                   bayesian = TRUE)
      # }
      #
      #
      # estimatedGVMatInit <- list(breederInfoInit$estimatedGVByMLRInfo[[names(bsInfoInit$populations[length(bsInfoInit$populations)])]]$testingEstimatedGVByMLR)

      self$computeLociEffInit()
      lociEffectsInit <- self$lociEffectsInit
      genoMatNow <- bsInfoInit$populations[[length(bsInfoInit$populations)]]$genoMat
      genoMatWithIntNow <- cbind(Intercept = rep(1, nrow(genoMatNow)),
                                 genoMatNow)

      estimatedGVMatInit <- list(genoMatWithIntNow[, rownames(lociEffectsInit)] %*% lociEffectsInit)
      names(estimatedGVMatInit) <- bsInfoInit$populations[[length(bsInfoInit$populations)]]$name
      estimatedGVMatInitList <- rep(list(estimatedGVMatInit), nIterSimulation)
      names(estimatedGVMatInitList) <- paste0("Iteration_", 1:nIterSimulation)

      self$estimatedGVMatInit <- estimatedGVMatInit[[1]]
      self$estimatedGVMatList <- estimatedGVMatInitList
    },


    #' @description
    #' estimate/extract marker/QTL effects information
    computeLociEffInit = function() {
      # Read arguments from `self`
      bsInfoInit <- self$bsInfoInit
      breederInfoInit <- self$breederInfoInit
      lociEffMethod <- self$lociEffMethod
      trainingPopType <- self$trainingPopType
      trainingPopInit <- self$trainingPopInit
      trainingIndNamesInit <- self$trainingIndNamesInit
      methodMLRInit <- self$methodMLRInit
      multiTraitInit <- self$multiTraitInit
      samplingMrkEffInit <- self$samplingMrkEffInit
      seedMrkEffSamplingInit <- self$seedMrkEffSamplingInit
      verbose <- self$verbose



      if (lociEffMethod == "true") {
        lociEffectsInit <- bsInfoInit$lociEffects
      } else if (lociEffMethod == "estimated") {
        trainingPopName <- names(breederInfoInit$populationsFB)[trainingPopInit]
        estimatedMrkEffName <- paste0(trainingPopName[length(trainingPopName)], "_", methodMLRInit)

        if (is.null(breederInfoInit$estimatedMrkEffInfo[[estimatedMrkEffName]])) {
          breederInfoInit$estimateMrkEff(trainingPop = trainingPopInit,
                                         trainingIndNames = trainingIndNamesInit,
                                         methodMLR = methodMLRInit,
                                         multiTrait = multiTraitInit,
                                         alpha = 0.5,
                                         nIter = 12000,
                                         burnIn = 3000,
                                         thin = 5,
                                         bayesian = TRUE)
        }
        lociEffectsInit <- breederInfoInit$lociEffects(bsInfo = bsInfoInit,
                                                       trainingPop = trainingPopInit,
                                                       methodMLR = methodMLRInit,
                                                       samplingMrkEff = samplingMrkEffInit,
                                                       seedMrkEffSampling = seedMrkEffSamplingInit)
      }

      self$lociEffectsInit <- lociEffectsInit
    },





    #' @description
    #' start simulation of breeding scheme
    startSimulation = function() {
      # Read arguments from `self`
      simBsName <- self$simBsName
      bsInfoInit <- self$bsInfoInit
      breederInfoInit <- self$breederInfoInit
      nIterSimulation <- self$nIterSimulation
      nGenerationProceed <- self$nGenerationProceed
      nRefreshMemoryEvery <- self$nRefreshMemoryEvery
      updateBreederInfo <- self$updateBreederInfo
      phenotypingInds <- self$phenotypingInds
      nRepForPheno <- self$nRepForPheno
      updateModels <- self$updateModels
      nSelectionWaysVec <- self$nSelectionWaysVec
      selectionMethodList <- self$selectionMethodList
      traitNoSelList <- self$traitNoSelList
      blockSplitMethod <- self$blockSplitMethod
      nMrkInBlock <- self$nMrkInBlock
      minimumSegmentLength <- self$minimumSegmentLength
      nSelInitOPVList <- self$nSelInitOPVList
      nIterOPV <- self$nIterOPV
      nProgeniesEMBVVec <- self$nProgeniesEMBVVec
      nIterEMBV <- self$nIterEMBV
      nCoresEMBV <- self$nCoresEMBV
      clusteringForSelList <- self$clusteringForSelList
      nClusterList <- self$nClusterList
      nTopClusterList <- self$nTopClusterList
      nTopEachList <- self$nTopEachList
      nSelList <- self$nSelList
      multiTraitsEvalMethodList <- self$multiTraitsEvalMethodList
      hSelList <- self$hSelList
      matingMethodVec <- self$matingMethodVec
      allocateMethodVec <- self$allocateMethodVec
      weightedAllocationMethodList <- self$weightedAllocationMethodList
      traitNoRAList <- self$traitNoRAList
      hList <- self$hList
      minimumUnitAllocateVec <- self$minimumUnitAllocateVec
      includeGVPVec <- self$includeGVPVec
      nNextPopVec <- self$nNextPopVec
      nameMethod <- self$nameMethod
      nCores <- self$nCores
      overWriteRes <- self$overWriteRes
      showProgress <- self$showProgress
      returnMethod <- self$returnMethod
      saveAllResAt <- self$saveAllResAt
      evaluateGVMethod <- self$evaluateGVMethod
      nTopEval <- self$nTopEval
      traitNoEval <- self$traitNoEval
      hEval <- self$hEval
      verbose <- self$verbose

      lociEffectsInit <- self$lociEffectsInit

      populationNameInit <- names(bsInfoInit$populations[length(bsInfoInit$populations)])

      iterNames <- paste0("Iteration_", 1:nIterSimulation)

      if (!is.null(saveAllResAt)) {
        saveAllResAtSplit <- stringr::str_split(string = list.files(saveAllResAt),
                                                pattern = "_")
        saveAllResAtSplitLast <- lapply(X = saveAllResAtSplit,
                                        FUN = function(saveAllResAtSplitVec) {
                                          return(saveAllResAtSplitVec[length(saveAllResAtSplitVec)])
                                        })

        saveAllNumeric <- unique(sort(as.numeric(stringr::str_remove(saveAllResAtSplitLast, ".rds"))))
      }


      if (is.null(self$lociEffectsInit)) {
        self$computeLociEffInit()
      }
      lociEffectsInit <- self$lociEffectsInit

      if (is.null(self$simBsRes[[simBsName]])) {
        self$simBsRes[[simBsName]] <- list()

        if ("all" %in% returnMethod) {
          self$simBsRes[[simBsName]]$all <- list()
        }

        if ("max" %in% returnMethod) {
          self$simBsRes[[simBsName]]$max <- rep(x = NA, nIterSimulation)
          names(self$simBsRes[[simBsName]]$max) <- iterNames
        }

        if ("mean" %in% returnMethod) {
          self$simBsRes[[simBsName]]$mean <- rep(x = NA, nIterSimulation)
          names(self$simBsRes[[simBsName]]$mean) <- iterNames
        }

        if ("median" %in% returnMethod) {
          self$simBsRes[[simBsName]]$median <- rep(x = NA, nIterSimulation)
          names(self$simBsRes[[simBsName]]$median) <- iterNames
        }

        if ("min" %in% returnMethod) {
          self$simBsRes[[simBsName]]$min <- rep(x = NA, nIterSimulation)
          names(self$simBsRes[[simBsName]]$min) <- iterNames
        }

        if ("var" %in% returnMethod) {
          self$simBsRes[[simBsName]]$var <- rep(x = NA, nIterSimulation)
          names(self$simBsRes[[simBsName]]$var) <- iterNames
        }
      }


      if (nCores == 1) {
        if (showProgress) {
          pb <- utils::txtProgressBar(min = 0, max = nIterSimulation, style = 3)
        }



        simulationCounts <- 0
        for (iterNo in 1:nIterSimulation) {
          if (showProgress) {
            utils::setTxtProgressBar(pb, iterNo)
          }

          iterName <- iterNames[iterNo]

          if (!((is.null(self$simBsRes[[simBsName]]$all[[iterName]])) &
                (length(self$trueGVMatList[[iterName]]) <= 1))) {
            if (overWriteRes) {
              conductSimulation <- TRUE
            } else {
              conductSimulation <- FALSE
            }
          } else {
            if (any(c("all", "summary") %in% returnMethod)) {
              conductSimulation <- TRUE
            } else {
              if (any(sapply(returnMethod, function(x) is.na(self$simBsRes[[simBsName]][[x]][iterName])))) {
                conductSimulation <- TRUE
              } else {
                if (overWriteRes) {
                  conductSimulation <- TRUE
                } else {
                  conductSimulation <- FALSE
                }
              }
            }
          }

          if (!is.null(saveAllResAt)) {
            if (!overWriteRes) {
              conductSimulation <- !(iterNo %in% saveAllNumeric)
            }
          }

          if (conductSimulation) {
            simulationCounts <- simulationCounts + 1
            bsInfo <- bsInfoInit$clone(deep = FALSE)
            breederInfo <- breederInfoInit$clone(deep = FALSE)
            lociEffects <- lociEffectsInit


            # trueGVMatList
            if (is.null(self$trueGVMatList[[iterName]])) {
              self$trueGVMatList[[iterName]][[populationNameInit]] <- self$trueGVMatInit
            }

            # estimatedGVMatList
            if (is.null(self$estimatedGVMatList[[iterName]])) {
              self$estimatedGVMatList[[iterName]][[populationNameInit]] <- self$estimatedGVMatInit
            }

            for (genProceedNo in 1:nGenerationProceed) {
              crossInfoNow <- myBreedSimulatR::crossInfo$new(parentPopulation = bsInfo$populations[[length(bsInfo$populations)]],
                                                             nSelectionWays = nSelectionWaysVec[genProceedNo],
                                                             selectionMethod = selectionMethodList[[genProceedNo]],
                                                             traitNoSel = traitNoSelList[[genProceedNo]],
                                                             userSI = NULL,
                                                             lociEffects = lociEffects,
                                                             blockSplitMethod = blockSplitMethod,
                                                             nMrkInBlock = nMrkInBlock,
                                                             minimumSegmentLength = minimumSegmentLength,
                                                             nSelInitOPV = nSelInitOPVList[[genProceedNo]],
                                                             nIterOPV = nIterOPV,
                                                             nProgeniesEMBV = nProgeniesEMBVVec[genProceedNo],
                                                             nIterEMBV = nIterEMBV,
                                                             nCoresEMBV = nCoresEMBV,
                                                             clusteringForSel = clusteringForSelList[[genProceedNo]],
                                                             nCluster = nClusterList[[genProceedNo]],
                                                             nTopCluster = nTopClusterList[[genProceedNo]],
                                                             nTopEach = nTopEachList[[genProceedNo]],
                                                             nSel = nSelList[[genProceedNo]],
                                                             multiTraitsEvalMethod = multiTraitsEvalMethodList[[genProceedNo]],
                                                             hSel = hSelList[[genProceedNo]],
                                                             matingMethod = matingMethodVec[genProceedNo],
                                                             allocateMethod = allocateMethodVec[genProceedNo],
                                                             weightedAllocationMethod = weightedAllocationMethodList[[genProceedNo]],
                                                             nProgenies = NULL,
                                                             traitNoRA = traitNoRAList[[genProceedNo]],
                                                             h = hList[[genProceedNo]],
                                                             minimumUnitAllocate = minimumUnitAllocateVec[genProceedNo],
                                                             includeGVP = includeGVPVec[genProceedNo],
                                                             nNextPop = nNextPopVec[genProceedNo],
                                                             nPairs = NULL,
                                                             nameMethod = nameMethod,
                                                             indNames = NULL,
                                                             seedSimRM = NA,
                                                             seedSimMC = NA,
                                                             selCands = NULL,
                                                             crosses = NULL,
                                                             verbose = verbose)
              bsInfo$nextGeneration(crossInfo = crossInfoNow)


              if (updateBreederInfo[genProceedNo]) {
                breederInfo$getNewPopulation(bsInfo = bsInfo,
                                             generationNew = NULL,
                                             genotyping = TRUE,
                                             genotypedIndNames = NULL)
                if (phenotypingInds[genProceedNo]) {
                  breederInfo$phenotyper(bsInfo = bsInfo,
                                         generationOfInterest = NULL,
                                         estimateGV = TRUE,
                                         estimatedGVMethod = "lme4",
                                         nRep = nRepForPheno[genProceedNo])

                  if (updateModels[genProceedNo]) {
                    lociEffects <- private$lociEffects(bsInfo = bsInfo$clone(deep = FALSE),
                                                       breederInfo = breederInfo$clone(deep = FALSE))
                  }
                }
              }


              if (any(c("all", "summary") %in% returnMethod)) {
                populationNameNow <- names(bsInfo$populations)[length(bsInfo$populations)]
                trueGVMat <- bsInfo$populations[[length(bsInfo$populations)]]$trueGVMat
                self$trueGVMatList[[iterName]][[populationNameNow]] <- trueGVMat

                # if (breederInfo$generation < bsInfo$generation) {
                #   breederInfo$getNewPopulation(bsInfo = bsInfo,
                #                                generationNew = bsInfo$generation,
                #                                genotyping = TRUE,
                #                                genotypedIndNames = NULL)
                # }

                # if (is.null(breederInfo$estimatedGVByMLRInfo[[names(bsInfo$populations[length(bsInfo$populations)])]])) {
                #   breederInfo$estimateGVByMLR(trainingPop = self$trainingPopInit,
                #                               trainingIndNames = self$trainingIndNamesInit,
                #                               testingPop = length(breederInfo$populationsFB),
                #                               methodMLR = self$methodMLRInit,
                #                               multiTrait = self$multiTraitInit,
                #                               alpha = 0.5,
                #                               nIter = 12000,
                #                               burnIn = 3000,
                #                               thin = 5,
                #                               bayesian = TRUE)
                # }
                # estimatedGVMat <- breederInfo$estimatedGVByMLRInfo[[names(bsInfo$populations[length(bsInfo$populations)])]]$testingEstimatedGVByMLR


                genoMatNow <- bsInfo$populations[[length(bsInfo$populations)]]$genoMat
                genoMatWithIntNow <- cbind(Intercept = rep(1, nrow(genoMatNow)),
                                           genoMatNow)

                estimatedGVMat <- genoMatWithIntNow[, rownames(lociEffectsInit)] %*% lociEffectsInit
                self$estimatedGVMatList[[iterName]][[populationNameNow]] <- estimatedGVMat
              }
            }

            if (!is.null(saveAllResAt)) {
              fileNameBsInfoRes <- here::here(saveAllResAt,
                                              paste0(simBsName, "_bsInfo_", iterName, ".rds"))
              fileNameBreederInfoRes <- here::here(saveAllResAt,
                                                   paste0(simBsName, "_breederInfo_", iterName, ".rds"))

              saveRDS(object = bsInfo, file = fileNameBsInfoRes)
              saveRDS(object = breederInfo, file = fileNameBreederInfoRes)
            }


            if ("all" %in% returnMethod) {
              self$simBsRes[[simBsName]]$all[[iterName]] <- list(bsInfo = bsInfo,
                                                                 breederInfo = breederInfo)
            }
            if (any(returnMethod %in% c("summary", "max", "mean", "median", "min", "var"))) {
              populationNameNow <- names(bsInfo$populations)[length(bsInfo$populations)]
              if (any(c("all", "summary") %in% returnMethod)) {
                trueGVMat <- self$trueGVMatList[[iterName]][[populationNameNow]]
                estimatedGVMat <- self$estimatedGVMatList[[iterName]][[populationNameNow]]
              } else {
                trueGVMat <- bsInfo$populations[[length(bsInfo$populations)]]$trueGVMat
                self$trueGVMatList[[iterName]][[populationNameNow]] <- trueGVMat

                # if (breederInfo$generation < bsInfo$generation) {
                #   breederInfo$getNewPopulation(bsInfo = bsInfo,
                #                                generationNew = bsInfo$generation,
                #                                genotyping = TRUE,
                #                                genotypedIndNames = NULL)
                # }
                #
                # if (is.null(breederInfo$estimatedGVByMLRInfo[[names(bsInfo$populations[length(bsInfo$populations)])]])) {
                #   breederInfo$estimateGVByMLR(trainingPop = self$trainingPopInit,
                #                               trainingIndNames = self$trainingIndNamesInit,
                #                               testingPop = length(breederInfo$populationsFB),
                #                               methodMLR = self$methodMLRInit,
                #                               multiTrait = self$multiTraitInit,
                #                               alpha = 0.5,
                #                               nIter = 12000,
                #                               burnIn = 3000,
                #                               thin = 5,
                #                               bayesian = TRUE)
                # }
                # estimatedGVMat <- breederInfo$estimatedGVByMLRInfo[[names(bsInfo$populations[length(bsInfo$populations)])]]$testingEstimatedGVByMLR

                genoMatNow <- bsInfo$populations[[length(bsInfo$populations)]]$genoMat
                genoMatWithIntNow <- cbind(Intercept = rep(1, nrow(genoMatNow)),
                                           genoMatNow)

                estimatedGVMat <- genoMatWithIntNow[, rownames(lociEffectsInit)] %*% lociEffectsInit
                self$estimatedGVMatList[[iterName]][[populationNameNow]] <- estimatedGVMat
              }


              if (evaluateGVMethod == "true") {
                trueGVMatNow <- trueGVMat
              } else {
                trueGVMatNow <- estimatedGVMat
              }

              # trueGVMatScaled <- apply(X = trueGVMatNow, MARGIN = 2,
              #                          FUN = function(trueGV) {
              #                            return(scale(x = trueGV, center = TRUE,
              #                                         scale = as.logical(sd(trueGV))))
              #                          })
              trueGVMatInit <- self$trueGVMatInit
              trueGVMatScaled <- do.call(what = cbind,
                                         args = sapply(X = 1:ncol(trueGVMatNow),
                                                       FUN = function(traitNo) {
                                                         trueGVMean <- mean(trueGVMatInit[, traitNo])
                                                         trueGVSd <- sd(trueGVMatInit[, traitNo])
                                                         if (trueGVSd != 0) {
                                                           trueGVScaled <- (trueGVMatNow[, traitNo] - trueGVMean) / trueGVSd
                                                         } else {
                                                           trueGVScaled <- (trueGVMatNow[, traitNo] - trueGVMean)
                                                         }

                                                         return(trueGVScaled)
                                                       }, simplify = FALSE))

              rownames(trueGVMatScaled) <- rownames(trueGVMatNow)
              colnames(trueGVMatScaled) <- colnames(trueGVMatNow)

              trueEvals <- (trueGVMatScaled[, traitNoEval, drop = FALSE] %*% hEval)[, 1]

              if ("max" %in% returnMethod) {
                self$simBsRes[[simBsName]]$max[iterName] <- mean(x = sort(x = trueEvals, decreasing = TRUE)[1:nTopEval])
              }

              if ("mean" %in% returnMethod) {
                self$simBsRes[[simBsName]]$mean[iterName] <- mean(x = trueEvals)
              }

              if ("median" %in% returnMethod) {
                self$simBsRes[[simBsName]]$median[iterName] <- median(x = trueEvals)
              }

              if ("min" %in% returnMethod) {
                self$simBsRes[[simBsName]]$min[iterName] <- mean(x = sort(x = trueEvals, decreasing = FALSE)[1:nTopEval])
              }

              if ("var" %in% returnMethod) {
                self$simBsRes[[simBsName]]$var[iterName] <- var(x = trueEvals)
              }
            }


            if (simulationCounts %% nRefreshMemoryEvery == 0) {
              rm(trueGVMat); rm(estimatedGVMat); rm(trueGVMatNow); rm(trueGVMatScaled); rm(bsInfo); rm(breederInfo)
              gc(reset = TRUE); gc(reset = TRUE)
            }
          }

        }

        if (showProgress) {
          cat("\n")
        }
      } else {
        conductSimulations <- sapply(X = iterNames,
                                     FUN = function(iterName) {
                                       if (!((is.null(self$simBsRes[[simBsName]]$all[[iterName]])) &
                                             (length(self$trueGVMatList[[iterName]]) <= 1))) {
                                         if (overWriteRes) {
                                           conductSimulation <- TRUE
                                         } else {
                                           conductSimulation <- FALSE
                                         }
                                       } else {
                                         if (any(c("all", "summary") %in% returnMethod)) {
                                           conductSimulation <- TRUE
                                         } else {
                                           if (any(sapply(returnMethod, function(x) is.na(self$simBsRes[[simBsName]][[x]][iterName])))) {
                                             conductSimulation <- TRUE
                                           } else {
                                             if (overWriteRes) {
                                               conductSimulation <- TRUE
                                             } else {
                                               conductSimulation <- FALSE
                                             }
                                           }
                                         }
                                       }

                                       return(conductSimulation)
                                     })

        if (!is.null(saveAllResAt)) {
          if (!overWriteRes) {
            conductSimulations[saveAllNumeric] <- FALSE
          }
        }

        if (any(conductSimulations)) {
          if (showProgress) {
            simResAll <- pbmcapply::pbmclapply(X = (1:nIterSimulation)[conductSimulations],
                                               FUN = private$performOneSimulationTryError,
                                               mc.cores = nCores)
          } else {
            simResAll <- parallel::mclapply(X = (1:nIterSimulation)[conductSimulations],
                                            FUN = private$performOneSimulationTryError,
                                            mc.cores = nCores)
          }
          names(simResAll) <- iterNames[conductSimulations]

          if ("all" %in% returnMethod) {
            self$simBsRes[[simBsName]]$all[iterNames[conductSimulations]] <- lapply(simResAll, function(x) x$all)
          }

          if ("max" %in% returnMethod) {
            self$simBsRes[[simBsName]]$max[iterNames[conductSimulations]] <- unlist(lapply(simResAll, function(x) x$max))
          }

          if ("mean" %in% returnMethod) {
            self$simBsRes[[simBsName]]$mean[iterNames[conductSimulations]] <- unlist(lapply(simResAll, function(x) x$mean))
          }

          if ("median" %in% returnMethod) {
            self$simBsRes[[simBsName]]$median[iterNames[conductSimulations]] <- unlist(lapply(simResAll, function(x) x$median))
          }

          if ("min" %in% returnMethod) {
            self$simBsRes[[simBsName]]$min[iterNames[conductSimulations]] <- unlist(lapply(simResAll, function(x) x$min))
          }

          if ("var" %in% returnMethod) {
            self$simBsRes[[simBsName]]$var[iterNames[conductSimulations]] <- unlist(lapply(simResAll, function(x) x$var))
          }

          trueGVMatListNow <- lapply(simResAll, function(x) x$trueGVMatList)
          self$trueGVMatList[iterNames[conductSimulations]] <- sapply(iterNames[conductSimulations],
                                                                      function(iterName) {
                                                                        c(self$trueGVMatList[[iterName]],
                                                                          trueGVMatListNow[[iterName]])
                                                                      }, simplify = FALSE)

          estimatedGVMatListNow <- lapply(simResAll, function(x) x$estimatedGVMatList)
          self$estimatedGVMatList[iterNames[conductSimulations]] <- sapply(iterNames[conductSimulations],
                                                                           function(iterName) {
                                                                             c(self$estimatedGVMatList[[iterName]],
                                                                               estimatedGVMatListNow[[iterName]])
                                                                           }, simplify = FALSE)
        }
      }
    },



    #' @description
    #' start simulation of breeding scheme
    summaryResults = function() {
      # Read arguments from `self`
      simBsName <- self$simBsName
      bsInfoInit <- self$bsInfoInit
      showProgress <- self$showProgress
      evaluateGVMethod <- self$evaluateGVMethod
      nTopEval <- self$nTopEval
      simBsRes <- self$simBsRes
      summaryAllResAt <- self$summaryAllResAt
      nIterSimulation <- self$nIterSimulation
      iterNames <- paste0("Iteration_", 1:nIterSimulation)


      if (!is.null(summaryAllResAt)) {
        fileNameBsInfoRes0 <- here::here(summaryAllResAt,
                                         paste0(simBsName, "_bsInfo_"))
        fileNameBreederInfoRes0 <- here::here(summaryAllResAt,
                                              paste0(simBsName, "_breederInfo_"))
        if (showProgress) {
          listOfGVMatList <- pbmcapply::pbmclapply(X = iterNames,
                                                   FUN = private$extractGVMatList,
                                                   mc.cores = self$nCores)
        } else {
          listOfGVMatList <- parallel::mclapply(X = iterNames,
                                                FUN = private$extractGVMatList,
                                                mc.cores = self$nCores)
        }

        if (!is.null(listOfGVMatList$warning)) {
          listOfGVMatList <- listOfGVMatList$value
        }

        trueGVMatList <- lapply(listOfGVMatList, function(x) x$trueGVMatEachList)
        names(trueGVMatList) <- iterNames
        trueGVMatListNonNULL <- which(!unlist(lapply(trueGVMatList, is.null)))
        trueGVMatList <- trueGVMatList[trueGVMatListNonNULL]
        self$trueGVMatList <- trueGVMatList

        estimatedGVMatList <- lapply(listOfGVMatList, function(x) x$estimatedGVMatEachList)
        names(estimatedGVMatList) <- iterNames
        estimatedGVMatListNonNULL <- which(!unlist(lapply(estimatedGVMatList, is.null)))
        estimatedGVMatList <- estimatedGVMatList[estimatedGVMatListNonNULL]
        self$estimatedGVMatList <- estimatedGVMatList
      } else {
        trueGVMatList <- self$trueGVMatList
        estimatedGVMatList <- self$estimatedGVMatList
        if ((is.null(self$simBsRes[[simBsName]]$all)) &
            (length(self$trueGVMatList[[1]]) == 1)) {
          stop("Please start simulation with `returnMethod = 'summary'`. You do not have simulation results.")
        }
      }


      trueGVSummaryArrayList <- lapply(X = trueGVMatList,
                                       FUN = private$extractTrueSummaryRes)


      trueGVSummaryArray <- do.call(what = abind::abind,
                                    args = trueGVSummaryArrayList)
      dimnames(trueGVSummaryArray)[c(1, 3, 4)] <- list(Index = c("max", "mean", "median", "min", "var"),
                                                       Population = names(trueGVMatList[[1]]),
                                                       Iteration = names(trueGVMatList))


      self$trueGVSummaryArray <- trueGVSummaryArray



      estimatedGVSummaryArrayList <- lapply(X = estimatedGVMatList,
                                            FUN = private$extractEstimatedSummaryRes)


      estimatedGVSummaryArray <- do.call(what = abind::abind,
                                         args = estimatedGVSummaryArrayList)
      dimnames(estimatedGVSummaryArray)[c(1, 3, 4)] <- list(Index = c("max", "mean", "median", "min", "var"),
                                                            Population = names(estimatedGVMatList[[1]]),
                                                            Iteration = names(estimatedGVMatList))


      self$estimatedGVSummaryArray <- estimatedGVSummaryArray
    },


    #' @description
    #' Display information about the object
    print = function() {
      cat(paste0(
        "Name of Simulation Settings: ", self$simBsName, "\n",
        "Number of Simulations: ", self$nIterSimulation, "\n",
        "Number of Generations to be Proceeded: ", self$nGenerationProceed, "\n",
        "Number of Individuals for Next Generation: \n"
      ))
      print(self$nNextPopVec)
    },


    #' @description Draw figures for visualization of simulation results for summary statistics
    #' @param targetTrait [numeric] Target trait. character is OK, but numeric vector
    #'  corresponding to target traits is preferred. It should be a vector with length 1.
    #' @param targetPopulation [numeric] Target populations. character is OK, but numeric vector
    #'  corresponding to target traits is preferred.
    #' @param plotType [character] We offer "box", "violin", "lines", "density"
    #' to draw figures for simulation results.
    #' @param plotTargetDensity [character] When you choose "density" for `plotType`,
    #'  you should select which summary statistics will be plotted. It should be a vector with length 1.
    #' @param plotGVMethod [character] Which type of GV (true GV or estimated GV) will be used for plotting the simulation results
    #' @param adjust [numeric] the bandwidth used is actually adjust*bw. This makes it easy to specify values like ‘half the default’ bandwidth.
    #' (see: `adjust` in \link[stats]{density})
    #'
    plot = function(targetTrait = 1,
                     targetPopulation = NULL,
                     plotType = "box",
                     plotTargetDensity = "max",
                     plotGVMethod = "true",
                     adjust = 1e-05) {
      lociEffMethodsOffered <- c("true", "estimated")
      # targetTrait
      if (is.numeric(targetTrait)) {
        targetTraitName <- self$bsInfoInit$traitInfo$traitNames[targetTrait]
      } else if (is.character(targetTrait)) {
        targetTraitName <- targetTrait
      } else {
        stop("`targetTraitName` must be `numeric` or `character`!")
      }
      stopifnot(length(targetTraitName) == 1)


      # plotType
      plotTypeOffered <- c("box", "violin", "lines", "density")

      stopifnot(length(plotType) == 1)
      stopifnot(plotType %in% plotTypeOffered)


      # plotGVMethod
      if (!is.null(plotGVMethod)) {
        if (!(plotGVMethod %in% lociEffMethodsOffered)) {
          stop(paste0("We only offer the following methods for evaluating the simulation results: ",
                      paste(lociEffMethodsOffered, collapse = "; ")))
        }
      } else {
        plotGVMethod <- "true"
      }


      if (plotGVMethod == "true") {
        if (is.null(self$trueGVSummaryArray)) {
          self$summaryResults()
        }

        trueGVSummaryArray <- self$trueGVSummaryArray
      } else {
        if (is.null(self$estimatedGVSummaryArray)) {
          self$summaryResults()
        }

        trueGVSummaryArray <- self$estimatedGVSummaryArray
      }

      # targetPopulation
      if (is.null(targetPopulation)) {
        targetPopulation <- 1:dim(trueGVSummaryArray)[3]
      }
      targetPopulation <- targetPopulation[targetPopulation %in% (1:dim(trueGVSummaryArray)[3])]


      trueGVSummaryArray <- trueGVSummaryArray[ , , targetPopulation, , drop = FALSE]

      dimSummary <- dim(trueGVSummaryArray)
      dimnamesSummary <- dimnames(trueGVSummaryArray)

      trueGVSummaryDf <- data.frame(SummaryStatistics = rep(dimnamesSummary[[1]], prod(dimSummary[-1])),
                                    Trait = rep(rep(dimnamesSummary[[2]], each = dimSummary[1]),
                                                prod(dimSummary[3:4])),
                                    Population = rep(rep(dimnamesSummary[[3]], each = prod(dimSummary[1:2])),
                                                     prod(dimSummary[4])),
                                    Iteration = rep(dimnamesSummary[[4]], each = prod(dimSummary[1:3])),
                                    Value = c(trueGVSummaryArray))
      trueGVSummaryDf$SummaryStatistics <- factor(trueGVSummaryDf$SummaryStatistics, levels = dimnamesSummary[[1]])
      trueGVSummaryDf$Trait <- factor(trueGVSummaryDf$Trait, levels = dimnamesSummary[[2]])
      trueGVSummaryDf$Population <- factor(trueGVSummaryDf$Population, levels = dimnamesSummary[[3]])
      trueGVSummaryDf$Iteration <- factor(trueGVSummaryDf$Iteration, levels = dimnamesSummary[[4]])

      if (plotType %in% c("box", "violin")) {
        # trueGVSummaryDfTarget <- trueGVSummaryDf[trueGVSummaryDf$SummaryStatistics %in% plotTarget, ]
        trueGVSummaryDfTarget <- trueGVSummaryDf[trueGVSummaryDf$Trait %in% targetTraitName, ]
        trueGVSummaryDfTarget$Value <- round(trueGVSummaryDfTarget$Value, 3)
        plt <- plotly::plot_ly(
          data = trueGVSummaryDfTarget,
          x = ~ Population,
          y = ~ Value,
          split = ~ SummaryStatistics,
          type = plotType,
          hoverinfo = "text",
          # boxpoints = "all",
          # jitter = 0.3,
          # pointpos = -1.8,
          text = paste0(apply(trueGVSummaryDfTarget, 1, function(l) {
            paste(names(l), ":", l, collapse = "\n")
          }))
        ) %>%
          plotly::layout(title = list(text = targetTraitName),
                         # yaxis = list(title = list(text = plotTarget))
                         yaxis = list(title = list(text = paste0(plotGVMethod, " GV")))
          )
        if (plotType == "box") {
          plt <- plt %>% plotly::layout(boxmode = "group")
        } else if (plotType == "violin") {
          plt <- plt %>% plotly::layout(violinmode = "group")
        }
      } else if (plotType %in% c("lines")) {
        trueGVSummaryMeanArray <- apply(X = trueGVSummaryArray,
                                        MARGIN = 1:3, FUN = mean)

        dimSummaryMean <- dim(trueGVSummaryMeanArray)
        dimnamesSummaryMean <- dimnames(trueGVSummaryMeanArray)

        trueGVSummaryMeanDf <- data.frame(SummaryStatistics = rep(dimnamesSummaryMean[[1]], prod(dimSummaryMean[-1])),
                                          Trait = rep(rep(dimnamesSummaryMean[[2]], each = dimSummaryMean[1]),
                                                      prod(dimSummaryMean[3])),
                                          Population = rep(dimnamesSummaryMean[[3]], each = prod(dimSummaryMean[1:2])),
                                          Value = c(trueGVSummaryMeanArray))
        trueGVSummaryMeanDf$SummaryStatistics <- factor(trueGVSummaryMeanDf$SummaryStatistics, levels = dimnamesSummaryMean[[1]])
        trueGVSummaryMeanDf$Trait <- factor(trueGVSummaryMeanDf$Trait, levels = dimnamesSummaryMean[[2]])
        trueGVSummaryMeanDf$Population <- factor(trueGVSummaryMeanDf$Population, levels = dimnamesSummaryMean[[3]])

        trueGVSummaryMeanDfTarget <- trueGVSummaryMeanDf[trueGVSummaryMeanDf$Trait %in% targetTraitName, ]
        trueGVSummaryMeanDfTarget$Value <- round(trueGVSummaryMeanDfTarget, 3)
        plt <- plot_ly(
          data = trueGVSummaryMeanDfTarget,
          x = ~ Population,
          y = ~ Value,
          split = ~ SummaryStatistics,
          # line = list(color = colorVec[1],
          #             width = widthVec[1],
          #             dash = dashVec[1]),
          type = "scatter",
          mode = "markers+lines",
          hoverinfo = "text",
          text = paste0(apply(trueGVSummaryMeanDfTarget, 1, function(l) {
            paste(names(l), ":", l, collapse = "\n")
          }))
        ) %>%
          plotly::layout(title = list(text = targetTraitName),
                         yaxis = list(title = list(text = paste0(plotGVMethod, " GV"))))
      } else if (plotType == "density") {
        trueGVSummaryDfTarget <- trueGVSummaryDf[trueGVSummaryDf$Trait %in% targetTraitName, ]
        trueGVSummaryDfTargetSS <- trueGVSummaryDfTarget[trueGVSummaryDfTarget$SummaryStatistics %in% plotTargetDensity, ]

        densityValueDfList <- lapply(X = dimnames(trueGVSummaryArray)[[3]],
                                     FUN = function(popName) {
                                       trueGVSummaryDfTargetSSEachPop <- trueGVSummaryDfTargetSS[trueGVSummaryDfTargetSS$Population %in% popName, ]
                                       densityResEachPop <- density(x = sort(trueGVSummaryDfTargetSSEachPop$Value), adjust = adjust)

                                       x <- densityResEachPop$x
                                       x <- c(min(x), x)
                                       y <- cumsum(densityResEachPop$y / sum(densityResEachPop$y))
                                       y <- c(0, y)

                                       return(data.frame(x, y))
                                     })
        densityValueDf <- do.call(what = rbind,
                                  args = densityValueDfList)
        densityValueDf$Population <- rep(dimnames(trueGVSummaryArray)[[3]], unlist(lapply(densityValueDfList, nrow)))
        densityValueDf$Population <- factor(densityValueDf$Population, levels = dimnames(trueGVSummaryArray)[[3]])

        plt <- plot_ly(
          data = densityValueDf,
          x = ~ x,
          y = ~ y,
          split = ~ Population,
          # line = list(color = colorVec[1],
          #             width = widthVec[1],
          #             dash = dashVec[1]),
          type = "scatter",
          mode = "lines"
          # name = nameVec[1]
        ) %>%
          plotly::layout(title = list(text = paste0(targetTraitName, "-", plotTargetDensity)),
                         xaxis = list(title = list(text = paste0(plotGVMethod, " GV"))))
      }


      return(plt)
    }
  ),

  private = list(
    # @description marker and QTL effects used for crossInfo object
    #
    # @param ind1 [individual class] parent 1
    # @param ind2 [individual class] parent 2
    # @param names [character] names of the descendants
    # @param n [numeric] number of descendants
    lociEffects = function(bsInfo,
                            breederInfo,
                            alpha = 0.5,
                            nIter = 5000,
                            burnIn = 1000,
                            thin = 5,
                            bayesian = FALSE) {
      # Read arguments from `self`
      lociEffMethod <- self$lociEffMethod
      trainingPopType <- self$trainingPopType
      methodMLR <- self$methodMLR
      multiTrait <- self$multiTrait
      verbose <- self$verbose



      if (lociEffMethod == "true") {
        lociEffects <- bsInfo$lociEffects
      } else if (lociEffMethod == "estimated") {
        trainingPopName <- names(breederInfo$populationsFB)
        infoName <- paste0(trainingPopName[length(trainingPopName)], "_", methodMLR)

        if (trainingPopType == "all") {
          trainingPop <- 1:length(breederInfo$populationsFB)
        } else {
          trainingPop <- length(breederInfo$populationsFB)
        }

        if (is.null(breederInfo$estimatedMrkEffInfo[[infoName]])) {
          breederInfo$estimateMrkEff(trainingPop = trainingPop,
                                     methodMLR = methodMLR,
                                     multiTrait = multiTrait,
                                     alpha = alpha,
                                     nIter = nIter,
                                     burnIn = burnIn,
                                     thin = thin,
                                     bayesian = bayesian)
        }
        lociEffects <- breederInfo$lociEffects(bsInfo = bsInfo,
                                               trainingPop = trainingPop,
                                               methodMLR = methodMLR)
      }


      return(lociEffects)
    },




    # @description Proceed breeding scheme (one simulation)
    #
    # @param iterNo [numeric] Iteration No.
    performOneSimulation = function(iterNo) {
      simBsName <- self$simBsName
      bsInfoInit <- self$bsInfoInit
      breederInfoInit <- self$breederInfoInit
      nIterSimulation <- self$nIterSimulation
      nGenerationProceed <- self$nGenerationProceed
      nRefreshMemoryEvery <- self$nRefreshMemoryEvery
      updateBreederInfo <- self$updateBreederInfo
      phenotypingInds <- self$phenotypingInds
      nRepForPheno <- self$nRepForPheno
      updateModels <- self$updateModels
      nSelectionWaysVec <- self$nSelectionWaysVec
      selectionMethodList <- self$selectionMethodList
      traitNoSelList <- self$traitNoSelList
      blockSplitMethod <- self$blockSplitMethod
      nMrkInBlock <- self$nMrkInBlock
      minimumSegmentLength <- self$minimumSegmentLength
      nSelInitOPVList <- self$nSelInitOPVList
      nIterOPV <- self$nIterOPV
      nProgeniesEMBVVec <- self$nProgeniesEMBVVec
      nIterEMBV <- self$nIterEMBV
      nCoresEMBV <- self$nCoresEMBV
      clusteringForSelList <- self$clusteringForSelList
      nClusterList <- self$nClusterList
      nTopClusterList <- self$nTopClusterList
      nTopEachList <- self$nTopEachList
      nSelList <- self$nSelList
      multiTraitsEvalMethodList <- self$multiTraitsEvalMethodList
      hSelList <- self$hSelList
      matingMethodVec <- self$matingMethodVec
      allocateMethodVec <- self$allocateMethodVec
      weightedAllocationMethodList <- self$weightedAllocationMethodList
      traitNoRAList <- self$traitNoRAList
      hList <- self$hList
      minimumUnitAllocateVec <- self$minimumUnitAllocateVec
      includeGVPVec <- self$includeGVPVec
      nNextPopVec <- self$nNextPopVec
      nameMethod <- self$nameMethod
      nCores <- self$nCores
      overWriteRes <- self$overWriteRes
      showProgress <- self$showProgress
      returnMethod <- self$returnMethod
      saveAllResAt <- self$saveAllResAt
      evaluateGVMethod <- self$evaluateGVMethod
      nTopEval <- self$nTopEval
      traitNoEval <- self$traitNoEval
      hEval <- self$hEval
      verbose <- self$verbose

      lociEffectsInit <- self$lociEffectsInit

      populationNameInit <- names(bsInfoInit$populations[length(bsInfoInit$populations)])

      iterNames <- paste0("Iteration_", 1:nIterSimulation)
      lociEffectsInit <- self$lociEffectsInit


      iterName <- iterNames[iterNo]
      simRes <- list()
      simRes$trueGVMatList <- list()
      simRes$estimatedGVMatList <- list()

      bsInfo <- bsInfoInit$clone(deep = FALSE)
      breederInfo <- breederInfoInit$clone(deep = FALSE)
      lociEffects <- lociEffectsInit

      # trueGVMatList
      if (is.null(self$trueGVMatList[[iterName]])) {
        simRes$trueGVMatList[[populationNameInit]] <- self$trueGVMatInit
      }

      # estimatedGVMatList
      if (is.null(self$estimatedGVMatList[[iterName]])) {
        simRes$estimatedGVMatList[[populationNameInit]] <- self$estimatedGVMatInit
      }


      for (genProceedNo in 1:nGenerationProceed) {
        crossInfoNow <- myBreedSimulatR::crossInfo$new(parentPopulation = bsInfo$populations[[length(bsInfo$populations)]],
                                                       nSelectionWays = nSelectionWaysVec[genProceedNo],
                                                       selectionMethod = selectionMethodList[[genProceedNo]],
                                                       traitNoSel = traitNoSelList[[genProceedNo]],
                                                       userSI = NULL,
                                                       lociEffects = lociEffects,
                                                       blockSplitMethod = blockSplitMethod,
                                                       nMrkInBlock = nMrkInBlock,
                                                       minimumSegmentLength = minimumSegmentLength,
                                                       nSelInitOPV = nSelInitOPVList[[genProceedNo]],
                                                       nIterOPV = nIterOPV,
                                                       nProgeniesEMBV = nProgeniesEMBVVec[genProceedNo],
                                                       nIterEMBV = nIterEMBV,
                                                       nCoresEMBV = nCoresEMBV,
                                                       clusteringForSel = clusteringForSelList[[genProceedNo]],
                                                       nCluster = nClusterList[[genProceedNo]],
                                                       nTopCluster = nTopClusterList[[genProceedNo]],
                                                       nTopEach = nTopEachList[[genProceedNo]],
                                                       nSel = nSelList[[genProceedNo]],
                                                       multiTraitsEvalMethod = multiTraitsEvalMethodList[[genProceedNo]],
                                                       hSel = hSelList[[genProceedNo]],
                                                       matingMethod = matingMethodVec[genProceedNo],
                                                       allocateMethod = allocateMethodVec[genProceedNo],
                                                       weightedAllocationMethod = weightedAllocationMethodList[[genProceedNo]],
                                                       nProgenies = NULL,
                                                       traitNoRA = traitNoRAList[[genProceedNo]],
                                                       h = hList[[genProceedNo]],
                                                       minimumUnitAllocate = minimumUnitAllocateVec[genProceedNo],
                                                       includeGVP = includeGVPVec[genProceedNo],
                                                       nNextPop = nNextPopVec[genProceedNo],
                                                       nPairs = NULL,
                                                       nameMethod = nameMethod,
                                                       indNames = NULL,
                                                       seedSimRM = NA,
                                                       seedSimMC = NA,
                                                       selCands = NULL,
                                                       crosses = NULL,
                                                       verbose = verbose)
        bsInfo$nextGeneration(crossInfo = crossInfoNow)


        if (updateBreederInfo[genProceedNo]) {
          breederInfo$getNewPopulation(bsInfo = bsInfo,
                                       generationNew = NULL,
                                       genotyping = TRUE,
                                       genotypedIndNames = NULL)
          if (phenotypingInds[genProceedNo]) {
            breederInfo$phenotyper(bsInfo = bsInfo,
                                   generationOfInterest = NULL,
                                   estimateGV = TRUE,
                                   estimatedGVMethod = "lme4",
                                   nRep = nRepForPheno[genProceedNo])

            if (updateModels[genProceedNo]) {
              lociEffects <- private$lociEffects(bsInfo = bsInfo$clone(deep = FALSE),
                                                 breederInfo = breederInfo$clone(deep = FALSE))
            }
          }
        }


        if (any(c("all", "summary") %in% returnMethod)) {
          populationNameNow <- names(bsInfo$populations)[length(bsInfo$populations)]
          trueGVMat <- bsInfo$populations[[length(bsInfo$populations)]]$trueGVMat
          simRes$trueGVMatList[[populationNameNow]] <- trueGVMat

          # if (breederInfo$generation < bsInfo$generation) {
          #   breederInfo$getNewPopulation(bsInfo = bsInfo,
          #                                generationNew = bsInfo$generation,
          #                                genotyping = TRUE,
          #                                genotypedIndNames = NULL)
          # }
          #
          # if (is.null(breederInfo$estimatedGVByMLRInfo[[names(bsInfo$populations[length(bsInfo$populations)])]])) {
          #   breederInfo$estimateGVByMLR(trainingPop = self$trainingPopInit,
          #                               trainingIndNames = self$trainingIndNamesInit,
          #                               testingPop = length(breederInfo$populationsFB),
          #                               methodMLR = self$methodMLRInit,
          #                               multiTrait = self$multiTraitInit,
          #                               alpha = 0.5,
          #                               nIter = 12000,
          #                               burnIn = 3000,
          #                               thin = 5,
          #                               bayesian = TRUE)
          # }
          # estimatedGVMat <- breederInfo$estimatedGVByMLRInfo[[names(bsInfo$populations[length(bsInfo$populations)])]]$testingEstimatedGVByMLR

          genoMatNow <- bsInfo$populations[[length(bsInfo$populations)]]$genoMat
          genoMatWithIntNow <- cbind(Intercept = rep(1, nrow(genoMatNow)),
                                     genoMatNow)

          estimatedGVMat <- genoMatWithIntNow[, rownames(lociEffectsInit)] %*% lociEffectsInit
          simRes$estimatedGVMatList[[populationNameNow]] <- estimatedGVMat
        }
      }

      if (!is.null(saveAllResAt)) {
        fileNameBsInfoRes <- here::here(saveAllResAt,
                                        paste0(simBsName, "_bsInfo_", iterName, ".rds"))
        fileNameBreederInfoRes <- here::here(saveAllResAt,
                                             paste0(simBsName, "_breederInfo_", iterName, ".rds"))

        saveRDS(object = bsInfo, file = fileNameBsInfoRes)
        saveRDS(object = breederInfo, file = fileNameBreederInfoRes)
      }


      if ("all" %in% returnMethod) {
        simRes$all <- list(bsInfo = bsInfo,
                           breederInfo = breederInfo)
      }
      if (any(returnMethod %in% c("summary", "max", "mean", "median", "min", "var"))) {
        populationNameNow <- names(bsInfo$populations)[length(bsInfo$populations)]
        if (any(c("all", "summary") %in% returnMethod)) {
          trueGVMat <- simRes$trueGVMatList[[populationNameNow]]
          estimatedGVMat <- simRes$estimatedGVMatList[[populationNameNow]]
        } else {
          trueGVMat <- bsInfo$populations[[length(bsInfo$populations)]]$trueGVMat
          simRes$trueGVMatList[[populationNameNow]] <- trueGVMat

          # if (breederInfo$generation < bsInfo$generation) {
          #   breederInfo$getNewPopulation(bsInfo = bsInfo,
          #                                generationNew = bsInfo$generation,
          #                                genotyping = TRUE,
          #                                genotypedIndNames = NULL)
          # }
          #
          # if (is.null(breederInfo$estimatedGVByMLRInfo[[names(bsInfo$populations[length(bsInfo$populations)])]])) {
          #   breederInfo$estimateGVByMLR(trainingPop = self$trainingPopInit,
          #                               trainingIndNames = self$trainingIndNamesInit,
          #                               testingPop = length(breederInfo$populationsFB),
          #                               methodMLR = self$methodMLRInit,
          #                               multiTrait = self$multiTraitInit,
          #                               alpha = 0.5,
          #                               nIter = 12000,
          #                               burnIn = 3000,
          #                               thin = 5,
          #                               bayesian = TRUE)
          # }
          # estimatedGVMat <- breederInfo$estimatedGVByMLRInfo[[names(bsInfo$populations[length(bsInfo$populations)])]]$testingEstimatedGVByMLR

          genoMatNow <- bsInfo$populations[[length(bsInfo$populations)]]$genoMat
          genoMatWithIntNow <- cbind(Intercept = rep(1, nrow(genoMatNow)),
                                     genoMatNow)

          estimatedGVMat <- genoMatWithIntNow[, rownames(lociEffectsInit)] %*% lociEffectsInit
          simRes$estimatedGVMatList[[populationNameNow]] <- estimatedGVMat
        }


        if (evaluateGVMethod == "true") {
          trueGVMatNow <- trueGVMat
        } else {
          trueGVMatNow <- estimatedGVMat
        }

        # trueGVMatScaled <- apply(X = trueGVMatNow, MARGIN = 2,
        #                          FUN = function(trueGV) {
        #                            return(scale(x = trueGV, center = TRUE,
        #                                         scale = as.logical(sd(trueGV))))
        #                          })

        trueGVMatInit <- self$trueGVMatInit
        trueGVMatScaled <- do.call(what = cbind,
                                   args = sapply(X = 1:ncol(trueGVMatNow),
                                                 FUN = function(traitNo) {
                                                   trueGVMean <- mean(trueGVMatInit[, traitNo])
                                                   trueGVSd <- sd(trueGVMatInit[, traitNo])
                                                   if (trueGVSd != 0) {
                                                     trueGVScaled <- (trueGVMatNow[, traitNo] - trueGVMean) / trueGVSd
                                                   } else {
                                                     trueGVScaled <- (trueGVMatNow[, traitNo] - trueGVMean)
                                                   }

                                                   return(trueGVScaled)
                                                 }, simplify = FALSE))
        rownames(trueGVMatScaled) <- rownames(trueGVMatNow)
        colnames(trueGVMatScaled) <- colnames(trueGVMatNow)

        trueEvals <- (trueGVMatScaled[, traitNoEval, drop = FALSE] %*% hEval)[, 1]

        if ("max" %in% returnMethod) {
          simRes$max <- mean(x = sort(x = trueEvals, decreasing = TRUE)[1:nTopEval])
        }

        if ("mean" %in% returnMethod) {
          simRes$mean <- mean(x = trueEvals)
        }

        if ("median" %in% returnMethod) {
          simRes$median <- median(x = trueEvals)
        }

        if ("min" %in% returnMethod) {
          simRes$min <- mean(x = sort(x = trueEvals, decreasing = FALSE)[1:nTopEval])
        }

        if ("var" %in% returnMethod) {
          simRes$var <- var(x = trueEvals)
        }
      }

      if (iterNo %% nRefreshMemoryEvery == 0) {
        rm(trueGVMat); rm(estimatedGVMat); rm(trueGVMatNow); rm(trueGVMatScaled); rm(bsInfo); rm(breederInfo)
        gc(reset = TRUE); gc(reset = TRUE)
      }

      return(simRes)
    },


    # @description Proceed breeding scheme with try-error (one simulation)
    #
    # @param iterNo [numeric] Iteration No.
    performOneSimulationTryError = function(iterNo) {
      simRes <- try(private$performOneSimulation(iterNo = iterNo),
                    silent = TRUE)

      nIterSimulation <- self$nIterSimulation
      iterNames <- paste0("Iteration_", 1:nIterSimulation)

      iterName <- iterNames[iterNo]

      bsInfoInit <- self$bsInfoInit
      populationNameInit <- names(bsInfoInit$populations[length(bsInfoInit$populations)])

      if ("try-error" %in% class(simRes)) {
        simRes <- list()
        simRes$trueGVMatList <- list()
        simRes$estimatedGVMatList <- list()

        # trueGVMatList
        if (is.null(self$trueGVMatList[[iterName]])) {
          simRes$trueGVMatList[[populationNameInit]] <- self$trueGVMatInit
        }

        # estimatedGVMatList
        if (is.null(self$estimatedGVMatList[[iterName]])) {
          simRes$estimatedGVMatList[[populationNameInit]] <- self$estimatedGVMatInit
        }
      }


      return(simRes)
    },


    # @description Extract GV matrix as a list from bsInfo & breederInfo objects
    #
    # @param iterName [character] Iteration Name
    extractGVMatList = function(iterName) {
      # Read arguments from `self`
      simBsName <- self$simBsName
      bsInfoInit <- self$bsInfoInit
      showProgress <- self$showProgress
      evaluateGVMethod <- self$evaluateGVMethod
      nTopEval <- self$nTopEval
      simBsRes <- self$simBsRes
      summaryAllResAt <- self$summaryAllResAt
      nIterSimulation <- self$nIterSimulation
      lociEffectsInit <- self$lociEffectsInit

      iterNames <- paste0("Iteration_", 1:nIterSimulation)

      fileNameBsInfoRes0 <- here::here(summaryAllResAt,
                                       paste0(simBsName, "_bsInfo_"))
      fileNameBreederInfoRes0 <- here::here(summaryAllResAt,
                                            paste0(simBsName, "_breederInfo_"))


      fileNameBsInfoRes <- paste0(fileNameBsInfoRes0, iterName, ".rds")
      bsInfoEach <- try(readRDS(file = fileNameBsInfoRes), silent = TRUE)

      if (!("try-error" %in% class(bsInfoEach))) {
        trueGVMatEachList <- lapply(X = bsInfoEach$populations,
                                    FUN = function(eachPop) {
                                      trueGVMatEachPop <- eachPop$trueGVMat

                                      return(trueGVMatEachPop)
                                    })
      } else {
        trueGVMatEachList <- NULL
      }


      # fileNameBreederInfoRes <- paste0(fileNameBreederInfoRes0,  iterName, ".rds")
      # breederInfoEach <- try(readRDS(file = fileNameBreederInfoRes), silent = TRUE)

      # if (!("try-error" %in% class(breederInfoEach))) {
      # if (breederInfoEach$generation < bsInfoEach$generation) {
      #   for (generationAdd in (breederInfoEach$generation + 1):bsInfoEach$generation) {
      #     breederInfoEach$getNewPopulation(bsInfo = bsInfoEach,
      #                                      generationNew = generationAdd,
      #                                      genotyping = estimated,
      #                                      genotypedIndNames = NULL)
      #   }
      # }
      #
      # for (generationNow in 1:length(breederInfoEach$populationsFB)) {
      #   eachPop <- breederInfoEach$populationsFB[[generationNow]]
      #   if (is.null(breederInfoEach$estimatedGVByMLRInfo[[eachPop$name]])) {
      #     breederInfoEach$estimateGVByMLR(trainingPop = self$trainingPopInit,
      #                                     trainingIndNames = self$trainingIndNamesInit,
      #                                     testingPop = generationNow,
      #                                     methodMLR = self$methodMLRInit,
      #                                     multiTrait = self$multiTraitInit,
      #                                     alpha = 0.5,
      #                                     nIter = 12000,
      #                                     burnIn = 3000,
      #                                     thin = 5,
      #                                     bayesian = TRUE)
      #   }
      # }
      # estimatedGVMatEachList <- lapply(X = breederInfoEach$populationsFB,
      #                                  FUN = function(eachPop) {
      #                                    estimatedGVMatEachPop <- breederInfoEach$estimatedGVByMLRInfo[[eachPop$name]]$testingEstimatedGVByMLR
      #
      #                                    return(estimatedGVMatEachPop)
      #                                  })
      #
      #       } else {
      #         estimatedGVMatEachList <- NULL
      #       }

      estimatedGVMatEachList <- lapply(X = bsInfoEach$populations,
                                       FUN = function(eachPop) {
                                         genoMatNow <- eachPop$genoMat
                                         genoMatWithIntNow <- cbind(Intercept = rep(1, nrow(genoMatNow)),
                                                                    genoMatNow)
                                         estimatedGVMatEachPop <- genoMatWithIntNow[, rownames(lociEffectsInit)] %*% lociEffectsInit

                                         return(estimatedGVMatEachPop)
                                       })

      return(list(trueGVMatEachList = trueGVMatEachList,
                  estimatedGVMatEachList = estimatedGVMatEachList))
    },


    # @description Extract summary results from true GV matrix
    #
    # @param trueGVMatListEach [list] Each list of true GV matrix
    extractTrueSummaryRes = function(trueGVMatListEach) {
      nTopEval <- self$nTopEval

      trueGVSummaryArrayEachList <- lapply(X = trueGVMatListEach,
                                           FUN = function(trueGVMatEachPop) {
                                             trueGVSummaryEachPop <- apply(X = trueGVMatEachPop,
                                                                           MARGIN = 2,
                                                                           FUN = function(trueGVMatEachPopEachTrait) {
                                                                             trueGVSummaryEachPopEachTrait <- c(mean(x = sort(x = trueGVMatEachPopEachTrait, decreasing = TRUE)[1:nTopEval]),
                                                                                                                mean(trueGVMatEachPopEachTrait),
                                                                                                                median(trueGVMatEachPopEachTrait),
                                                                                                                mean(x = sort(x = trueGVMatEachPopEachTrait, decreasing = FALSE)[1:nTopEval]),
                                                                                                                var(trueGVMatEachPopEachTrait))

                                                                             return(trueGVSummaryEachPopEachTrait)
                                                                           })
                                             trueGVSummaryArrayEachPop <- array(data = trueGVSummaryEachPop,
                                                                                dim = c(dim(trueGVSummaryEachPop), 1),
                                                                                dimnames = c(dimnames(trueGVSummaryEachPop),
                                                                                             list(Population = "")))


                                             return(trueGVSummaryArrayEachPop)
                                           })
      trueGVSummaryArrayEach <- do.call(what = abind::abind,
                                        args = trueGVSummaryArrayEachList)
      trueGVSummaryArrayEach <- array(data = trueGVSummaryArrayEach,
                                      dim = c(dim(trueGVSummaryArrayEach), 1),
                                      dimnames = c(dimnames(trueGVSummaryArrayEach),
                                                   list(Iteration = "")))
      return(trueGVSummaryArrayEach)
    },


    # @description Extract summary results from estimated GV matrix
    #
    # @param estimatedGVMatListEach [list] Each list of estimated GV matrix
    extractEstimatedSummaryRes = function(estimatedGVMatListEach) {
      nTopEval <- self$nTopEval

      estimatedGVSummaryArrayEachList <- lapply(X = estimatedGVMatListEach,
                                                FUN = function(estimatedGVMatEachPop) {
                                                  estimatedGVSummaryEachPop <- apply(X = estimatedGVMatEachPop,
                                                                                     MARGIN = 2,
                                                                                     FUN = function(estimatedGVMatEachPopEachTrait) {
                                                                                       estimatedGVSummaryEachPopEachTrait <- c(mean(x = sort(x = estimatedGVMatEachPopEachTrait, decreasing = TRUE)[1:nTopEval]),
                                                                                                                               mean(estimatedGVMatEachPopEachTrait),
                                                                                                                               median(estimatedGVMatEachPopEachTrait),
                                                                                                                               mean(x = sort(x = estimatedGVMatEachPopEachTrait, decreasing = FALSE)[1:nTopEval]),
                                                                                                                               var(estimatedGVMatEachPopEachTrait))

                                                                                       return(estimatedGVSummaryEachPopEachTrait)
                                                                                     })
                                                  estimatedGVSummaryArrayEachPop <- array(data = estimatedGVSummaryEachPop,
                                                                                          dim = c(dim(estimatedGVSummaryEachPop), 1),
                                                                                          dimnames = c(dimnames(estimatedGVSummaryEachPop),
                                                                                                       list(Population = "")))


                                                  return(estimatedGVSummaryArrayEachPop)
                                                })
      estimatedGVSummaryArrayEach <- do.call(what = abind::abind,
                                             args = estimatedGVSummaryArrayEachList)
      estimatedGVSummaryArrayEach <- array(data = estimatedGVSummaryArrayEach,
                                           dim = c(dim(estimatedGVSummaryArrayEach), 1),
                                           dimnames = c(dimnames(estimatedGVSummaryArrayEach),
                                                        list(Iteration = "")))
      return(estimatedGVSummaryArrayEach)
    }
  )
)
