# Author: Kosuke Hamazaki hamazaki@ut-biomet.org
# 2020 The University of Tokyo
#
# Description:
# Definition of simBsOpt class




#' R6 Class Representing a Breeding Scheme
#'
#' @description
#' simBsOpt object store specific information of simulation results of breeding scheme.
#'
# @details
# Details: simBsOpt object store specific information of simulation results of breeding scheme.
#'
#' @export
#' @import R6
simBsOpt <- R6::R6Class(
  "simBsOpt",
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
    #' @field nIterSimulation [numeric] Number of iterations for this simulation setting
    nIterSimulation = NULL,
    #' @field nGenerationProceed [numeric] Number of generations to be proceeded
    nGenerationProceed = NULL,
    #' @field nGenerationProceedSimulation [numeric] Number of generations to be proceeded in simulation for optimizing policy
    nGenerationProceedSimulation = NULL,
    #' @field setGoalAsFinalGeneration [logical] Set goal for simulation of breeding scheme as the real final generation
    setGoalAsFinalGeneration = NULL,
    #' @field performOptimization [logical] Perform optimization of policy in each generation or not
    performOptimization = NULL,
    #' @field useFirstOptimizedValue [logical] Perform optimization of policy only for the initial population and use the optimized hyperprameters permanently.
    useFirstOptimizedValue = NULL,
    #' @field performRobustOptimization [logical] Whether or not performing robust optimization.
    performRobustOptimization = NULL,
    #' @field lowerQuantile [logical] Lower quantile for the robust optimization (value at risk)
    lowerQuantile = NULL,
    #' @field sameAcrossGeneration [logical] Use same hyper prameter across generations or not
    sameAcrossGeneration = NULL,
    #' @field hMin [numeric] Lower bound of hyper parameters
    hMin = NULL,
    #' @field hMax [numeric] Upper bound of hyper parameters
    hMax = NULL,
    #' @field nTotalIterForOneOptimization [numeric] Number of total iterations that can be assigned for one optimization process
    nTotalIterForOneOptimization = NULL,
    #' @field nIterSimulationPerEvaluation [numeric] Number of simulations per one evaluation of the hyperparameter set of your interest
    nIterSimulationPerEvaluation = NULL,
    #' @field nIterSimulationForOneMrkEffect [numeric] Number of simulations per one evaluation of the hyperparameter set of your interest for one set of marker effects
    nIterSimulationForOneMrkEffect = NULL,
    #' @field nIterMrkEffectForRobustOptimization [numeric] Number of sets of marker effects utilized for the robust optimization
    nIterMrkEffectForRobustOptimization = NULL,
    #' @field nIterOptimization [numeric] Number of iterations required for one optimization
    nIterOptimization = NULL,
    #' @field nMaxEvalPerNode [numeric] Number of maximum evaluation per node when optimizing hyper parameter by StoSOO
    nMaxEvalPerNode = NULL,
    #' @field maxDepth [numeric] Maximum depth of tree when optimizing hyper parameter by StoSOO
    maxDepth = NULL,
    #' @field nChildrenPerExpansion [numeric] Number of children per one expansion of nodes when optimizing hyper parameter by StoSOO
    nChildrenPerExpansion = NULL,
    #' @field confidenceParam [numeric] Confidence parameter of StoSOO, this parameter determines the width of the estimates of rewards
    confidenceParam = NULL,
    #' @field returnOptimalNodes [numeric] When (how many iterations) to return (or save) the optimal nodes when optimizing hyper parameter by StoSOO
    returnOptimalNodes = NULL,
    #' @field saveTreeNameBase [character] Base name of the tree to be saved
    saveTreeNameBase = NULL,
    #' @field whenToSaveTrees [numeric] When (how many iterations) to save the tree in StoSOO
    whenToSaveTrees = NULL,
    #' @field currentTreeMid [tree] tree class object that is saved as a `currentTree` in the previous analysis
    currentTreeMid = NULL,
    #' @field optimalNodesMid [list] tree class object that is saved as a `optimalNodes` in the previous analysis
    optimalNodesMid = list(),
    #' @field nTopEvalForOpt [numeric] Number of individuals to be evaluated when evaluating population max or population min for optimization of hyperparameters
    nTopEvalForOpt = NULL,
    #' @field rewardWeightVec [numeric] When returning reward function, `rewardWeightVec` will be multiplied by estimated GVs for each generation to evaluate the method.
    #' If you want to apply discounted method, you can achieve by `rewardWeightVec = sapply(1:nGenerationProceedSimulation, function(genProceedNo) gamma ^ (genProceedNo - 1))` where `gamma` is discounted rate.
    #' Or if you want to evaluate just the final generation, you can achieve by `rewardWeightVec = c(rep(0, nGenerationProceedSimulation - 1), 1)`.
    rewardWeightVec = NULL,
    #' @field digitsEval [numeric] When you evaluate each hyperparameter, you can round the average of evaluates (`eval`) with `round(eval, digitsEval)`.
    digitsEval = NULL,
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
    #' @field updateBreederInfoSimulation [logical] Update breederInfo or not for each generation in simulations for optimization
    updateBreederInfoSimulation = NULL,
    #' @field phenotypingIndsSimulation [logical] Phenotyping individuals or not for each generation in simulations for optimization
    phenotypingIndsSimulation = NULL,
    #' @field nRepForPhenoSimulation [numeric] Number of replications to be phenotyped for each generation in simulations for optimization
    nRepForPhenoSimulation = NULL,
    #' @field updateModelsSimulation [logical] Whether or not updating the model for each generation in simulations for optimization
    #' (If `phenotypingInds = FALSE` for generation of your interest, `updateModels` will be automatically `FALSE` for that generation.)
    updateModelsSimulation = NULL,
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
    #' @field nCoresPerOptimization [numeric] Number of cores used for simulations of breeding scheme when optimizing hyperparameters
    nCoresPerOptimization = NULL,
    #' @field overWriteRes [logical] Overwrite simulation results when the targeted results already exists
    overWriteRes = NULL,
    #' @field showProgress [logical] Show progress bar or not
    showProgress = NULL,
    #' @field returnMethod [character] Which type of results will be returned (saved) in the object
    returnMethod = NULL,
    #' @field saveAllResAt [character] If NULL, we won't save the simulation results. Else, we will save bsInfo and breederInfo for each iteration in the directory defined by `saveAllResAt` path.
    saveAllResAt = NULL,
    #' @field evaluateGVMethod [character] Which type of GV (true GV or estimated GV) will be used for evaluation of the simulation results.
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
    #' @field hLens [numeric] Length of hyperparameter vector
    hLens = NULL,
    #' @field hStart [numeric] Initial values of hyper parameters
    hStart = NULL,
    #' @field solnInit [list] List of solution of StoSOO for initial state
    solnInit = NULL,
    #' @field hVecOptsList [list] List of optimized hyperparameters in each generation
    hVecOptsList = list(),
    #' @field optimalHyperParamMatsList [list] List of optimized hyper parameters given finite numbers of budget for optimization
    optimalHyperParamMatsList = list(),
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



    #' @description Create a new simBsOpt object.
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
    #' @param nIterSimulation [numeric] Number of iterations for this simulation setting
    #' @param nGenerationProceed [numeric] Number of generations to be proceeded
    #' @param nGenerationProceedSimulation [numeric] Number of generations to be proceeded in simulation for optimizing policy
    #' @param setGoalAsFinalGeneration [logical] Set goal for simulation of breeding scheme as the real final generation
    #' @param performOptimization [logical] Perform optimization of policy in each generation or not
    #' @param useFirstOptimizedValue [logical] Perform optimization of policy only for the initial population and use the optimized hyperprameters permanently.
    #' @param performRobustOptimization [logical] Whether or not performing robust optimization.
    #' @param lowerQuantile [logical] Lower quantile for the robust optimization (value at risk)
    #' @param sameAcrossGeneration [logical] Use same hyper prameter across generations or not
    #' @param hMin [numeric] Lower bound of hyper parameters
    #' @param hMax [numeric] Upper bound of hyper parameters
    #' @param nTotalIterForOneOptimization [numeric] Number of total iterations that can be assigned for one optimization process
    #' @param nIterSimulationPerEvaluation [numeric] Number of simulations per one evaluation of the hyperparameter set of your interest
    #' @param nIterSimulationForOneMrkEffect [numeric] Number of simulations per one evaluation of the hyperparameter set of your interest for one set of marker effects
    #' @param nIterMrkEffectForRobustOptimization [numeric] Number of sets of marker effects utilized for the robust optimization
    #' @param nIterOptimization [numeric] Number of iterations required for one optimization
    #' @param nMaxEvalPerNode [numeric] Number of maximum evaluation per node when optimizing hyper parameter by StoSOO
    #' @param maxDepth [numeric] Maximum depth of tree when optimizing hyper parameter by StoSOO
    #' @param nChildrenPerExpansion [numeric] Number of children per one expansion of nodes when optimizing hyper parameter by StoSOO
    #' @param confidenceParam [numeric] Confidence parameter of StoSOO, this parameter determines the width of the estimates of rewards
    #' @param returnOptimalNodes [numeric] When (how many iterations) to return (or save) the optimal nodes when optimizing hyper parameter by StoSOO
    #' @param saveTreeNameBase [character] Base name of the tree to be saved
    #' @param whenToSaveTrees [numeric] When (how many iterations) to save the tree in StoSOO
    #' @param currentTreeMid [tree] tree class object that is saved as a `currentTree` in the previous analysis
    #' @param optimalNodesMid [list] tree class object that is saved as a `optimalNodes` in the previous analysis
    #' @param nTopEvalForOpt [numeric] Number of individuals to be evaluated when evaluating population max or population min for optimization of hyperparameters
    #' @param rewardWeightVec [numeric] When returning reward function, `rewardWeightVec` will be multiplied by estimated GVs for each generation to evaluate the method.
    #' If you want to apply discounted method, you can achieve by `rewardWeightVec = sapply(1:nGenerationProceedSimulation, function(genProceedNo) gamma ^ (genProceedNo - 1))` where `gamma` is discounted rate.
    #' Or if you want to evaluate just the final generation, you can achieve by `rewardWeightVec = c(rep(0, nGenerationProceedSimulation - 1), 1)`.
    #' @param digitsEval [numeric] When you evaluate each hyperparameter, you can round the average of evaluates (`eval`) with `round(eval, digitsEval)`.
    #' @param nRefreshMemoryEvery [numeric] Every `nRefreshMemoryEvery` iterations, we refresh memory used for simulations by `gc(reset = TRUE)`.
    #' @param updateBreederInfo [logical] Update breederInfo or not for each generation
    #' @param phenotypingInds [logical] Phenotyping individuals or not for each generation
    #' @param nRepForPhenoInit [numeric] Number of replications to be phenotyped for initial population
    #' @param nRepForPheno [numeric] Number of replications to be phenotyped for each generation
    #' @param updateModels [logical] Whether or not updating the model for each generation
    #' (If `phenotypingInds = FALSE` for generation of your interest, `updateModels` will be automatically `FALSE` for that generation.)
    #' @param updateBreederInfoSimulation [logical] Update breederInfo or not for each generation in simulations for optimization
    #' @param phenotypingIndsSimulation [logical] Phenotyping individuals or not for each generation in simulations for optimization
    #' @param nRepForPhenoSimulation [numeric] Number of replications to be phenotyped for each generation in simulations for optimization
    #' @param updateModelsSimulation [logical] Whether or not updating the model for each generation in simulations for optimization
    #' (If `phenotypingInds = FALSE` for generation of your interest, `updateModels` will be automatically `FALSE` for that generation.)
    #' @param performRobustOptimization [logical] Whether or not performing robust optimization.
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
    #' @param minimumUnitAllocateVec [numeric] (vector of) Minimum number of allocated progenies for each pair.
    #' @param includeGVPVec [logical] Whether or not to consider genetic variance of progenies of each pair when determining the number of progenies per each pair for each generation
    #' @param nNextPopVec [numeric] Number of progenies for the next generation  for each generation
    #' @param nameMethod [character] Method for naming individuals
    #' @param nCores [numeric] Number of cores used for simulations of breeding scheme
    #' @param nCoresPerOptimization [numeric] Number of cores used for simulations of breeding scheme when optimizing hyperparameters
    #' @param overWriteRes [logical] Overwrite simulation results when the targeted results already exists
    #' @param showProgress [logical] Show progress bar or not
    #' @param returnMethod [character] Which type of results will be returned (saved) in the object
    #' @param saveAllResAt [character] If NULL, we won't save the simulation results. Else, we will save bsInfo and breederInfo for each iteration in the directory defined by `saveAllResAt` path.
    #' @param evaluateGVMethod [character] Which type of GV (true GV or estimated GV) will be used for evaluation of the simulation results
    #' @param nTopEval [numeric] Number of individuals to be evaluated when evaluating population max or population min#'
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
                          nIterSimulation = NULL,
                          nGenerationProceed = NULL,
                          nGenerationProceedSimulation = NULL,
                          setGoalAsFinalGeneration = TRUE,
                          performOptimization = NULL,
                          useFirstOptimizedValue = NULL,
                          performRobustOptimization = FALSE,
                          lowerQuantile = NULL,
                          sameAcrossGeneration = TRUE,
                          hMin = NULL,
                          hMax = NULL,
                          nTotalIterForOneOptimization = NULL,
                          nIterSimulationPerEvaluation = NULL,
                          nIterSimulationForOneMrkEffect = NULL,
                          nIterMrkEffectForRobustOptimization = NULL,
                          nIterOptimization = NULL,
                          nMaxEvalPerNode = NULL,
                          maxDepth = NULL,
                          nChildrenPerExpansion = NULL,
                          confidenceParam = NULL,
                          returnOptimalNodes = NULL,
                          saveTreeNameBase = NULL,
                          whenToSaveTrees = NA,
                          currentTreeMid = NULL,
                          optimalNodesMid = list(),
                          nTopEvalForOpt = NULL,
                          rewardWeightVec = NULL,
                          digitsEval = NULL,
                          nRefreshMemoryEvery = NULL,
                          updateBreederInfo = TRUE,
                          phenotypingInds = FALSE,
                          nRepForPhenoInit = NULL,
                          nRepForPheno = NULL,
                          updateModels = FALSE,
                          updateBreederInfoSimulation = TRUE,
                          phenotypingIndsSimulation = FALSE,
                          nRepForPhenoSimulation = NULL,
                          updateModelsSimulation = FALSE,
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
                          minimumUnitAllocateVec = NULL,
                          includeGVPVec = FALSE,
                          nNextPopVec = NULL,
                          nameMethod = "pairBase",
                          nCores = NULL,
                          nCoresPerOptimization = NULL,
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


      # nGenerationProceedSimulation
      if (!is.null(nGenerationProceedSimulation)) {
        stopifnot(is.numeric(nGenerationProceedSimulation))
        nGenerationProceedSimulation <- floor(nGenerationProceedSimulation)
        stopifnot(nGenerationProceedSimulation >= 1)
      } else {
        nGenerationProceedSimulation <- nGenerationProceed
        message(paste0("`nGenerationProceedSimulation` is not specified. We substitute `nGenerationProceedSimulation = ",
                       nGenerationProceedSimulation,"` instead."))
      }


      # setGoalAsFinalGeneration
      stopifnot(is.logical(setGoalAsFinalGeneration))

      # performOptimization
      if (!is.null(performOptimization)) {
        stopifnot(is.logical(performOptimization))
      } else {
        performOptimization <- rep(FALSE, nGenerationProceed)
        performOptimization[1] <- TRUE

        message(paste0("`performOptimization` is not specified. Instead, we substitute `performOptimization = c(",
                       paste(performOptimization, collapse = ", "), ")`"))
      }

      if (!(length(performOptimization) %in% c(1, nGenerationProceed))) {
        stop(paste("length(performOptimization) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(performOptimization) == 1) {
        performOptimization <- rep(performOptimization, nGenerationProceed)
      }
      names(performOptimization) <- 1:nGenerationProceed


      # useFirstOptimizedValue
      if (!is.null(useFirstOptimizedValue)){
        stopifnot(is.logical(useFirstOptimizedValue))

        if (useFirstOptimizedValue) {
          if (nGenerationProceed != nGenerationProceedSimulation) {
            useFirstOptimizedValue <- FALSE
          }

          if (any(performOptimization[-1])) {
            useFirstOptimizedValue <- FALSE
          }

          if (!useFirstOptimizedValue) {
            message(paste0("You cannot set `useFirstOptimizedValue` as `TRUE` with this `nGenerationProceed`, `nGenerationProceedSimulation`, and `performOptimization`. We substitute `useFirstOptimizedValue = ",
                           useFirstOptimizedValue,"` instead."))
          }
        }
      } else {
        if ((nGenerationProceed == nGenerationProceedSimulation) | (!all(performOptimization[-1]))) {
          useFirstOptimizedValue <- TRUE
        } else {
          useFirstOptimizedValue <- FALSE
        }

        message(paste0("`useFirstOptimizedValue` is not specified. We substitute `useFirstOptimizedValue = ",
                       useFirstOptimizedValue,"` instead."))
      }


      # performRobustOptimization
      if (performRobustOptimization & (lociEffMethod == "true")) {
        performRobustOptimization <- FALSE
        message(paste0("When `lociEffMethod == 'true'`, we don't need to perform robust optimization. We substitute `performRobustOptimization = ",
                       performRobustOptimization,"` instead."))
      }

      # lowerQuantile
      if (!is.null(lowerQuantile)) {
        stopifnot(is.numeric(lowerQuantile))
        stopifnot(lowerQuantile > 0)
        stopifnot(lowerQuantile < 1)
      } else {
        lowerQuantile <- 0.05
        message(paste0("`lowerQuantile` is not specified. We substitute `lowerQuantile = ",
                       lowerQuantile,"` instead."))
      }

      # nTotalIterForOneOptimization
      if (!is.null(nTotalIterForOneOptimization)) {
        stopifnot(is.numeric(nTotalIterForOneOptimization))
        nTotalIterForOneOptimization <- floor(nTotalIterForOneOptimization)
        stopifnot(nTotalIterForOneOptimization >= 1)
      } else {
        nTotalIterForOneOptimization <- 1e04
        message(paste0("`nTotalIterForOneOptimization` is not specified. We substitute `nTotalIterForOneOptimization = ",
                       nTotalIterForOneOptimization,"` instead."))
      }


      # nIterSimulationPerEvaluation
      if (!is.null(nIterSimulationPerEvaluation)) {
        stopifnot(is.numeric(nIterSimulationPerEvaluation))
        nIterSimulationPerEvaluation <- floor(nIterSimulationPerEvaluation)
        stopifnot(nIterSimulationPerEvaluation >= 1)
      } else {
        nIterSimulationPerEvaluation <- 10
        message(paste0("`nIterSimulationPerEvaluation` is not specified. We substitute `nIterSimulationPerEvaluation = ",
                       nIterSimulationPerEvaluation,"` instead."))
      }

      # nIterSimulationForOneMrkEffect
      if (!is.null(nIterSimulationForOneMrkEffect)) {
        stopifnot(is.numeric(nIterSimulationForOneMrkEffect))
        nIterSimulationForOneMrkEffect <- floor(nIterSimulationForOneMrkEffect)
        stopifnot(nIterSimulationForOneMrkEffect >= 1)
      } else {
        if (performRobustOptimization) {
          nIterSimulationForOneMrkEffect <- 1
        } else {
          nIterSimulationForOneMrkEffect <- nIterSimulationPerEvaluation
        }
        message(paste0("`nIterSimulationForOneMrkEffect` is not specified. We substitute `nIterSimulationForOneMrkEffect = ",
                       nIterSimulationForOneMrkEffect,"` instead."))
      }


      # nIterOptimization
      if (!is.null(nIterMrkEffectForRobustOptimization)) {
        stopifnot(is.numeric(nIterMrkEffectForRobustOptimization))
        nIterMrkEffectForRobustOptimization <- floor(nIterMrkEffectForRobustOptimization)
        stopifnot(nIterMrkEffectForRobustOptimization >= 1)
      } else {
        if (performRobustOptimization) {
          nIterMrkEffectForRobustOptimization <- round(nIterSimulationPerEvaluation / nIterSimulationForOneMrkEffect)
        } else {
          nIterMrkEffectForRobustOptimization <- 1
        }

        message(paste0("`nIterMrkEffectForRobustOptimization` is not specified. We substitute `nIterMrkEffectForRobustOptimization = ",
                       nIterMrkEffectForRobustOptimization,"` instead."))
      }


      if (nIterSimulationPerEvaluation != nIterSimulationForOneMrkEffect * nIterMrkEffectForRobustOptimization) {
        nIterSimulationPerEvaluation <- nIterSimulationForOneMrkEffect * nIterMrkEffectForRobustOptimization
        message((paste0("We substitute `nIterSimulationPerEvaluation = ",
                        nIterSimulationPerEvaluation,"` instead.")))
      }


      # nIterOptimization
      if (!is.null(nIterOptimization)) {
        stopifnot(is.numeric(nIterOptimization))
        nIterOptimization <- floor(nIterOptimization)
        stopifnot(nIterOptimization >= 1)
      } else {
        nIterOptimization <- round(nTotalIterForOneOptimization / nIterSimulationPerEvaluation)
        message(paste0("`nIterOptimization` is not specified. We substitute `nIterOptimization = ",
                       nIterOptimization,"` instead."))
      }

      if (nTotalIterForOneOptimization != nIterSimulationPerEvaluation * nIterOptimization) {
        nTotalIterForOneOptimization <- nIterSimulationPerEvaluation * nIterOptimization
        message((paste0("We substitute `nTotalIterForOneOptimization = ",
                        nTotalIterForOneOptimization,"` instead.")))
      }


      # nChildrenPerExpansion
      if (!is.null(nChildrenPerExpansion)) {
        stopifnot(is.numeric(nChildrenPerExpansion))
        nChildrenPerExpansion <- floor(x = nChildrenPerExpansion)
        stopifnot(nChildrenPerExpansion >= 2)
      } else {
        nChildrenPerExpansion <- 3
        message(paste0("You do not specify `nChildrenPerExpansion`. We set `nChildrenPerExpansion = ",
                       nChildrenPerExpansion, "`."))
      }


      # nMaxEvalPerNode
      if (!is.null(nMaxEvalPerNode)) {
        stopifnot(is.numeric(nMaxEvalPerNode))
        nMaxEvalPerNode <- ceiling(x = nMaxEvalPerNode)
        stopifnot(nMaxEvalPerNode >= 1)
      } else {
        nMaxEvalPerNode <- ceiling(x = nIterOptimization / (log(x = nIterOptimization) ^ 3))
        message(paste0("You do not specify `nMaxEvalPerNode`. We set `nMaxEvalPerNode = ",
                       nMaxEvalPerNode, "`."))
      }


      # maxDepth
      if (!is.null(maxDepth)) {
        stopifnot(is.numeric(maxDepth))
        maxDepth <- ceiling(x = maxDepth)
        stopifnot(maxDepth >= 1)
      } else {
        maxDepth <- ceiling(x = sqrt(x = nIterOptimization / nMaxEvalPerNode))
        message(paste0("You do not specify `maxDepth`. We set `maxDepth = ",
                       maxDepth, "`."))
      }
      maxDepthInR <- floor(x = logb(x = 9e15, base = nChildrenPerExpansion))
      if (maxDepth > maxDepthInR) {
        message(paste0("This function can only treat depth smaller than ", maxDepthInR, ".\n",
                       "We set `maxDepth = ", maxDepthInR, ".` We're sorry."))
        maxDepth <- maxDepthInR
      }


      # confidenceParam
      if (!is.null(confidenceParam)) {
        stopifnot(is.numeric(confidenceParam))
      } else {
        confidenceParam <- 1 / sqrt(x = nIterOptimization)
        message(paste0("You do not specify `confidenceParam`. We set `confidenceParam = ",
                       round(confidenceParam, 3), "`."))
      }


      # returnOptimalNodes
      if (!is.null(returnOptimalNodes)) {
        stopifnot(is.numeric(returnOptimalNodes))
        returnOptimalNodes <- floor(returnOptimalNodes)
        stopifnot(all(returnOptimalNodes >= 1))
        stopifnot(all(returnOptimalNodes <= nIterOptimization))
      } else {
        returnOptimalNodes <- 1:nIterOptimization
        message(paste0("You do not specify `returnOptimalNodes`. We set `returnOptimalNodes = 1:",
                       max(returnOptimalNodes), "`."))
      }


      # saveTreeNameBase
      if (is.null(saveTreeNameBase)) {
        whenToSaveTrees <- NULL
      }

      # whenToSaveTrees
      if (!is.null(whenToSaveTrees)) {
        if (!all(is.na(whenToSaveTrees))) {
          stopifnot(is.numeric(whenToSaveTrees))
          whenToSaveTrees <- whenToSaveTrees[!is.na(whenToSaveTrees)]
          whenToSaveTrees <- floor(whenToSaveTrees)
          stopifnot(all(whenToSaveTrees >= 1))
          stopifnot(all(whenToSaveTrees <= nIterOptimization))
        } else {
          whenToSaveTrees <- nIterOptimization
          message(paste0("You do not specify `whenToSaveTrees`. We set `whenToSaveTrees = ",
                         whenToSaveTrees, "`."))
        }
      }


      # currentTreeMid
      if (!is.null(currentTreeMid)) {
        if (!("tree" %in% class(currentTreeMid))) {
          currentTreeMid <- NULL
          message(paste0("Your `currentTreeMid` object is not `tree` class. We set `currentTreeMid = NULL`."))
        }
      }


      # optimalNodesMid
      if (is.list(optimalNodesMid)) {
        if (length(optimalNodesMid) != 0) {
          isNodeVec <- sapply(X = optimalNodesMid,
                              FUN = function(optimalNode) {
                                "node" %in% class(optimalNode)
                              })

          if (!all(isNodeVec)) {
            optimalNodesMid <- list()
            message(paste0("Your `optimalNodesMid` object does not consist of the objects of `node` class. We set `optimalNodesMid = list()`."))
          }
        }
      } else {
        optimalNodesMid <- list()
        message(paste0("Your `optimalNodesMid` object is not `list` class. We set `optimalNodesMid = list()`."))
      }


      # rewardWeightVec
      if (!is.null(rewardWeightVec)) {
        stopifnot(is.numeric(rewardWeightVec))
        stopifnot(all(rewardWeightVec >= 0))
      } else {
        rewardWeightVec <- c(rep(0, nGenerationProceedSimulation - 1), 1)
        message(paste0("`rewardWeightVec` is not specified. Instead, we substitute `rewardWeightVec = c(",
                       paste(rewardWeightVec, collapse = ", "), ")`."))
      }

      if (!(length(rewardWeightVec) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(rewardWeightVec) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(rewardWeightVec) == 1) {
        rewardWeightVec <- rep(rewardWeightVec, nGenerationProceedSimulation)
      }
      names(rewardWeightVec) <- 1:nGenerationProceedSimulation


      # digitsEval
      if (!is.null(digitsEval)) {
        stopifnot(is.numeric(digitsEval))
        digitsEval <- floor(digitsEval)
      } else {
        digitsEval <- 3
        message(paste0("`digitsEval` is not specified. We substitute `digitsEval = ",
                       digitsEval,"` instead."))
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


      # updateBreederInfoSimulation
      stopifnot(is.logical(updateBreederInfoSimulation))

      if (!(length(updateBreederInfoSimulation) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(updateBreederInfoSimulation) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(updateBreederInfoSimulation) == 1) {
        updateBreederInfoSimulation <- rep(updateBreederInfoSimulation, nGenerationProceedSimulation)
      }
      names(updateBreederInfoSimulation) <- 1:nGenerationProceedSimulation


      # phenotypingIndsSimulation
      stopifnot(is.logical(phenotypingIndsSimulation))

      if (!(length(phenotypingIndsSimulation) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(phenotypingIndsSimulation) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(phenotypingIndsSimulation) == 1) {
        phenotypingIndsSimulation <- rep(phenotypingIndsSimulation, nGenerationProceedSimulation)
      }
      names(phenotypingIndsSimulation) <- 1:nGenerationProceedSimulation
      phenotypingIndsSimulation[!updateBreederInfoSimulation] <- FALSE




      # nRepForPhenoSimulation
      if (!is.null(nRepForPhenoSimulation)) {
        stopifnot(is.numeric(nRepForPhenoSimulation))
        nRepForPhenoSimulation <- floor(nRepForPhenoSimulation)
        stopifnot(all(nRepForPhenoSimulation >= 0))
      } else {
        nRepForPhenoSimulation <- 1
        message(paste0("`nRepForPhenoSimulation` is not specified. We substitute `nRepForPhenoSimulation = ",
                       nRepForPhenoSimulation,"` instead."))
      }

      if (!(length(nRepForPhenoSimulation) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(nRepForPhenoSimulation) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(nRepForPhenoSimulation) == 1) {
        nRepForPhenoSimulation <- rep(nRepForPhenoSimulation, nGenerationProceedSimulation)
      }
      names(nRepForPhenoSimulation) <- 1:nGenerationProceedSimulation

      nRepForPhenoSimulation[!phenotypingIndsSimulation] <- 0



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


      # updateModels
      stopifnot(is.logical(updateModels))

      if (!(length(updateModels) %in% c(1, nGenerationProceed))) {
        stop(paste("length(updateModels) must be equal to 1 or equal to nGenerationProceed."))
      } else if (length(updateModels) == 1) {
        updateModels <- rep(updateModels, nGenerationProceed)
      }
      names(updateModels) <- 1:nGenerationProceed

      updateModels[!phenotypingInds] <- FALSE


      # updateModelsSimulation
      stopifnot(is.logical(updateModelsSimulation))

      if (!(length(updateModelsSimulation) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(updateModelsSimulation) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(updateModelsSimulation) == 1) {
        updateModelsSimulation <- rep(updateModelsSimulation, nGenerationProceedSimulation)
      }
      names(updateModelsSimulation) <- 1:nGenerationProceedSimulation

      updateModelsSimulation[!phenotypingIndsSimulation] <- FALSE


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

      if (!(length(nSelectionWaysVec) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(nSelectionWaysVec) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(nSelectionWaysVec) == 1) {
        nSelectionWaysVec <- rep(nSelectionWaysVec, nGenerationProceedSimulation)
      }
      names(nSelectionWaysVec) <- 1:nGenerationProceedSimulation


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

      if (!(length(selectionMethodList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(selectionMethodList) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(selectionMethodList) == 1) {
        selectionMethodList <- rep(selectionMethodList, nGenerationProceedSimulation)
      }
      names(selectionMethodList) <- 1:nGenerationProceedSimulation
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

      if (!(length(traitNoSelList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(traitNoSelList) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(traitNoSelList) == 1) {
        traitNoSelList <- rep(traitNoSelList, nGenerationProceedSimulation)
      }
      stopifnot(all(unlist(lapply(traitNoSelList, is.list))))
      stopifnot(all(unlist(lapply(traitNoSelList, function(traitNoSel) all(unlist(lapply(traitNoSel, is.numeric)))))))
      stopifnot(all(sapply(traitNoSelList, function(traitNoSel) all(unlist(lapply(traitNoSel, function(x) all(x >= 1)))))))
      stopifnot(all(sapply(traitNoSelList, function(traitNoSel) all(unlist(lapply(traitNoSel, function(x) all(x <= nTraits)))))))
      stopifnot(all(unlist(lapply(traitNoRAList, length)) == nSelectionWaysVec))

      names(traitNoSelList) <- 1:nGenerationProceedSimulation


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

      if (!(length(clusteringForSelList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(clusteringForSelList) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(clusteringForSelList) == 1) {
        clusteringForSelList <- rep(clusteringForSelList, nGenerationProceedSimulation)
      }
      names(clusteringForSelList) <- 1:nGenerationProceedSimulation
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

      if (!(length(nSelList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(nSelList) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(nSelList) == 1) {
        nSelList <- rep(nSelList, nGenerationProceedSimulation)
      }
      stopifnot(all(unlist(lapply(nSelList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(nSelList, is.numeric))))

      nSelList <- sapply(X = 1:nGenerationProceedSimulation,
                         FUN = function(generationProceedNo) {
                           nSel <- nSelList[[generationProceedNo]]
                           whereSelection <- whereSelectionList[[generationProceedNo]]

                           nSel[!whereSelection] <- nIndNow

                           return(nSel)
                         }, simplify = FALSE)


      names(nSelList) <- 1:nGenerationProceedSimulation


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

      if (!(length(nSelInitOPVList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(nSelInitOPVList) must be equal to 1 or equal to nGenerationProceedSimulation"))
      } else if (length(nSelInitOPVList) == 1) {
        nSelInitOPVList <- rep(nSelInitOPVList, nGenerationProceedSimulation)
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

        nSelInitOPVList[whereNSelSatisfy] <- sapply(X = (1:nGenerationProceedSimulation)[whereNSelSatisfy],
                                                    FUN = function(generationProceedNo) {
                                                      nSelInitOPV <- nSelInitOPVList[generationProceedNo]
                                                      nSel <- nSelList[generationProceedNo]

                                                      nSelInitOPV[nSelInitOPV < nSel] <- nSel[nSelInitOPV < nSel]

                                                      return(nSelInitOPV)
                                                    }, simplify = FALSE)
      }

      names(nSelInitOPVList) <- 1:nGenerationProceedSimulation



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

      if (!(length(nClusterList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(nClusterList) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(nClusterList) == 1) {
        nClusterList <- rep(nClusterList, nGenerationProceedSimulation)
      }
      stopifnot(all(unlist(lapply(nClusterList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(nClusterList, is.numeric))))

      nClusterList <- sapply(X = 1:nGenerationProceedSimulation,
                             FUN = function(generationProceedNo) {
                               nCluster <- nClusterList[[generationProceedNo]]
                               clusteringForSel <- clusteringForSelList[[generationProceedNo]]

                               nCluster[!clusteringForSel] <- 1

                               return(nCluster)
                             }, simplify = FALSE)


      names(nClusterList) <- 1:nGenerationProceedSimulation


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

      if (!(length(nTopClusterList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(nTopClusterList) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(nTopClusterList) == 1) {
        nTopClusterList <- rep(nTopClusterList, nGenerationProceedSimulation)
      }
      stopifnot(all(unlist(lapply(nTopClusterList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(nTopClusterList, is.numeric))))

      nTopClusterList <- sapply(X = 1:nGenerationProceedSimulation,
                                FUN = function(generationProceedNo) {
                                  nTopCluster <- nTopClusterList[[generationProceedNo]]
                                  clusteringForSel <- clusteringForSelList[[generationProceedNo]]

                                  nTopCluster[!clusteringForSel] <- 1

                                  return(nTopCluster)
                                }, simplify = FALSE)


      names(nTopClusterList) <- 1:nGenerationProceedSimulation



      # nTopEachList
      if (!is.null(nTopEachList)) {
        if (!is.list(nTopEachList)) {
          nTopEachList <- list(nTopEachList)
        }
      } else {
        nTopEachList <- sapply(X = 1:nGenerationProceedSimulation,
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

      if (!(length(nTopEachList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(nTopEachList) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(nTopEachList) == 1) {
        nTopEachList <- rep(nTopEachList, nGenerationProceedSimulation)
      }
      stopifnot(all(unlist(lapply(nTopEachList, length)) == nSelectionWaysVec))
      stopifnot(all(unlist(lapply(nTopEachList, is.numeric))))

      names(nTopEachList) <- 1:nGenerationProceedSimulation


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

      if (!(length(multiTraitsEvalMethodList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(multiTraitsEvalMethodList) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(multiTraitsEvalMethodList) == 1) {
        multiTraitsEvalMethodList <- rep(multiTraitsEvalMethodList, nGenerationProceedSimulation)
      }
      names(multiTraitsEvalMethodList) <- 1:nGenerationProceedSimulation
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
        hSelList <- sapply(1:nGenerationProceedSimulation,
                           function(generationProceedNo) {

                             hSelListNow <- sapply(X = traitNoSelList[[generationProceedNo]],
                                                   FUN = function(traitNoSelNow) {
                                                     rep(hSelList, length(traitNoSelNow))
                                                   }, simplify = FALSE)

                             return(hSelListNow)
                           }, simplify = FALSE)
      }

      if (!(length(hSelList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(hSelList) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(hSelList) == 1) {
        hSelList <- rep(hSelList, nGenerationProceedSimulation)
      }
      stopifnot(all(unlist(lapply(hSelList, is.list))))
      stopifnot(all(unlist(lapply(hSelList, function(hSel) all(unlist(lapply(hSel, is.numeric)))))))
      stopifnot(all(sapply(hSelList, function(hSel) all(unlist(lapply(hSel, function(x) all(x >= 0)))))))
      stopifnot(all(unlist(lapply(hSelList, length)) == nSelectionWaysVec))

      names(hSelList) <- 1:nGenerationProceedSimulation

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

      if (!(length(matingMethodVec) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(matingMethodVec) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(matingMethodVec) == 1) {
        matingMethodVec <- rep(matingMethodVec, nGenerationProceedSimulation)
      }
      names(matingMethodVec) <- 1:nGenerationProceedSimulation


      # allocateMethodVec
      if (!is.null(allocateMethodVec)) {
        stopifnot(is.character(allocateMethodVec))
        stopifnot(all(allocateMethodVec %in% allocateMethodsOffered))
      } else {
        allocateMethodVec <- "equalAllocation"
        message(paste0("`allocateMethodVec` is not specified. We substitute `allocateMethodVec = ",
                       allocateMethodVec,"` instead."))
      }

      if (!(length(allocateMethodVec) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(allocateMethodVec) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(allocateMethodVec) == 1) {
        allocateMethodVec <- rep(allocateMethodVec, nGenerationProceedSimulation)
      }
      names(allocateMethodVec) <- 1:nGenerationProceedSimulation



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

      if (!(length(weightedAllocationMethodList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(weightedAllocationMethodList) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(weightedAllocationMethodList) == 1) {
        weightedAllocationMethodList <- rep(weightedAllocationMethodList, nGenerationProceedSimulation)
      }
      weightedAllocationMethodList <- lapply(weightedAllocationMethodList, function(x) x[x %in% selectionMethodsWithSelection])

      names(weightedAllocationMethodList) <- 1:nGenerationProceedSimulation
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

      if (!(length(traitNoRAList) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(traitNoRAList) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(traitNoRAList) == 1) {
        traitNoRAList <- rep(traitNoRAList, nGenerationProceedSimulation)
      }
      stopifnot(all(unlist(lapply(traitNoRAList, is.numeric))))
      stopifnot(all(sapply(traitNoRAList, function(traitNoRA) all(traitNoRA >= 1))))

      names(traitNoRAList) <- 1:nGenerationProceedSimulation


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

      if (!(length(minimumUnitAllocateVec) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(minimumUnitAllocateVec) must be equal to 1 or equal to nGenerationProceedSimulation"))
      } else if (length(minimumUnitAllocateVec) == 1) {
        minimumUnitAllocateVec <- rep(minimumUnitAllocateVec, nGenerationProceedSimulation)
      }
      names(minimumUnitAllocateVec) <- 1:nGenerationProceedSimulation


      # includeGVPVec
      stopifnot(is.logical(includeGVPVec))

      if (!(length(includeGVPVec) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(includeGVPVec) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(includeGVPVec) == 1) {
        includeGVPVec <- rep(includeGVPVec, nGenerationProceedSimulation)
      }
      names(includeGVPVec) <- 1:nGenerationProceedSimulation


      # hLens
      hLens <- sapply(X = 1:nGenerationProceedSimulation,
                      FUN = function(genNow) {
                        length(traitNoRAList[[genNow]]) * (includeGVPVec[genNow] + length(weightedAllocationMethodList[[genNow]]))
                      }, simplify = TRUE)
      hNamesList <- sapply(X = 1:nGenerationProceedSimulation,
                           FUN = function(genNow) {
                             paste(bsInfoInit$traitInfo$traitNames[traitNoRAList[[genNow]]],
                                   c(weightedAllocationMethodList[[genNow]], "genVarProgeny"), sep = "-")[1:hLens[genNow]]
                           }, simplify = FALSE)

      names(hNamesList) <- paste0("Generation_", 1:nGenerationProceedSimulation)
      hVecNamesAll <- paste0(rep(names(hNamesList), hLens), "-", unlist(hNamesList, use.names = FALSE))


      # sameAcrossGeneration
      stopifnot(is.logical(sameAcrossGeneration))
      if (!((length(unique(traitNoRAList)) == 1) & (length(unique(weightedAllocationMethodList)) == 1))) {
        sameAcrossGeneration <- FALSE
        message(paste0("You cannot set `sameAcrossGeneration` as `TRUE` with this `traitNoRAList` and `weightedAllocationMethodList`. We substitute `sameAcrossGeneration = ",
                       sameAcrossGeneration,"` instead."))
      }
      hVecLen <- ifelse(sameAcrossGeneration, max(hLens), sum(hLens))
      if (sameAcrossGeneration) {
        hVecNames <- hNamesList[[which.max(hLens)]]
      } else {
        hVecNames <- hVecNamesAll
      }


      # hMin
      if (!is.null(hMin)) {
        stopifnot(is.numeric(hMin))
        stopifnot(hMin >= 0)
      } else {
        hMin <- 0
        message(paste0("`hMin` is not specified. We substitute `hMin = ",
                       hMin,"` instead."))
      }


      if (!(length(hMin) %in% c(1, hVecLen))) {
        stop(paste0("length(hMin) must be equal to 1 or equal to ", hVecLen, "."))
      } else if (length(hMin) == 1) {
        hMin <- rep(hMin, hVecLen)
      }


      # hMax
      if (!is.null(hMax)) {
        stopifnot(is.numeric(hMax))
        stopifnot(hMax > 0)
      } else {
        hMax <- 2
        message(paste0("`hMax` is not specified. We substitute `hMax = ",
                       hMax,"` instead."))
      }


      if (!(length(hMax) %in% c(1, hVecLen))) {
        stop(paste0("length(hMax) must be equal to 1 or equal to ", hVecLen, "."))
      } else if (length(hMax) == 1) {
        hMax <- rep(hMax, hVecLen)
      }


      # hStart
      hStart <- (hMin + hMax) / 2
      names(hMin) <- names(hMax) <- names(hStart) <- hVecNames

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

      if (!(length(nNextPopVec) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(nNextPopVec) must be equal to 1 or equal to nGenerationProceedSimulation."))
      } else if (length(nNextPopVec) == 1) {
        nNextPopVec <- rep(nNextPopVec, nGenerationProceedSimulation)
      }
      names(nNextPopVec) <- 1:nGenerationProceedSimulation


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


      if (!(length(nProgeniesEMBVVec) %in% c(1, nGenerationProceedSimulation))) {
        stop(paste("length(nProgeniesEMBVVec) must be equal to 1 or equal to nGenerationProceedSimulation"))
      } else if (length(nProgeniesEMBVVec) == 1) {
        nProgeniesEMBVVec <- rep(nProgeniesEMBVVec, nGenerationProceedSimulation)
      }
      names(nProgeniesEMBVVec) <- 1:nGenerationProceedSimulation


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



      # nTopEvalForOpt
      if (!is.null(nTopEvalForOpt)) {
        stopifnot(is.numeric(nTopEvalForOpt))
        nTopEvalForOpt <- floor(nTopEvalForOpt)
        stopifnot(nTopEvalForOpt >= 1)
        stopifnot(nTopEvalForOpt <= min(nNextPopVec))
      } else {
        nTopEvalForOpt <- 1
        message(paste0("`nTopEvalForOpt` is not specified. We substitute `nTopEvalForOpt = ",
                       nTopEvalForOpt,"` instead."))
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


      # nCoresPerOptimization
      if (!is.null(nCoresPerOptimization)) {
        stopifnot(is.numeric(nCoresPerOptimization))
        nCoresPerOptimization <- floor(nCoresPerOptimization)
        stopifnot(nCoresPerOptimization >= 1)
      } else {
        nCoresPerOptimization <- 1
        message(paste0("`nCoresPerOptimization` is not specified. We substitute `nCoresPerOptimization = ",
                       nCoresPerOptimization,"` instead."))
      }

      if (nCoresPerOptimization >= parallel::detectCores()) {
        warning("You are going to assign the number of cores larger than that of your PC to `nCoresPerOptimization` ! Is it OK ?")
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
          message(paste0("There is no directory named '", summaryAllResAt, "'. We will summarize the simulation results inside the `simBsOpt` object."))
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
      self$nIterSimulation <- nIterSimulation
      self$nGenerationProceed <- nGenerationProceed
      self$nGenerationProceedSimulation <- nGenerationProceedSimulation
      self$setGoalAsFinalGeneration <- setGoalAsFinalGeneration
      self$performOptimization <- performOptimization
      self$useFirstOptimizedValue <- useFirstOptimizedValue
      self$performRobustOptimization <- performRobustOptimization
      self$sameAcrossGeneration <- sameAcrossGeneration
      self$lowerQuantile <- lowerQuantile
      self$hMin <- hMin
      self$hMax <- hMax
      self$nTotalIterForOneOptimization <- nTotalIterForOneOptimization
      self$nIterSimulationPerEvaluation <- nIterSimulationPerEvaluation
      self$nIterSimulationForOneMrkEffect <- nIterSimulationForOneMrkEffect
      self$nIterMrkEffectForRobustOptimization <- nIterMrkEffectForRobustOptimization
      self$nIterOptimization <- nIterOptimization
      self$nMaxEvalPerNode <- nMaxEvalPerNode
      self$maxDepth <- maxDepth
      self$nChildrenPerExpansion <- nChildrenPerExpansion
      self$confidenceParam <- confidenceParam
      self$returnOptimalNodes <- returnOptimalNodes
      self$saveTreeNameBase <- saveTreeNameBase
      self$whenToSaveTrees <- whenToSaveTrees
      self$currentTreeMid <- currentTreeMid
      self$optimalNodesMid <- optimalNodesMid
      self$nTopEvalForOpt <- nTopEvalForOpt
      self$rewardWeightVec <- rewardWeightVec
      self$digitsEval <- digitsEval
      self$nRefreshMemoryEvery <- nRefreshMemoryEvery
      self$hLens <- hLens
      self$hStart <- hStart
      self$updateBreederInfo <- updateBreederInfo
      self$phenotypingInds <- phenotypingInds
      self$nRepForPhenoInit <- nRepForPhenoInit
      self$nRepForPheno <- nRepForPheno
      self$updateModels <- updateModels
      self$updateBreederInfoSimulation <- updateBreederInfoSimulation
      self$phenotypingIndsSimulation <- phenotypingIndsSimulation
      self$nRepForPhenoSimulation <- nRepForPhenoSimulation
      self$updateModelsSimulation <- updateModelsSimulation
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
      self$minimumUnitAllocateVec <- minimumUnitAllocateVec
      self$includeGVPVec <- includeGVPVec
      self$nNextPopVec <- nNextPopVec
      self$nameMethod <- nameMethod
      self$nCores <- nCores
      self$nCoresPerOptimization <- nCoresPerOptimization
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
      # estimatedGVMatInit <- list(breederInfoInit$estimatedGVByMLRInfo[[names(bsInfoInit$populations[length(bsInfoInit$populations)])]]$testingEstimatedGVByMLR)

      self$computeLociEffInit()
      lociEffects <- self$lociEffectsInit
      genoMatNow <- bsInfoInit$populations[[length(bsInfoInit$populations)]]$genoMat
      genoMatWithIntNow <- cbind(Intercept = rep(1, nrow(genoMatNow)),
                                 genoMatNow)

      estimatedGVMatInit <- list(genoMatWithIntNow[, rownames(lociEffects)] %*% lociEffects)
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
                                                       methodMLR = methodMLRInit)
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
      lociEffMethod <- self$lociEffMethod
      methodMLRInit <- self$methodMLRInit
      multiTraitInit <- self$multiTraitInit
      nIterSimulation <- self$nIterSimulation
      nGenerationProceed <- self$nGenerationProceed
      nGenerationProceedSimulation <- self$nGenerationProceedSimulation
      setGoalAsFinalGeneration <- self$setGoalAsFinalGeneration
      performOptimization <- self$performOptimization
      useFirstOptimizedValue <- self$useFirstOptimizedValue
      performRobustOptimization <- self$performRobustOptimization
      lowerQuantile <- self$lowerQuantile
      sameAcrossGeneration <- self$sameAcrossGeneration
      hMin <- self$hMin
      hMax <- self$hMax
      nTotalIterForOneOptimization <- self$nTotalIterForOneOptimization
      nIterSimulationPerEvaluation <- self$nIterSimulationPerEvaluation
      nIterSimulationForOneMrkEffect <- self$nIterSimulationForOneMrkEffect
      nIterMrkEffectForRobustOptimization <- self$nIterMrkEffectForRobustOptimization
      nIterOptimization <- self$nIterOptimization
      nMaxEvalPerNode <- self$nMaxEvalPerNode
      maxDepth <- self$maxDepth
      nChildrenPerExpansion <- self$nChildrenPerExpansion
      confidenceParam <- self$confidenceParam
      returnOptimalNodes <- self$returnOptimalNodes
      nTopEvalForOpt <- self$nTopEvalForOpt
      rewardWeightVec <- self$rewardWeightVec
      digitsEval <- self$digitsEval
      nRefreshMemoryEvery <- self$nRefreshMemoryEvery
      hLens <- self$hLens
      hStart <- self$hStart
      updateBreederInfo <- self$updateBreederInfo
      phenotypingInds <- self$phenotypingInds
      nRepForPhenoInit <- self$nRepForPhenoInit
      nRepForPheno <- self$nRepForPheno
      updateModels <- self$updateModels
      updateBreederInfoSimulation <- self$updateBreederInfoSimulation
      phenotypingIndsSimulation <- self$phenotypingIndsSimulation
      nRepForPhenoSimulation <- self$nRepForPhenoSimulation
      updateModelsSimulation <- self$updateModelsSimulation
      methodMLR <- self$methodMLR
      multiTrait <- self$multiTrait
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
      summaryAllResAt <- self$summaryAllResAt
      verbose <- self$verbose


      populationNameInit <- names(bsInfoInit$populations[length(bsInfoInit$populations)])

      iterNames <- paste0("Iteration_", 1:nIterSimulation)
      hVecOptsList <- self$hVecOptsList

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
          self$simBsRes[[simBsName]]$max <- c()
        }

        if ("mean" %in% returnMethod) {
          self$simBsRes[[simBsName]]$mean <- c()
        }

        if ("median" %in% returnMethod) {
          self$simBsRes[[simBsName]]$median <- c()
        }

        if ("min" %in% returnMethod) {
          self$simBsRes[[simBsName]]$min <- c()
        }

        if ("var" %in% returnMethod) {
          self$simBsRes[[simBsName]]$var <- c()
        }
      }


      if (useFirstOptimizedValue) {
        if (verbose) {
          print("Perform optimization of hyperparameters once.")
        }

        # soln <- OOR::StoSOO(par = hStart, fn = private$maximizeFunc,
        #                     nGenerationProceedSimulation = nGenerationProceedSimulation,
        #                     lower = hMin, upper = hMax,
        #                     nb_iter = nIterOptimization,
        #                     control = list(type = "sto", verbose = showProgress, max = TRUE))
        # self$solnInit <- soln
        # hVecOpt <- soln$par

        stoSOONow <- myBreedSimulatR::stoSOO$new(parameter = hStart,
                                                 optimizeFunc = private$maximizeFunc,
                                                 nGenerationProceedSimulation = nGenerationProceedSimulation,
                                                 lowerBound = hMin,
                                                 upperBound = hMax,
                                                 nIterOptimization = nIterOptimization,
                                                 nMaxEvalPerNode = nMaxEvalPerNode,
                                                 maxDepth = maxDepth,
                                                 nChildrenPerExpansion = nChildrenPerExpansion,
                                                 confidenceParam = confidenceParam,
                                                 maximize = TRUE,
                                                 optimizeType = "stochastic",
                                                 returnOptimalNodes = returnOptimalNodes,
                                                 saveTreeNameBase = paste0(self$saveTreeNameBase, "_Initial"),
                                                 whenToSaveTrees = self$whenToSaveTrees,
                                                 currentTree = self$currentTreeMid,
                                                 optimalNodes = self$optimalNodesMid,
                                                 withCheck = TRUE,
                                                 verbose = showProgress)
        stoSOONow$startOptimization()
        optimalNodesList <- stoSOONow$optimalNodes
        optimalHyperParamMat <- stoSOONow$optimalHyperParamMat

        hVecOpt <- stoSOONow$optimalParameter
        self$solnInit <- list(value = stoSOONow$optimalValue,
                              par = hVecOpt)

        self$optimalHyperParamMatsList[["Initial"]] <- optimalHyperParamMat
        hVecOptsList[["Initial"]] <- hVecOpt

        rm(stoSOONow)
        gc(reset = TRUE); gc(reset = TRUE)
        if (sameAcrossGeneration) {
          hListOpt <- sapply(X = hLens,
                             FUN = function(hLen) {
                               hVecOpt[1:hLen]
                             }, simplify = FALSE)
        } else {
          hListOpt <- split(x = hVecOpt, f = rep(1:nGenerationProceedSimulation, hLens))
        }


        # save
        if (verbose) {
          print("Perform simulation based on optimized hyperparameters.")
        }
        simBsOpt <- myBreedSimulatR::simBs$new(simBsName = simBsName,
                                               bsInfoInit = bsInfoInit,
                                               breederInfoInit = breederInfoInit,
                                               lociEffMethod = lociEffMethod,
                                               trainingPopType = trainingPopType,
                                               trainingPopInit = trainingPopInit,
                                               trainingIndNamesInit = trainingIndNamesInit,
                                               methodMLRInit = methodMLRInit,
                                               multiTraitInit = multiTraitInit,
                                               samplingMrkEffInit = FALSE,
                                               seedMrkEffSamplingInit = NA,
                                               nIterSimulation = nIterSimulation,
                                               nGenerationProceed = nGenerationProceed,
                                               nRefreshMemoryEvery = nRefreshMemoryEvery,
                                               updateBreederInfo = updateBreederInfo,
                                               phenotypingInds = phenotypingInds,
                                               nRepForPhenoInit = nRepForPhenoInit,
                                               nRepForPheno = nRepForPheno,
                                               updateModels = updateModels,
                                               methodMLR = methodMLR,
                                               multiTrait = multiTrait,
                                               nSelectionWaysVec = nSelectionWaysVec,
                                               selectionMethodList = selectionMethodList,
                                               traitNoSelList = traitNoSelList,
                                               blockSplitMethod = blockSplitMethod,
                                               nMrkInBlock = nMrkInBlock,
                                               minimumSegmentLength = minimumSegmentLength,
                                               nSelInitOPVList = nSelInitOPVList,
                                               nIterOPV = nIterOPV,
                                               nProgeniesEMBVVec = nProgeniesEMBVVec,
                                               nIterEMBV = nIterEMBV,
                                               nCoresEMBV = nCoresEMBV,
                                               clusteringForSelList = clusteringForSelList,
                                               nClusterList = nClusterList,
                                               nTopClusterList = nTopClusterList,
                                               nTopEachList = nTopEachList,
                                               nSelList = nSelList,
                                               multiTraitsEvalMethodList = multiTraitsEvalMethodList,
                                               hSelList = hSelList,
                                               matingMethodVec = matingMethodVec,
                                               allocateMethodVec = allocateMethodVec,
                                               weightedAllocationMethodList = weightedAllocationMethodList,
                                               includeGVPVec = includeGVPVec,
                                               traitNoRAList = traitNoRAList,
                                               hList = hListOpt,
                                               minimumUnitAllocateVec = minimumUnitAllocateVec,
                                               nNextPopVec = nNextPopVec,
                                               nameMethod = nameMethod,
                                               nCores = nCoresPerOptimization,
                                               overWriteRes = overWriteRes,
                                               showProgress = showProgress,
                                               returnMethod = returnMethod,
                                               saveAllResAt = saveAllResAt,
                                               evaluateGVMethod = evaluateGVMethod,
                                               nTopEval = nTopEval,
                                               traitNoEval = traitNoEval,
                                               hEval = hEval,
                                               summaryAllResAt = summaryAllResAt,
                                               verbose = verbose)
        simBsOpt$startSimulation()
        self$simBsRes[[simBsName]] <- simBsOpt$simBsRes[[simBsName]]
        self$trueGVMatList <- simBsOpt$trueGVMatList
        self$estimatedGVMatList <- simBsOpt$estimatedGVMatList
      } else {
        if (setGoalAsFinalGeneration) {
          nGenerationProceedSimulationNow <- min(nGenerationProceed, nGenerationProceedSimulation)
        } else {
          nGenerationProceedSimulationNow <- nGenerationProceedSimulation
        }
        if (verbose) {
          print(paste0("Iteration: ", "1-", nIterSimulation, ", Generation: ", 1,
                       ";  Perform optimization of hyperparameters."))
        }

        # soln <- OOR::StoSOO(par = hStart, fn = private$maximizeFunc,
        #                     nGenerationProceedSimulation = nGenerationProceedSimulationNow,
        #                     lower = hMin, upper = hMax,
        #                     nb_iter = nIterOptimization,
        #                     control = list(type = "sto", verbose = showProgress, max = TRUE))
        #
        # self$solnInit <- soln
        # hVecOpt <- soln$par


        stoSOONow <- myBreedSimulatR::stoSOO$new(parameter = hStart,
                                                 optimizeFunc = private$maximizeFunc,
                                                 nGenerationProceedSimulation = nGenerationProceedSimulation,
                                                 lowerBound = hMin,
                                                 upperBound = hMax,
                                                 nIterOptimization = nIterOptimization,
                                                 nMaxEvalPerNode = nMaxEvalPerNode,
                                                 maxDepth = maxDepth,
                                                 nChildrenPerExpansion = nChildrenPerExpansion,
                                                 confidenceParam = confidenceParam,
                                                 maximize = TRUE,
                                                 optimizeType = "stochastic",
                                                 returnOptimalNodes = returnOptimalNodes,
                                                 saveTreeNameBase = paste0(self$saveTreeNameBase, "_Initial"),
                                                 whenToSaveTrees = self$whenToSaveTrees,
                                                 currentTree = self$currentTreeMid,
                                                 optimalNodes = self$optimalNodesMid,
                                                 withCheck = TRUE,
                                                 verbose = showProgress)
        stoSOONow$startOptimization()
        optimalNodesList <- stoSOONow$optimalNodes
        optimalHyperParamMat <- stoSOONow$optimalHyperParamMat

        hVecOpt <- stoSOONow$optimalParameter
        self$solnInit <- list(value = stoSOONow$optimalValue,
                              par = hVecOpt)

        self$optimalHyperParamMatsList[["Initial"]] <- optimalHyperParamMat
        hVecOptsList[["Initial"]] <- hVecOpt

        rm(stoSOONow)
        gc(reset = TRUE); gc(reset = TRUE)

        if (nCores == 1) {
          # if (showProgress) {
          #   pb <- utils::txtProgressBar(min = 0, max = nIterSimulation, style = 3)
          # }



          simulationCounts <- 0
          for (iterNo in 1:nIterSimulation) {
            # if (showProgress) {
            #   utils::setTxtProgressBar(pb, iterNo)
            # }

            iterName <- iterNames[iterNo]
            hVecOpt <- hVecOptsList[["Initial"]]
            optimalHyperParamMat <- self$optimalHyperParamMatsList[["Initial"]]
            if (is.null(hVecOptsList[[iterName]])) {
              hVecOptsList[[iterName]] <- list()
            }

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
                if (any(sapply(returnMethod, function(x) is.null(self$simBsRes[[simBsName]][[x]][iterName])))) {
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
              hCount <- 0

              # trueGVMatList
              if (is.null(self$trueGVMatList[[iterName]])) {
                self$trueGVMatList[[iterName]][[populationNameInit]] <- self$trueGVMatInit
              }

              # estimatedGVMatList
              if (is.null(self$estimatedGVMatList[[iterName]])) {
                self$estimatedGVMatList[[iterName]][[populationNameInit]] <- self$estimatedGVMatInit
              }

              for (genProceedNo in 1:nGenerationProceed) {
                if (setGoalAsFinalGeneration) {
                  nGenerationProceedSimulationNow <- min(nGenerationProceed - genProceedNo + 1,
                                                         nGenerationProceedSimulation)
                } else {
                  nGenerationProceedSimulationNow <- nGenerationProceedSimulation
                }

                if ((genProceedNo >= 2) & (performOptimization[genProceedNo])) {
                  if (verbose) {
                    print(paste0("Iteration: ", iterNo, ", Generation: ", genProceedNo,
                                 ";  Perform optimization of hyperparameters."))
                  }

                  # soln <- OOR::StoSOO(par = hStart, fn = private$maximizeFunc,
                  #                     nGenerationProceedSimulation = nGenerationProceedSimulationNow,
                  #                     lower = hMin, upper = hMax,
                  #                     nb_iter = nIterOptimization,
                  #                     control = list(type = "sto", verbose = showProgress, max = TRUE))
                  #
                  # hVecOpt <- soln$par


                  stoSOONow <- myBreedSimulatR::stoSOO$new(parameter = hStart,
                                                           optimizeFunc = private$maximizeFunc,
                                                           nGenerationProceedSimulation = nGenerationProceedSimulationNow,
                                                           lowerBound = hMin,
                                                           upperBound = hMax,
                                                           nIterOptimization = nIterOptimization,
                                                           nMaxEvalPerNode = nMaxEvalPerNode,
                                                           maxDepth = maxDepth,
                                                           nChildrenPerExpansion = nChildrenPerExpansion,
                                                           confidenceParam = confidenceParam,
                                                           maximize = TRUE,
                                                           optimizeType = "stochastic",
                                                           returnOptimalNodes = returnOptimalNodes,
                                                           saveTreeNameBase = paste0(self$saveTreeNameBase, "_",
                                                                                     iterName, "_Generation_", genProceedNo),
                                                           whenToSaveTrees = self$whenToSaveTrees,
                                                           withCheck = TRUE,
                                                           verbose = showProgress)
                  stoSOONow$startOptimization()
                  optimalNodesList <- stoSOONow$optimalNodes
                  optimalHyperParamMat <- stoSOONow$optimalHyperParamMat

                  hVecOpt <- stoSOONow$optimalParameter

                  self$optimalHyperParamMatsList[[iterName]][[paste0("Generation_", genProceedNo)]] <- optimalHyperParamMat
                  hVecOptsList[[iterName]][[paste0("Generation_", genProceedNo)]] <- hVecOpt

                  hCount <- 1

                  rm(stoSOONow)
                  gc(reset = TRUE); gc(reset = TRUE)
                } else {
                  self$optimalHyperParamMatsList[[iterName]][[paste0("Generation_", genProceedNo)]] <- optimalHyperParamMat
                  hVecOptsList[[iterName]][[paste0("Generation_", genProceedNo)]] <- hVecOpt
                  hCount <- hCount + 1
                }


                if (sameAcrossGeneration) {
                  hListOpt <- sapply(X = hLens,
                                     FUN = function(hLen) {
                                       hVecOpt[1:hLen]
                                     }, simplify = FALSE)
                } else {
                  hListOpt <- split(x = hVecOpt, f = rep(1:nGenerationProceedSimulation, hLens))
                }


                crossInfoNow <- myBreedSimulatR::crossInfo$new(parentPopulation = bsInfo$populations[[length(bsInfo$populations)]],
                                                               nSelectionWays = nSelectionWaysVec[hCount],
                                                               selectionMethod = selectionMethodList[[hCount]],
                                                               traitNoSel = traitNoSelList[[hCount]],
                                                               userSI = NULL,
                                                               lociEffects = lociEffects,
                                                               blockSplitMethod = blockSplitMethod,
                                                               nMrkInBlock = nMrkInBlock,
                                                               minimumSegmentLength = minimumSegmentLength,
                                                               nSelInitOPV = nSelInitOPVList[[hCount]],
                                                               nIterOPV = nIterOPV,
                                                               nProgeniesEMBV = nProgeniesEMBVVec[hCount],
                                                               nIterEMBV = nIterEMBV,
                                                               nCoresEMBV = nCoresEMBV,
                                                               clusteringForSel = clusteringForSelList[[hCount]],
                                                               nCluster = nClusterList[[hCount]],
                                                               nTopCluster = nTopClusterList[[hCount]],
                                                               nTopEach = nTopEachList[[hCount]],
                                                               nSel = nSelList[[hCount]],
                                                               multiTraitsEvalMethod = multiTraitsEvalMethodList[[hCount]],
                                                               hSel = hSelList[[hCount]],
                                                               matingMethod = matingMethodVec[hCount],
                                                               allocateMethod = allocateMethodVec[hCount],
                                                               weightedAllocationMethod = weightedAllocationMethodList[[hCount]],
                                                               nProgenies = NULL,
                                                               traitNoRA = traitNoRAList[[hCount]],
                                                               h = hListOpt[[hCount]],
                                                               minimumUnitAllocate = minimumUnitAllocateVec[hCount],
                                                               includeGVP = includeGVPVec[hCount],
                                                               nNextPop = nNextPopVec[hCount],
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

                  estimatedGVMat <- genoMatWithIntNow[, rownames(lociEffects)] %*% lociEffects
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

                  estimatedGVMat <- genoMatWithIntNow[, rownames(lociEffects)] %*% lociEffects
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

                if ("varmin" %in% returnMethod) {
                  self$simBsRes[[simBsName]]$var[iterName] <- var(x = trueEvals)
                }
              }


              if (simulationCounts %% nRefreshMemoryEvery == 0) {
                rm(trueGVMat); rm(estimatedGVMat); rm(bsInfo); rm(breederInfo)
                if (!all(returnMethod == "all")) {
                  rm(trueGVMatNow); rm(trueGVMatScaled)
                }
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
                                           if ("all" %in% returnMethod) {
                                             conductSimulation <- TRUE
                                           } else {
                                             if (any(sapply(returnMethod, function(x) is.null(self$simBsRes[[simBsName]][[x]][iterName])))) {
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
                                                 FUN = private$performOneSimulationOptTryError,
                                                 mc.cores = nCores)
            } else {
              simResAll <- parallel::mclapply(X = (1:nIterSimulation)[conductSimulations],
                                              FUN = private$performOneSimulationOptTryError,
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
      }

      self$hVecOptsList <- hVecOptsList
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
        trueGVSummaryMeanDfTarget$Value <- round(trueGVSummaryMeanDfTarget$Value, 3)
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

    # @description marker and QTL effects used for crossInfo object
    # @param hVec [numeric] hyperparameter to be optimized
    maximizeFunc = function(hVec, nGenerationProceedSimulation) {
      performRobustOptimization <- self$performRobustOptimization

      if (self$sameAcrossGeneration) {
        hList <- sapply(X = self$hLens,
                        FUN = function(hLen) {
                          hVec[1:hLen]
                        }, simplify = FALSE)
      } else {
        hList <- split(x = hVec, f = rep(1:self$nGenerationProceedSimulation, self$hLens))
      }
      rewardWeightVec <- self$rewardWeightVec[(self$nGenerationProceedSimulation - nGenerationProceedSimulation + 1):self$nGenerationProceedSimulation]
      rewardWeightVec <- rewardWeightVec / sum(rewardWeightVec)

      nonZeroWeight <- rewardWeightVec != 0


      rewardOnlyLast <- (!any(nonZeroWeight[1:(nGenerationProceedSimulation - 1)])) & nonZeroWeight[nGenerationProceedSimulation]
      if (rewardOnlyLast) {
        returnMethod <- "max"
      } else {
        returnMethod <- "summary"
      }

      if (performRobustOptimization) {
        samplingMrkEffInit <- TRUE
        nCoresForOneMrkEff <- 1
      } else {
        samplingMrkEffInit <- FALSE
        nCoresForOneMrkEff <- self$nCoresPerOptimization
      }


      maximizeFuncForOneMrkEffect <- function(iterNoForMrkEffects) {
        simBsNow <- myBreedSimulatR::simBs$new(simBsName = self$simBsName,
                                               bsInfoInit = self$bsInfoInit,
                                               breederInfoInit = self$breederInfoInit,
                                               lociEffMethod = self$lociEffMethod,
                                               trainingPopType = self$trainingPopType,
                                               trainingPopInit = self$trainingPopInit,
                                               trainingIndNamesInit = self$trainingIndNamesInit,
                                               methodMLRInit = self$methodMLRInit,
                                               multiTraitInit = self$multiTraitInit,
                                               samplingMrkEffInit = samplingMrkEffInit,
                                               seedMrkEffSamplingInit = NA,
                                               nIterSimulation = self$nIterSimulationForOneMrkEffect,
                                               nGenerationProceed = nGenerationProceedSimulation,
                                               nRefreshMemoryEvery = self$nRefreshMemoryEvery,
                                               updateBreederInfo = self$updateBreederInfoSimulation[1:nGenerationProceedSimulation],
                                               phenotypingInds = self$phenotypingIndsSimulation[1:nGenerationProceedSimulation],
                                               nRepForPhenoInit = self$nRepForPhenoInit,
                                               nRepForPheno = self$nRepForPhenoSimulation[1:nGenerationProceedSimulation],
                                               updateModels = self$updateModelsSimulation[1:nGenerationProceedSimulation],
                                               methodMLR = self$methodMLR,
                                               multiTrait = self$multiTrait,
                                               nSelectionWaysVec = self$nSelectionWaysVec[1:nGenerationProceedSimulation],
                                               selectionMethodList = self$selectionMethodList[1:nGenerationProceedSimulation],
                                               traitNoSelList = self$traitNoSelList[1:nGenerationProceedSimulation],
                                               blockSplitMethod = self$blockSplitMethod,
                                               nMrkInBlock = self$nMrkInBlock,
                                               minimumSegmentLength = self$minimumSegmentLength,
                                               nSelInitOPVList = self$nSelInitOPVList[1:nGenerationProceedSimulation],
                                               nIterOPV = self$nIterOPV,
                                               nProgeniesEMBVVec = self$nProgeniesEMBVVec[1:nGenerationProceedSimulation],
                                               nIterEMBV = self$nIterEMBV,
                                               nCoresEMBV = self$nCoresEMBV,
                                               clusteringForSelList = self$clusteringForSelList[1:nGenerationProceedSimulation],
                                               nClusterList = self$nClusterList[1:nGenerationProceedSimulation],
                                               nTopClusterList = self$nTopClusterList[1:nGenerationProceedSimulation],
                                               nTopEachList = self$nTopEachList[1:nGenerationProceedSimulation],
                                               nSelList = self$nSelList[1:nGenerationProceedSimulation],
                                               multiTraitsEvalMethodList = self$multiTraitsEvalMethodList[1:nGenerationProceedSimulation],
                                               hSelList = self$hSelList[1:nGenerationProceedSimulation],
                                               matingMethodVec = self$matingMethodVec[1:nGenerationProceedSimulation],
                                               allocateMethodVec = self$allocateMethodVec[1:nGenerationProceedSimulation],
                                               weightedAllocationMethodList = self$weightedAllocationMethodList[1:nGenerationProceedSimulation],
                                               includeGVPVec = self$includeGVPVec[1:nGenerationProceedSimulation],
                                               traitNoRAList = self$traitNoRAList[1:nGenerationProceedSimulation],
                                               hList = hList[1:nGenerationProceedSimulation],
                                               minimumUnitAllocateVec = self$minimumUnitAllocateVec[1:nGenerationProceedSimulation],
                                               nNextPopVec = self$nNextPopVec[1:nGenerationProceedSimulation],
                                               nameMethod = self$nameMethod,
                                               nCores = nCoresForOneMrkEff,
                                               overWriteRes = self$overWriteRes,
                                               showProgress = FALSE,
                                               returnMethod = returnMethod,
                                               saveAllResAt = NULL,
                                               evaluateGVMethod = "estimated",
                                               nTopEval = self$nTopEvalForOpt,
                                               traitNoEval = self$traitNoEval,
                                               hEval = self$hEval,
                                               summaryAllResAt = NULL,
                                               verbose = FALSE)
        simBsNow$startSimulation()

        if (rewardOnlyLast) {
          maxEvalForOneMrkEffect <- mean(simBsNow$simBsRes[[simBsNow$simBsName]]$max, na.rm = TRUE)
        } else {
          rewardEachIter <- lapply(X = simBsNow$estimatedGVMatList,
                                   FUN = function(estimatedGVMatEachIter) {
                                     estimatedGVMatMax <- try(do.call(what = rbind,
                                                                      args = lapply(X = estimatedGVMatEachIter,
                                                                                    FUN = function(estimatedGVMatEachPop) {
                                                                                      apply(estimatedGVMatEachPop, 2, max)
                                                                                    })
                                     ), silent = TRUE)


                                     if ("try-error" %in% class(estimatedGVMatMax)) {
                                       reward <- NA
                                     } else {
                                       estimatedGVMaxEachPop <- try(estimatedGVMatMax[, self$traitNoEval, drop = FALSE] %*% as.matrix(self$hEval),
                                                                    silent = TRUE)
                                       if ("try-error" %in% class(estimatedGVMaxEachPop)) {
                                         reward <- NA
                                       } else {
                                         reward <- try(sum(estimatedGVMaxEachPop[-1, 1] * rewardWeightVec),
                                                       silent = TRUE)

                                         if ("try-error" %in% class(reward)) {
                                           reward <- NA
                                         }
                                       }
                                     }

                                     return(reward)
                                   })


          maxEvalForOneMrkEffect <- mean(unlist(rewardEachIter), na.rm = TRUE)
        }

        return(maxEvalForOneMrkEffect)
      }

      if (performRobustOptimization) {
        maximizeFuncForOneMrkEffectTryError <- function(iterNoForMrkEffects) {
          performMaximizeFuncForOneMrkEffect <- TRUE

          while (performMaximizeFuncForOneMrkEffect) {
            maxEvalForOneMrkEffect <- try(maximizeFuncForOneMrkEffect(iterNoForMrkEffects = iterNoForMrkEffects),
                                          silent = TRUE)
            performMaximizeFuncForOneMrkEffect <- "try-error" %in% class(maxEvalForOneMrkEffect)
          }

          return(maxEvalForOneMrkEffect)
        }

        if (self$nCoresPerOptimization == 1) {
          if (self$showProgress) {
            maxEvalsForMrkEffects <- unlist(
              pbapply::pblapply(X = 1:self$nIterMrkEffectForRobustOptimization,
                                FUN = maximizeFuncForOneMrkEffectTryError)
            )
          } else {
            maxEvalsForMrkEffects <- unlist(
              lapply(X = 1:self$nIterMrkEffectForRobustOptimization,
                     FUN = maximizeFuncForOneMrkEffectTryError)
            )
          }
        } else {
          if (self$showProgress) {
            maxEvalsForMrkEffects <- unlist(
              pbmcapply::pbmclapply(X = 1:self$nIterMrkEffectForRobustOptimization,
                                    FUN = maximizeFuncForOneMrkEffectTryError,
                                    mc.cores = self$nCoresPerOptimization)
            )
          } else {
            maxEvalsForMrkEffects <- unlist(
              parallel::mclapply(X = 1:self$nIterMrkEffectForRobustOptimization,
                                 FUN = maximizeFuncForOneMrkEffectTryError,
                                 mc.cores = self$nCoresPerOptimization)
            )
          }
        }


        maxEval <- quantile(x = maxEvalsForMrkEffects, probs = self$lowerQuantile)
      } else {
        maxEval <- maximizeFuncForOneMrkEffect()
      }


      if (!is.na(maxEval)) {
        maxEval <- round(maxEval,
                         digits = self$digitsEval)
      } else {
        maxEval <- -Inf
        message("Computation failed: maybe internal error occured !")
      }

      return(maxEval)
    },




    # @description Proceed optimized breeding scheme (one simulation)
    #
    # @param iterNo [numeric] Iteration No.
    performOneSimulationOpt = function(iterNo) {
      # Read arguments from `self`
      simBsName <- self$simBsName
      bsInfoInit <- self$bsInfoInit
      breederInfoInit <- self$breederInfoInit
      lociEffMethod <- self$lociEffMethod
      methodMLRInit <- self$methodMLRInit
      multiTraitInit <- self$multiTraitInit
      nIterSimulation <- self$nIterSimulation
      nGenerationProceed <- self$nGenerationProceed
      nGenerationProceedSimulation <- self$nGenerationProceedSimulation
      setGoalAsFinalGeneration <- self$setGoalAsFinalGeneration
      performOptimization <- self$performOptimization
      useFirstOptimizedValue <- self$useFirstOptimizedValue
      sameAcrossGeneration <- self$sameAcrossGeneration
      hMin <- self$hMin
      hMax <- self$hMax
      nTotalIterForOneOptimization <- self$nTotalIterForOneOptimization
      nIterSimulationPerEvaluation <- self$nIterSimulationPerEvaluation
      nIterOptimization <- self$nIterOptimization
      nMaxEvalPerNode <- self$nMaxEvalPerNode
      maxDepth <- self$maxDepth
      nChildrenPerExpansion <- self$nChildrenPerExpansion
      confidenceParam <- self$confidenceParam
      returnOptimalNodes <- self$returnOptimalNodes
      nTopEvalForOpt <- self$nTopEvalForOpt
      digitsEval <- self$digitsEval
      nRefreshMemoryEvery <- self$nRefreshMemoryEvery
      hLens <- self$hLens
      hStart <- self$hStart
      updateBreederInfo <- self$updateBreederInfo
      phenotypingInds <- self$phenotypingInds
      nRepForPhenoInit <- self$nRepForPhenoInit
      nRepForPheno <- self$nRepForPheno
      updateModels <- self$updateModels
      updateBreederInfoSimulation <- self$updateBreederInfoSimulation
      phenotypingIndsSimulation <- self$phenotypingIndsSimulation
      nRepForPhenoSimulation <- self$nRepForPhenoSimulation
      updateModelsSimulation <- self$updateModelsSimulation
      methodMLR <- self$methodMLR
      multiTrait <- self$multiTrait
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
      summaryAllResAt <- self$summaryAllResAt
      verbose <- self$verbose


      populationNameInit <- names(bsInfoInit$populations[length(bsInfoInit$populations)])

      iterNames <- paste0("Iteration_", 1:nIterSimulation)
      hVecOptsList <- self$hVecOptsList

      lociEffectsInit <- self$lociEffectsInit

      soln <- self$solnInit
      hVecOpt <- soln$par
      hVecOptsList[["Initial"]] <- hVecOpt
      optimalHyperParamMat <- self$optimalHyperParamMatsList[["Initial"]]


      iterName <- iterNames[iterNo]
      simRes <- list()
      simRes$trueGVMatList <- list()
      simRes$estimatedGVMatList <- list()

      if (is.null(hVecOptsList[[iterName]])) {
        hVecOptsList[[iterName]] <- list()
      }

      bsInfo <- bsInfoInit$clone(deep = FALSE)
      breederInfo <- breederInfoInit$clone(deep = FALSE)
      lociEffects <- lociEffectsInit
      hCount <- 0

      # trueGVMatList
      if (is.null(self$trueGVMatList[[iterName]])) {
        simRes$trueGVMatList[[populationNameInit]] <- self$trueGVMatInit
      }

      # estimatedGVMatList
      if (is.null(self$estimatedGVMatList[[iterName]])) {
        simRes$estimatedGVMatList[[populationNameInit]] <- self$estimatedGVMatInit
      }


      for (genProceedNo in 1:nGenerationProceed) {
        if (setGoalAsFinalGeneration) {
          nGenerationProceedSimulationNow <- min(nGenerationProceed - genProceedNo + 1,
                                                 nGenerationProceedSimulation)
        } else {
          nGenerationProceedSimulationNow <- nGenerationProceedSimulation
        }

        if ((genProceedNo >= 2) & (performOptimization[genProceedNo])) {
          # soln <- OOR::StoSOO(par = hStart, fn = private$maximizeFunc,
          #                     nGenerationProceedSimulation = nGenerationProceedSimulationNow,
          #                     lower = hMin, upper = hMax,
          #                     nb_iter = nIterOptimization,
          #                     control = list(type = "sto", verbose = 0, max = TRUE))
          #
          # hVecOpt <- soln$par

          stoSOONow <- myBreedSimulatR::stoSOO$new(parameter = hStart,
                                                   optimizeFunc = private$maximizeFunc,
                                                   nGenerationProceedSimulation = nGenerationProceedSimulationNow,
                                                   lowerBound = hMin,
                                                   upperBound = hMax,
                                                   nIterOptimization = nIterOptimization,
                                                   nMaxEvalPerNode = nMaxEvalPerNode,
                                                   maxDepth = maxDepth,
                                                   nChildrenPerExpansion = nChildrenPerExpansion,
                                                   confidenceParam = confidenceParam,
                                                   maximize = TRUE,
                                                   optimizeType = "stochastic",
                                                   returnOptimalNodes = returnOptimalNodes,
                                                   saveTreeNameBase = paste0(self$saveTreeNameBase, "_",
                                                                             iterName, "_Generation_", genProceedNo),
                                                   whenToSaveTrees = self$whenToSaveTrees,
                                                   withCheck = TRUE,
                                                   verbose = showProgress)
          stoSOONow$startOptimization()
          optimalNodesList <- stoSOONow$optimalNodes
          optimalHyperParamMat <- stoSOONow$optimalHyperParamMat

          hVecOpt <- stoSOONow$optimalParameter

          self$optimalHyperParamMatsList[[iterName]][[paste0("Generation_", genProceedNo)]] <- optimalHyperParamMat
          hVecOptsList[[iterName]][[paste0("Generation_", genProceedNo)]] <- hVecOpt
          hCount <- 1

          if (iterNo %% nRefreshMemoryEvery == 0) {
            rm(stoSOONow)
            gc(reset = TRUE); gc(reset = TRUE)
          }

        } else {
          self$optimalHyperParamMatsList[[iterName]][[paste0("Generation_", genProceedNo)]] <- optimalHyperParamMat
          hVecOptsList[[iterName]][[paste0("Generation_", genProceedNo)]] <- hVecOpt
          hCount <- hCount + 1
        }
        self$hVecOptsList <- hVecOptsList


        if (sameAcrossGeneration) {
          hListOpt <- sapply(X = hLens,
                             FUN = function(hLen) {
                               hVecOpt[1:hLen]
                             }, simplify = FALSE)
        } else {
          hListOpt <- split(x = hVecOpt, f = rep(1:nGenerationProceedSimulation, hLens))
        }



        crossInfoNow <- myBreedSimulatR::crossInfo$new(parentPopulation = bsInfo$populations[[length(bsInfo$populations)]],
                                                       nSelectionWays = nSelectionWaysVec[hCount],
                                                       selectionMethod = selectionMethodList[[hCount]],
                                                       traitNoSel = traitNoSelList[[hCount]],
                                                       userSI = NULL,
                                                       lociEffects = lociEffects,
                                                       blockSplitMethod = blockSplitMethod,
                                                       nMrkInBlock = nMrkInBlock,
                                                       minimumSegmentLength = minimumSegmentLength,
                                                       nSelInitOPV = nSelInitOPVList[[hCount]],
                                                       nIterOPV = nIterOPV,
                                                       nProgeniesEMBV = nProgeniesEMBVVec[hCount],
                                                       nIterEMBV = nIterEMBV,
                                                       nCoresEMBV = nCoresEMBV,
                                                       clusteringForSel = clusteringForSelList[[hCount]],
                                                       nCluster = nClusterList[[hCount]],
                                                       nTopCluster = nTopClusterList[[hCount]],
                                                       nTopEach = nTopEachList[[hCount]],
                                                       nSel = nSelList[[hCount]],
                                                       multiTraitsEvalMethod = multiTraitsEvalMethodList[[hCount]],
                                                       hSel = hSelList[[hCount]],
                                                       matingMethod = matingMethodVec[hCount],
                                                       allocateMethod = allocateMethodVec[hCount],
                                                       weightedAllocationMethod = weightedAllocationMethodList[[hCount]],
                                                       nProgenies = NULL,
                                                       traitNoRA = traitNoRAList[[hCount]],
                                                       h = hListOpt[[hCount]],
                                                       minimumUnitAllocate = minimumUnitAllocateVec[hCount],
                                                       includeGVP = includeGVPVec[hCount],
                                                       nNextPop = nNextPopVec[hCount],
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

          estimatedGVMat <- genoMatWithIntNow[, rownames(lociEffects)] %*% lociEffects
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

          estimatedGVMat <- genoMatWithIntNow[, rownames(lociEffects)] %*% lociEffects
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
        rm(trueGVMat); rm(estimatedGVMat); rm(bsInfo); rm(breederInfo)
        if (!all(returnMethod == "all")) {
          rm(trueGVMatNow); rm(trueGVMatScaled)
        }

        gc(reset = TRUE); gc(reset = TRUE)
      }

      return(simRes)
    },


    # @description Proceed optimized breeding scheme with try-error (one simulation)
    #
    # @param iterNo [numeric] Iteration No.
    performOneSimulationOptTryError = function(iterNo) {
      simRes <- try(private$performOneSimulationOpt(iterNo = iterNo),
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
      #   if (breederInfoEach$generation < bsInfoEach$generation) {
      #     for (generationAdd in (breederInfoEach$generation + 1):bsInfoEach$generation) {
      #       breederInfoEach$getNewPopulation(bsInfo = bsInfoEach,
      #                                        generationNew = generationAdd,
      #                                        genotyping = estimated,
      #                                        genotypedIndNames = NULL)
      #     }
      #   }
      #
      #   for (generationNow in 1:length(breederInfoEach$populationsFB)) {
      #     eachPop <- breederInfoEach$populationsFB[[generationNow]]
      #     if (is.null(breederInfoEach$estimatedGVByMLRInfo[[eachPop$name]])) {
      #       breederInfoEach$estimateGVByMLR(trainingPop = self$trainingPopInit,
      #                                       trainingIndNames = self$trainingIndNamesInit,
      #                                       testingPop = generationNow,
      #                                       methodMLR = self$methodMLRInit,
      #                                       multiTrait = self$multiTraitInit,
      #                                       alpha = 0.5,
      #                                       nIter = 12000,
      #                                       burnIn = 3000,
      #                                       thin = 5,
      #                                       bayesian = TRUE)
      #     }
      #   }
      #   estimatedGVMatEachList <- lapply(X = breederInfoEach$populationsFB,
      #                                    FUN = function(eachPop) {
      #                                      estimatedGVMatEachPop <- breederInfoEach$estimatedGVByMLRInfo[[eachPop$name]]$testingEstimatedGVByMLR
      #
      #                                      return(estimatedGVMatEachPop)
      #                                    })
      # } else {
      #   estimatedGVMatEachList <- NULL
      # }

      estimatedGVMatEachList <- lapply(X = bsInfoEach$populations,
                                       FUN = function(eachPop) {
                                         genoMatNow <- eachPop$genoMat
                                         genoMatWithIntNow <- cbind(Intercept = rep(1, nrow(genoMatNow)),
                                                                    genoMatNow)
                                         estimatedGVMatEachPop <- genoMatWithIntNow[, rownames(lociEffects)] %*% lociEffects

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
