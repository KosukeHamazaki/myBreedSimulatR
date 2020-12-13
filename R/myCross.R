# Author: Kosuke Hamazaki hamazaki@ut-biomet.org
# 2020 The University of Tokyo
#
# Description:
# Definition of individual crossing functions


#' R6 class representing a crossing information
#'
#' @description
#' crossInfo object store specific crossing information
#'
#'
#' @export
#' @import R6
crossInfo <- R6::R6Class(
  "crossInfo",
  lock_objects = FALSE,
  public = list(
    #' @field parentPopulation [population class] Parent population that will generatea new population of the next generation
    parentPopulation = NULL,
    #' @field nSelectionWays [numeric] Number of selection ways
    nSelectionWays = NULL,
    #' @field selectionMethod [character] Selection method
    selectionMethod = NULL,
    #' @field traitNoSel [numeric / list] (list of) number of trait No of your interest for selection
    traitNoSel = NULL,
    #' @field userSI [matrix] Selection index defined by user. If you set `userSI` and `selectionMethod = 'userSI'`,
    #' this argument will be used for selection. The number of rows equals to the number of individuals.
    #' If you have multiple traits, it will be a matrix of multiple columns.
    userSI = NULL,
    #' @field lociEffects [matrix] Effect of the genetic markers (including intercept).
    #' If you have multiple traits, it will be a matrix of multiple columns.
    lociEffects = NULL,
    #' @field blockSplitMethod [character] How to determine the markers belonging to each block when computing OHV.
    #' You can define the number of markers in each block (`nMrkInBlock`) or the minimum length of each segment (`minimumSegmentLength`).
    blockSplitMethod = NULL,
    #' @field nMrkInBlock [numeric] Number of markers in each block. This will be used for the computation of OHV.
    nMrkInBlock = NULL,
    #' @field minimumSegmentLength [numeric] Minimum length of each segment [cM]. This will be used for the computation of OHV.
    minimumSegmentLength = NULL,
    #' @field nIterOPV [numeric] Number of iterations for computation of OPV
    nIterOPV = NULL,
    #' @field nProgeniesEMBV [numeric] Number of progenies of double haploids produced when computing EMBV
    nProgeniesEMBV = NULL,
    #' @field nIterEMBV [numeric] Number of iterations to estimate EMBV
    nIterEMBV = NULL,
    #' @field nCoresEMBV [numeric] Number of cores used for EMBV estimation
    nCoresEMBV = NULL,
    #' @field clusteringForSel [logical] Apply clustering results of marker genotype to selection or not
    clusteringForSel = NULL,
    #' @field nCluster [numeric] Number of clusters
    nCluster = NULL,
    #' @field nTopCluster [numeric] Number of top clusters used for selection
    nTopCluster = NULL,
    #' @field nTopEach [numeric] Number of selected individuals in each cluster
    nTopEach = NULL,
    #' @field nSel [numeric] Number of selection candidates
    nSel = NULL,
    #' @field matingMethod [character] Mating method
    matingMethod = NULL,
    #' @field allocateMethod [character] Allocation method
    allocateMethod = NULL,
    #' @field weightedAllocationMethod [character] Which selection index will be used for weighted resource allocation
    weightedAllocationMethod = NULL,
    #' @field nProgenies [numeric] Number of progenies for each pair
    nProgenies = NULL,
    #' @field traitNoRA [numeric] trait No of your interest for resource allocation
    traitNoRA = NULL,
    #' @field h [numeric] Hyperprameter which determines how parent pair with high BV is emphasized when producing progenies
    h = NULL,
    #' @field includeGVP [logical] Whether or not to consider genetic variance of progenies of each pair when determining the number of progenies per each pair
    includeGVP = NULL,
    #' @field nNextPop [numeric] Number of progenies for the next generation
    nNextPop = NULL,
    #' @field nPairs [numeric] Number of parent pairs for the next generation
    nPairs = NULL,
    #' @field nameMethod [character] Method for naming individuals
    nameMethod = NULL,
    #' @field indNames [character] Character string vector specifying the individuals names
    #' of the new population
    indNames = NULL,
    #' @field seedSimRM [numeric] Random seed for mate pairs
    seedSimRM = NULL,
    #' @field seedSimMC [numeric] Random seed for make crosses
    seedSimMC = NULL,
    #' @field selCands [character] Names of selection candidates
    selCands = NULL,
    #' @field crosses [data.frame] data.frame with crossing instructions: parents names
    #' \code{ind1} \code{ind2}, number of descendant \code{n} and names of
    #' descendant \code{names}
    crosses = NULL,
    #' @field genDist [dist] `dist` object of genetic distance computed from marker genotype by Euclidean distance
    genDist = NULL,
    #' @field groups [list] List of named vectors of group IDs of each individual categorized by hierarchical clustering
    groups = list(),
    #' @field BV [matrix] matrix of the breeding values
    BV = NULL,
    #' @field WBV [matrix] matrix of the weighted breeding values
    WBV = NULL,
    #' @field OHV [matrix] matrix of the optimal haploid values
    OHV = NULL,
    #' @field EMBV [matrix] matrix of the expected maximum haploid breeding values
    EMBV = NULL,
    #' @field haploEffMaxArray [array] haploid value of each segment (haplotype block) for each individual
    haploEffMaxArray = NULL,

    #' @description Create a new population object.
    #' @param parentPopulation [population class] Parent population that will generate a new population of the next generation
    #' @param nSelectionWays [numeric] Number of selection ways
    #' @param selectionMethod [character] Selection method
    #' @param traitNoSel [numeric / list] (list of) number of trait No of your interest for selection
    #' @param userSI [matrix] Selection index defined by user. If you set `userSI` and `selectionMethod = 'userSI'`,
    #' this argument will be used for selection. The number of rows equals to the number of individuals.
    #' If you have multiple traits, it will be a matrix of multiple columns.
    #' @param lociEffects [matrix] Effect of the genetic markers (including intercept).
    #' If you have multiple traits, it will be a matrix of multiple columns.
    #' @param blockSplitMethod [character] How to determine the markers belonging to each block when computing OHV.
    #' You can define the number of markers in each block (`nMrkInBlock`) or the minimum length of each segment (`minimumSegmentLength`).
    #' @param nMrkInBlock [numeric] Number of markers in each block. This will be used for the computation of OHV.
    #' @param minimumSegmentLength [numeric] Minimum length of each segment [cM]. This will be used for the computation of OHV.
    #' @param nIterOPV [numeric] Number of iterations for computation of OPV
    #' @param nProgeniesEMBV [numeric] Number of progenies of double haploids produced when computing EMBV
    #' @param nIterEMBV [numeric] Number of iterations to estimate EMBV
    #' @param nCoresEMBV [numeric] Number of cores used for EMBV estimation
    #' @param clusteringForSel [logical] Apply clustering results of marker genotype to selection or not
    #' @param nCluster [numeric] Number of clusters
    #' @param nTopCluster [numeric] Number of top clusters used for selection
    #' @param nTopEach [numeric] Number of selected individuals in each cluster
    #' @param nSel [numeric] Number of selection candidates
    #' @param matingMethod [character] Mating method
    #' @param allocateMethod [character] Allocation method
    #' @param weightedAllocationMethod [character] Which selection index will be used for weighted resource allocation
    #' @param nProgenies [numeric] Number of progenies for each pair
    #' @param traitNoRA [numeric] Trait No of your interest for resource allocation
    #' @param h [numeric] Hyperparameter which determines how parent pair with high BV is emphasized when producing progenies
    #' @param includeGVP [logical] Whether or not to consider genetic variance of progenies of each pair when determining the number of progenies per each pair
    #' @param nNextPop [numeric] Number of progenies for the next generation
    #' @param nPairs [numeric] Number of parent pairs for the next generation
    #' @param nameMethod [character] Method for naming individuals
    #' @param indNames [character] Character string vector specifying the individuals names
    #' of the new population
    #' @param seedSimRM [numeric] Random seed for mate pairs
    #' @param seedSimMC [numeric] Random seed for make crosses
    #' @param selCands [character] Names of selection candidates
    #' @param crosses [data.frame] data.frame with crossing instructions: parents names
    #' \code{ind1} \code{ind2}, number of descendant \code{n} and names of
    #' descendant \code{names}
    #' @param verbose [boolean] display information
    #' @return A new `population` object.
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
    #'                      chrNames = c("C1", "C2", "C3"),
    #'                      nLoci = 100,
    #'                      recombRateOneVal = FALSE,
    #'                      effPopSize = 100,
    #'                      simInfo = mySimInfo,
    #'                      verbose = TRUE)
    #'
    #' ### create lociInfo object
    #' myLoci <- lociInfo$new(genoMap = NULL, specie = mySpec)
    #'
    #'
    #' ### create simulated population
    #' simulatedPop <- createPop(geno = NULL,
    #'                           haplo = NULL,
    #'                           lociInfo = myLoci,
    #'                           founderIsInitPop = TRUE,
    #'                           popName = "First Population",
    #'                           verbose = FALSE)
    #'
    #'
    #' ### create cross oinformation object
    #' myCrossInfo <- crossInfo$new(parentPopulation = simulatedPop,
    #'                              selectionMethod = "nonSelection",
    #'                              matingMethod = "randomMate",
    #'                              nNextPop = 100)
    #' print(myCrossInfo)
    #' myCrossInfo$designResourceAllocation()
    #' newIndsList <- myCrossInfo$makeCrosses
    initialize = function(parentPopulation,
                          nSelectionWays = NA,
                          selectionMethod = "nonSelection",
                          traitNoSel = NULL,
                          userSI = NULL,
                          lociEffects = NULL,
                          blockSplitMethod = NULL,
                          nMrkInBlock = NULL,
                          minimumSegmentLength = NULL,
                          nIterOPV = NULL,
                          nProgeniesEMBV = NULL,
                          nIterEMBV = NULL,
                          nCoresEMBV = NULL,
                          clusteringForSel = FALSE,
                          nCluster = NULL,
                          nTopCluster = NULL,
                          nTopEach = NULL,
                          nSel = NULL,
                          matingMethod = "randomMate",
                          allocateMethod = "equalAllocation",
                          weightedAllocationMethod = NULL,
                          nProgenies = NULL,
                          traitNoRA = NULL,
                          h = NULL,
                          includeGVP = FALSE,
                          nNextPop = NULL,
                          nPairs = NULL,
                          nameMethod = "pairBase",
                          indNames = NULL,
                          seedSimRM = NA,
                          seedSimMC = NA,
                          selCands = NULL,
                          crosses = NULL,
                          verbose = TRUE){

      selectionMethodsOffered <- c("nonSelection", "selectBV", "selectWBV", "selectOHV",
                                   "selectEMBV", "selectOPV", "userSI", "userSpecific")
      selectionMethodsWithSelection <- c("selectBV", "selectWBV", "selectOHV",
                                         "selectEMBV", "selectOPV", "userSI")
      selectionMethodsWithMrkEff <- c("selectBV", "selectWBV", "selectOHV", "selectEMBV", "selectOPV")
      matingMethodsOffered <- c("randomMate", "roundRobin", "diallel", "diallelWithSelfing",
                                "selfing", "maxGenDist", "makeDH", "userSpecific")
      allocateMethodsOffered <- c("equalAllocation", "weightedAllocation", "userSpecific")
      nameMethodsOffered <- c("pairBase", "individualBase")
      blockSplitMethodsOffered <- c("nMrkInBlock", "minimumSegmentLength")

      nIndNow <- parentPopulation$nInd
      nTraits <- parentPopulation$traitInfo$nTraits
      traitNames <- parentPopulation$traitInfo$traitNames

      # checks
      if (class(parentPopulation)[1] != "population") {
        stop(paste('class(parentPopulation)[1] != "population"\n"parentPopulation" must be a',
                   'population object see: ?population'))
      }


      if (!is.na(nSelectionWays)) {
        stopifnot(is.numeric(nSelectionWays))
        nSelectionWays <- floor(nSelectionWays)
        stopifnot(nSelectionWays >= 1)
      } else {
        nSelectionWays <- 1
        message(paste0("`nSelectionWays` is not specified. We substitute `nSelectionWays = ",
                       nSelectionWays,"` instead."))
      }


      if (!is.null(selectionMethod)) {
        if (!all(selectionMethod %in% selectionMethodsOffered)) {
          stop(paste0("We only offer the following selection methods: ",
                      paste(selectionMethodsOffered, collapse = "; ")))
        }
      } else {
        selectionMethod <- "userSpecific"
        message("You do not specify the selection method. User specific selection will be performed.")
      }

      if (!(length(selectionMethod) %in% c(1, nSelectionWays))) {
        stop(paste("length(selectionMethod) must be equal to 1 or equal to nSelectionWays."))
      } else if (length(selectionMethod) == 1) {
        selectionMethod <- rep(selectionMethod, nSelectionWays)
      }

      if (any(selectionMethodsWithMrkEff %in% selectionMethod)) {
        if (is.null(lociEffects)) {
          stop("You must specify `lociEffects` argument if you use selection based on SNP effects!!")
        }
      }

      if ("userSI" %in% selectionMethod) {
        if (is.null(userSI)) {
          stop("You must specify `userSI` argument if you use user-defined selection indices!!")
        }
      }

      if ("userSpecific" %in% selectionMethod) {
        if (is.null(selCands)) {
          stop("You must specify `selCands` argument if you do not specify the selection methods!!")
        }
      }
      whereSelection <- selectionMethod %in% selectionMethodsWithSelection

      if (!is.null(matingMethod)) {
        if (!(matingMethod %in% matingMethodsOffered)) {
          stop(paste0("We only offer the following mating methods: ",
                      paste(matingMethodsOffered, collapse = "; ")))
        }
      } else {
        matingMethod <- "userSpecific"
        message("You do not specify the mating method. User specific mating will be performed.")
      }
      if (matingMethod == "userSpecific") {
        if (is.null(crosses)) {
          stop("You must specify `crosses` argument if you do not specify the mating methods!!")
        }
      }


      if (!is.null(allocateMethod)) {
        if (!(allocateMethod %in% allocateMethodsOffered)) {
          stop(paste0("We only offer the following allocation methods: ",
                      paste(allocateMethodsOffered, collapse = "; ")))
        }
      } else {
        allocateMethod <- "userSpecific"
        message("You do not specify the allocation method. User specific allocation will be performed.")
      }
      if (allocateMethod == "userSpecific") {
        if (is.null(nProgenies) & is.null(crosses)) {
          stop("You must specify `nProgenies` or `crosses` argument if you do not specify the allocation methods!!")
        }
      }

      if (!is.null(weightedAllocationMethod)) {
        if (!(all(weightedAllocationMethod %in% selectionMethodsWithSelection))) {
          weightedAllocationMethod <- weightedAllocationMethod[weightedAllocationMethod %in% selectionMethodsWithSelection]
          message(paste0("We only offer the following weighted allocation methods: ",
                         paste(selectionMethodsWithSelection, collapse = "; ")))
          stopifnot(length(weightedAllocationMethod) >= 1)
        }
      } else {
        if (allocateMethod == "weightedAllocation") {
          weightedAllocationMethod <- "userSI"
          message("You do not specify the weighted allocation method. User selection indices will be used for weighted resource allocation")
        }
      }
      if (!is.null(weightedAllocationMethod)) {
        if ("userSI" %in% weightedAllocationMethod) {
          if (is.null(userSI)) {
            stop("You must specify `userSI` argument if you use user-defined selection indices!!")
          }
        } else if (any(weightedAllocationMethod %in% selectionMethodsWithMrkEff)) {
          if (is.null(lociEffects)) {
            stop("You must specify `lociEffects` argument if you use marker-based selection indices!!")
          }
        }
      }


      if (matingMethod == "makeDH") {
        if ("selectOPV" %in% weightedAllocationMethod) {
          weightedAllocationMethod <- weightedAllocationMethod[weightedAllocationMethod != "selectOPV"]
          message("If you use `makeDH` option for mating method, you cannot include OPV for weighted resource allocation method.")
        }
      }


      if (!is.null(nameMethod)) {
        if (!(nameMethod %in% nameMethodsOffered)) {
          stop(paste0("We only offer the following methods for naming individuals: ",
                      paste(nameMethodsOffered, collapse = "; ")))
        }
      } else {
        nameMethod <- "userSpecific"
        message("You do not specify the method for naming individuals. User specific naming will be performed.")
      }
      if (nameMethod == "userSpecific") {
        if (is.null(indNames)) {
          stop("You must specify `indNames` argument if you do not specify the method for naming individuals!!")
        }
      }



      if (!is.null(traitNoSel)) {
        if (is.numeric(traitNoSel)) {
          stopifnot(is.numeric(traitNoSel))
          traitNoSel <- floor(traitNoSel)
          stopifnot(all(traitNoSel >= 1))
          stopifnot(all(traitNoSel <= nTraits))
          traitNoSel <- rep(list(traitNoSel), nSelectionWays)
        } else if (is.list(traitNoSel)) {
          if (!(length(traitNoSel) %in% c(1, nSelectionWays))) {
            stop(paste("length(traitNoSel) must be equal to 1 or equal to nSelectionWays."))
          } else if (length(traitNoSel) == 1) {
            traitNoSel <- rep(traitNoSel, nSelectionWays)
          }
          stopifnot(all(unlist(lapply(traitNoSel[whereSelection], is.numeric))))
          traitNoSel[whereSelection] <- lapply(traitNoSel[whereSelection], floor)
          stopifnot(all(unlist(lapply(traitNoSel[whereSelection], function(x) all(x >= 1)))))
          stopifnot(all(unlist(lapply(traitNoSel[whereSelection], function(x) all(x <= nTraits)))))
          traitNoSel[!whereSelection] <- rep(list(NA), sum(!whereSelection))
        }
      } else {
        traitNoSel <- 1
        message(paste0("`traitNoSel` is not specified even though you choose ", selectionMethod,
                       " method. We substitute `traitNoSel = ", traitNoSel,"` instead."))
        traitNoSel <- rep(list(traitNoSel), nSelectionWays)
        traitNoSel[!whereSelection] <- rep(list(NA), sum(!whereSelection))
      }


      if (!is.null(traitNoRA)) {
        stopifnot(is.numeric(traitNoRA))
        traitNoRA <- floor(traitNoRA)
        stopifnot(all(traitNoRA >= 1))
        stopifnot(all(traitNoRA <= nTraits))
      } else {
        if (allocateMethod == "weightedAllocation"){
          traitNoRA <- 1
          message(paste0("`traitNoRA` is not specified even though you choose ", allocateMethod,
                         " method. We substitute `traitNoRA = ", traitNoRA,"` instead."))
        } else {
          traitNoRA <- NA
        }
      }


      if (!is.null(nSel)) {
        if (!(length(nSel) %in% c(1, nSelectionWays))) {
          stop(paste("length(nSel) must be equal to 1 or equal to nSelectionWays."))
        } else if (length(nSel) == 1) {
          nSel <- rep(nSel, nSelectionWays)
        }

        stopifnot(is.numeric(nSel))
        nSel <- floor(nSel)
        stopifnot(all(nSel <= nIndNow))
      } else {
        nSel <- nIndNow %/% 10
        message(paste0("`nSel` is not specified even though you choose ", selectionMethod,
                       " method. We substitute `nSel = ", nSel,"` instead."))
        nSel <- rep(nSel, nSelectionWays)
      }
      nSel[!whereSelection] <- nIndNow


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
        stopifnot(nMrkInBlock <= min(parentPopulation$specie$nLoci))
      } else {
        nMrkInBlock <- min(parentPopulation$specie$nLoci) %/% 10
        if (any(c("selectOHV", "selectOPV") %in% selectionMethod)) {
          message(paste0("`nMrkInBlock` is not specified even though you choose ", selectionMethod,
                         " method. We substitute `nMrkInBlock = ", nMrkInBlock,"` instead."))
        }
      }


      # minimumSegmentLength
      if (!is.null(minimumSegmentLength)) {
        stopifnot(is.numeric(minimumSegmentLength))
        minimumSegmentLength <- floor(minimumSegmentLength)
        stopifnot(minimumSegmentLength >= 1)
        stopifnot(minimumSegmentLength <= min(parentPopulation$specie$lChr))
      } else {
        minimumSegmentLength <- 6.25
        if (any(c("selectOHV", "selectOPV") %in% selectionMethod)) {
          message(paste0("`minimumSegmentLength` is not specified even though you choose ", selectionMethod,
                         " method. We substitute `minimumSegmentLength = ", minimumSegmentLength,"` instead."))
        }
      }



      # nIterOPV
      if (!is.null(nIterOPV)) {
        stopifnot(is.numeric(nIterOPV))
        nIterOPV <- floor(nIterOPV)
        stopifnot(nIterOPV >= 1)
      } else {
        nIterOPV <- 5000
        if ("selectOPV" %in% selectionMethod) {
          message(paste0("`nIterOPV` is not specified even though you choose ", selectionMethod,
                         " method. We substitute `nIterOPV = ", nIterOPV,"` instead."))
        }
      }


      # clusteringForSel
      if (!is.null(clusteringForSel)) {
        if (!(length(clusteringForSel) %in% c(1, nSelectionWays))) {
          stop(paste("length(clusteringForSel) must be equal to 1 or equal to nSelectionWays."))
        } else if (length(clusteringForSel) == 1) {
          clusteringForSel <- rep(clusteringForSel, nSelectionWays)
        }

        stopifnot(is.logical(clusteringForSel))
      } else {
        clusteringForSel <- FALSE
        message(paste0("`clusteringForSel` is not specified even though you choose ", selectionMethod,
                       " method. We substitute `clusteringForSel = ", clusteringForSel,"` instead."))
        clusteringForSel <- rep(clusteringForSel, nSelectionWays)
      }

      if ("selectOPV" %in% selectionMethod) {
        clusteringForSel[selectionMethod %in% "selectOPV"] <- FALSE
        message("When you use `selectOPV` for selection method, you cannot perform selection with clustering methods.")
      }


      if (!is.null(nCluster)) {
        if (!(length(nCluster) %in% c(1, nSelectionWays))) {
          stop(paste("length(nCluster) must be equal to 1 or equal to nSelectionWays."))
        } else if (length(nCluster) == 1) {
          nCluster <- rep(nCluster, nSelectionWays)
        }

        stopifnot(is.numeric(nCluster))
        nCluster <- floor(nCluster)
      } else {
        nCluster <- ifelse(clusteringForSel, 10, 1)
        message(paste0("`nCluster` is not specified even though you choose ", selectionMethod,
                       " method. We substitute `nCluster = ", nCluster,"` instead."))
        nCluster <- rep(nCluster, nSelectionWays)
      }
      nCluster[!whereSelection] <- NA


      if (!is.null(nTopCluster)) {
        if (!(length(nTopCluster) %in% c(1, nSelectionWays))) {
          stop(paste("length(nTopCluster) must be equal to 1 or equal to nSelectionWays."))
        } else if (length(nTopCluster) == 1) {
          nTopCluster <- rep(nTopCluster, nSelectionWays)
        }

        stopifnot(is.numeric(nTopCluster))
        nTopCluster <- floor(nTopCluster)
      } else {
        nTopCluster <- ifelse(clusteringForSel, 5, 1)
        message(paste0("`nTopCluster` is not specified even though you choose ", selectionMethod,
                       " method. We substitute `nTopCluster = ", nTopCluster,"` instead."))
        nTopCluster <- rep(nTopCluster, nSelectionWays)
      }
      nTopCluster[!whereSelection] <- NA

      if (any(nTopCluster[whereSelection] > nCluster[whereSelection])) {
        nTopClusterSel <- nTopCluster[whereSelection]
        nClusterSel <- nCluster[whereSelection]
        nTopClusterSel[nTopClusterSel > nClusterSel] <- nClusterSel[nTopClusterSel > nClusterSel]

        nTopCluster[whereSelection] <- nTopClusterSel
        message(paste0("nTopCluster will be set to satisfy that it is equal to or lower than nCluster!"))
      }

      if (!is.null(nTopEach)) {
        if (!(length(nTopEach) %in% c(1, nSelectionWays))) {
          stop(paste("length(nTopEach) must be equal to 1 or equal to nSelectionWays."))
        } else if (length(nTopEach) == 1) {
          nTopEach <- rep(nTopEach, nSelectionWays)
        }
        stopifnot(is.numeric(nTopEach))
        nTopEach <- floor(nTopEach)
      } else {
        nTopEach <- ifelse(clusteringForSel, nSel %/% nTopCluster, nSel)
        message(paste0("`nTopEach` is not specified even though you choose ", selectionMethod,
                       " method. We substitute `nTopEach = ", nTopEach,"` instead."))
        nTopEach <- rep(nTopEach, nSelectionWays)
      }
      nTopEach[!whereSelection] <- NA


      if (!is.null(nNextPop)) {
        stopifnot(is.numeric(nNextPop))
        nNextPop <- floor(nNextPop)
        stopifnot(all(nNextPop >= 1))
      } else {
        nNextPop <- nIndNow
        message(paste0("`nNextPop` is not specified. We substitute `nNextPop = ", nNextPop, "` instead."))
      }






      if (!is.null(nPairs)) {
        stopifnot(is.numeric(nPairs))
      }


      if (!is.null(nProgenies)) {
        stopifnot(is.numeric(nProgenies))
        if (!is.null(nPairs)) {
          if (length(nProgenies) == 1) {
            nProgenies <- rep(nProgenies, nPairs)
          }

          if (length(nProgenies) != nPairs) {
            nPairs <- length(nProgenies)
            warning("`nPairs` should be equal to `length(nProgenies)`!")
          }
        }
      }


      stopifnot(is.logical(includeGVP))
      if (matingMethod == "makeDH") {
        includeGVP <- FALSE
        message("If you use `makeDH` option for mating method, you cannot include genetic variance of progenies for weighted resource allocation method.")
      }


      if (!is.null(h)) {
        stopifnot(is.numeric(h))
        stopifnot(all(h >= 0))
      } else {
        h <- 0.1
        message(paste0("`h` is not specified. We substitute `h = ", h, "` instead."))
      }

      hLen <- length(traitNoRA) * (length(weightedAllocationMethod) + includeGVP)

      if (!(length(h) %in% c(1, hLen))) {
        stop(paste("length(h) must be equal to 1 or equal to `length(traitNoRA) * (length(weightedAllocationMethod) + includeGVP)`"))
      } else if (length(h) == 1) {
        h <- rep(h, hLen)
      }





      if (is.null(userSI)) {
        if (selectionMethod %in% "userSI") {
          stop(paste0("You should specify `userSI` object when you choose ", selectionMethod, " method!!"))
        }
      } else {
        stopifnot(is.numeric(userSI))
        userSI <- as.matrix(userSI)
        if (!all(class(userSI) %in% c("matrix", "array"))) {
          stop("`class(userSI)` should be 2-dimensional matrix.")
        } else {
          if (length(dim(userSI)) != 2) {
            stop("`class(userSI)` should be 2-dimensional matrix.")
          } else {
            if (ncol(userSI) > nTraits) {
              stop(paste0("`crosses` should have columns equal to or lower than ", nTraits, "."))
            } else {
              if (!any(colnames(userSI) %in% traitNames)) {
                stop(paste0('colnames(userSI) must include either of "',
                            paste0(traitNames, collapse = '"; "'), '".'))
              } else {
                userSI <- userSI[, traitNames[traitNames %in% colnames(userSI)], drop = FALSE]

                if (!all(names(parentPopulation$inds) %in% rownames(userSI))) {
                  stop(paste0('rownames(userSI) must include all individual names in parent population.'))
                }

                userSI <- userSI[names(parentPopulation$inds), , drop =  FALSE]
              }
            }
          }
        }
      }



      if (is.null(lociEffects)) {
        if (selectionMethod %in% selectionMethodsWithMrkEff) {
          stop(paste0("You should specify `lociEffects` object when you choose ", selectionMethod, " method!!"))
        }
      } else {
        stopifnot(is.numeric(lociEffects))
        lociEffects <- as.matrix(lociEffects)
        if (!all(class(lociEffects) %in% c("matrix", "array"))) {
          stop("`class(lociEffects)` should be 2-dimensional matrix.")
        } else {
          if (length(dim(lociEffects)) != 2) {
            stop("`class(lociEffects)` should be 2-dimensional matrix.")
          } else {
            if (ncol(lociEffects) > nTraits) {
              stop(paste0("`lociEffects` should have columns equal to or lower than ", nTraits, "."))
            } else {
              if (!any(colnames(lociEffects) %in% traitNames)) {
                stop(paste0('colnames(lociEffects) must include either of "',
                            paste0(traitNames, collapse = '"; "'), '".'))
              } else {
                lociEffects <- lociEffects[, traitNames[traitNames %in% colnames(lociEffects)], drop = FALSE]

                effectNames <- c("Intercept", colnames(parentPopulation$genoMat))

                if (!is.null(rownames(lociEffects))) {
                  if (nrow(lociEffects) != length(effectNames)) {
                    message("`nrow(lociEffects)` does not match with the number of markers (+ intercept)!")
                  }

                  if (any(rownames(lociEffects) %in% effectNames)) {
                    lociEffects <- apply(X = lociEffects, MARGIN = 2,
                                         FUN = function(SNPEffect) {
                                           SNPEffectRet <- SNPEffect[effectNames]
                                           SNPEffectRet[is.na(SNPEffectRet)] <- 0

                                           names(SNPEffectRet) <- effectNames
                                           return(SNPEffectRet)
                                         })
                  } else {
                    stop("The SNPs for selection you set were not included in the marker genotype!")
                  }
                } else {
                  if (nrow(lociEffects) != length(effectNames)) {
                    stop("Please specify which effects correspond to which markers!")
                  } else {
                    rownames(lociEffects) <- effectNames
                  }
                }
              }
            }
          }
        }
      }



      if (!is.null(seedSimRM)) {
        if (!is.na(seedSimRM)) {
          stopifnot(is.numeric(seedSimRM))
          seedSimRM <- floor(seedSimRM)
        } else {
          seedSimRM <- sample(x = 1e9, size = 1)
        }
      }

      if (!is.null(seedSimMC)) {
        if (!is.na(seedSimMC)) {
          stopifnot(is.numeric(seedSimMC))
          seedSimMC <- floor(seedSimMC)
        } else {
          seedSimMC <- sample(x = 1e9, size = 1)
        }
      }

      if (!is.null(selCands)) {
        stopifnot(is.character(selCands))
        stopifnot(length(selCands) == nSel)
      }

      if (!is.null(crosses)) {
        if (!all(class(crosses) %in% c("data.frame", "matrix", "array"))) {
          stop("`class(crosses)` should be `data.frame` or 2-dimensional matrix.")
        } else {
          if (length(dim(crosses)) != 2) {
            stop("`class(crosses)` should be `data.frame` or 2-dimensional matrix.")
          } else {
            if (ncol(crosses) != 4) {
              stop("`crosses` should consist of 4 columns ")
            } else {
              if (!all(c("ind1", "ind2", "n", "names") %in% colnames(crosses))) {
                stop('colnames(crosses) must be "ind1", "ind2", "n", "names".')
              } else {
                crosses <- crosses[, c("ind1", "ind2", "n", "names"), drop = FALSE]
                crosses <- data.frame(crosses)
                crosses <- crosses[crosses$n > 0, , drop = FALSE]

                if (!identical(nProgenies, crosses$n)) {
                  warning("`nProgenies` should be equal to `crosses$n`!")
                  nProgenies <- crosses$n
                }

                if (!identical(nPairs, nrow(crosses))) {
                  warning("`nPairs` should be equal to `nrow(crosses)`!")
                  nPairs <- nrow(crosses)
                }

                if (sum(nProgenies) != nNextPop) {
                  warning("`nNextPop` should be equal to `sum(nProgenies)`!")
                  nNextPop <- sum(nProgenies)
                }
              }
            }
          }
        }
      }


      if (!is.null(indNames)) {
        if (class(indNames) != "character") {
          stop("Please provide individuals' names as a character vector.")
        }


        if (length(indNames) == nNextPop) {
          nameMethod <- "individualBase"
        } else if (length(indNames) == 1) {
          if (nameMethod == "userSpecific") {
            nameMethod <- "pairBase"
            message("You set `indNames` with length 1, so `nameMethod` will be set as 'pairBase'.")
          }
        } else if (!is.null(nPairs)) {
          if (length(indNames) == nPairs) {
            nameMethod <- "pairBase"
          } else {
            stop('"length(indNames)" must be equal to "1", "nPairs" or to "nNextPop"')
          }
        }
      } else {
        generationNow <- parentPopulation$generation
        if (is.na(generationNow)) {
          generationNew <- 1
          warning(paste0("`generation` was not specified for parent population. ",
                         "The new population will be regarded as the first population. (`genration = 1`)"))
        } else {
          if (is.numeric(generationNow)) {
            generationNew <- generationNow + 1
          } else {
            stop("We only support `numeric` class for `parentPopulation$generation` object!")
          }
        }

        if (nameMethod == "individualBase") {
          indNames <- .charSeq(paste0("G", generationNew, "_"), seq(nNextPop))
        } else if (nameMethod == "pairBase") {
          if (!is.null(nPairs)) {
            if (nPairs != 1) {
              indNames <- .charSeq(paste0("G", generationNew, "_"), seq(nPairs))
            } else {
              indNames <- paste0("G", generationNew)
            }
          } else {
            indNames <- paste0("G", generationNew)
          }
        }
      }

      if (any(indNames %in% rownames(parentPopulation$genoMat))) {
        warning(paste0("The following offspring names will overlap the line names in the population: ",
                       paste(indNames[indNames %in% names(parentPopulation$inds)], collapse = "; ")))
      }




      # nProgeniesEMBV
      if (!is.null(nProgeniesEMBV)) {
        stopifnot(is.numeric(nProgeniesEMBV))
        nProgeniesEMBV <- floor(nProgeniesEMBV)
        stopifnot(nProgeniesEMBV >= 1)
      } else {
        nProgeniesEMBV <- round(2 * nNextPop / min(sum(nSel), parentPopulation$nInd))
        if ("selectEMBV" %in% selectionMethod) {
          message(paste0("`nProgeniesEMBV` is not specified even though you choose ", selectionMethod,
                         " method. We substitute `nProgeniesEMBV = ", nProgeniesEMBV,"` instead."))
        }
      }


      # nIterEMBV
      if (!is.null(nIterEMBV)) {
        stopifnot(is.numeric(nIterEMBV))
        nIterEMBV <- floor(nIterEMBV)
        stopifnot(nIterEMBV >= 1)
      } else {
        nIterEMBV <- 10
        if ("selectEMBV" %in% selectionMethod) {
          message(paste0("`nIterEMBV` is not specified even though you choose ", selectionMethod,
                         " method. We substitute `nIterEMBV = ", nIterEMBV,"` instead."))
        }
      }


      # nCoresEMBV
      if (!is.null(nCoresEMBV)) {
        stopifnot(is.numeric(nCoresEMBV))
        nCoresEMBV <- floor(nCoresEMBV)
        stopifnot(nCoresEMBV >= 1)
      } else {
        nCoresEMBV <- 1
        message(paste0("`nCoresEMBV` is not specified. We substitute `nCoresEMBV = ",
                       nCoresEMBV,"` instead."))
      }

      if (nCoresEMBV >= parallel::detectCores()) {
        warning("You are going to assign the number of cores larger than that of your PC to `nCoresEMBV` ! Is it OK ?")
      }


      self$parentPopulation <- parentPopulation
      self$nSelectionWays <- nSelectionWays
      self$selectionMethod <- selectionMethod
      self$traitNoSel <- traitNoSel
      self$traitNoRA <- traitNoRA
      self$userSI <- userSI
      self$lociEffects <- lociEffects
      self$blockSplitMethod <- blockSplitMethod
      self$nMrkInBlock <- nMrkInBlock
      self$minimumSegmentLength <- minimumSegmentLength
      self$nIterOPV <- nIterOPV
      self$clusteringForSel <- clusteringForSel
      self$nCluster <- nCluster
      self$nTopCluster <- nTopCluster
      self$nTopEach <- nTopEach
      self$nSel <- nSel
      self$matingMethod <- matingMethod
      self$allocateMethod <- allocateMethod
      self$weightedAllocationMethod <- weightedAllocationMethod
      self$nProgenies <- nProgenies
      self$h <- h
      self$includeGVP <- includeGVP
      self$nNextPop <- nNextPop
      self$nPairs <- nPairs
      self$nameMethod <- nameMethod
      self$indNames <- indNames
      self$seedSimRM <- seedSimRM
      self$seedSimMC <- seedSimMC
      self$selCands <- selCands
      self$crosses <- crosses
      self$nProgeniesEMBV <- nProgeniesEMBV
      self$nIterEMBV <- nIterEMBV
      self$nCoresEMBV <- nCoresEMBV
      self$verbose <- verbose
    },


    #' @description
    #' Compute genetic distance from marker genotype
    #'
    computeGenDist = function() {
      genoMat <- self$parentPopulation$genoMat
      distObj <- dist(x = genoMat, method = "euclidean")

      self$genDist <- distObj
    },


    #' @description
    #' Hierarchical clustering for marker genotype
    #' @param nCluster [numeric] Number of clusters
    #'
    hClustering = function(nCluster = NULL) {
      if (is.null(self$genDist)){
        self$computeGenDist()
      }
      distObj <- self$genDist
      treObj <- hclust(d = distObj, method = "ward.D2")

      if (is.null(nCluster)) {
        for (selectionWayNo in 1:self$nSelectionWays) {
          group <- cutree(tree = treObj, k = self$nCluster[selectionWayNo])
          self$groups[[paste0("nCluster_", nCluster)]] <- group
        }
      } else {
        group <- cutree(tree = treObj, k = nCluster)
        self$groups[[paste0("nCluster_", nCluster)]] <- group
      }
    },

    #' @description
    #' Select parent candidates
    #' If you set `addCands = TRUE`, you can add selection candidates by setting each parameter.
    #' @param nSelectionWaysPlus [numeric] Number of selection ways when you want to add candidates
    #' @param selectionMethod [character] Selection method when you want to add candidates
    #' @param traitNoSel [numeric / list] (list of) number of trait No of your interest for selection when you want to add candidates
    #' @param clusteringForSel [logical] Apply clustering results of marker genotype to selection or not when you want to add candidates
    #' @param nCluster [numeric] Number of clusters when you want to add candidates
    #' @param nTopCluster [numeric] Number of top clusters used for selection when you want to add candidates
    #' @param nTopEach [numeric] Number of selected individuals in each cluster when you want to add candidates
    #' @param nSel [numeric] Number of selection candidates when you want to add candidates
    #' @param parentCands [character] Names of selection candidates to be added
    #' @param addCands [logical] If you set `addCands = TRUE`, you can add selection candidates by setting each parameter.
    #' In other words, the parameters above does not make sense.
    #'
    #'
    selectParentCands = function(nSelectionWaysPlus = NA,
                                 selectionMethod = NULL,
                                 traitNoSel = NULL,
                                 clusteringForSel = NULL,
                                 nCluster = NULL,
                                 nTopCluster = NULL,
                                 nTopEach = NULL,
                                 nSel = NULL,
                                 parentCands = NULL,
                                 addCands = FALSE) {
      selectionMethodsOffered <- c("nonSelection", "selectBV", "selectWBV", "selectOHV",
                                   "selectEMBV", "selectOPV", "userSI", "userSpecific")
      selectionMethodsWithSelection <- c("selectBV", "selectWBV", "selectOHV",
                                         "selectEMBV", "selectOPV", "userSI")
      selectionMethodsWithMrkEff <- c("selectBV", "selectWBV", "selectOHV", "selectEMBV", "selectOPV")
      nIndNow <- self$parentPopulation$nInd
      nTraits <- self$parentPopulation$traitInfo$nTraits
      traitNames <- self$parentPopulation$traitInfo$traitNames


      nSelectionWays <- self$nSelectionWays
      selCands <- self$selCands

      if (!addCands) {
        selectionMethod <- self$selectionMethod
        clusteringForSel <- self$clusteringForSel
        nCluster <- self$nCluster
        nTopCluster <- self$nTopCluster
        nTopEach <- self$nTopEach
        nSel <- self$nSel
        traitNoSel <- self$traitNoSel
      } else {
        ### check
        if (!is.na(nSelectionWaysPlus)) {
          stopifnot(is.numeric(nSelectionWaysPlus))
          nSelectionWaysPlus <- floor(nSelectionWaysPlus)
          stopifnot(nSelectionWaysPlus >= 1)
        } else {
          nSelectionWaysPlus <- 1
          message(paste0("`nSelectionWaysPlus` is not specified. We substitute `nSelectionWaysPlus = ",
                         nSelectionWaysPlus,"` instead."))
        }


        if (!is.null(selectionMethod)) {
          if (!all(selectionMethod %in% selectionMethodsOffered)) {
            stop(paste0("We only offer the following selection methods: ",
                        paste(selectionMethodsOffered, collapse = "; ")))
          }
        } else {
          selectionMethod <- "userSpecific"
          message("You do not specify the selection method. User specific selection will be performed.")
        }

        if (!(length(selectionMethod) %in% c(1, nSelectionWaysPlus))) {
          stop(paste("length(selectionMethod) must be equal to 1 or equal to nSelectionWaysPlus."))
        } else if (length(selectionMethod) == 1) {
          selectionMethod <- rep(selectionMethod, nSelectionWaysPlus)
        }

        if (any(selectionMethodsWithMrkEff %in% selectionMethod)) {
          if (is.null(lociEffects)) {
            stop("You must specify `lociEffects` argument if you use selection based on SNP effects!!")
          }
        }

        if ("userSI" %in% selectionMethod) {
          if (is.null(userSI)) {
            stop("You must specify `userSI` argument if you use user-defined selection indices!!")
          }
        }

        if ("userSpecific" %in% selectionMethod) {
          if (is.null(selCands)) {
            stop("You must specify `selCands` argument if you do not specify the selection methods!!")
          }
        }
        whereSelection <- selectionMethod %in% selectionMethodsWithSelection




        if (!is.null(traitNoSel)) {
          if (is.numeric(traitNoSel)) {
            stopifnot(is.numeric(traitNoSel))
            traitNoSel <- floor(traitNoSel)
            stopifnot(all(traitNoSel >= 1))
            stopifnot(all(traitNoSel <= nTraits))
            traitNoSel <- rep(list(traitNoSel), nSelectionWaysPlus)
          } else if (is.list(traitNoSel)) {
            if (!(length(traitNoSel) %in% c(1, nSelectionWaysPlus))) {
              stop(paste("length(traitNoSel) must be equal to 1 or equal to nSelectionWaysPlus."))
            } else if (length(traitNoSel) == 1) {
              traitNoSel <- rep(traitNoSel, nSelectionWaysPlus)
            }
            stopifnot(all(unlist(lapply(traitNoSel[whereSelection], is.numeric))))
            traitNoSel[whereSelection] <- lapply(traitNoSel[whereSelection], floor)
            stopifnot(all(unlist(lapply(traitNoSel[whereSelection], function(x) all(x >= 1)))))
            stopifnot(all(unlist(lapply(traitNoSel[whereSelection], function(x) all(x <= nTraits)))))
            traitNoSel[!whereSelection] <- rep(list(NA), sum(!whereSelection))
          }
        } else {
          traitNoSel <- 1
          message(paste0("`traitNoSel` is not specified even though you choose ", selectionMethod,
                         " method. We substitute `traitNoSel = ", traitNoSel,"` instead."))
          traitNoSel <- rep(list(traitNoSel), nSelectionWaysPlus)
          traitNoSel[!whereSelection] <- rep(list(NA), sum(!whereSelection))
        }


        if (!is.null(nSel)) {
          if (!(length(nSel) %in% c(1, nSelectionWaysPlus))) {
            stop(paste("length(nSel) must be equal to 1 or equal to nSelectionWaysPlus."))
          } else if (length(nSel) == 1) {
            nSel <- rep(nSel, nSelectionWaysPlus)
          }

          stopifnot(is.numeric(nSel))
          nSel <- floor(nSel)
          stopifnot(all(nSel <= nIndNow))
        } else {
          nSel <- nIndNow %/% 10
          message(paste0("`nSel` is not specified even though you choose ", selectionMethod,
                         " method. We substitute `nSel = ", nSel,"` instead."))
          nSel <- rep(nSel, nSelectionWaysPlus)
        }
        nSel[!whereSelection] <- nIndNow


        if (!is.null(clusteringForSel)) {
          if (!(length(clusteringForSel) %in% c(1, nSelectionWays))) {
            stop(paste("length(clusteringForSel) must be equal to 1 or equal to nSelectionWays."))
          } else if (length(clusteringForSel) == 1) {
            clusteringForSel <- rep(clusteringForSel, nSelectionWays)
          }

          stopifnot(is.logical(clusteringForSel))
        } else {
          clusteringForSel <- FALSE
          message(paste0("`clusteringForSel` is not specified even though you choose ", selectionMethod,
                         " method. We substitute `clusteringForSel = ", clusteringForSel,"` instead."))
          clusteringForSel <- rep(clusteringForSel, nSelectionWays)
        }

        if ("selectOPV" %in% selectionMethod) {
          clusteringForSel[selectionMethod %in% "selectOPV"] <- FALSE
          message("When you use `selectOPV` for selection method, you cannot perform selection with clustering methods.")
        }


        if (!is.null(nCluster)) {
          if (!(length(nCluster) %in% c(1, nSelectionWaysPlus))) {
            stop(paste("length(nCluster) must be equal to 1 or equal to nSelectionWaysPlus."))
          } else if (length(nCluster) == 1) {
            nCluster <- rep(nCluster, nSelectionWaysPlus)
          }

          stopifnot(is.numeric(nCluster))
          nCluster <- floor(nCluster)
        } else {
          nCluster <- ifelse(clusteringForSel, 10, 1)
          message(paste0("`nCluster` is not specified even though you choose ", selectionMethod,
                         " method. We substitute `nCluster = ", nCluster,"` instead."))
          nCluster <- rep(nCluster, nSelectionWaysPlus)
        }
        nCluster[!whereSelection] <- NA


        if (!is.null(nTopCluster)) {
          if (!(length(nTopCluster) %in% c(1, nSelectionWaysPlus))) {
            stop(paste("length(nTopCluster) must be equal to 1 or equal to nSelectionWaysPlus."))
          } else if (length(nTopCluster) == 1) {
            nTopCluster <- rep(nTopCluster, nSelectionWaysPlus)
          }

          stopifnot(is.numeric(nTopCluster))
          nTopCluster <- floor(nTopCluster)
        } else {
          nTopCluster <- ifelse(clusteringForSel, 5, 1)
          message(paste0("`nTopCluster` is not specified even though you choose ", selectionMethod,
                         " method. We substitute `nTopCluster = ", nTopCluster,"` instead."))
          nTopCluster <- rep(nTopCluster, nSelectionWaysPlus)
        }
        nTopCluster[!whereSelection] <- NA

        if (any(nTopCluster[whereSelection] > nCluster[whereSelection])) {
          nTopClusterSel <- nTopCluster[whereSelection]
          nClusterSel <- nCluster[whereSelection]
          nTopClusterSel[nTopClusterSel > nClusterSel] <- nClusterSel[nTopClusterSel > nClusterSel]

          nTopCluster[whereSelection] <- nTopClusterSel
          message(paste0("nTopCluster will be set to satisfy that it is equal to or lower than nCluster!"))
        }

        if (!is.null(nTopEach)) {
          if (!(length(nTopEach) %in% c(1, nSelectionWaysPlus))) {
            stop(paste("length(nTopEach) must be equal to 1 or equal to nSelectionWaysPlus."))
          } else if (length(nTopEach) == 1) {
            nTopEach <- rep(nTopEach, nSelectionWaysPlus)
          }
          stopifnot(is.numeric(nTopEach))
          nTopEach <- floor(nTopEach)
        } else {
          nTopEach <- ifelse(clusteringForSel, nSel %/% nTopCluster, nSel)
          message(paste0("`nTopEach` is not specified even though you choose ", selectionMethod,
                         " method. We substitute `nTopEach = ", nTopEach,"` instead."))
          nTopEach <- rep(nTopEach, nSelectionWaysPlus)
        }
        nTopEach[!whereSelection] <- NA

        nSelectionWays <- nSelectionWaysPlus
        self$selectionMethod <- c(self$selectionMethod, selectionMethod)
        self$clusteringForSel <- c(self$clusteringForSel, clusteringForSel)
        self$nCluster <- c(self$nCluster, nCluster)
        self$nTopCluster <- c(self$nTopCluster, nTopCluster)
        self$nTopEach <- c(self$nTopEach, nTopEach)
        self$nSel <- c(self$nSel, nSel)
        self$traitNoSel <- c(self$traitNoSel, traitNoSel)
      }


      parentCandsTotal <- NULL
      for (selectionWayNo in 1:nSelectionWays) {
        if (selectionMethod[selectionWayNo] != "userSpecific") {
          if (selectionMethod[selectionWayNo] == "nonSelection") {
            parentCands <- names(self$parentPopulation$inds)
          } else if (selectionMethod[selectionWayNo] == "selectOPV") {
            indNamesAll <- names(self$parentPopulation$inds)

            # if (is.null(self$BV)) {
            #   BV <- self$computeBV
            # } else {
            #   BV <- self$BV
            # }
            # BVNow <- BV[, traitNoSel[[selectionWayNo]], drop = FALSE]
            # BVPositive <- t(t(BVNow) - apply(BVNow, 2, min))
            # BVSel <- apply(X = BVPositive, MARGIN = 1, FUN = prod)
            #
            # indNamesCands <- names(private$extractTopN(v = BVSel, n = nSel[selectionWayNo]))

            indNamesCands <- sample(x = indNamesAll, size = nSel[selectionWayNo])

            OPVNow <- private$computeOPV(indNamesCands = indNamesCands)[traitNoSel[[selectionWayNo]]]
            OPVSelNow <- sum(OPVNow)

            countOPV <- 0
            thresOPV <- self$nIterOPV %/% 3
            for (iterOPVNo in 1:self$nIterOPV) {
              indNamesNonCands <- indNamesAll[!(indNamesAll %in% indNamesCands)]
              # swapNos <- sample(1:max(1, nSel %/% 5), 1)
              # indNamesCandsCand <- c(sample(indNamesCands, nSel - swapNos), sample(indNamesNonCands, swapNos))
              indNamesCandsCand <- c(sample(indNamesCands, nSel - 1), sample(indNamesNonCands, 1))
              OPVNew <- private$computeOPV(indNamesCands = indNamesCands)[traitNoSel[[selectionWayNo]]]
              OPVSelNew <- sum(OPVNew)

              if (OPVSelNew > OPVSelNow) {
                countOPV <- 0
                OPVNow <- OPVNew
                OPVSelNow <- OPVSelNew
                indNamesCands <- indNamesCandsCand
              } else {
                countOPV <- countOPV + 1
                if (countOPV >= thresOPV) {
                  break
                }
              }
            }

            parentCands <- indNamesCands
          } else {
            if (selectionMethod == "selectBV") {
              if (is.null(self$BV)) {
                BV <- self$computeBV
              } else {
                BV <- self$BV
              }
            } else if (selectionMethod == "selectWBV") {
              if (is.null(self$WBV)) {
                BV <- self$computeWBV
              } else {
                BV <- self$WBV
              }
            } else if (selectionMethod == "selectOHV") {
              if (is.null(self$OHV)) {
                BV <- self$computeOHV
              } else {
                BV <- self$OHV
              }
            } else if (selectionMethod == "selectEMBV") {
              if (is.null(self$EMBV)) {
                BV <- self$computeEMBV
              } else {
                BV <- self$EMBV
              }
            } else if (selectionMethod == "userSI") {
              BV <- self$userSI
            }

            BVNow <- BV[, traitNoSel[[selectionWayNo]], drop = FALSE]
            BVPositive <- t(t(BVNow) - apply(BVNow, 2, min))
            BVSel <- apply(X = BVPositive, MARGIN = 1, FUN = prod)


            if (clusteringForSel) {
              if (is.null(self$groups[[paste0("nCluster_", nCluster[selectionWayNo])]])) {
                self$hClustering(nCluster = nCluster[selectionWayNo])
              }
              group <- self$groups[[paste0("nCluster_", nCluster[selectionWayNo])]]

              BVSplit <- split(BVSel, group)

              BVTopN <- lapply(BVSplit, private$extractTopN,
                               nTopEach[selectionWayNo], decreasing = TRUE)
              BVTopNMean <- sapply(BVTopN, mean)
              BVTopNMeanOrdered <- order(BVTopNMean, decreasing = TRUE)
              BVTopNMeanOrderedTops <- BVTopNMeanOrdered[1:nTopCluster[selectionWayNo]]

              BVTopN2 <- BVTopN[BVTopNMeanOrderedTops]
              BVTopN2Vec <- unlist(BVTopN2, use.names = FALSE)
              parentCands0 <- c(sapply(BVTopN2, names))
              names(BVTopN2Vec) <- parentCands0

              BVTopN2VecSorted <- sort(BVTopN2Vec, decreasing = TRUE)
              parentCands <- parentCands0[order(BVTopN2Vec, decreasing = TRUE)]

              if (nSel[selectionWayNo] <= length(parentCands)) {
                parentCands <- parentCands[1:nSel[selectionWayNo]]
              }

            } else {
              parentCands <- names(private$extractTopN(v = BVSel, n = nSel[selectionWayNo]))
            }
          }
        } else {
          if (!addCands) {
            parentCands <- selCands
          } else {
            if (!is.null(parentCands)) {
              parentCandsAll <- names(self$parentPopulation$inds)
              parentCands <- parentCands[parentCands %in% parentCandsAll]
            }
          }
        }

        parentCandsTotal <- c(parentCandsTotal, parentCands)
      }

      parentCandsTotal <- unique(parentCandsTotal)

      if (addCands) {
        selCands <- unique(c(selCands, parentCandsTotal))
        self$nSelectionWays <- self$nSelectionWays + nSelectionWaysPlus
      } else {
        selCands <- parentCandsTotal
      }

      self$selCands <- selCands
    },


    #' @description Allocate the number of progenies to each parent pair
    #' @param crosses0 [data.frame] data.frame with crossing instructions: parents names
    #' \code{ind1} \code{ind2}
    allocateProgenies = function (crosses0) {
      allocateMethod <- self$allocateMethod
      nNextPop <- self$nNextPop
      h <- self$h
      nameMethod <- self$nameMethod
      weightedAllocationMethod <- self$weightedAllocationMethod
      nPairs <- nrow(crosses0)

      if (allocateMethod == "equalAllocation") {
        crossesSorted <- crosses0

        nProgenyPerPair <- nNextPop %/% nPairs
        nProgenies <- rep(nProgenyPerPair, nPairs)
        nResids <- nNextPop %% nPairs

        if (nResids > 0) {
          nProgenies[1:nResids] <- nProgenies[1:nResids] + 1
        }
      } else if (allocateMethod == "weightedAllocation") {
        BVAll <- NULL

        if ("selectBV" %in% weightedAllocationMethod) {
          if (is.null(self$BV)) {
            BVNow <- self$computeBV[, self$traitNoRA, drop = FALSE]
          } else {
            BVNow <- self$BV[, self$traitNoRA, drop = FALSE]
          }
          BVAll <- cbind(BVAll, BVNow)
        }

        if ("selectWBV" %in% weightedAllocationMethod) {
          if (is.null(self$WBV)) {
            BVNow <- self$computeWBV[, self$traitNoRA, drop = FALSE]
          } else {
            BVNow <- self$WBV[, self$traitNoRA, drop = FALSE]
          }
          BVAll <- cbind(BVAll, BVNow)
        }

        if ("selectOHV" %in% weightedAllocationMethod) {
          if (is.null(self$OHV)) {
            BVNow <- self$computeOHV[, self$traitNoRA, drop = FALSE]
          } else {
            BVNow <- self$OHV[, self$traitNoRA, drop = FALSE]
          }
          BVAll <- cbind(BVAll, BVNow)
        }

        if ("selectEMBV" %in% weightedAllocationMethod) {
          if (is.null(self$EMBV)) {
            BVNow <- self$computeEMBV[, self$traitNoRA, drop = FALSE]
          } else {
            BVNow <- self$EMBV[, self$traitNoRA, drop = FALSE]
          }
          BVAll <- cbind(BVAll, BVNow)
        }

        if ("userSI" %in% weightedAllocationMethod) {
          BVNow <- self$userSI[, self$traitNoRA, drop = FALSE]
          BVAll <- cbind(BVAll, BVNow)
        }


        BVScaled <- scale(BVAll)
        if (self$matingMethod != "makeDH") {
          BVEachPair <- (BVScaled[crosses0[, 1], , drop = FALSE] +
                           BVScaled[crosses0[, 2], , drop = FALSE]) / 2

          if ("selectOPV" %in% weightedAllocationMethod) {
            OPVNow <- t(apply(X = crosses0, MARGIN = 1, private$computeOPV))
            BVEachPair <- cbind(BVEachPair, OPVNow[, self$traitNoRA])
          }

          if (self$includeGVP) {
            genVarProgenies <- private$computeGVP(crosses0 = crosses0)
            BVEachPair <- cbind(BVEachPair, genVarProgenies[, self$traitNoRA])
          }
        } else {
          BVEachPair <- BVScaled
        }


        weightedBVEachPair <- as.numeric(BVEachPair %*% as.matrix(h))
        nProgenies0 <- floor(nNextPop * exp(weightedBVEachPair) / sum(exp(weightedBVEachPair)))
        crossesSorted <- crosses0[order(weightedBVEachPair, decreasing = TRUE), ]
        nProgenies <- nProgenies0[order(weightedBVEachPair, decreasing = TRUE)]

        nResids <- nNextPop - sum(nProgenies0)

        if (nResids > 0) {
          nProgenies[1:nResids] <- nProgenies[1:nResids] + 1
        }
      } else if (allocateMethod == "userSpecific") {
        crossesSorted <- crosses0
        nProgenies <- self$nProgenies

        stopifnot(length(nProgenies) == nPairs)
      }

      positivePairs <- nProgenies > 0
      nProgenies <- nProgenies[positivePairs]
      nPairs <- length(nProgenies)
      crossesSorted <- crossesSorted[positivePairs, , drop = FALSE]
      colnames(crossesSorted) <- paste0("ind", 1:2)


      indNames <- self$indNames
      seed <- self$seedSimRM


      if (nameMethod == "individualBase") {
        repeatedDfList <- sapply(X = 1:nPairs,
                                 FUN = function(pairNo) {
                                   repeatedDf <- data.frame(matrix(data = rep(as.matrix(crossesSorted)[pairNo, ],
                                                                              nProgenies[pairNo]),
                                                                   nrow = nProgenies[pairNo], ncol = 2, byrow = TRUE))

                                   return(repeatedDf)
                                 }, simplify = FALSE)

        crossesWONames <- do.call(what = rbind,
                                  args = repeatedDfList)
        colnames(crossesWONames) <- paste0("ind", 1:2)

        crosses <- data.frame(
          crossesWONames,
          n = 1,
          names = indNames
        )
      } else if (nameMethod == "pairBase") {
        if (length(indNames) == 1) {
          indNames <- .charSeq(paste0(indNames, "_"), seq(nPairs))
        } else {
          indNames <- indNames[positivePairs]
        }

        crosses <- data.frame(
          crossesSorted,
          n = nProgenies,
          names = indNames
        )
      }


      self$crosses <- crosses
      self$indNames <- indNames
      self$nProgenies <- nProgenies
      self$nPairs <- nPairs
    },


    #' @description
    #' Design resource allocation (make cross table)
    #'
    designResourceAllocation = function() {
      matingMethod <- self$matingMethod

      if (is.null(self$selCands)) {
        self$selectParentCands(addCands = FALSE)
      }

      if (matingMethod != "userSpecific") {
        if (matingMethod == "randomMate") {
          crosses0 <- self$randomMate
        } else if (matingMethod == "roundRobin") {
          crosses0 <- self$roundRobin
        } else if (matingMethod == "diallel") {
          crosses0 <- self$diallel
        } else if (matingMethod == "diallelWithSelfing") {
          crosses0 <- self$diallelWithSelfing
        } else if (matingMethod == "selfing") {
          crosses0 <- self$selfing
        } else if (matingMethod == "maxGenDist") {
          crosses0 <- self$maxGenDist
        } else if (matingMethod == "makeDH") {
          crosses0 <- self$makeDH
        }

        self$allocateProgenies(crosses0 = crosses0)
      }
    },


    #' @description
    #' Display informations about the object
    print = function() {
      cat(paste0(
        "Parental population: ", self$parentPopulation$name, "\n",
        "Selection method: ", paste(self$selectionMethod, collapse = "; "), "\n",
        "Mating method: ", self$matingMethod, "\n",
        "Allocation method: ", self$allocateMethod, "\n",
        "Weighted allocation method: ", self$weightedAllocationMethod, "\n",
        "Naming method: ", self$nsmeMethod, "\n",
        "Number of selection candidates: ", sum(self$nSel), "\n",
        "Number of parent pairs: ", self$nPairs, "\n",
        "Number of progenies of a new population: ", self$nNextPop
      ))
    }


  ),
  active = list(

    #' @field computeBV [matrix] matrix of the breeding values
    computeBV = function(){
      parentPopulation <- self$parentPopulation
      lociEffects <- self$lociEffects
      genoMat <- parentPopulation$genoMat

      X <- cbind(Intercept = rep(1, nrow(genoMat)),
                 genoMat)
      BV <- X %*% lociEffects

      self$BV <- BV

      return(BV)
    },


    #' @field computeWBV [matrix] matrix of the weighted breeding values
    #' @references Jannink, Jean-Luc. “Dynamics of Long-Term Genomic Selection.”
    #'   Genetics Selection Evolution 42, no. 1 (December 2010).
    #'   https://doi.org/10.1186/1297-9686-42-35.
    computeWBV = function(){
      parentPopulation <- self$parentPopulation
      lociEffects <- self$lociEffects
      genoMat <- parentPopulation$genoMat

      X <- cbind(Intercept = rep(1, nrow(genoMat)),
                 genoMat)

      favAllel <- sign(lociEffects[-1, , drop = FALSE] > 0)

      w <- matrix(data = rep(parentPopulation$af, ncol(favAllel)),
                  nrow = nrow(favAllel),
                  ncol = ncol(favAllel),
                  dimnames = dimnames(favAllel))
      for (traitNo in 1:ncol(favAllel)){
        w[favAllel[, traitNo] == 0, traitNo] <- 1 - w[favAllel[, traitNo] == 0, traitNo]
      }

      w[w == 0] <- 1 # give weight 1 for fixed alleles
      w <- w ^ (-0.5)

      W_lociEffects <- rbind(lociEffects[1, , drop = FALSE],
                             lociEffects[-1, , drop = FALSE] * w)
      WBV <- X %*% W_lociEffects

      self$WBV <- WBV

      return(WBV)
    },


    #' @field computeOHV [matrix] matrix of the optimal haploid values
    #' @references Daetwyler, H.D., Hayden, M.J., Spangenberg, G.C. and Hayes, B.J. (2015)
    #' Selection on optimal haploid value increases genetic gain and preserves more genetic diversity relative to genomic selection.
    #' Genetics. 200(4): 1341-1348.
    computeOHV = function(){
      if (is.null(self$haploEffMaxArray)) {
        parentPopulation <- self$parentPopulation
        lociEffects <- self$lociEffects
        haploArray <- parentPopulation$haploArray
        nMrkInBlock <- self$nMrkInBlock
        minimumSegmentLength <- self$minimumSegmentLength
        blockSplitMethod <- self$blockSplitMethod
        genoMap <- parentPopulation$traitInfo$lociInfo$genoMap

        lociEffectsWOIntercept <- lociEffects[-1, , drop = FALSE]

        chr <- genoMap$chr
        chrUnique <- unique(chr)

        if (blockSplitMethod == "nMrkInBlock") {
          blockRangesList <- sapply(chrUnique,
                                    function(eachChr) {
                                      lociNos <- (1:nrow(genoMap))[chr %in% eachChr]
                                      splitNos <- split(lociNos, (lociNos - (lociNos[1] %% nMrkInBlock)) %/% nMrkInBlock)

                                      return(splitNos)
                                    }, simplify = FALSE)
        } else {
          lChr <- parentPopulation$specie$lChr
          nSegmentPerChr <- floor(lChr / minimumSegmentLength)
          lSegment <- lChr / nSegmentPerChr


          blockRangesList <- sapply(chrUnique,
                                    function(eachChr) {
                                      genoPosEachChr <- genoMap$pos[genoMap$chr == eachChr]
                                      lociNos <- (1:nrow(genoMap))[chr %in% eachChr]
                                      lSegmentsSplit <- cumsum(c(0, rep(lSegment[eachChr],
                                                                        nSegmentPerChr[eachChr])))
                                      blockOfEachPos <- cut(x = genoPosEachChr,
                                                            breaks = lSegmentsSplit)

                                      splitNos <- split(x = lociNos, f = blockOfEachPos)

                                      return(splitNos)
                                    }, simplify = FALSE)
        }

        blockRanges <- do.call(what = c,
                               args = blockRangesList)

        haploEffMaxList <- lapply(X = blockRanges,
                                  FUN = function (blockRangeNow) {
                                    haploArrayNow <- haploArray[, blockRangeNow, ]
                                    haploArray_1 <- haploArrayNow[, , 1]
                                    haploArray_2 <- haploArrayNow[, , 2]
                                    haploEff_1 <- haploArray_1 %*% lociEffectsWOIntercept[blockRangeNow, , drop = FALSE]
                                    haploEff_2 <- haploArray_2 %*% lociEffectsWOIntercept[blockRangeNow, , drop = FALSE]

                                    haploEffMax <- pmax(haploEff_1, haploEff_2)

                                    haploEffMaxOneArray <- array(data = haploEffMax,
                                                                 dim = c(dim(haploEffMax), 1),
                                                                 dimnames = c(dimnames(haploEffMax),
                                                                              list(Block = "")))

                                    return(haploEffMaxOneArray)
                                  })
        haploEffMaxArray <- do.call(what = abind::abind,
                                    args = haploEffMaxList)

        self$haploEffMaxArray <- haploEffMaxArray
      } else {
        haploEffMaxArray <- self$haploEffMaxArray
      }

      OHV0 <- 2 * apply(haploEffMaxArray, c(1, 2), sum)
      OHV <- OHV0 + matrix(data = rep(lociEffects[1, ], nrow(OHV0)),
                           nrow = nrow(OHV0), ncol = ncol(OHV0),
                           byrow = TRUE)

      self$OHV <- OHV

      return(OHV)
    },



    #' @field computeEMBV [matrix] matrix of the expected maximum haploid breeding values
    #' @references Müller, D., Schopp, P. and Melchinger, A.E. (2018)
    #' Selection on expected maximum haploid breeding values can increase genetic gain in recurrent genomic selection.
    #' G3 (Bethesda). 8(4): 1173-1181.
    computeEMBV = function(){
      parentPopulation <- self$parentPopulation
      nProgeniesEMBV <- self$nProgeniesEMBV
      nIterEMBV <- self$nIterEMBV
      nCoresEMBV <- self$nCoresEMBV

      indNamesEMBV <- paste0(names(parentPopulation$inds), "_DH")

      if (self$verbose) {
        EMBVArrayList <- pbmcapply::pbmclapply(X = 1:nIterEMBV,
                                               FUN = function (iterEMBVNo) {
                                                 newIndsEMBV <- mapply(private$makeSingleDH,
                                                                       parentPopulation$inds,
                                                                       indNamesEMBV,
                                                                       rep(nProgeniesEMBV, parentPopulation$nInd),
                                                                       USE.NAMES = FALSE)
                                                 newIndsEMBV <- unlist(newIndsEMBV)


                                                 newPopEMBV <- population$new(name = "ForEMBV",
                                                                              generation = parentPopulation$generation + 1,
                                                                              traitInfo = parentPopulation$traitInfo,
                                                                              inds = newIndsEMBV,
                                                                              verbose = FALSE)
                                                 trueGVMatAll <- newPopEMBV$trueGVMat
                                                 EMBVNow <- apply(X = trueGVMatAll,
                                                                  MARGIN = 2,
                                                                  FUN = function (trueGVEach) {
                                                                    return(tapply(X = trueGVEach,
                                                                                  INDEX = rep(indNamesEMBV, each = nProgeniesEMBV),
                                                                                  FUN = max))
                                                                  })[indNamesEMBV, , drop = FALSE]

                                                 EMBVOneArrayNow <- array(data = EMBVNow,
                                                                          dim = c(dim(EMBVNow), 1),
                                                                          dimnames = c(dimnames(EMBVNow),
                                                                                       list(Iteration = "")))
                                                 rm(trueGVMatAll); rm(newPopEMBV); rm(newIndsEMBV)
                                                 gc(reset = TRUE)

                                                 return(EMBVOneArrayNow)
                                               }, mc.cores = nCoresEMBV)
      } else {
        EMBVArrayList <- parallel::mclapply(X = 1:nIterEMBV,
                                            FUN = function (iterEMBVNo) {
                                              newIndsEMBV <- mapply(private$makeSingleDH,
                                                                    parentPopulation$inds,
                                                                    indNamesEMBV,
                                                                    rep(nProgeniesEMBV, parentPopulation$nInd),
                                                                    USE.NAMES = FALSE)
                                              newIndsEMBV <- unlist(newIndsEMBV)


                                              newPopEMBV <- population$new(name = "ForEMBV",
                                                                           generation = parentPopulation$generation + 1,
                                                                           traitInfo = parentPopulation$traitInfo,
                                                                           inds = newIndsEMBV,
                                                                           verbose = FALSE)
                                              trueGVMatAll <- newPopEMBV$trueGVMat
                                              EMBVNow <- apply(X = trueGVMatAll,
                                                               MARGIN = 2,
                                                               FUN = function (trueGVEach) {
                                                                 return(tapply(X = trueGVEach,
                                                                               INDEX = rep(indNamesEMBV, each = nProgeniesEMBV),
                                                                               FUN = max))
                                                               })[indNamesEMBV, , drop = FALSE]

                                              EMBVOneArrayNow <- array(data = EMBVNow,
                                                                       dim = c(dim(EMBVNow), 1),
                                                                       dimnames = c(dimnames(EMBVNow),
                                                                                    list(Iteration = "")))
                                              rm(trueGVMatAll); rm(newPopEMBV); rm(newIndsEMBV)
                                              gc(reset = TRUE)

                                              return(EMBVOneArrayNow)
                                            }, mc.cores = nCoresEMBV)
      }


      EMBVArray <- do.call(what = abind::abind,
                           args = EMBVArrayList)

      EMBV <- apply(EMBVArray, c(1, 2), mean)
      rownames(EMBV) <- names(parentPopulation$inds)

      self$EMBV <- EMBV

      return(EMBV)
    },




    #' @field randomMate [data.frame] \code{data.frame} of the crossing table by random mating
    randomMate = function() {
      if (is.null(self$selCands)) {
        inds <- names(self$parentPopulation$inds)
      } else {
        inds <- self$selCands
      }
      n <- self$nNextPop
      names <- self$indNames
      seed <- self$seedSimRM

      if (length(names) == 1) {
        names <- .charSeq(paste0(names, "_"), seq(n))
      } else if (length(names) != n) {
        stop('"length(names)" must be equal to "1" or to "n"')
      }

      set.seed(seed = seed)
      crosses0 <- data.frame(
        ind1 = sample(inds, n, replace = TRUE),
        ind2 = sample(inds, n, replace = TRUE)
      )

      return(crosses0)
    },

    #' @field roundRobin [data.frame] \code{data.frame} of the crossing table by round-robin
    roundRobin = function() {
      if (is.null(self$selCands)) {
        selCands <- names(self$parentPopulation$inds)
      } else {
        selCands <- self$selCands
      }
      seed <- self$seedSimRM

      set.seed(seed = seed)
      selCandsRand <- sample(selCands)
      crosses0 <- data.frame(ind1 = selCandsRand,
                             ind2 = c(selCandsRand[-1],
                                      selCandsRand[1]))

      return(crosses0)
    },


    #' @field diallel [data.frame] \code{data.frame} of the crossing table by diallel
    diallel = function() {
      if (is.null(self$selCands)) {
        selCands <- names(self$parentPopulation$inds)
      } else {
        selCands <- self$selCands
      }

      crosses0 <- data.frame(t(combn(x = selCands, m = 2)))
      colnames(crosses0) <- paste0("ind", 1:2)

      return(crosses0)
    },


    #' @field diallelWithSelfing [data.frame] \code{data.frame} of the crossing table by diallel with selfing
    diallelWithSelfing = function() {
      if (is.null(self$selCands)) {
        selCands <- names(self$parentPopulation$inds)
      } else {
        selCands <- self$selCands
      }

      crossesDiallel <- t(combn(x = selCands, m = 2))
      crossesSelfing <- cbind(selCands, selCands)
      crosses0 <- data.frame(rbind(crossesDiallel,
                                   crossesSelfing))
      colnames(crosses0) <- paste0("ind", 1:2)

      return(crosses0)
    },


    #' @field selfing [data.frame] \code{data.frame} of the crossing table by selfing
    selfing = function() {
      if (is.null(self$selCands)) {
        selCands <- names(self$parentPopulation$inds)
      } else {
        selCands <- self$selCands
      }

      crosses0 <- data.frame(cbind(selCands, selCands))
      colnames(crosses0) <- paste0("ind", 1:2)

      return(crosses0)
    },


    #' @field maxGenDist [data.frame] \code{data.frame} of the crossing table by mating pairs whose genetic distance is maximum
    maxGenDist = function() {
      if (is.null(self$genDist)){
        self$computeGenDist()
      }
      distObj <- self$genDist
      genDistMat <- as.matrix(distObj)
      if (is.null(self$selCands)) {
        selCands <- names(self$parentPopulation$inds)
      } else {
        selCands <- self$selCands
      }

      genDistMatSel <- genDistMat[selCands, selCands]
      genSimilSel <- as.dist(1 / genDistMatSel)
      # genDistSel <- - genDistMatSel

      genDistTSP <- TSP::TSP(x = genSimilSel)

      iterSolve <- 100
      minLength <- Inf
      bestTour <- NULL
      for (iterNo in 1:iterSolve) {
        maxDistTour <- TSP::solve_TSP(genDistTSP, method = "nn")
        maxDistTour <- TSP::solve_TSP(genDistTSP, method = "two_opt",
                                      tour = maxDistTour,
                                      two_opt_repetitions = 1000)
        if (TSP::tour_length(maxDistTour) < minLength) {
          # if the obtained length is the shorter than the current best
          minLength <- TSP::tour_length(maxDistTour) # update the shortest path
          bestTour <- maxDistTour
        }
      }

      # show the best crosses
      tmp <- labels(bestTour)
      bestPath <- c(tmp, tmp[1])
      ind1 <- bestPath[-length(bestPath)]
      ind2 <- bestPath[-1]

      crosses0 <- data.frame(ind1 = ind1,
                             ind2 = ind2)

      return(crosses0)
    },


    #' @field makeDH [data.frame] \code{data.frame} of the crossing table by makeDH
    makeDH = function() {
      if (is.null(self$selCands)) {
        selCands <- names(self$parentPopulation$inds)
      } else {
        selCands <- self$selCands
      }

      crosses0 <- data.frame(cbind(selCands, NA))
      colnames(crosses0) <- paste0("ind", 1:2)

      return(crosses0)
    },


    #' @field makeCrosses [list] return a list of new individuals
    makeCrosses = function() {
      if (self$matingMethod != "makeDH") {
        crosses <- self$crosses
        pop <- self$parentPopulation
        seed <- self$seedSimMC

        # checks
        if (!all(colnames(crosses) %in% c("ind1", "ind2", "n", "names"))) {
          stop('colnames(crosses) must be "ind1", "ind2", "n", "names".')
        }
        crosses$ind1 <- as.character(crosses$ind1)
        crosses$ind2 <- as.character(crosses$ind2)
        crosses$names <- as.character(crosses$names)

        # no NA in parnets
        if (any(is.na(crosses$ind1)) || any(is.na(crosses$ind2))) {
          stop('Columns "ind1" and "ind2" should not contain any "NA"')
        }

        # parents in population
        if (any(!crosses$ind1 %in% names(pop$inds))
            || any(!crosses$ind2 %in% names(pop$inds))) {
          notFound <- crosses$ind1[which(!(crosses$ind1 %in% names(pop$inds)))]
          notFound <- c(notFound,
                        crosses$ind2[which(!(crosses$ind2 %in% names(pop$inds)))])
          notFound <- paste(notFound, collapse = '" ; "')
          stop(paste0('Parents not found in the population: "', notFound, '"'))
        }


        # number of offspring
        crosses$n[which(is.na(crosses$n))] <- 1
        if (!is.numeric(crosses$n)) {
          stop(paste0('Column n should be numeric values'))
        }
        if (!all(crosses$n == floor(crosses$n))) {
          stop(paste0('Column n should be integer values'))
        }
        # names
        if (any(is.na(crosses$name))) {
          noNames <- crosses[is.na(crosses$name),]
          noNames$name <- paste(noNames$ind1, "x", noNames$ind2, "-",
                                seq_len(nrow(noNames)), "-")
          crosses[is.na(crosses$name),"names"] <- noNames$name
        }

        set.seed(seed = seed)

        newInds <- mapply(private$makeSingleCross,
                          pop$inds[crosses$ind1],
                          pop$inds[crosses$ind2],
                          crosses$name,
                          crosses$n,
                          USE.NAMES = FALSE)

        newInds <- unlist(newInds)

        # offsprings names do not already exist in population
        if (any(vapply(newInds, function(x){x$name}, "character") %in% names(pop$inds))) {
          nameInPop <-
            unique(crosses$names[which(crosses$names %in% names(pop$inds))])
          nameInPop <- paste(nameInPop, collapse = '" ; "')
          message(paste0(
            'Offspring names already exist in the population: "',
            nameInPop,
            '"'
          ))
        }
      } else {
        newInds <- self$makeDHs
      }
      return(newInds)
    },



    #' @field makeDHs [list] return a list of new double haploids
    makeDHs = function() {
      crosses <- self$crosses
      pop <- self$parentPopulation
      seed <- self$seedSimMC

      # checks
      if (!all(colnames(crosses) %in% c("ind1", "ind2", "n", "names"))) {
        stop('colnames(crosses) must be "ind1", "ind2", "n", "names".')
      }
      crosses$ind1 <- as.character(crosses$ind1)
      crosses$ind2 <- as.character(NA)
      crosses$names <- as.character(crosses$names)

      # no NA in parents1
      if (any(is.na(crosses$ind1))) {
        stop('Columns "ind1" should not contain any "NA"')
      }

      # parents in population
      if (any(!crosses$ind1 %in% names(pop$inds))) {
        notFound <- crosses$ind1[which(!(crosses$ind1 %in% names(pop$inds)))]
        notFound <- paste(notFound, collapse = '" ; "')
        stop(paste0('Parents not found in the population: "', notFound, '"'))
      }


      # number of offspring
      crosses$n[which(is.na(crosses$n))] <- 1
      if (!is.numeric(crosses$n)) {
        stop(paste0('Column n should be numeric values'))
      }
      if (!all(crosses$n == floor(crosses$n))) {
        stop(paste0('Column n should be integer values'))
      }

      # names
      if (any(is.na(crosses$name))) {
        noNames <- crosses[is.na(crosses$name),]
        noNames$name <- paste(noNames$ind1, "x", noNames$ind2, "-",
                              seq_len(nrow(noNames)), "-")
        crosses[is.na(crosses$name),"names"] <- noNames$name
      }

      set.seed(seed = seed)

      newInds <- mapply(private$makeSingleDH,
                        pop$inds[crosses$ind1],
                        crosses$name,
                        crosses$n,
                        USE.NAMES = FALSE)

      newInds <- unlist(newInds)

      # offsprings names do not already exist in population
      if (any(vapply(newInds, function(x){x$name}, "character") %in% names(pop$inds))) {
        nameInPop <-
          unique(crosses$names[which(crosses$names %in% names(pop$inds))])
        nameInPop <- paste(nameInPop, collapse = '" ; "')
        message(paste0(
          'Offspring names already exist in the population: "',
          nameInPop,
          '"'
        ))
      }

      self$crosses <- crosses


      return(newInds)
    }

  ),
  private = list(
    # @description Make double haploid
    #
    # @param ind1 [individual class] parent 1
    # @param names [character] names of the descendants
    # @param n [numeric] number of descendants
    makeSingleDH = function(ind1, names, n = 1){

      #  Names
      if (is.null(names) || is.na(names)) {
        stop('"names" should be provided')
      }

      if (length(names) != n) {
        if (length(names) == 1) {
          names <- paste0(names, "-", c(1:n))
        } else {
          stop('length(names) should either be equal to one or to n')
        }
      }


      gam1 <- ind1$generateGametes(n)

      #  Haplotype
      haplo <- sapply(X = gam1, FUN = function (g1) {
        rbind(g1, g1)
      }, simplify = FALSE)


      newInds <- lapply(c(1:n), function(i){
        individual$new(name = names[[i]],
                       specie = ind1$specie,
                       traitInfo = ind1$traitInfo,
                       parent1 = ind1$name,
                       parent2 = NA,
                       haplo = haplotype$new(ind1$haplo$lociInfo, haplo[[i]]),
                       verbose = FALSE)
      })

      names(newInds) <- names

      return(newInds)
    },



    # @description Cross two individuals together
    #
    # @param ind1 [individual class] parent 1
    # @param ind2 [individual class] parent 2
    # @param names [character] names of the descendants
    # @param n [numeric] number of descendants
    makeSingleCross = function(ind1, ind2, names, n = 1){

      #  Names
      if (is.null(names) || is.na(names)) {
        stop('"names" should be provided')
      }

      if (length(names) != n) {
        if (length(names) == 1) {
          names <- paste0(names, "-", c(1:n))
        } else {
          stop('length(names) should either be equal to one or to n')
        }
      }


      gam1 <- ind1$generateGametes(n)
      gam2 <- ind2$generateGametes(n)

      #  Haplotype
      haplo <- mapply(function(g1, g2){
        rbind(g1, g2)
      }, gam1, gam2,
      SIMPLIFY = FALSE)

      newInds <- lapply(c(1:n), function(i){
        individual$new(name = names[[i]],
                       specie = ind1$specie,
                       traitInfo = ind1$traitInfo,
                       parent1 = ind1$name,
                       parent2 = ind2$name,
                       haplo = haplotype$new(ind1$haplo$lociInfo, haplo[[i]]),
                       verbose = FALSE)
      })

      names(newInds) <- names

      return(newInds)
    },


    # @description Extract top n individuals from vector v
    # @param v [numeric] Numeric vector
    # @param n [numeric] Number of top individuals to be extracted
    extractTopN = function (v, n, decreasing = TRUE) {
      return(sort(x = v, decreasing = decreasing)[1:n])
    },


    # @description computeGVP [matrix] matrix of the genetic variance of progenies
    # @param crosses0 [data.frame] data.frame with crossing instructions: parents names
    # \code{ind1} \code{ind2}
    computeGVP = function (crosses0) {
      parentPopulation <- self$parentPopulation
      genoMat <- parentPopulation$genoMat
      lociEffects <- self$lociEffects
      lociEffectsSquare <- (lociEffects ^ 2)[-1, , drop = FALSE]

      genVarProgenies <- t(apply(X = crosses0,
                                 MARGIN = 1,
                                 FUN = function (eachPair) {
                                   diffGenoMat <- genoMat[eachPair[1], ] - genoMat[eachPair[2], ]
                                   genVarEachMrk <- abs(diffGenoMat)
                                   genVarEachMrk[genVarEachMrk == 1] <- 1 / 4
                                   genVarEachMrk[genVarEachMrk == 2] <- 1 / 2

                                   genVarProgeniesEachPair <- crossprod(lociEffectsSquare,
                                                                        as.matrix(genVarEachMrk))

                                   return(genVarProgeniesEachPair)
                                 }))
      rownames(genVarProgenies) <- 1:nrow(crosses0)
      colnames(genVarProgenies) <- colnames(lociEffects)

      genVarProgeniesScaled <- scale(genVarProgenies)

      return(genVarProgeniesScaled)
    },

    # @description computeOPV [matrix] matrix of the optimal population value
    # @param indNamesCands [character] vector of individual names of selection candidates
    computeOPV = function (indNamesCands) {
      if (is.null(self$haploEffMaxArray)) {
        parentPopulation <- self$parentPopulation
        lociEffects <- self$lociEffects
        haploArray <- parentPopulation$haploArray
        nMrkInBlock <- self$nMrkInBlock
        minimumSegmentLength <- self$minimumSegmentLength
        blockSplitMethod <- self$blockSplitMethod
        genoMap <- parentPopulation$traitInfo$lociInfo$genoMap

        lociEffectsWOIntercept <- lociEffects[-1, , drop = FALSE]

        chr <- genoMap$chr
        chrUnique <- unique(chr)

        if (blockSplitMethod == "nMrkInBlock") {
          blockRangesList <- sapply(chrUnique,
                                    function(eachChr) {
                                      lociNos <- (1:nrow(genoMap))[chr %in% eachChr]
                                      splitNos <- split(lociNos, (lociNos - (lociNos[1] %% nMrkInBlock)) %/% nMrkInBlock)

                                      return(splitNos)
                                    }, simplify = FALSE)
        } else {
          lChr <- parentPopulation$specie$lChr
          nSegmentPerChr <- floor(lChr / minimumSegmentLength)
          lSegment <- lChr / nSegmentPerChr

          blockRangesList <- sapply(chrUnique,
                                    function(eachChr) {
                                      genoPosEachChr <- genoMap$pos[genoMap$chr == eachChr]
                                      lociNos <- (1:nrow(genoMap))[chr %in% eachChr]
                                      lSegmentsSplit <- cumsum(c(0, rep(lSegment[eachChr],
                                                                        nSegmentPerChr[eachChr])))
                                      blockOfEachPos <- cut(x = genoPosEachChr,
                                                            breaks = lSegmentsSplit)

                                      splitNos <- split(x = lociNos, f = blockOfEachPos)

                                      return(splitNos)
                                    }, simplify = FALSE)
        }

        blockRanges <- do.call(what = c,
                               args = blockRangesList)

        haploEffMaxList <- lapply(X = blockRanges,
                                  FUN = function (blockRangeNow) {
                                    haploArrayNow <- haploArray[, blockRangeNow, ]
                                    haploArray_1 <- haploArrayNow[, , 1]
                                    haploArray_2 <- haploArrayNow[, , 2]
                                    haploEff_1 <- haploArray_1 %*% lociEffectsWOIntercept[blockRangeNow, , drop = FALSE]
                                    haploEff_2 <- haploArray_2 %*% lociEffectsWOIntercept[blockRangeNow, , drop = FALSE]

                                    haploEffMax <- pmax(haploEff_1, haploEff_2)

                                    haploEffMaxOneArray <- array(data = haploEffMax,
                                                                 dim = c(dim(haploEffMax), 1),
                                                                 dimnames = c(dimnames(haploEffMax),
                                                                              list(Block = "")))

                                    return(haploEffMaxOneArray)
                                  })
        haploEffMaxArray <- do.call(what = abind::abind,
                                    args = haploEffMaxList)

        self$haploEffMaxArray <- haploEffMaxArray
      } else {
        haploEffMaxArray <- self$haploEffMaxArray
      }


      haploEffMaxArrayCands <- haploEffMaxArray[indNamesCands, , , drop = FALSE]

      OPV <- apply(X = apply(X = haploEffMaxArrayCands,
                             MARGIN = c(2, 3),
                             FUN = max),
                   MARGIN = 1,
                   FUN = sum)

      return(OPV)
    }

  )
)


#' Cross two individuals together
#'
#' @param ind1 parent 1
#' @param ind2 parent 2
#' @param names names of the descendants
#' @param n number of descendants
#' @param verbose print informations
#'
#' @return list of new individuals
#' @export
#'
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
#'                      chrNames = c("C1", "C2", "C3"),
#'                      nLoci = c(3, 4, 5),
#'                      recombRateOneVal = FALSE,
#'                      effPopSize = 3,
#'                      simInfo = mySimInfo,
#'                      verbose = TRUE)
#'
#' ### create lociInfo object
#' myLoci <- lociInfo$new(genoMap = NULL, specie = mySpec)
#'
#' ### simulate haplotype
#' rawHaplo1 <- matrix(sample(c(0, 1), (3 + 4 + 5) * 2, replace = TRUE),
#'                     nrow = 2)
#' colnames(rawHaplo1) <- paste0("Locus_", 1:(3 + 4 + 5))
#'
#' myHaplo1 <- myBreedSimulatR::haplotype$new(lociInfo = myLoci,
#'                                            haplo = rawHaplo1)
#'
#' rawHaplo2 <- matrix(sample(c(0, 1), (3 + 4 + 5) * 2, replace = TRUE),
#'                     nrow = 2)
#' colnames(rawHaplo2) <- paste0("Locus_", 1:(3 + 4 + 5))
#'
#' myHaplo2 <- myBreedSimulatR::haplotype$new(lociInfo = myLoci,
#'                                            haplo = rawHaplo2)
#'
#' ### create individuals:
#' myInd1 <-  individual$new(name = "Ind 1",
#'                           specie = mySpec,
#'                           parent1 = "OkaaSan1",
#'                           parent2 = "OtouSan1",
#'                           haplo = myHaplo1,
#'                           verbose = TRUE)
#'
#' myInd2 <-  individual$new(name = "Ind 2",
#'                           specie = mySpec,
#'                           parent1 = "OkaaSan2",
#'                           parent2 = "OtouSan2",
#'                           haplo = myHaplo2,
#'                           verbose = TRUE)
#' offspring <- makeSingleCross(myInd1, myInd2, names = "off 1")
#' offspring
makeSingleCross <- function(ind1, ind2, names, n = 1, verbose = TRUE){

  #  Names
  if (is.null(names) || is.na(names)) {
    stop('"names" should be provided')
  }

  if (length(names) != n) {
    if (length(names) == 1) {
      names <- paste0(names, "-", c(1:n))
    } else {
      stop('length(names) should either be equal to one or to n')
    }
  }


  gam1 <- ind1$generateGametes(n)
  gam2 <- ind2$generateGametes(n)

  #  Haplotype
  haplo <- mapply(function(g1, g2){
    rbind(g1, g2)
  }, gam1, gam2,
  SIMPLIFY = FALSE)

  newInds <- lapply(c(1:n), function(i){
    individual$new(name = names[[i]],
                   specie = ind1$specie,
                   traitInfo = ind1$traitInfo,
                   parent1 = ind1$name,
                   parent2 = ind2$name,
                   haplo = haplotype$new(ind1$haplo$lociInfo, haplo[[i]]),
                   verbose = FALSE)
  })

  names(newInds) <- names

  newInds
}




#' Proceed to several crosses
#'
#' @param crosses data.frame with crossing instructions: parents names
#' \code{ind1} \code{ind2}, number of descendant \code{n} and names of
#' descendant \code{names}
#' @param pop list of individuals containing the parents
#' @param seed Random seed for make crosses
#'
#' @return list of new individuals
#' @export
#'
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
#'                      chrNames = c("C1", "C2", "C3"),
#'                      nLoci = c(3, 4, 5),
#'                      recombRateOneVal = FALSE,
#'                      effPopSize = 3,
#'                      simInfo = mySimInfo,
#'                      verbose = TRUE)
#'
#' ### create lociInfo object
#' myLoci <- lociInfo$new(genoMap = NULL, specie = mySpec)
#'
#' ### simulate haplotype
#' rawHaplo1 <- matrix(sample(c(0, 1), (3 + 4 + 5) * 2, replace = TRUE),
#'                     nrow = 2)
#' colnames(rawHaplo1) <- paste0("Locus_", 1:(3 + 4 + 5))
#'
#' myHaplo1 <- myBreedSimulatR::haplotype$new(lociInfo = myLoci,
#'                                            haplo = rawHaplo1)
#'
#' rawHaplo2 <- matrix(sample(c(0, 1), (3 + 4 + 5) * 2, replace = TRUE),
#'                     nrow = 2)
#' colnames(rawHaplo2) <- paste0("Locus_", 1:(3 + 4 + 5))
#'
#' myHaplo2 <- myBreedSimulatR::haplotype$new(lociInfo = myLoci,
#'                                            haplo = rawHaplo2)
#'
#' ### create individuals:
#' myInd1 <-  individual$new(name = "Ind 1",
#'                           specie = mySpec,
#'                           parent1 = "OkaaSan1",
#'                           parent2 = "OtouSan1",
#'                           haplo = myHaplo1,
#'                           verbose = TRUE)
#'
#' myInd2 <-  individual$new(name = "Ind 2",
#'                           specie = mySpec,
#'                           parent1 = "OkaaSan2",
#'                           parent2 = "OtouSan2",
#'                           haplo = myHaplo2,
#'                           verbose = TRUE)
#'
#' myInd3 <-  individual$new(name = "Ind 3",
#'                           specie = mySpec,
#'                           parent1 = "OkaaSan1",
#'                           parent2 = "OtouSan1",
#'                           haplo = myHaplo1,
#'                           verbose = TRUE)
#'
#' ### crate population
#' myPop <- population$new(name = "My Population 1",
#'                         generation = 1,
#'                         inds = list(myInd1, myInd2, myInd3),
#'                         verbose = FALSE)
#'
#' ### make crosses
#' crossToDo <- data.frame(ind1 = c("Ind 1", "Ind 1", "Ind 2"),
#'                         ind2 = c("Ind 2", "Ind 3", "Ind 3"),
#'                         n = c(1, 1, 2),
#'                         names = c("Off 1-2", "Off 1-3", "Off 2-3"))
#' makeCrosses(crossToDo, myPop)
makeCrosses <- function(crosses, pop, seed = NULL){

  # checks
  if (!all(colnames(crosses) %in% c("ind1", "ind2", "n", "names"))) {
    stop('colnames(crosses) must be "ind1", "ind2", "n", "names".')
  }
  crosses$ind1 <- as.character(crosses$ind1)
  crosses$ind2 <- as.character(crosses$ind2)
  crosses$names <- as.character(crosses$names)

  # no NA in parnets
  if (any(is.na(crosses$ind1)) || any(is.na(crosses$ind2))) {
    stop('Columns "ind1" and "ind2" should not contain any "NA"')
  }

  # parents in population
  if (any(!crosses$ind1 %in% names(pop$inds))
      || any(!crosses$ind2 %in% names(pop$inds))) {
    notFound <- crosses$ind1[which(! crosses$ind1 %in% names(pop$inds))]
    notFound <- c(notFound,
                  crosses$ind2[which(! crosses$ind2 %in% names(pop$inds))])
    notFound <- paste(notFound, collapse = '" ; "')
    stop(paste0('Parents not found in the population: "', notFound, '"'))
  }


  # number of offspring
  crosses$n[which(is.na(crosses$n))] <- 1
  if (!is.numeric(crosses$n)) {
    stop(paste0('Column n should be numeric values'))
  }
  if (!all(crosses$n == floor(crosses$n))) {
    stop(paste0('Column n should be integer values'))
  }
  # names
  if (any(is.na(crosses$name))) {
    noNames <- crosses[is.na(crosses$name),]
    noNames$name <- paste(noNames$ind1, "x", noNames$ind2, "-",
                          seq_len(nrow(noNames)), "-")
    crosses[is.na(crosses$name),"names"] <- noNames$name
  }

  set.seed(seed = seed)

  newInds <- mapply(makeSingleCross,
                    pop$inds[crosses$ind1],
                    pop$inds[crosses$ind2],
                    crosses$name,
                    crosses$n,
                    USE.NAMES = FALSE)

  newInds <- unlist(newInds)

  # offsprings names do not already exist in population
  if (any(vapply(newInds, function(x){x$name}, "character") %in% names(pop$inds))) {
    nameInPop <-
      unique(crosses$names[which(crosses$names %in% names(pop$inds))])
    nameInPop <- paste(nameInPop, collapse = '" ; "')
    message(paste0(
      'Offspring names already exist in the population: "',
      nameInPop,
      '"'
    ))
  }

  newInds

}
