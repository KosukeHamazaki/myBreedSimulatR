# Author: Kosuke Hamazaki hamazaki@ut-biomet.org
# 2020 The University of Tokyo
#
# Description:
# Definition of bsInfo class




#' R6 Class Representing a Breeding Scheme
#'
#' @description
#' bsInfo object store specific information of one breeding scheme.
#'
# @details
# Details: bsInfo object store specific information of one breeding scheme.
#'
#' @export
#' @import R6
bsInfo <- R6::R6Class(
  "bsInfo",
  public = list(
    #' @field bsName [character] name of this breeding scheme
    bsName = "Undefined",
    #' @field simInfo [simInfo class] Simulation information
    #' (see:\link[myBreedSimulatR]{simInfo})
    simInfo = NULL,
    #' @field specie [specie class] Specie of the SNPs
    #'   (see:\link[myBreedSimulatR]{specie})
    specie = NULL,
    #' @field lociInfo [lociInfo object] information about the individuals haplotypes'
    #'   SNPs (see:\link[breedSimulatR]{lociInfo})
    lociInfo = NULL,
    #' @field traitInfo [traitInfo class] Specific information of traits
    #'   (see:\link[myBreedSimulatR]{traitInfo})
    traitInfo = NULL,
    #' @field founderIsInitPop [logical] Founder haplotype will be regarded as first population or not.
    founderIsInitPop = NULL,
    #' @field seedSimHaplo [numeric] Random seed for selecting haplotype from founder haplotype
    seedSimHaplo = NULL,
    #' @field seedSimRM [numeric] Random seed for mate pairs
    seedSimRM = NULL,
    #' @field seedSimMC [numeric] Random seed for make crosses
    seedSimMC = NULL,
    #' @field popNameBase [character] base of population's name.
    popNameBase = NULL,
    #' @field populations [list] A list of population objects
    populations = NULL,
    #' @field crossInfoList [list] A list of crossInfo objects
    crossInfoList = NULL,
    #' @field generation [list] current generation No. in the breeding scheme
    generation = NULL,
    #' @field herit [numeric] Heritability for each trait (plot-based/line-based)
    herit = NULL,
    #' @field envSpecificEffects [numeric] Effects specific to each environments / treatments.
    #' If `multiTraitsAsEnvs = FALSE`, envSpecificEffects will be 0 for all traits.
    envSpecificEffects = NULL,
    #' @field residCor [matrix] Residual correlation between traits
    residCor = NULL,


    #' @description Create a new bsInfo object.
    #' @param bsName [character] Name of this breeding scheme
    #' @param simInfo [simInfo class] Simulation information
    #' (see:\link[myBreedSimulatR]{simInfo})
    #' @param specie [specie class] Specie of the SNPs
    #'   (see:\link[myBreedSimulatR]{specie})
    #' @param lociInfo [lociInfo object] information about the individuals haplotypes'
    #'   SNPs (see:\link[breedSimulatR]{lociInfo})
    #' @param traitInfo [traitInfo class] Specific information of traits
    #'   (see:\link[myBreedSimulatR]{traitInfo})
    #' @param geno [data.frame / matrix] genotype of the individuals encoded in allele dose.
    #' If you use real data, you must specify `geno` or `haplo` argument.
    #' @param haplo [array] haplotype of the individuals scored with 0 and 1 (3-dimensional array).
    #' @param founderIsInitPop [logical] Founder haplotype will be regarded as first population or not.
    #' @param seedSimHaplo [numeric] Random seed for selecting haplotype from founder haplotype
    #' @param seedSimRM [numeric] Random seed for mate pairs
    #' @param seedSimMC [numeric] Random seed for make crosses
    #' @param popNameBase [character] base of population's name.
    #' @param initIndNames [character] NULL or character string vector specifying the individuals
    #'   names for initial population. If NULL, \code{rownames(geno)} will be used.
    #' @param herit [numeric] Heritability for each trait (plot-based/line-based)
    #' @param envSpecificEffects [numeric] Effects specific to each environments / treatments.
    #' If `multiTraitsAsEnvs = FALSE`, envSpecificEffects will be 0 for all traits.
    #' @param residCor [matrix] Residual correlation between traits
    #' @param verbose [logical] Display info (optional)
    #' @return A new `bsInfo` object.
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


    initialize = function(bsName = "Undefined",
                          simInfo,
                          specie,
                          lociInfo,
                          traitInfo,
                          geno = NULL,
                          haplo = NULL,
                          founderIsInitPop = TRUE,
                          seedSimHaplo = NA,
                          seedSimRM = NA,
                          seedSimMC = NA,
                          popNameBase = "Population",
                          initIndNames = NULL,
                          herit = NULL,
                          envSpecificEffects = NULL,
                          residCor = NULL,
                          verbose = TRUE) {

      # simInfo class
      if (class(simInfo)[1] != "simInfo") {
        stop(paste('class(simInfo)[1] != "simInfo"\n"simInfo" must be a',
                   'simInfo object see: ?simInfo'))
      }

      # specie class
      if (class(specie)[1] != "Specie") {
        stop(paste('class(specie)[1] != "Specie"\n"specie" must be a',
                   'Specie object see: ?specie'))
      }

      # lociInfo class
      if (class(lociInfo)[1] != "lociInfo") {
        stop(paste('class(lociInfo)[1] != "lociInfo"\n"lociInfo" must be a',
                   'lociInfo object see: ?lociInfo'))
      }

      # traitInfo class
      if (class(traitInfo)[1] != "traitInfo") {
        stop(paste('class(traitInfo)[1] != "traitInfo"\n"traitInfo" must be a',
                   'traitInfo object see: ?traitInfo'))
      }


      ploidy <- lociInfo$specie$ploidy

      # seedSimHaplo
      if (!is.null(seedSimHaplo)) {
        if (!is.na(seedSimHaplo)) {
          stopifnot(is.numeric(seedSimHaplo))
          seedSimHaplo <- floor(seedSimHaplo)
        } else {
          seedSimHaplo <- sample(x = 1e9, size = 1)
        }
      }

      if (!lociInfo$specie$simInfo$simGeno) {
        if (is.null(geno) & is.null(haplo)) {
          stop("If you use real data, you must specify `geno` or `haplo` argument!")
        }

        if (is.null(geno)) {
          if (is.array(haplo)) {
            if (length(dim(haplo)) == 3) {
              geno <- apply(haplo, c(1, 2), sum)
            } else {
              stop("`haplo` object must be 3-dimensional array!")
            }
          } else {
            stop("`haplo` object must be 3-dimensional array!")
          }
        }

        if (is.null(haplo)) {
          if (is.data.frame(geno)) {
            geno <- as.matrix(geno)
          }

          if (is.array(geno)) {
            if (length(dim(geno)) == 2) {
              haplo <- array(NA, dim = c(dim(geno), ploidy),
                             dimnames = list(indNames = rownames(geno),
                                             lociNames = colnames(geno),
                                             ploidy = paste0("ploidy_", 1:ploidy)))

              for (i in 1:ploidy) {
                haplo[, , i] <- as.matrix(geno) / ploidy
              }
              message('All individuals are regarded as homozygotes.')
            } else {
              stop("`geno` object must be 2-dimensional array (= matrix) or data.frame!")
            }
          } else {
            stop("`geno` object must be 2-dimensional array (= matrix) or data.frame!")
          }
        }

        stopifnot(is.data.frame(geno) | is.array(geno))
        stopifnot(is.array(haplo))
        stopifnot(length(dim(geno)) == 2)
        stopifnot(length(dim(haplo)) == 3)
        stopifnot(all(dim(geno) == dim(haplo)[1:2]))
        stopifnot(dim(haplo)[3] == 2)
      } else {
        if (is.null(geno) & is.null(haplo)) {
          if (verbose) {
            cat("Create population: Initialize marker genotype from simulated founder haplotypes...\n")
          }
          founderHaplo <- lociInfo$founderHaplo
          set.seed(seed = seedSimHaplo)
          haploPair <- matrix(sample(1:nrow(founderHaplo), nrow(founderHaplo), replace = TRUE), ncol = 2)
          haplo <- sapply(1:(nrow(founderHaplo) / ploidy),
                          function(x) {
                            return(founderHaplo[haploPair[x, ], ])
                          },
                          simplify = FALSE)
          haplo <- do.call(cbind, haplo)
          dim(haplo) <- c(ploidy, ncol(founderHaplo), nrow(founderHaplo) / ploidy)
          haplo <- aperm(haplo, perm = c(3, 2, 1))
          if (founderIsInitPop) {
            indNamesNow <- .charSeq(paste0("G", 1, "_"), seq(nrow(haplo)))
          } else {
            indNamesNow <- .charSeq(paste0("G", 0, "_"), seq(nrow(haplo)))
          }
          dimnames(haplo) <- list(indNames = indNamesNow,
                                  lociNames = colnames(founderHaplo),
                                  ploidy = paste0("ploidy_", 1:ploidy))

          geno <- apply(haplo, c(1, 2), sum)
        } else if (is.null(geno)) {
          if (is.array(haplo)) {
            if (length(dim(haplo)) == 3) {
              geno <- apply(haplo, c(1, 2), sum)
            } else {
              stop("`haplo` object must be 3-dimensional array!")
            }
          } else {
            stop("`haplo` object must be 3-dimensional array!")
          }

          geno <- apply(haplo, c(1, 2), sum)
        } else if (is.null(haplo)) {
          if (is.data.frame(geno)) {
            geno <- as.matrix(geno)
          }

          if (is.array(geno)) {
            if (length(dim(geno)) == 2) {
              haplo <- array(NA, dim = c(dim(geno), ploidy),
                             dimnames = list(indNames = rownames(geno),
                                             lociNames = colnames(geno),
                                             ploidy = paste0("ploidy_", 1:ploidy)))

              for (i in 1:ploidy) {
                haplo[, , i] <- as.matrix(geno) / ploidy
              }
              message('All individuals are regarded as homozygotes.')
            } else {
              stop("`geno` object must be 2-dimensional array (= matrix) or data.frame!")
            }
          } else {
            stop("`geno` object must be 2-dimensional array (= matrix) or data.frame!")
          }
        } else {
          stopifnot(sum(abs(geno - apply(haplo, c(1, 2), sum))) == 0)
        }

        stopifnot(is.data.frame(geno) | is.array(geno))
        stopifnot(is.array(haplo))
        stopifnot(length(dim(geno)) == 2)
        stopifnot(length(dim(haplo)) == 3)
        stopifnot(all(dim(geno) == dim(haplo)[1:2]))
        stopifnot(dim(haplo)[3] == 2)
      }
      if (any(!colnames(geno) %in% lociInfo$genoMap$lociNames)) {
        stop('Some markers of "geno" are not in "lociInfo"')
      }

      if (any(!lociInfo$genoMap$lociNames %in% colnames(geno))) {
        warning('Some markers of "lociInfo" are not in "geno"')
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


      populations <- list()
      initPopName <- paste0(popNameBase, "_", 1)
      initialPopulation <- createPop(geno = geno,
                                     haplo = haplo,
                                     lociInfo = lociInfo,
                                     traitInfo = traitInfo,
                                     founderIsInitPop = founderIsInitPop,
                                     seedSimRM = seedSimRM,
                                     seedSimMC = seedSimMC,
                                     indNames = initIndNames,
                                     popName = initPopName,
                                     verbose = verbose)

      populations[[initPopName]] <- initialPopulation


      self$bsName <- bsName
      self$simInfo <- simInfo
      self$specie <- specie
      self$lociInfo <- lociInfo
      self$traitInfo <- traitInfo
      self$populations <- populations
      self$crossInfoList <- list()
      self$popNameBase <- popNameBase
      self$founderIsInitPop <- founderIsInitPop
      self$seedSimHaplo <- seedSimHaplo
      self$seedSimRM <- seedSimRM
      self$seedSimMC <- seedSimMC
      self$generation <- 1
      self$herit <- herit
      self$envSpecificEffects <- envSpecificEffects
      self$residCor <- residCor
    },



    #' @description
    #' create next population
    #' @param crossInfo [crossInfo class] Information of crossing (including selection scheme)
    #' (see:\link[myBreedSimulatR]{crossInfo})
    nextGeneration = function(crossInfo) {
      populations <- self$populations
      generation <- self$generation
      crossInfoList <- self$crossInfoList

      crossInfoName <- paste0(generation, "_to_", generation + 1)

      generation <- generation + 1
      newPopName <- paste0(self$popNameBase, "_", generation)
      newPop <- population$new(name = newPopName,
                               generation = generation,
                               traitInfo = self$traitInfo,
                               crossInfo = crossInfo,
                               verbose = FALSE)
      populations[[newPopName]] <- newPop
      crossInfoList[[crossInfoName]] <- crossInfo

      self$generation <- generation
      self$populations <- populations
      self$crossInfoList <- crossInfoList
    },


    #' @description
    #' remove latest population
    removeLatestPop = function() {
      populations <- self$populations
      generation <- self$generation
      crossInfoList <- self$crossInfoList

      if (length(populations) == 1) {
        warning(paste0("We cannot remove all the populations. ",
                       "Please remain at least 1 population or redefine the initial population with `self$initialize`!"))
      } else {
        populations[[length(populations)]] <- NULL
        crossInfoList[[length(populations) - 1]] <- NULL
        generation <- generation - 1
      }


      self$generation <- generation
      self$populations <- populations
      self$crossInfoList <- crossInfoList
    },


    #' @description
    #' remove initial population
    removeInitialPop = function() {
      populations <- self$populations
      crossInfoList <- self$crossInfoList

      if (length(populations) == 1) {
        warning(paste0("We cannot remove all the populations. ",
                       "Please remain at least 1 population or redefine the initial population with `self$initialize`!"))
      } else {
        populations[[1]] <- NULL
        crossInfoList[[1]] <- NULL
      }


      self$populations <- populations
      self$crossInfoList <- crossInfoList
    },


    #' @description
    #' assemble populations over generation
    #' @param targetPop [character / numeric] population names or No.s you want to assemble.
    #' If NULL, assemble all the populations
    overGeneration = function(targetPop = NULL) {
      populations <- self$populations
      generation <- self$generation

      if (is.null(targetPop)) {
        targetPop <- 1:length(populations)
      }

      targetPop <- targetPop[targetPop %in% (1:length(populations))]
      targetPopulations <- populations[targetPop]

      nTargetPop <- length(targetPop)
      indsOverGeneration <- NULL
      for (i in 1:nTargetPop) {
        indsNow <- targetPopulations[[i]]$inds
        indsOverGeneration <- c(indsOverGeneration, indsNow)
      }

      targetPopName <- names(targetPopulations)
      targetPopNo <- unlist(lapply(stringr::str_split(string = targetPopName,
                                                      pattern = "_"),
                                   function(x) as.numeric(x[2])))
      popOGName <- paste0(self$popNameBase, "_",
                          paste(targetPopNo, collapse = "_"))

      popOverGeneration <- population$new(name = popOGName,
                                          generation = NA,
                                          traitInfo = self$traitInfo,
                                          inds = indsOverGeneration,
                                          verbose = FALSE)

      return(popOverGeneration)
    },


    #' @description
    #' search parents of an individual of interest
    #' @param indName [character] individual name of interest
    parentInd = function(indName) {
      populations <- self$populations
      whichPop <- which(!is.na(unlist(lapply(populations, function (pop) {
        charmatch(x = indName,
                  table = names(pop$inds))
      }))))
      if (length(whichPop) == 1) {
        indInfo <- populations[[whichPop]]$inds[[indName]]
      } else if (length(whichPop) >= 2) {
        indInfo <- populations[[whichPop[1]]]$inds[[indName]]
      } else if (length(whichPop) == 0) {
        stop("There is no individual named '", indName, " ' in this bredding scheme!!")
      }

      parent1 <- indInfo$parent1
      parent2 <- indInfo$parent2

      return(c(Parent_1 = parent1, Parent_2 = parent2))
    },


    #' @description
    #' Display information about the object
    print = function() {
      cat(paste0(
        "Name of Breeding Scheme: ", self$bsName, "\n",
        "Name of Specie: ", self$specie$specName, "\n",
        "Number of Loci: ", self$lociInfo$nLoci(), "\n",
        "Number of Traits: ", self$traitInfo$nTraits, "\n",
        "Current Generation No.: ", self$generation, "\n",
        "Number of individuals for each population: \n"
      ))
      print(unlist(lapply(self$populations, function(x) x$nInd)))
    },




    #' @description Draw figures for visualization of each data (GVs)
    #' @param plotTarget [character] Target of figure, select either one of
    #' "trueAGV", "trueDGV", "trueEGV", "trueGV", "trueAGVET", "trueDGVET", "trueEGVET",
    #'  "trueGVET", "trueAGVCT", "trueDGVCT", "trueEGVCT", "trueGVCT".
    #' @param targetTrait [numeric] Target traits. character is OK, but numeric vector
    #'  corresponding to target traits is preferred.
    #' @param targetPopulation [numeric] Target populations. character is OK, but numeric vector
    #'  corresponding to target traits is preferred.
    #' @param plotType [character] We offer "box", "violin", "jitter", "lines"
    #' to draw figures for genotypic values.
    #' @param facet_grid [logical] When you choose "jitter" for plotType,
    #'  you can select whether you display the plot for wach trait with grid.
    #' @param unitSd [numeric] When you choose "lines", we will show the mean, mean + unitSd * sd,
    #'  mean - unitSd * sd.
    #' @param colorVecBase [character] vector representing color of the line for traits in "lines" option
    #' @param widthVecBase [numeric] vector representing width of the line in "lines" option
    #' @param dashVecBase [character]vector representing solid / dash in "lines" option
    #'
    plot = function (plotTarget = "trueGV",
                     targetTrait = 1:self$traitInfo$nTraits,
                     targetPopulation = 1:self$generation,
                     plotType = "box",
                     facet_grid = FALSE,
                     unitSd = 1,
                     colorVecBase = c("black", "red", "green", "blue",
                                      "lightblue", "purple", "yellow",
                                      "gray", "pink", "orange"),
                     widthVecBase = c(1.5, 0.8, 0.8),
                     dashVecBase = c("solid", "dash", "dash")) {
      supportTargets <- c("trueAGV", "trueDGV", "trueEGV", "trueGV",
                          "trueAGVET", "trueDGVET", "trueEGVET", "trueGVET",
                          "trueAGVCT", "trueDGVCT", "trueEGVCT", "trueGVCT")
      GVSeries <- c("trueAGV", "trueDGV", "trueEGV", "trueGV",
                    "trueAGVET", "trueDGVET", "trueEGVET", "trueGVET",
                    "trueAGVCT", "trueDGVCT", "trueEGVCT", "trueGVCT")
      traitNames <- self$traitInfo$traitNames

      if (is.character(targetTrait)) {
        targetTrait <- match(targetTrait, traitNames)
      }

      if (length(plotTarget) == 1) {
        trueGVMatList <- lapply(self$populations[targetPopulation],
                                function (pop) {
                                  round(pop[[paste0(plotTarget, "Mat")]], 5)[, targetTrait, drop = FALSE]
                                })
        trueGVMatNow <- do.call(what = rbind,
                                args = trueGVMatList)

        parentInds <- t(sapply(rownames(trueGVMatNow), self$parentInd))
        trueGVDataFrame <- data.frame(Ind = rep(rownames(trueGVMatNow),
                                                ncol(trueGVMatNow)),
                                      Parent_1 = rep(parentInds[, 1],
                                                     ncol(trueGVMatNow)),
                                      Parent_2 = rep(parentInds[, 2],
                                                     ncol(trueGVMatNow)),
                                      Trait = rep(colnames(trueGVMatNow),
                                                  each = nrow(trueGVMatNow)),
                                      GV = c(trueGVMatNow),
                                      Population = rep(rep(targetPopulation,
                                                           unlist(lapply(self$populations[targetPopulation],
                                                                         function(x) x$nInd))),
                                                       ncol(trueGVMatNow)))

        if (plotType %in% c("box", "violin")) {
          plt <- plot_ly(
            data = trueGVDataFrame,
            x = ~ Population,
            y = ~ GV,
            split = ~ Trait,
            type = plotType,
            hoverinfo = "text",
            # boxpoints = "all",
            # jitter = 0.3,
            # pointpos = -1.8,
            text = paste0(apply(trueGVDataFrame, 1, function(l) {
              paste(names(l), ":", l, collapse = "\n")
            }))
          ) %>%
            plotly::layout(title = list(text = plotTarget),
                           yaxis = list(title = list(text = plotTarget)))
          if (plotType == "box") {
            plt <- plt %>% plotly::layout(boxmode = "group")
          } else if (plotType == "violin") {
            plt <- plt %>% plotly::layout(violinmode = "group")
          }

        } else if (plotType %in% c("jitter")) {
          plt <- ggplot(trueGVDataFrame,
                        aes(x = Population, y = GV, colour = Trait)) +
            geom_jitter(aes(text = paste0(apply(trueGVDataFrame[, 1:3], 1, function(l) {
              paste(names(l), ":", l, collapse = "\n")
            }))), width = 0.25, alpha = 0.6) +
            theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
            labs(title = plotTarget)
          if (facet_grid) {
            plt <- plt + facet_grid(. ~ Trait)
          }

          plt <- ggplotly(plt)

        } else if (plotType %in% c("lines")) {

          meanGV <- do.call(rbind,
                            lapply(trueGVMatList,
                                   function(mat) {
                                     apply(mat, 2, mean)
                                   }))
          sdGV <- do.call(rbind,
                          lapply(trueGVMatList,
                                 function(mat) {
                                   apply(mat, 2, sd)
                                 }))
          upperGV <- meanGV + unitSd * sdGV
          lowerGV <- meanGV - unitSd * sdGV


          trueGVMeanDataFrame <- data.frame(Type = rep(c("Mean", "Upper", "Lower"),
                                                       each = nrow(meanGV) * ncol(meanGV)),
                                            Trait = rep(rep(colnames(meanGV),
                                                            each = nrow(meanGV)), 3),
                                            GV = c(c(meanGV), c(upperGV), c(lowerGV)),
                                            Population = rep(targetPopulation,
                                                             ncol(meanGV) * 3))

          trueGVMeanDataFrame <- data.frame(Population = targetPopulation,
                                            cbind(meanGV, upperGV, lowerGV))
          colnames(trueGVMeanDataFrame)[-1] <-
            c(sapply(c("Mean", "Upper", "Lower"),
                     function(x) {
                       paste0(colnames(meanGV),
                              "_", x)
                     }))

          repColor <- self$traitInfo$nTraits %/% length(colorVecBase) + 1
          colorVecEach <- rep(colorVecBase, repColor)[targetTrait]
          colorVec <- rep(colorVecEach, 3)
          widthVec <- rep(widthVecBase, each = length(targetTrait))
          dashVec <- rep(dashVecBase, each = length(targetTrait))
          nameVec <- colnames(trueGVMeanDataFrame)[-1]

          plt <- plot_ly(
            data = trueGVMeanDataFrame,
            x = ~ Population,
            y = ~ trueGVMeanDataFrame[, 2],
            line = list(color = colorVec[1],
                        width = widthVec[1],
                        dash = dashVec[1]),
            type = "scatter",
            mode = "lines",
            name = nameVec[1]
          ) %>%
            plotly::layout(title = list(text = plotTarget),
                           yaxis = list(title = list(text = plotTarget)))


          for (i in 2:(ncol(trueGVMeanDataFrame) - 1)) {
            plt <- plt %>% add_trace(y = trueGVMeanDataFrame[, i + 1],
                                     line = list(color = colorVec[i],
                                                 width = widthVec[i],
                                                 dash = dashVec[i]),
                                     name = nameVec[i])
          }

        }
      } else {
        if (length(targetTrait) >= 2) {
          stop("When you want to compare the difference among GV types, you can only set 1 trait!!")
        }


        trueGVMatList <- lapply(self$populations[targetPopulation],
                                function (pop) {
                                  sapply(plotTarget, function(plotTargetNow) {
                                    round(pop[[paste0(plotTargetNow, "Mat")]], 5)[, targetTrait]
                                  })
                                })


        trueGVMatNow <- do.call(what = rbind,
                                args = trueGVMatList)

        parentInds <- t(sapply(rownames(trueGVMatNow), self$parentInd))
        trueGVDataFrame <- data.frame(Ind = rep(rownames(trueGVMatNow),
                                                ncol(trueGVMatNow)),
                                      Parent_1 = rep(parentInds[, 1],
                                                     ncol(trueGVMatNow)),
                                      Parent_2 = rep(parentInds[, 2],
                                                     ncol(trueGVMatNow)),
                                      GVType = rep(colnames(trueGVMatNow),
                                                  each = nrow(trueGVMatNow)),
                                      GV = c(trueGVMatNow),
                                      Population = rep(rep(targetPopulation,
                                                           unlist(lapply(self$populations[targetPopulation],
                                                                         function(x) x$nInd))),
                                                       ncol(trueGVMatNow)))




        if (plotType %in% c("box", "violin")) {
          plt <- plot_ly(
            data = trueGVDataFrame,
            x = ~ Population,
            y = ~ GV,
            split = ~ GVType,
            type = plotType,
            hoverinfo = "text",
            # boxpoints = "all",
            # jitter = 0.3,
            # pointpos = -1.8,
            text = paste0(apply(trueGVDataFrame, 1, function(l) {
              paste(names(l), ":", l, collapse = "\n")
            }))
          ) %>%
            plotly::layout(title = list(text = self$traitInfo$traitNames[targetTrait]),
                           yaxis = list(title = "value"))
          if (plotType == "box") {
            plt <- plt %>% plotly::layout(boxmode = "group")
          } else if (plotType == "violin") {
            plt <- plt %>% plotly::layout(violinmode = "group")
          }

        } else if (plotType %in% c("jitter")) {
          plt <- ggplot(trueGVDataFrame,
                        aes(x = Population, y = GV, colour = GVType)) +
            geom_jitter(
              aes(text = paste0(apply(trueGVDataFrame[, 1:3], 1, function(l) {
              paste(names(l), ":", l, collapse = "\n")
            }))),
            width = 0.25, alpha = 0.6) +
            theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
            labs(title = self$traitInfo$traitNames[targetTrait])
          if (facet_grid) {
            plt <- plt + facet_grid(. ~ GVType)
          }

          plt <- ggplotly(plt)

        } else if (plotType %in% c("lines")) {

          meanGV <- do.call(rbind,
                            lapply(trueGVMatList,
                                   function(mat) {
                                     apply(mat, 2, mean)
                                   }))
          sdGV <- do.call(rbind,
                          lapply(trueGVMatList,
                                 function(mat) {
                                   apply(mat, 2, sd)
                                 }))
          upperGV <- meanGV + unitSd * sdGV
          lowerGV <- meanGV - unitSd * sdGV


          trueGVMeanDataFrame <- data.frame(Type = rep(c("Mean", "Upper", "Lower"),
                                                       each = nrow(meanGV) * ncol(meanGV)),
                                            GVType = rep(rep(colnames(meanGV),
                                                            each = nrow(meanGV)), 3),
                                            GV = c(c(meanGV), c(upperGV), c(lowerGV)),
                                            Population = rep(targetPopulation,
                                                             ncol(meanGV) * 3))

          trueGVMeanDataFrame <- data.frame(Population = targetPopulation,
                                            cbind(meanGV, upperGV, lowerGV))
          colnames(trueGVMeanDataFrame)[-1] <-
            c(sapply(c("Mean", "Upper", "Lower"),
                     function(x) {
                       paste0(colnames(meanGV),
                              "_", x)
                     }))

          repColor <- self$GVTypeInfo$nGVTypes %/% length(colorVecBase) + 1
          colorVecEach <- rep(colorVecBase, repColor)[targetGVType]
          colorVec <- rep(colorVecEach, 3)
          widthVec <- rep(widthVecBase, each = length(targetGVType))
          dashVec <- rep(dashVecBase, each = length(targetGVType))
          nameVec <- colnames(trueGVMeanDataFrame)[-1]

          plt <- plot_ly(
            data = trueGVMeanDataFrame,
            x = ~ Population,
            y = ~ trueGVMeanDataFrame[, 2],
            line = list(color = colorVec[1],
                        width = widthVec[1],
                        dash = dashVec[1]),
            type = "scatter",
            mode = "lines",
            name = nameVec[1]
          ) %>%
            plotly::layout(title = list(text = self$traitInfo$traitNames[targetTrait]),
                           yaxis = list(title = "value"))


          for (i in 2:(ncol(trueGVMeanDataFrame) - 1)) {
            plt <- plt %>% add_trace(y = trueGVMeanDataFrame[, i + 1],
                                     line = list(color = colorVec[i],
                                                 width = widthVec[i],
                                                 dash = dashVec[i]),
                                     name = nameVec[i])
          }

        }

      }
      print(plt)
    }
  ),

  active = list(
    #' @field lociEffects [matrix] marker and QTL effects used for crossInfo object
    #'
    lociEffects = function () {
      lociInfo <- self$lociInfo
      traitInfo <- self$traitInfo
      nLoci <- lociInfo$nLoci()
      lociNames <- lociInfo$genoMap$lociNames
      lociNames <- stringr::str_sort(x = lociNames, numeric = TRUE)
      nTraits <- traitInfo$nTraits
      traitNames <- traitInfo$traitNames

      lociEffects <- matrix(data = 0,
                           nrow = nLoci,
                           ncol = nTraits,
                           dimnames = list(lociNames,
                                           traitNames))
      for(traitNo in 1:nTraits) {
        qtlPos <- traitInfo$qtlPos[[traitNo]]
        actionTypeNow <- traitInfo$actionType[[traitNo]]
        lociEffects[qtlPos[actionTypeNow == 0], traitNo] <- (traitInfo$qtlEff[[traitNo]])[actionTypeNow == 0]
      }

      lociEffects <- rbind(Intercept = rep(0, nTraits),
                          lociEffects)


      return(lociEffects)
    }
  )
)
