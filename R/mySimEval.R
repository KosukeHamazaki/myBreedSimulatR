# Author: Kosuke Hamazaki hamazaki@ut-biomet.org
# 2020 The University of Tokyo
#
# Description:
# Definition of simEval class




#' R6 Class Representing a Breeding Scheme
#'
#' @description
#' simEval object store specific information of simulation results of breeding scheme.
#'
# @details
# Details: simEval object store specific information of simulation results of breeding scheme.
#'
#' @export
#' @import R6
simEval <- R6::R6Class(
  "simEval",
  public = list(
    #' @field simEvalName [character] Name of this simulation of breeding schemes
    simEvalName = NULL,
    #' @field simBsList [list] list of simulation results of breeding scheme
    #'   (see:\link[myBreedSimulatR]{simBs}; \link[myBreedSimulatR]{simBsOpt})
    simBsList = NULL,
    #' @field verbose [logical] Display info (optional)
    verbose = NULL,



    #' @description Create a new simEval object.
    #' @param simEvalName [character] Name of this evaluation of simulation results
    #' @param simBsList [list] list of simulation results of breeding scheme
    #'   (see:\link[myBreedSimulatR]{simBs}; \link[myBreedSimulatR]{simBsOpt}; \link[myBreedSimulatR]{simBsCma})
    #' @param verbose [logical] Display info (optional)
    #' @return A new `simEval` object.
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


    initialize = function(simEvalName = "Undefined",
                          simBsList,
                          verbose = TRUE) {

      # simEvalName
      if (is.null(simEvalName)) {
        simEvalName <- "Undefined"
      }
      stopifnot(is.character(simEvalName))

      # simBsList class
      stopifnot(is.list(simBsList))

      simBsListClass <- unlist(lapply(simBsList, function(x) class(x)[[1]]))

      if (!all(simBsListClass %in% c("simBs", "simBsOpt", "simBsCma"))) {
        stop(paste('"simBsList" must be a list of simBs or simBsOpt object see: ?simBs; ?simBsOpt'))
      }

      simBsNames <- unlist(lapply(simBsList, function(x) x$simBsName))
      names(simBsList) <- simBsNames

      # Save arguments in `self`
      self$simEvalName <- simEvalName
      self$simBsList <- simBsList
      self$verbose <- verbose
    },





    #' @description
    #' start simulation & summary results of breeding scheme
    prepareSimRes = function() {
      # Read arguments from `self`
      simEvalName <- self$simEvalName

      self$simBsList <- lapply(X = self$simBsList,
                               FUN = function(simBsEach) {
                                 if (is.null(simBsEach$trueGVMatList)) {
                                   if (is.null(simBsEach$simBsRes[[simBsEach$simBsName]]$all)) {
                                     simBsEach$returnMethod <- unique(c("all",
                                                                        simBsEach$returnMethod))

                                     if (self$verbose) {
                                       print("Start simulation of breeding scheme")
                                     }
                                     simBsEach$startSimulation()
                                   }

                                   # evaluateGVMethodOri <- simBsEach$evaluateGVMethod
                                   # simBsEach$evaluateGVMethod <- "true"

                                   if (self$verbose) {
                                     print("Start summarization of simulation results")
                                   }
                                   simBsEach$summaryResults()
                                   # simBsEach$evaluateGVMethod <- evaluateGVMethodOri
                                 }
                                 return(simBsEach)
                               })

    },




    #' @description
    #' Display information about the object
    print = function() {
      cat(paste0(
        "Name of Evaluation: ", self$simEvalName, "\n",
        "Number of Simulation Methods: ", length(self$simBsList), "\n",
        "Names of Simulation Methods: \n"
      ))
      names(self$simBsList)
    },


    #' @description Draw figures for visualization of simulation results for summary statistics to compare strategies
    #' @param targetTrait [numeric] Target trait. character is OK, but numeric vector
    #'  corresponding to target traits is preferred. It should be a vector with length 1.
    #' @param targetPopulation [numeric] Target populations. character is OK, but numeric vector
    #'  corresponding to target traits is preferred.
    #' @param plotType [character] We offer "box", "violin", "lines", "density"
    #' to draw figures for simulation results.
    #' @param plotTarget [character] You should select which summary statistics will be plotted. It should be a vector with length 1.
    #' @param returnGain [logical] Return genetic gain (difference against initial population) or not
    #' @param plotGVMethod [character] Which type of GV (true GV or estimated GV) will be used for plotting the simulation results
    #' @param adjust [numeric] the bandwidth used is actually adjust*bw. This makes it easy to specify values like ‘half the default’ bandwidth.
    #' (see: `adjust` in \link[stats]{density})
    #'
    plot = function(targetTrait = 1,
                     targetPopulation = NULL,
                     plotType = "box",
                     plotTarget = "max",
                     returnGain = TRUE,
                     plotGVMethod = "true",
                     adjust = 1e-05) {
      lociEffMethodsOffered <- c("true", "estimated")
      simBsList <- self$simBsList

      # targetTrait
      if (is.numeric(targetTrait)) {
        targetTraitName <- simBsList[[1]]$bsInfoInit$traitInfo$traitNames[targetTrait]
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


      if (any(unlist(lapply(simBsList, function(simBsEach) is.null(simBsEach$trueGVSummaryArray))))) {
        self$prepareSimRes()
      }


      # plotTarget
      plotTargetsOffered <- c("all", "summary", "max", "mean", "median", "min", "var")

      stopifnot(length(plotTarget) == 1)
      stopifnot(plotTarget %in% plotTargetsOffered)

      # plotGVMethod
      if (!is.null(plotGVMethod)) {
        if (!(plotGVMethod %in% lociEffMethodsOffered)) {
          stop(paste0("We only offer the following methods for evaluating the simulation results: ",
                      paste(lociEffMethodsOffered, collapse = "; ")))
        }
      } else {
        plotGVMethod <- "true"
      }

      trueGVSummaryDfList <- lapply(X = simBsList,
                                    FUN = function(simBsEach) {
                                      if (plotGVMethod == "true") {
                                        trueGVSummaryArray <- simBsEach$trueGVSummaryArray
                                      } else {
                                        trueGVSummaryArray <- simBsEach$estimatedGVSummaryArray
                                      }

                                      # targetPopulation
                                      if (is.null(targetPopulation)) {
                                        if (plotType %in% c("box", "violin", "lines")) {
                                          targetPopulation <- 1:dim(trueGVSummaryArray)[3]
                                        } else if (plotType == "density") {
                                          targetPopulation <- dim(trueGVSummaryArray)[3]
                                        }
                                      }
                                      targetPopulation <- targetPopulation[targetPopulation %in% (1:dim(trueGVSummaryArray)[3])]
                                      trueGVSummaryArrayPop1 <- trueGVSummaryArray[ , , rep(1, length(targetPopulation)), , drop = FALSE]

                                      trueGVSummaryArray <- trueGVSummaryArray[ , , targetPopulation, , drop = FALSE]
                                      if (returnGain) {
                                        trueGVSummaryArray <- trueGVSummaryArray - trueGVSummaryArrayPop1
                                      }

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

                                      return(trueGVSummaryDf)
                                    })



      trueGVSummaryDfAll <- data.frame(do.call(what = rbind,
                                               args = trueGVSummaryDfList),
                                       Strategy = rep(names(trueGVSummaryDfList),
                                                      lapply(trueGVSummaryDfList, nrow)))
      trueGVSummaryDfAll$Strategy <- factor(trueGVSummaryDfAll$Strategy, levels = names(trueGVSummaryDfList))


      trueGVSummaryDfTrait <- trueGVSummaryDfAll[trueGVSummaryDfAll$Trait %in% targetTraitName, ]
      trueGVSummaryDfTraitSS <- trueGVSummaryDfTrait[trueGVSummaryDfTrait$SummaryStatistics %in% plotTarget, ]
      trueGVSummaryDfTraitSS$Value <- round(trueGVSummaryDfTraitSS$Value, 3)
      if (plotType %in% c("box", "violin")) {
        plt <- plotly::plot_ly(
          data = trueGVSummaryDfTraitSS,
          x = ~ Population,
          y = ~ Value,
          split = ~ Strategy,
          type = plotType,
          hoverinfo = "text",
          # boxpoints = "all",
          # jitter = 0.3,
          # pointpos = -1.8,
          text = paste0(apply(trueGVSummaryDfTraitSS, 1, function(l) {
            paste(names(l), ":", l, collapse = "\n")
          }))
        ) %>%
          plotly::layout(title = list(text = paste0(targetTraitName, "-", plotTarget)),
                         # yaxis = list(title = list(text = plotTarget))
                         yaxis = list(title = list(text = paste0(plotGVMethod, " GV")))
          )
        if (plotType == "box") {
          plt <- plt %>% plotly::layout(boxmode = "group")
        } else if (plotType == "violin") {
          plt <- plt %>% plotly::layout(violinmode = "group")
        }
      } else if (plotType %in% c("lines")) {
        trueGVSummaryMeanDfList <- lapply(X = simBsList,
                                          FUN = function(simBsEach) {
                                            if (plotGVMethod == "true") {
                                              trueGVSummaryArray <- simBsEach$trueGVSummaryArray
                                            } else {
                                              trueGVSummaryArray <- simBsEach$estimatedGVSummaryArray
                                            }

                                            # targetPopulation
                                            if (is.null(targetPopulation)) {
                                              targetPopulation <- 1:dim(trueGVSummaryArray)[3]
                                            }
                                            targetPopulation <- targetPopulation[targetPopulation %in% (1:dim(trueGVSummaryArray)[3])]
                                            trueGVSummaryArrayPop1 <- trueGVSummaryArray[ , , rep(1, length(targetPopulation)), , drop = FALSE]

                                            trueGVSummaryArray <- trueGVSummaryArray[ , , targetPopulation, , drop = FALSE]
                                            if (returnGain) {
                                              trueGVSummaryArray <- trueGVSummaryArray - trueGVSummaryArrayPop1
                                            }

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

                                            return(trueGVSummaryMeanDf)
                                          })



        trueGVSummaryMeanDfAll <- data.frame(do.call(what = rbind,
                                                     args = trueGVSummaryMeanDfList),
                                             Strategy = rep(names(trueGVSummaryMeanDfList),
                                                            lapply(trueGVSummaryMeanDfList, nrow)))
        trueGVSummaryMeanDfAll$Strategy <- factor(trueGVSummaryMeanDfAll$Strategy, levels = names(trueGVSummaryMeanDfList))


        trueGVSummaryMeanDfTrait <- trueGVSummaryMeanDfAll[trueGVSummaryMeanDfAll$Trait %in% targetTraitName, ]
        trueGVSummaryMeanDfTraitSS <- trueGVSummaryMeanDfTrait[trueGVSummaryMeanDfTrait$SummaryStatistics %in% plotTarget, ]
        trueGVSummaryMeanDfTraitSS$Value <- round(trueGVSummaryMeanDfTraitSS$Value, 3)


        plt <- plot_ly(
          data = trueGVSummaryMeanDfTraitSS,
          x = ~ Population,
          y = ~ Value,
          split = ~ Strategy,
          # line = list(color = colorVec[1],
          #             width = widthVec[1],
          #             dash = dashVec[1]),
          type = "scatter",
          mode = "markers+lines",
          hoverinfo = "text",
          text = paste0(apply(trueGVSummaryMeanDfTraitSS, 1, function(l) {
            paste(names(l), ":", l, collapse = "\n")
          }))
          # name = nameVec[1]
        ) %>%
          plotly::layout(title = list(text = paste0(targetTraitName, "-", plotTarget)),
                         yaxis = list(title = list(text = paste0(plotGVMethod, " GV"))))
      } else if (plotType == "density") {
        densityValueDfList <- lapply(X = names(trueGVSummaryDfList),
                                     FUN = function(strategyName) {
                                       trueGVSummaryDfTraitSSEachStr <- trueGVSummaryDfTraitSS[trueGVSummaryDfTraitSS$Strategy %in% strategyName, ]
                                       densityResEachStr <- density(x = sort(trueGVSummaryDfTraitSSEachStr$Value), adjust = adjust)

                                       x <- densityResEachStr$x
                                       x <- c(min(x), x)
                                       y <- cumsum(densityResEachStr$y / sum(densityResEachStr$y))
                                       y <- c(0, y)

                                       return(data.frame(x, y))
                                     })
        densityValueDf <- do.call(what = rbind,
                                  args = densityValueDfList)
        densityValueDf$Strategy <- rep(names(trueGVSummaryDfList), unlist(lapply(densityValueDfList, nrow)))
        densityValueDf$Strategy <- factor(densityValueDf$Strategy, levels = names(trueGVSummaryDfList))

        plt <- plot_ly(
          data = densityValueDf,
          x = ~ x,
          y = ~ y,
          split = ~ Strategy,
          # line = list(color = colorVec[1],
          #             width = widthVec[1],
          #             dash = dashVec[1]),
          type = "scatter",
          mode = "lines"
          # name = nameVec[1]
        ) %>%
          plotly::layout(title = list(text = paste0(targetTraitName, "-", plotTarget,
                                                    "-", unique(trueGVSummaryDfList[[1]]$Population))),
                         xaxis = list(title = list(text = paste0(plotGVMethod, " GV"))))
      }


      return(plt)
    }
  )
)
