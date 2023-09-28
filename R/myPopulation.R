# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# Definition of Population class



#' R6 class representing a population
#'
#' @description
#' population object store specific information about multiple individuals
#'
#'
#' @export
#' @import R6
population <- R6::R6Class(
  "population",
  lock_objects = FALSE,
  public = list(
    #' @field name [string] Name of the population
    name = NULL,
    #' @field generation [integer] Generation of the population
    generation = NULL,
    #' @field specie [specie class] Specie of the SNPs
    #'   (see:\link[breedSimulatR]{specie})
    specie = NULL,
    #' @field traitInfo [traitInfo class] Specific information of traits
    #'   (see:\link[myBreedSimulatR]{traitInfo})
    traitInfo = NULL,
    #' @field crossInfo [crossInfo class] Information of crossing (including selection scheme)
    #'   (see:\link[breedSimulatR]{crossInfo})
    crossInfo = NULL,
    #' @field inds [list] list of population's individuals
    inds = list(),
    #' @field trialInfo [trialInfo class] Specific information of field trial
    trialInfo = NULL,
    #' @field phenotypicValues [array] individual x traits x replication (3-dimensional array) of phenotypic values
    phenotypicValues = NULL,
    #' @field verbose [boolean] display information
    verbose = NULL,

    #' @description Create a new population object.
    #' @param name [string] name of the population
    #' @param generation [integer] Generation of the population
    #' @param inds [individual class or list] list of individuals of the
    #'     population (see:\link[breedSimulatR]{individual})
    #' @param traitInfo [traitInfo class] Specific information of traits
    #'   (see:\link[myBreedSimulatR]{traitInfo})
    #' @param crossInfo [crossInfo class] Information of crossing (including selection scheme)
    #' (see:\link[myBreedSimulatR]{crossInfo})
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
    #
    #' ### create specie information
    #' mySpec <- specie$new(nChr = 3,
    #'                      lChr = c(100, 150, 200),
    #'                      specName = "Example 1",
    #'                      ploidy = 2,
    #'                      mutRate = 10^-8,
    #'                      recombRate = 10^-6,
    #'                      chrNames = c("C1", "C2", "C3"),
    #'                      nLoci = c(3, 4, 5),
    #'                      effPopSize = 3,
    #'                      simInfo = mySimInfo,
    #'                      verbose = TRUE)
    #
    #' ### create lociInfo object
    #' myLoci <- lociInfo$new(genoMap = NULL, specie = mySpec)
    #
    #' ### create traitInfo object
    #' myTrait <- traitInfo$new(lociInfo = myLoci,
    #'                          nMarkers = c(2, 4, 3),
    #'                          nTraits = 2,
    #'                          nQTLs = matrix(c(1, 0, 2,
    #'                                           1, 0, 1),
    #'                                         nrow = 2,
    #'                                         byrow = TRUE),
    #'                          actionTypeEpiSimple = TRUE,
    #'                          qtlOverlap = TRUE,
    #'                          nOverlap = c(1, 0, 1),
    #'                          effCor = 0.6,
    #'                          propDomi = 0.2,
    #'                          interactionMean = c(1, 0))
    #' plot(myTrait, alphaMarker = 0.4)
    #
    #
    #' ### simulate haplotype
    #' rawHaplo1 <- matrix(sample(c(0, 1), (3 + 4 + 5) * 2, replace = TRUE),
    #'                     nrow = 2)
    #' colnames(rawHaplo1) <- paste0("Locus_", 1:(3 + 4 + 5))
    #
    #' myHaplo1 <- myBreedSimulatR::haplotype$new(lociInfo = myLoci,
    #'                                            haplo = rawHaplo1)
    #
    #' rawHaplo2 <- matrix(sample(c(0, 1), (3 + 4 + 5) * 2, replace = TRUE),
    #'                     nrow = 2)
    #' colnames(rawHaplo2) <- paste0("Locus_", 1:(3 + 4 + 5))
    #
    #' myHaplo2 <- myBreedSimulatR::haplotype$new(lociInfo = myLoci,
    #'                                            haplo = rawHaplo2)
    #
    #' ### create individuals:
    #' myInd1 <-  individual$new(name = "Ind 1",
    #'                           specie = myTrait$lociInfo$specie,
    #'                           traitInfo = myTrait,
    #'                           parent1 = "OkaaSan1",
    #'                           parent2 = "OtouSan1",
    #'                           haplo = myHaplo1,
    #'                           verbose = TRUE)
    #
    #' myInd2 <-  individual$new(name = "Ind 2",
    #'                           specie = myTrait$lociInfo$specie,
    #'                           traitInfo = myTrait,
    #'                           parent1 = "OkaaSan2",
    #'                           parent2 = "OtouSan2",
    #'                           haplo = myHaplo2,
    #'                           verbose = TRUE)
    #
    #' myInd3 <-  individual$new(name = "Ind 3",
    #'                           specie = myTrait$lociInfo$specie,
    #'                           traitInfo = myTrait,
    #'                           parent1 = "OkaaSan1",
    #'                           parent2 = "OtouSan1",
    #'                           haplo = myHaplo1,
    #'                           verbose = TRUE)
    #
    #' myPop <- population$new(name = "My Population 1",
    #'                         generation = 1,
    #'                         traitInfo = myTrait,
    #'                         inds = list(myInd1, myInd2, myInd3),
    #'                         verbose = FALSE)
    #
    #' myPop$trueEGVMat
    #' myPop$plot(plotTarget = "trueAGV",
    #'            plotType = "violin")
    #' myPop$plot(plotTarget = "trueGV",
    #'            plotType = "scatter",
    #'            scatterAxes = c(2, 1))

    initialize = function(name = NULL,
                          generation = NA,
                          traitInfo = NULL,
                          crossInfo = NULL,
                          inds = list(),
                          verbose = TRUE){
      # checks
      if (is.null(name)) {
        name <- "Unspecified"
      } else name <- as.character(name)

      if (!is.na(generation)) {
        stopifnot(is.numeric(generation))
        generation <- round(generation, 0)
      }

      # traitInfo class
      if (!is.null(traitInfo)) {
        if (class(traitInfo)[1] != "traitInfo") {
          stop(paste('class(traitInfo)[1] != "traitInfo"\n"traitInfo" must be a',
                     'traitInfo object see: ?traitInfo'))
        }
        specie <- traitInfo$lociInfo$specie
      } else {
        specie <- NULL
      }

      if (!is.null(crossInfo)) {
        if (class(crossInfo)[1] != "crossInfo") {
          stop(paste('class(crossInfo)[1] != "crossInfo"\n"crossInfo" must be a',
                     'crossInfo object see: ?crossInfo'))
        }

        if (is.null(crossInfo$crosses) & (crossInfo$matingMethod != "nonCross")) {
          crossInfo$designResourceAllocation()
        }
        inds <- crossInfo$makeCrosses

        if (is.na(generation)) {
          generationBefore <- crossInfo$parentPopulation$generation

          if (is.na(generationBefore)) {
            generation <- 1
            warning(paste0("`generation` was not specified for parent population. ",
                           "The new population will be regarded as the first population. (`genration = 1`)"))
          } else {
            generation <- generationBefore + 1
          }
        }
      }

      if (class(inds)[1] == "individual") {
        inds <- list(inds)
      } else if (class(inds) != "list") {
        stop(paste("inds must be an individual object or a list of",
                   "individuals objects"))
      }

      if (verbose) {
        cat("Create population: Add individuals...\n")
        i <- 1
        tot <- length(inds)
      }
      self$name <- name
      self$generation <- generation
      self$specie <- specie
      self$traitInfo <- traitInfo
      self$crossInfo <- crossInfo
      self$verbose <- verbose

      # for (ind in inds) {
      #   if (verbose) {
      #     cat(paste0("\r", round(i / tot * 100), "%"))
      #     i <- i + 1
      #   }
      #   private$addInd(ind)
      # }

      self$addInds(inds = inds)
      if (verbose) cat(paste("\nA new population created:", self$name, "!\n"))
    },
    #' @description Add individuals to the population
    #' @param inds [individual class or list] list of individuals of the
    #'     population (see:\link[breedSimulatR]{individual})
    #' @examples
    #' # create new individual
    #'
    #' rawHaplo4 <- matrix(sample(c(0, 1), (3 + 4 + 5) * 2, replace = TRUE),
    #'                     nrow = 2)
    #' colnames(rawHaplo4) <- sprintf(fmt = paste0("SNP%0", 2,"i"),
    #'                               1:(3 + 4 + 5))
    #' haplo4 <- haplotype$new(lociInfo = myLoci,
    #'                        haplo = rawHaplo4)
    #' myInd4 <-  individual$new(name = "Ind 4",
    #'                          specie = mySpec,
    #'                          parent1 = "OkaaSan",
    #'                          parent2 = "OtouSan",
    #'                          haplo = haplo4,
    #'                          verbose = FALSE)
    #'
    #' # add individual
    #' print(self)
    #' self$addInds(myInd4)
    #' print(self)
    addInds = function(inds){
      # checks
      if (class(inds)[1] == "individual") {
        inds <- list(inds)
      }

      if (length(self$inds) == 0) {
        if (is.null(self$specie)) {
          self$specie <- inds[[1]]$specie
        }
      }

      indNames <- unlist(lapply(inds, function(ind) ind$name))

      self$inds[indNames] <- inds
      invisible(NULL)
    },
    #' @description Remove individuals from the population
    #' @param indNames [character] character vetcor of the individuals' names
    #' @examples
    #' print(self)
    #' self$remInds("Ind 2")
    #' print(self)
    remInds = function(indNames){

      # check
      if (class(indNames) != "character") {
        stop("Please provide individuals' names as a character vector.")
      }

      if (any(!(indNames %in% names(self$inds)))) {
        id <- which(!(indNames %in% names(self$inds)))
        warning(paste("Some individuals to remove are not in the population:",
                      paste(indNames[id], collapse = " ; ")))
      }

      self$inds[indNames] <- NULL
      invisible(NULL)
    },
    #' @description Remove individuals from the population
    #' @param trialInfoNow [trialInfo class] trialInfo class object
    #' @param herit [numeric] Heritability for each trait (plot-based/line-based)
    #' @param nRep [numeric] Replication of the field trial (common to all traits)
    #' @param multiTraitsAsEnvs [logical] Treat multiple traits as multiple environments or not
    #' @param envSpecificEffects [numeric] Effects specific to each environments / treatments.
    #' If `multiTraitsAsEnvs = FALSE`, envSpecificEffects will be 0 for all traits.
    #' @param residCor [matrix] Residual correlation between traits
    #' @param seedResid [numeric] Random seed for selecting residuals
    #' @examples
    #' print(self$trialInfo)
    #' self$inputTrialInfo()
    #' print(self$trialInfo)
    inputTrialInfo = function(trialInfoNow = NULL,
                              herit = NULL,
                              nRep = NULL,
                              multiTraitsAsEnvs = FALSE,
                              envSpecificEffects = NULL,
                              residCor = NULL,
                              seedResid = NA){
      if (is.null(trialInfoNow)) {
        trialInfoNow <- myBreedSimulatR::trialInfo$new(
          population = self,
          herit = herit,
          nRep = nRep,
          multiTraitsAsEnvs = multiTraitsAsEnvs,
          envSpecificEffects = envSpecificEffects,
          residCor = residCor,
          seedResid = seedResid
        )
      }

      # traitInfo class
      if (class(trialInfoNow)[1] != "trialInfo") {
        stop(paste('class(trialInfoNow)[1] != "trialInfo"\n"trialInfoNow" must be a',
                   'trialInfo object see: ?trialInfo'))
      }

      self$trialInfo <- trialInfoNow
    },

    #' @description Input phenotypic values
    #' @param phenotypicValues [array] individual x traits x replication (3-dimensional array) of phenotypic values
    #' If you use real phenotypic data, please specify your phenotypic values.
    #' If you simulate phenotypic data, please set `phenotypicValues = NULL`.
    inputPhenotypicValues = function(phenotypicValues = NULL) {
      if (!is.null(self$trialInfo)) {
        trialInfo <- self$trialInfo
        simPheno <- self$traitInfo$lociInfo$specie$simInfo$simPheno
        if (simPheno) {
          resid <- trialInfo$resid
          envSpecificEffects <- trialInfo$envSpecificEffects

          trueGVMat <- self$trueGVMat

          trueGVArray <- array(data = rep(trueGVMat, trialInfo$nRep),
                               dim = dim(resid),
                               dimnames = dimnames(resid))
          envSpeEffArray <- aperm(array(data = rep(envSpecificEffects,
                                                   self$nInd * trialInfo$nRep),
                                        dim = dim(resid)[c(2, 1, 3)],
                                        dimnames = dimnames(resid)[c(2, 1, 3)]),
                                  perm = c(2, 1, 3))


          phenotypicValues <- envSpeEffArray + trueGVArray + resid
        } else {
          if (!is.null(phenotypicValues)) {
            stopifnot(is.array(phenotypicValues))
            if (length(dim(phenotypicValues)) == 2) {
              if (ncol(phenotypicValues) != self$traitInfo$nTraits) {
                stop("The column of `phenotypicValues` corresponds to traits!")
              }

              if (trialInfo$nRep != 1) {
                stop("Number of replications should be 1 if you input `phenotypicValues` as 2-dimensional array!")
              }

              phenotypicValues <- array(data = phenotypicValues,
                                        dim = c(dim(phenotypicValues), 1),
                                        dimnames = c(dimnames(phenotypicValues),
                                                     list(repNames = "Rep_1")))
            } else if (length(dim(phenotypicValues)) == 3) {
              if (ncol(phenotypicValues) != self$traitInfo$nTraits) {
                stop("The column of `phenotypicValues` corresponds to traits!")
              }

              if (trialInfo$nRep != dim(phenotypicValues)[3]) {
                stop("Third dimension of `phenotypicValues` should be equal to number of replications!")
              }

            } else {
              stop("`phenotypicValues` should be 3-diensional array!!")
            }

          } else {
            stop("Please specify phenotypic values if you use real phenotype data.")
          }
        }
      } else {
        stop("You should define `self$trialInfo` by using `trialInfo` class first.")
      }

      self$phenotypicValues <- phenotypicValues
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
    },

    #' @description Draw figures for visualization of each data (GVs, phenotypes, and GRM)
    #' @param plotTarget [character] Target of figure, select either one of
    #' "trueAGV", "trueDGV", "trueEGV", "trueGV", "trueAGVET", "trueDGVET", "trueEGVET",
    #'  "trueGVET", "trueAGVCT", "trueDGVCT", "trueEGVCT", "trueGVCT", "phenotypicValues".
    #' @param plotType [character] We offer "box", "violin", "scatter" (or "scatter3d")
    #' to draw figures for genotypic/phenotypic values.
    #' @param scatterAxes [numeric/character] If you select "scatter" option for `plotType`,
    #'  you should design which traits will be assigned to each axis. You can
    #'  define by indices of traits or trait names.
    #'
    plot = function(plotTarget = "trueGV",
                     plotType = "box",
                     scatterAxes = 1:min(2, self$traitInfo$nTraits)) {
      supportTargets <- c("trueAGV", "trueDGV", "trueEGV", "trueGV",
                          "trueAGVET", "trueDGVET", "trueEGVET", "trueGVET",
                          "trueAGVCT", "trueDGVCT", "trueEGVCT", "trueGVCT",
                          "phenotypicValues")
      GVSeries <- c("trueAGV", "trueDGV", "trueEGV", "trueGV",
                    "trueAGVET", "trueDGVET", "trueEGVET", "trueGVET",
                    "trueAGVCT", "trueDGVCT", "trueEGVCT", "trueGVCT")
      if (length(plotTarget) == 1) {
        if (!(plotTarget %in% supportTargets)) {
          stop(paste0("We only offer the following 'plotTarget' options: ",
                      paste(supportTargets, collapse = "; "), ". \n",
                      "Please select one of these options!!"))
        }
        if (plotTarget %in% GVSeries) {
          trueGVMatNow <- self[[paste0(plotTarget, "Mat")]]
          trueGVMatNow <- round(trueGVMatNow, 5)
          trueGVDataFrame <- data.frame(Ind = rep(rownames(trueGVMatNow), self$traitInfo$nTraits),
                                        Trait = rep(colnames(trueGVMatNow), each = nrow(trueGVMatNow)),
                                        GV = c(trueGVMatNow))

          if (plotType %in% c("box", "violin")) {
            plt <- plot_ly(
              data = trueGVDataFrame,
              x = ~ Trait,
              y = ~ GV,
              type = plotType,
              hoverinfo = "text",
              text = paste0(apply(trueGVDataFrame, 1, function(l) {
                paste(names(l), ":", l, collapse = "\n")
              }))
            ) %>%
              plotly::layout(yaxis = list(title = list(text = plotTarget)))

          } else if (plotType %in% c("scatter", "scatter3d")) {
            if (is.numeric(scatterAxes)) {
              stopifnot(all(scatterAxes <= self$traitInfo$nTraits))
              scatterAxesTitles <- self$traitInfo$traitNames[scatterAxes]
            } else if (is.character(scatterAxes)) {
              stopifnot(all(scatterAxes %in% self$traitInfo$traitNames))
              scatterAxesTitles <- scatterAxes
            } else {
              stop(paste0("`scatterAxes` should be a vector of numerics indicating",
                          " the trait No., or chatcters of trait names!!"))
            }
            if (length(scatterAxes) == 1) {
              plt <- plot_ly(x = trueGVMatNow[, scatterAxes[1]],
                             type = "scatter",
                             mode = "markers",
                             hoverinfo = "text",
                             text = apply(data.frame(indNames = rownames(trueGVMatNow),
                                                     trueGVMatNow), 1, function(l) {
                                                       paste(names(l), ":", l, collapse = "\n")
                                                     })) %>%
                plotly::layout(xaxis = list(title = list(text = scatterAxesTitles)))
            } else if (length(scatterAxes) == 2) {
              plt <- plot_ly(x = trueGVMatNow[, scatterAxes[1]],
                             y = trueGVMatNow[, scatterAxes[2]],
                             type = "scatter",
                             mode = "markers",
                             hoverinfo = "text",
                             text = apply(data.frame(indNames = rownames(trueGVMatNow),
                                                     trueGVMatNow), 1, function(l) {
                                                       paste(names(l), ":", l, collapse = "\n")
                                                     })) %>%
                plotly::layout(xaxis = list(title = list(text = scatterAxesTitles[1])),
                               yaxis = list(title = list(text = scatterAxesTitles[2])))
            } else if (length(scatterAxes) == 3) {
              plt <- plot_ly(x = trueGVMatNow[, scatterAxes[1]],
                             y = trueGVMatNow[, scatterAxes[2]],
                             z = trueGVMatNow[, scatterAxes[3]],
                             type = "scatter3d",
                             mode = "markers",
                             hoverinfo = "text",
                             text = apply(data.frame(indNames = rownames(trueGVMatNow),
                                                     trueGVMatNow), 1, function(l) {
                                                       paste(names(l), ":", l, collapse = "\n")
                                                     })) %>%
                plotly::layout(scene = list(
                  xaxis = list(title = list(text = scatterAxesTitles[1])),
                  yaxis = list(title = list(text = scatterAxesTitles[2])),
                  zaxis = list(title = list(text = scatterAxesTitles[3]))
                ))
            } else {
              stop("`scatterAxes` should be a vector with length equal to or less than 3!")
            }
          }
        } else if (plotTarget == "phenotypicValues") {
          if (is.null(self$phenotypicValues)) {
            self$inputPhenotypicValues()
          }
          phenotypicValues <- self$phenotypicValues

          truePVMatNow <- round(apply(phenotypicValues, c(1, 2), mean, na.rm = TRUE), 5)

          truePVDataFrame <- data.frame(Ind = rep(rownames(phenotypicValues), prod(dim(phenotypicValues)[2:3])),
                                        Trait = rep(rep(colnames(phenotypicValues), each = nrow(phenotypicValues)),
                                                    dim(phenotypicValues)[3]),
                                        Rep = rep(dimnames(phenotypicValues)[[3]],
                                                  each = prod(dim(phenotypicValues)[1:2])),
                                        PV = round(c(phenotypicValues), 5))

          if (plotType %in% c("box", "violin")) {
            plt <- plot_ly(
              data = truePVDataFrame,
              x = ~ Trait,
              y = ~ PV,
              split = ~ Rep,
              type = plotType,
              hoverinfo = "text",
              text = paste0(apply(truePVDataFrame, 1, function(l) {
                paste(names(l), ":", l, collapse = "\n")
              }))
            ) %>%
              plotly::layout(yaxis = list(title = list(text = plotTarget)))
            if (plotType == "box") {
              plt <- plt %>% plotly::layout(boxmode = "group")
            } else if (plotType == "violin") {
              plt <- plt %>% plotly::layout(violinmode = "group")
            }

          } else if (plotType %in% c("scatter", "scatter3d")) {
            if (is.numeric(scatterAxes)) {
              stopifnot(all(scatterAxes <= self$traitInfo$nTraits))
              scatterAxesTitles <- self$traitInfo$traitNames[scatterAxes]
            } else if (is.character(scatterAxes)) {
              stopifnot(all(scatterAxes %in% self$traitInfo$traitNames))
              scatterAxesTitles <- scatterAxes
            } else {
              stop(paste0("`scatterAxes` should be a vector of numerics indicating",
                          " the trait No., or chatcters of trait names!!"))
            }
            if (length(scatterAxes) == 1) {
              plt <- plot_ly(x = truePVMatNow[, scatterAxes[1]],
                             type = "scatter",
                             mode = "markers",
                             hoverinfo = "text",
                             text = apply(data.frame(indNames = rownames(truePVMatNow),
                                                     truePVMatNow), 1, function(l) {
                                                       paste(names(l), ":", l, collapse = "\n")
                                                     })) %>%
                plotly::layout(xaxis = list(title = list(text = scatterAxesTitles)))
            } else if (length(scatterAxes) == 2) {
              plt <- plot_ly(x = truePVMatNow[, scatterAxes[1]],
                             y = truePVMatNow[, scatterAxes[2]],
                             type = "scatter",
                             mode = "markers",
                             hoverinfo = "text",
                             text = apply(data.frame(indNames = rownames(truePVMatNow),
                                                     truePVMatNow), 1, function(l) {
                                                       paste(names(l), ":", l, collapse = "\n")
                                                     })) %>%
                plotly::layout(xaxis = list(title = list(text = scatterAxesTitles[1])),
                               yaxis = list(title = list(text = scatterAxesTitles[2])))
            } else if (length(scatterAxes) == 3) {
              plt <- plot_ly(x = truePVMatNow[, scatterAxes[1]],
                             y = truePVMatNow[, scatterAxes[2]],
                             z = truePVMatNow[, scatterAxes[3]],
                             type = "scatter3d",
                             mode = "markers",
                             hoverinfo = "text",
                             text = apply(data.frame(indNames = rownames(truePVMatNow),
                                                     truePVMatNow), 1, function(l) {
                                                       paste(names(l), ":", l, collapse = "\n")
                                                     })) %>%
                plotly::layout(scene = list(
                  xaxis = list(title = list(text = scatterAxesTitles[1])),
                  yaxis = list(title = list(text = scatterAxesTitles[2])),
                  zaxis = list(title = list(text = scatterAxesTitles[3]))
                ))
            } else {
              stop("`scatterAxes` should be a vector with length equal to or less than 3!")
            }
          }
        }
      } else if (length(plotTarget) >= 2) {
        if (!all(plotTarget %in% supportTargets)) {
          stop(paste0("We only offer the following 'plotTarget' options: ",
                      paste(supportTargets, collapse = "; "), ". \n",
                      "Please select one of these options!!"))
        }

        if (plotType %in% c("box", "violin")) {
          trueGVDataFrames <- NULL
          for (matNo in 1:length(plotTarget)) {
            plotTargetNow <- plotTarget[matNo]

            if (plotTargetNow %in% GVSeries) {
              trueGVMatNow <- round(self[[paste0(plotTargetNow, "Mat")]], 5)
            } else if (plotTargetNow == "phenotypicValues") {
              trueGVMatNow <- round(apply(self$phenotypicValues, c(1, 2), mean, na.rm = TRUE), 5)
            }
            trueGVDataFrameNow <- data.frame(Ind = rep(rownames(trueGVMatNow), self$traitInfo$nTraits),
                                             Trait = rep(colnames(trueGVMatNow), each = self$nInd),
                                             Target = rep(plotTargetNow, self$traitInfo$nTraits * self$nInd),
                                             value = c(trueGVMatNow))
            trueGVDataFrames <- rbind(trueGVDataFrames, trueGVDataFrameNow)
          }

          plt <- plot_ly(
            data = trueGVDataFrames,
            x = ~ Trait,
            y = ~ value,
            split = ~ Target,
            type = plotType,
            hoverinfo = "text",
            text = paste0(apply(trueGVDataFrames, 1, function(l) {
              paste(names(l), ":", l, collapse = "\n")
            }))
          ) %>%
            plotly::layout(yaxis = list(title = list(text = plotTarget)))
          if (plotType == "box") {
            plt <- plt %>% plotly::layout(boxmode = "group")
          } else if (plotType == "violin") {
            plt <- plt %>% plotly::layout(violinmode = "group")
          }

        } else if (plotType %in% c("scatter", "scatter3d")) {
          if (is.numeric(scatterAxes)) {
            stopifnot(all(scatterAxes <= self$traitInfo$nTraits))
            scatterAxesTitles <- self$traitInfo$traitNames[scatterAxes]
          } else if (is.character(scatterAxes)) {
            stopifnot(all(scatterAxes %in% self$traitInfo$traitNames))
            scatterAxesTitles <- scatterAxes
          } else {
            stop(paste0("`scatterAxes` should be a vector of numerics indicating",
                        " the trait No., or chatcters of trait names!!"))
          }
          stopifnot(length(plotTarget) == length(scatterAxes))

          trueGVMatScatter <- NULL
          for (matNo in 1:length(plotTarget)) {
            plotTargetNow <- plotTarget[matNo]

            if (plotTargetNow %in% GVSeries) {
              trueGVMatNow <- round(self[[paste0(plotTargetNow, "Mat")]], 5)
            } else if (plotTargetNow == "phenotypicValues") {
              trueGVMatNow <- round(apply(self$phenotypicValues, c(1, 2), mean, na.rm = TRUE), 5)
            }
            trueGVMatAddNow <- trueGVMatNow[, scatterAxes[matNo], drop = FALSE]
            colnames(trueGVMatAddNow) <- paste0(plotTargetNow, "_",
                                                colnames(trueGVMatAddNow))
            trueGVMatScatter <- cbind(trueGVMatScatter, trueGVMatAddNow)
          }
          scatterAxesTitles <- colnames(trueGVMatScatter)

          if (length(scatterAxes) == 2) {
            plt <- plot_ly(x = trueGVMatScatter[, 1],
                           y = trueGVMatScatter[, 2],
                           type = "scatter",
                           mode = "markers",
                           hoverinfo = "text",
                           text = apply(data.frame(indNames = rownames(trueGVMatScatter),
                                                   trueGVMatScatter), 1, function(l) {
                                                     paste(names(l), ":", l, collapse = "\n")
                                                   })) %>%
              plotly::layout(xaxis = list(title = list(text = scatterAxesTitles[1])),
                             yaxis = list(title = list(text = scatterAxesTitles[2])))
          } else if (length(scatterAxes) == 3) {
            plt <- plot_ly(x = trueGVMatScatter[, 1],
                           y = trueGVMatScatter[, 2],
                           z = trueGVMatScatter[, 3],
                           type = "scatter3d",
                           mode = "markers",
                           hoverinfo = "text",
                           text = apply(data.frame(indNames = rownames(trueGVMatScatter),
                                                   trueGVMatScatter), 1, function(l) {
                                                     paste(names(l), ":", l, collapse = "\n")
                                                   })) %>%
              plotly::layout(scene = list(
                xaxis = list(title = list(text = scatterAxesTitles[1])),
                yaxis = list(title = list(text = scatterAxesTitles[2])),
                zaxis = list(title = list(text = scatterAxesTitles[3]))
              ))
          } else {
            stop("`scatterAxes` should be a vector with length equal to or less than 3!")
          }
        }
      }

      print(plt)
    }

  ),
  active = list(
    #' @field nInd [numeric] number of individual in the population
    nInd = function(){
      length(self$inds)
    },
    #' @field genoMat [matrix] matrix of all the genotypes of the population
    #'   encoded in allele doses. (individuals in row and markers in column)
    genoMat = function(){
      if (length(self$inds) > 0) {
        t(vapply(self$inds, function(ind){
          ind$haplo$allelDose
        }, vector(mode = "numeric",
                  length = length(self$inds[[1]]$haplo$allelDose)))
        )
      } else {
        NULL
      }
    },
    #' @field trueAGVMat [matrix] matrix of true additive genotypic values
    trueAGVMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueAGVs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueAGVs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueAGVs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },
    #' @field trueDGVMat [matrix] matrix of true dominant genotypic values
    trueDGVMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueDGVs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueDGVs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueDGVs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },
    #' @field trueEGVMat [matrix] matrix of true epistatic genotypic values
    trueEGVMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueEGVs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueEGVs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueEGVs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },
    #' @field trueGVMat [matrix] matrix of true genotypic values
    trueGVMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueGVs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueGVs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueGVs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },
    #' @field trueAGVETMat [matrix] matrix of true additive genotypic values specific to each trait
    trueAGVETMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueAGVETs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueAGVETs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueAGVETs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },
    #' @field trueDGVETMat [matrix] matrix of true dominant genotypic values specific to each trait
    trueDGVETMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueDGVETs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueDGVETs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueDGVETs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },
    #' @field trueEGVETMat [matrix] matrix of true epistatic genotypic values specific to each trait
    trueEGVETMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueEGVETs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueEGVETs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueEGVETs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },
    #' @field trueGVETMat [matrix] matrix of true genotypic values specific to each trait
    trueGVETMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueGVETs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueGVETs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueGVETs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },

    #' @field trueAGVCTMat [matrix] matrix of true additive genotypic values common across trait
    trueAGVCTMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueAGVCTs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueAGVCTs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueAGVCTs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },
    #' @field trueDGVCTMat [matrix] matrix of true dominant genotypic values common across trait
    trueDGVCTMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueDGVCTs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueDGVCTs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueDGVCTs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },
    #' @field trueEGVCTMat [matrix] matrix of true epistatic genotypic values common across trait
    trueEGVCTMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueEGVCTs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueEGVCTs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueEGVCTs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },
    #' @field trueGVCTMat [matrix] matrix of true genotypic values common across trait
    trueGVCTMat = function() {
      if (length(self$inds) >= 2) {
        if (self$traitInfo$nTraits >= 2) {
          mat <- t(vapply(self$inds, function(ind) {
            ind$trueGVCTs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)))
        } else {
          mat <- matrix(vapply(self$inds, function(ind) {
            ind$trueGVCTs
          },
          FUN.VALUE = vector(mode = "numeric",
                             length = self$traitInfo$nTraits)),
          nrow = self$nInd)
          rownames(mat) <- names(self$inds)
          colnames(mat) <- self$traitInfo$traitNames
        }
      } else if (length(self$inds) == 1) {
        mat <- t(self$inds[[1]]$trueGVCTs)
        rownames(mat) <- names(self$inds)[1]
      } else {
        mat <- NULL
      }

      return(mat)
    },


    #' @field haploArray [array] 3-dimensional array of all the haplotypes of the population
    #'   encoded with {0, 1}. (individuals in row, markers in column, and ploidy in 3rd dimension)
    haploArray = function(){
      if (length(self$inds) > 0) {
        haploList <-  lapply(self$inds, function(ind) {
          do.call(cbind, ind$haplo$values)
        })
        haploList
        haploArray <- do.call(rbind, haploList)
        dim(haploArray) <- c(self$specie$ploidy,
                             self$nInd,
                             sum(self$specie$nLoci))
        haploArray <- aperm(haploArray, c(2, 3, 1))
        dimnames(haploArray) <- list(indNames = names(self$inds),
                                     lociNames = colnames(haploList[[1]]),
                                     ploidy = rownames(haploList[[1]]))

        return(haploArray)
      } else {
        NULL
      }
    },

    #' @field af [named vector] allele frequency
    af = function(){
      ploidy <- self$specie$ploidy
      stopifnot(ploidy %in% c(1, 2))

      genoMat <- self$genoMat
      freq <- Rfast::colsums(genoMat) / (self$nInd * ploidy)
      names(freq) <- colnames(genoMat)

      return(freq)
    },

    #' @field maf [named vector] minor allele frequency
    maf = function(){
      maf <- pmin(self$af, 1 - self$af)

      return(maf)
    },


    #' @field heteroRate [named vector] ratio of heterozygotes
    heteroRate = function(){
      ploidy <- self$specie$ploidy
      stopifnot(ploidy %in% c(1, 2))
      genoMat <- self$genoMat

      heteroRate <- apply(genoMat, 2, function(mrk) {
        mean(!(mrk %in% c(0, ploidy)))
      })

      return(heteroRate)
    }

  ),
  private = list(
    # @description Add new individual to the population
    # @param ind [individual class] individual
    #   (see:\link[breedSimulatR]{individual})
    # @return NULL
    addInd = function(ind) {

      # checks class
      if (class(ind)[1] != "individual") {
        stop('variable "ind" must be of class "individual".')
      }

      # check species
      if (is.null(self$specie)) {
        self$specie <- ind$specie
      } else if (!isTRUE(all.equal(self$specie, ind$specie))) {
        stop(paste("Individual of a different species than the population's",
                   "one.\nPlease add", self$specie$name, "individuals."))
      }

      # check name
      if (ind$name %in% names(self$inds)) {
        stop(paste("Individual with the same name already exists",
                   "in the population:", ind$name))
      }

      # add new individual
      self$inds[[ind$name]] <- ind
      NULL
    }
  )
)





#' Create population object from genotype data.frame
#'
#' @param geno [data.frame / matrix] genotype of the individuals encoded in allele dose.
#' If you use real data, you must specify `geno` or `haplo` argument.
#' @param haplo [array] haplotype of the individuals scored with 0 and 1 (3-dimensional array).
#' @param lociInfo [lociInfo object] information about the individuals haplotypes'
#'   SNPs (see:\link[breedSimulatR]{lociInfo})
#' @param traitInfo [traitInfo class] Specific information of traits
#'   (see:\link[myBreedSimulatR]{traitInfo})
#' @param founderIsInitPop [logical] Founder haplotype will be regarded as first population or not.
#' @param nGenerationRM [numeric] In this simulation, if `founderIsInitPop = FALSE`, random mating is repeated before setting the initial population.
#' In more details, first random mating is repeated `nGenerationRM` times.
#' Then, random mating with random selection to resemble a population bottleneck is repeated `nGenerationRSM` times.
#' Finally, random mating is repeated `nGenerationRM2` times to remove close family relationships.
#' These procedures are similar to Müller et al, G3, 2018.
#' @param nGenerationRSM [numeric] Number of generations for random selection and mating
#' @param nGenerationRM2 [numeric] Number of generations for second random mating
#' @param propSelRS [numeric] Proportion of number p¥of selected individuals for random selection and mating
#' @param seedSimHaplo [numeric] Random seed for selecting haplotype from founder haplotype
#' @param seedSimRM [numeric] Random seed for mate pairs
#' @param seedSimRS [numeric] Random seed for random selection
#' @param seedSimMC [numeric] Random seed for make crosses
#' @param indNames [character] NULL or character string vector specifying the individuals
#'   names. If NULL, \code{rownames(geno)} will be used.
#' @param popName [character] population's name.
#' @param verbose [logical] display information
#' @return population object (see:\link[breedSimulatR]{population})
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
#' plot(myTrait)
#
#' ### create simulated population
#' simulatedPop <- createPop(geno = NULL,
#'                           haplo = NULL,
#'                           lociInfo = myLoci,
#'                           traitInfo = myTrait,
#'                           founderIsInitPop = TRUE,
#'                           popName = "First Population",
#'                           verbose = FALSE)
#' simulatedPop$plot(plotTarget = "trueGV",
#'                   plotType = "violin")
#' simulatedPop$plot(plotTarget = "trueGV",
#'                   plotType = "scatter",
#'                   scatterAxes = 1:3)
#'
createPop <- function(geno = NULL,
                      haplo = NULL,
                      lociInfo,
                      traitInfo = NULL,
                      founderIsInitPop = FALSE,
                      nGenerationRM = 100,
                      nGenerationRSM = 10,
                      nGenerationRM2 = 3,
                      propSelRS = 0.1,
                      seedSimHaplo = NA,
                      seedSimRM = NA,
                      seedSimRS = NA,
                      seedSimMC = NA,
                      indNames = NULL,
                      popName = NULL,
                      verbose = TRUE) {
  if (verbose) {
    cat("Create population: Initialization...\n")
  }
  # check parameters:
  if (class(lociInfo)[1] != "lociInfo") {
    stop('"class(lociInfo)" must be "lociInfo"')
  }

  # traitInfo class
  if (!is.null(traitInfo)) {
    if (class(traitInfo)[1] != "traitInfo") {
      stop(paste('class(traitInfo)[1] != "traitInfo"\n"traitInfo" must be a',
                 'traitInfo object see: ?traitInfo'))
    }
  }

  ploidy <- lociInfo$specie$ploidy

  if (!is.null(seedSimHaplo)) {
    if (!is.na(seedSimHaplo)) {
      stopifnot(is.numeric(seedSimHaplo))
      seedSimHaplo <- floor(seedSimHaplo)
    } else {
      seedSimHaplo <- sample(x = 1e9, size = 1)
    }
  }

  # seedSimHaplo
  if (!is.null(seedSimHaplo)) {
    if (!is.na(seedSimHaplo)) {
      stopifnot(is.numeric(seedSimHaplo))
      seedSimHaplo <- floor(seedSimHaplo)
    } else {
      seedSimHaplo <- sample(x = 1e9, size = 1)
    }
  }


  # nGenerationRM
  if (!is.null(nGenerationRM)) {
    stopifnot(is.numeric(nGenerationRM))
    nGenerationRM <- floor(nGenerationRM)
    stopifnot(nGenerationRM >= 0)
  }


  # nGenerationRSM
  if (!is.null(nGenerationRSM)) {
    stopifnot(is.numeric(nGenerationRSM))
    nGenerationRSM <- floor(nGenerationRSM)
    stopifnot(nGenerationRSM >= 0)
  }


  # nGenerationRM2
  if (!is.null(nGenerationRM2)) {
    stopifnot(is.numeric(nGenerationRM2))
    nGenerationRM2 <- floor(nGenerationRM2)
    stopifnot(nGenerationRM2 >= 0)
  }


  if (all(c(nGenerationRM, nGenerationRSM, nGenerationRM2) == 0)) {
    if (!founderIsInitPop) {
      message("Because you set all the generations for random mating = 0, `founderIsInitPop` is set as TRUE.")
    }
    founderIsInitPop <- TRUE
  }

  if (founderIsInitPop) {
    if (!all(c(nGenerationRM, nGenerationRSM, nGenerationRM2) == 0)) {
      message("Because you set `founderIsInitPop = TRUE`, all the generations for random mating is replaced by 0")
    }
  }

  # geno & haplo
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

  if (is.null(indNames)) {
    if (is.null(rownames(geno))) {
      stop('"rownames(geno)" is NULL, please specify "indNames"')
    } else {
      if (founderIsInitPop) {
        indNames <- rownames(geno)
      } else {
        indNames <- .charSeq(paste0("G", 1, "_"), seq(nrow(haplo)))
      }
    }
  } else if (length(indNames) == 1) {
    indNames <- sprintf(
      fmt = paste0(indNames,
                   "%0", floor(log10(nrow(geno))) + 1, "i"),
      seq(nrow(geno)))
  } else if (length(indNames) != nrow(geno)) {
    stop(paste0("length(indNames) = ", length(indNames),
                '\n"length(indNames)" must be equal to "1" or "nrow(geno)"'))
  }



  if (!founderIsInitPop) {
    if (any(indNames %in% rownames(geno))) {
      warning(paste0("The following offspring names will overlap the line names in the population: ",
                     paste(indNames[indNames %in% rownames(geno)], collapse = "; ")))
    }

    if (is.null(rownames(geno))) {
      rownames(geno) <- rownames(haplo) <- .charSeq(paste0("G", 0, "_"), seq(nrow(haplo)))
    }
  } else {
    if (is.null(rownames(geno))) {
      rownames(geno) <- rownames(haplo) <- indNames
    }
  }

  if (!all.equal(unique(indNames), indNames)) {
    stop('All values of "indNames" must be different')
  }

  # if (!all(unique(unlist(geno)) %in% c(0,2))) {
  #   stop(paste0('Some values of "geno" are different from 0 or 2.\n',
  #               'All individuals must be homozygotes and genotypes must be ',
  #               'encoded as allele doses'))
  # }

  listInds <- list()
  geno <- as.matrix(geno)

  if (verbose) {
    cat("Create population: Create individuals...\n")
    prog <- 0
    nInd <- nrow(geno)
  }

  if (founderIsInitPop) {
    indNamesNow <- indNames
  } else {
    indNamesNow <- rownames(geno)
  }

  for (i in seq(nrow(geno))) {
    if (verbose) {
      prog <- i / nInd
      cat(paste0("\r", round(prog * 100), "%"))
    }

    haploNow <- t(haplo[i, , ])
    listInds[[i]] <- individual$new(
      name = indNamesNow[i],
      specie = lociInfo$specie,
      traitInfo = traitInfo,
      parent1 = NA,
      parent2 = NA,
      haplo = haplotype$new(lociInfo = lociInfo, haplo = haploNow),
      verbose = FALSE
    )
  }

  if (verbose) {
    cat("\nCreate population: Create population object...\n")
    prog <- 0
  }

  if (founderIsInitPop) {
    popNow <- population$new(name = popName, generation = 1,
                             traitInfo = traitInfo, crossInfo = NULL,
                             inds = listInds, verbose = verbose)
  } else {
    if (verbose) {
      cat("\nCreate population: Create initial population object (generation = 0)...\n")
    }

    initPop <- population$new(name = "0th population", generation = 0,
                              traitInfo = traitInfo, crossInfo = NULL,
                              inds = listInds, verbose = verbose)

    # propSelRS
    if (!is.null(propSelRS)) {
      stopifnot(is.numeric(propSelRS))
      stopifnot(propSelRS >= 0)
      stopifnot(propSelRS <= 1)
    }



    # seedSimRM
    if (!is.null(seedSimRM)) {
      if (!is.na(seedSimRM)) {
        stopifnot(is.numeric(seedSimRM))
        seedSimRM <- floor(seedSimRM)
      } else {
        seedSimRM <- sample(x = 1e9, size = 1)
      }
    }

    # seedSimRS
    if (!is.null(seedSimRS)) {
      if (!is.na(seedSimRS)) {
        stopifnot(is.numeric(seedSimRS))
        seedSimRS <- floor(seedSimRS)
      } else {
        seedSimRS <- sample(x = 1e9, size = 1)
      }
    }

    # seedSimMC
    if (!is.null(seedSimMC)) {
      if (!is.na(seedSimMC)) {
        stopifnot(is.numeric(seedSimMC))
        seedSimMC <- floor(seedSimMC)
      } else {
        seedSimMC <- sample(x = 1e9, size = 1)
      }
    }

    proceedGen <- function(proceedType) {
      nNextPop <- nrow(haplo)
      if (proceedType == "randomMate") {
        selectionMethod <- "nonSelection"
        nSel <- nNextPop
        nGenerationProceed <- nGenerationRM
      } else if (proceedType == "randomSelMate") {
        selectionMethod <- "userSpecific"
        nSel <- round(nNextPop * propSelRS)
        nGenerationProceed <- nGenerationRSM
      } else if (proceedType == "randomMate2") {
        selectionMethod <- "nonSelection"
        nSel <- nNextPop
        nGenerationProceed <- nGenerationRM2
      }

      for (generationProceedNo in 1:nGenerationProceed) {
        generation <- popNow$generation + 1
        if (generation == nGenerationTotal) {
          generation <- 1
          indNamesNow <- indNames
        } else {
          indNamesNow <- .charSeq(paste0("G", generation, "_"),
                                  seq(nNextPop))
        }

        if (proceedType == "randomSelMate") {
          set.seed(seedSimRS)
          selCands <- sample(x = names(popNow$inds),
                             size = nSel,
                             replace = FALSE)
        } else {
          selCands <- NULL
        }


        crossInfoNow <- crossInfo$new(parentPopulation = popNow,
                                      nSelectionWays = 1,
                                      selectionMethod = selectionMethod,
                                      traitNoSel = 1,
                                      userSI = NULL,
                                      lociEffects = NULL,
                                      blockSplitMethod = "nMrkInBlock",
                                      nMrkInBlock = 1,
                                      minimumSegmentLength = 1,
                                      nSelInitOPV = nNextPop,
                                      nIterOPV = 1e04,
                                      nProgeniesEMBV = 50,
                                      nIterEMBV = 5,
                                      nCoresEMBV = 1,
                                      clusteringForSel = FALSE,
                                      nCluster = 1,
                                      nTopCluster = 1,
                                      nTopEach = nNextPop,
                                      nSel = nSel,
                                      multiTraitsEvalMethod = "sum",
                                      hSel = 1,
                                      matingMethod = "randomMate",
                                      allocateMethod = "equalAllocation",
                                      weightedAllocationMethod = NULL,
                                      nProgenies = NULL,
                                      traitNoRA = 1,
                                      h = 0.1,
                                      minimumUnitAllocate = 1,
                                      includeGVP = FALSE,
                                      nNextPop = nNextPop,
                                      nPairs = NULL,
                                      nameMethod = "individualBase",
                                      indNames = indNamesNow,
                                      seedSimRM = seedSimRM,
                                      seedSimMC = seedSimMC,
                                      selCands = selCands,
                                      crosses = NULL,
                                      verbose = FALSE)

        popNow <- population$new(name = popName,
                                 generation = generation,
                                 traitInfo = crossInfoNow$parentPopulation$traitInfo,
                                 crossInfo = crossInfoNow,
                                 verbose = FALSE)
      }

      return(popNow)
    }

    nGenerationTotal <- nGenerationRM + nGenerationRSM + nGenerationRM2

    if (verbose) {
      cat("\nCreate population: Initialize population by random mating...\n")
    }
    popNow <- initPop

    if (nGenerationRM > 0) {
      if (verbose) {
        cat(paste0("Number of first random mating: ", nGenerationRM, "\n"))
      }
      popNow <- proceedGen("randomMate")
    }

    if (nGenerationRSM > 0) {
      if (verbose) {
        cat(paste0("Number of random selection and mating: ", nGenerationRSM, "\n"))
      }
      popNow <- proceedGen("randomSelMate")
    }

    if (nGenerationRM2 > 0) {
      if (verbose) {
        cat(paste0("Number of second random mating: ", nGenerationRM2, "\n"))
      }
      popNow <- proceedGen("randomMate2")
    }
  }


  return(popNow)
}