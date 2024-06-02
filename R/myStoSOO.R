# Author: Kosuke Hamazaki hamazaki@ut-biomet.org
# 2021 The University of Tokyo
#
# Description:
# Definition of stoSOO class (, node class, layer class, and tree class)




#' R6 Class Representing a function optimizer by StoSOO (Stochastic Simultaneous Optimistic Optimization)
#'
#' @description
#' stoSOO object store specific information on global optimization for stochastic functions given a finite number of budget (StoSOO).
#'
# @details
# Details: stoSOO object store specific information on global optimization for stochastic functions given a finite number of budget (StoSOO).
#'
#' @export
#' @import R6
#' @references
#' R. Munos (2011), Optimistic optimization of deterministic functions without the knowledge of its smoothness,
#' \emph{NIPS}, 783-791. \cr \cr
#' M. Valko, A. Carpentier and R. Munos (2013), Stochastic Simultaneous Optimistic Optimization,
#' \emph{ICML}, 19-27 \url{http://hal.inria.fr/hal-00789606}. Matlab code: \url{https://team.inria.fr/sequel/software/StoSOO}. \cr \cr
#' P. Preux, R. Munos, M. Valko (2014), Bandits attack function optimization, \emph{IEEE Congress on Evolutionary Computation (CEC)}, 2245-2252.
stoSOO <- R6::R6Class(
  classname = "stoSOO",

  public = list(
    #' @field parameter [vector] Vector with length defining the dimensionality of the optimization problem. Providing actual values of par is not necessary (NAs are just fine).
    parameter = NULL,
    #' @field paramLen [numeric] Number of parameters
    paramLen = NULL,
    #' @field optimizeFunc [function] Scalar function to be optimized, with first argument to be optimized over
    optimizeFunc = NULL,
    #' @field lowerBound [numeric] Vectors of lower bounds on the variables
    lowerBound = NULL,
    #' @field upperBound [numeric] Vectors of upper bounds on the variables
    upperBound = NULL,
    #' @field nIterOptimization [numeric] Number of function evaluations allocated to optimization
    nIterOptimization = NULL,
    #' @field nMaxEvalPerNode [numeric] Maximum number of evaluations per leaf
    nMaxEvalPerNode = NULL,
    #' @field maxDepth [numeric] Maximum depth of the tree
    maxDepth = NULL,
    #' @field nChildrenPerExpansion [numeric] Number of children per expansion
    nChildrenPerExpansion = NULL,
    #' @field confidenceParam [numeric] Confidence parameter (see Valko et al., 2013)
    confidenceParam = NULL,
    #' @field maximize [logical] If TRUE, performs maximization
    maximize = NULL,
    #' @field optimizeType [character] Either 'deterministic' for optimizing a deterministic function or 'stochastic' for a stochastic one
    optimizeType = NULL,
    #' @field returnOptimalNodes [numeric] When (how many iterations) to return (or save) the optimal nodes when optimizing hyper parameter by StoSOO
    returnOptimalNodes = NULL,
    #' @field saveTreeNameBase [character] Base name of the tree to be saved
    saveTreeNameBase = NULL,
    #' @field whenToSaveTrees [numeric] When (how many iterations) to save the tree in StoSOO
    whenToSaveTrees = NULL,
    #' @field withCheck [logical] Check arguments for `node`, `layer`, and `tree` class or not
    withCheck = NULL,
    #' @field verbose [logical] Display information
    verbose = NULL,

    #' @field widthBase [numeric] Base of width of the estimates of rewards
    widthBase = NULL,
    #' @field funcScale [numeric] Scale for function to be optimized. If `maximize = TRUE`, `funcScale = 1`, and else `funcScale = -1`.
    funcScale = NULL,
    #' @field maximizeFunc [function] Function to be maximized given parameters scaled from 0 to 1.
    maximizeFunc = NULL,

    #' @field currentTree [tree class] `tree` class object for the current status
    currentTree = NULL,
    #' @field optimalNodes [list] List of optimal nodes (`node` class object) corresponding to `returnOptimalNodes`
    optimalNodes = list(),
    #' @field optimalHyperParamMat [matrix] Matrix of optimal parameters
    optimalHyperParamMat = NULL,
    #' @field optimalNodeFinal [node class] Optimal node (`node` class object) for the final tree
    optimalNodeFinal = NULL,
    #' @field optimalParameter [numeric] Optimal parameter estimated by StooSOO given a finite number of evaluations
    optimalParameter = NULL,
    #' @field optimalValue [numeric] Optimal value estimated by StooSOO given a finite number of evaluations
    optimalValue = NULL,


    #' @description Create a new stoSOO object.
    #' @param parameter [vector] Vector with length defining the dimensionality of the optimization problem. Providing actual values of par is not necessary (NAs are just fine).
    #' @param optimizeFunc [function] Scalar function to be optimized, with first argument to be optimized over
    #' @param ... [logical/numeric/character/etc...] Optional additional arguments to `optimizeFunc`
    #' @param lowerBound [numeric] Vectors of lower bounds on the variables
    #' @param upperBound [numeric] Vectors of upper bounds on the variables
    #' @param nIterOptimization [numeric] Number of function evaluations allocated to optimization
    #' @param nMaxEvalPerNode [numeric] Maximum number of evaluations per leaf
    #' @param maxDepth [numeric] Maximum depth of the tree
    #' @param nChildrenPerExpansion [numeric] Number of children per expansion
    #' @param confidenceParam [numeric] Confidence parameter (see Valko et al., 2013)
    #' @param maximize [logical] If TRUE, performs maximization
    #' @param optimizeType [character] Either 'deterministic' for optimizing a deterministic function or 'stochastic' for a stochastic one
    #' @param returnOptimalNodes [numeric] When (how many iterations) to return (or save) the optimal nodes when optimizing hyper parameter by StoSOO
    #' @param saveTreeNameBase [character] Base name of the tree to be saved
    #' @param whenToSaveTrees [numeric] When (how many iterations) to save the tree in StoSOO
    #' @param currentTree [tree] tree class object that is saved as a `currentTree` in the previous analysis
    #' @param optimalNodes [list] List of optimal nodes (`node` class object) corresponding to `returnOptimalNodes`
    #' @param withCheck [logical] Check arguments for `node`, `layer`, and `tree` class or not
    #' @param verbose [logical] Display information
    #'
    initialize = function(
    parameter,
    optimizeFunc,
    ...,
    lowerBound = NULL,
    upperBound = NULL,
    nIterOptimization = NULL,
    nMaxEvalPerNode = NULL,
    maxDepth = NULL,
    nChildrenPerExpansion = NULL,
    confidenceParam = NULL,
    maximize = NULL,
    optimizeType = NULL,
    returnOptimalNodes = NULL,
    saveTreeNameBase = NULL,
    whenToSaveTrees = NA,
    currentTree = NULL,
    optimalNodes = list(),
    withCheck = FALSE,
    verbose = TRUE
    ) {

      # some definitions
      optimizeTypesOffered <- c("stochastic", "deterministic")


      # parameter
      stopifnot(is.vector(parameter))
      paramLen <- length(parameter)


      # optimizeFunc
      stopifnot(is.function(optimizeFunc))


      # lowerBound
      if (!is.null(lowerBound)) {
        stopifnot(is.numeric(lowerBound))
        stopifnot(is.vector(lowerBound))
      } else {
        lowerBound <- rep(0, paramLen)
        message("You do not specify `lowerBound`. We set `lowerBound = rep(0, length(parameter))`.")
      }

      if (!(length(lowerBound) %in% c(1, paramLen))) {
        warning(paste0("`length(lowerBound)` should be equal to 1 or length(parameter) ! \n",
                       "We substitute `lowerBound` by `rep(lowerBound[1], length(parameter))` instead."))
        lowerBound <- rep(lowerBound[1], paramLen)
      } else if (length(lowerBound) == 1) {
        lowerBound <- rep(lowerBound, paramLen)
      }


      # upperBound
      if (!is.null(upperBound)) {
        stopifnot(is.numeric(upperBound))
        stopifnot(is.vector(upperBound))
      } else {
        upperBound <- rep(1, paramLen)
        message("You do not specify `upperBound`. We set `upperBound = rep(1, length(parameter))`.")
      }

      if (!(length(upperBound) %in% c(1, paramLen))) {
        warning(paste0("`length(upperBound)` should be equal to 1 or length(parameter) ! \n",
                       "We substitute `upperBound` by `rep(upperBound[1], length(parameter))` instead."))
        upperBound <- rep(upperBound[1], paramLen)
      } else if (length(upperBound) == 1) {
        upperBound <- rep(upperBound, paramLen)
      }

      stopifnot(all(upperBound >= lowerBound))


      # nIterOptimization
      if (!is.null(nIterOptimization)) {
        stopifnot(is.numeric(nIterOptimization))
        nIterOptimization <- floor(x = nIterOptimization)
        stopifnot(nIterOptimization >= 2)
      } else {
        nIterOptimization0 <- 100 * exp(paramLen - 1)
        nIterOptimization <- round(x = nIterOptimization0, digits = - (floor(log10(nIterOptimization0)) - 1))
        message(paste0("You do not specify `nIterOptimization`. We set `nIterOptimization = ",
                       nIterOptimization, "`."))
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



      # maximize
      if (is.null(maximize)) {
        maximize <- TRUE
        message(paste0("You do not specify `maximize`. We set `maximize = ",
                       maximize, "`."))
      }
      stopifnot(is.logical(maximize))


      # optimizeType
      if (!is.null(optimizeType)) {
        stopifnot(is.character(optimizeType))
        if (!(optimizeType %in% optimizeTypesOffered)) {
          stop(paste0("We only offer the following optimizing types: \n\t'",
                      paste(optimizeTypesOffered, collapse = "', '"), "'."))
        }
      } else {
        optimizeType <- "stochastic"
        message(paste0("You do not specify `optimizeType`. We set `optimizeType = '",
                       optimizeType, "'`."))
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

      if (optimizeType == "deterministic") {
        if (nMaxEvalPerNode != 1) {
          message(paste0("You set `optimizeType = 'deterministic'`. Thus, we set `nMaxEvalPerNode = 1`."))
        }
        nMaxEvalPerNode <- 1
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
        returnOptimalNodes <- nIterOptimization
        message(paste0("You do not specify `returnOptimalNodes`. We set `returnOptimalNodes = ",
                       returnOptimalNodes, "`."))
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

      # currentTree
      if (!is.null(currentTree)) {
        if (!("tree" %in% class(currentTree))) {
          currentTree <- NULL
          message(paste0("Your `currentTree` object is not `tree` class. We set `currentTree = NULL`."))
        }
      }

      # optimalNodes; optimalHyperParamMat
      if (is.list(optimalNodes)) {
        if (length(optimalNodes) != 0) {
          isNodeVec <- sapply(X = optimalNodes,
                              FUN = function(optimalNode) {
                                "node" %in% class(optimalNode)
                              })

          if (!all(isNodeVec)) {
            optimalNodes <- list()
            message(paste0("Your `optimalNodes` object does not consist of the objects of `node` class. We set `optimalNodes = list()`."))
          }
        }
      } else {
        optimalNodes <- list()
        message(paste0("Your `optimalNodes` object is not `list` class. We set `optimalNodes = list()`."))
      }

      if (length(optimalNodes) == 0) {
        optimalHyperParamMat <- rbind(Iteration_0 = rep(0.5, paramLen))
      } else {
        optimalHyperParamMat <- do.call(what = rbind,
                                        args = lapply(X = optimalNodes,
                                                      FUN = function(eachOptimalNode) {
                                                        eachOptimalNode$xRepresentative
                                                      }))
      }


      # verbose
      if (!is.null(verbose)) {
        verbose <- as.numeric(verbose)
        stopifnot(verbose >= 0)
      } else {
        verbose <- 1
        message(paste0("You do not specify `verbose`. We set `verbose = ",
                       verbose, "`."))
      }


      # widthBase
      widthBase <- log(nIterOptimization * nMaxEvalPerNode / confidenceParam) / 2
      # widthBase <- log(nIterOptimization ^ 2 / confidenceParam) / 2

      # funcScale
      funcScale <- ifelse(test = maximize, yes = 1, no = -1)


      # maximizeFunc
      maximizeFunc <- function(parameter) {
        parameterForOriginalFunc <- parameter * (upperBound - lowerBound) + lowerBound
        maximizeVal <- optimizeFunc(parameterForOriginalFunc, ...) / funcScale

        return(maximizeVal)
      }



      self$parameter <- parameter
      self$paramLen <- paramLen
      self$optimizeFunc <- optimizeFunc
      self$lowerBound <- lowerBound
      self$upperBound <- upperBound
      self$nIterOptimization <- nIterOptimization
      self$nMaxEvalPerNode <- nMaxEvalPerNode
      self$maxDepth <- maxDepth
      self$nChildrenPerExpansion <- nChildrenPerExpansion
      self$confidenceParam <- confidenceParam
      self$maximize <- maximize
      self$optimizeType <- optimizeType
      self$returnOptimalNodes <- returnOptimalNodes
      self$saveTreeNameBase <- saveTreeNameBase
      self$whenToSaveTrees <- whenToSaveTrees
      self$currentTree <- currentTree
      self$optimalNodes <- optimalNodes
      self$optimalHyperParamMat <- optimalHyperParamMat
      self$withCheck <- withCheck
      self$verbose <- verbose

      self$widthBase <- widthBase
      self$funcScale <- funcScale
      self$maximizeFunc <- maximizeFunc
    },


    #' @description
    #' start global optimization of stochastic function by StoSOO
    startOptimization = function() {
      if (is.null(self$currentTree)) {
        if (self$verbose) {
          cat(paste0("------ Iteration ", 1,
                     " of ", self$nIterOptimization, " Will be Performed ------\n"))
        }

        currentTree <- myBreedSimulatR::tree$new(
          xMinRoot = rep(0, self$paramLen),
          xMaxRoot = rep(1, self$paramLen),
          xRepresentativeRoot = rep(0.5, self$paramLen),
          paramLen = self$paramLen,
          nMaxEvalPerNode = self$nMaxEvalPerNode,
          widthBase = self$widthBase,
          maximizeFunc = self$maximizeFunc,
          funcScale = self$funcScale,
          optimizeType = self$optimizeType,
          nChildrenPerExpansion = self$nChildrenPerExpansion,
          maxDepth = self$maxDepth,
          withCheck = self$withCheck,
          verbose = self$verbose
        )
      } else {
        currentTree <- self$currentTree
      }


      iterationCounterOld <- currentTree$iterationCounter - 1
      # saveOptimalNodesOld <- rep(FALSE, length(self$returnOptimalNodes))
      saveTreesOld <- rep(FALSE, length(self$whenToSaveTrees))

      if (!is.null(self$saveTreeNameBase)) {
        splitSaveTreeNameBase <- stringr::str_split(string = self$saveTreeNameBase,
                                                    pattern = "/")[[1]]

        fileNameSaveTree <- here::here(dirname(self$saveTreeNameBase),
                                       paste0(splitSaveTreeNameBase[length(splitSaveTreeNameBase)],
                                              "_StoSOO_tree.rds"))
        fileNameOptimalNodes <- here::here(dirname(self$saveTreeNameBase),
                                           paste0(splitSaveTreeNameBase[length(splitSaveTreeNameBase)],
                                                  "_StoSOO_optimal_nodes_list.rds"))
        fileNameOptimalParams <- here::here(dirname(self$saveTreeNameBase),
                                            paste0(splitSaveTreeNameBase[length(splitSaveTreeNameBase)],
                                                   "_StoSOO_optimal_parameters.csv"))
      }

      while (currentTree$iterationCounter < self$nIterOptimization) {
        if (self$verbose) {
          cat(paste0("\n------ Iteration ", currentTree$iterationCounter + 1,
                     " of ", self$nIterOptimization, " Will be Performed ------\n"))
        }
        currentTree$performOneUpdate()

        currentOptimalNode <- currentTree$evaluateCurrentOptimalNode

        if (!is.null(currentOptimalNode)) {
          if (length(self$optimalNodes) == 0) {
            self$optimalNodes[[paste0("Iteration_", currentTree$iterationCounter)]] <-
              currentOptimalNode
          } else {
            saveOptimalNodes <- !identical(self$optimalNodes[[length(self$optimalNodes)]]$xRepresentative,
                                           currentOptimalNode$xRepresentative)
            if (saveOptimalNodes) {
              cat(paste0("\n------ Iteration ", currentTree$iterationCounter,
                         ": Optimal Parameters Updated ------\n"))
              self$optimalNodes[[paste0("Iteration_", currentTree$iterationCounter)]] <-
                currentOptimalNode

              optimalNodesList <- self$optimalNodes
              optimalHyperParamMat <- do.call(what = rbind,
                                              args = lapply(X = optimalNodesList,
                                                            FUN = function(eachOptimalNode) {
                                                              eachOptimalNode$xRepresentative
                                                            }))
              self$optimalHyperParamMat <- optimalHyperParamMat

              print(optimalHyperParamMat)

              if (!is.null(self$saveTreeNameBase)) {
                saveRDS(object = optimalNodesList,
                        file = fileNameOptimalNodes)

                write.csv(x = optimalHyperParamMat,
                          file = fileNameOptimalParams,
                          row.names = TRUE)
              }
            }
          }
        }
        # saveOptimalNodes <- self$returnOptimalNodes <= currentTree$iterationCounter
        # whereToSave <- which(as.logical(saveOptimalNodes - saveOptimalNodesOld))
        # if (length(whereToSave) >= 1) {
        #   self$optimalNodes[paste0("Iteration_", self$returnOptimalNodes[whereToSave])] <-
        #     rep(list(currentTree$evaluateCurrentOptimalNode), length(whereToSave))
        # }

        saveTrees <- self$whenToSaveTrees <= currentTree$iterationCounter
        whereToSaveTrees <- which(as.logical(saveTrees - saveTreesOld))
        if (length(whereToSaveTrees) >= 1) {
          if (!is.null(self$saveTreeNameBase)) {
            saveRDS(object = currentTree,
                    file = fileNameSaveTree)

            cat(paste0("\n------ Iteration ", currentTree$iterationCounter,
                       ": Current Optimal Parameters ------\n"))
            print(self$optimalHyperParamMat)
          }
        }

        self$currentTree <- currentTree
        # saveOptimalNodesOld <- saveOptimalNodes
        saveTreesOld <- saveTrees
        if (iterationCounterOld == currentTree$iterationCounter) {
          break
        } else {
          iterationCounterOld <- currentTree$iterationCounter
        }
      }

      self$optimalNodeFinal <- currentTree$evaluateCurrentOptimalNode
      self$optimalParameter <- self$lowerBound + (self$upperBound - self$lowerBound) * self$optimalNodeFinal$xRepresentative
      self$optimalValue <- self$funcScale * self$optimalNodeFinal$rewardMean
    },


    #' @description
    #' Display information about the object
    print = function() {
      cat(paste0("Number of Parameters: ", self$paramLen, "\n",
                 "Number of Iterations: ", self$nIterOptimization, "\n",
                 "Number of Maximum Evaluations per Node: ", self$nMaxEvalPerNode, "\n",
                 "Number of Children per Expansion: ", self$nChildrenPerExpansion, "\n",
                 "Max depth: ", self$maxDepth, "\n",
                 "Confidence Parameter: ", round(self$confidenceParam, 4), "\n",
                 "Maximize or Not: ", self$maximize, "\n",
                 "Optimization Type: ", self$optimizeType, "\n",
                 "Return optimal nodes after: ", paste(self$returnOptimalNodes, collapse = " "),
                 " Iterations \n",
                 "Lower Bound: \n"))
      print(self$lowerBound)
      cat(paste0("Upper Bound: \n"))
      print(self$upperBound)
    }
  )
)






#' R6 Class Representing a node in tree for StoSOO
#'
#' @description
#' node object store specific information on each node in tree for StoSOO
#'
# @details
# Details: node object store specific information on each node in tree for StoSOO
#'
#' @export
#' @import R6
node = R6::R6Class(
  classname = "node",

  public = list(
    #' @field depth [numeric] Depth of this node
    depth = NULL,
    #' @field position [numeric] Position of this node
    position = NULL,
    #' @field xMin [numeric] Minimum value of this node (cell)
    xMin = NULL,
    #' @field xMax [numeric] Maximum value of this node (cell)
    xMax = NULL,
    #' @field xRepresentative [numeric] Representative value of this node (cell)
    xRepresentative = NULL,
    #' @field isLeaf [logical] Is this node a leaf of the tree or not
    isLeaf = NULL,
    #' @field isMax [logical] Does this node returns a maximum UCB in the layer or not
    isMax = NULL,
    #' @field isEvaluationFinished [logical] Is evaluation of this node finished or not
    isEvaluationFinished = NULL,
    #' @field nEvals [numeric] Number of evaluations for this node
    nEvals = NULL,
    #' @field rewards [numeric] Rewards evaluated (returned) by `maximizeFunc` for this node
    rewards = NULL,
    #' @field rewardMean [numeric] Empirical average of `rewards`
    rewardMean = NULL,
    #' @field ucbValue [numeric] Upper confidence bound of rewards
    ucbValue = NULL,
    #' @field nMaxEvalPerNode [numeric] Maximum number of evaluations per leaf
    nMaxEvalPerNode = NULL,
    #' @field widthBase [numeric] Base of width of the estimates of rewards
    widthBase = NULL,
    #' @field maximizeFunc [function] Function to be maximized given parameters scaled from 0 to 1.
    maximizeFunc = NULL,
    #' @field funcScale [numeric] Scale for function to be optimized. If `maximize = TRUE`, `funcScale = 1`, and else `funcScale = -1`.
    funcScale = NULL,
    #' @field withCheck [logical] Check arguments for `node`, `layer`, and `tree` class or not
    withCheck = NULL,
    #' @field verbose [logical] Display information
    verbose = NULL,

    #' @field childrenNodes [list] List of children nodes for this node (after the node is expanded)
    childrenNodes = NULL,


    #' @description Create a new node object
    #' @param depth [numeric] Depth of this node
    #' @param position [numeric] Position of this node
    #' @param xMin [numeric] Minimum value of this node (cell)
    #' @param xMax [numeric] Maximum value of this node (cell)
    #' @param xRepresentative [numeric] Representative value of this node (cell)
    #' @param isLeaf [logical] Is this node a leaf of the tree or not
    #' @param isMax [logical] Does this node returns a maximum UCB in the layer or not
    #' @param isEvaluationFinished [logical] Is evaluation of this node finished or not
    #' @param nEvals [numeric] Number of evaluations for this node
    #' @param rewards [numeric] Rewards evaluated (returned) by `maximizeFunc` for this node
    #' @param rewardMean [numeric] Empirical average of `rewards`
    #' @param ucbValue [numeric] Upper confidence bound of rewards
    #' @param nMaxEvalPerNode [numeric] Maximum number of evaluations per leaf
    #' @param widthBase [numeric] Base of width of the estimates of rewards
    #' @param maximizeFunc [function] Function to be maximized given parameters scaled from 0 to 1.
    #' @param funcScale [numeric] Scale for function to be optimized. If `maximize = TRUE`, `funcScale = 1`, and else `funcScale = -1`.
    #' @param withCheck [logical] Check arguments for `node`, `layer`, and `tree` class or not
    #' @param verbose [logical] Display information
    #'
    initialize = function(
    depth,
    position,
    xMin,
    xMax,
    xRepresentative = NULL,
    isLeaf = TRUE,
    isMax = FALSE,
    isEvaluationFinished = FALSE,
    nEvals = 0,
    rewards = c(),
    rewardMean = 0,
    ucbValue = 0,
    nMaxEvalPerNode,
    widthBase,
    maximizeFunc,
    funcScale,
    withCheck = FALSE,
    verbose = TRUE
    ) {
      if (withCheck) {
        # depth
        stopifnot(is.numeric(depth))
        depth <- floor(x = depth)
        stopifnot(depth >= 1)

        # position
        stopifnot(is.numeric(position))
        position <- floor(x = position)
        stopifnot(position >= 1)

        # xMin
        stopifnot(is.numeric(xMin))
        stopifnot(all(xMin >= 0))

        # xMax
        stopifnot(is.numeric(xMax))
        stopifnot(all(xMax >= xMin))
        stopifnot(all(xMax <= 1))

        # xRepresentative
        if (is.null(xRepresentative)) {
          xRepresentative <- (xMin + xMax) / 2
        }

        # isLeaf
        stopifnot(is.logical(isLeaf))

        # isMax
        stopifnot(is.logical(isMax))

        # nEvals
        stopifnot(is.numeric(nEvals))
        nEvals <- floor(x = nEvals)
        stopifnot(nEvals >= 0)
        # stopifnot(nEvals <= nMaxEvalPerNode)

        # isEvaluationFinished
        stopifnot(is.logical(isEvaluationFinished))
        if (nEvals >= nMaxEvalPerNode) {
          isEvaluationFinished <- TRUE
        }

        # rewards
        if (nEvals >= 1) {
          stopifnot(is.numeric(rewards))
        }
        stopifnot(length(rewards) == nEvals)

        # rewardMean
        if (is.null(rewardMean)) {
          if (nEvals >= 1) {
            rewardMean <- mean(x = rewards, na.rm = TRUE)
          } else {
            rewardMean <- 0
          }
        } else {
          stopifnot(is.numeric(rewardMean))
        }

        # ucbValue
        if (is.null(ucbValue)) {
          if (nEvals >= 1) {
            ucbValue <- rewardMean + sqrt(x = widthBase / nEvals)
          } else {
            ucbValue <- 0
          }
        } else {
          stopifnot(is.numeric(ucbValue))
        }

        # maximizeFunc
        stopifnot(is.function(maximizeFunc))

        # funcScale
        stopifnot(is.numeric(funcScale))
        funcScale <- sign(funcScale)


        # verbose
        verbose <- as.logical(verbose)
      }

      self$depth <- depth
      self$position <- position
      self$xMin <- xMin
      self$xMax <- xMax
      self$xRepresentative <- xRepresentative
      self$isLeaf <- isLeaf
      self$isMax <- isMax
      self$isEvaluationFinished <- isEvaluationFinished
      self$nEvals <- nEvals
      self$rewards <- rewards
      self$rewardMean <- rewardMean
      self$ucbValue <- ucbValue
      self$nMaxEvalPerNode <- nMaxEvalPerNode
      self$widthBase <- widthBase
      self$maximizeFunc <- maximizeFunc
      self$funcScale <- funcScale
      self$withCheck <- withCheck
      self$verbose <- verbose

      self$performEvaluation()
    },


    #' @description
    #' perform evaluation for this node (evaluate function and compute reward and UCB values)
    performEvaluation = function() {
      funcScale <- self$funcScale

      if (!self$isEvaluationFinished) {
        if (self$verbose) {
          cat(paste0("Perform evaluation for (",
                     self$depth, ", ", self$position,
                     "), ", self$nEvals + 1,
                     " times. \n x = ",
                     paste(round(self$xRepresentative, 5), collapse = " "),
                     ";  f(x) = "))
        }

        newReward <- self$maximizeFunc(parameter = self$xRepresentative)

        if (self$verbose) {
          cat(paste0(funcScale * round(newReward, 5), ". \n"))
        }

        self$nEvals <- self$nEvals + 1
        self$isEvaluationFinished <- self$nEvals >= self$nMaxEvalPerNode
        self$rewards <- c(self$rewards, newReward)
        self$rewardMean <- mean(x = self$rewards, na.rm = TRUE)
        self$ucbValue <- self$rewardMean + sqrt(x = self$widthBase / self$nEvals)

        if (self$isEvaluationFinished & self$verbose) {
          cat(paste0("Evaluation finished for (",
                     self$depth, ", ", self$position,
                     "). \n x = ",
                     paste(round(self$xRepresentative, 5), collapse = " "),
                     ";  reward = ", funcScale * round(self$rewardMean, 5),
                     ";  ucb = ", funcScale * round(self$ucbValue, 5), ". \n"))
        }
      } else {
        if (self$verbose) {
          cat(paste0("Evaluation finished for (",
                     self$depth, ", ", self$position,
                     "). \n x = ",
                     paste(round(self$xRepresentative, 5), collapse = " "),
                     ";  reward = ", funcScale * round(self$rewardMean, 5),
                     ";  ucb = ", funcScale * round(self$ucbValue, 5), ". \n"))
        }
      }
    },


    #' @description
    #' expand this node and create new children for the node
    #' @param nChildrenPerExpansion [numeric] Number of children per expansion
    #'
    expandNewNode = function(nChildrenPerExpansion) {
      if (is.null(self$childrenNodes)) {
        if (self$isLeaf & self$isMax & self$isEvaluationFinished) {
          xMin <- self$xMin
          xMax <- self$xMax

          splitDim <- which.max(xMax - xMin)

          if (self$verbose) {
            cat(paste0("Expand new ", nChildrenPerExpansion,
                       " children for (",
                       self$depth, ", ", self$position,
                       "). \n"))
          }

          childrenNodes <- sapply(X = 1:nChildrenPerExpansion,
                                  FUN = function(childrenNo) {
                                    newXMin <- xMin
                                    newXMax <- xMax

                                    newXMin[splitDim] <- xMin[splitDim] + (xMax - xMin)[splitDim] * (childrenNo - 1) / nChildrenPerExpansion
                                    newXMax[splitDim] <- xMin[splitDim] + (xMax - xMin)[splitDim] * childrenNo / nChildrenPerExpansion
                                    newXRepresentative <- (newXMin + newXMax) / 2
                                    newPosition <- nChildrenPerExpansion * (self$position - 1) + childrenNo

                                    if (all(round(newXRepresentative, 5) == round(self$xRepresentative, 5))) {
                                      newNEvals <- self$nEvals
                                      newRewards <- self$rewards
                                      newRewardMean <- self$rewardMean
                                      newUcbValue <- self$ucbValue
                                    } else {
                                      newNEvals <- 0
                                      newRewards <- c()
                                      newRewardMean <- 0
                                      newUcbValue <- 0
                                    }

                                    newNode <- myBreedSimulatR::node$new(
                                      depth = self$depth + 1,
                                      position = newPosition,
                                      xMin = newXMin,
                                      xMax = newXMax,
                                      xRepresentative = newXRepresentative,
                                      isLeaf = TRUE,
                                      isMax = FALSE,
                                      isEvaluationFinished = FALSE,
                                      nEvals = newNEvals,
                                      rewards = newRewards,
                                      rewardMean = newRewardMean,
                                      ucbValue = newUcbValue,
                                      nMaxEvalPerNode = self$nMaxEvalPerNode,
                                      widthBase = self$widthBase,
                                      maximizeFunc = self$maximizeFunc,
                                      funcScale = self$funcScale,
                                      withCheck = self$withCheck,
                                      verbose = self$verbose
                                    )

                                    return(newNode)
                                  },
                                  simplify = FALSE)

          self$isLeaf <- FALSE
          self$isEvaluationFinished <- TRUE
          self$childrenNodes <- childrenNodes

          if (self$verbose) {
            cat(paste0("Finished expansion of new ", nChildrenPerExpansion,
                       " children for (",
                       self$depth, ", ", self$position,
                       "). \n"))
          }

        }
      }
    },


    #' @description
    #' Display information about the object
    print = function() {
      cat(paste0("Depth: ", self$depth, "\n",
                 "Position: ", self$position, "\n",
                 "Minimum: ", paste(round(self$xMin, 4), collapse = " "), "\n",
                 "Maximum: ", paste(round(self$xMax, 4), collapse = " "), "\n",
                 "Representative: ", paste(round(self$xRepresentative, 4), collapse = " "), "\n",
                 "Leaf or Not: ", self$isLeaf, "\n",
                 "Maximum Node or Not: ", self$isMax, "\n",
                 "Evaluation Finished or Not: ", self$isEvaluationFinished, "\n",
                 "Number of Evaluations: ", self$nEvals, "\n",
                 "Rewards: ", paste(self$funcScale * round(self$rewards, 4), collapse = " "), "\n",
                 "Average of Rewards: ", self$funcScale * round(self$rewardMean, 4), "\n",
                 "UCB: ", self$funcScale * round(self$ucbValue, 4), "\n"))
    }
  ))










#' R6 Class Representing a layer in tree for StoSOO
#'
#' @description
#' layer object store specific information on each layer in tree for StoSOO
#'
# @details
# Details: layer object store specific information on each layer in tree for StoSOO
#'
#' @export
#' @import R6
layer = R6::R6Class(
  classname = "layer",


  public = list(
    #' @field nodesList [list] List of nodes (`node` class object) in this layer
    nodesList = NULL,
    #' @field depth [numeric] Depth of this layer
    depth = NULL,
    #' @field optimizeType [character] Either 'deterministic' for optimizing a deterministic function or 'stochastic' for a stochastic one
    optimizeType = NULL,
    #' @field nChildrenPerExpansion [numeric] Number of children per expansion
    nChildrenPerExpansion = NULL,
    #' @field nNodes [numeric] Number nodes in this layer
    nNodes = NULL,
    #' @field positions [numeric] Positions of the nodes in this layer
    positions = NULL,
    #' @field ucbValues [numeric] Upper confidence bounds of the nodes in this layer
    ucbValues = NULL,
    #' @field rewardMeans [numeric] Empirical averages of `rewards` of the nodes in this layer
    rewardMeans = NULL,
    #' @field isLeaves [numeric] Are the nodes in this layer leaves of the tree or not
    isLeaves = NULL,
    #' @field withCheck [logical] Check arguments for `node`, `layer`, and `tree` class or not
    withCheck = NULL,

    #' @field maxUcbValues [numeric] Maximum UCB value (or `rewardMean` for 'deterministic' function) of the leaf node in the layer
    maxUcbValues = NULL,
    #' @field positionMaxUcbValues [numeric] Position of the leaf node which shows maximum UCB value (or `rewardMean` for 'deterministic' function)
    positionMaxUcbValues = NULL,
    #' @field maxUcbValuesSoFar [numeric] Maximum UCB value for the layers shallower than this layer
    maxUcbValuesSoFar = NULL,



    #' @description Create a new layer object
    #' @param nodesList [list] List of nodes (`node` class object) in this layer
    #' @param depth [numeric] Depth of this layer
    #' @param optimizeType [character] Either 'deterministic' for optimizing a deterministic function or 'stochastic' for a stochastic one
    #' @param nChildrenPerExpansion [numeric] Number of children per expansion
    #' @param withCheck [logical] Check arguments for `node`, `layer`, and `tree` class or not
    initialize = function(
    nodesList,
    depth = NULL,
    optimizeType,
    nChildrenPerExpansion,
    withCheck = FALSE
    ) {
      if (withCheck) {
        # some definitions
        optimizeTypesOffered <- c("stochastic", "deterministic")


        # optimizeType
        if (!is.null(optimizeType)) {
          stopifnot(is.character(optimizeType))
          if (!(optimizeType %in% optimizeTypesOffered)) {
            stop(paste0("We only offer the following optimizing types: \n\t'",
                        paste(optimizeTypesOffered, collapse = "', '"), "'."))
          }
        } else {
          optimizeType <- "stochastic"
          message(paste0("You do not specify `optimizeType`. We set `optimizeType = '",
                         optimizeType, "'`."))
        }


        # nodesList
        if (!("node" %in% class(nodesList))) {
          stopifnot(is.list(nodesList))
          stopifnot(all(unlist(lapply(nodesList, function(eachNode) "node" %in% class(eachNode)))))
        } else {
          nodesList <- list(nodesList)
        }
      }
      positions <- unlist(x = lapply(X = nodesList,
                                     FUN = function(eachNode) eachNode$position))
      if (any(duplicated(x = positions))) {
        message("Duplicated positions... Is the positions of the nodes correct?")
      }


      depths <- unlist(x = lapply(X = nodesList,
                                  FUN = function(eachNode) eachNode$depth))
      stopifnot(length(unique(depths)) == 1)

      nNodes <- length(x = nodesList)

      if (withCheck) {
        # depth
        if (!is.null(depth)) {
          stopifnot(is.numeric(depth))
          depth <- floor(x = depth)
          stopifnot(depth >= 1)
          stopifnot(depth == unique(depths))
        } else {
          depth <- unique(depth)
        }
      }

      # ucbValues
      ucbValues <- unlist(x = lapply(X = nodesList,
                                     FUN = function(eachNode) eachNode$ucbValue))
      names(ucbValues) <- positions


      # rewardMeans
      rewardMeans <- unlist(x = lapply(X = nodesList,
                                       FUN = function(eachNode) eachNode$rewardMean))
      names(rewardMeans) <- positions


      # isLeaves
      isLeaves <- unlist(x = lapply(X = nodesList,
                                    FUN = function(eachNode) eachNode$isLeaf))
      names(isLeaves) <- positions

      self$nodesList <- nodesList
      self$depth <- depth
      self$nNodes <- nNodes
      self$optimizeType <- optimizeType
      self$positions <- positions
      self$ucbValues <- ucbValues
      self$rewardMeans <- rewardMeans
      self$isLeaves <- isLeaves
      self$nChildrenPerExpansion <- nChildrenPerExpansion
      self$withCheck <- withCheck

      self$sortedByPosition()
    },


    #' @description
    #' Add new nodes to the layer object
    #' @param newNodesList [list] List of new nodes (`node` class object) to be added to this layer
    #'
    addNodes = function(
    newNodesList = NULL
    ) {
      nodesList <- self$nodesList

      # newNodesList
      if (!("node" %in% class(newNodesList))) {
        stopifnot(is.list(newNodesList))
        stopifnot(all(unlist(lapply(newNodesList, function(eachNode) "node" %in% class(eachNode)))))
      } else {
        newNodesList <- list(newNodesList)
      }

      newPositions <- unlist(x = lapply(X = newNodesList,
                                        FUN = function(eachNode) eachNode$position))
      if (any(duplicated(x = c(self$positions, newPositions)))) {
        message("Duplicated positions... Is the positions of the nodes correct?")
      }

      newDepths <- unlist(x = lapply(X = newNodesList,
                                     FUN = function(eachNode) eachNode$depth))
      stopifnot(length(unique(newDepths)) == 1)

      nNewNodes <- length(x = newNodesList)


      # depth
      stopifnot(self$depth == unique(newDepths))


      # ucbValues
      newUcbValues <- unlist(x = lapply(X = newNodesList,
                                        FUN = function(eachNode) eachNode$ucbValue))
      names(newUcbValues) <- newPositions

      # rewardMeans
      newRewardMeans <- unlist(x = lapply(X = newNodesList,
                                          FUN = function(eachNode) eachNode$rewardMean))
      names(newRewardMeans) <- newPositions


      # isLeaves
      newIsLeaves <- unlist(x = lapply(X = newNodesList,
                                       FUN = function(eachNode) eachNode$isLeaf))
      names(newIsLeaves) <- newPositions

      self$nodesList <- c(self$nodesList, newNodesList)
      self$nNodes <- self$nNodes + nNewNodes
      self$positions <- c(self$positions, newPositions)
      self$ucbValues <- c(self$ucbValues, newUcbValues)
      self$rewardMeans <- c(self$rewardMeans, newRewardMeans)
      self$isLeaves <- c(self$isLeaves, newIsLeaves)

      self$sortedByPosition()
    },


    #' @description
    #' Sort nodes in this layer by position
    sortedByPosition = function() {
      positions <- self$positions
      positionsOrd <- order(positions, decreasing = FALSE)

      self$nodesList <- self$nodesList[positionsOrd]
      self$positions <- self$positions[positionsOrd]
      self$ucbValues <- self$ucbValues[positionsOrd]
      self$rewardMeans <- self$rewardMeans[positionsOrd]
      self$isLeaves <- self$isLeaves[positionsOrd]
    },


    #' @description
    #' Evaluate maximum value of the leaf node in this layer
    evaluateMaximum = function() {
      rewardMeans <- self$rewardMeans
      ucbValues <- self$ucbValues
      isLeaves <- self$isLeaves

      if (any(isLeaves)) {
        if (self$optimizeType == "stochastic") {
          maxUcbValues <- max(ucbValues[isLeaves], na.rm = TRUE)
          positionMaxUcbValues <- self$positions[which(ucbValues == maxUcbValues)]
        } else {
          maxUcbValues <- max(rewardMeans[isLeaves], na.rm = TRUE)
          positionMaxUcbValues <- self$positions[which(rewardMeans == maxUcbValues)]
        }

        self$nodesList[isLeaves] <- lapply(X = self$nodesList[isLeaves],
                                           FUN = function(eachNode) {
                                             eachNode$isMax <- ifelse(test = eachNode$position %in% positionMaxUcbValues,
                                                                      yes = TRUE, no = FALSE)

                                             return(eachNode)
                                           })

        self$maxUcbValues <- maxUcbValues
        self$positionMaxUcbValues <- positionMaxUcbValues
      }
    },


    #' @description
    #' Update layer information once
    #' @param maxUcbValuesSoFar [numeric] Maximum UCB value for the layers shallower than this layer
    #'
    performOneAction = function(maxUcbValuesSoFar) {
      if (any(self$isLeaves)) {
        self$evaluateMaximum()
        positions <- self$positions
        maxUcbValues <- self$maxUcbValues
        positionMaxUcbValues <- self$positionMaxUcbValues

        for (positionId in 1:length(positionMaxUcbValues)) {
          position <- positionMaxUcbValues[positionId]

          if (maxUcbValues >= maxUcbValuesSoFar) {
            nodeNow <- self$nodesList[[which(positions == position)]]
            if (nodeNow$nEvals < nodeNow$nMaxEvalPerNode) {
              nodeNow$performEvaluation()
              self$ucbValues[positions == position] <- nodeNow$ucbValue
              self$rewardMeans[positions == position] <- nodeNow$rewardMean
            } else {
              nodeNow$expandNewNode(nChildrenPerExpansion = self$nChildrenPerExpansion)
              self$isLeaves[positions == position] <- nodeNow$isLeaf
            }

            self$nodesList[[which(positions == position)]] <- nodeNow
            maxUcbValuesSoFar <- maxUcbValues
          }

        }
      }

      self$maxUcbValuesSoFar <- maxUcbValuesSoFar
    },


    #' @description
    #' Display information about the object
    print = function() {
      self$evaluateMaximum()

      cat(paste0("Number of Nodes: ", self$nNodes, "\n",
                 "Depth: ", self$depth, "\n",
                 "Positions: ", paste(self$positions, collapse = " "), "\n",
                 "UCB Values: ", paste(round(self$ucbValues, 4), collapse = " "), "\n",
                 "Mean of Rewards: ", paste(round(self$rewardMeans, 4), collapse = " "), "\n",
                 "Leaves or Not: ", paste(self$isLeaves, collapse = " "), "\n",
                 "Maximum of UCB: ", round(self$maxUcbValues, 4), "\n",
                 "Position of Maximum: ", self$positionMaxUcbValues, "\n",
                 "Optimization Type: ", self$optimizeType, "\n",
                 "Number of Children Per Expansion: ", self$nChildrenPerExpansion, "\n"))
    }
  )
)








#' R6 Class Representing a tree for StoSOO
#'
#' @description
#' tree object store specific information on tree for StoSOO
#'
# @details
# Details: tree object store specific information on tree for StoSOO
#'
#' @export
#' @import R6
tree <- R6::R6Class(
  classname = "tree",


  public = list(
    #' @field xMinRoot [numeric] Minimum value of the root node (cell) in the tree
    xMinRoot = NULL,
    #' @field xMaxRoot [numeric] Maximum value of the root node (cell) in the tree
    xMaxRoot = NULL,
    #' @field xRepresentativeRoot [numeric] Representative value of the root node (cell) in the tree
    xRepresentativeRoot = NULL,
    #' @field paramLen [numeric] Number of parameters
    paramLen = NULL,
    #' @field nMaxEvalPerNode [numeric] Maximum number of evaluations per leaf
    nMaxEvalPerNode = NULL,
    #' @field widthBase [numeric] Base of width of the estimates of rewards
    widthBase = NULL,
    #' @field maximizeFunc [function] Function to be maximized given parameters scaled from 0 to 1.
    maximizeFunc = NULL,
    #' @field funcScale [numeric] Scale for function to be optimized. If `maximize = TRUE`, `funcScale = 1`, and else `funcScale = -1`.
    funcScale = NULL,
    #' @field optimizeType [character] Either 'deterministic' for optimizing a deterministic function or 'stochastic' for a stochastic one
    optimizeType = NULL,
    #' @field nChildrenPerExpansion [numeric] Number of children per expansion
    nChildrenPerExpansion = NULL,
    #' @field maxDepth [numeric] Maximum depth of the tree
    maxDepth = NULL,
    #' @field withCheck [logical] Check arguments for `node`, `layer`, and `tree` class or not
    withCheck = NULL,
    #' @field verbose [logical] Display information
    verbose = NULL,

    #' @field iterationCounter [numeric] Number of evaluations for the target function done in this tree object
    iterationCounter = NULL,
    #' @field rootNode [numeric] Root node information (`node` class object)
    rootNode = NULL,
    #' @field rootLayer [numeric] Root layer information (`layer` class object)
    rootLayer = NULL,
    #' @field layersList [numeric] List of layers (`layer` class object) in this tree
    layersList = NULL,
    #' @field depths [numeric] Depths of the layers in this tree
    depths = NULL,
    #' @field maxUcbValuesSoFar [numeric] Maximum UCB value for the layers shallower than this layer
    maxUcbValuesSoFar = NULL,



    #' @description Create a new tree object
    #' @param xMinRoot [numeric] Minimum value of the root node (cell) in the tree
    #' @param xMaxRoot [numeric] Maximum value of the root node (cell) in the tree
    #' @param xRepresentativeRoot [numeric] Representative value of the root node (cell) in the tree
    #' @param paramLen [numeric] Number of parameters
    #' @param nMaxEvalPerNode [numeric] Maximum number of evaluations per leaf
    #' @param widthBase [numeric] Base of width of the estimates of rewards
    #' @param maximizeFunc [function] Function to be maximized given parameters scaled from 0 to 1.
    #' @param funcScale [numeric] Scale for function to be optimized. If `maximize = TRUE`, `funcScale = 1`, and else `funcScale = -1`.
    #' @param optimizeType [character] Either 'deterministic' for optimizing a deterministic function or 'stochastic' for a stochastic one
    #' @param nChildrenPerExpansion [numeric] Number of children per expansion
    #' @param maxDepth [numeric] Maximum depth of the tree
    #' @param withCheck [logical] Check arguments for `node`, `layer`, and `tree` class or not
    #' @param verbose [logical] Display information
    initialize = function(
    xMinRoot = NULL,
    xMaxRoot = NULL,
    xRepresentativeRoot = NULL,
    paramLen = NULL,
    nMaxEvalPerNode,
    widthBase,
    maximizeFunc,
    funcScale,
    optimizeType,
    nChildrenPerExpansion,
    maxDepth,
    withCheck = FALSE,
    verbose = TRUE
    ) {
      if (withCheck) {
        # some definitions
        optimizeTypesOffered <- c("stochastic", "deterministic")


        # paramLen
        if (!is.null(paramLen)) {
          stopifnot(is.numeric(paramLen))
          paramLen <- floor(paramLen)
          stopifnot(paramLen >= 1)
        }

        # xMinRoot
        if (!is.null(xMinRoot)) {
          stopifnot(is.numeric(xMinRoot))
          stopifnot(is.vector(xMinRoot))
        } else {
          if (!is.null(paramLen)) {
            xMinRoot <- rep(0, paramLen)
            message("You do not specify `xMinRoot`. We set `xMinRoot = rep(0, paramLen)`.")
          } else {
            stop("You should set `paramLen` when `xMinRoot` is NULL")
          }
        }

        if (!is.null(paramLen)) {
          if (!(length(xMinRoot) %in% c(1, paramLen))) {
            warning(paste0("`length(xMinRoot)` should be equal to 1 or paramLen ! \n",
                           "We substitute `xMinRoot` by `rep(xMinRoot[1], paramLen)` instead."))
            xMinRoot <- rep(xMinRoot[1], paramLen)
          } else if (length(xMinRoot) == 1) {
            xMinRoot <- rep(xMinRoot, paramLen)
          }
        } else {
          paramLen <- length(xMinRoot)
        }


        # xMaxRoot
        if (!is.null(xMaxRoot)) {
          stopifnot(is.numeric(xMaxRoot))
          stopifnot(is.vector(xMaxRoot))
        } else {
          xMaxRoot <- rep(1, paramLen)
          message("You do not specify `xMaxRoot`. We set `xMaxRoot = rep(1, paramLen)`.")
        }

        if (!(length(xMaxRoot) %in% c(1, paramLen))) {
          warning(paste0("`length(xMaxRoot)` should be equal to 1 or paramLen ! \n",
                         "We substitute `xMaxRoot` by `rep(xMaxRoot[1], paramLen)` instead."))
          xMaxRoot <- rep(xMaxRoot[1], paramLen)
        } else if (length(xMaxRoot) == 1) {
          xMaxRoot <- rep(xMaxRoot, paramLen)
        }

        stopifnot(all(xMaxRoot >= xMinRoot))


        # xRepresentativeRoot
        if (is.null(xRepresentativeRoot)) {
          xRepresentativeRoot <- (xMinRoot + xMaxRoot) / 2
        }

        # maximizeFunc
        stopifnot(is.function(maximizeFunc))


        # optimizeType
        if (!is.null(optimizeType)) {
          stopifnot(is.character(optimizeType))
          if (!(optimizeType %in% optimizeTypesOffered)) {
            stop(paste0("We only offer the following optimizing types: \n\t'",
                        paste(optimizeTypesOffered, collapse = "', '"), "'."))
          }
        } else {
          optimizeType <- "stochastic"
          message(paste0("You do not specify `optimizeType`. We set `optimizeType = '",
                         optimizeType, "'`."))
        }

        # verbose
        verbose <- as.logical(verbose)
      }

      # save arguments
      self$xMinRoot <- xMinRoot
      self$xMaxRoot <- xMaxRoot
      self$xRepresentativeRoot <- xRepresentativeRoot
      self$paramLen <- paramLen
      self$nMaxEvalPerNode <- nMaxEvalPerNode
      self$widthBase <- widthBase
      self$maximizeFunc <- maximizeFunc
      self$funcScale <- funcScale
      self$nChildrenPerExpansion <- nChildrenPerExpansion
      self$maxDepth <- maxDepth
      self$optimizeType <- optimizeType
      self$withCheck <- withCheck
      self$verbose <- verbose

      rootNode <- myBreedSimulatR::node$new(depth = 1,
                                            position = 1,
                                            xMin = xMinRoot,
                                            xMax = xMaxRoot,
                                            xRepresentative = xRepresentativeRoot,
                                            isLeaf = TRUE,
                                            isMax = FALSE,
                                            isEvaluationFinished = FALSE,
                                            nEvals = 0,
                                            rewards = c(),
                                            rewardMean = 0 ,
                                            ucbValue = 0,
                                            nMaxEvalPerNode = nMaxEvalPerNode,
                                            widthBase = widthBase,
                                            maximizeFunc = maximizeFunc,
                                            funcScale = funcScale,
                                            withCheck = withCheck,
                                            verbose = verbose)

      rootLayer <- myBreedSimulatR::layer$new(nodesList = list(rootNode),
                                              depth = 1,
                                              nChildrenPerExpansion = nChildrenPerExpansion,
                                              optimizeType = optimizeType,
                                              withCheck = withCheck)

      layersList <- list(rootLayer)


      depths <- 1

      maxUcbValuesSoFar <- - Inf

      self$iterationCounter <- 1
      self$rootNode <- rootNode
      self$rootLayer <- rootLayer
      self$layersList <- layersList
      self$depths <- depths
      self$maxUcbValuesSoFar <- maxUcbValuesSoFar
    },


    #' @description
    #' Update the tree information once
    performOneUpdate = function() {
      self$maxUcbValuesSoFar <- - Inf
      layersList <- self$layersList

      for (depth in 1:min(max(self$depths), self$maxDepth)) {
        layerNow <- layersList[[depth]]
        layerNow$evaluateMaximum()
        isLeavesForMax <- unlist(lapply(X = layerNow$nodesList[which(layerNow$positions %in% layerNow$positionMaxUcbValues)],
                                        FUN = function(eachNode) eachNode$isLeaf))
        layerNow$performOneAction(maxUcbValuesSoFar = self$maxUcbValuesSoFar)

        self$maxUcbValuesSoFar <- layerNow$maxUcbValuesSoFar
        maxNodes <- layerNow$nodesList[which(layerNow$positions %in% layerNow$positionMaxUcbValues)]

        for (nodeNo in 1:length(maxNodes)) {
          if (!is.null(maxNodes[[nodeNo]]$childrenNodes)) {
            if (isLeavesForMax[nodeNo]) {
              newChildrenNodes <- maxNodes[[nodeNo]]$childrenNodes
              newChildrenDepth <- newChildrenNodes[[1]]$depth

              if (length(layersList) < newChildrenDepth) {
                newLayer <- myBreedSimulatR::layer$new(nodesList = newChildrenNodes,
                                                       depth = newChildrenDepth,
                                                       nChildrenPerExpansion = self$nChildrenPerExpansion,
                                                       optimizeType = self$optimizeType,
                                                       withCheck = self$withCheck)

                layersList <- c(layersList, list(newLayer))
                self$depths <- newChildrenDepth
              } else {
                layersList[[newChildrenDepth]]$addNodes(newNodesList = newChildrenNodes)
              }
              self$iterationCounter <- self$iterationCounter + (self$nChildrenPerExpansion %/% 2) * 2
            }
          } else {
            if (layerNow$maxUcbValues >= self$maxUcbValuesSoFar) {
              self$iterationCounter <- self$iterationCounter + 1
            }
          }
        }
      }

      self$layersList <- layersList
    },


    #' @description
    #' Display information about the object
    print = function() {
      cat(paste0("Number of Parameters: ", self$paramLen, "\n",
                 "Max Depth: ", max(self$depths), "\n",
                 "Number of Iterations: ", self$iterationCounter, "\n",
                 "Number of Maximum Evaluations per Node: ", self$nMaxEvalPerNode, "\n",
                 "Optimization Type: ", self$optimizeType, "\n",
                 "Number of Children per Expansion: ", self$nChildrenPerExpansion, "\n",
                 "Maximize or Not: ", as.logical(self$funcScale + 1), "\n"))
    }

  ),


  active = list(
    #' @field evaluateCurrentOptimalNode [node] Evaluate the optimal node in the current tree
    evaluateCurrentOptimalNode = function() {
      layersList <- self$layersList
      if (length(layersList) >= 2) {
        deepestLayer <- layersList[[length(layersList) - 1]]
        whichIsLeaves <- unlist(lapply(X = deepestLayer$nodesList,
                                       FUN = function(eachNode) eachNode$isLeaf))
        deepestRewardMeans <- deepestLayer$rewardMeans[!whichIsLeaves]

        currentOptimalNode <- (deepestLayer$nodesList[!whichIsLeaves])[[which.max(deepestRewardMeans)]]$clone(deep = FALSE)
      } else {
        currentOptimalNode <- NULL
      }

      return(currentOptimalNode)
    }
  )
)
