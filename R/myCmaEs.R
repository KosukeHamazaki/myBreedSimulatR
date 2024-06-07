# Author: Kosuke Hamazaki hamazaki@ut-biomet.org
# 2023 The University of Tokyo
#
# Description:
# Definition of cmaES class (, node class, layer class, and tree class)




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
cmaES <- R6::R6Class(
  classname = "cmaES",

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
    #' @field maximize [logical] If TRUE, performs maximization
    maximize = NULL,
    #' @field stopFitness [numeric] Stop if function value is smaller than or equal to stopfitness. This is the only way for the CMA-ES to "converge".
    stopFitness = NULL,
    #' @field keepBest [logical] Return the best overall solution and not the best solution in the last population. Defaults to TRUE.
    keepBest = NULL,
    #' @field sigmaInit [numeric] Initial variance estimates. Can be a single number or a vector of length D, where D is the dimension of the parameter space.
    sigmaInit = NULL,
    #' @field lambda [numeric] Number of offspring. Must be greater than or equal to mu.
    lambda = NULL,
    #' @field mu [numeric] Population size.
    mu = NULL,
    #' @field recombWeights [numeric] Recombination weights.
    recombWeights = NULL,
    #' @field etaRankMuAuto [logical] Update learning rate for rank-mu update automatically.
    etaRankMuAuto = NULL,
    #' @field etaRankMu [numeric] Learning rate for rank-mu update.
    etaRankMu = NULL,
    #' @field etaRankOne [numeric] Learning rate for rank-one update.
    etaRankOne = NULL,
    #' @field constStep [numeric] Cumulation constant for step-size.
    constStep = NULL,
    #' @field constCov [numeric] Cumulation constant for covariance matrix.
    constCov = NULL,
    #' @field dampStep [numeric] Damping for step-size.
    dampStep = NULL,
    #' @field maximize [logical] Is the function `optimizeFunc` vectorized?
    vectorized = NULL,
    #' @field saveStepSize [logical] Save current step size in each iteration.
    saveStepSize = NULL,
    #' @field savePC [logical] Save current principle components of the covariance matrix progCov in each iteration.
    savePC = NULL,
    #' @field saveCurrentPop [logical] Save current population in each iteration.
    saveCurrentPop = NULL,
    #' @field saveValue [logical] Save function values of the current population in each iteration.
    saveValue = NULL,
    #' @field scaleTorelance [numeric] Tolerance for scale.
    scaleTorelance = NULL,
    #' @param saveObjNameBase [character] Base name of the `cmaES` object to be saved
    saveObjNameBase = NULL,
    #' @field whenToSaveObj [numeric] When (how many iterations) to save the `cmaES` object
    whenToSaveObj = NULL,
    #' @field verbose [logical] Display information
    verbose = NULL,
    #' @param fileNameSaveObj [character] File name of the `cmaES` object to be saved
    fileNameSaveObj = NULL,
    #' @param fileNameOptimalParams [character] File name of the optimal parameters to be saved
    fileNameOptimalParams = NULL,
    #' @param saveObjOld [logical] Finish saving object or not.
    saveObjOld = NULL,
    #' @field muEff [numeric] Effetive number of progenies.
    muEff = NULL,
    #' @field nDeltaFuncHist [numeric] Number of iterations of deltaFunc to be used for the computation of gammas
    nDeltaFuncHist = NULL,
    #' @field dGamma [numeric] Numeric which is used when increasing gammas.
    dGamma = NULL,
    #' @field deltaTh [numeric] Numeric which is used when increasing gammas.
    deltaTh = NULL,
    #' @field funcScale [integer] An overall scaling to be applied to the value of `optimizeFunc` during optimization. If negative, turns the problem into a maximization problem. Optimization is performed on `optimizeFunc(par) / fnscale`.
    funcScale = NULL,
    #' @field minimizeFunc [function] Function to be minimized by CMA-ES.
    minimizeFunc = NULL,
    #' @field optimalValue [numeric] Best function value.
    optimalValue = NULL,
    #' @field optimalParameter [numeric] Best parameter.
    optimalParameter = NULL,
    #' @field optimalValues [numeric] Best function values. (considering functional scaling)
    optimalValues = NULL,
    #' @field optimalHyperParamMat [matrix] Matrix of optimal parameters
    optimalHyperParamMat = NULL,
    #' @field stepSizes [numeric] Log of step sizes.
    stepSizes = NULL,
    #' @field covPCs [matrix] Log of principle components of the covariance matrix.
    covPCs = NULL,
    #' @field pops [array] Log of populations.
    pops = NULL,
    #' @field values [matrix] Log of function values.
    values = NULL,
    #' @field deltaFuncs [numeric] Numeric vector of history for the deltaFunc which is used to compute gammas.
    deltaFuncs = NULL,
    #' @field gammas [matrix] Penalty coefficient.
    gammas = NULL,
    #' @field pc [numeric] Evolution path for covariance matrix.
    pc = NULL,
    #' @field ps [numeric] Evolution path for step size.
    ps = NULL,
    #' @field B [matrix] Eigen vectors of covariance matrix.
    B = NULL,
    #' @field D [matrix] Diagonal matrix of eigen values of covariance matrix.
    D = NULL,
    #' @field BD [matrix] `B %*% D`.
    BD = NULL,
    #' @field sigma [numeric] Variance estimates. Can be a single number or a vector of length D, where D is the dimension of the parameter space.
    sigma = NULL,
    #' @field progCov [matrix] Covariance matrix of multivaraite normal distribution.
    progCov = NULL,
    #' @field chi [numeric] Value to correct evolution path for step size
    chi = NULL,
    #' @field iterNo [numeric] Iteration No.
    iterNo = NULL,
    #' @field countEval [numeric] Total number of function evaluations so far.
    countEval = NULL,
    #' @field constrViolations [numeric] Number of times to violate constraints.
    constrViolations = NULL,
    #' @field msg [character] Message corrsponding to the results.
    msg = NULL,
    #' @field paramNames [character] Parameter names.
    paramNames = NULL,
    #' @field progMean [matrix] Matrix of parameter means of progenies (history)
    progMeans = NULL,
    #' @field progParams [matrix] Parameter sets of progenies.
    progParams = NULL,
    #' @field progFit [numeric] Function values of progenies.
    progFit = NULL,
    #' @field continueOptimization [logical] Continue optimization in loop.
    continueOptimization = TRUE,

    #' @description Create a new cmaES object.
    #' @param parameter [vector] Vector with length defining the dimensionality of the optimization problem. Providing actual values of par is not necessary (NAs are just fine).
    #' @param paramLen [numeric] Number of parameters
    #' @param optimizeFunc [function] Scalar function to be optimized, with first argument to be optimized over
    #' @param lowerBound [numeric] Vectors of lower bounds on the variables
    #' @param upperBound [numeric] Vectors of upper bounds on the variables
    #' @param nIterOptimization [numeric] Number of function evaluations allocated to optimization
    #' @param maximize [logical] If TRUE, performs maximization
    #' @param stopFitness [numeric] Stop if function value is smaller than or equal to stopfitness. This is the only way for the CMA-ES to "converge".
    #' @param keepBest [logical] Return the best overall solution and not the best solution in the last population. Defaults to TRUE.
    #' @param sigmaInit [numeric] Initial variance estimates. Can be a single number or a vector of length D, where D is the dimension of the parameter space.
    #' @param lambda [numeric] Number of offspring. Must be greater than or equal to mu.
    #' @param mu [numeric] Population size.
    #' @param recombWeights [numeric] Recombination weights.
    #' @param etaRankMuAuto [logical] Update learning rate for rank-mu update automatically.
    #' @param etaRankMu [numeric] Learning rate for rank-mu update.
    #' @param etaRankOne [numeric] Learning rate for rank-one update.
    #' @param constStep [numeric] Cumulation constant for step-size.
    #' @param constCov [numeric] Cumulation constant for covariance matrix.
    #' @param dampStep [numeric] Damping for step-size.
    #' @param maximize [logical] Is the function `optimizeFunc` vectorized?
    #' @param saveStepSize [logical] Save current step size in each iteration.
    #' @param savePC [logical] Save current principle components of the covariance matrix progCov in each iteration.
    #' @param saveCurrentPop [logical] Save current population in each iteration.
    #' @param saveValue [logical] Save function values of the current population in each iteration.
    #' @param scaleTorelance [numeric] Tolerance for scale.
    #' @param saveObjNameBase [character] Base name of the `cmaES` object to be saved
    #' @param whenToSaveObj [numeric] When (how many iterations) to save the `cmaES` object
    #' @param verbose [logical] Display information
    #'
    initialize = function(
    parameter,
    optimizeFunc,
    ...,
    lowerBound = NULL,
    upperBound = NULL,
    nIterOptimization = NULL,
    maximize = NULL,
    stopFitness = NULL,
    keepBest = NULL,
    sigmaInit = NULL,
    lambda = NULL,
    mu = NULL,
    recombWeights = NULL,
    etaRankMuAuto = NULL,
    etaRankMu = NULL,
    etaRankOne = NULL,
    constStep = NULL,
    constCov = NULL,
    dampStep = NULL,
    vectorized = NULL,
    saveStepSize = NULL,
    savePC = NULL,
    saveCurrentPop = NULL,
    saveValue = NULL,
    scaleTorelance = NULL,
    saveObjNameBase = NULL,
    whenToSaveObj = NULL,
    verbose = TRUE
    ) {

      # some definitions
      # optimizeTypesOffered <- c("stochastic", "deterministic")


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
        lowerBound <- rep(-Inf, paramLen)
        message("You do not specify `lowerBound`. We set `lowerBound = rep(-Inf, length(parameter))`.")
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
        upperBound <- rep(Inf, paramLen)
        message("You do not specify `upperBound`. We set `upperBound = rep(Inf, length(parameter))`.")
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


      # maximize
      if (is.null(maximize)) {
        maximize <- TRUE
        message(paste0("You do not specify `maximize`. We set `maximize = ",
                       maximize, "`."))
      }
      stopifnot(is.logical(maximize))


      # stopFitness
      if (is.null(stopFitness)) {
        stopFitness <- -Inf
        # message(paste0("You do not specify `stopFitness`. We set `stopFitness = ",
        #                stopFitness, "`."))
      }


      # keepBest
      if (is.null(keepBest)) {
        keepBest <- TRUE
        # message(paste0("You do not specify `keepBest`. We set `keepBest = ",
        #                keepBest, "`."))
      }
      stopifnot(is.logical(keepBest))


      # sigmaInit
      if (!is.null(sigmaInit)) {
        stopifnot(is.numeric(sigmaInit))
      } else {
        sigmaInit <- 0.5
        # message(paste0("You do not specify `sigmaInit`. We set `sigmaInit = ",
        #                sigmaInit, "`."))
      }


      # lambda
      if (!is.null(lambda)) {
        stopifnot(is.numeric(lambda))
      } else {
        lambda <- 4 + floor(3 * log(paramLen))
        message(paste0("You do not specify `lambda`. We set `lambda = ",
                       lambda, "`."))
      }


      # mu
      if (!is.null(mu)) {
        stopifnot(is.numeric(mu))
      } else {
        mu <- floor(lambda / 2)
        message(paste0("You do not specify `mu`. We set `mu = ",
                       mu, "`."))
      }


      # recombWeights
      if (!is.null(recombWeights)) {
        stopifnot(is.numeric(recombWeights))
      } else {
        # recombWeights <- log(mu + 1) - log(1:mu)
        recombWeights <- log(mu + 1 / 2) - log(1:mu)
        recombWeights <- recombWeights / sum(recombWeights)
        # message(paste0("You do not specify `recombWeights`. We set `recombWeights = log(mu + 1) - log(1:mu) / sum(log(mu + 1) - log(1:mu))`."))
      }

      # muEff
      muEff <- 1 / sum(recombWeights ^ 2)


      # etaRankOne
      if (!is.null(etaRankOne)) {
        stopifnot(is.numeric(etaRankOne))
      } else {
        # etaRankOne <- (1 / etaRankMu) * 2 / (paramLen + 1.4) ^ 2 +
        #   (1 - 1 / etaRankMu) * ((2 * etaRankMu - 1) / ((paramLen + 2) ^ 2 + 2 * etaRankMu))

        etaRankOne <- 2 / ((paramLen + 1.3) ^ 2 + muEff)
        # message(paste0("You do not specify `etaRankOne`. We set `etaRankOne = ",
        #                round(etaRankOne, 3), "`."))
      }

      # etaRankMuAuto
      if (is.null(etaRankMuAuto)) {
        etaRankMuAuto <- TRUE
        # message(paste0("You do not specify `etaRankMuAuto`. We set `etaRankMuAuto = ",
        #                etaRankMuAuto, "`."))
      }
      stopifnot(is.logical(etaRankMuAuto))

      # etaRankMu
      if (!is.null(etaRankMu)) {
        stopifnot(is.numeric(etaRankMu))
      } else {
        # etaRankMu <- muEff

        etaRankMu <- min(1 - etaRankOne, 2 * (muEff - 2 + 1 / muEff) / ((nIterOptimization + 2) ^ 2 + muEff))
        # message(paste0("You do not specify `etaRankMu`. We set `etaRankMu = ",
        #                round(etaRankMu, 3), "`."))
      }

      # constStep
      if (!is.null(constStep)) {
        stopifnot(is.numeric(constStep))
      } else {
        # constStep <- (muEff + 2) / (paramLen + muEff + 3)
        constStep <- (muEff + 2) / (paramLen + muEff + 5)
        # message(paste0("You do not specify `constStep`. We set `constStep = ",
        #                round(constStep, 3), "`."))
      }

      # constCov
      if (!is.null(constCov)) {
        stopifnot(is.numeric(constCov))
      } else {
        # constCov <- 4 / (paramLen + 4)
        constCov <- (4 + muEff / paramLen) / (paramLen + 4 + 2 * muEff / paramLen)
        # message(paste0("You do not specify `constCov`. We set `constCov = ",
        #                round(constCov, 3), "`."))
      }


      # dampStep
      if (!is.null(dampStep)) {
        stopifnot(is.numeric(dampStep))
      } else {
        dampStep <- 1 + 2 * max(0, sqrt((muEff - 1) / (paramLen + 1)) - 1) + constStep
        # message(paste0("You do not specify `dampStep`. We set `dampStep = ",
        #                round(dampStep, 3), "`."))
      }


      # vectorized
      if (is.null(vectorized)) {
        vectorized <- FALSE
        # message(paste0("You do not specify `vectorized`. We set `vectorized = ",
        #                vectorized, "`."))
      }
      stopifnot(is.logical(vectorized))


      # saveStepSize
      if (is.null(saveStepSize)) {
        saveStepSize <- FALSE
        # message(paste0("You do not specify `saveStepSize`. We set `saveStepSize = ",
        #                saveStepSize, "`."))
      }
      stopifnot(is.logical(saveStepSize))


      # savePC
      if (is.null(savePC)) {
        savePC <- FALSE
        # message(paste0("You do not specify `savePC`. We set `savePC = ",
        #                savePC, "`."))
      }
      stopifnot(is.logical(savePC))


      # saveCurrentPop
      if (is.null(saveCurrentPop)) {
        saveCurrentPop <- FALSE
        # message(paste0("You do not specify `saveCurrentPop`. We set `saveCurrentPop = ",
        #                saveCurrentPop, "`."))
      }
      stopifnot(is.logical(saveCurrentPop))


      # saveValue
      if (is.null(saveValue)) {
        saveValue <- FALSE
        # message(paste0("You do not specify `saveValue`. We set `saveValue = ",
        #                saveValue, "`."))
      }
      stopifnot(is.logical(saveValue))


      # scaleTorelance
      if (is.null(scaleTorelance)) {
        scaleTorelance <- 1e-12
      }

      # saveObjNameBase
      if (is.null(saveObjNameBase)) {
        whenToSaveObj <- NULL
      }

      # whenToSaveObj
      if (!is.null(whenToSaveObj)) {
        if (!all(is.na(whenToSaveObj))) {
          stopifnot(is.numeric(whenToSaveObj))
          whenToSaveObj <- whenToSaveObj[!is.na(whenToSaveObj)]
          whenToSaveObj <- floor(whenToSaveObj)
          stopifnot(all(whenToSaveObj >= 1))
          stopifnot(all(whenToSaveObj <= nIterOptimization))
        } else {
          whenToSaveObj <- nIterOptimization
          message(paste0("You do not specify `whenToSaveObj`. We set `whenToSaveObj = ",
                         whenToSaveObj, "`."))
        }
      }

      # fileNameSaveObj, fileNameOptimalNodes, fileNameOptimalParams
      if (!is.null(saveObjNameBase)) {
        splitSaveObjNameBase <- stringr::str_split(string = saveObjNameBase,
                                                   pattern = "/")[[1]]

        fileNameSaveObj <- here::here(dirname(saveObjNameBase),
                                      paste0(splitSaveObjNameBase[length(splitSaveObjNameBase)],
                                             "_CMA-ES_object.rds"))
        fileNameOptimalParams <- here::here(dirname(saveObjNameBase),
                                            paste0(splitSaveObjNameBase[length(splitSaveObjNameBase)],
                                                   "_CMA-ES_optimal_parameters.csv"))
      } else {
        fileNameSaveObj <- NULL
        fileNameOptimalParams <- NULL
      }


      # verbose
      if (is.null(verbose)) {
        verbose <- FALSE
        # message(paste0("You do not specify `verbose`. We set `verbose = ",
        #                verbose, "`."))
      }
      stopifnot(is.logical(verbose))

      # saveObjOld
      saveObjOld <- rep(FALSE, length(whenToSaveObj))


      # nDeltaFuncHist
      nDeltaFuncHist <- 20 + floor(3 * paramLen / lambda)

      # dGamma
      dGamma <- min(muEff, 1 / (10 * paramLen))

      # deltaTh
      deltaTh <- 3 * max(1, sqrt(paramLen) / muEff)


      # funcScale
      funcScale <- ifelse(test = maximize, yes = -1, no = 1)


      # minimizeFunc
      minimizeFunc <- function(parameter) {
        # parameterForOriginalFunc <- parameter * (upperBound - lowerBound) + lowerBound
        minimizeVal <- optimizeFunc(parameter, ...) / funcScale

        return(minimizeVal)
      }


      # optimalValue
      optimalValue <- Inf

      # optimalParameter
      optimalParameter <- NULL

      # optimalValues
      optimalValues <- NULL

      # optimalHyperParamMat
      optimalHyperParamMat <- NULL

      # stepSizes
      if (saveStepSize) {
        stepSizes <- rep(0, nIterOptimization)
      } else {
        stepSizes <- NULL
      }


      # covPCs
      if (savePC) {
        covPCs <- matrix(data = 0,
                         nrow = nIterOptimization,
                         ncol = paramLen)
      } else {
        covPCs <- NULL
      }


      # pops
      if (saveCurrentPop) {
        pops <- array(data = 0,
                      dim = c(paramLen,
                              mu, nIterOptimization))
      } else {
        pops <- NULL
      }


      # values
      if (saveValue) {
        values <- matrix(data = 0,
                         nrow = nIterOptimization,
                         ncol = mu)
      } else {
        values <- NULL
      }

      # deltaFuncs
      deltaFuncs <- NULL

      # gammas
      gammas <- rep(0, paramLen)

      # pc
      pc <- rep(0, paramLen)

      # ps
      ps <- rep(0, paramLen)

      # B
      B <- diag(paramLen)

      # D
      D <- diag(paramLen)

      # BD
      BD <- B %*% D

      # sigma
      sigma <- sigmaInit

      # progCov
      progCov <- BD %*% t(BD)

      # chi
      chi <- sqrt(paramLen) * (1 - 1 / (4 * paramLen) + 1 / (21 * paramLen ^ 2))

      # iterNo
      iterNo <- 0L

      # countEval
      countEval <- 0L

      # constrViolations
      constrViolations <- 0L

      # msg
      msg <- NULL

      # paramNames
      paramNames <- names(parameter)

      # progMean
      progMean <- parameter

      # progParams
      progParams <- matrix(data = 0, nrow = paramLen, ncol = lambda)

      # progFit
      progFit <- rep(0, lambda)

      # continueOptimization
      continueOptimization <- TRUE



      self$parameter <- parameter
      self$paramLen <- paramLen
      self$optimizeFunc <- optimizeFunc
      self$lowerBound <- lowerBound
      self$upperBound <- upperBound
      self$nIterOptimization <- nIterOptimization
      self$maximize <- maximize
      self$stopFitness <- stopFitness
      self$keepBest <- keepBest
      self$sigmaInit <- sigmaInit
      self$lambda <- lambda
      self$mu <- mu
      self$recombWeights <- recombWeights
      self$etaRankMu <- etaRankMu
      self$etaRankOne <- etaRankOne
      self$constStep <- constStep
      self$constCov <- constCov
      self$dampStep <- dampStep
      self$vectorized <- vectorized
      self$saveStepSize <- saveStepSize
      self$savePC <- savePC
      self$saveCurrentPop <- saveCurrentPop
      self$saveValue <- saveValue
      self$scaleTorelance <- scaleTorelance
      self$saveObjNameBase <- saveObjNameBase
      self$whenToSaveObj <- whenToSaveObj
      self$verbose <- verbose

      self$fileNameSaveObj <- fileNameSaveObj
      self$fileNameOptimalParams <- fileNameOptimalParams
      self$saveObjOld <- saveObjOld

      self$etaRankMuAuto <- etaRankMuAuto
      self$nDeltaFuncHist <- nDeltaFuncHist
      self$dGamma <- dGamma
      self$deltaTh <- deltaTh

      self$funcScale <- funcScale
      self$minimizeFunc <- minimizeFunc

      self$optimalValue <- optimalValue
      self$optimalParameter <- optimalParameter
      self$optimalValues <- optimalValues
      self$optimalHyperParamMat <- optimalHyperParamMat

      self$stepSizes <- stepSizes
      self$covPCs <- covPCs
      self$pops <- pops
      self$values <- values

      self$deltaFuncs <- deltaFuncs
      self$gammas <- gammas
      self$muEff <- muEff
      self$pc <- pc
      self$ps <- ps
      self$B <- B
      self$D <- D
      self$BD <- BD
      self$sigma <- sigma
      self$progCov <- progCov
      self$chi <- chi
      self$iterNo <- iterNo
      self$countEval <- countEval
      self$constrViolations <- constrViolations
      self$msg <- msg
      self$paramNames <- paramNames
      self$progMeans <- as.matrix(progMean)
      self$progParams <- progParams
      self$progFit <- progFit
      self$continueOptimization <- continueOptimization
    },


    #' @description
    #' Proceed one generation of CMA-ES
    proceedOneGeneration = function() {
      st <- Sys.time()

      ## parameters
      parameter <- self$parameter
      paramLen <- self$paramLen
      optimizeFunc <- self$optimizeFunc
      lowerBound <- self$lowerBound
      upperBound <- self$upperBound
      nIterOptimization <- self$nIterOptimization
      maximize <- self$maximize
      stopFitness <- self$stopFitness
      keepBest <- self$keepBest
      sigmaInit <- self$sigmaInit
      lambda <- self$lambda
      mu <- self$mu
      recombWeights <- self$recombWeights
      etaRankMu <- self$etaRankMu
      etaRankOne <- self$etaRankOne
      constStep <- self$constStep
      constCov <- self$constCov
      dampStep <- self$dampStep
      vectorized <- self$vectorized
      saveStepSize <- self$saveStepSize
      savePC <- self$savePC
      saveCurrentPop <- self$saveCurrentPop
      saveValue <- self$saveValue
      scaleTorelance <- self$scaleTorelance
      saveObjNameBase <- self$saveObjNameBase
      whenToSaveObj <- self$whenToSaveObj
      verbose <- self$verbose

      fileNameSaveObj <- self$fileNameSaveObj
      fileNameOptimalParams <- self$fileNameOptimalParams
      saveObjOld <- self$saveObjOld
      nDeltaFuncHist <- self$nDeltaFuncHist
      dGamma <- self$dGamma
      deltaTh <- self$deltaTh
      muEff <- self$muEff

      funcScale <- self$funcScale
      minimizeFunc <- self$minimizeFunc

      optimalValue <- self$optimalValue
      optimalParameter <- self$optimalParameter
      optimalValues <- self$optimalValues
      optimalHyperParamMat <- self$optimalHyperParamMat
      stepSizes <- self$stepSizes
      covPCs <- self$covPCs
      pops <- self$pops
      values <- self$values

      deltaFuncs <- self$deltaFuncs
      gammas <- self$gammas
      pc <- self$pc
      ps <- self$ps
      B <- self$B
      D <- self$D
      BD <- self$BD
      sigma <- self$sigma
      progCov <- self$progCov
      chi <- self$chi
      iterNo <- self$iterNo
      countEval <- self$countEval
      constrViolations <- self$constrViolations
      msg <- self$msg
      progMean <- self$progMeans[, ncol(self$progMeans)]
      paramNames <- self$paramNames
      progParams <- self$progParams
      progFit <- self$progFit
      continueOptimization <- self$continueOptimization


      ## preparation
      iterNo <- iterNo + 1L

      if (verbose) {
        cat(sprintf("------ Iteration %i of %i ------",
                    iterNo, nIterOptimization))
        cat("\n")
        cat("Current parameter mean: ")
        cat("\n")
        print(progMean)
      }


      if (!keepBest) {
        optimalValue <- Inf
        optimalParameter <- NULL
      }

      if (saveStepSize) {
        stepSizes[iterNo] <- sigma
      }


      ## sampling progenies
      progZ <- matrix(data = rnorm(paramLen * lambda),
                      nrow = paramLen,
                      ncol = lambda)
      progParams <- progMean + sigma * (BD %*% progZ)
      progParamsIn <- ifelse(progParams > lowerBound,
                             ifelse(progParams < upperBound,
                                    progParams, upperBound),
                             lowerBound)
      if (!is.null(paramNames)) {
        rownames(progParamsIn) <- paramNames
      }



      ## evaluate function values
      if (vectorized) {
        progFuncVal <- minimizeFunc(parameter = progParamsIn)
      } else {
        # progFuncVal <- apply(X = progParamsIn, MARGIN = 2,
        #                      FUN = minimizeFunc)

        progFuncVal <- rep(NA, ncol(progParamsIn))
        for (i in 1:ncol(progParamsIn)) {
          stNow <- Sys.time()
          progFuncVal[i] <- minimizeFunc(parameter = progParamsIn[, i])
          edNow <- Sys.time()

          timeCheck <- as.numeric(difftime(time1 = edNow, time2 = stNow, units = "secs")) > 100
          # if (verbose) {
          if (timeCheck & verbose) {
            cat(paste0("Finish evaluation of ", i, " / ",
                       lambda, " candidate."))
            cat("\n")
            cat(paste0("Time: ", round(as.numeric(edNow - stNow), 3),
                       " ", attr(edNow - stNow, "units")))
            cat("\n\n")
          }
        }
      }

      countEval <- countEval + lambda

      ## check penalty
      iqr <- quantile(x = progFuncVal)[4] - quantile(x = progFuncVal)[2]
      deltaFunc <- iqr / (sigma ^ 2 * sum(diag(progCov)) / paramLen)
      deltaFuncs <- c(deltaFuncs, deltaFunc)
      if (length(deltaFuncs) >= nDeltaFuncHist) {
        deltaFuncsUsed <- deltaFuncs[(length(deltaFuncs) - nDeltaFuncHist + 1):length(deltaFuncs)]
      } else {
        deltaFuncsUsed <- deltaFuncs
      }

      if (length(deltaFuncsUsed) >= 3) {
        deltaMed3 <- median(deltaFuncsUsed[(length(deltaFuncsUsed) - 2):length(deltaFuncsUsed)])
        if (deltaMed3 > 0) {
          satisfiedSeq <- which(abs(log(deltaFuncsUsed) - log(deltaMed3)) < log(5))
        } else {
          satisfiedSeq <- which(deltaFuncsUsed == 0)
        }
        if (any(diff(satisfiedSeq) != 1)) {
          deltaFit <- median(deltaFuncsUsed[(satisfiedSeq[max(which(diff(satisfiedSeq) != 1)) + 1]):length(deltaFuncsUsed)])
        } else {
          deltaFit <- median(deltaFuncsUsed[satisfiedSeq[1]:length(deltaFuncsUsed)])
        }
      } else {
        deltaFit <- median(deltaFuncsUsed)
      }



      ## penalty coefficient
      progMeanIn <- ifelse(progMean > lowerBound,
                           ifelse(progMean < upperBound,
                                  progMean, upperBound),
                           lowerBound)
      if (!all(progMean == progMeanIn)) {
        if (all(gammas == 0) | (iterNo == 2)) {
          gammas <- rep(2 * deltaFit, paramLen)
        }

        deltaMean <- abs(progMean - progMeanIn) / (sigma * sqrt(diag(progCov)))
        gammas <- gammas * exp(dGamma * tanh(pmax(0, deltaMean - deltaTh) / 3) / 2)

        # if (any(gammas > 5 * deltaFit)) {
        # gammas[gammas > 5 * deltaFit] <- gammas[gammas > 5 * deltaFit] * exp(-dGamma / 3)
        # }
        gammas <- gammas * min(3 * deltaFit / sum(gammas), 1)
      }


      penalty <- colMeans(matrix(rep(gammas, lambda), ncol = lambda) *
                            (progParamsIn - progParams) ^ 2)
      # penalty <- 1 + colSums((progParamsIn - progParams) ^ 2)
      penalty[!is.finite(penalty)] <- .Machine$double.xmax / 2
      constrViolations <- constrViolations + sum(penalty > 0)
      # constrViolations <- constrViolations + sum(penalty > 1)


      progFit <- progFuncVal + penalty
      valid <- penalty <= 0
      # progFit <- progFuncVal * penalty
      # valid <- penalty <= 1


      ## select for next generation
      progFitOrd <- order(progFit, decreasing = FALSE)
      progFit <- progFit[progFitOrd]
      progFitOrdTopMu <- progFitOrd[1:mu]
      selParams <- progParams[, progFitOrdTopMu, drop = FALSE]
      # progMean <- drop(selParams %*% recombWeights)
      selZ <- progZ[, progFitOrdTopMu, drop = FALSE]
      BDZ <- BD %*% selZ
      progMean <- progMean + sigma * drop(BDZ %*% recombWeights)
      progMeanZ <- drop(selZ %*% recombWeights)

      if (saveCurrentPop) {
        pops[, , iterNo] <- selParams
      }
      if (saveValue) {
        values[iterNo, ] <- progFit[1:mu]
      }

      ## update parameters
      # ps <- (1 - constStep) * ps + sqrt(constStep * (2 - constStep) * muEff) * (B %*% progMeanZ)
      ps <- (1 - constStep) * ps + sqrt(constStep * (2 - constStep) * muEff) * progMeanZ
      # hSig <- drop((norm(ps) / sqrt(1 - (1 - constStep) ^ (2 * countEval / lambda)) / chi) < (1.4 + 2 / (paramLen + 1)))
      # pc <- (1 - constCov) * pc +
      #   hSig * sqrt(constCov * (2 - constCov) * muEff) * drop(BD %*% progMeanZ)
      pc <- (1 - constCov) * pc +
        sqrt(constCov * (2 - constCov) * muEff) * drop(BDZ %*% recombWeights)
      # progCov <- (1 - etaRankOne) * progCov +
      #   etaRankOne * (1 / etaRankMu) * (pc %o% pc + (1 - hSig) * constCov * (2 - constCov) * progCov) +
      #   etaRankOne * (1 - 1 / etaRankMu) * BDZ %*% diag(recombWeights) %*% t(BDZ)
      progCov <- (1 - etaRankOne) * progCov +
        etaRankOne * tcrossprod(pc) +
        etaRankMu * apply(
          X = do.call(
            what = abind::abind,
            args = sapply(
              X = 1:ncol(BDZ),
              FUN = function(x) {
                mat <- recombWeights[x] * (tcrossprod(BDZ[, x, drop = FALSE]) - progCov)
                arr <- array(data = mat,
                             dim = c(dim(mat), 1))
                return(arr)
              },
              simplify = FALSE
            )
          ),
          MARGIN = c(1, 2), FUN = sum
        )


      sigma <- sigma * exp((norm(as.matrix(ps)) / chi - 1) * constStep / dampStep)
      eigenRes <- eigen(progCov, symmetric = TRUE)
      eigenVals <- eigenRes$values
      if (savePC) {
        covPCs[iterNo, ] <- rev(sort(eigenVals, decreasing = FALSE))
      }
      if (!all(eigenVals >= sqrt(.Machine$double.eps) * abs(eigenVals[1]))) {
        msg <- "Covariance matrix 'progCov' is numerically not positive definite."
        continueOptimization <- FALSE
      }
      B <- eigenRes$vectors
      D <- diag(sqrt(eigenVals), length(eigenVals))
      BD <- B %*% D

      ## messages
      if (progFit[1] <= stopFitness) {
        msg <- "Stop fitness reached."
        continueOptimization <- FALSE
      }
      if (all(D < scaleTorelance) && all(sigma * pc < scaleTorelance)) {
        msg <- "All standard deviations smaller than tolerance."
        continueOptimization <- FALSE
      }
      if (progFit[1] == progFit[min(1 + floor(lambda / 2), 2 + ceiling(lambda / 4))]) {
        sigma <- sigma * exp(0.2 + constStep / dampStep)
        if (verbose) {
          cat("Flat fitness function. Increasing sigma.")
        }
      }
      ed <- Sys.time()
      if (verbose) {
        cat(sprintf("Current fitness: %f", progFit[1] * funcScale))
        cat("\n")
        cat(paste0("Time: ", round(as.numeric(ed - st), 3),
                   " ", attr(ed - st, "units")))
        cat("\n\n")
      }

      if (any(valid)) {
        whichBest <- which.min(progFuncVal[valid])
        if (progFuncVal[valid][whichBest] < optimalValue) {
          optimalValue <- progFuncVal[valid][whichBest]
          optimalParameter <- progParams[, valid, drop = FALSE][, whichBest]

          optimalValues <- c(optimalValues, optimalValue * funcScale)
          optimalHyperParamMat <- rbind(optimalHyperParamMat,
                                        t(optimalParameter))
          names(optimalValues)[length(optimalValues)] <-
            rownames(optimalHyperParamMat)[nrow(optimalHyperParamMat)] <-
            paste0("Iteration_", iterNo)

          if (verbose) {
            cat(paste0("\n------ Iteration ", iterNo,
                       ": Optimal Parameters Updated ------\n"))
            print(optimalHyperParamMat)
            cat("\n")
          }

          if (!is.null(saveObjNameBase)) {
            saveRDS(object = self,
                    file = fileNameSaveObj)

            write.csv(x = optimalHyperParamMat,
                      file = fileNameOptimalParams,
                      row.names = TRUE)
          }
        }
      }

      saveObj <- whenToSaveObj <= iterNo
      whereToSaveObj <- which(as.logical(saveObj - saveObjOld))
      if (length(whereToSaveObj) >= 1) {
        if (!is.null(saveObjNameBase)) {
          saveRDS(object = self,
                  file = fileNameSaveObj)

          if (verbose) {
            cat(paste0("\n------ Iteration ", iterNo,
                       ": Current Optimal Parameters ------\n"))
            print(optimalHyperParamMat)
            cat("\n")
          }
        }
      }

      saveObjOld <- saveObj


      ## save parameters
      self$optimalValue <- optimalValue
      self$optimalParameter <- optimalParameter
      self$optimalValues <- optimalValues
      self$optimalHyperParamMat <- optimalHyperParamMat
      self$stepSizes <- stepSizes
      self$covPCs <- covPCs
      self$pops <- pops
      self$values <- values

      self$saveObjOld <- saveObjOld
      self$deltaFuncs <- deltaFuncs
      self$gammas <- gammas
      self$pc <- pc
      self$ps <- ps
      self$B <- B
      self$D <- D
      self$BD <- BD
      self$sigma <- sigma
      self$progCov <- progCov
      self$chi <- chi
      self$iterNo <- iterNo
      self$countEval <- countEval
      self$constrViolations <- constrViolations
      self$msg <- msg
      self$paramNames <- paramNames
      self$progMeans <- cbind(self$progMeans, progMean)
      self$progParams <- progParams
      self$progFit <- progFit
      self$continueOptimization <- continueOptimization
    },



    #' @description
    #' start global optimization of stochastic function by StoSOO
    startOptimization = function() {
      iterNo <- self$iterNo
      nIterOptimization <- self$nIterOptimization
      continueOptimization <- self$continueOptimization
      if (self$etaRankMuAuto) {
        self$etaRankMu <- min(1 - self$etaRankOne,
                              2 * (self$muEff - 2 + 1 / self$muEff) / ((self$nIterOptimization + 2) ^ 2 + self$muEff))
      }

      # stepSizes
      if (self$saveStepSize) {
        if (length(self$stepSizes) < nIterOptimization) {
          self$stepSizes <- c(self$stepSizes,
                              rep(0, nIterOptimization - length(self$stepSizes)))
        }
      }


      # covPCs
      if (self$savePC) {
        if (nrow(self$covPCs) < nIterOptimization) {
          self$covPCs <- rbind(self$covPCs,
                               matrix(data = 0,
                                      nrow = nIterOptimization - nrow(self$covPCs),
                                      ncol = self$paramLen))
        }
      }


      # pops
      if (self$saveCurrentPop) {
        if (dim(self$pops)[3] < nIterOptimization) {
          self$pops <- abind::abind(self$pops,
                                    array(data = 0,
                                          dim = c(self$paramLen, self$mu,
                                                  nIterOptimization - dim(self$pops)[3])),
                                    along = 3)
        }
      }


      # values
      if (self$saveValue) {
        if (nrow(self$values) < nIterOptimization) {
          self$values <- rbind(self$values,
                               matrix(data = 0,
                                      nrow = nIterOptimization - nrow(self$values),
                                      ncol = self$mu))
        }
      }




      while ((iterNo < nIterOptimization) & continueOptimization) {
        self$proceedOneGeneration()
        iterNo <- self$iterNo
        nIterOptimization <- self$nIterOptimization
        continueOptimization <- self$continueOptimization
      }
    },


    #' @description
    #' Display information about the object
    print = function() {
      cat(paste0("Number of Parameters: ", self$paramLen, "\n",
                 "Number of Iterations: ", self$nIterOptimization, "\n",
                 "Maximize or Not: ", self$maximize, "\n",
                 "Lower Bound: \n"))
      print(self$lowerBound)
      cat(paste0("Upper Bound: \n"))
      print(self$upperBound)
    }
  )
)
