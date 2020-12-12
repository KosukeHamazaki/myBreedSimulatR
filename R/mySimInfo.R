# Author: Kosuke Hamazaki hamazaki@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# Definition of simInfo class




#' R6 Class representing a simulation information
#'
#' @description
#' simInfo object store specific information of one simulation study.
#'
#' @details
#' Details: simInfo object store specific information of one simulation study.
#'
#' @export
#' @import R6
simInfo <- R6::R6Class(
  "simInfo",
  public = list(
    #' @field simName [character] Name of the simulation study
    simName = NA,
    #' @field simGeno [logical] Simulate marker genotype or not
    simGeno = NA,
    #' @field simPheno [logical] Simulate phenotypic values or not
    simPheno = NA,
    #' @field nSimGeno [numeric] Number of simulations for generating marker genome
    nSimGeno = NA,
    #' @field nSimPheno [numeric] Number of simulations for pseudo phenotypic values
    nSimPheno = NA,
    #' @field nCoreMax [numeric] Number of cores you can use for this simulation study
    nCoreMax = NA,
    #' @field nCorePerGeno [numeric] Number of cores used per simulation of marker genotype (optional)
    nCorePerGeno = NA,
    #' @field nCorePerPheno [numeric] Number of cores used per simulation of phenotypic values (optional)
    nCorePerPheno = NA,
    #' @field saveDataFileName [character] The name of the file (including path) that saves the data (if NULL, the data won't be saved)
    saveDataFileName = NULL,


    #' @description Create a new specie object.
    #' @param simName [character] Name of the simulation study
    #' @param simGeno [logical] Simulate marker genotype or not
    #' @param simPheno simPheno [logical] Simulate phenotypic values or not
    #' @param nSimGeno [numeric] Number of simulations for generating marker genome (optional)
    #' @param nSimPheno [numeric] Number of simulations for pseudo phenotypic values (optional)
    #' @param nCoreMax [numeric] Number of cores you can use for this simulation study (optional)
    #' @param nCorePerGeno [numeric] Number of cores used per simulation of marker genotype (optional)
    #' @param nCorePerPheno [numeric] Number of cores used per simulation of phenotypic values (optional)
    #' @param saveDataFileName [character] The name of the file (including path) that saves the data (if NULL, the data won't be saved)
    #'   (optional)
    #' @param verbose [logical] Display info (optional)
    #' @return A new `simInfo` object.
    #' @examples
    #' mySimInfo <- simInfo$new(simName = "Simulation Example",
    #'                          simGeno = TRUE,
    #'                          simPheno = TRUE,
    #'                          nSimGeno = 1,
    #'                          nSimPheno = 3,
    #'                          nCoreMax = 4,
    #'                          nCorePerGeno = 1,
    #'                          nCorePerPheno = 3,
    #'                          saveDataFileName = NULL)
    #' print(mySimInfo)
    initialize = function(simName = "Simulation Example",
                          simGeno = TRUE,
                          simPheno = TRUE,
                          nSimGeno = NA,
                          nSimPheno = NA,
                          nCoreMax = NA,
                          nCorePerGeno = NA,
                          nCorePerPheno = NA,
                          saveDataFileName = NULL,
                          verbose = TRUE) {
      if (is.na(nCoreMax)) {
        message('"nCoreMax" was not specified. The nCoreMax had been set to "detectCores()"')
        nCoreMax <- parallel::detectCores()
      }

      if (simGeno) {
        if (is.na(nSimGeno)) {
          message('"nSimGeno" was not specified. The nSimGeno had been set to "1"')
          nSimGeno <- 1
        }

        if (nSimGeno != 1) {
          warning("We are very sorry, but now we only offer nSimGeno = 1.")
          nSimGeno <- 1
        }
        if (!is.numeric(nSimGeno)) stop("nSimGeno must be numeric.")
        if (floor(nSimGeno) - nSimGeno != 0) stop("nSimGeno must be integer.")

        if (is.na(nCorePerGeno)) {
          message('"nCorePerGeno" was not specified. The nCorePerGeno had been set to "1"')
          nCorePerGeno <- 1
        }
        if (!is.numeric(nCorePerGeno)) stop("nCorePerGeno must be numeric.")
        if (floor(nCorePerGeno) - nCorePerGeno != 0) stop("nCorePerGeno must be integer.")

        if (!simPheno) {
          message("If simGeno = TRUE, you should use simulated phenotypic values.")
          simPheno <- TRUE
        }
      } else {
        message("If simGeno = FALSE, you should use real marker genotype data.")
      }


      if (simPheno) {
        if (is.na(nSimPheno)) {
          message('"nSimPheno" was not specified. The nSimPheno had been set to "1"')
          nSimPheno <- 1
        }
        if (!is.numeric(nSimPheno)) stop("nSimPheno must be numeric.")
        if (floor(nSimPheno) - nSimPheno != 0) stop("nSimPheno must be integer.")

        if (is.na(nCorePerPheno)) {
          message('"nCorePerPheno" was not specified. The nCorePerPheno had been set to "1"')
          nCorePerPheno <- 1
        }
        if (!is.numeric(nCorePerPheno)) stop("nCorePerPheno must be numeric.")
        if (floor(nCorePerPheno) - nCorePerPheno != 0) stop("nCorePerPheno must be integer.")
      } else {
        message("If simPheno = FALSE, you should use real phenotypic values.")
      }

      if (is.null(saveDataFileName)) {
        message("The data won't be saved anywhere.")
      }


      self$simName <- simName
      self$simGeno <- simGeno
      self$simPheno <- simPheno
      self$nSimGeno <- nSimGeno
      self$nSimPheno <- nSimPheno
      self$nCoreMax <- nCoreMax
      self$nCorePerGeno <- nCorePerGeno
      self$nCorePerPheno <- nCorePerPheno
      self$saveDataFileName <- saveDataFileName


      if (verbose) cat(paste("A new simulation has emerged:", self$simName,
                             "!\n\n"))
    },

    #' @description
    #' Display information about the object
    print = function() {
      cat(paste0(
        "Name: ", self$simName, "\n",
        "Simulate marker genotype: ", self$simGeno, "\n",
        "Simulate phenotypic values: ", self$simPheno, "\n",
        "Number of simulations for genrating marker genotype: ", self$nSimGeno, "\n",
        "Number of simulations for pseudo phenotypic values: ", self$nSimPheno, "\n",
        "Number of cores used for this study: ", self$nCoreMax, "\n",
        "Number of cores used per simulation of genome: ", self$nCorePerGeno, "\n",
        "Number of cores used per simulation of phenotype: ", self$nCorePerPheno, "\n",
        "Save data at: ", self$saveDataFileName, "\n"
      ))
    }
  )
)
