# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# Definition of individuals class




#' R6 class representing an individual
#'
#' @description
#' individual object store specific information about one individual
#'
#'
#' @export
#' @import R6
individual <- R6::R6Class(
  "individual",
  public = list(
    #' @field name [string] Name of the individual
    name = NULL,
    #' @field specie [specie class] Specie of the SNPs
    #'   (see:\link[myBreedSimulatR]{specie})
    specie = NULL,
    #' @field traitInfo [traitInfo class] Specific information of traits
    #'   (see:\link[myBreedSimulatR]{traitInfo})
    traitInfo = NULL,
    #' @field parent1 [string] Name of the individual's parent
    parent1 = NULL,
    #' @field parent2 [string] Name of the individual's parent
    parent2 = NULL,
    #' @field haplo [haplotype class] Haplotype of the individual (see:
    #'   \link[myBreedSimulatR]{haplotype})
    haplo = NULL,

    #' @description Create a new individual object.
    #' @param name [string] name of the individual
    #' @param specie [specie class] Specie of the SNPs
    #'   (see:\link[myBreedSimulatR]{specie})
    #' @param traitInfo [traitInfo class] Specific information of traits
    #'   (see:\link[myBreedSimulatR]{traitInfo})
    #' @param parent1 [string] Name of the individual's parent
    #' @param parent2 [string] Name of the individual's parent
    #' @param haplo [haplotype class] Haplotype of the individual (see:
    #'   \link[myBreedSimulatR]{haplotype})
    #' @param verbose [boolean] display information
    #' @return A new `individual` object.
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
    #'                      effPopSize = 3,
    #'                      simInfo = mySimInfo,
    #'                      verbose = TRUE)
    #'
    #' ### create lociInfo object
    #' myLoci <- lociInfo$new(genoMap = NULL, specie = mySpec)
    #'
    #'
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
    #' ### simulate haplotype
    #' rawHaplo <- matrix(sample(c(0, 1), (3 + 4 + 5) * 2, replace = TRUE),
    #'                    nrow = 2)
    #' colnames(rawHaplo) <- paste0("Locus_", 1:(3 + 4 + 5))
    #'
    #' haplo <- myBreedSimulatR::haplotype$new(lociInfo = myLoci,
    #'                                         haplo = rawHaplo)
    #'
    #' ### create individuals:
    #' myInd <- individual$new(name = "Ind 1",
    #'                         specie = myTrait$lociInfo$specie,
    #'                         traitInfo = myTrait,
    #'                         parent1 = "OkaaSan", parent2 = "OtouSan",
    #'                         haplo = myHaplo, verbose = TRUE)

    initialize = function(name = "Unnamed",
                          specie = specie$new(),
                          traitInfo = NULL,
                          parent1 = NA,
                          parent2 = NA,
                          haplo = NULL,
                          verbose = TRUE){
      # specie class
      if (class(specie)[1] != "Specie") {
        stop(paste('class(specie)[1] != "Specie"\n"specie" must be a',
                   'Specie object see: ?specie'))
      }

      # traitInfo class
      if (!is.null(traitInfo)) {
        if (class(traitInfo)[1] != "traitInfo") {
          stop(paste('class(traitInfo)[1] != "traitInfo"\n"traitInfo" must be a',
                     'traitInfo object see: ?traitInfo'))
        }
      }

      # haplo class
      if (!is.null(haplo)) {
        if (class(haplo)[1] != "haplotype") {
          stop(paste('class(haplo)[1] != "haplotype"\n"haplo" must be a',
                     'haplotype object see: ?haplotype'))
        }
      }

      self$name <- name
      self$specie <- specie
      self$traitInfo <- traitInfo
      self$parent1 <- parent1
      self$parent2 <- parent2
      self$haplo <- haplo
      private$checkHaplo()
      if (verbose) cat(paste("A new ind is borned:", self$name, "!"))
    },

    #' @description Generate Gametes
    #' @param n [float] number of gametes to create (default: 1)
    #' @return list of gametes. A gamete is a named vectors with value 0 or 1.
    #' @examples
    #' myInd$generateGametes()
    #' myInd$generateGametes(2)
    #'
    # generateGametes = function(n = 1) {
    #   gametes <- lapply(1:n, function(x) {
    #     gamete <- mapply(function(haploNow, genoMap) {
    #       chrName <- genoMap$chr[1]
    #       chrLen <- self$specie$lChr[chrName]
    #
    #       if (self$specie$recombRateOneVal) {
    #         if (is.na(self$specie$recombRate)) {
    #           stop('specie$recombRate must be specify in order to generate gemtes')
    #         }
    #         # number of recombination events:
    #         nRecomb <- rbinom(1, chrLen, self$specie$recombRate)
    #
    #         Rpos <- sample.int(chrLen, nRecomb)
    #
    #         gamHaplo <- integer(ncol(haploNow))
    #         # split SNP beetween two chromosome
    #         g <- runif(1) + 1
    #         if (length(Rpos) == 0) {
    #           gamHaplo <- haploNow[g, ]
    #         } else {
    #           ids <- unique(c(0,
    #                           sort(findInterval(Rpos, genoMap$pos)),
    #                           length(genoMap$pos))
    #           )
    #           for (i in seq_len(length(ids) - 1)) {
    #             gamHaplo[(ids[[i]] + 1):ids[[i + 1]]] <-
    #               haploNow[g, (ids[[i]] + 1):ids[[i + 1]]]
    #             g <- - g + 3
    #           }
    #         }
    #         names(gamHaplo) <- colnames(haploNow)
    #       } else {
    #         rec <- genoMap$rec
    #         samples <- runif(length(rec))
    #         crossOver <- ((rec - samples) >= 0)
    #         selectHaplo <- cumsum(crossOver) %% 2
    #         selectHaplo <- selectHaplo + 1
    #         whichHaplo <- (runif(1) < 0.5) + 1
    #         gamHaplo <- haploNow[whichHaplo, ]
    #         gamHaplo[selectHaplo == 2] <- haploNow[3 - whichHaplo, selectHaplo == 2]
    #       }
    #
    #       return(gamHaplo)
    #     },
    #     self$haplo$values,
    #     self$haplo$lociInfo$genoMapList,
    #     SIMPLIFY = FALSE)
    #
    #     gamete <- unlist(gamete, use.names = FALSE)
    #     names(gamete) <- names(self$haplo$allelDose)
    #     gamete
    #
    #   })
    #
    #   gametes
    # }

    generateGametes = function(n = 1) {
      gametes <- lapply(1:n, function(x) {
        gamete <- mapply(function(haploNow, genoMap) {
          chrName <- genoMap$chr[1]
          chrLen <- self$specie$lChr[chrName]

          isCrossOver <- runif(1) >= 0.5

          if (isCrossOver) {
            if (self$specie$recombRateOneVal) {
              if (is.na(self$specie$recombRate)) {
                stop('specie$recombRate must be specify in order to generate gemtes')
              }
              # number of recombination events:
              nRecomb <- rbinom(1, chrLen, 2 * self$specie$recombRate)

              Rpos <- sample.int(chrLen, nRecomb)

              gamHaplo <- integer(ncol(haploNow))
              # split SNP beetween two chromosome
              g <- runif(1) + 1
              if (length(Rpos) == 0) {
                gamHaplo <- (runif(1) < 0.5) + 1
              } else {
                ids <- unique(c(0,
                                sort(findInterval(Rpos, genoMap$pos)),
                                length(genoMap$pos))
                )
                for (i in seq_len(length(ids) - 1)) {
                  gamHaplo[(ids[[i]] + 1):ids[[i + 1]]] <-
                    haploNow[g, (ids[[i]] + 1):ids[[i + 1]]]
                  g <- - g + 3
                }
              }
              names(gamHaplo) <- colnames(haploNow)
            } else {
              rec <- genoMap$rec
              samples <- runif(length(rec))
              crossOverProb <- rec
              crossOverProb[2:length(rec)] <- 2 * crossOverProb[2:length(rec)]
              crossOver <- ((crossOverProb - samples) >= 0)
              selectHaplo <- cumsum(crossOver) %% 2
              selectHaplo <- selectHaplo + 1
              whichHaplo <- (runif(1) < 0.5) + 1
              gamHaplo <- haploNow[whichHaplo, ]
              gamHaplo[selectHaplo == 2] <- haploNow[3 - whichHaplo, selectHaplo == 2]
            }
          } else {
            whichHaplo <- (runif(1) < 0.5) + 1
            gamHaplo <- haploNow[whichHaplo, ]
          }


          return(gamHaplo)
        },
        self$haplo$values,
        self$haplo$lociInfo$genoMapList,
        SIMPLIFY = FALSE)

        gamete <- unlist(gamete, use.names = FALSE)
        names(gamete) <- names(self$haplo$allelDose)

        return(gamete)
      })

      return(gametes)
    }
  ),
  active = list(
    #' @field trueAGVs [numeric] true additive genotypic values
    trueAGVs = function() {
      traitInfo <- self$traitInfo
      haplo <- self$haplo
      if (!is.null(traitInfo)) {
        if (traitInfo$lociInfo$specie$simInfo$simPheno) {
          trueAGVs <- sapply(1:traitInfo$nTraits, function(traitNo) {
            qtlPosNow <- traitInfo$qtlPos[[traitNo]]
            qtlGenoNow <- haplo$allelDose[qtlPosNow]
            qtlEffNow <- traitInfo$qtlEff[[traitNo]]
            actionTypeNow <- traitInfo$actionType[[traitNo]]

            additiveGV <- qtlGenoNow[actionTypeNow == 0] / 2 *
              qtlEffNow[actionTypeNow == 0]

            return(sum(additiveGV))
          })
        } else {
          trueAGVs <- rep(NA, traitInfo$nTraits)
        }
        names(trueAGVs) <- traitInfo$traitNames
      } else {
        trueAGVs <- NULL
      }

      return(trueAGVs)
    },

    #' @field trueAGVETs [numeric] true additive genotypic values specific to each trait
    trueAGVETs = function() {
      traitInfo <- self$traitInfo
      haplo <- self$haplo
      if (!is.null(traitInfo)) {
        if (traitInfo$lociInfo$specie$simInfo$simPheno) {
          trueAGVETs <- sapply(1:traitInfo$nTraits, function(traitNo) {
            qtlEachPosNow <- traitInfo$qtlEachPos[[traitNo]]
            qtlEachNamesNow <- traitInfo$qtlEachNamesList[[traitNo]]
            qtlEachGenoNow <- haplo$allelDose[qtlEachPosNow]
            qtlEachEffNow <- traitInfo$qtlEachEff[[traitNo]]
            actionTypeEachNow <- (traitInfo$actionType[[traitNo]])[qtlEachNamesNow]

            additiveGVEach <- qtlEachGenoNow[actionTypeEachNow == 0] / 2 *
              qtlEachEffNow[actionTypeEachNow == 0]

            return(sum(additiveGVEach))
          })
        } else {
          trueAGVETs <- rep(NA, traitInfo$nTraits)
        }
        names(trueAGVETs) <- traitInfo$traitNames
      } else {
        trueAGVETs <- NULL
      }

      return(trueAGVETs)
    },

    #' @field trueAGVCTs [numeric] true additive genotypic values common across traits
    trueAGVCTs = function() {
      traitInfo <- self$traitInfo
      if (!is.null(traitInfo)) {
        if (traitInfo$lociInfo$specie$simInfo$simPheno) {
          trueAGVCTs <- self$trueAGVs - self$trueAGVETs
        } else {
          trueAGVCTs <- rep(NA, traitInfo$nTraits)
        }
        names(trueAGVCTs) <- traitInfo$traitNames
      } else {
        trueAGVCTs <- NULL
      }

      return(trueAGVCTs)
    },


    #' @field trueDGVs [numeric] true dominant genotypic values
    trueDGVs = function() {
      traitInfo <- self$traitInfo
      haplo <- self$haplo
      if (traitInfo$lociInfo$specie$simInfo$simPheno) {
        if (!is.null(traitInfo)) {
          trueDGVs <- sapply(1:traitInfo$nTraits, function(traitNo) {
            qtlPosNow <- traitInfo$qtlPos[[traitNo]]
            qtlGenoNow <- haplo$allelDose[qtlPosNow]
            qtlEffNow <- traitInfo$qtlEff[[traitNo]]
            actionTypeNow <- traitInfo$actionType[[traitNo]]

            dominanceGV <- ifelse(qtlGenoNow[actionTypeNow == 1] == 1,
                                  qtlEffNow[actionTypeNow == 1], 0)

            return(sum(dominanceGV))
          })
        } else {
          trueDGVs <- rep(NA, traitInfo$nTraits)
        }
        names(trueDGVs) <- traitInfo$traitNames
      } else {
        trueDGVs <- NULL
      }

      return(trueDGVs)
    },


    #' @field trueDGVETs [numeric] true dominance genotypic values specific to each trait
    trueDGVETs = function() {
      traitInfo <- self$traitInfo
      haplo <- self$haplo
      if (!is.null(traitInfo)) {
        if (traitInfo$lociInfo$specie$simInfo$simPheno) {
          trueDGVETs <- sapply(1:traitInfo$nTraits, function(traitNo) {
            qtlEachPosNow <- traitInfo$qtlEachPos[[traitNo]]
            qtlEachNamesNow <- traitInfo$qtlEachNamesList[[traitNo]]
            qtlEachGenoNow <- haplo$allelDose[qtlEachPosNow]
            qtlEachEffNow <- traitInfo$qtlEachEff[[traitNo]]
            actionTypeEachNow <- (traitInfo$actionType[[traitNo]])[qtlEachNamesNow]

            dominanceGVEach <- ifelse(qtlEachGenoNow[actionTypeEachNow == 1] == 1,
                                      qtlEachEffNow[actionTypeEachNow == 1], 0)

            return(sum(dominanceGVEach))
          })
        } else {
          trueDGVETs <- rep(NA, traitInfo$nTraits)
        }
        names(trueDGVETs) <- traitInfo$traitNames
      } else {
        trueDGVETs <- NULL
      }

      return(trueDGVETs)
    },

    #' @field trueDGVCTs [numeric] true dominant genotypic values common across traits
    trueDGVCTs = function() {
      traitInfo <- self$traitInfo
      if (!is.null(traitInfo)) {
        if (traitInfo$lociInfo$specie$simInfo$simPheno) {
          trueDGVCTs <- self$trueDGVs - self$trueDGVETs
        } else {
          trueDGVCTs <- rep(NA, traitInfo$nTraits)
        }
        names(trueDGVCTs) <- traitInfo$traitNames
      } else {
        trueDGVCTs <- NULL
      }

      return(trueDGVCTs)
    },



    #' @field trueEGVs [numeric] true epistatic genotypic values
    trueEGVs = function() {
      traitInfo <- self$traitInfo
      haplo <- self$haplo
      if (!is.null(traitInfo)) {
        if (traitInfo$lociInfo$specie$simInfo$simPheno) {
          trueEGVs <- sapply(1:traitInfo$nTraits, function(traitNo) {
            nEpiNow <- traitInfo$nEpi[traitNo]

            if (nEpiNow > 0) {
              qtlEpiPosNow <- traitInfo$qtlEpiPos[[traitNo]]
              qtlGenoEpiNow <- matrix(haplo$allelDose[qtlEpiPosNow],
                                      nrow = nEpiNow)
              qtlEpiEffNow <- traitInfo$qtlEpiEff[[traitNo]]
              actionTypeEpiNow <- traitInfo$actionTypeEpi[[traitNo]]


              qtlGenoEpiNow[actionTypeEpiNow == 0] <- qtlGenoEpiNow[actionTypeEpiNow == 0] / 2
              qtlGenoEpiNow[actionTypeEpiNow == 1] <- ifelse(qtlGenoEpiNow[actionTypeEpiNow == 1] == 1, 1, 0)

              return(sum(qtlEpiEffNow * apply(qtlGenoEpiNow, 1, prod)))

              # epistasisGV <- sapply(1:nEpiNow, function(epiNow) {
              #   qtlGenoEpiEachNow <- qtlGenoEpiNow[epiNow, ]
              #   qtlGenoEpiEachNow[actionTypeEpiNow[epiNow, ] == 0] <-
              #     qtlGenoEpiEachNow[actionTypeEpiNow[epiNow, ] == 0] / 2
              #   qtlGenoEpiEachNow[actionTypeEpiNow[epiNow, ] == 1] <-
              #     ifelse(qtlGenoEpiEachNow[actionTypeEpiNow[epiNow, ] == 1] == 1, 1, 0)
              #
              #   epistasisGVEach <- qtlEpiEffNow[epiNow] * prod(qtlGenoEpiEachNow)
              # })
              #
              # return(sum(epistasisGV))
            } else {
              return(0)
            }
          })
        } else {
          trueEGVs <- rep(NA, traitInfo$nTraits)
        }
        names(trueEGVs) <- traitInfo$traitNames

      } else {
        trueEGVs <- NULL
      }

      return(trueEGVs)
    },


    #' @field trueEGVETs [numeric] true epistatic genotypic values between effects specific to each trait
    trueEGVETs = function() {
      traitInfo <- self$traitInfo
      haplo <- self$haplo
      if (!is.null(traitInfo)) {
        if (traitInfo$lociInfo$specie$simInfo$simPheno) {
          trueEGVETs <- sapply(1:traitInfo$nTraits, function(traitNo) {
            nEpiNow <- traitInfo$nEpi[traitNo]

            if (nEpiNow > 0) {
              qtlEpiPosNow <- traitInfo$qtlEpiPos[[traitNo]]
              qtlEpiNamesNow <- traitInfo$qtlEpiNamesList[[traitNo]]
              qtlPleNamesNow <- traitInfo$qtlPleNamesList[[traitNo]]

              includePleEff <- apply(sapply(1:length(qtlPleNamesNow), function(i) {
                stringr::str_detect(string = qtlEpiNamesNow, pattern = qtlPleNamesNow[i])
              }), 1, any)
              qtlEachEpiPosNow <- qtlEpiPosNow[!includePleEff, , drop = FALSE]
              nEpiEachNow <- nrow(qtlEachEpiPosNow)

              if (nEpiEachNow > 0) {
                qtlEachGenoEpiNow <- matrix(haplo$allelDose[qtlEachEpiPosNow],
                                            nrow = nEpiEachNow)
                qtlEachEpiEffNow <- (traitInfo$qtlEpiEff[[traitNo]])[!includePleEff]
                actionTypeEachEpiNow <- (traitInfo$actionTypeEpi[[traitNo]])[!includePleEff, , drop = FALSE]


                qtlEachGenoEpiNow[actionTypeEachEpiNow == 0] <- qtlEachGenoEpiNow[actionTypeEachEpiNow == 0] / 2
                qtlEachGenoEpiNow[actionTypeEachEpiNow == 1] <- ifelse(qtlEachGenoEpiNow[actionTypeEachEpiNow == 1] == 1, 1, 0)

                return(sum(qtlEachEpiEffNow * apply(qtlEachGenoEpiNow, 1, prod)))

                # epistasisEachGV <- sapply(1:nEpiEachNow, function(epiNow) {
                #   qtlEachGenoEpiEachNow <- qtlEachGenoEpiNow[epiNow, ]
                #   qtlEachGenoEpiEachNow[actionTypeEachEpiNow[epiNow, ] == 0] <-
                #     qtlEachGenoEpiEachNow[actionTypeEachEpiNow[epiNow, ] == 0] / 2
                #   qtlEachGenoEpiEachNow[actionTypeEachEpiNow[epiNow, ] == 1] <-
                #     ifelse(qtlEachGenoEpiEachNow[actionTypeEachEpiNow[epiNow, ] == 1] == 1, 1, 0)
                #
                #   epistasisEachGVEach <- qtlEachEpiEffNow[epiNow] * prod(qtlEachGenoEpiEachNow)
                #
                #   return(epistasisEachGVEach)
                # })
                #
                # return(sum(epistasisEachGV))
              } else {
                return(0)
              }
            } else {
              return(0)
            }
          })
        } else {
          trueEGVETs <- rep(NA, traitInfo$nTraits)
        }
        names(trueEGVETs) <- traitInfo$traitNames

      } else {
        trueEGVETs <- NULL
      }

      return(trueEGVETs)
    },


    #' @field trueEGVCTs [numeric] true epistatic genotypic values between effects common across each trait
    trueEGVCTs = function() {
      traitInfo <- self$traitInfo
      haplo <- self$haplo
      if (!is.null(traitInfo)) {
        if (traitInfo$lociInfo$specie$simInfo$simPheno) {
          trueEGVCTs <- sapply(1:traitInfo$nTraits, function(traitNo) {
            nEpiNow <- traitInfo$nEpi[traitNo]

            if (nEpiNow > 0) {
              qtlEpiPosNow <- traitInfo$qtlEpiPos[[traitNo]]
              qtlEpiNamesNow <- traitInfo$qtlEpiNamesList[[traitNo]]
              qtlEachNamesNow <- traitInfo$qtlEachNamesList[[traitNo]]
              qtlPleNamesNow <- traitInfo$qtlPleNamesList[[traitNo]]

              includeEachEff <- apply(sapply(1:length(qtlEachNamesNow), function(i) {
                stringr::str_detect(string = qtlEpiNamesNow, pattern = qtlEachNamesNow[i])
              }), 1, any)
              qtlPleEpiPosNow <- qtlEpiPosNow[!includeEachEff, , drop = FALSE]
              nEpiPleNow <- nrow(qtlPleEpiPosNow)

              if (nEpiPleNow > 0) {
                qtlPleGenoEpiNow <- matrix(haplo$allelDose[qtlPleEpiPosNow],
                                            nrow = nEpiPleNow)
                qtlPleEpiEffNow <- (traitInfo$qtlEpiEff[[traitNo]])[!includeEachEff]
                actionTypePleEpiNow <- (traitInfo$actionTypeEpi[[traitNo]])[!includeEachEff, , drop = FALSE]


                qtlPleGenoEpiNow[actionTypePleEpiNow == 0] <- qtlPleGenoEpiNow[actionTypePleEpiNow == 0] / 2
                qtlPleGenoEpiNow[actionTypePleEpiNow == 1] <- ifelse(qtlPleGenoEpiNow[actionTypePleEpiNow == 1] == 1, 1, 0)

                return(sum(qtlPleEpiEffNow * apply(qtlPleGenoEpiNow, 1, prod)))


                # epistasisPleGV <- sapply(1:nEpiPleNow, function(epiNow) {
                #   qtlPleGenoEpiEachNow <- qtlPleGenoEpiNow[epiNow, ]
                #   qtlPleGenoEpiEachNow[actionTypePleEpiNow[epiNow, ] == 0] <-
                #     qtlPleGenoEpiEachNow[actionTypePleEpiNow[epiNow, ] == 0] / 2
                #   qtlPleGenoEpiEachNow[actionTypePleEpiNow[epiNow, ] == 1] <-
                #     ifelse(qtlPleGenoEpiEachNow[actionTypePleEpiNow[epiNow, ] == 1] == 1, 1, 0)
                #
                #   epistasisPleGVEach <- qtlPleEpiEffNow[epiNow] * prod(qtlPleGenoEpiEachNow)
                #
                #   return(epistasisPleGVEach)
                # })
                #
                # return(sum(epistasisPleGV))
              } else {
                return(0)
              }
            } else {
              return(0)
            }
          })
        } else {
          trueEGVCTs <- rep(NA, traitInfo$nTraits)
        }
        names(trueEGVCTs) <- traitInfo$traitNames

      } else {
        trueEGVCTs <- NULL
      }

      return(trueEGVCTs)
    },


    #' @field trueGVs [numeric] true genotypic values
    trueGVs = function() {
      traitInfo <- self$traitInfo
      if (!is.null(traitInfo)) {
        if (traitInfo$lociInfo$specie$simInfo$simPheno) {
          trueAGVs <- self$trueAGVs
          trueDGVs <- self$trueDGVs
          trueEGVs <- self$trueEGVs

          trueGVs <- trueAGVs + trueDGVs + trueEGVs
        } else {
          trueGVs <- rep(NA, traitInfo$nTraits)
        }
        names(trueGVs) <- traitInfo$traitNames
      } else {
        trueGVs <- NULL
      }

      return(trueGVs)
    },

    #' @field trueGVETs [numeric] true genotypic values specific to each trait
    trueGVETs = function() {
      traitInfo <- self$traitInfo
      if (!is.null(traitInfo)) {
        if (traitInfo$lociInfo$specie$simInfo$simPheno) {
          trueAGVETs <- self$trueAGVETs
          trueDGVETs <- self$trueDGVETs
          trueEGVETs <- self$trueEGVETs

          trueGVETs <- trueAGVETs + trueDGVETs + trueEGVETs
        } else {
          trueGVETs <- rep(NA, traitInfo$nTraits)
        }
        names(trueGVETs) <- traitInfo$traitNames
      } else {
        trueGVETs <- NULL
      }

      return(trueGVETs)
    },



    #' @field trueGVCTs [numeric] true genotypic values common across trait
    trueGVCTs = function() {
      traitInfo <- self$traitInfo
      if (!is.null(traitInfo)) {
        if (traitInfo$lociInfo$specie$simInfo$simPheno) {
          trueAGVCTs <- self$trueAGVCTs
          trueDGVCTs <- self$trueDGVCTs
          trueEGVCTs <- self$trueEGVCTs

          trueGVCTs <- trueAGVCTs + trueDGVCTs + trueEGVCTs
        } else {
          trueGVCTs <- rep(NA, traitInfo$nTraits)
        }
        names(trueGVCTs) <- traitInfo$traitNames
      } else {
        trueGVCTs <- NULL
      }

      return(trueGVCTs)
    }

  ),
  private = list(
    # @description
    # Check the number of chromosomes between `haplo` and `specie` is the
    # same
    # @return boolean
    checkHaplo = function() {
      if (length(self$haplo$values) != self$specie$nChr) {
        stop(paste0("Number of chromosomes in haplo is; ",
                    length(self$haplo$values), " but must be equal to: ",
                    self$specie$nChr, " (number of chr of ",
                    self$specie$name, ")"))
      }
    }
  )
)
