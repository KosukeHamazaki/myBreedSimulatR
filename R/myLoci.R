# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# Definition of lociInfo class




#' R6 Class representing a set of SNP markers
#'
#' @description lociInfo object store specific information of a set of SNP
#' markers including QTLs
#'
#' @details This class is useful for setting the Haplotype class. (see:
#' \code{\link[myBreedSimulatR]{haplotype}})
#'
#' @export
#' @import R6
lociInfo <- R6::R6Class(
  "lociInfo",
  public = list(
    #' @field genoMap [data.frame] Coordinate of all SNPs.
    #'
    #' 3 columns:
    #' \itemize{
    #'  \item{\code{lociNames}:} {SNP&QTL's IDs}
    #'  \item{\code{chr}:} {Chromosome holding the SNP}
    #'  \item{\code{pos}:} {SNP position on the chromosome}
    #'  }
    genoMap = data.frame(lociNames = c(),
                         chr = c(),
                         pos = c(),
                         stringsAsFactors = FALSE),

    #' @field specie [specie class] Specie of the SNPs
    #'   (see:\link[myBreedSimulatR]{specie})
    specie = NULL,

    #' @field posGeneticDist [logical] Position is based on genetic distance or not (physical distance).
    #' If you simulate genome data, this argument will automatically be TRUE.
    #' If this argument is FALSE, `specie$recombRateOneVal` will be TRUE.
    posGeneticDist = NULL,

    #' @field seedSimGeno [numeric] Seed for simulating marker genotype
    seedSimGeno = NA,

    #' @field founderHaplo [matrix] Haplotype of founders (\eqn{n \times m} matrix) by simulation (for real data, founderHaplo = NULL)
    founderHaplo = NULL,

    #' @field genoMapList [list] Named list of dataframes with the coordinate
    #'   of all SNPs. for each chromosomes
    genoMapList = list(),

    #' @field ids [list] Named list of the SNP ids for all chromosomes
    ids = list(),

    #' @description Create a new lociInfo object.
    #' @param genoMap [data.frame] Coordinate of all SNPs. (needed for real data)
    #'
    #'   3 columns: \itemize{ \item{\code{chr}:} {Chromosome holding the SNP}
    #'   \item{\code{pos}:} {SNP position on the chromosome}
    #'   \item{\code{lociNames}:} {SNP's IDs} }
    #' @param specie [specie class] Specie of the SNPs
    #'   (see:\link[myBreedSimulatR]{specie})
    #' @param posGeneticDist [logical] Position is based on genetic distance or not (physical distance).
    #' If you simulate genome data, this argument will automatically be TRUE.
    #' If this argument is FALSE, `specie$recombRateOneVal` will be TRUE.
    #' @param seedSimGeno [numeric] Random seed for simulation of marker genotype (optional)
    #' @param lociNames [character] Names of the markers used for simulated marker genotype (optional)
    #' @param founderNames [character] Names of the founders. It should be equal to ploidy * effective population size (optional)
    #' @return A new `lociInfo` object.
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
    #'
    #' ### create specie information
    #' mySpec <- specie$new(nChr = 3,
    #'                      lChr = c(100, 150, 200),
    #'                      specName = "Example 1",
    #'                      ploidy = 2,
    #'                      mutRate = 10^-8,
    #'                      recombRate = 10^-7,
    #'                      recombRateOneVal = FALSE,
    #'                      chrNames = c("C1", "C2", "C3"),
    #'                      nLoci = 100,
    #'                      effPopSize = 100,
    #'                      simInfo = mySimInfo,
    #'                      verbose = TRUE)
    #'
    #' ### create SNP information
    #' myLoci <- lociInfo$new(genoMap = NULL,
    #'                      specie = mySpec,
    #'                      seedSimGeno = NA,
    #'                      lociNames = NULL,
    #'                      founderNames = NULL)
    #' print(myLoci)
    #'
    initialize = function(genoMap = NULL,
                          specie,
                          posGeneticDist = TRUE,
                          seedSimGeno = NA,
                          lociNames = NULL,
                          founderNames = NULL) {

      # CHECKS:
      # parameters classes
      if (class(specie)[1] != "Specie") {
        stop('"class(specie)" must be "Specie"')
      }

      if (!specie$simInfo$simGeno) {
        if (is.null(genoMap)) {
          stop("You must specify the `genoMap` information because you use don't simulate marker genotype data.")
        }

        if (!posGeneticDist) {
          specie$recombRateOneVal <- TRUE
          message("You use real data and phyical position for genetic map.\n",
                  "Thus, `specie$recombRateOneVal` will be set to TRUE.")
        }
      } else {
        posGeneticDist <- TRUE

        nChr <- specie$nChr
        nCoreTotal <- specie$simInfo$nCoreMax
        nCorePerGeno <- specie$simInfo$nCorePerGeno
        nCoreNow <- max(nCoreTotal %/% nCorePerGeno, 1)
        lChr <- specie$lChr
        nLoci <- specie$nLoci
        effPopSizeHaplo <- specie$ploidy * specie$effPopSize

        minMAF <- 0.01
        recBTpieces <- 1e-04
        piecesPerM <- 1 / recBTpieces
        nPiecesPerChr <- lChr / 100 * piecesPerM
        mutRate <- specie$mutRate

        if (!is.null(seedSimGeno)) {
          if (!is.na(seedSimGeno)) {
            stopifnot(is.numeric(seedSimGeno))
            seedSimGeno <- floor(seedSimGeno)
          } else {
            seedSimGeno <- sample(x = 1e9, size = 1)
          }
        }

        if (is.null(lociNames)) {
          lociNames <- paste0("Locus_", 1:sum(nLoci))
        }

        if (is.null(founderNames)) {
          founderNames <- paste0("Founder_", 1:effPopSizeHaplo)
        }



        if (length(lociNames) != sum(nLoci)) {
          stop("`length(lociNames)` should be equal to `sum(self$specie$nLoci)`!")
        }

        if (length(founderNames) != effPopSizeHaplo) {
          stop("`length(founderNames)` should be equal to `self$specie$ploidy * self$specie$effPopSize`!")
        }


        # if ((length(unique(lChr)) == 1) & (length(unique(nLoci)) == 1)) {
        # coalSim <- getCoalescentSim(effPopSize = effPopSizeHaplo, nMrkOrMut = sum(nLoci),
        #                             nChr = nChr, nPiecesPerChr = unique(nPiecesPerChr), mutRate = mutRate,
        #                             recBTpieces = recBTpieces, minMAF = minMAF, seed = seedSimGeno)
        # coalSim$map$Chr <- specie$chrNames[coalSim$map$Chr]
        # } else {
        coalSimList <- pbmcmapply(FUN = function(nLociNow, nPiecesPerChrNow) {
          coalSimNow <- getCoalescentSim(effPopSize = effPopSizeHaplo, nMrkOrMut = nLociNow,
                                         nChr = 1, nPiecesPerChr = nPiecesPerChrNow, mutRate = mutRate,
                                         recBTpieces = recBTpieces, minMAF = minMAF, seed = seedSimGeno)

          return(list(coalSimNow))
        },
        nLoci,  ### nLociNow
        nPiecesPerChr,   ### nPieciesPerChrNow
        mc.cores = nCoreNow
        )

        coalSimMarkers <- do.call(cbind, lapply(coalSimList, function(x) x$markers))
        coalSimMap <- do.call(rbind, lapply(coalSimList, function(x) x$map))
        coalSimMap$Chr <- specie$chrNames[rep(1:nChr, nLoci)]

        coalSim <- list(markers = coalSimMarkers,
                        map = coalSimMap)
        # }


        genoMap <- data.frame(lociNames = lociNames,
                              chr = coalSim$map$Chr,
                              pos = coalSim$map$Pos,
                              stringsAsFactors = FALSE)

        founderHaplo <- coalSim$markers
        rownames(founderHaplo) <- founderNames
        colnames(founderHaplo) <- lociNames

        self$seedSimGeno <- seedSimGeno
        self$founderHaplo <- founderHaplo
      }


      if (class(genoMap) != "data.frame") {
        stop('"class(genoMap)" must be "data.frame"')
      }
      # genoMap's colnames
      if (!all(colnames(genoMap) %in% c("chr", "pos", "lociNames"))) {
        stop('"colnames(genoMap)" must be "chr", "pos" and "lociNames"')
      }
      if (!all(unique(genoMap$chr) %in% specie$chrNames)) {
        stop(paste0('"Chromosomes\'names specified in "genoMap" do ',
                    'not match those specified in "specie"\n',
                    'unique(genoMap$chr) = ',
                    paste0(unique(genoMap$chr), collapse = " "), '\n',
                    'specie$chrNames = ',
                    paste0(specie$chrNames, collapse = " ")))
      }
      if (length(genoMap$lociNames) != length(unique(genoMap$lociNames))) {
        stop('Some markers appear several times in "genoMap"')
      }

      # remove factors
      genoMap[, vapply(genoMap, is.factor, TRUE)] <-
        lapply(genoMap[, vapply(genoMap, is.factor, TRUE)], as.character)

      # # convert to integer
      # genoMap$pos <- as.integer(genoMap$pos)

      # round pos
      genoMap$pos <- round(genoMap$pos, 2)

      if (posGeneticDist) {
        diffPos <- diff(genoMap$pos)
        recombRates <- (1 - exp(-2 * diffPos / 100)) / 2
        recombRates[recombRates < 0] <- 0.5
        recombRates <- c(0.5, recombRates)
      } else {
        recombRates <- rep(NA, nrow(genoMap))
      }
      genoMap$rec <- recombRates

      # sort position in increasing order (needed for the function
      # "findInterval" in individual's generateGametes method)
      genoMap <- genoMap[order(genoMap$chr, genoMap$pos), ]


      self$genoMap <- genoMap
      self$specie <- specie
      self$posGeneticDist <- posGeneticDist
      self$genoMapList <- split(genoMap,
                                genoMap$chr)

      self$ids <- lapply(specie$chrNames, function(chr){
        genoMap[genoMap$chr == chr, "lociNames"]
      })
      names(self$ids) <- specie$chrNames

      if (!self$specie$simInfo$simGeno) {
        self$specie$nLoci <- self$nLoci(self$specie$chrNames)
        self$specie$lChr <- unlist(lapply(self$genoMapList, function(x) {
          ceiling(max(x$pos))
        }))

        message("Since you use real data, `specie$nLoci` & `specie$lChr` information is updated.")
      }
    },

    #' @description
    #' Get the number of SNPs per chromosomes
    #' @param chr [str or numeric] chromosome id
    #' @examples
    #' myLoci$nLoci()
    #' myLoci$nLoci(c("C2","C3"))
    nLoci = function(chr=NA) {

      if (length(chr) == 1 && is.na(chr)) return(nrow(self$genoMap))

      stopifnot((is.character(chr) || is.numeric(chr)))

      if (is.numeric(chr)) {
        chr <- self$specie$chrNames[chr]
      }
      stopifnot(chr %in% self$specie$chrNames)
      vapply(chr, function(chr) nrow(self$genoMap[self$genoMap$chr == chr,]),
             1)

    },
    #' @description
    #' Get information about specific SNPs
    #' @param lociNames [str] loci ids
    #' @examples
    #' myLoci$getInfo("Loci_1")
    #' myLoci$getInfo(c("Loci_1", "Loci_3"))
    getInfo = function(lociNames) {
      self$genoMap[match(lociNames, self$genoMap$lociNames),]
    },

    #' @description Display summary information about the object: specie, number
    #' of SNP, SNP coordinates.
    #' @param fh  From head. If this argument is TRUE, first part (row) of data will be shown (like head() function).
    #' If FALSE, last part (row) of your data will be shown (like tail() function).
    #' @param fl From left. If this argument is TRUE, first part (column) of data will be shown (like head() function).
    #' If FALSE, last part (column) of your data will be shown (like tail() function).
    #' @param rown  The number of rows shown in console.
    #' @param coln  The number of columns shown in console.
    #' @param rowst  The start point for the direction of row.
    #' @param colst  The start point for the direction of column.
    print = function(fh = TRUE, fl = TRUE,
                     rown = 6, coln = 6,
                     rowst = 1, colst = 1) {
      cat(paste0(
        "specie: ", self$specie$specName, "\n",
        self$nLoci(), " Markers on ",
        length(unique(self$genoMap$chr)), " chromosomes :\n"
      ))
      print(self$nLoci(self$specie$chrNames))
      cat("genoMap:\n")
      df <- self$genoMap
      # df <- df[order(df$lociNames), ]

      See(data = df, fh = fh, fl = fl,
          rown = rown, coln = coln,
          rowst = rowst, colst = colst)
    },

    #' @description
    #' plot chromosome map using the \pkg{plotly} package
    #' @param alpha transparency see \link[plotly]{plot_ly}
    #'
    #' @import plotly
    #'
    #' @examples
    #' myLoci$plot(alpha = 0.1)
    plot = function(alpha = 0.01) {
      ends <- self$specie$lChr

      plotly::plot_ly(data = self$genoMap,
                      x = ~ chr,
                      y = ~ pos,
                      type = "scatter",
                      mode = "markers",
                      alpha = alpha,
                      name = "Loci",
                      hoverinfo = 'text',
                      text = apply(self$genoMap, 1, function(l) {
                        paste(names(l), ":", l, collapse = "\n")
                      })) %>%
        plotly::add_markers(x = rep(names(ends), 2),
                            y = c(ends, rep(0, length(ends))),
                            alpha = 1,
                            name = "Chromosome's edges",
                            hoverinfo = 'text',
                            text = paste(rep(names(ends), 2),
                                         ": length =",
                                         c(ends, rep(0, length(ends)))))
    }
  )
)
