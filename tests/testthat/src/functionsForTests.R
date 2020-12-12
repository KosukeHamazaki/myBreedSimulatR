# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# File defining usefull function for test scripts



##### Initialisation functions ####
create_spec <- function(nChr = round(runif(1, 1, 10)),
                        lChr = round(pmax(rnorm(nChr, 450, 50), 200)),# > 200
                        ploidy = 2,
                        recombRate = 3 / sum(lChr),
                        name = "Undefinded") {

  specie$new(nChr = nChr,
             lChr = lChr,
             ploidy = ploidy,
             recombRate = recombRate,
             specName = name,
             verbose = F)
}

create_SNP <- function(spec, nMarker = NULL){

  if (is.null(nMarker)) {
    nMarker <- round(spec$lChr/10)
  } else {
    stopifnot(all(nMarker < spec$lChr))
    nMarker <- rep(nMarker, spec$nChr)
  }

  # generate arbitrary marker position
  pos <- c()
  for (i in seq(spec$nChr)) {
    pos <- c(pos, sample(spec$lChr[i], nMarker[i]))
  }

  # generate arbitrary lociNames
  lociNames <- .charSeq("Loci", sample(sum(nMarker)*50, sum(nMarker)))
  genoMap <- data.frame(chr = rep(spec$chrNames, times = nMarker),
                         pos = pos,
                         lociNames = lociNames)
  # create lociInfo object
  myLoci <- lociInfo$new(genoMap = genoMap, specie = spec)
  SNPs
}

create_haplo <- function(SNPs, af = NULL){

  if (is.null(af)) {
    af <- 0.5
  } else {
    stopifnot(length(af) == myLoci$nLoci())
  }

  rawHaplo <- matrix(rbinom(myLoci$nLoci() * myLoci$specie$ploidy, 1,
                            af),
                     nrow = myLoci$specie$ploidy)
  colnames(rawHaplo) <- myLoci$genoMap$lociNames
  haplo <- haplotype$new(lociInfo = myLoci,
                         haplo = rawHaplo)
}


create_inds <- function(haploList){
  if (class(haploList)[1]  == "Haplotype") {
    haploList <- list(haploList)
  }

  spec <- haploList[[1]]$lociInfo$specie
  inds <- mapply(function(haplo, id){
    myInd <- individual$new(name = paste("Ind", id),
                            specie = spec,
                            parent1 = paste("OkaaSan", id),
                            parent2 = paste("OtouSan", id),
                            haplo = haplo,
                            verbose = F)
  },
  haploList, c(seq_along(haploList)))

  if (length(inds) == 1) {
    return(inds[[1]])
  }
  inds
}

.charSeq <- breedSimulatR:::.charSeq
