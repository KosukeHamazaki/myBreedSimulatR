# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# Test file for the lociInfo class


# set.seed(6705) # for reproductible RNG


##### Initialisation functions ####
if (interactive()) {
  devtools::load_all()
  source("tests/testthat/src/functionsForTests.R")
} else source("src/functionsForTests.R")



#### TESTS ####
test_that("lociInfo initialization", {
  #### Initialisation:
  mySpec <- create_spec()
  nMarkers <- round(mySpec$lChr/10)

  # generate positions
  pos <- unlist(lapply(seq(mySpec$nChr),
                       function(chr){
                         sample(mySpec$lChr[chr], nMarkers[chr])
                       }))

  # generate arbitrary lociNames
  lociNames <- .charSeq("Loci", sample(sum(nMarkers)*50, sum(nMarkers)))

  # SNP coordinates data.frame
  genoMap <- data.frame(chr = rep(mySpec$chrNames, times = nMarkers),
                         pos = pos,
                         lociNames = lociNames)

  #### Tests:
  # create lociInfo object
  expect_error({myLoci <- lociInfo$new(genoMap = genoMap, specie = mySpec)},
               NA)

  expect_identical(myLoci$specie, mySpec)
  expect_is(myLoci$genoMap, "data.frame")
  expect_is(myLoci$genoMapList, "list")
  expect_equal(length(myLoci$genoMapList), mySpec$nChr)
  expect_identical(names(myLoci$genoMapList), mySpec$chrNames)
  expect_equal(myLoci$nLoci(), sum(nMarkers))

  # check myLoci$genoMap is sorted increasingly for all chromosomes (needed for
  # the function "findInterval" in individual's generateGametes method)
  for (chr in mySpec$chrNames) {
    expect_true(!is.unsorted(myLoci$genoMap[myLoci$genoMap$chr == chr, "pos"]))
  }

})

test_that("lociInfo errors", {
  #### Initialisation:
  mySpec <- create_spec(nChr = 5)
  nMarkers <- round(mySpec$lChr/10)

  # generate positions
  pos <- unlist(lapply(seq(mySpec$nChr),
                       function(chr){
                         sample(mySpec$lChr[chr], nMarkers[chr])
                       }))

  # generate arbitrary lociNames
  lociNames <- .charSeq("Loci", sample(sum(nMarkers)*50, sum(nMarkers)))
  # Test differents chromosome names than those in specie
  chrNames <- c("toto", mySpec$chrNames[-1])
  genoMap <- data.frame(chr = rep(chrNames, times = nMarkers),
                         pos = pos,
                         lociNames = lociNames)
  expect_error({myLoci <- lociInfo$new(genoMap = genoMap, specie = mySpec)},
               paste('"Chromosomes\'names specified in "genoMap"',
                     'do not match those specified in "specie"'))

  genoMap <- data.frame(chr = rep(chrNames, times = nMarkers),
                         pos = pos,
                         lociNames = lociNames,
                         stringsAsFactors = FALSE)
  expect_error({myLoci <- lociInfo$new(genoMap = genoMap, specie = mySpec)},
               paste('"Chromosomes\'names specified in "genoMap"',
                     'do not match those specified in "specie"'))

})




test_that("lociInfo stringAsFactor",{
  #### Initialisation:
  mySpec <- create_spec()
  nMarkers <- round(mySpec$lChr/10)

  # generate positions
  pos <- unlist(lapply(seq(mySpec$nChr),
                       function(chr){
                         sample(mySpec$lChr[chr], nMarkers[chr])
                       }))

  # generate arbitrary lociNames
  lociNames <- .charSeq("Loci", sample(sum(nMarkers)*50, sum(nMarkers)))

  # SNP coordinates data.frame
  genoMap_SAF_T <- data.frame(chr = rep(mySpec$chrNames, times = nMarkers),
                         pos = pos,
                         lociNames = lociNames,
                         stringsAsFactors = TRUE)
  genoMap_SAF_F <- data.frame(chr = rep(mySpec$chrNames, times = nMarkers),
                               pos = pos,
                               lociNames = lociNames,
                               stringsAsFactors = FALSE)

  # create lociInfo object
  SNPs_SAF_T <- lociInfo$new(genoMap = genoMap_SAF_T, specie = mySpec)
  SNPs_SAF_F <- lociInfo$new(genoMap = genoMap_SAF_F, specie = mySpec)

  #### Tests:
  expect_error(expect_identical(SNPs_SAF_T$genoMap, genoMap_SAF_T))
  expect_equal(summary(SNPs_SAF_T$genoMap), summary(genoMap_SAF_F))
  expect_equal(SNPs_SAF_T, SNPs_SAF_F)
})

test_that("lociInfo nLoci method", {
  #### Initialisation:
  mySpec <- create_spec(nChr = 3)
  nMarkers <- round(mySpec$lChr/10)

  # generate positions
  pos <- unlist(lapply(seq(mySpec$nChr),
                       function(chr){
                         sample(mySpec$lChr[chr], nMarkers[chr])
                       }))

  # generate arbitrary lociNames
  lociNames <- .charSeq("Loci", sample(sum(nMarkers)*50, sum(nMarkers)))

  # SNP coordinates data.frame
  genoMap <- data.frame(chr = rep(mySpec$chrNames, times = nMarkers),
                         pos = pos,
                         lociNames = lociNames)

  # create lociInfo object
  myLoci <- lociInfo$new(genoMap = genoMap, specie = mySpec)

  #### Tests:
  expect_equal(myLoci$nLoci(), sum(nMarkers))
  expect_named(myLoci$nLoci(1))
  expect_equal(myLoci$nLoci(1), nMarkers[1])
  expect_equal(myLoci$nLoci("Chr2"), nMarkers[2])
  expect_equal(myLoci$nLoci(c("Chr2", "Chr3")), nMarkers[c(2, 3)])
})


test_that("lociInfo getInfo method", {
  #### Initialisation:
  mySpec <- create_spec(nChr = 3)
  nMarkers <- round(mySpec$lChr/10)

  # generate positions
  pos <- unlist(lapply(seq(mySpec$nChr),
                       function(chr){
                         sample(mySpec$lChr[chr], nMarkers[chr])
                       }))

  # generate arbitrary lociNames
  lociNames <- .charSeq("Loci", sample(sum(nMarkers)*50, sum(nMarkers)))

  # SNP coordinates data.frame
  genoMap <- data.frame(chr = rep(mySpec$chrNames, times = nMarkers),
                         pos = pos,
                         lociNames = lociNames)

  # create lociInfo object
  myLoci <- lociInfo$new(genoMap = genoMap, specie = mySpec)

  snp <- sample(lociNames, 1)
  expect_identical(myLoci$getInfo(snp),
                   myLoci$genoMap[myLoci$genoMap$lociNames == snp, ])
  snps <- sort(sample(lociNames, 10))
  ref <- myLoci$genoMap[myLoci$genoMap$lociNames %in% snps, ]
  ref <- ref[order(ref$lociNames), ]
  expect_equal(myLoci$getInfo(snps),
               ref)
})


test_that("lociInfo plot method", {
  #### Initialisation:
  mySpec <- create_spec(nChr = 3)
  nMarkers <- round(mySpec$lChr/10)

  # generate positions
  pos <- unlist(lapply(seq(mySpec$nChr),
                       function(chr){
                         sample(mySpec$lChr[chr], nMarkers[chr])
                       }))

  # generate arbitrary lociNames
  lociNames <- .charSeq("Loci", sample(sum(nMarkers)*50, sum(nMarkers)))

  # SNP coordinates data.frame
  genoMap <- data.frame(chr = rep(mySpec$chrNames, times = nMarkers),
                         pos = pos,
                         lociNames = lociNames)

  # create lociInfo object
  myLoci <- lociInfo$new(genoMap = genoMap, specie = mySpec)


  expect_error({p <- myLoci$plot(alpha = 1)}, NA)
  expect_is(p, "plotly")
})



test_that("lociInfo's \"print\" methods", {
  #### Initialisation:
  mySpec <- create_spec(nChr = 20)
  nMarkers <- round(mySpec$lChr/10)

  # generate positions
  pos <- unlist(lapply(seq(mySpec$nChr),
                       function(chr){
                         sample(mySpec$lChr[chr], nMarkers[chr])
                       }))

  # generate arbitrary lociNames
  lociNames <- .charSeq("Loci", sample(sum(nMarkers)*50, sum(nMarkers)))

  # SNP coordinates data.frame
  genoMap <- data.frame(chr = rep(mySpec$chrNames, times = nMarkers),
                         pos = pos,
                         lociNames = lociNames)

  # create lociInfo object
  myLoci <- lociInfo$new(genoMap = genoMap, specie = mySpec)

  expect_output(print(SNPs), "specie: Undefinded")
  expect_output(print(SNPs),
                paste(sum(nMarkers),
                      "Markers on",
                      mySpec$nChr, "chromosomes :"))
  expect_output(print(SNPs), "genoMap:")
})
