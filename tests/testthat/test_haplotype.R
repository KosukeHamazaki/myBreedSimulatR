# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# Test File -- test haplotype class.


# set.seed(8241) # for reproductible RNG

##### Initialisation functions ####
if (interactive()) {
  devtools::load_all()
  source("tests/testthat/src/functionsForTests.R")
} else source("src/functionsForTests.R")



#### TESTS ####
test_that("haplotype normal initialisation", {
  #### Initialisation:
  mySpec <- create_spec()
  myLoci create_SNP(mySpec)
  # simulate haplotype data
  rawHaplo <- matrix(sample(c(0, 1), myLoci$nLoci() * mySpec$ploidy, replace = T),
                     nrow = mySpec$ploidy)
  colnames(rawHaplo) <- myLoci$genoMap$lociNames


  #### Tests:
  # create haplotype object
  expect_error({haplo <- haplotype$new(lociInfo = myLoci,
                                       haplo = rawHaplo)},
               NA)

  expect_is(haplo$lociInfo, "lociInfo")
  expect_is(haplo$values, "list")
  expect_equal(length(haplo$values), mySpec$nChr)
  expect_equal(names(haplo$values), mySpec$chrNames)
  expect_is(unlist(haplo$values), "integer")
  expect_equal(length(haplo$allelDose), myLoci$nLoci())
  expect_equal(haplo$allelDose, Rfast::colsums(rawHaplo)[names(haplo$allelDose)])
})

test_that("haplotype particular initialisation", {
  #### Initialisation:
  mySpec <- create_spec()
  myLoci create_SNP(mySpec)
  # simulate haplotype
  rawHaplo <- matrix(sample(c(0, 1), myLoci$nLoci() * mySpec$ploidy, replace = T),
                     nrow = mySpec$ploidy)
  colnames(rawHaplo) <- myLoci$genoMap$lociNames
  rawHaploInit <- rawHaplo

  #### Tests:
  # OK with some missing data in rawHaplo
  rawHaplo[sample(length(rawHaplo), 10)] <- NA
  expect_error(haplotype$new(lociInfo = myLoci,
                             haplo = rawHaplo),
               NA)
  rawHaplo <- rawHaploInit

  # ERROR if some markers not in lociInfo:
  colnames(rawHaplo)[sample(myLoci$nLoci(), 10)] <- paste("wrong Name", 1:10)
  expect_error(haplotype$new(lociInfo = myLoci,
                             haplo = rawHaplo),
               "colnames\\(haplo\\) must be the names of the markers")
  rawHaplo <- rawHaploInit

  # ERROR if markers not specified:
  colnames(rawHaplo) <- NULL
  expect_error(haplotype$new(lociInfo = myLoci,
                             haplo = rawHaplo),
               "haplo must be a named matrix")
  rawHaplo <- rawHaploInit

  # ERROR if markers in "haplo" are a subset of the markers in "SNPs":
  # TODO for improvment this can error could be remove inf the future in the
  # subset is included in "SNPs"
  rawHaplo <- rawHaplo[,sample(ncol(rawHaplo), ncol(rawHaplo) - 10)]
  expect_error(haplotype$new(lociInfo = myLoci,
                             haplo = rawHaplo),
               paste("ncol\\(haplo\\) must be equal to the number of",
                     "markers in lociInfo"))
  rawHaplo <- rawHaploInit

})

test_that("same haplotype for clones", {
  # clone generated with haplotype in a different order

  #### Initialisation:
  mySpec <- create_spec()
  myLoci create_SNP(mySpec)
  # simulate haplotype data
  rawHaplo1 <- matrix(sample(c(0, 1), myLoci$nLoci() * mySpec$ploidy, replace = T),
                     nrow = mySpec$ploidy)
  colnames(rawHaplo1) <- myLoci$genoMap$lociNames
  rawHaplo2 <- rawHaplo1[,sample(colnames(rawHaplo1))]

  #### Tests:
  # create haplotype object
  h1 <- haplotype$new(lociInfo = myLoci, haplo = rawHaplo1)
  h2 <- haplotype$new(lociInfo = myLoci, haplo = rawHaplo2)
  expect_equal(h1, h2)
  expect_equal(h1$allelDose, h2$allelDose)
  expect_equal(h1$values, h2$values)

})
