# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# Test File -- test population class.


# set.seed(5754) # for reproductible RNG


##### Initialisation functions ####
if (interactive()) {
  devtools::load_all()
  source("tests/testthat/src/functionsForTests.R")
} else source("src/functionsForTests.R")



#### TESTS ####
test_that("population initialisation", {
  #### Initialisation
  mySpec <- create_spec()
  myLoci create_SNP(mySpec)
  nInds <- 5
  haploList <- lapply(seq(nInds), function(x){
    create_haplo(SNPs)
  })
  indList <- create_inds(haploList)


  #### Tests:
  expect_error({myPop <- population$new(name = "My Population 1",
                          inds = indList,
                          verbose = FALSE)},
               NA)
  expect_equal(myPop$nInd, nInds)
  expect_is(myPop$inds, "list")
  expect_equal(length(myPop$inds), myPop$nInd)
  expect_is(myPop$genoMat, "matrix")
  expect_equal(dim(myPop$genoMat), c(myPop$nInd, myLoci$nLoci()))

  # check individuals' id in the object pop (names(myPop$inds)) do not depend of
  # the id of the individuals in the input list (names(indList)) :
  names(indList) <- paste("toto", seq_along(indList))
  myPop <- population$new(name = "My Population 1",
                          inds = indList,
                          verbose = FALSE)
  expect_equal(names(myPop$inds),
               as.character(vapply(indList, function(x){x$name},
                                   "character")))

  # check initialisation without parameters
  expect_error({myPop <- population$new(verbose = FALSE)}, NA)
  expect_is(myPop$inds, "list")
  expect_equal(myPop$nInd, 0)
  expect_equal(length(myPop$inds), myPop$nInd)

})



test_that("population initialisation particular cases", {
  #### Initialisation
  mySpec <- create_spec()
  myLoci create_SNP(mySpec)
  nInds <- 5
  haploList <- lapply(seq(nInds), function(x){
    create_haplo(SNPs)
  })
  indList <- create_inds(haploList)

  # check initialisation with one individual
  expect_error({myPop <- population$new(name = "My Population 1",
                                        inds = indList[[1]],
                                        verbose = FALSE)},
               NA)


})



test_that("population errors", {
  #### Initialisation
  mySpec <- create_spec()
  myLoci create_SNP(mySpec)

  haplo1 <- create_haplo(SNPs)
  haplo2 <- create_haplo(SNPs)
  ind1 <-  create_inds(haplo1)
  ind2 <-  create_inds(haplo2)
  ind2$name <- "Ind 2"


  #### Tests:
  expect_error({myPop <- population$new(name = "My Population 1",
                                        inds = list(ind1, ind2),
                                        verbose = FALSE)},
               NA)

  ## Check individuals with same name
  ind2$name <- "Ind 1"
  expect_error({myPop <- population$new(name = "My Population 1",
                                        inds = list(ind1, ind2),
                                        verbose = FALSE)},
                paste("Individual with the same name already",
                      "exists in the population:"))



  ## Check individuals of differents species
  mySpec1 <- create_spec(name = "Spec 1")
  mySpec2 <- create_spec(name = "Spec 2")
  SNPs1 <- create_SNP(mySpec1)
  SNPs2 <- create_SNP(mySpec2)
  haplo1 <- create_haplo(SNPs1)
  haplo2 <- create_haplo(SNPs2)
  ind1 <-  create_inds(haplo1)
  ind2 <-  create_inds(haplo2)
  ind2$name <- "Ind 2"

  expect_error({myPop <- population$new(name = "My Population 1",
                                        inds = list(ind1, ind2),
                                        verbose = FALSE)},
               "different species")
})


test_that("population add individuals", {
  #### Initialisation
  mySpec <- create_spec()
  myLoci create_SNP(mySpec)
  nInds <- 6
  haploList <- lapply(seq(nInds), function(x){
    create_haplo(SNPs)
  })
  indList <-  create_inds(haploList)

  myPop <- population$new(name = "My Population 1",
                          inds = indList[1:3],
                          verbose = FALSE)

  #### Tests:
  expect_error(myPop$addInds(inds = indList[[4]]), NA)
  expect_error(myPop$addInds(inds = indList[5:6]), NA)


})

#### TESTS ####
test_that("population remove individuals", {
  #### Initialisation
  mySpec <- create_spec()
  myLoci create_SNP(mySpec)
  nInds <- 5
  haploList <- lapply(seq(nInds), function(x){
    create_haplo(SNPs)
  })
  indList <-  create_inds(haploList)

  myPop <- population$new(name = "My Population 1",
                          inds = indList,
                          verbose = FALSE)

  #### Tests:
  expect_error(myPop$remInds("Ind 3"), NA)
  expect_warning(myPop$remInds("Ind 3"),
                 "Some individuals to remove are not in the population:")
  expect_error(myPop$remInds(names(myPop$inds)), NA)
  expect_warning(myPop$remInds("Ind 1"),
                 "Some individuals to remove are not in the population:")

})


test_that("population creation", {
  #### Initialisation
  mySpec <- create_spec(nChr = 10, lChr = 10^6)

  if (interactive()) {
    genoMap <- read.csv(file = "tests/testthat/src/genoMap.csv", header = T)
  } else genoMap <- read.csv(file = "src/genoMap.csv", header = T)
  myLoci <- lociInfo$new(genoMap = genoMap, specie = mySpec)

  if (interactive()) {
    geno <- read.csv(file = "tests/testthat/src/genotype.csv",
                     header = T,
                     row.names = 1)
  } else {
    geno <- read.csv(file = "src/genotype.csv",
                     header = T,
                     row.names = 1)
  }
  expect_true(all(dim(geno) > 2))
  expect_equal(ncol(geno), myLoci$nLoci())


  # TEST:
  expect_error({myPop <- createPop(geno = geno,
                                   lociInfo = myLoci,
                                   indNames = NULL,
                                   popName = "My pop",
                                   verbose = FALSE)},
               NA)
  expect_equal(myPop$nInd, nrow(geno))
  expect_equal(names(myPop$inds), row.names(geno))
  expect_equal(myPop$genoMat, as.matrix(geno[, sort(colnames(geno))]))

  expect_error({myPop <- createPop(geno = geno,
                                   lociInfo = myLoci,
                                   indNames = "My Inds - ",
                                   popName = "My pop",
                                   verbose = FALSE)},
               NA)
  expect_true(all(grepl("My Inds - ", names(myPop$inds))))
  expect_equal(row.names(myPop$genoMat), names(myPop$inds))

})


test_that("population $genoMat", {
  #### Initialisation
  mySpec <- create_spec()
  myLoci create_SNP(mySpec)
  nInds <- 5
  haploList <- lapply(seq(nInds), function(x){
    create_haplo(SNPs)
  })

  indList <-  create_inds(haploList)

  myPop <- population$new(name = "My Population 1",
                          inds = indList,
                          verbose = FALSE)

  expect_equal(rownames(myPop$genoMat), names(myPop$inds))

})




test_that("population allele freq", {
  #### Initialisation
  mySpec <- create_spec()
  myLoci create_SNP(mySpec)
  nInds <- 20
  haploList <- lapply(seq(nInds), function(x){
    create_haplo(SNPs)
  })

  indList <-  create_inds(haploList)

  myPop <- population$new(name = "My Population 1",
                          inds = indList,
                          verbose = FALSE)

  expect_error(myPop$af, NA)
  expect_error(myPop$maf, NA)

  expect_is(myPop$af, "numeric")
  expect_is(myPop$maf, "numeric")

  expect_true(all(myPop$af >= 0))
  expect_true(all(myPop$maf >= 0))

  expect_true(all(myPop$af <= 1))
  expect_true(all(myPop$maf <= 0.5))

  expect_equal(myPop$af[myPop$af <= 0.5], myPop$maf[myPop$af <= 0.5])
  expect_equal(myPop$af[myPop$af >= 0.5], 1 - myPop$maf[myPop$af >= 0.5])

  expect_equal(sort(names(myPop$af)),
               sort(myLoci$genoMap$lociNames))
  expect_equal(sort(names(myPop$maf)),
               sort(myLoci$genoMap$lociNames))

  expect_equal(names(myPop$af), colnames(myPop$genoMat))
  expect_equal(names(myPop$maf), colnames(myPop$genoMat))
})
