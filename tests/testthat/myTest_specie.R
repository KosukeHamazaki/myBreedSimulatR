# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 / 2020 The University of Tokyo
#
# Description:
# Test file for the Specie class

if (interactive()) {
  devtools::load_all()
}


test_that("specie initialization", {

  expect_error({mySpec <- specie$new(nChr = 3,
                                     lChr = c(100, 150, 200),
                                     specName = "Geneticae Exempulus",
                                     ploidy = 2,
                                     mutRate = 10^-8,
                                     recombRate = 10^-7,
                                     chrNames = c("C1", "C2", "C3"),
                                     nLoci = c(300, 400, 250),
                                     effPopSize = 100,
                                     simulateCrop = TRUE,
                                     recombRateOneVal = TRUE,
                                     verbose = F)},
               NA)


  expect_identical(mySpec$specName, "Geneticae Exempulus")
  expect_identical(mySpec$nChr, 3)
  expect_identical(mySpec$ploidy, 2)
  expect_identical(as.numeric(mySpec$lChr), c(100, 150, 200))
  expect_identical(names(mySpec$lChr), c("C1", "C2", "C3"))
  expect_identical(mySpec$mutRate, 10^-8)
  expect_identical(mySpec$chrNames, c("C1", "C2", "C3"))
  expect_identical(mySpec$recombRate, 10^-7)
  expect_identical(mySpec$nLoci, c(300, 400, 250))
  expect_identical(mySpec$effPopSize, 100)
  expect_identical(mySpec$simulateCrop, TRUE)
  expect_identical(mySpec$recombRateOneVal, TRUE)
  expect_output(specie$new(1, 10, verbose = T), paste("A new species has",
                                                      "emerged: Undefinded !"))
  expect_output(specie$new(1, 10, verbose = F), NA)

  expect_error(specie$new(nChr = 3.5,
                          lChr = c(100, 150, 200),
                          verbose = F),
               "nChr must be integer.")
  expect_error(specie$new(nChr = 3,
                          lChr = c(100, 150.2, 200),
                          verbose = F),
               "lChr must be integers.")
  expect_error(specie$new(nChr = 3,
                          lChr = c(100, 150, 200),
                          simulateCrop = TRUE,
                          verbose = F),
               "When you simulate marker genotype, you must specify both `nLoci` & `effPopSize`!")
  expect_error(specie$new(nChr = 3,
                          lChr = c(100, 150, 200),
                          nLoci = c(300, 400.2, 250),
                          effPopSize = 100,
                          simulateCrop = TRUE,
                          verbose = F),
               "nLoci must be integers.")
  expect_error(specie$new(nChr = 3,
                          lChr = c(100, 150, 200),
                          nLoci = c(300, 400, 250),
                          effPopSize = 100.7,
                          simulateCrop = TRUE,
                          verbose = F),
               "effPopSize must be integer.")
})


test_that("specie initialization without optional values", {

  expect_error(specie$new(nChr = 3,
                          lChr = c(100, 150, 200),
                          verbose = F),
               NA)

  mySpec <- specie$new(nChr = 3,
                       lChr = c(100, 150, 200),
                       verbose = F)

  expect_identical(mySpec$specName, "Undefinded")
  expect_identical(mySpec$nChr, 3)
  expect_identical(mySpec$ploidy, 2)
  expect_identical(as.numeric(mySpec$lChr), c(100, 150, 200))
  expect_identical(names(mySpec$lChr), c("Chr1", "Chr2", "Chr3"))
  expect_identical(mySpec$mutRate, NA)
  expect_identical(mySpec$chrNames, c("Chr1", "Chr2", "Chr3"))
  expect_identical(mySpec$recombRate, NA)
  expect_identical(mySpec$nLoci,  NA)
  expect_identical(mySpec$effPopSize, NA)
  expect_identical(mySpec$simulateCrop, FALSE)
  expect_identical(mySpec$recombRateOneVal, TRUE)
})


test_that("specie's \"getChrLength\" methods", {
  mySpec <- specie$new(nChr = 3,
                       lChr = c(100, 150, 200),
                       verbose = F)

  expect_is(mySpec$getChrLength(), "numeric")
  expect_named(mySpec$getChrLength())

  expect_identical(as.numeric(mySpec$getChrLength()), c(100, 150, 200))
  expect_identical(as.numeric(mySpec$getChrLength(1)), 100)
  expect_identical(as.numeric(mySpec$getChrLength("Chr2")), 150)

  expect_identical(names(mySpec$getChrLength()), c("Chr1", "Chr2", "Chr3"))
  expect_identical(names(mySpec$getChrLength(3)), "Chr3")
  expect_identical(names(mySpec$getChrLength("Chr1")), "Chr1")
})


test_that("specie's \"print\" methods", {
  mySpec <- specie$new(nChr = 3,
                       lChr = c(100, 150, 200),
                       verbose = F)

  expect_output(print(mySpec), paste0("Name: Undefinded\\n",
                                      "Number of Chromosomes: 3\\n",
                                      "Ploidy: 2\\n",
                                      "Mutation rate : NA\\n",
                                      "Recombination Rate: NA\\n",
                                      "Effective population size: NA\\n",
                                      "Simulate marker genotyope: FALSE\\n",
                                      "Assume same recombination rate across genome: TRUE\\n",
                                      "Chromosome length & Number of loci (including markers & QTLs):\\n",
                                      "    chrNames chrLength nLoci \\nChr1",
                                      "    Chr1       100   NA\\nChr2",
                                      "    Chr2       150   NA\\nChr3",
                                      "    Chr3       200   NA"
  ))
})
