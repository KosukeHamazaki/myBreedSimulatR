# Author: Julien Diot juliendiot@ut-biomet.org
# 2020 The University of Tokyo
#
# Description:
# R script to generate package's data

library(dplyr)
library(breedSimulatR)

# load genotype data:
example_genotypes <- read.table("tools/data/genotypes.txt.gz", header = T, sep = "\t")

# load snp coordinates:
example_genoMap <- read.table("tools/data/snp_coords.txt.gz", header = T, sep = "\t")

# load snp effects:
lociEffects <- read.table("tools/data/snpEffects.txt", header = T, sep = "\t")
example_lociEffects <- lociEffects$x
names(example_lociEffects) <- rownames(lociEffects)

# check homogeneity between geno and snp cood
if (any(!colnames(example_genotypes) %in% example_genoMap$SNPid)) {
  stop('Some markers of "example_genotypes" are not in "example_genoMap".')
}

if (any(!example_genoMap$SNPid %in% colnames(example_genotypes))) {
  warning('Some markers of "example_genoMap" are not in "example_genotypes".')
}


# # create specie
# example_specie <- specie$new(specName = "Statisticae exempli",
#                              nChr = 10,
#                              lChr = 1e6,
#                              ploidy = 2,
#                              recombRate = 3/1e6)
#
# # create lociInfo
# example_myLoci <- lociInfo$new(genoMap = example_genoMap,
#                             specie = example_specie)
#
# # create pop
# example_pop <- createPop(geno = example_genotypes,
#                          lociInfo = example_lociEffects,
#                          popName = "Example population")


# checks
# stopifnot(all.equal(as.matrix(example_genotypes[,sort(colnames(example_genotypes))]), example_pop$genoMat))
# stopifnot(all.equal(names(example_lociEffects), colnames(example_pop$genoMat)))



exampleData <- list(
  genoMap = example_genoMap,
  genotypes = example_genotypes,
  lociEffects = example_lociEffects
)


file.remove(list.files("data/", full.names = TRUE))
usethis::use_data(exampleData, overwrite = TRUE)

names(exampleData)[1] <- "genoMap"
colnames(exampleData$genoMap)[3] <- "lociNames"
dimnames(exampleData$genotypes) <- list(indNames = rownames(exampleData$genotypes),
                                        lociNames = colnames(exampleData$genotypes))
exampleData$genotypes <- cbind(Intercept = rep(1, nrow(exampleData$genotypes)),
                               exampleData$genotypes)
exampleData$lociEffects <- c(Intercept = 0, exampleData$lociEffects)
exampleData$genotypicValues <- as.matrix(exampleData$genotypes) %*%
  as.matrix(exampleData$lociEffects[colnames(exampleData$genotypes)])
myExampleData <- exampleData
usethis::use_data(myExampleData, overwrite = TRUE)
