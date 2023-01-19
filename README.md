# myBreedSimulatR
R package providing classes and functions to simulate breeding schemes. 
R Breeding Simulator used in the paper "Simulation-based future-oriented strategy can optimize decision making in a breeding program".

This package is a variant of "breedSimulatR", which is available from the "ut-biomet/breedSimulateR" repository on the GitHub.
Our package is not perfect to be used for general breeding simulations, so we recommend you use "breedSimulatR" to simulate breeding programs for your project.

## Installation

You can install `myBreedSimulatR` from [GitHub](https://github.com/) with:

``` r
if (!require("devtools")) {
    install.packages("devtools")
}
devtools::install_github("KosukeHamazaki/myBreedSimulatR")
```

You can check the installation with these lines:

``` r
library(myBreedSimulatR)
help(package = myBreedSimulatR)
```
