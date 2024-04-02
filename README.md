# Bits and pieces of code to use with quitte-style data frames

R package **quitte**, version **0.3129.0**

[![CRAN status](https://www.r-pkg.org/badges/version/quitte)](https://cran.r-project.org/package=quitte)  [![R build status](https://github.com/pik-piam/quitte/workflows/check/badge.svg)](https://github.com/pik-piam/quitte/actions) [![codecov](https://codecov.io/gh/pik-piam/quitte/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/quitte) [![r-universe](https://pik-piam.r-universe.dev/badges/quitte)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

A collection of functions for easily dealing with
    quitte-style data frames, doing multi-model comparisons and plots.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("quitte")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("quitte-data-analysis") # REMIND/IAM Data Analysis Using quitte
```

## Questions / Problems

In case of questions / problems please contact Michaja Pehl <michaja.pehl@pik-potsdam.de>.

## Citation

To cite package **quitte** in publications use:

Pehl M, Bauer N, Hilaire J, Levesque A, Luderer G, Schultes A, Dietrich J, Richters O (2024). _quitte: Bits and pieces of code to use with quitte-style data frames_. R package version 0.3129.0, <https://github.com/pik-piam/quitte>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {quitte: Bits and pieces of code to use with quitte-style data frames},
  author = {Michaja Pehl and Nico Bauer and Jérôme Hilaire and Antoine Levesque and Gunnar Luderer and Anselm Schultes and Jan Philipp Dietrich and Oliver Richters},
  year = {2024},
  note = {R package version 0.3129.0},
  url = {https://github.com/pik-piam/quitte},
}
```
