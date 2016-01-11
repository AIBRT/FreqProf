# FreqProf
R package for Frequency Profiles

### Brief Summary
This package aims at computing and plotting frequency profiles - a method for visualizing the behavior of individuals
in real time or post-hoc. It serves two functions, 1) to convert binary behavioral observation data into a frequency
profile data format, and 2) to plot this data according to three paramaters. The three parameters are 1) window size,
2) step size, and 3) resolution. At one extreme, the parameters produce a cumulative record plot, at the other, they
produce a barcode plot. This package includes a user-friendly interface - using the "Shiny App" framework - allowing
people with no prior R programming experience to easily input their data, utilize the visualization tools, explore the 
effects of manipulating the parameters, and download publication quality plots. The FreqProf package also includes 
functions that allow the importation of data encoded in .bin, .fpw, and .csv formats.

### Getting Started
Install the current version of the package by using `devtools::install_github("AIBRT/FreqProf")`, and then start the 
Shiny App example with `runEx()` after you've installed the package.

Copyright American Institute for Behavioral Research and Technology (http://aibrt.org/).