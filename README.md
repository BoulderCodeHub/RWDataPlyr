# RWDataPlyr

Contains R package to read and manipulate data from RiverWareTM ([riverware.org](http://www.riverware.org)) that are saved as rdf (RiverWare data format) files.  

[![Travis-CI Build Status](https://travis-ci.org/rabutler/RWDataPlyr.svg?branch=master)](https://travis-ci.org/rabutler/RWDataPlyr) [![codecov](https://codecov.io/gh/rabutler/RWDataPlyr/branch/master/graphs/badge.svg)](https://codecov.io/gh/rabutler/RWDataPlyr)

## Installation

Package can be installed from GitHub, and we suggest building the vignette. 

```
if(!require(devtools)){
	install.packages('devtools')
	library(devtools)
}
devtools::install_github('BoulderCodeHub/RWDataPlyr', build_vignettes = TRUE)
```

## Usage

Check out the vignette:

```
vignette("rwdataplyr", package = "RWDataPlyr")
```

## Log:
* 2016-11-01: version 0.4.1.1 available. The package is now actually called RWDataPlyr.
* 2016-10-20: version 0.4.1 available
* Previous versions were originally available as the `RWDataPlot` package
  * 2016-07-13: version 0.4 available
  * 2016-03-22: version 0.3 available
  * 2015-07-01: version 0.2 available
  * 2014-09-16: working to create an R Package from existing code.
  
## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Bureau of Reclamation, an agency of the United States Department of Interior. 

Although this code has been used by the Reclamation, no warranty, expressed or implied, is made by Reclamation or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by Reclamation in connection therewith.

This software is provided "AS IS."
