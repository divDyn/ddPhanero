﻿# Phanerozoic-level diversity analyses with the R package divDyn

Ádám T. Kocsis, John Alroy, Carl J. Reddin and Wolfgang Kiessling

version 1.0.1 (updated to be used with divDyn 0.8)

*2019-06-13, initial commit date: 2018-08-29*

## About

This repository contains all data and additional functions used in the Phanerozoic-scale examples and vignette for the paper describing the 'divDyn' R package. You can access the most recently updated report at

https://github.com/divDyn/ddPhanero/blob/master/doc/dd_phanero.pdf

in pdf, and at

https://github.com/divDyn/ddPhanero/raw/master/doc/dd_phanero_web.html

in html format (GitHub cannot render this properly, save the html to your disk and open it).

This is not the development repository of the divDyn package. You can find that at 

http://github.com/divDyn/r_package


## Structure

The repository is organized in the following way:

* ``doc``: The main document in version-specific folders
  - *dd_phanero.pdf*: The report .pdf file. The file is copied to ``doc/`` which can be referenced regardless of the version.
  - *dd_phanero.Rmd*: The source rmarkdown file.

* ``data``: All relevant data objects in binary .RData format. 
  * ``PaleoDB``: Downloaded data from the Paleobiology Database (http://paleobiodb.org) with their metadata.
  * ``Stratigraphy``: Additional data that can be used to assign occurrences to stratigraphic stages, in version-specific folders.

* ``export``: Tables and figures directly output by the scripts presented in ``scripts``. 
  - *first.pdf*, *onetrial.pdf*, *many.pdf*: Curves of richness.
  - *detrending.pdf*: Detrending of taxonomic rates and richness curves based on multiple methods.
  - *sumRates.pdf*: Taxonomic turnover rates with different methods.
  - *valBin.csv*, *valStage.csv*, *pBin.csv*, *pStage.csv*: Estimates and *p*-values from hypothesis tests with different methods.
  * ``combined``: Higher-level combinations and re-renders of the outputs. 
    - *subs.ai*, *rates.pdf*, *subs.png*: Figure 1. The Adobe Illustrator file uses *first.pdf*, *onetrial.pdf*, *many.pdf* with links.
    - *rates.ai*, *rates.pdf*, *rates.png*: Figure 2. *sumRates.pdf* with panel labels. The Adobe Illustrator file uses *sumRates.pdf* with a link.
    - *conditionalTable.xlsx*: Table 3. MS Excel table that uses *pBin.csv* and *pStage.csv* to conditionally format *valBin.csv* and *valStage.csv*.
* ``scripts`` contains all R scripts that are used in the analysis in version-specific folders.In these:
  - *yyyy-mm-dd_marineAnimals_ddPhanero.R*: Analysis script, more-or-less the same as the code presented in the same version .pdf file in the ``doc`` folder.
  - *phanDyn.R*: Functions used in the analyses.
  * ``strat``: Version-specific folders of stratigraphy-correction scripts.
    - *cambProcess.R*: for the Cambrian period.
    - *ordProcess.R*: for the Ordovician period.


# Change log

## [1.0.1] (divDyn update to 0.8) - 2019-06-13
### Updates
- updated source data file on 2019-05-31
- used package versions (0.8.x): some variables were renamed (e.g. "bin" to "ten", when referring to the 10 my time scale). And changing some argumentation (na.rm=T for subsampling). 
- rendered html vignette
- The file processing Ordovician stratigraphy was modified to remove the hard-coded 'bin' column, which was not used anyway.
- The quota for CR was dropped from 4800 to 4700 for the ten-million year time scale  and to 1000 from 1100 with the stage-level time scale, due to the lower number of occurrences passed. This decrease in the number of occurrences is visible after the taxonomic filtering part. 


## [0.4.1] (production, SI) - 2019-01-24 
### Fixed in doc/
- typos and links (previously redirected links), minor phrasing changes
- character formatting issues
- bad reference to Table 3


## [0.4] (revision) - 2019-01-07 
### Updated
- used package versions (0.7.x)
- source data
- mistypes in phanDyn.R file

### Added
- categorization of environmental variables
- additional filter for the empty quotes genus entries <" "> 


## [0.3] (first submission) - 2018-09-20 
### Repo initialized
- used package versions (0.6.x)
