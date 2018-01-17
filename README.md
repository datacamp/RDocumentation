![rdocumentation_package_banner](https://cloud.githubusercontent.com/assets/1741726/18202790/b757fa44-7112-11e6-99e0-f20e8f3f93ff.png)

[![Travis-CI Build Status](https://travis-ci.org/datacamp/RDocumentation.svg?branch=master)](https://travis-ci.org/datacamp/RDocumentation)
[![codecov](https://codecov.io/gh/datacamp/RDocumentation/branch/master/graph/badge.svg)](https://codecov.io/gh/datacamp/RDocumentation)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RDocumentation)](https://cran.r-project.org/package=RDocumentation)
[![Rdoc](http://staging.rdocumentation.org/badges/version/RDocumentation)](http://rdocumentation.org/packages/RDocumentation)

Enhance the search/help functionality in R with [RDocumentation.org](http://www.rdocumentation.org), and discover what R packages are most popular.

# Installation

To install the latest stable version from CRAN:

```R
install.packages("RDocumentation")
```

You can also use `devtools` to install the latest development version:

```R
devtools::install_github("datacamp/RDocumentation")
library(RDocumentation)
```

If the package is loaded, it overrides the basic help functions from the utils package:

* `help()` : for help about specific topic or packages
* `help.search()` : for help about fuzzy topics or packages
* `?`: shortcut for the two help functions, one question mark calls `help`, two calls `help.search`.

# Features:

* Search through all CRAN, Bioconductor, Github packages and their archives thanks to [RDocumentation](http://www.rdocumentation.org).

* Browse beautifully formatted and designed help pages.

* Post reviews and help package authors to improve their documentation

