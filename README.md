![rdocumentation_package_banner](https://cloud.githubusercontent.com/assets/1741726/17591508/cb39a3d6-5fde-11e6-9b49-1434dd417c64.png)


[![Rdoc](http://staging.rdocumentation.org/badges/version/Rdocumentation)](http://rdocumentation.org/packages/Rdocumentation)

Enhance the search/help functionality in R with [RDocumentation.org](http://www.Rdocumentation.org), and discover what R packages are most popular.

# Installation

To install the package, use `devtools`:

```R
devtools::install_github("datacamp/Rdocumentation")
library(Rdocumentation)
```

The package overwrite the basic help functions from the utils package:

* `help()` : for help about specific topic or packages
* `help.search()` : for help about fuzzy topics or packages
* `?`: shortcut for the two help functions, one questionmark calls `help`, two calls `help.search`.

#Features:

* Search through all CRAN, Bioconductor, Github packages and their archives thanks to [RDocumentation](http://www.rdocumentation.org).

* Browse beautifully formatted and designed help pages.

* Check if you have the latest version of a package and install or update with a single click.

<img style="margin-left:5px" width="186" alt="screen shot 2016-08-11 at 16 23 58" src="https://cloud.githubusercontent.com/assets/1741726/17591907/795459ce-5fe0-11e6-9e97-f118bbecf0e0.png">
<img style="margin-left:20px" width="136" alt="screen shot 2016-08-11 at 16 25 59" src="https://cloud.githubusercontent.com/assets/1741726/17591884/5f130f60-5fe0-11e6-8f1d-c3e7a245b176.png">
<img style="margin-left:20px" width="141" alt="screen shot 2016-08-11 at 16 24 55" src="https://cloud.githubusercontent.com/assets/1741726/17591898/6dff5c22-5fe0-11e6-8d81-3a0081c5c850.png">


* Run examples with a single click, no more copy/pasting

<img style="margin-left:20px" width="557" alt="Run examples" src="https://cloud.githubusercontent.com/assets/1741726/17591750/baa828a2-5fdf-11e6-931c-24472ea4b236.png">


* Post reviews and help package authors to improve their documentation
