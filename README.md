Rdocumentation
==============

Enhance the search/help functionality in R with [Rdocumentation.org](http://www.Rdocumentation.org), and discover what R packages are most popular.

# Installation

To install the package, use `devtools`:

```R
devtools::install_github("datacamp/Rdocumentation")
library(Rdocumentation)
```
#key functions:

The key functions overwrite the basic help functions from the utils package:

* `help()` : for help about specific topic or packages
* `help.search()` : for help about fuzzy topics or packages
* `?`: shortcut for the two help functions, one questionmark calls `help`, two calls `help.search`.

#features

*the online database search is used, meaning all packages and topics on CRAN,BioConductor and Github or searched
*when going to a package page, the current version you have installed of the package is automatically checked, the page will provide an 'Install' button to easily install the package, or 'Update' button if your version is not the latest
*when going to a topic page, the `examples` section provides a `run examples` button, which will run the examples in Rstudio
*when logging in to Rdocumentation in the help pane, you credentials are automatically saved locally, and the package will automatically log you in on loading. 
