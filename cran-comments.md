## Test environments

* local OS X install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.3.1, R 3.2.5, R-devel.
* win-builder (release, devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

Additional comments:
- This is a rather important update that fixes problems regarding user experience. 
In the previous version this package override the native "help, ? and help.search" functions and users would be unable to undo that without deinstalling the package.

## Downstream dependencies

There are no downstream dependencies for this package.