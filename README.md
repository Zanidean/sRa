Overview
---------

sRa is a package designed to speed-up institutional research by providing set of generalized functions for common tasks:


- ~~`xmR()`: Generates XMR data.~~ Not ready just yet.

- `xmR_chart()`: Takes the output from xmR and makes an XMR chart, mostly used for diagnostics.

- `fuzzyClean()`: Uses approximate string matching to fix typos and slight differences in vectors.

- `sdi()`: Simpson Diversity Index calculator.

- `foip()`: For making sure your data is foip'd.

- `columnCollapse()`: Uniting two columns of similar data.

- `round2()`: This functions as you expect a rounding function to. R uses IEEE to round, which can make some differences in analysis.

- `stringFilter() / stringSubset()`: Filters/Subsets a column based on a string.


Installation
------------
``` R
install.packages(devtools)
devtools::install_github("zanidean/sRa")
```
