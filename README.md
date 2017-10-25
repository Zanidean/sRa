Overview
---------

sRa is a package designed to speed-up institutional research by providing a set of generalized functions for common tasks:

- `xmR()`: [Generates XMR data.](https://sramhc.shinyapps.io/xmrbuilder/)

- `xmR_chart()`: Takes the output from xmR and makes an XMR chart, mostly used for diagnostics.

- `fuzzyClean()`: Uses approximate string matching to fix typos and slight differences in vectors.

- `sdi()`: Simpson Diversity Index calculator.

- `foip()`: For making sure your data is foip'd.

Installation
------------
``` R
install.packages(devtools)
devtools::install_github("zanidean/sRa")
```
