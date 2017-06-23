There is unlikely anything of much use to you here now as some of the major functions are undergoing a big re-write. 

For installation: 
```{r}
install.packages(devtools)
devtools::install_github("zanidean/sRa")
```
**xmR**: Generates XMR data. 

**xmR_Chart**: Takes the output from xmR and makes an XMR chart, mostly used for diagnostics.

**fuzzy_clean**: Uses approximate string matching to fix typos and slight differences in vectors.

**SDI**: Simpson Diversity Index Calculator

**foipR**: For making sure your data is foip'd.

**columnCollapse**: Uniting two columns of similar data.

**round2**: R used IEEE floating point to round, which can make some differences in analysis. This functions as you expect a rounding function to. 

**stringFilter/stringSubset**: Filters/subsets a column based on a string.
