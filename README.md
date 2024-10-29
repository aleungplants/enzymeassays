# Install:
```
install.packages("devtools")
devtools::install_github("aleungplants/enzymeassays")
```

# Functions
`read_chl(filepath, units)` reads the .WAV file you give it and returns a single row dataframe. 
- `filepath` is the path (or a vector of paths) to the .WAV file(s) you want to read. `list.files()` and/or `here::here()` is an easy way to get a list of filepaths.
- `units` is the nmol/ml or ug/ml (from Porra et al. 1989). By default if you don't type anything here it'll be nmol/ml. 

`read_activity(filepath, start, end)` reads the .CSV file(s) you give it and returns a dataframe with a rate in absorbance units (AU) per second.
- `filepath` here is same as `read_chl`
- `start` and `end` are for specifying the timeframe you want to calculate the AU ~ Time slope from. A `start` is needed, but an `end` is optional. They can be single numbers or a vector of numbers, in which case they should be the same length as the vector of filepaths.
