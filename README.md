`read_chl(filepath, units)` reads the .WAV file you give it and returns a single row dataframe. 
- `filepath` is the path to the .WAV file you want to read. `here::here()` is an easy way to get this filepath.
- `units` is the nmol/ml or ug/ml (from Porra et al. 1989). By default if you don't type anything here it'll be nmol/ml. 
- `purrr::map_dfr` is a nice way to use a list of filepaths and bind them by rows, but I believe it has bene superseded in `purrr`. So `map()` with a row binding function might work well, or you can use a for loop.

`read_activity(filepath, start, end)` reads the .CSV file you give it and returns a dataframe with a rate in absorbance units (AU) per second.
- `start` and `end` are for specifying the timeframe you want to calculate the AU ~ Time slope from. A `start` is needed, but an `end` is optional.
- Similar to `read_chl`, you'll need to loop through your files, especially if you have start/end times for specific files.
