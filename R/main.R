#' Get chlorophyll concentrations
#'
#' Read .WAV file for chlorophyll concentrations in 80% aqueous acetone according to Porra et al. 1989 BBA
#'
#' @param filepath String of filepath to .WAV file
#' @param units nmol_ml or ug_ml or both in a vector
#' @return A dataframe containing chlorophyll concentrations in specified units
read_wav <- function(filepaths,
                     units = c("nmol_ml")) {

  wav <- read.csv(filepaths, skip = 8, header = FALSE) %>%
    dplyr::mutate(Filename = stringr::word(string = filepaths, start = -1, sep = "/")) %>%
    dplyr::mutate(Wavelength = 190+2*(dplyr::row_number()-1)) %>%
    dplyr::mutate(Absorbance = V1,
           .after = "Wavelength",
           .keep = "unused") %>%
    dplyr::filter(Wavelength %in% c(750,664,646)) %>% #%in% designates filter for values in these rows of wavelength.
    dplyr::mutate(Wavelength = paste("A", Wavelength, sep = "")) %>%
    tidyr::pivot_wider(names_from = "Wavelength", values_from = "Absorbance")

  if ("nmol_ml" %in% units) {
    wav <- wav %>%
      dplyr::mutate(Chl_a_nmol_ml = 13.7*(A664-A750)-2.85*(A664-A750),
             Chl_b_nmol_ml = 22.39*(A646-A750)-5.42*(A664-A750),
             Chl_ab_nmol_ml = 19.54*(A646-A750)+8.29*(A664-A750))
  }
  if ("ug_ml" %in% units) {
    wav <- wav %>%
      dplyr::mutate(Chl_a_ug_ml = 12.25*(A664-A750)-2.55*(A646-A750),
             Chl_b_ug_ml = 20.31*(A646-A750)-4.91*(A664-A750),
             Chl_ab_ug_ml = 17.76*(A646-A750)+7.34*(A664-A750))
  }
  wav <- wav %>%
    dplyr::select(!c(A646, A664, A750))
  return(wav)

}
#' Get chlorophyll concentrations (one or more files)
#' Read one or more .WAV files for chlorophyll concentrations in 80% aqueous acetone according to Porra et al. 1989 BBA
#'
#' @param filepaths String or vector of filepath(s) to .WAV file
#' @param units nmol_ml or ug_ml or both in a vector
#' @return A dataframe containing chlorophyll concentrations in specified units
#' @export
read_chl <- function(filepaths, units) {
  purrr::map(filepaths, ~read_wav(., units)) %>%
        dplyr::bind_rows()
}

#' Get enzyme activities
#'
#' Read .CSV file and calculate the slope in Absorbance Units, AU, per second. A start time must be provided.
#'
#' @param filepaths String or vector containing filepath(s) to .CSV files
#' @return A dataframe enzyme activity in AU/s
#' @export
read_activity <- function(filepaths,
                          start, # must specify the reaction start time
                          end) { # if end is missing, uses the points until the end

  csv <- read.csv(filepaths, skip = 1, header = FALSE) %>%
    dplyr::rename(Absorbance = V2, # at 340 nm
                  Time = V1) # in seconds

  if (missing(end)) {
    end <- csv %>%
      dplyr::summarise(Time = max(Time)) %>%
      dplyr::pull(Time)
  }

  csv <- csv %>%
    dplyr::filter(Time %>% between(start, end))

  activity <- lm(Absorbance ~ Time, data = csv) %>%
    summary() %>%
    broom::tidy() %>%
    dplyr::filter(term == "Time") %>%
    dplyr::mutate(Filename = stringr::word(string = filepaths, start = -1, sep = "/"),
                  Activity_AU_s = estimate/6.22,
                  .before = "term",
                  .keep = "none")

  return(activity)
}
