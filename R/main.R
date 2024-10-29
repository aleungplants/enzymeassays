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

#' Read HP8452 .CSV file
#'
#' Read .CSV file and add a filename column.
#'
#' @param filepath String containing filepath(s) to .CSV files
#' @param start Start of reaction. By default the function will use the lowest value of Time if missing.
#' @param end Start of reaction. By default the function will use the highest value of Time if missing.
#' @param dropfolder Optional logical for whether to add the filepath to the dataframe
#' @param dropfolder Optional logical for whether to keep the whole filepath or drop the folders leading up to the file.
#' @return A dataframe with time points of absorbance measurements
read_csv8452 <- function(filepath, start, end, dropfolder = TRUE, droppath = FALSE) {

  csv <- read.csv(filepath, skip = 1, header = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Filename = filepath,
                  .before = "V1") %>%
    dplyr::rename(Absorbance = V2, # at 340 nm
                  Time = V1)

  if (missing(start)) {
    start <- csv %>% dplyr::pull(Time) %>% min()
  }

  if (missing(end) | is.na(end)) {
    end <- csv %>% dplyr::pull(Time) %>% max()
  }

  csv <- csv %>%
    dplyr::filter(Time %>% dplyr::between(start, end))

  if (dropfolder == TRUE){
    csv <- csv %>%
      dplyr::mutate(Filename = stringr::word(string = Filename, start = -1, sep = "/"))
  }

  if (droppath == TRUE){
    csv <- csv %>% dplyr::select(!Filename)
  }

  return(csv)

}

#' Get enzyme activities
#'
#' Read .CSV file and calculate the slope in Absorbance Units, AU, per second. A start time must be provided.
#'
#' @param filepaths String or vector containing filepath(s) to .CSV files
#' @param dropfolder Optional logical for whether to keep the whole filepath or drop the folders leading up to the file.
#' @return A dataframe enzyme activity in AU/s
#' @export

filepaths <- list.files("/Users/art/Library/CloudStorage/OneDrive-UniversityofToronto/CO2\ study/CO2\ study\ analysis/data/enzyme", pattern = "*.CSV", full.names = TRUE)[1:3]

# filepath <- filepaths[1]

read_activity <- function(filepaths,
                          start, # must specify the reaction start time
                          end = NA,
                          dropfolder = TRUE) { # optional

  if (missing(start)) {stop("You must specify reaction start times.")}

  filepath_df <- tibble::tibble(Filepath = filepaths,
                                Start = start,
                                End = end)

  csv <- filepath_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Data = list(read_csv8452(Filepath, Start, End, droppath = TRUE)))

  activity <- csv %>%
    dplyr::group_by(Filepath) %>%
    dplyr::mutate(Fits = purrr::map(Data, ~ lm(Absorbance ~ Time, data = .))) %>%
    dplyr::mutate(TidyFits = purrr::map(Fits, broom::tidy)) %>%
    tidyr::unnest(TidyFits) %>%
    dplyr::filter(term == "Time") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Activity_AU_s = estimate/6.22) %>%
    dplyr::select(Filepath, Activity_AU_s)

  if (dropfolder == TRUE) {
    activity <- activity %>%
      dplyr::mutate(Filepath = stringr::word(string = Filepath, start = -1, sep = "/"))
  }

  return(activity)
}
