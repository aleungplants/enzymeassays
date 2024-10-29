#' Get enzyme activity
#'
#' Read .CSV file and calculate the slope in Absorbance Units, AU, per second. A start time must be provided.
#'
#' @param filename Filepath to a file
#' @return A dataframe enzyme activity in AU/s
#' @export
read_activity <- function(filename,
                          start, # must specify the reaction start time
                          end) { # if end is missing, uses the points until the end

  csv <- read.csv(filename, skip = 1, header = FALSE) %>%
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
    dplyr::mutate(Filename = stringr::word(string = filename, start = -1, sep = "/"),
                  Activity_AU_s = estimate/6.22,
                  .before = "term",
                  .keep = "none")

  return(activity)
}
