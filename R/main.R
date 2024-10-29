# Functions for analysis of data generated from the Hewlett Packard 8452A spectrophotometer.

# Read .WAV file for chlorophyll concentrations in 80% aqueous acetone according to Porra et al. 1989 BBA
read_wav <- function(filename, # filepath to the .WAV file
                     units = c("nmol_ml")) {

  wav <- read.csv(filename, skip = 8, header = FALSE) %>%
    mutate(Filename = stringr::word(string = filename, start = -1, sep = "/")) %>%
    mutate(Wavelength = 190+2*(row_number()-1)) %>%
    mutate(Absorbance = V1,
           .after = "Wavelength",
           .keep = "unused") %>%
    filter(Wavelength %in% c(750,664,646)) %>% #%in% designates filter for values in these rows of wavelength.
    mutate(Wavelength = paste("A", Wavelength, sep = "")) %>%
    tidyr::pivot_wider(names_from = "Wavelength", values_from = "Absorbance")

  if ("nmol_ml" %in% units) {
    wav <- wav %>%
      mutate(Chl_a_nmol_ml = 13.7*(A664-A750)-2.85*(A664-A750),
             Chl_b_nmol_ml = 22.39*(A646-A750)-5.42*(A664-A750),
             Chl_ab_nmol_ml = 19.54*(A646-A750)+8.29*(A664-A750))
  }
  if ("ug_ml" %in% units) {
    wav <- wav %>%
      mutate(Chl_a_ug_ml = 12.25*(A664-A750)-2.55*(A646-A750),
             Chl_b_ug_ml = 20.31*(A646-A750)-4.91*(A664-A750),
             Chl_ab_ug_ml = 17.76*(A646-A750)+7.34*(A664-A750))
  }
  wav <- wav %>%
    select(!c(A646, A664, A750))
  return(wav)

}

read_activity <- function(filename,
                          start, # must specify the reaction start time
                          end) { # if end is missing, uses the points until the end

  csv <- read.csv(filename, skip = 1, header = FALSE) %>%
    rename(Absorbance = V2, # at 340 nm
           Time = V1) # in seconds

  if (missing(end)) {
    end <- csv %>%
      summarise(Time = max(Time)) %>%
      pull(Time)
  }

  csv <- csv %>%
    filter(Time %>% between(start, end))

  activity <- lm(Absorbance ~ Time, data = csv) %>%
    summary() %>%
    broom::tidy() %>%
    filter(term == "Time") %>%
    mutate(Filename = stringr::word(string = filename, start = -1, sep = "/"),
           Activity_AU_s = estimate/6.22,
           .before = "term",
           .keep = "none")

  return(activity)
}
