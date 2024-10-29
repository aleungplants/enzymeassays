#' Get chlorophyll concentrations
#'
#' Read .WAV file for chlorophyll concentrations in 80% aqueous acetone according to Porra et al. 1989 BBA
#'
#' @param filename Filepath to a file
#' @param units nmol_ml or ug_ml
#' @return A dataframe containing chlorophyll concentrations in specified units
#' @export
read_chl <- function(filename,
                     units = c("nmol_ml")) {

  wav <- read.csv(filename, skip = 8, header = FALSE) %>%
    dplyr::mutate(Filename = stringr::word(string = filename, start = -1, sep = "/")) %>%
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
