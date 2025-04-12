# R/data_load.R

#' Load and preprocess data
#'
#' @return A list of preprocessed datasets
#' @export
init_data <- function() {


  list(
    #use qs for efficiency
    openpl = qread("data/openpl.qs"),
    minima = readRDS("data/minima.rds"),
    all_lifter_names = readRDS("data/all_lifter_names.rds")
  )
}
