#' global.R
#'
#' @import dplyr
#' @import shiny
#' @import bslib
#' @import readr
#' @import ggplot2
#' @importFrom lubridate year
#'
#' @noRd


source("R/helpers/helper_methods.R")

openpl <- read_csv("data/openpowerlifting-2025-02-22-48890240.csv")

#Data preprocessing
openpl <- openpl %>% mutate(Equipment = recode(Equipment,
                                               "Wraps" = "Equipped",
                                               "Multi-ply" = "Equipped",
                                               "Single-ply" = "Equipped",
                                               "Straps" = "Equipped",
                                               "Unlimited" = "Equipped"),
                            year = year(Date),
                            Comp = paste(year,MeetName,sep ="_") )


#need a clause here based on whether they put tested or not.
openpl <- openpl %>% filter(Event == "SBD")


# #For testing only
# openpl <- openpl %>% filter(Name == "Ian Grant #1")


all_lifter_names <- c("No Lifter Selected",unique(openpl$Name))




