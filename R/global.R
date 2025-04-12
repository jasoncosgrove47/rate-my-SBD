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

library(dplyr)
library(shiny)
library(bslib)
library(readr)
library(ggplot2)
library(lubridate)
library(golem)
library(tidyverse)
library(dplyr)
library(glue)
library(plotly)
library(stringr)
library(tidyr)
library(shiny)
library(bslib)


source("R/helpers/helper_methods.R")



## initialise the app

### read in the datasets
openpl <- read_csv("data/openpowerlifting-2025-02-22-48890240.csv")

minima <- read_csv("data/minima.csv") %>% rename(WeightClass = `Catég.`)


#Data preprocessing
openpl <- openpl %>% mutate(Equipment = recode(Equipment,
                                               "Wraps" = "Equipped",
                                               "Multi-ply" = "Equipped",
                                               "Single-ply" = "Equipped",
                                               "Straps" = "Equipped",
                                               "Unlimited" = "Equipped"),
                            year = year(Date),
                            Comp = paste(year,MeetName,sep ="_") )


#focus on full meets only
openpl <- openpl %>% filter(Event == "SBD")


# set this global variable for drop down menus.
all_lifter_names <- c("No Lifter Selected",unique(openpl$Name))

# #For testing only
# lifter <- openpl %>% filter(Name == "Ian Grant #1")



