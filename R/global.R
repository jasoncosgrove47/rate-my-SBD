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

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(RSQLite)
library(DBI)
library(shinybusy)

library(bslib)
library(readr)

library(lubridate)
library(golem)
library(tidyverse)
library(glue)

library(stringr)
library(tidyr)
library(qs)

minima = readRDS("data/minima.rds")

minima_category <- c("subjr", "jr"   , "open" , "M1" ,"M2", "M3", "M4")

minima_weightclass <- c( "53","59","66","74","83","93","105","120","120 +",
                         "43","47","52","57","63","69","76","84","84 +" )

minima_sex <- c("M","F")

all_lifter_names = readRDS("data/all_lifter_names.rds")

source("R/helpers/helper_methods.R")



