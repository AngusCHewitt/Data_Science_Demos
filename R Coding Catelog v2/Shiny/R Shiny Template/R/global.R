# Load dependences
library(igraph)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(pins)
library(forcats)
library(tidygraph)

source("modules/about.R")

# Define extra functions (if there are lots, move into functions.R)
'%ni%' <- Negate('%in%')

navbarPageWithLogo <- function(..., logo) {
  navbar <- navbarPage(...)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], logo
  )
  navbar
}

