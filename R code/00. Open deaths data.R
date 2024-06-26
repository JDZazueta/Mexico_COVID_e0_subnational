################################################################################
# Article: Mexico’s surge of violence and COVID-19 drive life expectancy losses 2015–2021
# Title:   Open death data
# Authors: Daniel, Paola, Maria & José Manuel
# Data:    Proyecciones de poblacion CONAPO (2023), CONEVAL (2016-2018), and
#          Deaths from INEGI
################################################################################


# ---------------------------------------------------------------------------- #
#  0. Working directory and Packages
# ---------------------------------------------------------------------------- #

# To clear everything in R, before start the analysis and open functions
rm(list = ls())

pacman::p_load(here)

source(here::here("R Code/00. Functions for analysis.R"))

# ---------------------------------------------------------------------------- #
#     0. Function to open all datasets
# ---------------------------------------------------------------------------- #


# Function to open files
read_Data <- function(file) {

  data <- read.dbf(file)
  ifelse(data$ANIO_REGIS<98,

         data_set <- data %>%
           dplyr::select(ANIO_OCUR, ANIO_REGIS, SEXO, EDAD,
                         ANIO_NACIM,ENT_RESID,ENT_REGIS,
                         CAUSA_DEF, LISTA_BAS) %>%
           rename(YEAR = ANIO_REGIS,
                  LISTA = LISTA_BAS),
         data_set <- data %>%
           dplyr::select(ANIO_OCUR, ANIO_REGIS, SEXO, EDAD,
                         ANIO_NACIM,ENT_RESID,ENT_REGIS,
                         CAUSA_DEF, LISTA_MEX) %>%
           rename(YEAR = ANIO_REGIS,
                  LISTA = LISTA_MEX)
  )
  return(data_set)

}

# ---------------------------------------------------------------------------- #
#     1. Open all databases
# ---------------------------------------------------------------------------- #

# Create a list with files names
file.names <- list.files(here::here("Data/INEGI"))
# Open files by applying the function
Deaths_1990_2021 <- do.call(rbind, lapply(file.names, read_Data))

# We save the data
save(Deaths_1990_2021, file = here::here("Data/tmp/Raw deaths 1990-2021.RData"))

