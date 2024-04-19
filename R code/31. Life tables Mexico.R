################################################################################
# Article: Mexico’s surge of violence and COVID-19 drive life expectancy losses 2015–2021
# Title:   Life tables
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
#  1. Open Data
# ---------------------------------------------------------------------------- #

# -------------------------------- #
#  Data
# -------------------------------- #

# State
Data_state <- get(load(here::here("Data/Final/Data_98_21_CoD_state_CONAPO_2023.RData")))

# National
Data_National <- get(load(here::here("Data/Final/Data_98_21_CoD_National_CONAPO_2023.RData")))

# ---------------------------------------------------------------------------- #
#     2. Compute life tables
# ---------------------------------------------------------------------------- #

# - Combined National and State data
Data_analysis <- Data_National %>%
  rename(ENTIDAD_NAME = ENTIDAD,
         ENTIDAD = CVE_GEO) %>%
  rbind(Data_state) %>%
  dplyr::select(ENTIDAD, ENTIDAD_NAME, YEAR,
                SEX, Age_group, mx) %>%
  data.table()


# -- Compute Life tables (we use the mx from CONAPO)
Life_table_Mex <- Data_analysis[,cbind(SLT(nmx = mx,
                                           age = Age_group,
                                           Sex = SEX,
                                           nax = NULL,
                                           n= c(1,4,rep(5,19)))),
                                by=list(YEAR,SEX,  ENTIDAD, ENTIDAD_NAME)]

# --- Life expectancy at birth and at age 65
Mexico_e0_e65 <- Life_table_Mex %>%
  filter(Age==0 | Age==65) %>%
  dplyr::select( ENTIDAD, ENTIDAD_NAME, YEAR, Sex, Age, ex)


save(Mexico_e0_e65, file = here::here("Data/Final/Mexico_e0_e65.RData"))
