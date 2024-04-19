################################################################################
# Article: Mexico’s surge of violence and COVID-19 drive life expectancy losses 2015–2021
# Title:   Prepare CONAPO'S data
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
#     1. Open databases
# ---------------------------------------------------------------------------- #

#################################
#  Exposure data
#################################

PSY <- read.csv(here::here("Data/CONAPO/2023/00_Pob_Mitad_1950_2070.csv"), header = T, sep = ",", stringsAsFactors=FALSE)

colnames(PSY)[2] <- "Year"
colnames(PSY)[5] <- "Age"

#table(PSY$CVE_GEO)

CONAPO_exp_98_2021_aggregated <- PSY %>%
  filter(Year>=1998 & Year<=2021) %>%
  filter(ENTIDAD!="Rep\x9cblica Mexicana") %>%
  mutate(Age_group = case_when(Age == 0 ~ 0,
                               between(Age, 1, 4) ~ 1,
                               between(Age, 5, 94) ~ (age %/% 5) * 5,
                               between(Age, 95, 120) ~ 95),
         SEX = case_when(SEXO=="Hombres" ~ "MALES",
                         SEXO=="Mujeres" ~ "FEMALES")) %>%
  rename(YEAR = Year) %>%
  group_by(YEAR,Age_group, SEX, ENTIDAD, CVE_GEO) %>%
  summarize(PSY=sum(POBLACION))


#################################
#  Deaths data
#################################

Dx <- read.csv(here::here("Data/CONAPO/2023/01_Defunciones_1950_2070.csv"), header = T, sep = ",", stringsAsFactors=FALSE)

colnames(Dx)[2] <- "Year"
colnames(Dx)[6] <- "Age"

table(Dx$ENTIDAD)

CONAPO_dx_98_2021_aggregated <- Dx %>%
  filter(Year>=1998 & Year<=2021) %>%
  filter(ENTIDAD!="Rep\x9cblica Mexicana") %>%
  mutate(Age_group = case_when(Age == 0 ~ 0,
                               between(Age, 1, 4) ~ 1,
                               between(Age, 5, 94) ~ (age %/% 5) * 5,
                               between(Age, 95, 120) ~ 95),
         SEX = case_when(SEXO=="Hombres" ~ "MALES",
                         SEXO=="Mujeres" ~ "FEMALES")) %>%
  rename(YEAR = Year) %>%
  group_by(YEAR,Age_group, SEX, ENTIDAD, CVE_GEO) %>%
  summarize(Dx=sum(DEFUNCIONES))

#################################
#  Combine exposures and deaths
#################################


CONAPO_1998_2021 <- merge(CONAPO_exp_98_2021_aggregated,
                          CONAPO_dx_98_2021_aggregated,
                          by=c("YEAR", "SEX", "Age_group",
                               "ENTIDAD", "CVE_GEO"))


save(CONAPO_1998_2021, file = here::here("Data/tmp/CONAPO_2023_1998_2021_aggregated.RData"))


# ---------------------------------------------------------------------------- #
#     3. Compute standard population
# ---------------------------------------------------------------------------- #

# We will use total population of 2010 as standard population
CONAPO_StandarPopulation <- CONAPO_exp_98_2021_aggregated %>%
  group_by(YEAR,Age_group) %>%
  summarize(Total=sum(PSY)) %>%
  filter(YEAR==2010) %>%
  mutate(MEX = sum(Total),
         Prop = Total/MEX) %>%
  dplyr::select(-c(MEX, Total))


save(CONAPO_StandarPopulation, file = here::here("Data/tmp/CONAPO_StandarPopulation.RData"))

