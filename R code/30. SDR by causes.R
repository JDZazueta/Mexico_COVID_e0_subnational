################################################################################
# Article: Mexico’s surge of violence and COVID-19 drive life expectancy losses 2015–2021
# Title:   Compute age.standardized death rates
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

source(here::here("R Code/00.Functions for analysis.R"))

# ---------------------------------------------------------------------------- #
#  1. Open Data
# ---------------------------------------------------------------------------- #

# -------------------------------- #
#  Data
# -------------------------------- #

Data_state <- get(load(here::here("Data/Final/Data_98_21_CoD_state_CONAPO_2023.RData")))


Data_National <- get(load(here::here("Data/Final/Data_98_21_CoD_National_CONAPO_2023.RData")))

# Open data of standard population
Standard_pop <- get(load(here::here("Data/tmp/CONAPO_StandarPopulation.RData")))


# ---------------------------------------------------------------------------- #
#     2. Compute Age-standardized mortality rates National
# ---------------------------------------------------------------------------- #

# - Combined National and State data
Data_analysis <- Data_National %>%
  rename(ENTIDAD_NAME = ENTIDAD,
         ENTIDAD = CVE_GEO) %>%
  rbind(Data_state)

# Replace with 0 missing values
Data_analysis$`7`[is.na(Data_analysis$`7`)] <- 0
Data_analysis$`9`[is.na(Data_analysis$`9`)] <- 0
Data_analysis$`10`[is.na(Data_analysis$`10`)] <- 0

# - Compute Age-standardized mortality rates
MEX_ASMR_national_states <- Data_analysis %>%
  mutate(Total = `0` + `1` + `2` + `3` + `4` + `5` +
           `6` + `7` + `8` + `9` + `10`,
         Rest = round(1 - Total,2),
         `0` = `0` + Rest,
         mx_0 = round(`0`*mx,6),
         mx_1 = round(`1`*mx,6),
         mx_2 = round(`2`*mx,6),
         mx_3 = round(`3`*mx,6),
         mx_4 = round(`4`*mx,6),
         mx_5 = round(`5`*mx,6),
         mx_6 = round(`6`*mx,6),
         mx_7 = round(`7`*mx,6),
         mx_8 = round(`8`*mx,6),
         mx_9 = round(`9`*mx,6),
         mx_10 = round(`10`*mx,6),
         check = mx_0 + mx_1 + mx_2 + mx_3 + mx_4 +
           mx_5 + mx_6 + mx_7 + mx_7 + mx_8 +
           mx_9 + mx_10) %>%
  dplyr::select(ENTIDAD, ENTIDAD_NAME,PSY, Dx, mx,
                Age_group, SEX, YEAR, mx, mx_0,
                mx_1, mx_2, mx_3, mx_4, mx_5,
                mx_6, mx_7, mx_8, mx_9, mx_10) %>%
  rename(Year = YEAR)  %>%
  mutate(Total_mx = mx_0 + mx_1 + mx_2 + mx_3 + mx_4 + mx_5 + mx_6 + mx_7 + mx_8 + mx_9 + mx_10,
         mx_amenable = mx_1 + mx_2 + mx_6 + mx_7 + mx_8 + mx_9,
         mx_diabetes = mx_3,
         mx_violence = mx_4,
         mx_Covid = mx_10,
         Dx_total = Total_mx*PSY,
         Dx_amenable = mx_amenable*PSY,
         Dx_diabetes = mx_diabetes*PSY,
         Dx_violence = mx_violence*PSY,
         Dx_Covid = mx_Covid*PSY) %>%
  group_by(Year, SEX, Age_group, ENTIDAD, ENTIDAD_NAME) %>%
  summarize(PSY=sum(PSY),
            Dx = sum(Dx),
            Dx_total = sum(Dx_total),
            Dx_amenable = sum(Dx_amenable),
            Dx_diabetes = sum(Dx_diabetes),
            Dx_violence = sum(Dx_violence),
            Dx_Covid = sum(Dx_Covid)) %>%
  left_join(Standard_pop)  %>%
  mutate(mx_INEGI = Dx_total/PSY,
         AESMR = Prop*mx_INEGI,
         mx_violence = Dx_violence/PSY,
         mx_COVID = Dx_Covid/PSY,
         mx_diabetes = Dx_diabetes/PSY,
         mx_amenable = Dx_amenable/PSY,
         ASAmenable= Prop*mx_amenable,
         ASDiabetes = Prop*mx_diabetes,
         ASVR = Prop*mx_violence,
         ASCOVID = Prop*mx_COVID) %>%
  group_by(Year, SEX, ENTIDAD, ENTIDAD_NAME) %>%
  summarize(SDR=sum(AESMR)*100000,
            SDR_violence=sum(ASVR)*100000,
            SDR_amenable=sum(ASAmenable)*100000,
            SDR_diabetes=sum(ASDiabetes)*100000,
            SDR_COVID = sum(ASCOVID)*100000)

save(MEX_ASMR_national_states, file = here::here("Data/Final/MEX_ASMR.RData"))

