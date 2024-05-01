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

source(here::here("R Code/00. Functions for analysis.R"))

# ---------------------------------------------------------------------------- #
#  1. Open Data
# ---------------------------------------------------------------------------- #

# -------------------------------- #
#  Data
# -------------------------------- #

Data_state <- get(load("Data/Final/Data_98_21_CoD_state_CONAPO_2023.RData"))


Data_National <- get(load("Data/Final/Data_98_21_CoD_National_CONAPO_2023.RData"))

# Open data of standard population
Standard_pop <- get(load("Data/tmp/CONAPO_StandarPopulation.RData"))


# ---------------------------------------------------------------------------- #
#     2. Compute Age-standardized mortality rates National
# ---------------------------------------------------------------------------- #

# - Combined National and State data
Data_analysis <- Data_National %>% 
  rename(ENTIDAD_NAME = ENTIDAD,
         ENTIDAD = CVE_GEO) %>% 
  rbind(Data_state)

# Replace with 0 missing values
Data_analysis$infectious[is.na(Data_analysis$infectious)] <- 0
Data_analysis$perinatal[is.na(Data_analysis$perinatal)] <- 0
Data_analysis$covid[is.na(Data_analysis$covid)] <- 0


# - Compute Age-standardized mortality rates
MEX_ASMR_national_states <- Data_analysis %>% 
  mutate(Total = all_other + circulatory + diabetes + digestive + homicides + infectious +
           neoplasms + other_external +  perinatal + respiratory + covid,
         mx_circulatory = round(circulatory*mx,6),
         mx_diabetes = round(diabetes*mx,6),
         mx_digestive = round(digestive*mx,6),
         mx_homicides = round(homicides*mx,6),
         mx_infectious = round(infectious*mx,6),
         mx_neoplasms = round(neoplasms*mx,6),
         mx_other_external = round(other_external*mx,6),
         mx_perinatal = round(perinatal*mx,6),
         mx_respiratory = round(respiratory*mx,6),
         mx_covid = round(covid*mx,6),
         mx_all_other = round(all_other*mx,6),
         check =  mx_circulatory + mx_diabetes + mx_digestive + mx_homicides +
           mx_infectious + mx_neoplasms + mx_other_external + mx_perinatal + mx_respiratory +
           mx_covid + mx_all_other) %>% 
  dplyr::select(ENTIDAD, ENTIDAD_NAME,PSY, Dx, mx,
                Age_group, SEX, YEAR, mx, 
                mx_circulatory, mx_diabetes, mx_digestive, mx_homicides, mx_infectious,
                mx_neoplasms, mx_other_external, mx_perinatal, mx_respiratory, mx_covid, mx_all_other) %>% 
  rename(Year = YEAR)  %>% 
  mutate(Total_mx =  mx_circulatory + mx_diabetes + mx_digestive + mx_homicides +
           mx_infectious + mx_neoplasms + mx_other_external + mx_perinatal + mx_respiratory +
           mx_covid + mx_all_other,
         mx_amenable = mx_circulatory + mx_neoplasms + mx_respiratory + mx_infectious + mx_digestive + mx_perinatal, 
         mx_diabetes = mx_diabetes,
         mx_violence = mx_homicides,
         mx_Covid = mx_covid,
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
  
save(MEX_ASMR_national_states, file = "Data/Final/MEX_ASMR.RData")

