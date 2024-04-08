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
source("R Code/00. Functions for analysis.R") 

# ---------------------------------------------------------------------------- #
#     1. Open databases
# ---------------------------------------------------------------------------- #

#################################
#  Exposure data
#################################

PSY <- read.csv("Data/CONAPO/2023/00_Pob_Mitad_1950_2070.csv", header = T, sep = ",", stringsAsFactors=FALSE)

colnames(PSY)[2] <- "Year"
colnames(PSY)[5] <- "Age"

#table(PSY$CVE_GEO)

CONAPO_exp_98_2021_aggregated <- PSY %>% 
  filter(Year>=1998 & Year<=2021) %>% 
  filter(ENTIDAD!="Rep\x9cblica Mexicana") %>% 
  mutate(Age_group = case_when(Age==0 ~ 0,
                               Age>=1 & Age<=4 ~ 1,
                               Age>=5 & Age<=9 ~ 5,
                               Age>=10 & Age<=14 ~ 10,
                               Age>=15 & Age<=19 ~ 15,
                               Age>=20 & Age<=24 ~ 20,
                               Age>=25 & Age<=29 ~ 25,
                               Age>=30 & Age<=34 ~ 30,
                               Age>=35 & Age<=39 ~ 35,
                               Age>=40 & Age<=44 ~ 40,
                               Age>=45 & Age<=49 ~ 45,
                               Age>=50 & Age<=54 ~ 50,
                               Age>=55 & Age<=59 ~ 55,
                               Age>=60 & Age<=64 ~ 60,
                               Age>=65 & Age<=69 ~ 65,
                               Age>=70 & Age<=74 ~ 70,
                               Age>=75 & Age<=79 ~ 75,
                               Age>=80 & Age<=84 ~ 80,
                               Age>=85 & Age<=89 ~ 85,
                               Age>=90 & Age<=94 ~ 90,
                               Age>=95 & Age<=120 ~ 95),
         SEX = case_when(SEXO=="Hombres" ~ "MALES",
                         SEXO=="Mujeres" ~ "FEMALES")) %>% 
  rename(YEAR = Year) %>% 
  group_by(YEAR,Age_group, SEX, ENTIDAD, CVE_GEO) %>% 
  summarize(PSY=sum(POBLACION))


#################################
#  Deaths data
#################################

Dx <- read.csv("Data/CONAPO/2023/01_Defunciones_1950_2070.csv", header = T, sep = ",", stringsAsFactors=FALSE)

colnames(Dx)[2] <- "Year"
colnames(Dx)[6] <- "Age"

table(Dx$ENTIDAD)

CONAPO_dx_98_2021_aggregated <- Dx %>% 
  filter(Year>=1998 & Year<=2021) %>% 
  filter(ENTIDAD!="Rep\x9cblica Mexicana") %>% 
  mutate(Age_group = case_when(Age==0 ~ 0,
                               Age>=1 & Age<=4 ~ 1,
                               Age>=5 & Age<=9 ~ 5,
                               Age>=10 & Age<=14 ~ 10,
                               Age>=15 & Age<=19 ~ 15,
                               Age>=20 & Age<=24 ~ 20,
                               Age>=25 & Age<=29 ~ 25,
                               Age>=30 & Age<=34 ~ 30,
                               Age>=35 & Age<=39 ~ 35,
                               Age>=40 & Age<=44 ~ 40,
                               Age>=45 & Age<=49 ~ 45,
                               Age>=50 & Age<=54 ~ 50,
                               Age>=55 & Age<=59 ~ 55,
                               Age>=60 & Age<=64 ~ 60,
                               Age>=65 & Age<=69 ~ 65,
                               Age>=70 & Age<=74 ~ 70,
                               Age>=75 & Age<=79 ~ 75,
                               Age>=80 & Age<=84 ~ 80,
                               Age>=85 & Age<=89 ~ 85,
                               Age>=90 & Age<=94 ~ 90,
                               Age>=95 & Age<=120 ~ 95),
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


save(CONAPO_1998_2021, file = "Data/tmp/CONAPO_2023_1998_2021_aggregated.RData")


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


save(CONAPO_StandarPopulation, file = "Data/tmp/CONAPO_StandarPopulation.RData")

