################################################################################
# Article: Mexico’s surge of violence and COVID-19 drive life expectancy losses 2015–2021
# Title:   Combine deaths from INEGI and CONAPOS mx
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

# --------------------------
# 1.1 Deaths INEGI
# --------------------------

INEGI <- get(load("Data/tmp/Data_deaths_1998_2021_adj.RData"))

table(INEGI$ENT_REGIS, INEGI$YEAR)
# Transform to numeric
INEGI$ENTIDAD <- as.numeric(as.character(INEGI$ENT_REGIS))
INEGI$Age_group <- as.numeric(as.character(INEGI$Age_group))
# To check that it was assign correctly
table(INEGI$ENTIDAD,INEGI$ENT_REGIS)
table(INEGI$Age_group)

# Data
Data_5x1_adj <- INEGI 

Data_5x1_adj <- data.table(Data_5x1_adj)

Data_5x1_adj <- Data_5x1_adj %>% 
  filter(ENT_REGIS!=50)


# ------------------------
#. 1.1.1 National
# ------------------------

Data_5x1_adj_National <- Data_5x1_adj[,list(Dx=sum(Dx)),
                                   list(SEX, YEAR,
                                        Age_group, CoD_ICD10)]



Data_5x1_adj_National_wider <- Data_5x1_adj %>% 
  group_by(SEX, YEAR, Age_group, CoD_ICD10) %>% 
  summarize(Dx=sum(Dx)) %>% 
  group_by(SEX, YEAR, Age_group) %>% 
  mutate(Total_Dx = sum(Dx)) %>% 
  ungroup() %>% 
  mutate(Prop_mx = Dx/Total_Dx) %>% 
  dplyr::select(-c(Dx, Total_Dx)) %>% 
  pivot_wider(names_from = CoD_ICD10,
              values_from = Prop_mx) 


# ------------------------
#. 1.1.2 State
# ------------------------
Data_5x1_adj_State_wider <- Data_5x1_adj %>% 
  group_by(SEX, YEAR, ENTIDAD,
           Age_group, CoD_ICD10) %>% 
  summarize(Dx=sum(Dx)) %>% 
  group_by(SEX, YEAR, Age_group, ENTIDAD) %>% 
  mutate(Total_Dx = sum(Dx)) %>% 
  ungroup() %>% 
  mutate(Prop_mx = Dx/Total_Dx) %>% 
  dplyr::select(-c(Dx, Total_Dx)) %>% 
  pivot_wider(names_from = CoD_ICD10,
              values_from = Prop_mx) 


# --------------------------
# 1.2 CONAPO mx
# --------------------------

CONAPO_mx <-get(load("Data/tmp/CONAPO_2023_1998_2021_aggregated.RData"))

CONAPO_mx_State_tomerge <- CONAPO_mx %>% 
  filter(ENTIDAD!="República Mexicana") %>% 
  mutate(mx = Dx/PSY) %>%
  rename(ENTIDAD_NAME = ENTIDAD,
         ENTIDAD = CVE_GEO)

CONAPO_mx_National_tomerge <- CONAPO_mx %>% 
  filter(ENTIDAD=="República Mexicana") %>% 
  mutate(mx = Dx/PSY) 

# ---------------------------------------------------------------------------- #
#    2. Combine databases
# ---------------------------------------------------------------------------- #

# ------------------------
#. 2.1 National
# ------------------------

Data_analysis_National_CONAPO_2023 <- merge(CONAPO_mx_National_tomerge,
                                            Data_5x1_adj_National_wider,
                           by=c("YEAR","SEX", "Age_group"))

Data_analysis_National_2023 <- Data_analysis_National_CONAPO_2023 %>% 
  filter(YEAR==2015 | YEAR==2019 |
           YEAR==2020 | YEAR==2021) %>% 
  rename(ENTIDAD_NAME = ENTIDAD,
         ENTIDAD = CVE_GEO)



save(Data_analysis_National_2023, file = "Data/Final/Data_analysis_CoD_National_CONAPO_2023.RData")


# ------------------------
#. 2.2 State
# ------------------------

Data_analysis_State_CONAPO_2023 <- merge(CONAPO_mx_State_tomerge,
                                            Data_5x1_adj_State_wider,
                                            by=c("YEAR","SEX", "Age_group", "ENTIDAD"))

Data_analysis_State_2023 <- Data_analysis_State_CONAPO_2023 %>% 
  filter(YEAR==2015 | YEAR==2019 |
           YEAR==2020 | YEAR==2021)



save(Data_analysis_State_2023, file = "Data/Final/Data_analysis_CoD_State_CONAPO_2023.RData")


# ------------------------
#. 2.3 Long trend for ASDR
# ------------------------

# 2.3.1 National
save(Data_analysis_National_CONAPO_2023, file = "Data/Final/Data_98_21_CoD_National_CONAPO_2023.RData")

# 2.3.2 State
save(Data_analysis_State_CONAPO_2023, file = "Data/Final/Data_98_21_CoD_state_CONAPO_2023.RData")

