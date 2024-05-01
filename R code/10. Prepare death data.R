################################################################################
# Article: Mexico’s surge of violence and COVID-19 drive life expectancy losses 2015–2021
# Title:   Prepare data on causes of death
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

Data <- get(load(here::here("Data/tmp/Raw deaths 1990-2021.RData")))

# ---------------------------------------------------------------------------- #
#     2. Clean data
# ---------------------------------------------------------------------------- #

# First we create age variables
#table(Data$EDAD)

Data$Age_year <- substr(Data$EDAD,0,1)
Data$Time_Age <- substr(Data$EDAD,2,4)
Data$Time_Age <- as.numeric(Data$Time_Age)

# To check
#table(Data$Time_Age)
#hist(Data$Time_Age[Data$Time_Age<=120])


# ------------
#  Causes fo death
# ------------

# To create causes of death we need to separate to construct two variables for easy
# manipulation

Data$ICD_Group <- substr(Data$CAUSA_DEF,0,1)
Data$ICD_number <- substr(Data$CAUSA_DEF,2,3)
Data$ICD_number <- as.numeric(Data$ICD_number)
Data$ICD_base <- substr(Data$CAUSA_DEF, 0, 3)


# We create the variables
#   1. Check_year => if the year of ocurres correpond to year of register of the death
#   2. Check_age => if there no info of age registration
#   3. Check_age_birth => if the age correspond to the year of birth
#   4. We used the same classification of causes of death as Garcia and Aburo 2019
#      but we add covid for 2020
#         Labels of the variable:
#     1 = Circulatory diseases (cardiovascular, stroke)
#         I05-I09, I11, I13, I21-I51, I60-I69
#     2 = Neoplasms	C00-C97
#     3 = Diabetes	E10-E14
#     4 = Homicides and other violent causes with undetermined intention	X85-Y09, Y10-Y34, Y35-Y36
#     5 = Other external causes (including traffic accidents, injuries and suicide)	V01-V89, V90-X59, X60-X84
#     6 = Respiratory Diseases	J00-J98
#     7 = Infectious Diseases	A00-B99
#     8 = Digestive Diseases	K00-K92
#     9 = Conditions originated in the perinatal period	P00-P96
#    10 = COVID


circulatory <- str_c("I", padnum(c(5:9, 11, 13, 21:51, 60:69)))
neoplasms <- str_c("C", padnum(0:97))
diabetes <- str_c("E", padnum(10:14))
homicides <- c(str_c("X", padnum(85:99)),
               str_c("Y", padnum(1:36)))
other_external <- c(str_c("X", padnum(0:84)),
                    str_c("Y", padnum(40:99)),
                    str_c("W", padnum(0:99)),
                    str_c("V", padnum(1:99)))
respiratory <- str_c("J", padnum(0:98))
infectious <- c(str_c("A", padnum(0:99)),
                str_c("B", padnum(0:99)))
digestive <- str_c("K", padnum(0:92))
perinatal <- str_c("P", padnum(0:96))


Deaths_raw_svar_2 <- Data %>%
  mutate(Age = case_when(Age_year==4 ~ Time_Age,
                         Age_year!=4 ~ 0),
         Age_missing = case_when(Age==998 ~ 1,
                                 Age!=998 ~ 0),
         Age_birth = YEAR-ANIO_NACIM,
         Dif_age_age_birth = Age - Age_birth,
         Check_age_birth = case_when(Dif_age_age_birth>=-1 & Dif_age_age_birth<=1 ~ 0,
                                     Dif_age_age_birth<=-2 | Dif_age_age_birth<=1 ~ 1),
         CoD_ICD10 = case_when(LISTA == "06T" ~ "covid", # COVID
                               ICD_base %in% circulatory ~ "circulatory",
                               ICD_base %in% neoplasms ~ "neoplasms",
                               ICD_base %in% diabetes ~ "diabetes",
                               ICD_base %in% homicides ~ "homicides",
                               ICD_base %in% other_external ~ "other_external",
                               ICD_base %in% respiratory ~ "respiratory",
                               ICD_base %in% infectious ~ "infectious",
                               ICD_base %in% digestive ~ "digestive",
                               ICD_base %in% perinatal ~ "perinatal",
                               TRUE ~ "all_other"), # all other causes
         Age_group = case_when(Age == 0 ~ 0,
                               between(Age, 1, 4) ~ 1,
                               between(Age, 5, 94) ~ (Age %/% 5) * 5,
                               between(Age, 95, 120) ~ 95,
                               Age == 998 ~ 998),
         n = 1)

Deaths_raw_svar_2$CoD_ICD10[is.na(Deaths_raw_svar_2$CoD_ICD10)] <- 0


# Proportion of deaths by year
round(prop.table(table(Deaths_raw_svar_2$YEAR,
                       Deaths_raw_svar_2$CoD_ICD10),1),4)*100

# Transform to data.table
Deaths_raw_svar_2 <- data.table(Deaths_raw_svar_2)

Deaths_aggregated <- Deaths_raw_svar_2[,list(Dx = sum(n)),
                                       by=list(YEAR,SEXO,ENT_REGIS, 
                                               Age_group, CoD_ICD10)]


Deaths_aggregated_98_21 <- Deaths_aggregated %>% 
  filter(YEAR>=1998)

sum(Deaths_aggregated_98_21$Dx)
# n= 14,549,180

# ---------------------------------------------------------------------------- #
#     3. Redistribution of the missing
# ---------------------------------------------------------------------------- #


Deaths_aggregated_98_21$SEXO <- factor(Deaths_aggregated_98_21$SEXO,
                                       levels = c(1,2,9),
                                       labels = c("Males", "Females", "Unknown"))

# We transform the data to get 
Deaths_aggregated_98_21_wider <- Deaths_aggregated_98_21 %>% 
  pivot_wider(names_from = SEXO,
              values_from = Dx)

# We replace the missing columns with 0
Deaths_aggregated_98_21_wider$Males[is.na(Deaths_aggregated_98_21_wider$Males)]<-0
Deaths_aggregated_98_21_wider$Females[is.na(Deaths_aggregated_98_21_wider$Females)]<-0
Deaths_aggregated_98_21_wider$Unknown[is.na(Deaths_aggregated_98_21_wider$Unknown)]<-0

# Check how many deaths with unkonw sex
sum(Deaths_aggregated_98_21_wider$Unknown)
# n= 8,202



# We redistribute the persons with Unknow sex by, Age, Year, State and CoD
# For cases where we don't have sex, and we have more Unkonw deaths, we
# apply the average share of deaths by Year

Deaths_aggregated_98_21_wider_2 <- Deaths_aggregated_98_21_wider %>% 
  group_by(Age_group, YEAR, ENT_REGIS, CoD_ICD10) %>% 
  mutate(Total_M = sum(Males),
         Total_F = sum(Females)) %>% 
  ungroup() %>% 
  mutate(Total_pop =  Total_M + Total_F,
         Average_SM = Total_M/Total_pop,
         Average_SF = Total_F/Total_pop,
         Total_u =  Males + Females + Unknown,
         Total = Males + Females,
         Mayor_Unkonw = case_when(Unknown<Total~ 0,
                                  Unknown>Total~ 1),
         Share_M = Males/Total,
         Share_F = Females/Total,
         Unkonw_M = case_when(Mayor_Unkonw==0 ~ Unknown*Share_M,
                              Mayor_Unkonw==1 ~ Unknown*Average_SM),
         Unkonw_F = case_when(Mayor_Unkonw==0 ~ Unknown*Share_F,
                              Mayor_Unkonw==1 ~ Unknown*Average_SF),
         New_M = Males + Unkonw_M,
         New_F = Females + Unkonw_F,
         New_Total = New_M + New_F,
         Check = New_Total-Total_u,
         Casos_especiales = case_when(Check==0 ~0,
                                      Check>0~1)) 

#we have a problem with the NA
Deaths_aggregated_98_21_wider_2$New_M[is.na(Deaths_aggregated_98_21_wider_2$New_M)]<-0
Deaths_aggregated_98_21_wider_2$New_F[is.na(Deaths_aggregated_98_21_wider_2$New_F)]<-0
Deaths_aggregated_98_21_wider_2$New_Total[is.na(Deaths_aggregated_98_21_wider_2$New_Total)]<-0

sum(Deaths_aggregated_98_21_wider_2$New_Total)
# N= 14,549,027

# Base distribution de missing by Sex, age, and CoD
Deaths_aggregated_98_21_distri <- Deaths_aggregated_98_21_wider_2 %>% 
  dplyr::select(YEAR,CoD_ICD10,Age_group, ENT_REGIS,
                New_M, New_F,New_Total) %>% 
  rename(FEMALES = New_F,
         MALES = New_M,
         TOTAL = New_Total) %>% 
  dplyr::select(-TOTAL) %>% 
  pivot_longer(!c(YEAR, Age_group, CoD_ICD10, ENT_REGIS),
               names_to = "SEX",
               values_to = "Dx") 

  
# Total with missing age 14,549,027
sum(Deaths_aggregated_98_21_distri$Dx)
# n= 14,549,064 with no missing age at death

# Data set with only missing age
Deaths_aggregated_98_21_age <- Deaths_aggregated_98_21_distri %>% 
  pivot_wider(names_from = Age_group,
              values_from = Dx)

# Fill NAN cells with 0 for redistribution
Deaths_aggregated_98_21_age$`0`[is.na(Deaths_aggregated_98_21_age$`0`)] <- 0
Deaths_aggregated_98_21_age$`1`[is.na(Deaths_aggregated_98_21_age$`1`)] <- 0
Deaths_aggregated_98_21_age$`5`[is.na(Deaths_aggregated_98_21_age$`5`)] <- 0
Deaths_aggregated_98_21_age$`10`[is.na(Deaths_aggregated_98_21_age$`10`)] <- 0
Deaths_aggregated_98_21_age$`15`[is.na(Deaths_aggregated_98_21_age$`15`)] <- 0
Deaths_aggregated_98_21_age$`20`[is.na(Deaths_aggregated_98_21_age$`20`)] <- 0
Deaths_aggregated_98_21_age$`25`[is.na(Deaths_aggregated_98_21_age$`25`)] <- 0
Deaths_aggregated_98_21_age$`30`[is.na(Deaths_aggregated_98_21_age$`30`)] <- 0
Deaths_aggregated_98_21_age$`35`[is.na(Deaths_aggregated_98_21_age$`35`)] <- 0
Deaths_aggregated_98_21_age$`40`[is.na(Deaths_aggregated_98_21_age$`40`)] <- 0
Deaths_aggregated_98_21_age$`45`[is.na(Deaths_aggregated_98_21_age$`45`)] <- 0
Deaths_aggregated_98_21_age$`50`[is.na(Deaths_aggregated_98_21_age$`50`)] <- 0
Deaths_aggregated_98_21_age$`55`[is.na(Deaths_aggregated_98_21_age$`55`)] <- 0
Deaths_aggregated_98_21_age$`60`[is.na(Deaths_aggregated_98_21_age$`60`)] <- 0
Deaths_aggregated_98_21_age$`65`[is.na(Deaths_aggregated_98_21_age$`65`)] <- 0
Deaths_aggregated_98_21_age$`70`[is.na(Deaths_aggregated_98_21_age$`70`)] <- 0
Deaths_aggregated_98_21_age$`75`[is.na(Deaths_aggregated_98_21_age$`75`)] <- 0
Deaths_aggregated_98_21_age$`80`[is.na(Deaths_aggregated_98_21_age$`80`)] <- 0
Deaths_aggregated_98_21_age$`85`[is.na(Deaths_aggregated_98_21_age$`85`)] <- 0
Deaths_aggregated_98_21_age$`90`[is.na(Deaths_aggregated_98_21_age$`90`)] <- 0
Deaths_aggregated_98_21_age$`95`[is.na(Deaths_aggregated_98_21_age$`95`)] <- 0
Deaths_aggregated_98_21_age$`998`[is.na(Deaths_aggregated_98_21_age$`998`)] <- 0


# ---------------------------------------------
#  Resitribution of age
# --------------------------------------------

Deaths_aggregated_98_21_age_longer <- Deaths_aggregated_98_21_age %>% 
  group_by(SEX, YEAR, ENT_REGIS, CoD_ICD10) %>% 
  mutate(Total_Missing = sum(`998`),
         Total_Age = sum(`0` + `1` + `5` + 
                           `10` + `15` +
                           `20` + `25` +
                           `30` + `35` +
                           `40` + `45` +
                           `50` + `55` +
                           `60` + `65` +
                           `70` + `75` +
                           `80` + `85` +
                           `90` + `95`),
         Total_total = Total_Missing + Total_Age) %>% 
  dplyr::select(-`998`) %>% 
  pivot_longer(!c(SEX, YEAR, ENT_REGIS, CoD_ICD10,
                  Total_Missing, Total_Age, Total_total),
               names_to = "Age_group",
               values_to =  "Dx") %>% 
  mutate(Share_Age = Dx/Total_Age,
         Check = case_when(Total_Missing==0 & Total_Age==0 & Total_total==0 ~ 0,
                           Total_Missing!=0 | Total_Age!=0 | Total_total!=0 ~ 1),
         Dx_new = case_when(Check==0 ~ 0,
                            Check==1 ~ Dx + (Share_Age*Total_Missing))) %>% 
  group_by(SEX, YEAR, ENT_REGIS, CoD_ICD10) %>% 
  mutate(Total_check = sum(Dx_new))
  
  
sum(Deaths_aggregated_98_21_age_longer$Dx_new)
# Total 14,549,064

# -------------------------------------------------
#  Prepare final dataset
# -------------------------------------------------

Data_deaths_1998_2021_adj_pre <- Deaths_aggregated_98_21_age_longer %>% 
  dplyr::select(YEAR, Age_group, SEX, CoD_ICD10, ENT_REGIS, Dx_new) %>% 
  rename(Dx = Dx_new)

Data_deaths_1998_2021_adj_pre$Dx[is.na(Data_deaths_1998_2021_adj_pre$Dx)] <- 0

Data_deaths_1998_2021_adj_pre <- data.table(Data_deaths_1998_2021_adj_pre)  
Data_deaths_1998_2021_adj_State <- Data_deaths_1998_2021_adj_pre[,list(Dx=sum(Dx)),
                                                                 by=list(Age_group,YEAR, ENT_REGIS,
                                                                         SEX, CoD_ICD10)]

sum(Data_deaths_1998_2021_adj_State$Dx)
# n=14,549,064


# Prepare death counts for National
Data_deaths_1998_2021_adj_National <- Data_deaths_1998_2021_adj_pre[,list(Dx=sum(Dx)),
                                                                    by=list(Age_group,YEAR,
                                                                            SEX, CoD_ICD10)]

sum(Data_deaths_1998_2021_adj_National$Dx)
Data_deaths_1998_2021_adj_National$ENT_REGIS <- 50

################
# Combine National and state
################

Data_deaths_1998_2021_adj <- rbind(Data_deaths_1998_2021_adj_State,
                                   Data_deaths_1998_2021_adj_National)

# We save the data
save(Data_deaths_1998_2021_adj,
     file = "R Code/Data/tmp/Data_deaths_1998_2021_adj.RData")



Data_deaths_1998_2021_adj_check_national <- Data_deaths_1998_2021_adj_National %>% 
  group_by(YEAR) %>% 
  summarize(Dx_total = sum(Dx)) 


Data_deaths_1998_2021_adj_check_state <- Data_deaths_1998_2021_adj_State %>% 
  group_by(YEAR) %>% 
  summarize(Dx_total = sum(Dx)) 

