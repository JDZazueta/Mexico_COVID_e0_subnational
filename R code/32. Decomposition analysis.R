################################################################################
# Article: The contribution of age and causes of death to life expectancy and 
#          lifespan inequality changes in Mexico, 2015-2019 & 2019-2020
# Title:   Compute life tables and decomposition
# Authors: Daniel, Paola & José Manuel
# Data:    Proyecciones de poblacion CONAPO
#          Pob mitad del año
################################################################################


# ---------------------------------------------------------------------------- #
#  0. Working directory and Packages
# ---------------------------------------------------------------------------- #

# To clear everything in R, before start the analysis
rm(list = ls())

pacman::p_load(here)

source(here::here("R Code/00. Functions for analysis.R"))

# ---------------------------------------------------------------------------- #
#  1. Open Data
# ---------------------------------------------------------------------------- #

# -------------------------------- #
#  Data
# -------------------------------- #

Data_state <- get(load("Data/Final/Data_analysis_CoD_State_CONAPO_2023.RData"))


Data_National <- get(load("Data/Final/Data_analysis_CoD_National_CONAPO_2023.RData"))

# ---------------------------------------------------------------------------- #
#  2. Combine data
# ---------------------------------------------------------------------------- #


# - Combined National and State data
Data_analysis <- Data_National %>% 
  rbind(Data_state)

# Replace with 0 missing values
Data_analysis$infectious[is.na(Data_analysis$infectious)] <- 0
Data_analysis$perinatal[is.na(Data_analysis$perinatal)] <- 0
Data_analysis$covid[is.na(Data_analysis$covid)] <- 0


# - Prepare mx weighted by the proportion of causes of death
Data_analysis_longer <- Data_analysis %>% 
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
  dplyr::select(ENTIDAD, ENTIDAD_NAME, mx,
                Age_group, SEX, YEAR, mx, 
                mx_circulatory, mx_diabetes, mx_digestive, mx_homicides, mx_infectious,
                mx_neoplasms, mx_other_external, mx_perinatal, mx_respiratory, mx_covid, mx_all_other) %>% 
  rename(Year = YEAR)  %>% 
  pivot_longer(!c(ENTIDAD, ENTIDAD_NAME,
                  Age_group, SEX, Year),
               names_to =  "cause",
               values_to = "mx") %>% 
  data.table()


Matrix_2015 <- Data_analysis_longer %>% 
  filter(Year==2015 & SEX=="MALES" & ENTIDAD==0) %>% 
  pivot_wider(names_from = cause,
              values_from = mx) %>% 
  dplyr::select(mx_circulatory, mx_diabetes, mx_digestive, mx_homicides, mx_infectious,
                mx_neoplasms, mx_other_external, mx_perinatal, mx_respiratory, mx_covid, mx_all_other) %>% 
  as.matrix()



Matrix_2019 <- Data_analysis_longer %>% 
  filter(Year==2019 & SEX=="MALES" & ENTIDAD==0) %>% 
  pivot_wider(names_from = cause,
              values_from = mx) %>% 
  dplyr::select(mx_circulatory, mx_diabetes, mx_digestive, mx_homicides, mx_infectious,
                mx_neoplasms, mx_other_external, mx_perinatal, mx_respiratory, mx_covid, mx_all_other) %>% 
  as.matrix()



# ---------------------------------------------------------------------------- #
#  3. Decomposition function
# ---------------------------------------------------------------------------- #

Decomp_function <- function(data, Age_group, year, cause, mx){
  
  # -- Create subsets
  Data_2015 <- data[year==2015]
  Data_2019 <- data[year==2019]
  Data_2020 <- data[year==2020]
  Data_2021 <- data[year==2021]
  
  # -- Arrange those subsets
  
  # -- Data 2015
  Matrix_2015 <- Data_2015 %>% 
    pivot_wider(names_from = cause,
                values_from = mx) %>% 
    dplyr::select(mx_circulatory, mx_diabetes, mx_digestive, mx_homicides, mx_infectious,
                  mx_neoplasms, mx_other_external, mx_perinatal, mx_respiratory, mx_covid, mx_all_other) %>% 
    as.matrix()
  
  # -- Data 2019
  Matrix_2019 <- Data_2019 %>% 
    pivot_wider(names_from = cause,
                values_from = mx) %>% 
    dplyr::select(mx_circulatory, mx_diabetes, mx_digestive, mx_homicides, mx_infectious,
                  mx_neoplasms, mx_other_external, mx_perinatal, mx_respiratory, mx_covid, mx_all_other) %>% 
    as.matrix()
  
  # -- Data 2019
  Matrix_2020 <- Data_2020 %>% 
    pivot_wider(names_from = cause,
                values_from = mx) %>% 
    dplyr::select(mx_circulatory, mx_diabetes, mx_digestive, mx_homicides, mx_infectious,
                  mx_neoplasms, mx_other_external, mx_perinatal, mx_respiratory, mx_covid, mx_all_other) %>% 
    as.matrix()
  
  
  # -- Data 2021
  Matrix_2021 <- Data_2021 %>% 
    pivot_wider(names_from = cause,
                values_from = mx) %>% 
    dplyr::select(mx_circulatory, mx_diabetes, mx_digestive, mx_homicides, mx_infectious,
                  mx_neoplasms, mx_other_external, mx_perinatal, mx_respiratory, mx_covid, mx_all_other) %>% 
    as.matrix()
  
  
  # -- Decomposition by period
  
  #########################
  # 2015 - 2019
  #########################
  
  Decomp_result_15_19 <- horiuchi(func = e0frommxch,
                                  pars1 = c(Matrix_2015),
                                  pars2 = c(Matrix_2019),
                                  N = 100)
  
  dim(Decomp_result_15_19) <- dim(Matrix_2015) 
  
  
  Decomp_result_15_19 <- data.frame(Decomp_result_15_19)
  colnames(Decomp_result_15_19) <- cause_names_final
  Decomp_result_15_19$Age <- c(0,1,seq(5,95,5))
  Decomp_result_15_19 <- gather(data = Decomp_result_15_19,key = 
                                  Cause,value = Contribution,-Age)
  Decomp_result_15_19 <- data.table(Decomp_result_15_19)
  Decomp_result_15_19$Period <- "2015-2019"
  
  #########################
  # 2019 - 2020
  #########################
  
  Decomp_result_19_20 <- horiuchi(func = e0frommxch,
                                  pars1 = c(Matrix_2019),
                                  pars2 = c(Matrix_2020),
                                  N = 100)
  
  dim(Decomp_result_19_20) <- dim(Matrix_2019) 
  
  
  Decomp_result_19_20 <- data.frame(Decomp_result_19_20)
  colnames(Decomp_result_19_20) <- cause_names_final
  Decomp_result_19_20$Age <- c(0,1,seq(5,95,5))
  Decomp_result_19_20 <- gather(data = Decomp_result_19_20,key = 
                                  Cause,value = Contribution,-Age)
  Decomp_result_19_20 <- data.table(Decomp_result_19_20)
  Decomp_result_19_20$Period <- "2019-2020"
  
  
  #########################
  # 2020 - 2021
  #########################
  
  Decomp_result_20_21 <- horiuchi(func = e0frommxch,
                                  pars1 = c(Matrix_2020),
                                  pars2 = c(Matrix_2021),
                                  N = 100)
  
  dim(Decomp_result_20_21) <- dim(Matrix_2020) 
  
  
  Decomp_result_20_21 <- data.frame(Decomp_result_20_21)
  colnames(Decomp_result_20_21) <- cause_names_final
  Decomp_result_20_21$Age <- c(0,1,seq(5,95,5))
  Decomp_result_20_21 <- gather(data = Decomp_result_20_21,key = 
                                  Cause,value = Contribution,-Age)
  Decomp_result_20_21 <- data.table(Decomp_result_20_21)
  Decomp_result_20_21$Period <- "2020-2021"
  
  #########################
  # 2015 - 2021
  #########################
  
  Decomp_result_15_21 <- horiuchi(func = e0frommxch,
                                  pars1 = c(Matrix_2015),
                                  pars2 = c(Matrix_2021),
                                  N = 100)
  
  dim(Decomp_result_15_21) <- dim(Matrix_2015) 
  
  
  Decomp_result_15_21 <- data.frame(Decomp_result_15_21)
  colnames(Decomp_result_15_21) <- cause_names_final
  Decomp_result_15_21$Age <- c(0,1,seq(5,95,5))
  Decomp_result_15_21 <- gather(data = Decomp_result_15_21,key = 
                                  Cause,value = Contribution,-Age)
  Decomp_result_15_21 <- data.table(Decomp_result_15_21)
  Decomp_result_15_21$Period <- "2015-2021"
  
  #########################
  # 2019 - 2021
  #########################
  
  Decomp_result_19_21 <- horiuchi(func = e0frommxch,
                                  pars1 = c(Matrix_2019),
                                  pars2 = c(Matrix_2021),
                                  N = 100)
  
  dim(Decomp_result_19_21) <- dim(Matrix_2019) 
  
  
  Decomp_result_19_21 <- data.frame(Decomp_result_19_21)
  colnames(Decomp_result_19_21) <- cause_names_final
  Decomp_result_19_21$Age <- c(0,1,seq(5,95,5))
  Decomp_result_19_21 <- gather(data = Decomp_result_19_21,key = 
                                  Cause,value = Contribution,-Age)
  Decomp_result_19_21 <- data.table(Decomp_result_19_21)
  Decomp_result_19_21$Period <- "2019-2021"
  
  
  # -- Combine decomp outputs
  
  Decomp_result <- rbind(Decomp_result_15_19,
                         Decomp_result_19_20,
                         Decomp_result_20_21,
                         Decomp_result_15_21,
                         Decomp_result_19_21)
  
  return(Decomp_result)
  
}


# ---------------------------------------------------------------------------- #
#  4. Decomposition analysis
# ---------------------------------------------------------------------------- #

# Performed decomposition analysis
# This might take ~ 8mn
#tic()
Decomp_e0_results <- Data_analysis_longer[, Decomp_function(.SD,
                                                            Age_group = Age_group,
                                                            cause = cause,
                                                            year = Year,
                                                            mx = mx),
                                          by=list(ENTIDAD,ENTIDAD_NAME, SEX)]


#toc()
save(Decomp_e0_results,
     file = "Data/Final/Decomp_e0_results_state.RData")

