################################################################################
# Article: Mexico’s surge of violence and COVID-19 drive life expectancy losses 2015–2021
# Title:   Packages and functions
# Authors: Daniel, Paola, Maria & José Manuel
# Data:    Proyecciones de poblacion CONAPO (2023), CONEVAL (2016-2018), and
#          Deaths from INEGI
################################################################################

# Packages

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(tidyverse, data.table, foreign, magrittr,
               patchwork, hrbrthemes, here, ggdark, segmented,
               lmtest, scales, RColorBrewer, ggpubr, purrr,
               ggridges, ggalluvial, DemoDecomp, parallel, tictoc,
               sp, grid, stringr)

# Constants

cause_names<-c("0"="Rest of causes",
               "1"="Circulatory",
               "2"="Neoplasm",
               "3"="Diabetes",
               "4"="Homicides & Violence",
               "5"="Other externals",
               "6"= "Respiratory",
               "7"="Infectious diseases",
               "8"="Digestive",
               "9"="Perinatal",
               "10" = "COVID")

cause_names_order <- c("Circulatory","COVID", "Diabetes",
                       "Digestive", "Homicides & Violence",
                       "Infectious diseases", "Neoplasm",
                       "Other externals", "Perinatal",
                       "Respiratory","Rest of causes")



Color_big_groups <- c("paleturquoise3", "#FB6A4A", "#8C96C6", "red4",
                      "Grey80", "Grey40")


# functions

padnum <- function(numbers) {

    str_pad(numbers, width = 2, side = "left", pad = "0")

}


#display.brewer.pal(n = 10, name = 'RdBu')
#brewer.pal(n = 10, name = "RdBu")
#.    0 = All other causes
#     1 = Circulatory diseases (cardiovascular, stroke)
#         I05-I09, I11, I13, I21-I51, I60-I69
#     2 = Neoplasms	C00-C97
#     3 = Diabetes	E10-E14
#     4 = Homicides and other violent causes with undetermined intention	X85-Y09, Y10-Y34, Y35-Y36
#     5 = Other external causes (including traffic accidents, injuries and suicide)	W00-W99, V01-V89, V90-X59, X60-X84, Y40-Y99
#     6 = Respiratory Diseases	J00-J98
#     7 = Infectious Diseases	A00-B99
#     8 = Digestive Diseases	K00-K92
#     9 = Conditions originated in the perinatal period	P00-P96
#    10 = COVID


# Amenable: Circulatory, neoplasm, respiratory, infectious diseases,
#           digestive, perinatal



# ---------------------------------------------------------------------------- #
#  Function Life table
# ---------------------------------------------------------------------------- #

#SEX 1: Male, 2: Female, n=1 because is one age-groups interval
SLT <- function (nmx, age, Sex, nax = NULL, n){

  if (is.null(nax)) {
    nax <- 0.5 * n

    if (n[2] == 4) {
      if (Sex == "MALES") {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (Sex == "FEMALES") {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }

  nqx <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx <- c(nqx[-(length(nqx))], 1)
  for (i in 1:length(nqx)) {
    if (nqx[i] > 1)
      nqx[i] <- 1
  }
  npx <- 1 - nqx
  l0 = 100000 #standard
  lx <- round(cumprod(c(l0, npx)))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLx <- n * lxpn + ndx * nax
  #nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  lt <- data.frame(Age = age,
                   Sex,
                   n,
                   nmx = round(nmx, 4),
                   nqx = round(nqx, 4),
                   nax = round(nax, 4),
                   npx = round(npx,4),
                   lx, ndx,
                   Lx = round(nLx,3),
                   Tx = round(Tx,3),
                   ex = round(ex,3))

  return(lt)
}

# ---------------------------------------------------------------------------- #
#  Function Life table for decomposition
# ---------------------------------------------------------------------------- #

# Life expectancy function
e0.frommx_h <- function(nmx =  mx, Sex=1, age = c(0,1,seq(5, 95, 5)), nax = NULL){
  n   <- c(diff(age), 999)

  if (is.null(nax)) {
    nax <- 0.5 * n

    if (n[2] == 4) {
      if (Sex == 1) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (Sex == 2) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1

  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  e0 <- ex[1]

  return(e0)
}

e0frommxch <- function(mxcvec){
  dim(mxcvec) <- c(21,length(mxcvec)/21)
  mx <- rowSums(mxcvec)
  e0.frommx_h(mx)
}

# ---------------------------------------------------------------------------- #
#  Decomposition function
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
    dplyr::select(mx_0,
                  mx_1, mx_2, mx_3, mx_4, mx_5,
                  mx_6, mx_7, mx_8, mx_9, mx_10) %>%
    as.matrix()

  # -- Data 2019
  Matrix_2019 <- Data_2019 %>%
    pivot_wider(names_from = cause,
                values_from = mx) %>%
    dplyr::select(mx_0,
                  mx_1, mx_2, mx_3, mx_4, mx_5,
                  mx_6, mx_7, mx_8, mx_9, mx_10) %>%
    as.matrix()

  # -- Data 2019
  Matrix_2020 <- Data_2020 %>%
    pivot_wider(names_from = cause,
                values_from = mx) %>%
    dplyr::select(mx_0,
                  mx_1, mx_2, mx_3, mx_4, mx_5,
                  mx_6, mx_7, mx_8, mx_9, mx_10) %>%
    as.matrix()


  # -- Data 2021
  Matrix_2021 <- Data_2021 %>%
    pivot_wider(names_from = cause,
                values_from = mx) %>%
    dplyr::select(mx_0,
                  mx_1, mx_2, mx_3, mx_4, mx_5,
                  mx_6, mx_7, mx_8, mx_9, mx_10) %>%
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
  colnames(Decomp_result_15_19) <- cause_names
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
  colnames(Decomp_result_19_20) <- cause_names
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
  colnames(Decomp_result_20_21) <- cause_names
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
  colnames(Decomp_result_15_21) <- cause_names
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
  colnames(Decomp_result_19_21) <- cause_names
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

