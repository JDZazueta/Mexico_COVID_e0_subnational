################################################################################
# Article: Mexico’s surge of violence and COVID-19 drive life expectancy losses 2015–2021
# Title:   Supplemental Figures on decomposition of causes of death
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
#  1. Open datasets
# ---------------------------------------------------------------------------- #

# - Decomposition
get(load(here::here("Data/Final/Decomp_e0_results_state.RData")))

# ---------------------------------------------------------------------------- #
#  2. Prepare data for figures
# ---------------------------------------------------------------------------- #


Decomp_e0_results$ENTIDAD <- factor(Decomp_e0_results$ENTIDAD, levels = c(seq(0,32,1)),
                                    labels = c("National",
                                               "Aguascalientes", "Baja California", "Baja California Sur",
                                               "Campeche", "Coahulia", "Colima",
                                               "Chiapas", "Chihuahua", "Mexico City",
                                               "Durango", "Guanajuato", "Guerrero",
                                               "Hidalgo", "Jalisco","Mexico",
                                               "Michoacan", "Morelos", "Nayarit",
                                               "Nuevo Leon", "Oaxaca", "Puebla",
                                               "Queretaro", "Quintana Roo", "San Luis Potosi",
                                               "Sinaloa", "Sonora", "Tabasco",
                                               "Tamaulipas", "Tlaxcala", "Veracruz",
                                               "Yucatan", "Zacatecas"))

# ---------------------------------------------------------------------------- #
#  2. Prepare data for figures
# ---------------------------------------------------------------------------- #


Data_Supplemental_CoD_analysis <- Decomp_e0_results %>%
  filter(ENTIDAD!="National") %>%
  mutate(Sex = case_when(SEX=="MALES" ~ 1,
                         SEX=="FEMALES" ~ 2)) %>%
  dplyr::select(-SEX) %>%
  group_by(Period, Sex, Cause, ENTIDAD) %>%
  summarise(Contribution=sum(Contribution)) %>%
  group_by(Period, Sex, ENTIDAD) %>%
  mutate(Total = sum(Contribution)) %>%
  ungroup() %>%
  mutate(Relative = Contribution/Total*100,
         Groups = case_when(Cause=="COVID" ~ 1,
                            Cause=="Diabetes" ~ 2,
                            Cause=="Circulatory" |
                              Cause=="Digestive" | Cause=="Infectious diseases" |
                              Cause=="Neoplasm" | Cause=="Perinatal" |
                              Cause=="Respiratory" ~ 3,
                            Cause=="Homicides & Violence" ~ 4,
                            Cause=="Other externals" ~ 5,
                            Cause=="Rest of causes" ~ 6),
         Region = case_when(ENTIDAD=="Chihuahua" | ENTIDAD=="Sinaloa" |
                              ENTIDAD=="Durango" | ENTIDAD=="Baja California" |
                              ENTIDAD=="Nuevo Leon" | ENTIDAD=="Tamaulipas" |
                              ENTIDAD=="Sonora" | ENTIDAD=="Coahulia" |
                              ENTIDAD=="San Luis Potosi" | ENTIDAD=="Zacatecas" |
                              ENTIDAD=="Baja California Sur" ~ 1,
                            ENTIDAD=="Nayarit" | ENTIDAD=="Colima" |
                              ENTIDAD=="Jalisco" | ENTIDAD=="Aguascalientes" |
                              ENTIDAD=="Michoacan" | ENTIDAD=="Tlaxcala" |
                              ENTIDAD=="Hidalgo" | ENTIDAD=="Mexico" |
                              ENTIDAD=="Mexico City" | ENTIDAD=="Queretaro" |
                              ENTIDAD=="Guanajuato" ~ 2,
                            ENTIDAD=="Guerrero" | ENTIDAD=="Morelos" |
                              ENTIDAD=="Veracruz" | ENTIDAD=="Oaxaca" |
                              ENTIDAD=="Quintana Roo" | ENTIDAD=="Tabasco" |
                              ENTIDAD=="Hidalgo" | ENTIDAD=="Mexico" |
                              ENTIDAD=="Puebla" | ENTIDAD=="Campeche" |
                              ENTIDAD=="Yucatan" | ENTIDAD=="Chiapas" ~ 3)) %>%
  group_by(ENTIDAD, Period, Sex, Groups, Region) %>%
  summarize(Contribution = sum(Contribution)) %>%
  group_by(ENTIDAD, Period, Sex, Region) %>%
  mutate(Total_ex = sum(Contribution))


Data_Supplemental_CoD_analysis$Groups <- factor(Data_Supplemental_CoD_analysis$Groups,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c("COVID", "Diabetes", "Amenable diseases",
                                            "Homicides & Violence","External causes",
                                            "Rest of causes"))


Data_Supplemental_CoD_analysis$Region <- factor(Data_Supplemental_CoD_analysis$Region,
                                 levels = c(1,2,3),
                                 labels = c("North",
                                            "Central",
                                            "South"))



Data_Supplemental_CoD_analysis$Sex <- factor(Data_Supplemental_CoD_analysis$Sex, levels = c(1,2),
                              labels = c("Males","Females"))



######################
# Data SM Violence
######################

Data_SMFig_Violence <- Data_Supplemental_CoD_analysis %>%
  filter(Period=="2015-2019" | Period=="2019-2020" | Period=="2020-2021") %>%
  filter(Groups=="Homicides & Violence") %>%
  group_by(Sex, Period, Region) %>%
  mutate(Total_violence = sum(Contribution),
         Entidad_order = fct_reorder(ENTIDAD, Total_violence))

######################
# Data SM COVID
######################

Data_SMFig_COVID <- Data_Supplemental_CoD_analysis %>%
  filter(Period=="2019-2020" | Period=="2020-2021") %>%
  filter(Groups=="COVID") %>%
  group_by(Sex, ENTIDAD, Region, Groups, Period) %>%
  summarize(Contribution = sum(Contribution))

######################
# Data SM Diabetes
######################

Data_SMFig_Diabetes <- Data_Supplemental_CoD_analysis %>%
  filter(Period=="2015-2019" | Period=="2019-2020" | Period=="2020-2021") %>%
  filter(Groups=="Diabetes") %>%
  group_by(Sex, ENTIDAD, Region, Groups, Period) %>%
  summarize(Contribution = sum(Contribution))

######################
# Data SM Amenable causes
######################

Data_SMFig_Amenable <- Data_Supplemental_CoD_analysis %>%
  filter(Period=="2015-2019" | Period=="2019-2020" | Period=="2020-2021") %>%
  filter(Groups=="Amenable diseases") %>%
  group_by(Sex, ENTIDAD, Region, Groups, Period) %>%
  summarize(Contribution = sum(Contribution))




# ---------------------------------------------------------------------------- #
#       3. Supplemental Figures on Causes of Death
# ---------------------------------------------------------------------------- #

# ----------------------------------------
# SM 3 The contribution of homicides and violence to life expectancy changes in
#       different periods by state and sex, 2015–2021.
# ----------------------------------------

Figure_SM_3 <- ggplot(Data_SMFig_Violence,
                    aes(x=reorder(ENTIDAD, -Contribution),
                        y=Contribution,
                        color=Sex)) +
  geom_point(aes(shape=Sex, color=Sex), size=3) +
  facet_grid(Region ~ Period, scales = "free") +
  scale_shape_manual(values=c(15,2))+
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", size= 1.5) +
  theme_classic() +
  scale_color_manual(values = c("red4", "red4")) +
  theme(text = element_text(size = 16),
        legend.position="bottom",
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 45),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  labs(shape = "Sex",
       color="Sex",
       y= bquote("Contribution to life expectancy change" ~e[0]~ "(in years)"),
       x="")
Figure_SM_3
ggsave(filename = "Supplemental Figure 3.png",
       path = here::here("Figures/Supplementary Figures/"),
       dpi = 320, width = 8, height = 9,
       bg = "transparent")


# ----------------------------------------
# SM 4 The contribution of COVID-19 to life expectancy changes in
#       different periods by state and sex, 2019–2021.
# ----------------------------------------


Figure_SM_4 <- ggplot(Data_SMFig_COVID,
                    aes(x=reorder(ENTIDAD, -Contribution),
                        y=Contribution,
                        color=Sex)) +
  geom_point(aes(shape=Sex, color=Sex), size=3) +
  facet_grid(Region ~ Period, scales = "free") +
  scale_shape_manual(values=c(15,2))+
  coord_flip() +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed", size= 1.5) +
  scale_color_manual(values = c("paleturquoise3", "paleturquoise3")) +
  theme(text = element_text(size = 16),
        legend.position="bottom",
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 45),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  labs(shape = "Sex",
       y= bquote("Contribution to life expectancy change" ~e[0]~ "(in years)"),
       x="")
Figure_SM_4
ggsave(filename = "Supplemental Figure 4.png",
       path = here::here("Figures/Supplementary Figures/"),
       dpi = 320, width = 8, height = 9,
       bg = "transparent")

# ----------------------------------------
# SM 5 The contribution of diabetes to life expectancy changes in
#       different periods by state and sex, 2015–2021.
# ----------------------------------------

Figure_SM_5 <- ggplot(Data_SMFig_Diabetes,
                    aes(x=reorder(ENTIDAD, -Contribution),
                        y=Contribution,
                        color=Sex)) +
  geom_point(aes(shape=Sex, color=Sex), size=3) +
  facet_grid(Region ~ Period, scales = "free") +
  scale_shape_manual(values=c(15,2))+
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", size= 1.5) +
  theme_classic() +
  scale_color_manual(values = c("#FB6A4A", "#FB6A4A")) +
  theme(text = element_text(size = 16),
        legend.position="bottom",
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 0),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  #guides(fill=guide_legend(ncol=4)) +
  labs(shape = "Sex",
       y= bquote("Contribution to life expectancy change" ~e[0]~ "(in years)"),
       x="")
Figure_SM_5
ggsave(filename = "Supplemental Figure 5.png",
       path = here::here("Figures/Supplementary Figures/"),
       dpi = 320, width = 8, height = 9,
       bg = "transparent")

# ----------------------------------------
# SM 6 The contribution of causes amenable to healthcare to life expectancy changes in
#       different periods by state and sex, 2015–2021.
# ----------------------------------------

Figure_SM_6 <- ggplot(Data_SMFig_Amenable,
                      aes(x=reorder(ENTIDAD, -Contribution),
                          y=Contribution,
                          color=Sex)) +
  geom_point(aes(shape=Sex, color=Sex), size=3) +
  facet_grid(Region ~ Period, scales = "free") +
  scale_shape_manual(values=c(15,2))+
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", size= 1.5) +
  theme_classic() +
  scale_color_manual(values = c("#8C96C6", "#8C96C6")) +
  theme(text = element_text(size = 16),
        legend.position="bottom",
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 45),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  #guides(fill=guide_legend(ncol=4)) +
  labs(shape = "Sex",
       y= bquote("Contribution to life expectancy change" ~e[0]~ "(in years)"),
       x="")
Figure_SM_6
ggsave(filename = "Supplemental Figure 6.png",
       path = here::here("Figures/Supplementary Figures/"),
       dpi = 320, width = 8, height = 9,
       bg = "transparent")



