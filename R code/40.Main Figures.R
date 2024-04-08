################################################################################
# Article: Mexico’s surge of violence and COVID-19 drive life expectancy losses 2015–2021
# Title:   Main Figures 
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
#  1. Open datasets
# ---------------------------------------------------------------------------- #

# - Decomposition
get(load("Data/Final/Decomp_e0_results_state.RData"))

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
# ------------------
# Prepare data Figure 1
# ------------------

Data_Figure_1 <- Decomp_e0_results %>% 
  mutate(Age_10 =case_when(Age>=0 & Age<=9 ~ 0,
                           Age>=10 & Age<=19 ~ 10,
                           Age>=20 & Age<=29 ~ 20,
                           Age>=30 & Age<=39 ~ 30,
                           Age>=40 & Age<=49 ~ 40,
                           Age>=50 & Age<=59 ~ 50,
                           Age>=60 & Age<=69 ~ 60,
                           Age>=70 & Age<=79 ~ 70,
                           Age>=80 & Age<=89 ~ 80,
                           Age>=90~ 90),
         Sex = case_when(SEX=="MALES" ~ 1,
                         SEX=="FEMALES" ~ 2)) %>% 
  dplyr::select(-Age, -SEX) %>% 
  rename(Age = Age_10) %>% 
  group_by(Period, Sex, Age, Cause, ENTIDAD) %>% 
  summarize(Contribution = sum(Contribution)) %>% 
  mutate(Groups = case_when(Cause=="COVID" ~ 1,
                            Cause=="Diabetes" ~ 2,
                            Cause=="Digestive" | Cause=="Infectious diseases" |
                              Cause=="Neoplasm" | Cause=="Perinatal" |
                              Cause=="Circulatory"  | Cause=="Respiratory" ~ 3,
                            Cause=="Homicides & Violence" ~ 4, 
                            Cause=="Other externals" ~ 5,
                            Cause=="Rest of causes" ~ 6)) %>% 
  group_by(Age, Sex, Period, Groups, ENTIDAD) %>% 
  summarize(Contribution = sum(Contribution)) %>% 
  mutate(Total_Age = sum(Contribution)) %>% 
  filter(Period=="2015-2019" |
           Period=="2019-2020" |
           Period=="2020-2021")  %>% 
  filter(ENTIDAD=="National")


Data_Figure_1$Sex <- factor(Data_Figure_1$Sex, levels = c(1,2),
                            labels = c("Males","Females"))

Data_Figure_1$Age <- factor(Data_Figure_1$Age, levels = c(seq(0,90,10)),
                            labels = c("0-9","10-19", 
                                       "20-29", "30-39",
                                       "40-49", "50-59",
                                       "60-69", "70-79",
                                       "80-89", "95 +"))

Data_Figure_1$Groups <- factor(Data_Figure_1$Groups,
                               levels = c(1,2,3,4,5,6),
                               labels = c("COVID", "Diabetes", "Amenable diseases",
                                          "Homicides & Violence",
                                          "External causes", "All other causes"))


# ------------------
# Prepare data Figure 2-5
# ------------------


Data_figure_2_5 <- Decomp_e0_results %>% 
  filter(ENTIDAD!="National") %>% 
  mutate(Sex = case_when(SEX=="MALES" ~ 1,
                         SEX=="FEMALES" ~ 2),
         Groups = case_when(Cause=="COVID" ~ 1,
                            Cause=="Diabetes" ~ 2,
                            Cause=="Digestive" | Cause=="Infectious diseases" |
                              Cause=="Neoplasm" | Cause=="Perinatal" |
                              Cause=="Circulatory"  | Cause=="Respiratory" ~ 3,
                            Cause=="Homicides & Violence" ~ 4, 
                            Cause=="Other externals" ~ 5,
                            Cause=="Rest of causes" ~ 6)) %>% 
  dplyr::select(-SEX) %>% 
  group_by(Period, Sex, Groups, ENTIDAD) %>% 
  summarise(Contribution=sum(Contribution)) %>% 
  group_by(Period, Sex, ENTIDAD) %>% 
  mutate(Total = sum(Contribution)) %>% 
  ungroup() %>% 
  mutate(Relative = Contribution/Total*100,
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

# - Labels Figure 2

Data_figure_2_5$Sex <- factor(Data_figure_2_5$Sex, levels = c(1,2),
                              labels = c("Males","Females"))

Data_figure_2_5$Groups <- factor(Data_figure_2_5$Groups,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c("COVID", "Diabetes", "Amenable diseases",
                                            "Homicides & Violence",
                                            "External causes", "All other causes"))


Data_figure_2_5$Region <- factor(Data_figure_2_5$Region,
                                 levels = c(1,2,3),
                                 labels = c("North", 
                                            "Central", 
                                            "South"))


######################
# Data figure 2
######################

Data_fig_2 <- Data_figure_2_5 %>% 
  filter(Period=="2015-2021") %>% 
  filter(Groups=="Homicides & Violence") %>% 
  group_by(Sex, Period, Region) %>% 
  mutate(Total_violence = sum(Contribution),
         Entidad_order = fct_reorder(ENTIDAD, Total_violence))  

######################
# Data figure 3
######################

Data_fig_3 <- Data_figure_2_5 %>% 
  filter(Period=="2019-2021") %>%
  filter(Groups=="COVID") %>% 
  group_by(Sex, ENTIDAD, Region, Groups, Period) %>% 
  summarize(Contribution = sum(Contribution))  

######################
# Data figure 4
######################

Data_fig_4 <- Data_figure_2_5 %>% 
  filter(Period=="2019-2021") %>%
  filter(Groups=="Diabetes") %>% 
  group_by(Sex, ENTIDAD, Region, Groups, Period) %>% 
  summarize(Contribution = sum(Contribution))  

######################
# Data figure 5
######################

Data_fig_5 <- Data_figure_2_5 %>% 
  filter(Period=="2015-2021") %>% 
  filter(Groups=="Amenable diseases") %>% 
  group_by(Sex, ENTIDAD, Region, Groups, Period) %>% 
  summarize(Contribution = sum(Contribution))  


# ---------------------------------------------------------------------------- #
#       3. Main Figures
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#  Figure 1. Cause-specific contributions to life-expectancy changes, by age, sex, 
#            and period, 2015–2021.
# ---------------------------------------------------------------------------- #

Data_Figure_1_total <- Data_Figure_1 %>% 
  group_by(Sex, Period) %>% 
  summarise(Contribution=sum(Contribution))

# Text Figure 1
Data_MEX_label_10x1 <- data.frame(Contribution=c(-0.25, -1, -0.11,
                                                 -0.2, -1, -0.11),
                                  Age = c(9,3,7,
                                          9,4,2),
                                  Sex = c("Males", "Males", "Males",
                                          "Females", "Females", "Females"),
                                  Groups = c("COVID","COVID","COVID",
                                             "COVID","COVID","COVID"),
                                  Period = c("2015-2019", "2019-2020", "2020-2021",
                                             "2015-2019", "2019-2020", "2020-2021"),
                                  Text = c("Life expectancy change:", 
                                           "Life expectancy change:", 
                                           "Life expectancy change:",
                                           "Life expectancy change:", 
                                           "Life expectancy change:", 
                                           "Life expectancy change:"))
# Text Figure 1
Data_MEX_label_10x1_2 <- data.frame(Contribution=c(-0.25, -1, -0.11,
                                                   -0.2, -1, -0.11),
                                    Age = c(8.5,2.5,6.5,
                                            8.5,3.5,1.5),
                                    Sex = c("Males", "Males", "Males",
                                            "Females", "Females", "Females"),
                                    Groups = c("COVID","COVID","COVID",
                                               "COVID","COVID","COVID"),
                                    Period = c("2015-2019", "2019-2020", "2020-2021",
                                               "2015-2019", "2019-2020", "2020-2021"),
                                    Text = c("-0.76y", 
                                             "-5.28y", 
                                             " 0.14y",
                                             " 0.09y", 
                                             "-3.53y", 
                                             "-0.43y"))



Figure_1 <- ggplot(Data_Figure_1, 
                   aes(x=Age, y=Contribution,
                       fill=Groups, pattern=Groups)) +
  geom_bar(stat = "identity", position = "stack",
           colour="black", alpha=0.7) +
  facet_grid(factor(Sex)~factor(Period), scales = "free")+
  coord_flip() +
  geom_text(data = Data_MEX_label_10x1, aes(label = Text), size = 3.7,
            position = position_dodge(width =  1), color="black") +
  geom_text(data = Data_MEX_label_10x1_2, aes(label = Text), size = 3.7,
            position = position_dodge(width =  1), color="black") +
  theme_classic() +
  scale_fill_manual(values = Color_big_groups) +
  theme(text = element_text(size = 16), 
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.9, "cm"),
        legend.key.width = unit(.9,"cm"),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=4)) +
  labs(fill = "Causes of death",
       y= bquote("Life expectancy change" ~e[0]~ "(in years)"),
       x="Age groups")
Figure_1
ggsave(filename = "Figure 1.png",
       path= "Figures/Main Figures/TLGH/",
       dpi = 320, width = 10, height = 8,
       scale=2.5,
       units = "cm",
       bg = "transparent")


# ---------------------------------------------------------------------------- #
#  Figure 2. The contribution of homicides and violence to life expectancy changes between 2015–2021, by state and sex.
# ---------------------------------------------------------------------------- #

Figure_2 <- ggplot(Data_fig_2,
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
        axis.text.x = element_text(angle = 0),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  labs(shape = "Sex",
       color="Sex",
       y= bquote("Contribution to life expectancy change" ~e[0]~ "(in years)"),
       x="")
Figure_2
ggsave(filename = "Figure 2.png",
       path= "Figures/Main Figures/TLGH/",
       dpi = 320, width = 8, height = 9,
       bg = "transparent")


# ---------------------------------------------------------------------------- #
#  Figure 3. The contribution of COVID-19 to life expectancy changes between 2015–2021, by state and sex.
# ---------------------------------------------------------------------------- #

Figure_3 <- ggplot(Data_fig_3,
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
        axis.text.x = element_text(angle = 0),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  labs(shape = "Sex",
       y= bquote("Contribution to life expectancy change" ~e[0]~ "(in years)"),
       x="")
Figure_3
ggsave(filename = "Figure 3.png",
       path= "Figures/Main Figures/TLGH/",
       dpi = 320, width = 8, height = 9,
       bg = "transparent")

# ---------------------------------------------------------------------------- #
#  Figure 4. The contribution of diabetes to life expectancy changes between 2015–2021, by state and sex.
# ---------------------------------------------------------------------------- #

Figure_4 <- ggplot(Data_fig_4,
                   aes(x=reorder(ENTIDAD, -Contribution), 
                       y=Contribution,
                       color=Sex)) +
  geom_point(aes(shape=Sex, color=Sex), size=3) +
  facet_grid(Region ~ Period, scales = "free") + 
  scale_shape_manual(values=c(15,2))+
  coord_flip() +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed", size= 1.5) +
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
  labs(shape = "Sex",
       y= bquote("Contribution to life expectancy change" ~e[0]~ "(in years)"),
       x="")
Figure_4
ggsave(filename = "Figure 4.png",
       path= "Figures/Main Figures/TLGH/",
       dpi = 320, width = 8, height = 9,
       bg = "transparent")

# ---------------------------------------------------------------------------- #
#  Figure 5. The contribution of causes amenable to healthcare to life expectancy changes between 2015–2021, by state and sex.
# ---------------------------------------------------------------------------- #


Figure_5 <- ggplot(Data_fig_5,
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
        axis.text.x = element_text(angle = 0),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  labs(y= bquote("Contribution to life expectancy change" ~e[0]~ "(in years)"),
       x="")
Figure_5
ggsave(filename = "Figure 5.png",
       path= "Figures/Main Figures/TLGH/",
       dpi = 320, width = 8, height = 9,
       bg = "transparent")

