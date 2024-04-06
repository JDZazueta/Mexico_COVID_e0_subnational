################################################################################
# Article: Mexico’s surge of violence and COVID-19 drive life expectancy losses 2015–2021
# Title:   Supplemental Figures on trends in e0 and causes of death
# Authors: Daniel, Paola, Maria & José Manuel
# Data:    Proyecciones de poblacion CONAPO (2023), CONEVAL (2016-2018), and
#          Deaths from INEGI
################################################################################


# ---------------------------------------------------------------------------- #
#  0. Working directory and Packages
# ---------------------------------------------------------------------------- #

# To clear everything in R, before start the analysis and open functions
rm(list = ls())
source("R Code/Final/00. Functions for analysis.R") 

# ---------------------------------------------------------------------------- #
#  1. Open data sets
# ---------------------------------------------------------------------------- #

# - Data life expectancy trends
get(load("R Code/Data/Final/Mexico_e0_e65.RData"))

# - Data ASDR
get(load("R Code/Data/Final/MEX_ASMR.RData"))

# ---------------------------------------------------------------------------- #
#  2. Prepare data for figures
# ---------------------------------------------------------------------------- #

# ------------------------
#. Data Supplemental Figure 1
# ------------------------
Data_preSM_1 <- Mexico_e0_e65 %>% 
  filter(ENTIDAD_NAME!="República Mexicana") %>% 
  mutate(sexo = case_when(Sex=="MALES" ~ 1,
                          Sex=="FEMALES" ~ 2))



Data_preSM_1$ENTIDAD <- factor(Data_preSM_1$ENTIDAD, levels = c(seq(1,32,1)),
                                    labels = c("Aguascalientes", "Baja California", "Baja California Sur",
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


Data_SM_figure_1 <- Data_preSM_1 %>% 
  filter(Age==0) %>% 
  filter(YEAR==2015 | YEAR==2019 |  YEAR==2020 | YEAR==2021) %>% 
  pivot_wider(names_from = YEAR,
              values_from = ex) %>% 
  mutate(Change = `2021` - `2019`,
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
                              ENTIDAD=="Yucatan" | ENTIDAD=="Chiapas" ~ 3)) 


Data_SM_figure_1$sexo <- factor(Data_SM_figure_1$sexo,
                            levels = c(1,2),
                            labels = c("Males", "Females"))

Data_SM_figure_1$Region <- factor(Data_SM_figure_1$Region,
                                         levels = c(1,2,3),
                                         labels = c("North", 
                                                    "Central", 
                                                    "South"))

# -----------------------------------
#. Trends in causes of death
# -----------------------------------




# ---------------------------------------------------------------------------- #
#       3. Supplemental Figures on Trends
# ---------------------------------------------------------------------------- #

# ----------------------------------------
# Supplemental Figure 1. Life expectancy changes between 2019¬–2021, by sex, state, and region. 
# ----------------------------------------

Data_text_state <- data.frame(`2021`=c(65, 65),
                              `2019`=c(70, 70),
                              ENTIDAD = c("Yucatán","Campeche"),
                              sexo= c("Males", "Males"),
                              Region = c("South", "South"),
                              Text = c("2019", "2021"))

Figure_SM_1 <- ggplot(Data_SM_figure_1) +
  geom_segment( aes(x=reorder(ENTIDAD,  -`2021`), 
                    xend=ENTIDAD, 
                    y=`2019`, yend=`2021`), color="grey") +
  geom_point( aes(x=ENTIDAD, y=`2019`, color="2019"), 
              size=3 ) +
  geom_point( aes(x=ENTIDAD, y=`2021`, color="2021"), 
              size=3) +
  facet_grid(Region~sexo, scales ="free") +
  coord_flip() +
  theme_bw()+
  scale_color_manual(values = c("red4", "grey40")) +
  theme(text = element_text(size = 16), 
        legend.position=c(.10, 0.75),
        #legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 45),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=4)) +
  labs(color = "Year",
       y= bquote("Life expectancy at birth " ~ e[0] ~ ""), 
       x="")
Figure_SM_1
ggsave(filename = "Supplemental Figure 1.png",
       path= "Figures/Supplementary material/TLGH/Trends/",
       dpi = 320, width = 10, height = 10,
       bg = "transparent")



# ----------------------------------------
# Supplemental Figure 6. Life expectancy losses between 2019¬–2021, by sex, state, and region. 
# ----------------------------------------

Figure_SM_6 <- ggplot(Data_SM_figure_1,
                      mapping = aes(x=reorder(ENTIDAD,-Change),
                       y=Change)) +
  geom_point(size = 3.5) +
  facet_grid(Region~sexo, scales ="free") +
  coord_flip() +
  theme_bw()+
  theme(text = element_text(size = 16), 
        legend.position=c(.10, 0.75),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 0),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=4)) +
  labs(color = "Year",
       y= bquote("Losses in fife expectancy at birth " ~ e[0] ~ ""), 
       x="")
Figure_SM_6
ggsave(filename = "Supplemental Figure 6.png",
       path= "Figures/Supplementary material/TLGH/Trends/",
       dpi = 320, width = 10, height = 12,
       bg = "transparent")



# ----------------------------------------
# Supplemental Figure 7. Life expectancy trends by state
# ----------------------------------------

Data_SM_7 <- Mexico_e0_e65 %>% 
  #filter(ENTIDAD_NAME!="República Mexicana") %>% 
  mutate(sexo = case_when(Sex=="MALES" ~ 1,
                          Sex=="FEMALES" ~ 2)) %>% 
  filter(Age==0)

Data_SM_7$sexo <- factor(Data_SM_7$sexo,
                                levels = c(1,2),
                                labels = c("Males", "Females"))


Data_SM_7$ENTIDAD <- factor(Data_SM_7$ENTIDAD, levels = c(seq(0,32,1)),
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


Figure_SM_7 <- ggplot(Data_SM_7,
                      mapping = aes(x=YEAR,
                                    y=ex, color=sexo)) +
  geom_line(size = 2.5) +
  facet_wrap(.~ENTIDAD, ncol=5) +
  theme_classic()+
  scale_x_continuous(breaks = c(seq(1999,2021,10))) +
  scale_color_manual(values = c("#762A83","#E7D4E8")) +
  theme(text = element_text(size = 16), 
        #legend.position="right",
        legend.position=c(.80, 0.03),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 25),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=4)) +
  labs(color = "Sex",
       y= bquote("Life expectancy at birth " ~ e[0] ~ ""), 
       x="Year")
Figure_SM_7
ggsave(filename = "Supplemental Figure 7.png",
       path= "Figures/Supplementary material/TLGH/Trends/",
       dpi = 320, width = 10, height = 12,
       bg = "transparent")




################################################################################
#       Age-standardized trends
################################################################################

# ----------------------------------------
# Supplemental Figure 7. SDR trends
# ----------------------------------------

MEX_ASMR_national_states

Data_SM_8 <- MEX_ASMR_national_states %>% 
  #filter(ENTIDAD_NAME!="República Mexicana") %>% 
  mutate(sexo = case_when(SEX=="MALES" ~ 1,
                          SEX=="FEMALES" ~ 2)) 

Data_SM_8$sexo <- factor(Data_SM_8$sexo,
                         levels = c(1,2),
                         labels = c("Males", "Females"))


Data_SM_8$ENTIDAD <- factor(Data_SM_8$ENTIDAD, levels = c(seq(0,32,1)),
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


# -----------------------------
#. All Causes
# -----------------------------

Figure_SM_8 <- ggplot(Data_SM_8,
                      mapping = aes(x=Year,
                                    y=SDR, color=sexo)) +
  geom_line(size = 2.5) +
  facet_wrap(.~ENTIDAD, ncol=5) +
  theme_classic()+
  scale_x_continuous(breaks = c(seq(1999,2021,10))) +
  scale_color_manual(values = c("#762A83","#E7D4E8")) +
  theme(text = element_text(size = 16), 
        #legend.position="right",
        legend.position=c(.80, 0.03),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 25),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=1)) +
  labs(color = "Sex",
       y= "SDR per 100,000 deaths (all-causes)", 
       x="Year")
Figure_SM_8
ggsave(filename = "Supplemental Figure 8.png",
       path= "Figures/Supplementary material/TLGH/Trends/",
       dpi = 320, width = 10, height = 12,
       bg = "transparent")


# -----------------------------
#. Violence
# -----------------------------

Figure_SM_9 <- ggplot(Data_SM_8,
                      mapping = aes(x=Year,
                                    y=SDR_violence, color=sexo)) +
  geom_line(size = 2.5) +
  facet_wrap(.~ENTIDAD, ncol=5, scales = "free") +
  theme_classic()+
  scale_x_continuous(breaks = c(seq(1999,2021,10))) +
  scale_color_manual(values = c("indianred","darkred")) +
  theme(text = element_text(size = 16), 
        #legend.position="right",
        legend.position=c(.80, 0.03),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 25),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=4)) +
  labs(color = "Sex",
       y= "SDR per 100,000 deaths (Homicdes & Violences)", 
       x="Year")
Figure_SM_9
ggsave(filename = "Supplemental Figure 9.png",
       path= "Figures/Supplementary material/TLGH/Trends/",
       dpi = 320, width = 10, height = 12,
       bg = "transparent")



# -----------------------------
#. COVID-19
# -----------------------------

Figure_SM_10 <- ggplot(Data_SM_8,
                      mapping = aes(x=Year,
                                    y=SDR_violence, color=sexo)) +
  geom_line( size = 2.5) +
  facet_wrap(.~ENTIDAD, ncol=5, scales = "free") +
  theme_classic()+
  scale_x_continuous(breaks = c(seq(1999,2021,10))) +
  scale_color_manual(values = c("paleturquoise2","paleturquoise4")) +
  #scale_shape_manual(values=c("solid","dashed"))+
  theme(text = element_text(size = 16), 
        #legend.position="right",
        legend.position=c(.80, 0.03),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 25),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=4)) +
  labs(color = "Sex",
       y= "SDR per 100,000 deaths (COVID)", 
       x="Year")
Figure_SM_10
ggsave(filename = "Supplemental Figure 10.png",
       path= "Figures/Supplementary material/TLGH/Trends/",
       dpi = 320, width = 10, height = 12,
       bg = "transparent")


# -----------------------------
#. Diabetes
# -----------------------------

Figure_SM_11 <- ggplot(Data_SM_8,
                       mapping = aes(x=Year,
                                     y=SDR_diabetes, color=sexo)) +
  geom_line( size = 2.5) +
  facet_wrap(.~ENTIDAD, ncol=5, scales = "free") +
  theme_classic()+
  scale_x_continuous(breaks = c(seq(1999,2021,10))) +
  scale_color_manual(values = c("orange3","#FB6A4A")) +
  theme(text = element_text(size = 11), 
        #legend.position="right",
        legend.position=c(.80, 0.03),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 25),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=4)) +
  labs(color = "Sex",
       y= "SDR per 100,000 deaths (Diabetes)", 
       x="Year")
Figure_SM_11
ggsave(filename = "Supplemental Figure 11.png",
       path= "Figures/Supplementary material/TLGH/Trends/",
       dpi = 320, width = 10, height = 12,
       bg = "transparent")


# -----------------------------
#. Amenable causes
# -----------------------------

Figure_SM_12 <- ggplot(Data_SM_8,
                       mapping = aes(x=Year,
                                     y=SDR_amenable, color=sexo)) +
  geom_line( size = 2.5) +
  facet_wrap(.~ENTIDAD, ncol=5, scales = "free") +
  theme_classic()+
  scale_x_continuous(breaks = c(seq(1999,2021,10))) +
  scale_color_manual(values = c("purple3","#8C96C6")) +
  theme(text = element_text(size = 11), 
        #legend.position="right",
        legend.position=c(.80, 0.03),
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 25),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=4)) +
  labs(color = "Sex",
       y= "SDR per 100,000 deaths (causes amenable to healthcare)", 
       x="Year")
Figure_SM_12
ggsave(filename = "Supplemental Figure 12.png",
       path= "Figures/Supplementary material/TLGH/Trends/",
       dpi = 320, width = 10, height = 12,
       bg = "transparent")
