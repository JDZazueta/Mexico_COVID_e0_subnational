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
source("R Code/Final/00. Functions for analysis.R") 

# ---------------------------------------------------------------------------- #
#  1. Open datasets
# ---------------------------------------------------------------------------- #


# --------------------------------
# Coneval
# --------------------------------

CONEVAL <- read.csv("R Code/Data/CONEVAL/Indicadores_poverty_2016_2022.csv", header = T)

table(CONEVAL$ent)


CONEVAL_data <- CONEVAL %>% 
  mutate(ent = recode(ent,
                      "Ciudad de M\xe9xico" = "Mexico City",
                      "Michoac\xe1n de Ocampo" = "Michoacan",
                      "Coahuila de Zaragoza" = "Coahulia",
                      "M\xe9xico" = "Mexico",
                      "San Luis Potos\xed" = "San Luis Potosi",
                      "Nuevo Le\xf3n" = "Nuevo Leon",
                      "Quer\xe9taro" = "Queretaro",
                      "Veracruz de Ignacio de la Llave" = "Veracruz",
                      "Yucat\xe1n" = "Yucatan")) %>% 
  rename(ENTIDAD = ent) %>% 
  dplyr::select(ENTIDAD, sexo, year, pobreza, carencias) %>% 
  pivot_wider(names_from = year,
              values_from = c(pobreza, carencias)) %>% 
  mutate(pobreza_t1 = (pobreza_2016 + pobreza_2018 + pobreza_2020)/3,
         pobreza_t2 = pobreza_2020,
         pobreza_t3 = (pobreza_2020 + pobreza_2022)/2,
         carencias_t1 = (carencias_2016 + carencias_2018 + carencias_2020)/3,
         carencias_t2 = carencias_2020,
         carencias_t3 = (carencias_2020 +  carencias_2022)/2) %>% 
  dplyr::select(ENTIDAD,  sexo,
                pobreza_t1, pobreza_t2, pobreza_t3,
                carencias_t1, carencias_t2, carencias_t3) %>% 
  pivot_longer(!c(ENTIDAD, sexo),
               names_to = "Variable",
               values_to = "Value") %>% 
  mutate(Indicador = case_when((Variable=="pobreza_t1" | Variable=="pobreza_t2" | Variable=="pobreza_t3") ~ "Pobreza",
                               (Variable=="carencias_t1" | Variable=="carencias_t2" | Variable=="carencias_t3") ~ "Carencias"),
         Period = case_when(Variable=="pobreza_t1" | Variable=="carencias_t1"  ~ "2015-2019",
                            Variable=="pobreza_t2" | Variable=="carencias_t2"  ~ "2020",
                            Variable=="pobreza_t3" | Variable=="carencias_t3"  ~ "2020-2021"),
         Sex = case_when(sexo==1 ~ "Males",
                         sexo==2 ~ "Females")) %>% 
  dplyr::select(ENTIDAD, Sex, Indicador, Period, Value)  %>% 
  pivot_wider(names_from = Indicador,
              values_from = Value) %>% 
  mutate(Region = case_when(ENTIDAD=="Chihuahua" | ENTIDAD=="Sinaloa" |
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


# ---------------------------------------------------------------------------- #
#  2. Figures
# ---------------------------------------------------------------------------- #


CONEVAL_data$Region <- factor(CONEVAL_data$Region,
                              levels = c(1,2,3),
                              labels = c("North", 
                                         "Central", 
                                         "South"))


Figure_1 <- ggplot(CONEVAL_data, mapping = aes(x=reorder(ENTIDAD,-Pobreza),
                                               y=Pobreza*100,
                                               color=Sex))+
  geom_point(aes(shape=Sex, color=Sex), size=3) +
  facet_grid(Region~Period, scales = "free") +
  coord_flip() +
  theme_classic() +
  scale_color_manual(values = c("grey40", "red4")) +
  theme(text = element_text(size = 16), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        #legend.background = element_rect(fill="transparent", 
        #                                 size=1, linetyarrangepe="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 0),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  #guides(fill=guide_legend(ncol=4)) +
  labs(#title = bquote(~'A) Violence contribution to ∆ '~ e[0] ~'2000-2021, Mexico' ),
    #title = "A) Males",
    #shape = "Period",
    #color="Region",
    y= "Poverty levels (%)",
    x="")
Figure_1
ggsave(filename = "Supplemental Figure 13.png",
       path= "Figures/Supplementary material/TLGH/CONEVAL/",
       dpi = 320, width = 8, height = 9,
       bg = "transparent")



Figure_2 <- ggplot(CONEVAL_data, mapping = aes(x=reorder(ENTIDAD,-Carencias),
                                               y=Carencias*100,
                                               color=Sex))+
  geom_point(aes(shape=Sex, color=Sex), size=3) +
  facet_grid(Region~Period, scales = "free") +
  coord_flip() +
  theme_classic() +
  scale_color_manual(values = c("grey40", "red4")) +
  theme(text = element_text(size = 16), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        #legend.background = element_rect(fill="transparent", 
        #                                 size=1, linetyarrangepe="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 0),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  #guides(fill=guide_legend(ncol=4)) +
  labs(#title = bquote(~'A) Violence contribution to ∆ '~ e[0] ~'2000-2021, Mexico' ),
    #title = "A) Males",
    #shape = "Period",
    #color="Region",
    y= "Social vulnerability (%)",
    x="")
Figure_2
ggsave(filename = "Supplemental Figure 14.png",
       path= "Figures/Supplementary material/TLGH/CONEVAL/",
       dpi = 320, width = 8, height = 9,
       bg = "transparent")











