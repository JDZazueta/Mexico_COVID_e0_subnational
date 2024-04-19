################################################################################
# Article: Mexico’s surge of violence and COVID-19 drive life expectancy losses 2015–2021
# Title:   Mexico region map
# Authors: Daniel, Paola, Maria & José Manuel
################################################################################

pacman::p_load(here, sp, ggplot2, grid, devtools)
pacman::p_load_gh("diegovalle/mxmaps") # maps package from Diego Valle Jones is on github

# Create a new vector to fill with our regions
df_mxstate$region2 <- 0

# Define the regions
df_mxstate$region2[c(3, 19, 25, 24, 26, 5, 2, 28, 10, 32, 8)] <- "1. North"
df_mxstate$region2[c(9, 22, 13, 15, 14, 18, 6, 16, 29, 1, 11)] <- "2. Central"
df_mxstate$region2[c(17, 23, 21, 12, 31, 4, 20, 30, 27, 7)] <- "3. South"

# Assign our regions as the plotting value
df_mxstate$value <- df_mxstate$region2

# Basic choropleth
M <- mxstate_choropleth(df_mxstate, num_colors = 3)

# Aesthetic parameters
M$theme$legend.text$size <- 10
q <- unit(0.8, "lines")
M$theme$legend.key.size <- q
M$theme$legend.position <- "bottom"
M$theme$legend.title <- element_blank()

colors <- c("1. North" = "#8C96C6", "2. Central" = "red4", "3. South" = "paleturquoise3")

M2 <- M + scale_colour_manual(values = colors,
                              breaks = c("1. North", "2. Central", "3. South"),
                              labels = c("North", "Central", "South"),
                              aesthetics = c("colour", "fill"))

M2
ggsave(filename = "Region map.png",
       path = here::here("Figures/Supplementary Figures/"),
       dpi = 320, width = 8, height = 9,
       bg = "transparent")

