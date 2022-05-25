## Methods Figure
library(sf)
library(h3jsr)
library(ggplot2)
library(extrafont)
library(tidyverse)
library(gridExtra)
library(cowplot)
#font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)

# 1. Data -----------------------------------------------------------------

## Read in the retail centre
rc <- st_read("Output Data/LL.gpkg")

## Polyfill
rc_h3 <- polyfill(rc, 11, simple = FALSE)
rc_h3 <- h3_to_polygon(unlist(rc_h3$h3_polyfillers), simple = FALSE)

## Assign random numbers of devices 
rc_map <- rc_h3
rc_map$RC_ID <- "RC_EW_2735"
rc_map$nDevices <- sample(40, size = nrow(rc_map), replace = TRUE)

## Calculate medianDevices
rc_avg <- rc_map %>%
  as.data.frame() %>%
  group_by(RC_ID) %>%
  summarise(totalDevices = sum(nDevices))

## Calculate medianDevicesH3
rc_avg_h3 <- rc_avg %>%
  mutate(medianDevicesH3 = totalDevices / nrow(rc_h3))
rc$medianDevicesH3 <- rc_avg_h3$medianDevicesH3

# 2. Mapping --------------------------------------------------------------

## Plot 1
n1 <- ggplot() +
  geom_sf(data = rc, fill = NA, col = "black", lwd = 1) +
  theme_bw() +
  labs(title = "A) Lodge Lane, Liverpool", 
       subtitle = "Retail centre boundary") +
  theme(text = element_text(family = "Times"),
        axis.text = element_blank(),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 11))
## Plot 2
n2 <- ggplot() +
  geom_sf(data = rc_h3, fill = "gray", alpha = 0.5, col = "gray", lwd = 0.5) +
  geom_sf(data = rc, fill = NA, col = "black", lwd = 1) +
  theme_bw() +
  labs(title = "B) Lodge Lane, Liverpool",
       subtitle = "Nested H3 cells",
       caption = "Comprises 16 H3 cells \n(resolution 11)") +
  theme(text = element_text(family = "Times"),
        axis.text = element_blank(),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 11))

## Plot 3 
n3 <- ggplot() +
  geom_sf(data = rc_map, aes(fill = nDevices), alpha = 0.8, lwd = 0) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  geom_sf(data = rc, fill = NA, col = "black", lwd = 1) +
  theme_bw() +
  labs(title = "C) Lodge Lane, Liverpool",
       subtitle = "H3 cell activity",
       caption = "Raw number of devices \n(generate randomly)") +
  theme(text = element_text(family = "Times"),
        axis.text = element_blank(),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 11))

## Plot 4
n4 <- ggplot() +
  geom_sf(data = rc, fill = "#fd8d3c", col = "black", lwd = 1, alpha = 0.75) +
  theme_bw() +
  labs(title = "D) Lodge Lane, Liverpool",
       subtitle = "Centre average activity",
       caption = "Average devices per cell \n= 17.8") +
  theme(text = element_text(family = "Times"),
        axis.text = element_blank(),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 11))

legend <- get_legend(n3)
n3 <- n3 +
  theme(legend.position = "none")
grid.arrange(n1, n2, n3, legend, n4, nrow = 1)
