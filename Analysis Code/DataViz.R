library(tidyverse)
library(ggplot2)
library(ggpubr)
library(sf)
library(lubridate)
library(vroom)
library(corrplot)
library(extrafont)
library(qdapRegex)
options(scipen = 999)
class_lvls <- c("Regional Centre",
                "Major Town Centre",
                "Town Centre", 
                "District Centre",
                "Market Town",
                "Local Centre")
nuts_list <- c("London (England)",  "Wales", "Scotland",
               "North West (England)",  "South East (England)", 
               "South West (England)")
windowsFonts(Times = windowsFont("Times New Roman"))
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2")
source("Source Code/Helper Functions.R")

# 1. Data -----------------------------------------------------------------

## COVID-19 Cases --------
cv <- read.csv("Input Data/UK_CovidRates_v3.csv")
cvDay <- cv %>%
  select(date, newCasesBySpecimenDate) %>%
  setNames(c("Date", "newCases")) %>%
  mutate(Date = as.character(Date)) %>%
  mutate(Date = as.Date(Date)) %>% 
  select(Date, newCases) %>%
  filter(Date >= "2021-08-01" & Date <= "2022-07-31") %>%
  mutate(newCases = newCases / 1000,
         newCases = round(newCases, 0)) %>% 
  arrange(Date)
# cvWeek <- cv %>% 
#   mutate(Date = as.character(Date)) %>%
#   mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>% 
#   mutate(WeekDate = floor_date(Date - 1, "weeks") + 1) %>%
#   select(Date, WeekDate, newCases) %>%
#   arrange(Date) %>%
#   group_by(WeekDate) %>%
#   summarise(weekCases = sum(newCases)) %>%
#   mutate(weekCases = weekCases / 100000) %>%
#   filter(WeekDate >= "2021-09-06" & WeekDate <= "2022-06-27")

## Geolytix Mobility Data ---------

## Read in and prepare daily measure
dat <- vroom("Input Data/MobilityAggregates_v3.csv")
dat <- dat %>%
  mutate(Date = as.Date(Date))
datDate <- dat %>%
  select(Date) %>%
  distinct()

## Retail Centre Data -------------

### Safeguarded indicators
ind <- read.csv("Input Data/CDRC_RetailCentre_Indicators.csv")
indDesc <- ind %>%
  select(RC_ID, Classification)
ind <- ind %>%
  filter(!is.na(pctCloneTown)) %>%
  select(RC_ID, 
         n.LDC.2020, propComparison, propConvenience, propLeisure, propService,
         propChain, propIndependent, pctCloneTown, propVacant, PropStructuralVacant,
         propVacantChange, vulnerabilityIndex, onlineExposure, eResilience)

### Deprivation profiles
dep <- st_read("Input Data/CDRC_RetailCentre_WalkingDeprivation_v2.gpkg")
dep <- dep %>%
  as.data.frame() %>%
  select(RC_ID, AvgIMDScore)

## Assembling Final Dataset -----------

### Merging on all the retail centre & covid data
input <- merge(dat, ind, by = "RC_ID", all.x = TRUE)
input <- merge(input, dep, by = "RC_ID", all.x = TRUE)
input <- merge(input, cvDay, by = "Date", all.x = TRUE)

### Tidying up
input <- input %>%
  select(Date, RC_ID, Classification, Ai, newCases, 
         n.LDC.2020, propComparison, propConvenience, propService, propLeisure,
         propChain, propIndependent, pctCloneTown,
         propVacant, PropStructuralVacant, propVacantChange, 
         vulnerabilityIndex, onlineExposure, eResilience,
         AvgIMDScore) %>%
  drop_na()

### Attach region
rc <- st_read("Input Data/CDRC_RetailCentre_WalkingDeprivation_v2.gpkg")
rc <- rc %>%
  select(RC_ID, RC_Name, Classification, geom) %>%
  filter(Classification %in% class_lvls) %>%
  filter(grepl('London|Scotland|Wales|North West|North East|South East|South West|East Midlands|Yorkshire and The Humber', RC_Name)) %>%
  mutate(Region = RC_Name,
         Region = qdapRegex::rm_between(Region, '(', ')', extract = TRUE))
rc_df <- rc %>%
  as.data.frame() %>%
  select(RC_ID, Region) %>%
  mutate(Region = as.character(Region),
         Region = factor(Region, levels = c("London; England", "South East; England",
                                            "South West; England", "East Midlands; England",
                                            "North West; England", "North East; England",
                                            "Yorkshire and The Humber; England", "Wales", "Scotland")))

input <- merge(input, rc_df, by = "RC_ID", all.x = TRUE)
input <- input %>%
  filter(!is.na(Region)) %>%
  mutate(Region = gsub("\\;.*", "", Region)) %>%
  mutate(Region = factor(Region, levels = c("London", "South East",
                                            "South West", "East Midlands",
                                            "North West", "North East",
                                            "Yorkshire and The Humber", "Wales", "Scotland"))) %>%
  arrange(RC_ID, Date)


# 2. Data Visualisation 1) COVID-19 and National Activity -----------------

## Plot 1 - COVID cases over time
p1 <- ggplot(cvDay) +
  aes(x = Date, y = newCases) +
  geom_col() +
  ylab("Daily COVID-19 cases (1000s)") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  scale_y_continuous(breaks = seq(0, 300, by = 50)) +
  geom_vline(xintercept = as.Date("2021-11-27"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-02-14"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-05-20"), color = "red", lwd = 2) +
  geom_label(aes(x = as.Date("2021-11-20"), y = 150,
                 label = "27th November:\nOmicron BA.1 detected",
                 size = 4, family = "Times")) +
  geom_label(aes(x = as.Date("2022-02-14"), y = 150,
                 label = "14th February:\nOmicron BA.2 detected",
                 size = 4, family = "Times")) +
  geom_label(aes(x = as.Date("2022-05-20"), y = 150,
                 label = "20th May:\nOmicron BA.4 and BA.5\ndeclared as VOC",
                 size = 4, family = "Times")) +
  geom_segment(aes(x = as.Date("2021-12-06"), y= 275, xend = as.Date("2022-01-31"), yend = 275),
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  geom_label(aes(x = as.Date("2022-01-06"), y = 260,
                 label = "'Plan B' measures in effect",
                 size = 4, family = "Times")) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "none")
p1

## Plot 2 - Daily activity over time
p3 <- input %>%
  ggplot(aes(x = Date, y = Ai)) +
  geom_smooth(color = "black") +
  ylab(bquote(bold("A"["it"]))) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  geom_vline(xintercept = as.Date("2021-11-27"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-02-14"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-05-20"), color = "red", lwd = 2) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "none")
p3


## Assemble
ggarrange(p1, p3, nrow = 2)
ggsave("Outputs and Figures/Figure 2.tiff", width = 14, height = 12)

# 4. Data Visualisation 2) Functional Analyses and Regions ----------------------------

input %>%
  mutate(Classification = factor(Classification, levels = c("Regional Centre",
                                                            "Major Town Centre",
                                                            "Town Centre", 
                                                            "District Centre",
                                                            "Market Town",
                                                            "Local Centre"))) %>%
  ggplot(aes(x = Date, y = Ai, group = Classification, color = Classification)) +
  geom_smooth() +
  scale_color_manual(values = cbp2) +
  ylab(bquote(bold("A"["it"]))) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  geom_vline(xintercept = as.Date("2021-11-27"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-02-14"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-05-20"), color = "red", lwd = 2) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text.align = 0)
ggsave("Outputs and Figures/Figure 3.tiff", width = 14, height = 10)

input %>%
  mutate(Classification = factor(Classification, levels = c("Regional Centre",
                                                            "Major Town Centre",
                                                            "Town Centre", 
                                                            "District Centre",
                                                            "Market Town",
                                                            "Local Centre"))) %>%
  ggplot(aes(x = Date, y = Ai, color = Classification)) +
  geom_smooth() +
  scale_color_manual(values = cbp2) +
  ylab(bquote(bold("A"["it"])))  +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_vline(xintercept = as.Date("2021-11-27"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-02-14"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-05-20"), color = "red", lwd = 2) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text.align = 0) +
  facet_wrap(~ Region, scales = "free", ncol = 3)
ggsave("Outputs and Figures/Figure 4.tiff", width = 16, height = 14)

# 5. Data Visualisation 3) Structural Analysis ----------------------------

### Composition of centres
t1a <- getMajor("propLeisure")
t1b <- getMinor("propLeisure")

## Diversity of offer
t2a <- getMajor("propChain")
t2b <- getMinor("propChain")

### Existing struggles
t3a <- getMajor("PropStructuralVacant")
t3b <- getMinor("PropStructuralVacant")

ggarrange(t1a, t1b, t2a, t2b, t3a, t3b,
          nrow = 3, ncol = 2,
          labels = c("A", "B", "C","D", "E", "F"))
ggsave("Outputs and Figures/Figure 5.tiff", 
       width = 14, height = 12)


### Deprivation 
t4a <- getMajor("AvgIMDScore")
t4b <- getMinor("AvgIMDScore")

## e-Resilience
t5a <- getMajor("eResilience")
t5b <- getMinor("eResilience")

ggarrange(t4a, t4b, t5a, t5b,
          nrow = 2, ncol = 2,
          labels = c("A", "B", "C","D"))
ggsave("Outputs and Figures/Figure 6.tiff", 
       width = 14, height = 8)


