library(tidyverse)
library(ggplot2)
library(ggpubr)
library(sf)
library(lubridate)
library(vroom)
library(corrplot)
library(extrafont)
options(scipen = 999)

font_import()
loadfonts(device = "win", quiet = TRUE)
windowsFonts(Times = windowsFont("Times New Roman"))

fonts()
# 1. Data -----------------------------------------------------------------

## COVID-19 Cases --------
cv <- read.csv("Input Data/UK_CovidRates_Updated.csv.csv")
cvDay <- cv %>%
  mutate(Date = as.character(Date)) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>% 
  select(Date, newCases) %>%
  arrange(Date)
cvWeek <- cv %>% 
  mutate(Date = as.character(Date)) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>% 
  mutate(WeekDate = floor_date(Date - 1, "weeks") + 1) %>%
  select(Date, WeekDate, newCases) %>%
  arrange(Date) %>%
  group_by(WeekDate) %>%
  summarise(weekCases = sum(newCases)) %>%
  mutate(weekCases = weekCases / 100000) %>%
  filter(WeekDate >= "2021-09-06" & WeekDate <= "2022-06-27")


## Geolytix Mobility Data ---------

## Read in and prepare daily measure
dat <- read.csv("Input Data/MobilityAggregates.csv.csv")
dat <- dat %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) 

## Retail Centre Data -------------

### Safeguarded indicators
ind <- read.csv("Input Data/CDRC_RetailCentre_Indicators.csv")
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


# 2. Data Analysis --------------------------------------------------------

## Calculating baseline of Ai
base <- input %>%
  mutate(WeekDate = floor_date(Date - 1, "weeks") + 1) %>%
  select(RC_ID, Classification, WeekDate, Ai) %>%
  group_by(WeekDate) %>%
  summarise(avgAi = mean(Ai)) %>%
  filter(WeekDate >= "2021-08-02" & WeekDate <= "2021-08-30") %>%
  summarise(avgAi = mean(avgAi))

## Calculate change from baseline
chg <- input %>%
  mutate(WeekDate = floor_date(Date - 1, "weeks") + 1) %>%
  select(RC_ID, Classification, WeekDate, Ai) %>%
  group_by(WeekDate) %>%
  summarise(avgAi = mean(Ai)) %>%
  ungroup() %>%
  arrange(WeekDate) %>%
  mutate(AiChange = avgAi - base$avgAi,
         pctChange = (avgAi/base$avgAi - 1) * 100) %>%
  filter(WeekDate >= "2021-08-02")



# 3. Data Visualisation 1) COVID-19 and National Activity -----------------

p1 <- ggplot(cvWeek) +
  aes(x = WeekDate, y = weekCases) +
  geom_col() +
  ylab("Weekly COVID-19 cases (100,000s)") +
  xlab(NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  scale_y_continuous(breaks = seq(0, 12, by = 3)) +
  geom_vline(xintercept = as.Date("2021-11-27"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-02-14"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-05-20"), color = "red", lwd = 2) +
  geom_label(aes(x = as.Date("2021-11-20"), y = 7.5,
                 label = "27th November:\nOmicron BA.1 detected",
                 size = 4, family = "Times")) +
  geom_label(aes(x = as.Date("2022-02-14"), y = 7.5,
                 label = "14th February:\nOmicron BA.2 detected",
                 size = 4, family = "Times")) +
  geom_label(aes(x = as.Date("2022-05-20"), y = 7.5,
                 label = "20th May:\nOmicron BA.4 and BA.5\ndeclared as VOC",
                 size = 4, family = "Times")) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "none")


p2<- chg %>%
  filter(WeekDate >= "2021-09-06") %>%
  ggplot() +
  aes(x = WeekDate, y = pctChange) +
  geom_col() +
  ylab("Weekly change in Ai from baseline (%)") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  scale_y_continuous(breaks = seq(-60, 20, by = 10)) +
  geom_vline(xintercept = as.Date("2021-11-27"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-02-14"), color = "red", lwd = 2) +
  geom_vline(xintercept = as.Date("2022-05-20"), color = "red", lwd = 2) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "none")

ggarrange(p1, p2, nrow = 2, labels = c("A", "B"))
