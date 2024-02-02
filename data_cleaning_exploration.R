#Data cleaning and exploration 

library(here)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(plyr)
#_______________________________________________________________________________
dt1 <- read.csv(here("Data","Lobster_Abundance_All_Years_20240117.csv"))
#_______________________________________________________________________________
# Package ID: knb-lter-sbc.77.8 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Abundance, size and fishing effort for California Spiny Lobster (Panulirus interruptus), ongoing since 2012.
# Data set creator:    - Santa Barbara Coastal LTER 
# Data set creator:  Daniel C Reed -  
# Data set creator:  Robert J Miller -  
# Contact:    - Information Manager, Santa Barbara Coastal LTER   - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#accessed this data from the SBC-LTER server (path: sbclter/internal/research/Reef/Working/Data/Lobster abundance and fishing pressure/Lobster_Abundance_All_Years_20240117)



# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$MONTH)!="factor") dt1$MONTH<- as.factor(dt1$MONTH)                                   
# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE) 
if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$TRANSECT)!="factor") dt1$TRANSECT<- as.factor(dt1$TRANSECT)
if (class(dt1$REPLICATE)!="factor") dt1$REPLICATE<- as.factor(dt1$REPLICATE)
if (class(dt1$SIZE_MM)=="factor") dt1$SIZE_MM <-as.numeric(levels(dt1$SIZE_MM))[as.integer(dt1$SIZE_MM) ]               
if (class(dt1$SIZE_MM)=="character") dt1$SIZE_MM <-as.numeric(dt1$SIZE_MM)
if (class(dt1$COUNT)=="factor") dt1$COUNT <-as.numeric(levels(dt1$COUNT))[as.integer(dt1$COUNT) ]               
if (class(dt1$COUNT)=="character") dt1$COUNT <-as.numeric(dt1$COUNT)
if (class(dt1$NUM_AO)=="factor") dt1$NUM_AO <-as.numeric(levels(dt1$NUM_AO))[as.integer(dt1$NUM_AO) ]               
if (class(dt1$NUM_AO)=="character") dt1$NUM_AO <-as.numeric(dt1$NUM_AO)
if (class(dt1$AREA)=="factor") dt1$AREA <-as.numeric(levels(dt1$AREA))[as.integer(dt1$AREA) ]               
if (class(dt1$AREA)=="character") dt1$AREA <-as.numeric(dt1$AREA)

# Convert Missing Values to NA for non-dates

dt1$SIZE_MM <- ifelse((trimws(as.character(dt1$SIZE_MM))==trimws("-99999")),NA,dt1$SIZE_MM)               
suppressWarnings(dt1$SIZE_MM <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$SIZE_MM))==as.character(as.numeric("-99999"))),NA,dt1$SIZE_MM))
dt1$COUNT <- ifelse((trimws(as.character(dt1$COUNT))==trimws("-99999")),NA,dt1$COUNT)               
suppressWarnings(dt1$COUNT <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$COUNT))==as.character(as.numeric("-99999"))),NA,dt1$COUNT))
dt1$NUM_AO <- ifelse((trimws(as.character(dt1$NUM_AO))==trimws("-99999")),NA,dt1$NUM_AO)               
suppressWarnings(dt1$NUM_AO <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$NUM_AO))==as.character(as.numeric("-99999"))),NA,dt1$NUM_AO))
dt1$AREA <- ifelse((trimws(as.character(dt1$AREA))==trimws("-99999")),NA,dt1$AREA)               
suppressWarnings(dt1$AREA <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$AREA))==as.character(as.numeric("-99999"))),NA,dt1$AREA))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(YEAR)
summary(MONTH)
summary(DATE)
summary(SITE)
summary(TRANSECT)
summary(REPLICATE)
summary(SIZE_MM)
summary(COUNT)
summary(NUM_AO)
summary(AREA) 
# Get more details on character variables

summary(as.factor(dt1$MONTH)) 
summary(as.factor(dt1$SITE)) 
summary(as.factor(dt1$TRANSECT)) 
summary(as.factor(dt1$REPLICATE))
detach(dt1) 
#_______________________________________________________________________________

dt_clean <- dt1 %>%
  clean_names()

winter <- dt_clean %>%
  filter(month %in% c("11","12"))

summer <- dt_clean %>%
  filter(month %in% c("7","8","9"))

summer23 <- dt_clean %>%
  filter(month %in% c("7","8","9") & year == 2023)

#size (log10) 
ggplot(winter, aes(x = site, y = log10(size_mm))) +
  geom_boxplot(na.rm = TRUE) +
  ylim(1.25,2.75) + 
  theme_minimal()

ggplot(summer23, aes(x = site, y = log10(size_mm))) +
  geom_boxplot(na.rm = TRUE) +
  ylim(1.25,2.75) + 
  theme_minimal()

#size(not logged)
ggplot(winter, aes(x = site, y = size_mm)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(0,200) + 
  theme_minimal() +
  labs(title = "Winter 2023")

ggplot(summer23, aes(x = site, y = size_mm)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(0,200) + 
  theme_minimal() +
  labs(title = "Summer 2023")


#at first glance it's looking like there's not much difference between size for summer and winter of 2023

#abundance - think about best way to count lobsters 
summer_density <- summer %>%
  group_by(year, site, transect, replicate) %>%
  summarize(counts = sum(count)) %>%
  mutate(area_m2 = 300) %>%
  group_by(year, site) %>%
  summarize(counts = sum (counts), area_m2 = sum(area_m2)) %>%
  mutate(ind_m2 = counts/area_m2) %>%
  mutate(year = as.factor(year))

winter_density <- winter %>%
  group_by(year, site, transect, replicate) %>%
  summarize(counts = sum(count)) %>%
  mutate(area_m2 = 300) %>%
  group_by(year, site) %>%
  summarize(counts = sum (counts), area_m2 = sum(area_m2)) %>%
  mutate(ind_m2 = counts/area_m2) %>%
  mutate(year = as.factor(year))

###

ggplot(summer_density %>%
         filter(year == "2023"), aes(site, ind_m2, fill = year)) +
  geom_col(position = "dodge") +
  ylim(0, 0.05) +
  theme_minimal()

ggplot(summer_density %>%
         filter(year == "2022"), aes(site, ind_m2, fill = year)) +
  geom_col(position = "dodge") +
  ylim(0, 0.05) +
  theme_minimal()

ggplot(summer_density %>%
         filter(year == "2021"), aes(site, ind_m2, fill = year)) +
  geom_col(position = "dodge") +
  ylim(0, 0.05) +
  theme_minimal()

ggplot(summer_density %>%
         filter(year == "2020"), aes(site, ind_m2, fill = year)) +
  geom_col(position = "dodge") +
  theme_minimal()

ggplot(summer_density %>%
         filter(year == "2019"), aes(site, ind_m2, fill = year)) +
  geom_col(position = "dodge") +
  theme_minimal()

ggplot(summer_density %>%
         filter(year == "2018"), aes(site, ind_m2, fill = year)) +
  geom_col(position = "dodge") +
  theme_minimal()

ggplot(summer_density, aes(site, ind_m2)) +
  geom_col(position = "dodge") +
  ylim(0, 0.05) +
  theme_minimal()

####
ggplot(winter_density %>%
         filter(year == "2023"), aes(site, ind_m2)) +
  geom_col(position = "dodge") +
  ylim(0, 0.05) +
  theme_minimal() +
  labs(title = "P. interruptus density in Nov-Dec 2023", x = "Site", y = "Ind. per m^2")

###next - look into how to plot size distributions 
#use a histogram

ggplot(dt_clean %>%
         filter(month %in% c("7","8","9"), year == 2023), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE)

ggplot(dt_clean %>%
         filter(month %in% c("11","12")), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE)

## size distributions by site 

pnap <- ggplot(dt_clean %>%
         filter(month %in% c("7","8","9"), site == "NAPL"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 5) +
  xlim(0, 250) +
  labs(title = "Naples")

ggplot(dt_clean %>%
         filter(month %in% c("7","8","9"), site == "IVEE"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 5) +
  xlim(0, 250) +
  labs(title = "Isla Vista")

ggplot(dt_clean %>%
         filter(month %in% c("7","8","9"), site == "CARP"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 5) +
  xlim(0, 250) +
  labs(title = "Carpinteria")

ggplot(dt_clean %>%
         filter(month %in% c("7","8","9"), site == "MOHK"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 5) +
  xlim(0, 250) +
  labs(title = "Mohawk")

ggplot(dt_clean %>%
         filter(month %in% c("7","8","9"), site == "AQUE"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 5) +
  xlim(0, 250) +
  labs(title = "Arroyo Quemado")

###

mu <- dt_clean %>% 
  na.omit() %>% 
  group_by(site) %>% 
  summarize(mean_size = mean(size_mm))

pnap + geom_vline(data=mu, aes(xintercept=grp.mean, color="red"),
                   linetype="dashed")









