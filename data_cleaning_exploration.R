#Data cleaning and exploration 

library(here)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggupset) # https://github.com/const-ae/ggupset
library(hrbrthemes) # https://github.com/hrbrmstr/hrbrthemes
library(viridis)
library(janitor)
#library(plyr)
library(patchwork)
#_______________________________________________________________________________
#not sure why this isn't working: dt1 <- read.csv(here("Data","Lobster_Abundance_All_Years_20240117.csv"))
dt1 <- read.csv("/Users/julia/Documents/SBC-LTER/Spiny Lobster/lobster_surveys/Data/Lobster_Abundance_All_Years_20240117.csv")
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

#add a column for "legal" or "not legal" 
dt_clean <- dt_clean %>%
  mutate(status = cut(size_mm, breaks=c(-Inf, 82, Inf), labels=c("not_legal","legal")))

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
  mutate(year = as.factor(year)) %>%
  mutate(season = "summer")

winter_density <- winter %>%
  group_by(year, site, transect, replicate) %>%
  summarize(counts = sum(count)) %>%
  mutate(area_m2 = 300) %>%
  group_by(year, site) %>%
  summarize(counts = sum (counts), area_m2 = sum(area_m2)) %>%
  mutate(ind_m2 = counts/area_m2) %>%
  mutate(year = as.factor(year)) %>%
  mutate(season = "winter")

#combine summer and winter density data frames 
tot_density <- full_join(winter_density, summer_density)

### plots of density by site 2023 only 
density2023 <- ggplot(tot_density %>%
         filter(year == "2023"), aes(site, ind_m2, fill = season)) +
  geom_col(position = "dodge") +
  ylim(0, 0.08) +
  theme_minimal() +
  labs(title = "P. interruptus densities 2023", x = "Site", y = "Ind. per m^2") +
  scale_fill_manual(values=c("wheat3", "seagreen4"))

#save 2023 density plot 
ggsave(here("Plots","2023_density.jpg"), density2023, 
       width = 4, height = 4, units = "in", dpi = 600)

### plots of density by site all years but only 2023 winter  
density_all <- ggplot(tot_density, aes(site, ind_m2, fill = season)) +
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "P. interruptus densities", x = "Site", y = "Ind. per m^2") + 
  scale_fill_manual(values=c("wheat3", "seagreen4"), name = "Season", 
                 labels = c("Summer 2012-2023", "Winter 2023"))

#try it logged 
density_log <- ggplot(tot_density, aes(site, log10(ind_m2), fill = season)) +
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "P. interruptus densities", x = "Site", y = "log10(Ind. per m^2)") + 
  scale_fill_manual(values=c("wheat3", "seagreen4"), name = "Season", 
                    labels = c("Summer 2012-2023", "Winter 2023"))
#save both versions 
ggsave(here("Plots","total_density.jpg"), density_all, 
       width = 6, height = 4, units = "in", dpi = 600)

ggsave(here("Plots","log10_total_density.jpg"), density_log, 
       width = 6, height = 4, units = "in", dpi = 600)

#make one excluding 2012 for Naples only, because it was an outlier (only 6 individuals counted)
density_log_no2012nap <- ggplot(tot_density %>%
                        filter(year != 2012|site != "NAPL"), aes(site, log10(ind_m2), fill = season)) +
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "P. interruptus densities", x = "Site", y = "log10(Ind. per m^2)") + 
  scale_fill_manual(values=c("wheat3", "seagreen4"), name = "Season", 
                    labels = c("Summer 2012-2023", "Winter 2023"))
#there's still a big difference 
#_______________________________________________________________________________
#Now compare densities of legal size vs. too small 
summer_density_area <- summer %>% 
  group_by(year, site, transect, replicate, status) %>%
  summarize(counts = sum(count)) %>%
  mutate(area_m2 = 300) %>%
  group_by(year, site) %>%
  summarize(area_m2 = sum(area_m2)) %>%
  mutate(year = as.factor(year)) %>%
  mutate(season = "summer")

summer_density_legal <- summer %>%
  filter(status == "legal") %>%
  group_by(year, site, status) %>%
  summarize(counts = sum (count)) %>%
  mutate(year = as.factor(year)) %>%
  left_join(summer_density_area, summer_density_legal, by = c("year","site")) %>%
  mutate(ind_m2 = counts/area_m2)

winter_density_area <- winter %>%
  group_by(year, site, transect, replicate, status) %>%
  summarize(counts = sum(count)) %>%
  mutate(area_m2 = 300) %>%
  group_by(year, site) %>%
  summarize(area_m2 = sum(area_m2)) %>%
  mutate(year = as.factor(year)) %>%
  mutate(season = "winter")

winter_density_legal <- winter %>%
  filter(status == "legal") %>%
  group_by(year, site, status) %>%
  summarize(counts = sum(count)) %>%
  mutate(year = as.factor(year)) %>%
  left_join(winter_density_area, winter_density_legal, by = c("year", "site")) %>%
  mutate(ind_m2 = counts/area_m2)

  

#combine summer and winter density data frames 
tot_density_legal <- full_join(winter_density_legal, summer_density_legal)
#_______________________________________________________________________________
#also make non-legal densities 
summer_density_small <- summer %>% 
  filter(status == "not_legal") %>%
  group_by(year, site, status) %>%
  summarize(counts = sum (count)) %>%
  mutate(year = as.factor(year)) %>%
  left_join(summer_density_area, summer_density_small, by = c("year", "site")) %>%
  mutate(ind_m2 = counts/area_m2)


winter_density_small <- winter %>%
  filter(status == "not_legal") %>%
  group_by(year, site, status) %>%
  summarize(counts = sum(count)) %>%
  mutate(year = as.factor(year)) %>%
  left_join(winter_density_area, winter_density_small, by = c("year", "site")) %>%
  mutate(ind_m2 = counts/area_m2)

#combine summer and winter density data frames 
tot_density_small <- full_join(winter_density_small, summer_density_small)

#____________________________________________________

density2023_legal <- ggplot(tot_density_legal %>%
                        filter(year == "2023"), aes(site, ind_m2, fill = season)) +
  geom_col(position = "dodge") +
  ylim(0, 0.03) +
  theme_minimal() +
  labs(title = "P. interruptus densities 2023 legal size", x = "Site", y = "Ind. per m^2") +
  scale_fill_manual(values=c("wheat3", "seagreen4"))

#save 2023 density plot 
ggsave(here("Plots","2023_density_legal.jpg"), density2023_legal, 
       width = 4, height = 4, units = "in", dpi = 600)

#Do the same for non-legal size_________________________________________________ 
density2023_small <- ggplot(tot_density_small %>%
                              filter(year == "2023"), aes(site, ind_m2, fill = season)) +
  geom_col(position = "dodge") +
  ylim(0, 0.03) +
  theme_minimal() +
  labs(title = "P. interruptus densities 2023 non-legal size", x = "Site", y = "Ind. per m^2") +
  scale_fill_manual(values=c("wheat3", "seagreen4"))

#save 2023 density plot 
ggsave(here("Plots","2023_density_small.jpg"), density2023_small, 
       width = 4, height = 4, units = "in", dpi = 600)


#____________________________________________________
#some more plots, looking at different years 
ggplot(summer_density %>%
         filter(year == "2023"), aes(site, ind_m2)) +
  geom_col(position = "dodge", fill = "brown") +
  ylim(0, 0.05) +
  theme_minimal() +
  labs(title = "P. interruptus density in Aug 2023", x = "Site", y = "Ind. per m^2")

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
  geom_col(position = "dodge", fill = "brown") +
  ylim(0, 0.05) +
  theme_minimal() +
  labs(title = "P. interruptus density in Nov & Dec 2023", x = "Site", y = "Ind. per m^2")

###next - look into how to plot size distributions 
#use a histogram

ggplot(dt_clean %>%
         filter(month %in% c("7","8","9"), year == 2023), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE)

ggplot(dt_clean %>%
         filter(month %in% c("11","12")), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE)

#_______________________________________________________________________________
## size distributions by site 

dt_clean %>% uncount(count)

pnap <- ggplot(dt_clean %>%
                 uncount(count) %>%
         filter(month %in% c("7","8","9"), site == "NAPL"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 5) +
  xlim(0, 250) +
  labs(title = "Naples", x = "Size (mm)", y = "Count") +
  theme_minimal()

pive <- ggplot(dt_clean %>%
                 uncount(count) %>%
         filter(month %in% c("7","8","9"), site == "IVEE"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 5) +
  xlim(0, 250) +
  labs(title = "Isla Vista", x = "Size (mm)", y = "Count")+
  theme_minimal()

pcar <- ggplot(dt_clean %>%
                 uncount(count) %>%
         filter(month %in% c("7","8","9"), site == "CARP"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 5) +
  xlim(0, 250) +
  labs(title = "Carpinteria", x = "Size (mm)", y = "Count") +
  theme_minimal()

pmoh <- ggplot(dt_clean %>%
                 uncount(count) %>%
         filter(month %in% c("7","8","9"), site == "MOHK"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 5) +
  xlim(0, 250) +
  labs(title = "Mohawk", x = "Size (mm)", y = "Count") +
  theme_minimal()

paqu <- ggplot(dt_clean %>%
                 uncount(count) %>%
         filter(month %in% c("7","8","9"), site == "AQUE"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 5) +
  xlim(0, 250) +
  labs(title = "Arroyo Quemado", x = "Size (mm)", y = "Count") +
  theme_minimal()

### find mean size for each site

nap_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("7","8","9"), site == "NAPL") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

ive_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("7","8","9"), site == "IVEE") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

moh_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("7","8","9"), site == "MOHK") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

car_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("7","8","9"), site == "CARP") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

aqu_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("7","8","9"), site == "AQUE") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

napl_plot <- pnap + geom_vline(data=nap_mean, aes(xintercept=mean_size, color="red"),
                   linetype="dashed") +
  theme(legend.position = "none")

ivee_plot <- pive + geom_vline(data=ive_mean, aes(xintercept=mean_size, color="red"),
                  linetype="dashed") +
  theme(legend.position = "none")

carp_plot <- pcar + geom_vline(data=car_mean, aes(xintercept=mean_size, color="red"),
                  linetype="dashed") +
  theme(legend.position = "none")

mohk_plot <- pmoh + geom_vline(data=moh_mean, aes(xintercept=mean_size, color="red"),
                  linetype="dashed") +
  theme(legend.position = "none")

aque_plot <- paqu + geom_vline(data=aqu_mean, aes(xintercept=mean_size, color="red"),
                  linetype="dashed") +
  theme(legend.position = "none")

#_______________________________________________________________________________
#combine all plots using patchwork 

size_hist <- aque_plot + napl_plot + ivee_plot + mohk_plot + carp_plot

#save figure 
ggsave(here("Plots","lobster_size_hist.jpg"), size_hist, 
       width = 15, height = 7, units = "in", dpi = 600)
#_______________________________________________________________________________
#try a density plot to look at size distributions 

ggplot(data=dt_clean, aes(x = size_mm, group = site, fill = site)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~site) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )
#not very interesting
ggplot(data = dt_clean, aes(x=size_mm, color=site, fill=site)) +
  geom_density(alpha=0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  ylab("") 
#not interesting at all but pretty themes 
#let's make the histograms better-looking next: 
dt_clean %>% 
  na.omit() %>%
ggplot(aes(x = size_mm, color = site, fill = site)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  labs(fill = "Site")

#_______________________________________________________________________________
#now look at summer vs. winter size distributions, 2023 only 
#summer 
pnap_summer_23 <- ggplot(dt_clean %>%
                           uncount(count) %>%
                        filter(month %in% c("7","8","9"), site == "NAPL", year == 2023), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 10) +
  xlim(0, 250) +
  labs(title = "Naples Summer 2023", x = "Size (mm)", y = "Count") +
  theme_minimal()

pive_summer_23 <- ggplot(dt_clean %>%
                           uncount(count) %>%
                        filter(month %in% c("7","8","9"), site == "IVEE", year == 2023), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 10) +
  xlim(0, 250) +
  labs(title = "Isla Vista Summer 2023", x = "Size (mm)", y = "Count")+
  theme_minimal()

pcar_summer_23 <- ggplot(dt_clean %>%
                           uncount(count) %>%
                        filter(month %in% c("7","8","9"), site == "CARP", year == 2023), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 10) +
  xlim(0, 250) +
  labs(title = "Carpinteria Summer 2023", x = "Size (mm)", y = "Count") +
  theme_minimal()

pmoh_summer_23 <- ggplot(dt_clean %>%
                           uncount(count) %>%
                        filter(month %in% c("7","8","9"), site == "MOHK", year == 2023), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 10) +
  xlim(0, 250) +
  labs(title = "Mohawk Summer 2023", x = "Size (mm)", y = "Count") +
  theme_minimal()

paqu_summer_23 <- ggplot(dt_clean %>%
                           uncount(count) %>%
                        filter(month %in% c("7","8","9"), site == "AQUE", year == 2023), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 10) +
  xlim(0, 250) +
  labs(title = "Arroyo Quemado Summer 2023", x = "Size (mm)", y = "Count") +
  theme_minimal()

###

nap_summer23_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("7","8","9"), year == 2023, site == "NAPL") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

ive_summer23_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("7","8","9"), year == 2023, site == "IVEE") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

moh_summer23_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("7","8","9"), year == 2023, site == "MOHK") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

car_summer23_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("7","8","9"), year == 2023, site == "CARP") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

aqu_summer23_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("7","8","9"), year == 2023, site == "AQUE") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

napl_plot_summer23 <- pnap_summer_23 + geom_vline(data=nap_summer23_mean, aes(xintercept=mean_size, color="red"),
                                             linetype="dashed") +
  theme(legend.position = "none")

ivee_plot_summer23 <- pive_summer_23 + geom_vline(data=ive_summer23_mean, aes(xintercept=mean_size, color="red"),
                                             linetype="dashed") +
  theme(legend.position = "none")

carp_plot_summer23 <- pcar_summer_23 + geom_vline(data=car_summer23_mean, aes(xintercept=mean_size, color="red"),
                                             linetype="dashed") +
  theme(legend.position = "none")

mohk_plot_summer23 <- pmoh_summer_23 + geom_vline(data=moh_summer23_mean, aes(xintercept=mean_size, color="red"),
                                             linetype="dashed") +
  theme(legend.position = "none")

aque_plot_summer23 <- paqu_summer_23 + geom_vline(data=aqu_summer23_mean, aes(xintercept=mean_size, color="red"),
                                             linetype="dashed") +
  theme(legend.position = "none")

#winter 
pnap_winter <- ggplot(dt_clean %>%
                        uncount(count) %>%
                 filter(month %in% c("11","12"), site == "NAPL"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 10) +
  xlim(0, 250) +
  labs(title = "Naples Winter 2023", x = "Size (mm)", y = "Count") +
  theme_minimal()

pive_winter <- ggplot(dt_clean %>%
                        uncount(count) %>%
                 filter(month %in% c("11","12"), site == "IVEE"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 10) +
  xlim(0, 250) +
  labs(title = "Isla Vista Winter 2023", x = "Size (mm)", y = "Count")+
  theme_minimal()

pcar_winter <- ggplot(dt_clean %>%
                        uncount(count) %>%
                 filter(month %in% c("11","12"), site == "CARP"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 10) +
  xlim(0, 250) +
  labs(title = "Carpinteria Winter 2023", x = "Size (mm)", y = "Count") +
  theme_minimal()

pmoh_winter <- ggplot(dt_clean %>%
                        uncount(count) %>%
                 filter(month %in% c("11","12"), site == "MOHK"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 10) +
  xlim(0, 250) +
  labs(title = "Mohawk Winter 2023", x = "Size (mm)", y = "Count") +
  theme_minimal()

paqu_winter <- ggplot(dt_clean %>%
                        uncount(count) %>%
                 filter(month %in% c("11","12"), site == "AQUE"), aes(x = size_mm)) +
  geom_histogram(na.rm = TRUE, binwidth = 10) +
  xlim(0, 250) +
  labs(title = "Arroyo Quemado Winter 2023", x = "Size (mm)", y = "Count") +
  theme_minimal()

###

nap_winter_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("11","12"), site == "NAPL") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

ive_winter_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("11","12"), site == "IVEE") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

moh_winter_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("11","12"), site == "MOHK") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

car_winter_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("11","12"), site == "CARP") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

aqu_winter_mean <- dt_clean %>% 
  uncount(count) %>%
  filter(month %in% c("11","12"), site == "AQUE") %>%
  na.omit() %>% 
  summarize(mean_size = mean(size_mm))

napl_plot_winter <- pnap_winter + geom_vline(data=nap_mean, aes(xintercept=mean_size, color="red"),
                               linetype="dashed") +
  theme(legend.position = "none")

ivee_plot_winter <- pive_winter + geom_vline(data=ive_mean, aes(xintercept=mean_size, color="red"),
                               linetype="dashed") +
  theme(legend.position = "none")

carp_plot_winter <- pcar_winter + geom_vline(data=car_mean, aes(xintercept=mean_size, color="red"),
                               linetype="dashed") +
  theme(legend.position = "none")

mohk_plot_winter <- pmoh_winter + geom_vline(data=moh_mean, aes(xintercept=mean_size, color="red"),
                               linetype="dashed") +
  theme(legend.position = "none")

aque_plot_winter <- paqu_winter + geom_vline(data=aqu_mean, aes(xintercept=mean_size, color="red"),
                               linetype="dashed") +
  theme(legend.position = "none")
#_______________________________________________________________________________
#combine all plots using patchwork 

size_hist_season_nap <- napl_plot_summer23 + napl_plot_winter
size_hist_season_ive <- ivee_plot_summer23 + ivee_plot_winter
size_hist_season_car <- carp_plot_summer23 + carp_plot_winter
size_hist_season_moh <- mohk_plot_summer23 + mohk_plot_winter
size_hist_season_aqu <- aque_plot_summer23 + aque_plot_winter


#save figures 
ggsave(here("Plots","napl_size_hist.jpg"), size_hist_season_nap, 
       width = 15, height = 7, units = "in", dpi = 600)

ggsave(here("Plots","ivee_size_hist.jpg"), size_hist_season_ive, 
       width = 15, height = 7, units = "in", dpi = 600)

ggsave(here("Plots","carp_size_hist.jpg"), size_hist_season_car, 
       width = 15, height = 7, units = "in", dpi = 600)

ggsave(here("Plots","mohk_size_hist.jpg"), size_hist_season_moh, 
       width = 15, height = 7, units = "in", dpi = 600)

ggsave(here("Plots","aque_size_hist.jpg"), size_hist_season_aqu, 
       width = 15, height = 7, units = "in", dpi = 600)

#TLDR: was accidentally using plyr insread of dplyr to summarize and it wasn't working but now it's working














