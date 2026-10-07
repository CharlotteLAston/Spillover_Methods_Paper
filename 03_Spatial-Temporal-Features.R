library(tidyverse)
library(dplyr)
library(googlesheets4)
library(stringdist)
library(fuzzyjoin)
library(countrycode)
library(sf)
library(ggplot2)
library(ggpubfigs)
library(forcats)
library(ggbreak)
library(readxl)
library(writexl)
library(DescTools)

rm(list = ls())

#### Set up directories ----
working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

# setwd(working.dir)
# source("X_Functions.R")

dat.dir <- paste(working.dir, "Data", sep="/")
sg.dir <- paste(working.dir, "Staging", sep="/")
fig.dir <- paste(working.dir, "Figures", sep="/")

cbPalette <- c("#332288", "#44AA99", "#CC6677", "#DDCC77","#AA4499","#88CCEE", "#882255", "#117733")
a4.width=160

#### Read in data ----
setwd(sg.dir)
bands <- read_xlsx("sampling_bands.xlsx")
distance.results <- read_xlsx("cleaned_methods_distance.xlsx") 
regression.results <- read_xlsx("cleaned_methods_regression.xlsx")
study_dat <- read_xlsx("cleaned_methods_study_dat.xlsx") %>%   # This has metadata attached 
  filter(!result.group %in% "genetics")
length(unique(study_dat$paper.id))

distance.results <- distance.results %>% 
  left_join(study_dat)

regression.results <- regression.results %>% 
  left_join(study_dat)

setwd(dat.dir)
sampling.bands <- read_excel("methods_distance_results_long.xlsx", sheet=3) %>% 
  mutate(band.end = as.numeric(band.end)) %>% 
  mutate(notes = paste0(mpa.name, sep="_", notes)) %>% 
  group_by(paper.id, mpa.name, notes) %>% 
  mutate(interval = band.end - band.start)


## Plot of maximum distances studied in papers ----

max.distances.bands <- sampling.bands %>% 
  group_by(paper.id, mpa.name) %>% 
  slice_max(order_by = band.end, n = 1) %>% 
  rename(max.distance = "band.end") %>% 
  dplyr::select(paper.id, mpa.name, max.distance, notes)

additional.distances <- read_excel("methods_distance_results_long.xlsx", sheet=2) %>%  # Papers that presented results over time but still gave distances they sampled at
  mutate(max.distance = as.double(max.distance))
  
# check.papers <- max.distances.bands %>% 
#   dplyr::select(paper.id, mpa.name, max.distance, notes) %>% 
#   rbind(additional.distances)
# 
# temp <- study_dat %>% 
#   filter(!paper.id %in% check.papers$paper.id) %>% 
#   distinct(paper.id, mpa.name, .keep_all=T) %>% 
#   dplyr::select(paper.id, mpa.name)


distance.spillover <- max.distances.bands %>% 
  rbind(additional.distances) %>% 
  distinct(paper.id, mpa.name, .keep_all=T) %>% 
  left_join(study_dat) %>% 
  filter(!grepl("Larval|larval", sampling.method)) %>% 
  mutate(study.type = ifelse(grepl("Commercial fishing survey|Commercial surveys|Surveys|VMS|Log-book data|AIS|Interviews|log-book data|Catch world records|Experimental|experimental", sampling.method), "Fisheries", "Ecological")) %>% 
  filter(paper.id %in% study_dat$paper.id)

distance.mpa.fisheries <- distance.spillover %>% 
  filter(study.type %in% "Fisheries")


distance.mpa.ecological <- distance.spillover %>% 
  filter(study.type %in% "Ecological")


# Plot ecological study distances
p_distances_ecological <- distance.mpa.ecological %>% 
  filter(max.distance >=0) %>% 
  mutate(max.distance.km = max.distance/1000) %>% 
  group_by(paper.id) %>%
  ggplot() +
  geom_density(aes(x=max.distance.km), fill="#117733", alpha=0.75)+
  #scale_x_continuous(breaks = seq(0, 70, by = 5)) +
  theme_classic()+
  theme(strip.background = element_blank())+
  ylab("Density")+
  xlab("Maximum distance (km)")
p_distances_ecological


# Plot ecological study distances
p_distances_fisheries <- distance.mpa.fisheries %>% 
  filter(max.distance >=0) %>% 
  mutate(max.distance.km = max.distance/1000) %>% 
  group_by(paper.id) %>%
  ggplot() +
  geom_density(aes(x=max.distance.km), fill="#882255", alpha=0.75)+
  #scale_x_continuous(breaks = seq(0, 800, by = 25)) +
  theme_classic()+
  theme(strip.background = element_blank())+
  ylab("Density")+
  xlab("Maximum distance (km)")
p_distances_fisheries

## Create panel and save plot
p1 <- p_distances_ecological
p2 <- p_distances_fisheries

# Explicitly assign each plot to its position
layout <- "
123
123
"

max.distances <- p1 + plot_spacer() +  p2 + 
  plot_layout(design = layout, heights = c(1,1,1), widths = c(1,0.1,1))
max.distances

setwd(fig.dir)
ggsave(max.distances, filename="Max-distance-sampled.png",height = a4.width, width = a4.width*1.5, units  ="mm", dpi = 300 )


#### Plot bands widths that different papers studied ----
bands.mpa <- bands %>% 
  filter(!paper.id %in% c(406, 1199)) %>% # Filter out papers that actually didn't sample in intervals but presented data like they did 
  filter(!interval.type %in% "Non-standard sampling") %>% # Check whether 209 and 364 have remained in here because they shouldn't
  distinct(paper.id, mpa.name, interval, .keep_all=T) %>% 
  mutate(interval = as.integer(interval)) %>% 
  filter(!is.na(interval)) %>% 
  group_by(interval) %>% 
  summarise(no.papers = n()) %>% 
  ungroup()

head(bands.mpa)
interval.plot <- bands.mpa %>% 
  ggplot()+
  geom_col(aes(x=interval, y=no.papers), position="identity", stat = "identity", width = 0.05, alpha = 0.6) +
  theme_classic()
interval.plot

#### Plot of max distances and bands ----
study.types <- distance.spillover %>% 
  ungroup() %>% 
  dplyr::select(paper.id, study.type)

sampling.bands <- sampling.bands %>% 
  left_join(study.types) %>% 
  left_join(study_dat) %>% 
  mutate(label = ifelse(sampling.bands %in% "no" & analysis.bands %in% "yes", "*", NA))
length(unique(sampling.bands$paper.id))


sampling.bands %>% 
  left_join(study_dat) %>% 
  filter(!grepl("Larval|larval", sampling.method)) %>% 
  mutate(study.type = ifelse(grepl("Commercial fishing survey|Commercial surveys|Surveys|VMS|Log-book data|AIS|Interviews|log-book data|Catch world records|Experimental|experimental", sampling.method), "Fisheries", "Ecological")) %>% 
  filter(study.type %in% "Ecological") %>% 
  distinct(paper.id, mpa.name, interval, .keep_all=T) %>%
  filter(interval >0) %>% 
  ungroup() %>% 
  summarise(Mean = mean(interval, na.rm=T),
            SD = sd(interval, na.rm=T),
            Max = max(interval, na.rm=T),
            Min = min(interval, na.rm=T),
            Median = median(interval, na.rm=T))

sampling.bands %>% 
  left_join(study_dat) %>% 
  filter(!grepl("Larval|larval", sampling.method)) %>% 
  mutate(study.type = ifelse(grepl("Commercial fishing survey|Commercial surveys|Surveys|VMS|Log-book data|AIS|Interviews|log-book data|Catch world records|Experimental|experimental", sampling.method), "Fisheries", "Ecological")) %>% 
  filter(study.type %in% "Ecological") %>% 
  distinct(paper.id, mpa.name, interval, .keep_all=T) %>%
  filter(interval >0) %>% 
  ungroup() %>% 
  {table(.$interval)}


bar.data <- sampling.bands %>%
  # group_by(study.type, paper.id, mpa.name, notes, label) %>%
  # summarise(max.dist = max(band.end, na.rm = TRUE),
  #           min.dist = min(band.start, na.rm=TRUE)) %>% 
  # ungroup() %>% 
  filter(!notes %in% "apo island protected landscape and seascape_catch data") %>% 
  filter(!paper.id %in% 310) %>% 
  arrange(paper.id, mpa.name, notes, band.end) %>% 
  group_by(paper.id, mpa.name, notes) %>% 
  mutate(order.id = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(study.type = ifelse(is.na(study.type), "Fisheries", study.type)) %>%  # All of the missing ones are fisheries data
  distinct(paper.id, mpa.name, notes, band.start, band.end, .keep_all=T) 

plot.order <- bar.data %>% 
  dplyr::select(paper.id, mpa.name, notes, order.id)

sampling.bands <- sampling.bands %>% 
  filter(!paper.id %in% 310) %>% 
  left_join(plot.order) %>% 
  distinct(paper.id, mpa.name, notes, band.start, band.end, .keep_all=T) 

labels <- bar.data %>% 
  group_by(order.id) %>% 
  slice_max(order_by = band.end, n=1)

total.distance.plot <- ggplot() +
  geom_segment(
    data = bar.data,
    aes(x = log((band.start/1000)+1), xend = log((band.end/1000)+1), y = order.id, yend = order.id, colour=study.type),
    linewidth = 4, alpha = 0.5
  ) +
  scale_colour_manual(values=c("#117733", "#882255"), name="Data type")+
  geom_point(
    data = sampling.bands,
    aes(x = log((band.end/1000)+1), y = order.id),
    shape = "|", size = 2.25, colour = "black"
  ) + 
  geom_text(data=labels, aes(x = log((band.end/1000)+1), y = order.id, label = label), hjust = -0.5, vjust=0.75, size=6)+
  xlab("Distance sampled (log km +1)") +
  ylab("Papers")+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
total.distance.plot

setwd(fig.dir)
ggsave(total.distance.plot, filename="Distance_bars.png",height = a4.width*1.2, width = a4.width, units  ="mm", dpi = 300 )


#* For papers that didn't stratify by sampling bands ----
distance.dat.continuous <- distance.results %>% 
  distinct(paper.id, clean.distance, .keep_all=T) %>% 
  dplyr::select(paper.id, mpa.name, clean.distance, sampling.bands, analysis.bands) %>% 
  rbind(regression.results.distances) %>% 
  filter(!is.na(clean.distance)) %>% 
  mutate(study.type = ifelse(paper.id %in% distance.mpa.ecological$paper.id, "Ecological", "Fisheries")) %>% 
  #filter(paper.id %in% distance.mpa.ecological$paper.id) %>% 
  filter(clean.distance>0) %>% 
  filter(sampling.bands =="no" & analysis.bands =="no")


distance.dat %>% 
  distinct(paper.id, mpa.name, clean.distance, .keep_all=T) %>%
  arrange(paper.id, mpa.name, clean.distance) %>%
  group_by(study.type, paper.id, mpa.name) %>%
  mutate(
    lower_bound = lag(clean.distance), # Shifts the row that R is looking at down by one so that we're subtracting the right values
    upper_bound = clean.distance,
    interval    = upper_bound - lower_bound
  ) %>%
  ungroup() %>% 
  filter(study.type %in% "Fisheries") %>% 
  summarise(Mean = mean(interval, na.rm=T),
            SD = sd(interval, na.rm=T),
            Max = max(interval, na.rm=T),
            Min = min(interval, na.rm=T),
            Median = median(interval, na.rm=T),
            Mode.Dist = Mode(interval, na.rm=T))

bar.data.continuous <- distance.dat.continuous %>%
  group_by(study.type, paper.id) %>%
  summarise(max.dist = max(clean.distance, na.rm = TRUE),
            min.dist = min(clean.distance, na.rm=TRUE)) %>% 
  mutate(min.dist = 0)

paper.order.continuous <- bar.data.continuous %>% 
  arrange(max.dist) %>% 
  pull(paper.id)

bar.data.continuous <- bar.data.continuous %>%
  mutate(paper.id = factor(paper.id, levels = paper.order.continuous))

distance.dat.continuous <- distance.dat.continuous %>%
  mutate(paper.id = factor(paper.id, levels = paper.order.continuous)) 

total.distance.plot <- ggplot() +
  geom_segment(
    data = bar.data.continuous,
    aes(x = log((min.dist/1000)+1), xend = log((max.dist/1000)+1), y = paper.id, yend = paper.id, colour=study.type),
    linewidth = 4, alpha = 0.5
  ) +
  scale_colour_manual(values=c("#117733", "#882255"), name="Data type")+
  geom_point(
    data = distance.dat.continuous,
    aes(x = log((clean.distance/1000)+1), y = paper.id),
    shape = "|", size = 2.25, colour = "black"
  ) + 
  #geom_text(data=bar.data, aes(x = log((max.dist/1000)+1), y = paper.id, label = label), hjust = -0.5, vjust=0.75, size=6)+
  xlab("Distance sampled (log km +1)") +
  ylab("Papers")+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
total.distance.plot



#### Plot of times that were studied -----

time.bands <- study_dat %>% 
  dplyr::select(paper.id, mpa.name, year.established, data.start, data.end) %>% 
  distinct(paper.id, mpa.name, data.start, data.end, .keep_all=T) %>% 
  filter(data.start>0) %>% 
  mutate(mpa.id = as.integer(factor(mpa.name, levels = unique(mpa.name))))

data.years <- time.bands %>% 
  mutate(data.end = ifelse(data.start==data.end, data.end+1, data.end)) %>% 
  mutate(data.end = ifelse(year.established==data.end, data.end+1, data.end)) %>% 
  dplyr::select(paper.id, data.start, data.end, mpa.id) 

data.established <- time.bands %>% 
  dplyr::select(paper.id, year.established, mpa.id)

temp <- time.bands %>% 
  mutate(difference = data.start-year.established) %>% 
  mutate(no.years = data.end-data.start)


data.lines <- time.bands %>% 
  group_by(mpa.name) %>%
  slice_max(data.end, n = 1) %>%
  ungroup()

years.plot <- ggplot() +
  geom_segment(
    data = data.years,
    aes(x = data.start, xend = data.end, y = mpa.id, yend = mpa.id),
    linewidth = 2, alpha = 0.5, colour="#88CCEE"
  ) +
  geom_segment(
    data = data.lines,
    aes(x = year.established, xend = data.end, y = mpa.id, yend = mpa.id),
    linewidth = 0.25, colour="black"
  ) +
  geom_point(
    data = data.established,
    aes(x = year.established, y = mpa.id),
    shape="l", size = 2.25, colour = "black"
  ) +
  xlab("Time period") +
  ylab("Marine Protected Areas")+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")
years.plot

## Orientations studied
setwd(dat.dir)
orientations <- read_excel("methods_clean_study-data.xlsx") %>% 
  filter(paper.id %in% study_dat$paper.id) %>% 
  distinct(paper.id, mpa.name, .keep_all=TRUE)

table(orientations$direction.studied)

length(unique(study_dat$mpa.name))



