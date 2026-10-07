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

rm(list = ls())

#### Set up directories ----
working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

# setwd(working.dir)
# source("X_Functions.R")

dat.dir <- paste(working.dir, "Data", sep="/")
sg.dir <- paste(working.dir, "Staging", sep="/")
fig.dir <- paste(working.dir, "Figures", sep="/")

a4.width=160

#### Read in and join data ----

setwd(dat.dir)
dat <- read_xlsx("methods_clean_study-data.xlsx")
metadata <- read_xlsx("clean_metadata.xlsx") %>% 
  mutate(mpa.name = tolower(mpa.name),
         reserve.name = tolower(reserve.name)) %>% 
  dplyr::select(paper.id, study.design, inside.outside, true.counterfactual, counterfactual.type, counterfactual.justification, counterfactual.strength, data.start, data.end, mpa.name, reserve.name)

results_types <- readRDS("all_results_long.RDS") %>% 
  dplyr::select(paper.id, result.group) %>% 
  distinct(paper.id, result.group)

### Join data
dat_full <- dat %>% 
  mutate(mpa.name = tolower(mpa.name),
         reserve.name = tolower(reserve.name),
         size = as.double(size)) %>% 
  dplyr::select(-true.counterfactual) %>% 
  left_join(metadata, by=c("paper.id", "mpa.name", "reserve.name")) %>% 
  filter(!paper.id %in% c(8, 980)) %>%  # Papers that only reported significant results 
  left_join(results_types) %>% 
  mutate(result.group = ifelse(is.na(result.group)|paper.id %in% c(181, 1408, 420), "movement", result.group)) %>% 
  filter(!result.group %in% "movement")


#### Fix any missing/incomplete data ----
temp <- dat_full %>% 
  filter(result.group %in% c("flexible", "point")) %>% 
  distinct(paper.id, .keep_all=T)

dat_full <- dat_full %>% 
  mutate(result.group = ifelse(paper.id==147, "distance", result.group),
         result.group = ifelse(paper.id==518, "time", result.group), # Not really a time paper but shouldn't be in with movement and it's distance
         result.group = ifelse(paper.id==865, "distance", result.group),
         result.group = ifelse(paper.id==1348, "genetics", result.group),
         result.group = ifelse(paper.id==1356, "genetics", result.group), # Technically larval/reproduction but I'm grouping these together
         result.group = ifelse(paper.id==1400, "time", result.group),
         result.group = ifelse(paper.id==1595, "distance", result.group),
         result.group = ifelse(paper.id==1020, "distance", result.group),
         result.group = ifelse(paper.id==422, "distance", result.group),
         result.group = ifelse(paper.id==576, "genetics", result.group),
         result.group = ifelse(paper.id==682, "distance", result.group),
         result.group = ifelse(paper.id==1615, "distance", result.group),
         result.group = ifelse(paper.id==1385, "time", result.group)) 


#### Format the bands of distance around MPAs ----
distance.bands <- read_xlsx("distance_results_bands.xlsx") %>% 
  dplyr::select(paper.id, mpa.name, distance.counterfactual, interval.type) %>% 
  mutate(mpa.name = tolower(mpa.name)) 
distance.results <- read_xlsx("methods_distance_results_long.xlsx") %>% # This now has regression results in here as well for studies that measured at distance intervals
  filter(!paper.id %in% c(8, 9, 65, 104, 107, 123, 146, 151, 196, 399, 474, 490, 533, 544, 613, 643, 646, 658, 667, 696, 790, 863, 920, 943, 932, 982, 999, 1005, 1047, 1053, 1060, 1133, 1134, 1422, 1477, 1590, 1618)) %>% # We didn't use this data in the initial review so make sure you're filtering out the papers that we decided weren't suitable
  mutate(clean.distance = as.numeric(clean.distance),
         spillover.distance = as.numeric(spillover.distance)) %>% 
  filter(!paper.id == 61) # Actually measured near/far over time rather than a gradient


## Create unique identifier, some papers sample over different boundaries or with slightly different methods for different metrics so these need to have their 
# band widths calculated separately 

modifiers <- distance.results %>% 
  mutate(across(where(is.character), ~na_if(.x, "NA"))) %>% 
  mutate(modifier.1 = str_extract_all(comments, regex("South(?![:blank:])|East orientation|eastern boudnary|West orientation|western boundary|South - |south-east|south-west|fished area to the south|fished area to the north|fished area to north|southern|northern boundary|Fished shoreline (Hole-in-the-Wall)|Fished shoreline (Shixini)|south transect|north boundary|northern transect|north orietation|south orientation", 
                                                      ignore_case=T))) %>%
  unnest_longer(modifier.1, keep_empty = TRUE) %>% 
  mutate(modifier.2 = str_extract_all(comments, regex("size class \\d+|distance classes"))) %>% 
  unnest_longer(modifier.2, keep_empty = TRUE) %>% 
  mutate(modifier.3 = str_extract_all(comments, regex("summer|spring|winter|(southeast|southeastern|northwest|northwestern|southwest|southwestern|northeastern|northeast) monsoon", ignore_case=T))) %>% 
  unnest_longer(modifier.3, keep_empty = TRUE) %>% 
  mutate(modifier.4 = str_extract_all(comments, regex("sandy|rocky reef|posidonia", ignore_case=T))) %>% 
  unnest_longer(modifier.4, keep_empty = TRUE) %>% 
  mutate(modifier.5 = str_extract_all(comments, regex("\\d+\\s?m depth|\\d+\\s?m deep", ignore_case=T))) %>% 
  unnest_longer(modifier.5, keep_empty = TRUE) %>% 
  mutate(modifier.6 = str_extract_all(comments, regex("control|outside|fished", ignore_case=T))) %>% 
  unnest_longer(modifier.6, keep_empty = TRUE) %>% 
  mutate(modifier.6 = str_extract_all(comments, regex("gull|scorpion|carrington", ignore_case=T))) %>% 
  unnest_longer(modifier.6, keep_empty = TRUE) %>% 
  unite("modifier", modifier.1, modifier.2, modifier.3, modifier.4, modifier.5, modifier.6, na.rm = TRUE, sep = "_") %>% 
  mutate(unique.unit = ifelse((is.na(family) & is.na(genus)), paste0(metric, sep="_", sample.unit, sep="_", modifier),
                              ifelse((!is.na(family) & is.na(genus)), paste0(metric, sep="_", family, sep="_", modifier),
                                     paste0(metric, sep="_", full.name, sep="_", modifier))))

distance.results <- distance.results %>% 
  mutate(unique.unit = modifiers$unique.unit)


## Fix names of MPAs in the distance results so things join nicely later
mpa.names <- distance.results %>% 
  dplyr::select(paper.id, mpa.name) %>% 
  distinct(mpa.name, .keep_all=T)

distance.results <- distance.results %>% 
  mutate(mpa.name = tolower(mpa.name)) %>% 
  mutate(mpa.name = ifelse(mpa.name %in% "apo island marine reserve", "apo island protected landscape and seascape", mpa.name),
         mpa.name = ifelse(mpa.name %in% "cebères-canyuls marine reserve", "cerbère-banyuls marine reserve", mpa.name),
         mpa.name = ifelse(mpa.name %in% "columbretes island marine reserve", "columbretes islands marine reserve", mpa.name),
         mpa.name = ifelse(mpa.name %in% "castle rocks marine reserve", "castle rock marine protected area", mpa.name),
         mpa.name = ifelse(mpa.name %in% "goukamma nature reserve", "goukamma marine protected area", mpa.name),
         mpa.name = ifelse(mpa.name %in% "dwesa-cwebe marine reserve", "dwesa-cwebe marine protected area", mpa.name),
         mpa.name = ifelse(mpa.name %in% "demersal commercial fishes", "loma pelada marine protected area", mpa.name),
         mpa.name = ifelse(mpa.name %in% "barabdos marine reserve", "barbados marine reserve", mpa.name),
         mpa.name = ifelse(mpa.name %in% "malindi. national marine park", "malindi national marine park", mpa.name),
         mpa.name = ifelse(mpa.name %in% "malinidi national marine park", "malindi national marine park", mpa.name),
         mpa.name = ifelse(mpa.name %in% "medes marine reserve", "medes islands marine reserve", mpa.name),
         mpa.name = ifelse(mpa.name %in% "torre guaceto marine reserve", "torre guaceto marine protected area", mpa.name),
         mpa.name = ifelse(mpa.name %in% "yam rosh hanikra", "yam rosh haniqra", mpa.name))


## Similarly some papers sample the same way over multiple time periods but when you arrange in order the same distances for multiple time periods get put together
# giving you small bands that don't actually exist, so we need to make sure we're only getting one time period per papaer

papers.phase <- distance.results %>% # Filtering out papers where a time period is mentioned at all
  filter(grepl("late|early|post|after|years|before|protection|dec-90|(19|20)\\d{2}", comments, ignore.case=TRUE)) %>% 
  distinct(paper.id, .keep_all=T) %>% 
  filter(!paper.id %in% c(176, 209, 490, 865, 1280, 72, 93, 118, 592, 643, 196, 790, 1550, 1133, 658, 1400, 803)) # remove papers that have been included using the filter but don't have separate phases

papers.phase.full <- distance.results %>%   
  group_by(paper.id) %>% 
  mutate(result.id = row_number()) %>% 
  ungroup() %>% 
  filter(paper.id %in% papers.phase$paper.id) %>% 
  # Some papers looked at an early and a late phase of protection, just using the late phase as there's no way to show on the plots the effect
  # of the length of time of protection
  mutate(phase = ifelse(paper.id==304 & (result.id<6|result.id==11), "early", "late"),
         phase = ifelse(paper.id==104 & grepl("Pre-MPA", comments), "early", phase ),
         phase = ifelse(paper.id==34 & grepl("1985-1988|1990-2001", comments), "early", phase),
         phase = ifelse(paper.id==367 & grepl("years 0-15", comments), "early", phase),
         phase = ifelse(paper.id==531 & grepl("Pre-MPA|One year|Two years|Three years", comments), "early", phase),
         phase = ifelse(paper.id==1325 & !comments %in% "2005", "early", phase),
         phase = ifelse(paper.id==1535 & grepl("1993-1997|1988-1992", comments), "early", phase),
         phase = ifelse(paper.id==398 & grepl("Before", comments), "early", phase),
         phase = ifelse(paper.id==791 & grepl("before", comments), "early", phase),
         phase = ifelse(paper.id==778 & grepl("1-8", comments), "early", phase),
         phase = ifelse(paper.id==1026 & result.id==2, "early", phase),
         phase = ifelse(paper.id==267 & grepl("Pre-MPA", comments), "early", phase),
         phase = ifelse(paper.id==1033 & grepl("1995-1999|2000-2004|2005-2009", comments), "early", phase),
         phase = ifelse(paper.id==982 & grepl("2003-2007", comments), "early", phase),
         phase = ifelse(paper.id==244 & grepl("before", comments), "early", phase),
         phase = ifelse(paper.id==803 & grepl("1996-2000|2001-2005|2000-2005|2000-2004|", comments), "early", phase),
         phase = ifelse(paper.id==694 & grepl("1998-2002|1999-2003", comments), "early", phase),
         phase = ifelse(paper.id==65 & grepl("1989|1990|1992|1994|1996|1998|1999|2000|2001", comments), "early", phase),
         phase = ifelse(paper.id==151 & grepl("before", comments), "early", phase),
         phase = ifelse(paper.id==613 & grepl("following protection", comments), "early", phase)) %>% 
  dplyr::select(paper.id, result.id, phase)

distance.results <- distance.results %>% 
  group_by(paper.id) %>% 
  mutate(result.id = row_number()) %>% 
  ungroup() %>% 
  full_join(papers.phase.full) %>% 
  mutate(phase = replace_na(phase, "none"))

#### Calculate width of the bands used to sample ----
band_width <- distance.results %>%
  distinct(paper.id, mpa.name, phase, unique.unit, clean.distance, distance.from.boundary) %>%
  arrange(paper.id, mpa.name, phase, unique.unit, clean.distance) %>%
  mutate(
    is.range = str_detect(distance.from.boundary, "^\\s*-?\\d+\\.?\\d*\\s*-\\s*-?\\d+\\.?\\d*\\s*$"),
    lower.boundary = if_else(is.range,
                             str_match(distance.from.boundary, "^\\s*(-?\\d+\\.?\\d*)\\s*-\\s*-?\\d+\\.?\\d*\\s*$")[,2],
                             NA_character_),
    upper.boundary = if_else(is.range,
                             str_match(distance.from.boundary, "^\\s*-?\\d+\\.?\\d*\\s*-\\s*(-?\\d+\\.?\\d*)\\s*$")[,2],
                             NA_character_),
    lower.boundary = as.numeric(lower.boundary),
    upper.boundary = as.numeric(upper.boundary)
  ) %>%
  select(-is.range) %>% 
  mutate(lower.boundary = ifelse(str_detect(distance.from.boundary, "^<\\d*"), 0, lower.boundary),
         upper.boundary = ifelse(str_detect(distance.from.boundary, "^<\\d*"), str_extract(distance.from.boundary, "(?<=<)\\d*"), upper.boundary)) %>%
  mutate(any.issues = ifelse(paper.id %in% c(362), "No max distance", NA))

# Make sure catch distances are not included in paper 34


band_width <- distance.results %>%
  distinct(paper.id, mpa.name, phase, unique.unit, clean.distance, distance.from.boundary) %>%
  arrange(paper.id, mpa.name, phase, unique.unit, clean.distance) %>%
  group_by(paper.id, mpa.name, phase, unique.unit) %>%
  mutate(
    lower_bound = lag(clean.distance), # Shifts the row that R is looking at down by one so that we're subtracting the right values
    upper_bound = clean.distance,
    interval    = upper_bound - lower_bound
  ) %>%
  ungroup() %>% 
  mutate(mpa.name = tolower(mpa.name)) %>% 
  left_join(distance.bands)

distance.results %>%
  filter(str_detect(distance.from.boundary, "-")) %>%
  distinct(distance.from.boundary) %>%
  print(n = Inf)


## Clean regression results that sampled over distance but not at specific bands
setwd(dat.dir)
regression.results <- read_xlsx("clean_regression_results_long.xlsx")

regression.results <- regression.results %>% 
  filter(!paper.id %in% distance.results$paper.id) %>% # Filter out the ones that have distance bands that have already been included in the distance dataset
  filter(!paper.id %in% c(8, 9, 65, 104, 107, 123, 146, 151, 196, 399, 474, 490, 533, 544, 613, 643, 646, 658, 667, 696, 790, 863, 920, 943, 932, 982, 999, 1005, 1047, 1053, 1060, 1133, 1134, 1422, 1477, 1590, 1618)) %>%  # We didn't use this data in the initial review so make sure you're filtering out the papers that we decided weren't suitable
  mutate(mpa.name = tolower(mpa.name)) %>% 
  mutate(across(where(is.character), ~case_when(
    .x %in% c("NA", "na") ~ NA_character_,
    TRUE ~ .x
  ))) %>% 
  mutate(clean.distance = str_remove_all(max.distance, "<|~|>|≤|≥")) %>% 
  mutate(clean.distance = str_replace_all(clean.distance, "(\\d)-(\\d)", "\\1,\\2")) %>% # where numbers have been put in with a hyphen connecting them, swap to , to make it difference from negative numbers
  mutate(clean.distance = case_when(
    str_detect(clean.distance, "-|,") ~ {     # If it contains a dash (range) or a comma, calculate average
      numbers <- str_extract_all(clean.distance, "-?\\d+(?:\\.\\d+)?") %>%
        map_dbl(~ max(as.numeric(.x)))
      numbers
    },
    TRUE ~ as.numeric(str_extract(clean.distance, "-?\\d+(?:\\.\\d+)?"))     # If it's just a single number, convert it
  )) %>% 
  mutate(clean.distance = ifelse(grepl("nm",max.distance), clean.distance*1852, clean.distance)) %>% # convert from nautical mile to m
  mutate(clean.distance = ifelse(grepl("immediately adjacent to reserve boundary",max.distance), 0, clean.distance))

# Remove any papers that aren't fisheries focused
full.data <- read.csv("/Users/00106632/Documents/UWA-Minderoo/Code/Spillover_Systematic-Review/Staging/clean_full_data.csv")

paper.focus <- full.data %>% 
  filter(paper.id %in% study_dat$paper.id) %>% 
  dplyr::select(paper.id, bioregion, reserve.name, mpa.name, zone.type, sample.unit, unique.unit, functional.group, significant, targeted) %>% 
  mutate(targeted = ifelse(is.na(targeted) & grepl("commercially important|Exploited|", functional.group), "yes", targeted),
         targeted = ifelse(is.na(targeted) & grepl("long-lines|Target species", sample.unit), "yes", targeted),
         targeted = ifelse(is.na(targeted) & grepl("Non-target", sample.unit), "no", targeted)) %>% 
  mutate(paper.focus = ifelse(is.na(targeted), "Biodiversity", targeted),
         paper.focus = ifelse(paper.focus %in% "yes", "Fisheries", paper.focus),
         paper.focus = ifelse(paper.focus %in% "no", "Biodiversity", paper.focus)) %>% 
  group_by(paper.id) %>% 
  mutate(any.fisheries = if_else(any(paper.focus %in% "Fisheries"), "Yes", "No")) %>% 
  distinct(paper.id, .keep_all=T)

## Save data
setwd(sg.dir)
write_xlsx(band_width, "sampling_bands.xlsx")
write_xlsx(distance.results, "cleaned_methods_distance.xlsx")
write_xlsx(regression.results, "cleaned_methods_regression.xlsx")
write_xlsx(study_dat, "cleaned_methods_study_dat.xlsx")

