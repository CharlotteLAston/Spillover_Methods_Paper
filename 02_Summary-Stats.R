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
library(scales)

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
setwd(dat.dir)
published <- read_xlsx("Year_Published.xlsx") %>% 
  dplyr::select(paper.id, year.published)

setwd(sg.dir)
bands <- read_xlsx("sampling_bands.xlsx")
distance.results <- read_xlsx("cleaned_methods_distance.xlsx") 
regression.results <- read_xlsx("cleaned_methods_regression.xlsx")
study_dat1 <- read_xlsx("cleaned_methods_study_dat.xlsx") %>%   # This has metadata attached 
  filter(!result.group %in% c("genetics", "egg production")) #%>% 
  filter(!paper.id %in% c(244))


distance.results <- distance.results %>% 
  left_join(study_dat)
regression.results <- regression.results %>% 
  left_join(study_dat)

full.data <- read.csv("/Users/00106632/Documents/UWA-Minderoo/Code/Spillover_Systematic-Review/Staging/clean_full_data.csv")
paper.focus <- full.data %>% 
  #filter(paper.id %in% study_dat$paper.id) %>% 
  dplyr::select(paper.id, bioregion, reserve.name, mpa.name, zone.type, sample.unit, unique.unit, functional.group, significant, targeted) %>% 
  mutate(targeted = ifelse(is.na(targeted) & grepl("commercially important|Exploited|", functional.group), "yes", targeted),
         targeted = ifelse(is.na(targeted) & grepl("long-lines|Target species", sample.unit), "yes", targeted),
         targeted = ifelse(is.na(targeted) & grepl("Non-target", sample.unit), "no", targeted)) %>% 
  mutate(paper.focus = ifelse(is.na(targeted), "Biodiversity", targeted),
         paper.focus = ifelse(paper.focus %in% "yes", "Fisheries", paper.focus),
         paper.focus = ifelse(paper.focus %in% "no", "Biodiversity", paper.focus)) %>% 
  group_by(paper.id) %>% 
  mutate(any.fisheries = if_else(any(paper.focus %in% "Fisheries"), "Yes", "No")) %>% 
  ungroup() %>% 
  filter(any.fisheries %in% "No") %>% 
  distinct(paper.id, .keep_all=T) 
  

# Four biodiversity only papers to be removed

length(unique(full.data$paper.id))
length(unique(paper.focus$paper.id))

study_dat <- study_dat %>% 
  filter(paper.id %in% paper.focus$paper.id) %>% 
  left_join(published)
length(unique(study_dat$paper.id))

temp <- anti_join(study_dat, study_dat1, by="paper.id")

#### Single value stats ----
length(unique(study_dat$paper.id))
# 73 papers 

length(unique(study_dat$mpa.name))
# 74 MPAs

# Study type 
study_dat %>% 
  distinct(paper.id, .keep_all=T) %>% 
  {table(.$study.design)}

# BACI   IC 
# 22     53 

# Type of analysis/results
study_dat %>% 
  distinct(paper.id, .keep_all=T) %>% 
  {table(.$result.group)}

# distance     time 
# 56           20 

# Combined table 
study_dat %>% 
  distinct(paper.id, .keep_all=T) %>% 
  {table(.$result.group, .$study.design)}

#            BACI IC
# distance   10   45
# time       12   8

## True counterfactual
study_dat %>% 
  distinct(paper.id, .keep_all=T) %>% 
  {table(.$true.counterfactual)}

# NA  No  Yes 
# 2   33  41 

## Adjusted for habitat
study_dat %>% 
  distinct(paper.id, .keep_all=T) %>% 
  {table(.$habitat.adjusted)}

# NA  No  Yes 
# 9   34  33 

## How adjusted for habitat
study_dat %>% 
  distinct(paper.id, .keep_all=T) %>% 
  {table(.$how.adjusted)}

# NA    Sampling   Statistical 
# 43    14         18 

## How many did both?
study_dat %>% 
  distinct(paper.id, .keep_all=T) %>% 
  {table(.$habitat.adjusted, .$true.counterfactual)}

# Row is habitat adjusted, column is counter-factual
#      NA  No  Yes
# NA   2   1   6
# No   0   17  17
# Yes  0   15  18

## Types of data 
metrics <- full.data %>% 
  filter(paper.id %in% study_dat$paper.id) %>% 
  distinct(paper.id, metric.group, .keep_all=T) %>% 
  mutate(metric.group = ifelse(metric.group %in% "Other" & metric %in% "Weight Per Unit Effort", "Fisheries catch", metric.group)) %>% 
  mutate(metric.group2 = ifelse(metric.group %in% c("Abundance", "Assemblage", "Biomass", "Density", "Recruitment", "Other"), "Ecological", "Fisheries"))

table(metrics$metric.group)

metrics %>%  
  group_by(paper.id) %>% 
  summarize(metric.group3 = case_when(
    all(c("Ecological", "Fisheries") %in% metric.group2) ~ "both",
    "Ecological" %in% metric.group2                ~ "ecological only",
    "Fisheries" %in% metric.group2                 ~ "Fisheries only",
    TRUE                                  ~ "neither"
  )) %>% 
  {table(.$metric.group3)}
  



## World regions
setwd(dat.dir)
un.region <- read.csv("world-regions-according-to-un.csv")

temp <-study_dat %>% 
  mutate(country = ifelse(country %in% "United States of America", "United States", country),
         country = ifelse(country %in% "United Kingdom (Cayman Islands)", "Cayman Islands", country),
         country = ifelse(country %in% "St. Lucia", "Saint Lucia", country),
         country = ifelse(country %in% "Scotland", "United Kingdom", country)) %>% 
  left_join(un.region, by=c("country"="Entity")) %>% 
  mutate(World.regions.according.to.UN = ifelse(country %in% "United States of America Minor Outlying Islands (the)", "Oceania (UN)", World.regions.according.to.UN ))

temp %>% 
  distinct(paper.id, country, .keep_all=T) %>% 
  {table(.$World.regions.according.to.UN)}

## Bioregion
study_dat %>% 
  distinct(paper.id, bioregion, .keep_all=T) %>% 
  {table(.$bioregion)}

#### Types of sampling design ---
sampling.design.dat <- study_dat %>% 
  filter(grepl("Purposive|purposive", site.selection)) %>% 
  distinct(paper.id, .keep_all=T) %>% 
  dplyr::select(paper.id, mpa.name, site.selection, site.placement, stratification, sample.selection) %>% 
  separate_longer_delim(cols=site.selection, delim=",") %>% 
  mutate(across(c(site.selection, site.placement, stratification, sample.selection), ~tolower(.x))) %>% 
  mutate(across(
    .cols = c(site.placement, stratification, sample.selection),              # 1. Target columns to change
    .fns = ~ if_else(site.selection %in% c("opportunistic/non-standard", " opportunistic/non-standard"), NA, .x) # 2. Condition on the OTHER column
  )) %>% 
  pivot_longer(cols=c(site.placement, stratification, sample.selection), names_to = "sampling.design", values_to="description") %>% 
  filter(!is.na(description)) 

study_dat %>% 
  filter(sample.selection %in% "not mentioned"|site.placement %in% "not mentioned") %>% 
  {length(unique(.$paper.id))}


sampling.design.dat %>% 
  #filter(sampling.design %in% "sample.selection") %>% 
  distinct(paper.id, .keep_all=T) %>% 
  {table(.$description)}

study_dat %>% 
  filter(site.selection %in% "Purposive") %>% 
  # filter(site.placement %in% "not mentioned"|sample.selection %in% "not mentioned") %>% 
  distinct(paper.id) %>% 
  count()

table(sampling.design.dat$sampling.design, sampling.design.dat$description)
table(sampling.design.dat$paper.id)

sampling.design.plot <- sampling.design.dat %>% 
  filter(site.selection %in% "purposive") %>% 
  filter(!sampling.design %in% "stratification") %>% 
  mutate(description = str_to_sentence(description)) %>% 
  mutate(description = fct_relevel(description,"Targeted", "Haphazard", "Systematic", "Random", "Not mentioned")) %>% 
  mutate(sampling.design = fct_relevel(sampling.design, "site.placement", "sample.selection")) %>% 
  ggplot(.) +
  geom_bar(aes(x=sampling.design, fill=description), position = "fill") +
  scale_fill_manual(values=c("#332288", "#88CCEE", "#44AA99","#117733", "grey70")) +
  scale_x_discrete(labels = c("Site choice", "Sample location\nchoice")) +
  scale_y_continuous(labels = c(0, 25, 50, 75, 100)) +
  xlab(NULL) +
  ylab("Percentage of papers (%)") +
  theme_classic() +
  theme(legend.title = element_blank())
sampling.design.plot

setwd(fig.dir)
ggsave(sampling.design.plot, filename="Sampling_designs.png",height = a4.width, width = a4.width, units  ="mm", dpi = 300 )


# Plot of data sources 
data.sources <- study_dat %>% 
  distinct(paper.id, .keep_all=T) %>% 
  mutate(surveys.fishers = ifelse(grepl("surveys|Surveys|observer|Interviews", sampling.method), 1, 0),
         ecological.sampling = ifelse(grepl("collectors|Quadrats|stereo-BRUV|UVC", sampling.method), 1, 0),
         experimental.fishing = ifelse(grepl("Experimental|experimental", sampling.method), 1, 0),
         historical.data = ifelse(grepl("historical", sampling.method), 1, 0),
         fisheries.data = ifelse(grepl("AIS|world records|log-book|Log-book|VMS", sampling.method), 1, 0)) %>% 
  pivot_longer(cols=c(surveys.fishers,experimental.fishing, ecological.sampling, historical.data, fisheries.data), names_to="category", values_to="used") %>% 
  mutate(data.source = ifelse(data.source %in% "Primary, secondary" & category %in% c("ecological.sampling", "experimental.fishing", "surveys.fishers"), "Primary",
                             ifelse(data.source %in% "Primary, secondary" & category %in% c("historical.data", "fisheries.data"), "Secondary", data.source))) 

data.sources %>% 
  group_by(paper.id) %>% 
  mutate(data.source = ifelse(any(data.source == "Primary") & any(data.source == "Secondary"), "Both", as.character(data.source))) %>% 
  distinct(paper.id, .keep_all=T) %>% 
  {table(.$data.source)}


data.sources.prop <- data.sources %>% 
  filter(used==1) %>%  
  count(category, name = "n_papers_using") %>%
  mutate(
    pct = n_papers_using / 71 * 100,
    parent_group = case_when(
      category %in% c("ecological.sampling", "surveys.fishers", "experimental.fishing") ~ "Primary",
      category %in% c("historical.data", "fisheries.data") ~ "Secondary"),
    category = factor(category,
                      levels = c("ecological.sampling", "experimental.fishing", "surveys.fishers", "historical.data", "fisheries.data"),
                      labels = c("Ecological\nsampling", "Experimental\nfishing", "Surveys\nof fishers", "Historical\nmonitoring", "Fishery-dependent\ndata")))


data.sources.plot <- data.sources.prop %>% 
  ggplot() +
  geom_col(aes(x = category, y = pct, fill = parent_group), width = 0.65) +
  scale_fill_manual(values = c("Primary" = "#117733", "Secondary" = "#332288")) +
  labs(
    x = NULL,
    y = "Percentage of papers (%)",
    fill = "Data source"
  ) +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
data.sources.plot

setwd(fig.dir)
ggsave(data.sources.plot, filename="Data_sources.png", height = a4.width, width = a4.width, units  ="mm", dpi = 300)

### Counter factuals ----

counterfactual <- study_dat %>% 
  mutate(across(where(is.character), ~na_if(., "NA"))) %>% 
  dplyr::select(paper.id, mpa.name, study.design, true.counterfactual, counterfactual.type, counterfactual.justification, counterfactual.strength) %>% 
  separate_longer_delim(cols=counterfactual.type, delim=",") %>% 
  mutate(counterfactual.type = ifelse(counterfactual.type %in% c("near/far design", "near/far"), "Discrete\nspatial zones", counterfactual.type),
         counterfactual.type = ifelse(counterfactual.type %in% c("combined spatial and temporal controls"), "Combined temporal\nand spatial data", counterfactual.type),
         counterfactual.type = ifelse(counterfactual.type %in% c(" spatial gradient", "distance gradient", "spatial gradient"), "Spatial\ngradient", counterfactual.type)) %>% 
  mutate(counterfactual.strength.group = fct_recode(counterfactual.strength, Strong="high", Moderate="moderate",Weak="low", Weak="weak")) %>% 
  filter(!is.na(counterfactual.type)) %>% 
  distinct(paper.id, counterfactual.type, .keep_all=T)

table(counterfactual$counterfactual.type, counterfactual$counterfactual.strength.group)

unique(study_dat$counterfactual.justification)

counterfactual.papers <- counterfactual %>% 
  group_by(paper.id) %>% 
  mutate(any.strong = if_else(any(counterfactual.strength.group %in% "Strong"), "Yes", "No")) %>% 
  distinct(paper.id, .keep_all=T)

table(counterfactual.papers$any.strong)
  
counterfactual.plot <- counterfactual %>% 
  #mutate(counterfactual.type = str_to_sentence(counterfactual.type)) %>% 
  mutate(counterfactual.type = fct_relevel(counterfactual.type, "Discrete\nspatial zones", "Spatial\ngradient", "Combined temporal\nand spatial data")) %>% 
  mutate(counterfactual.strength.group = fct_relevel(counterfactual.strength.group, "Weak", "Moderate", "Strong")) %>% 
  ggplot()+
  geom_bar(aes(x=counterfactual.type, fill=counterfactual.strength.group), position="fill")+
  scale_y_continuous(labels = percent)+
  scale_fill_manual(values = c("#88CCEE", "#44AA99", "#332288"), name="Counterfactual strength")+
  xlab(NULL)+
  ylab("Percentage of papers")+
  theme_classic()+
  theme(axis.text = element_text(family="sans"),
        legend.position = "bottom")+
  guides(fill=guide_legend(title.position = "top", title.hjust=0.5))+
  theme(
    # 1. Shrink the legend text and title
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    
    # 2. Shrink the physical size of the legend keys (the symbols)
    legend.key.size = unit(0.4, "cm"),
    
    # 3. Reduce spacing around the legend to make the box tighter
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt"),
    legend.spacing.x = unit(0.1, "cm")
  )
counterfactual.plot

setwd(fig.dir)
ggsave(counterfactual.plot, filename="Counterfactuals.png",height = a4.width, width = a4.width*0.75, units  ="mm", dpi = 300 )


#### How was habitat accounted for? ----

habitat <- study_dat %>% 
  dplyr::select(paper.id, mpa.name,year.published, data.source, habitat.adjusted, how.adjusted, habitat.restriction) %>% 
  filter(data.source %in% c("Primary", "Primary, secondary")) %>% 
  mutate(how.adjusted = ifelse(habitat.adjusted %in% "No", "None", how.adjusted)) %>% 
  mutate(how.adjusted = ifelse(habitat.restriction %in% "yes" & habitat.adjusted %in% "No", "Sampling", how.adjusted)) %>% 
  mutate(across(where(is.character), ~na_if(., "NA"))) %>% 
  distinct(paper.id, year.published, .keep_all=T) %>% 
  filter(!is.na(habitat.adjusted)) 
  

table(habitat$how.adjusted)


habitat.plot <- habitat %>%
  count(year.published, how.adjusted) %>%
  mutate(how.adjusted = fct_relevel(how.adjusted, "Sampling", "Statistical", "None")) %>% 
  ggplot(aes(x = year.published, y = n, fill = how.adjusted)) +
  geom_col() +
  labs(
    x = "Year published",
    y = "Number of papers",
    fill = "Habitat adjustment"
  ) +
  scale_fill_manual(values=c("None"="#332288", "Sampling"="#88CCEE", "Statistical"="#44AA99"))+
  theme_classic()+
  theme(axis.text = element_text(family="sans"),
        legend.position = "bottom")+
  guides(fill=guide_legend(title.position = "top", title.hjust=0.5))+
  theme(
    # 1. Shrink the legend text and title
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    
    # 2. Shrink the physical size of the legend keys (the symbols)
    legend.key.size = unit(0.4, "cm"),
    
    # 3. Reduce spacing around the legend to make the box tighter
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt"),
    legend.spacing.x = unit(0.1, "cm")
  )
habitat.plot

# 
# habitat.plot.perc <- habitat %>%
#   count(year.published, how.adjusted) %>%
#   mutate(how.adjusted = fct_relevel(how.adjusted, "Sampling", "Statistical", "None")) %>% 
#   ggplot(aes(x = year.published, y = n, fill = how.adjusted)) +
#   geom_col(position = "fill") +
#   labs(
#     x = "Year published",
#     y = "Proportion of papers",
#     fill = "Habitat adjustment"
#   ) +
#   scale_y_continuous(labels = scales::percent) +
#   scale_fill_manual(values=c("None"="#332288", "Sampling"="#88CCEE", "Statistical"="#44AA99"))+
#   theme_classic()+
#   theme(axis.text = element_text(family="sans"),
#         legend.position = "bottom")+
#   guides(fill=guide_legend(title.position = "top", title.hjust=0.5))
# habitat.plot.perc

p1 <- counterfactual.plot
p2 <- habitat.plot

# Explicitly assign each plot to its position
layout <- "
123
123
"

controlling.confounds <- p1 + plot_spacer() + p2 + 
  plot_layout(design = layout, heights = c(1, 1, 1), widths=c(1,0.1,1))
controlling.confounds

setwd(fig.dir)
ggsave(controlling.confounds, filename="Counterfactuals_habitat.png",height = a4.width, width = a4.width*1.2, units  ="mm", dpi = 300 )

#### Species studied and metrics used ----

species.list <- full.data %>% 
  filter(paper.id %in% study_dat$paper.id) %>% 
  filter(!is.na(species)) %>% 
  filter(!grepl("spp.", species))

length(unique(species.list$full.name))

species.list %>% 
  distinct(full.name, .keep_all=T) %>% 
  {table(.$broad.group)}

genus.list <- full.data %>% 
  filter(paper.id %in% study_dat$paper.id) %>% 
  filter(is.na(species)|grepl("spp.", species)) %>% 
  filter(!is.na(genus))

genus.list %>% 
  distinct(full.name, .keep_all=T) %>% 
  {table(.$broad.group)}

family.list <- full.data %>% 
  filter(paper.id %in% study_dat$paper.id) %>% 
  filter(is.na(genus))

family.list %>% 
  distinct(family, .keep_all=T) %>% 
  {table(.$broad.group)}

target.list <- full.data %>% 
  filter(paper.id %in% study_dat$paper.id) %>% 
  group_by(paper.id) %>% 
  summarize(target.group = case_when(
    all(c("yes", "no") %in% targeted) ~ "both",
    "yes" %in% targeted                ~ "yes only",
    "no" %in% targeted                 ~ "no only",
    TRUE                                  ~ "neither"
  ))
table(target.list$target.group)
# 367 target only, 676 both, 772 target only, 778 target only, 865 both, 1026 target only, 1033 target only, 1531 both

# both  neither yes only 
# 5        0       65
