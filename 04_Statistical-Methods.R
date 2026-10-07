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
library(patchwork)

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
study_dat <- read_xlsx("cleaned_methods_study_dat.xlsx") %>%   # This has metadata attached 
  filter(!result.group %in% "genetics")
setwd(dat.dir)
published <- read_xlsx("Year_Published.xlsx") %>% 
  dplyr::select(paper.id, year.published)

study_dat <- study_dat %>% 
  left_join(published)

#### Main methods used ---
# I think I want this over time as well - have we changed how we think about spillover form?
methods <- study_dat %>% 
  distinct(paper.id, .keep_all=T) %>% 
  mutate(spatial.group = ifelse(spatial.analysis.capacity %in% "discrete zone", "Discrete zone",
                                ifelse(grepl("fixed form",spatial.analysis.capacity), "Fixed form",
                                       ifelse(grepl("flexible", spatial.analysis.capacity), "Flexible form", NA)))) %>% 
  mutate(spatial.group = ifelse(grepl("but tested|but specifies|both linear|linear regression first", spatial.analysis.capacity), "Flexible form", spatial.group)) %>% 
  filter(!is.na(spatial.group))


table(methods$spatial.group)




methods.plot <- methods %>%
  count(year.published, spatial.group) %>%
  mutate(spatial.group = fct_relevel(spatial.group, "Flexible form", "Fixed form", "Discrete zone")) %>% 
  ggplot(aes(x = year.published, y = n, fill = spatial.group)) +
  geom_col() +
  labs(
    x = "Year published",
    y = "Number of papers",
    fill = "Type of spatial analysis"
  ) +
  scale_fill_manual(values=c("Discrete zone"="#332288", "Fixed form"="#88CCEE", "Flexible form"="#44AA99"))+
  theme_classic()
methods.plot

setwd(fig.dir)
ggsave(methods.plot, filename="stat_methods_yearly.png",height = a4.width, width = a4.width, units  ="mm", dpi = 300 )

## NEED PLOT FROM SUMMARY STATS SCRIPT TO BE LOADED IN ENVIRONMENT
p1 <- data.sources.plot +
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
p2 <- methods.plot +
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

# Explicitly assign each plot to its position
layout <- "
123
123
"

data.sources.x.stats.methods <- p1 + plot_spacer() +  p2 + 
  plot_layout(design = layout, heights = c(1,1,1), widths = c(1,0.1,1))
data.sources.x.stats.methods

setwd(fig.dir)
ggsave(data.sources.x.stats.methods, filename="Data-source_stats-methods.png",height = a4.width, width = a4.width*1.1, units  ="mm", dpi = 300 )

methods.total.papers <- methods %>% 
  group_by(year.published) %>% 
  summarise(n.papers = n())

methods.plot.perc <- methods %>%
  count(year.published, spatial.group) %>%
  mutate(spatial.group = fct_relevel(spatial.group, "Flexible form", "Fixed form", "Discrete zone")) %>% 
  ggplot(aes(x = year.published, y = n, fill = spatial.group)) +
  geom_col(position = "fill") +
  labs(
    x = "Year published",
    y = "Proportion of papers",
    fill = "Habitat adjustment"
  ) +
  geom_text(data=methods.total.papers, aes(x=year.published, y=1, label=n.papers), size=2.5, inherit.aes = FALSE, vjust=-0.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("Discrete zone"="#332288", "Fixed form"="#88CCEE", "Flexible form"="#44AA99"))+
  theme_classic()
methods.plot.perc





## Do we want to know about the statistical methods used?

temp <- study_dat %>% 
  distinct(paper.id, .keep_all=T) %>% 
  filter(grepl("linear regression", test.used, ignore.case=T))
# 15 used linear regression


## What about confounding effect of fisheries
setwd(sg.dir)
confounds <- read_excel("confound_quotes_coding.xlsx") %>% 
  mutate(paper.id = as.double(paper.id)) %>% 
  filter(paper.id %in% study_dat$paper.id) 

study.dat.not.fishing <- study_dat %>% 
  filter(grepl("UVC|Larval|stereo-BRUV|Quadrats", sampling.method)) %>% 
  distinct(paper.id, sampling.method) %>% 
  filter(!grepl("Interviews|experimental fishing|surveys|Experimental fishing|log-book data", sampling.method))

confounds.fishing <- confounds %>% 
  filter(paper.id %in% study.dat.not.fishing$paper.id) %>% 
  distinct(paper.id, confound.type, .keep_all=T)
table(confounds.fishing$confound.type)





