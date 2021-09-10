# Stats Analysis
# coder: attapun-an (attapunanivat@gmail.com)


# Working directory is set to root directory (one above the script folder)
rm(list=ls())  # clear memory

# load libraries ----
library(dplyr)
library(tidyr)


# import data ----
Hemi_Data <- read.csv("output/image-analysis-output.csv")
Plot_Data <- read.csv("data/Metadata.csv")

#check data
head(Hemi_Data)
str(Hemi_Data)
head(Plot_Data)
str(Plot_Data)

# data cleaning and alignment ----
Hemi_Data <- Hemi_Data %>% 
  select(File, CanOpen, LAI) %>% 
  rename(File.name = File)

Plot_Data <- Plot_Data %>%
  select(Overstorey.Species, Number.of.trees.in.plot, File.name) %>% 
  mutate(File.name = paste("EE5_",File.name, sep = ""))
  