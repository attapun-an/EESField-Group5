# Stats Analysis
# coder: attapun-an (attapunanivat@gmail.com)


# Working directory is set to root directory (one above the script folder)
rm(list=ls())  # clear memory

# load libraries ----
library(dplyr)      # for data manipulation
library(tidyr)
library(stringr)
library(ggplot2)    # to plot data
library(ggeffects)  # in case we have a mixed effect model? (not used as of now)


# import data ----
Hemi_Data <- read.csv("output/image-analysis-output.csv")           # imports data from our LAI calcs
Plot_Data <- read.csv("data/Metadata.csv")                          # imports data from the field
Species_Data <- read.csv("data/Modded Understorey diversity counts.csv")

#check data
head(Hemi_Data)
str(Hemi_Data)

head(Plot_Data)
str(Plot_Data)

head(Species_Data)
str(Species_Data)
# data cleaning and alignment ----

Hemi_Data_1 <- Hemi_Data %>% 
  select(File, CanOpen, LAI) %>%       # remove unwanted rows
  rename(File.name = File)             # rename so that the two have something in 
                                       # common that can be used to join them 
                                       
Plot_Data_1 <- Plot_Data %>%
  select(Overstorey.Species, Number.of.trees.in.plot, File.name, Alpha.Diversity) %>%    # remove unwanted rows
  mutate(File.name = paste("EE5_",File.name,".JPG", sep = "")) %>%                       # file number to file name so both are the same, helps with joining
  mutate(Overstorey.Species = case_when(                               # creates 3 categories for dominant vegetation
    grepl("Scots", substr(Overstorey.Species,1,5)) ~ "Scots pine",     
    grepl("Sitka", substr(Overstorey.Species,1,5)) ~ "Sitka spruce", 
    grepl("Larch", substr(Overstorey.Species,1,5))~"Larch"))

Species_Data_1 <- Species_Data %>%
  rename(Plot.Number = X) %>% 
  pivot_longer(!Plot.Number, names_to = "Names", values_to = "Presence") %>% 
  mutate(Group = substring(Names,1,1), Species = substring(Names,2)) %>% 
  mutate(Group = case_when(
    grepl("B", Group) ~ "Bryophyte",
    grepl("F", Group) ~ "Fungi",
    grepl("P", Group) ~ "Vascular.Plant",
    grepl("L", Group) ~ "Lichen")) %>% 
  select(!Names) %>% 
  mutate(Presence = as.logical(Presence)) %>% 
  group_by(Plot.Number,Group) %>% 
  summarise_if(isTRUE(Presence)), n()))


  
head(Species_Data_1)
str(Species_Data_1)

# combines the two data tables
Combined_Data <- left_join(Plot_Data_1, Hemi_Data_1, by="File.name")        
Combined_Data$Overstorey.Species <- as.factor(Combined_Data$Overstorey.Species) # changes data type of Overstorey.species to a factor 

str(Combined_Data)
head(Combined_Data)
# Model ----

# Canopy Openness vs. Species Richness
# note (dependent ~ Independent)
m1 <- lm(formula = Alpha.Diversity ~ CanOpen, data = Combined_Data)
summary(m1)
plot(m1) # plot residuals


# 
m2 <- lm(formula = CanOpen ~ Number.of.trees.in.plot, data = Combined_Data)
summary(m2)
plot(m2)


# VIsualize Data ----
(CanOpenvsRichness_Plot <- ggplot(Combined_Data, aes(x = CanOpen, y = Alpha.Diversity))+
    geom_point(aes(colour = Overstorey.Species))+
    geom_smooth(method = lm, color = "#A3A1A8")+
    theme_bw()
    )

(StockvsCanOpen_Plot <- ggplot(Combined_Data, aes(x = Number.of.trees.in.plot, y = CanOpen))+
    geom_point(aes(colour = Overstorey.Species))+
    geom_smooth(method = lm, color = "#A3A1A8")+
    theme_bw()
    )

# Save Plots ----
ggsave("output/plots/plot_CanOpenvsRichness.jpg", CanOpenvsRichness_Plot)
ggsave("output/plots/plot_StockvsCanOpen.jpg", StockvsCanOpen_Plot)
