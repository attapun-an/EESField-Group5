# Stats Analysis
# coder: attapun-an (attapunanivat@gmail.com)


# Working directory is set to root directory (one above the script folder)
rm(list=ls())  # clear memory

# load libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(bbplot)
library(ggeffects)


# import data ----
Hemi_Data <- read.csv("output/image-analysis-output.csv")
Plot_Data <- read.csv("data/Metadata.csv")

#check data
head(Hemi_Data)
str(Hemi_Data)
head(Plot_Data)
str(Plot_Data)

# data cleaning and alignment ----
Hemi_Data_1 <- Hemi_Data %>% 
  select(File, CanOpen, LAI) %>%                                                # remove unwanted rows
  rename(File.name = File)                                                      

Plot_Data_1 <- Plot_Data %>%
  select(Overstorey.Species, Number.of.trees.in.plot, File.name, Alpha.Diversity) %>% 
  mutate(File.name = paste("EE5_",File.name,".JPG", sep = "")) %>% 
  mutate(Overstorey.Species = case_when(
    grepl("Scots", substr(Overstorey.Species,1,5)) ~ "Scots pine",
    grepl("Sitka", substr(Overstorey.Species,1,5)) ~ "Sitka spruce", 
    grepl("Larch", substr(Overstorey.Species,1,5))~"Larch"))

Combined_Data <- left_join(Plot_Data_1, Hemi_Data_1, by="File.name")
Combined_Data$Overstorey.Species <- as.factor(Combined_Data$Overstorey.Species)

str(Combined_Data)
head(Combined_Data)
# Model ----

# Canopy Openness vs. Species Richness
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
