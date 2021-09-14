# Stats Analysis
# coder: attapun-an (attapunanivat@gmail.com)


# Working directory is set to root directory (one above the script folder)
rm(list=ls())  # clear memory

# Install packages ----
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("MASS")
install.packages("sfsmisc")
install.packages("stargazer")
install.packages("lme4")

# load libraries ----
library(dplyr)      # for data manipulation
library(tidyr)
library(stringr)
library(ggplot2)    # to plot data
library(ggeffects)  # in case we have a mixed effect model? (not used as of now)
library(MASS)       # to run robust linear models
library(sfsmisc)
library(stargazer)
library(lme4)

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
  dplyr::select(File, CanOpen, LAI) %>%       # remove unwanted rows
  rename(File.Name = File)             # rename so that the two have something in 
                                       # common that can be used to join them 
                                       
Plot_Data_1 <- Plot_Data %>%
  mutate(File_name = paste("EE5_",File_name,".JPG", sep = ""),         # file number to file name so both are the same, helps with joining
         Plot_number = Plot_number) %>%                                #
  mutate(Stand.Type = case_when(
    nchar(as.character(Overstorey_Species)) > 12 ~ "Mixed",
    TRUE ~ "Pure"
  )) %>% 
  mutate(Overstorey_Species = case_when(                               # creates 3 categories for dominant vegetation
    grepl("Scots", substr(Overstorey_Species,1,5)) ~ "Scots pine",     
    grepl("Sitka", substr(Overstorey_Species,1,5)) ~ "Sitka spruce", 
    grepl("Larch", substr(Overstorey_Species,1,5))~"Larch")) %>% 
  rename(File.Name = File_name, Plot.Number = Plot_number, Overstorey.Species = Overstorey_Species,
         Stem.Count = Stems_in_10m_radius_circular.plot, Grid.Ref = Grid_ref., Mean.Reading = Mean_reading.,
         pH.Readings = pH_readings, Soil.Moisture_1 = Soil_moisture_1, Soil.Moisture_2 = Soil_moisture_2,
         Soil.Moisture_3 = Soil_moisture_3, Soil.Moisture_4 = Soil_moisture_4)


str(Plot_Data_1)
count(Plot_Data_1, Stand.Type)


Species_Data_1 <- Species_Data %>%
  rename(Plot.Number = X) %>%                                                      
  pivot_longer(!Plot.Number, names_to = "Names", values_to = "Presence") %>%    # pivot data into long form
  mutate(Group = substring(Names,1,1), Species = substring(Names,2)) %>%        # Split Group and Species Name (They were put together in the mod because R can't handle two headers)
  mutate(Group = case_when(                                                     
    grepl("B", Group) ~ "Bryophytes",         # restore group names
    grepl("F", Group) ~ "Fungi",
    grepl("P", Group) ~ "Vascular.Plants",
    grepl("L", Group) ~ "Lichens")) %>% 
  dplyr::select(!Names) %>%                          # remove the Names column
  drop_na() %>%                                      # remove that Fkin row that took 3 hours being a pain in the a** (it was empty)
  group_by(Plot.Number, Group) %>%            
  summarise(n = sum(Presence)) %>%              
  pivot_wider(names_from = Group, values_from = n) %>% 
  relocate(Vascular.Plants, .after = Bryophytes)

# combines the three data tables
Combined_Data <- left_join(Plot_Data_1, Hemi_Data_1, by="File.Name") 
Combined_Data <- left_join(Combined_Data, Species_Data_1, by="Plot.Number") %>% 
  relocate(Plot.Number) %>% 
  mutate(Alpha.Diversity = Bryophytes + Vascular.Plants + Fungi + Lichens, 
         Soil.Moisture.Mean = (Soil.Moisture_1 + Soil.Moisture_2 + Soil.Moisture_3 + Soil.Moisture_4)/4,
         Stocking.Density = Stem.Count/(pi*100))

Combined_Data$Overstorey.Species <- as.factor(Combined_Data$Overstorey.Species) # changes data type of Overstorey.species to a factor 
Combined_Data$Transect <- as.factor(Combined_Data$Transect)

str(Combined_Data)
head(Combined_Data)
write.csv(Combined_Data, "output/main_analysis/Combined_Data.csv")

SpeciesSplit_Data <- Combined_Data %>% 
  dplyr::select(CanOpen, LAI, Bryophytes, Vascular.Plants, Fungi, Lichens) %>% 
  pivot_longer(-c(CanOpen, LAI), names_to = "Group", values_to = "Count")

head(SpeciesSplit_Data)

# Check the normality of the data ----
# Alpha Diversity
(hist <- ggplot(Combined_Data, aes(x = Alpha.Diversity)) +
   geom_histogram() +
   theme_bw())
# normality test (<0.05 = higher chance data not normal)
shapiro.test(Combined_Data$Alpha.Diversity)  
# bi-modal
ggsave("output/main_analysis/hist_richness.jpg")


# Canopy openness
(hist <- ggplot(Combined_Data, aes(x = CanOpen)) +
    geom_histogram() +
    theme_classic())
shapiro.test(Combined_Data$CanOpen)

# Stem count
(hist <- ggplot(Combined_Data, aes(x = Stem.Count)) +
    geom_histogram() +
    theme_classic())
shapiro.test(Combined_Data$CanOpen)

# Models ----

# PLUG IT ALL IN AHAHAHHAHAHAHHAHA (to check which factor is the most effect argh english.exe crashed)
modl_ALL <- lm(formula = Alpha.Diversity ~ CanOpen + Stem.Count + Soil.Moisture.Mean, data = Combined_Data)
summary(modl_ALL)
stargazer(modl_ALL, out = "output/main_analysis/Modl_ALL.txt",
          title = "Multiple Regression With All Predictor Variables vs Alpha Diversity" , 
          type = "text", report=("vc*p"))

# MIXED EFFECTS MODEL (to check for spatial auto correlation within transects)
modl_ALL_MIXED <- lmer(formula = Alpha.Diversity ~ CanOpen +Stem.Count + Soil.Moisture.Mean + pH.Readings + (1|Transect), 
                       data = Combined_Data)
summary(modl_ALL_MIXED)
# Yay, the transect is not responsible for any varience 


# Canopy Openness vs. Species Richness
# note (dependent ~ Independent)
modl_CanOpen <- lm(formula = Alpha.Diversity ~ CanOpen, data = Combined_Data)
summary(modl_CanOpen)
plot(modl_CanOpen) # plot residuals
f.robftest(modl_CanOpen)
stargazer(modl_CanOpen, out = "output/main_analysis/Modl_CanOpen.txt",
          type = "text", report=("vc*p"))

weights_CanOpen <- data.frame(Plot.Number = Combined_Data$Plot.Number, Residuals = modl_CanOpen$residuals,
                         Weight = modl_CanOpen$w) %>% 
  arrange(Weight);weights_CanOpen



# Model Canopy openness vs Stocking Density
modl_StkDen <- lm(formula = CanOpen ~ Stocking.Density, data = Combined_Data)
summary(modl_StkDen)
plot(modl_StkDen)
f.robftest(modl_StkDen)
stargazer(modl_StkDen, out = "output/main_analysis/modl_StkDen.txt",
          title = "Canopy Openness VS Stem Count",
          type = "text", report=("vc*p"))

m2_weights <- data.frame(Plot.Number = Combined_Data$Plot.Number, Residuals = modl_StkDns$residuals,
                         Weight = modl_StkDns$w) %>% 
  arrange(Weight); m2_weights


# Model pH vs Richness
modl_pH <- lm(formula = Alpha.Diversity ~ pH.Readings, data = Combined_Data)
modl_pH <- rlm(formula = Alpha.Diversity ~ pH.Readings, data = Combined_Data)
summary(modl_pH)
plot(modl_pH)
f.robftest(modl_pH)
stargazer(modl_pH, out = "output/main_analysis/Modl_pH.txt",
          type = "text", report=("vc*p"))


# Model Soil Moisture vs Richness
modl_SM <- lm(formula = Alpha.Diversity ~ Soil.Moisture.Mean, data = Combined_Data)
summary(modl_SM)
plot(modl_SM)
f.robftest(modl_SM)
stargazer(modl_SM, out = "output/main_analysis/Modl_SM.txt",
          type = "text", report=("vc*p"))

modl_Bryo <- lm(formula = Bryophytes ~ CanOpen, data = Combined_Data)
summary(modl_Bryo)
plot(modl_Bryo)
f.robftest(modl_Bryo)
stargazer(modl_Bryo, out = "output/main_analysis/Modl_Split_Bryo.txt",
          type = "text", report=("vc*p"))

modl_Vasc <- lm(formula = Vascular.Plants ~ CanOpen, data = Combined_Data)
summary(modl_Vasc)
plot(modl_Vasc)
f.robftest(modl_Vasc)
stargazer(modl_Vasc, out = "output/main_analysis/Modl_Split_Vasc.txt",
          type = "text", report=("vc*p"))

modl_Fung <- lm(formula = Fungi ~ CanOpen, data = Combined_Data)
summary(modl_Fung)
plot(modl_Fung)
f.robftest(modl_Fung)
stargazer(modl_Fung, out = "output/main_analysis/Modl_Split_Fung.txt",
          type = "text", report=("vc*p"))


# Visualize Data ----

# CanOpen
(CanOpenvsRichness_Plot <- ggplot(Combined_Data, aes(x = CanOpen, y = Alpha.Diversity))+
    geom_point(aes(colour = Overstorey.Species))+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_smooth(method = MASS::rlm, color = "#A3A1A8")+
    xlab("\n Canopy Openness")+
    ylab("Understorey Species Richness \n")+
    labs(colour = "Overstorey Species")+
    scale_colour_manual(values = c("#45FB93", "#BC49FF", "#E0765A"))+
    scale_fill_manual(values =c("#BAF3D2")) +
    theme_bw()+
    theme(axis.title = element_text(size=12))
 )
ggsave("output/main_analysis/plt_CanOpen.jpg",CanOpenvsRichness_Plot, width = 8.2, height = 5.16, units = "in")

# CanOpen Stand Type Separated
(CanOpenvsType_Plot <- ggplot(Combined_Data, aes(x = CanOpen, y = Alpha.Diversity))+
    geom_point(aes(colour = Overstorey.Species, shape = Stand.Type))+
    scale_x_continuous(expand = c(0, 0.02)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_smooth(method = MASS::rlm, color = "#A3A1A8")+
    xlab("\n Canopy Openness")+
    ylab("Understorey Species Richness \n")+
    labs(colour = "Overstorey Species")+
    scale_colour_manual(values = c("#45FB93", "#BC49FF", "#E0765A"))+
    scale_fill_manual(values =c("#BAF3D2")) +
    geom_text(aes(x = CanOpen, y = Alpha.Diversity, label= Plot.Number),
              nudge_y = 0.5)+
    theme_bw()+
    theme(axis.title = element_text(size=12))
)
ggsave("output/main_analysis/plt_CanOpen_Stand_Type.jpg",CanOpenvsType_Plot, width = 8.2, height = 5.16, units = "in")

# Stem vs CanOpen
(StockvsCanOpen_Plot <- ggplot(Combined_Data, aes(x = Stocking.Density, y = CanOpen))+
    geom_point(aes(colour = Overstorey.Species))+
    geom_smooth(method = MASS::rlm, color = "#A3A1A8")+
    scale_colour_manual(values = c("#45FB93", "#BC49FF", "#E0765A"))+
    xlab("\n Stocking Density")+
    ylab("Canopy Openness \n")+
    labs(colour = "Overstorey Species")+
    theme_bw()+
    theme(axis.title = element_text(size=12))
    )
ggsave("output/main_analysis/plt_StockvsCanOpen.jpg", StockvsCanOpen_Plot, width = 8.2, height = 5.16, units = "in")


(pHvsRichness_Plot <- ggplot(Combined_Data, aes(x = pH.Readings, y = Alpha.Diversity))+
    geom_point(aes(colour = Overstorey.Species))+
    geom_smooth(method = MASS::rlm, color = "#A3A1A8")+
    theme_bw()+
    theme(axis.title = element_text(size=12))
)
ggsave("output/main_analysis/plt_pH.jpg", pHvsRichness_Plot, width = 8.2, height = 5.16, units = "in")


(SoilMoisturevsRichness_Plot <- ggplot(Combined_Data, aes(x = Soil.Moisture.Mean, y = Alpha.Diversity))+
    geom_point(aes(colour = Overstorey.Species))+
    geom_smooth(method = MASS::rlm, color = "#A3A1A8")+
    theme_bw()+
    theme(axis.title = element_text(size=12))
)
ggsave("output/main_analysis/plt_SoilMoist.jpg", SoilMoisturevsRichness_Plot, width = 8.2, height = 5.16, units = "in")


# set colours:
Cols_Grp <- c("#B7F500", "#E09800", "#00E097", "#FA2100")

(CanOpenvsCount_Plot <- ggplot(SpeciesSplit_Data, aes(x = CanOpen, y = Count))+
  geom_point(aes(colour = Group))+
  coord_cartesian(ylim=c(0,9)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_smooth(method = MASS::rlm, aes(fill = Group, colour = Group))+
  scale_colour_manual(values = Cols_Grp) +
  scale_fill_manual(values = Cols_Grp) +
  xlab("\n Canopy Openness")+
  ylab("Count \n")+
  theme_bw()+
  theme(axis.title = element_text(size=12))
)
ggsave("output/main_analysis/plt_CanOpen_split.jpg", CanOpenvsCount_Plot, width = 8.2, height = 5.16, units = "in")
