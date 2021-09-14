# EES Field Course Project 5 2021
Coding Repository For EES Field Ecology [ECSC10033] Project 5  
Members: Natasha, Ailsa, Laura, James, Attapun  
Supervisor: Matt    
Coded by attapun-an (attapunanivat@gmail.com)  

#### Summary
Project 5 aims to investigate links between overstorey structure and understory diversity of coniferous forest. Our study site was in Cologin, Oban. Hemispherical Photography was used to calculate the LAI/Canopy Openness; Species richness and other possible confounding variables (pH, soil moisture) was collected for each plot. The images were processed in R to get the LAI/Canopy Openness. All the data was analyzed using linear models.

<img src="images/QR.png" width="150" height="150" />    


#### Index
| Item     | Description     |
| :------------- | :------------- |
| data      | Folder containing digitized versions of data collected in the field in the form of .xls files and .csv versions of those used for import into R   |
| images | contains hemispherical photos taken on the field for import into R |
| output | output folder that contains plots, processed images, data and model outputs from the main analysis and image analysis scripts <ul> <li> **image_test:** processed sample image from field data used to determine which channel and threshold level should be used for the batch script </li> <li> **main_analysis:** plots, model outputs, and data tables from the main analysis (figures to be used for the write up and posters) </li>  <li> **thresholds:** all the processed images and original side by sides to check that the threshold level is acceptable for each image </li> </li></ul>|
| scripts | R scripts used to run the code as well as R scripts from external sources (Hemiphot and hemi_photo_guide) that contain functions that are required to run the analysis scripts: <ul> <li> **image-analysis.R**  script that processes the images and outputs to `output/image-analysis-output.csv` the LAI and Canopy Openness for every image </li> <li> **main_analysis.R** compiles data from the data table and output from image analysis, runs calculations (e.g. averaging the soil moisture), previews the data, runs models on it, and plots graphs.</li>  </ul>|

#### Naming Conventions
| Type  | Description/Example   |
| :------------- | :------------- |
|Folders     |  snake_case |  
|Variables   | Pascal_Snake_Case | NA |
|Columns|Pascal.Case.Period.Seperated|


#### References:
Hans ter Steege (2018). Hemiphot.R: Free R scripts to analyse hemispherical photographs for canopy openness, leaf area index and photosynthetic active radiation under forest canopies.
Unpublished report. Naturalis Biodiversity Center, Leiden, The Netherlands
https://github.com/Naturalis/Hemiphot

johngodlee (2018) A guide to using hemispherical photos for estimation of forest/woodland canopy traits.
https://github.com/johngodlee/hemi_photo_guide


#### Packages and Specification
Coded on R 3.6.2 and 3.6.1   
`dplyr` `tidyr` `stringr` `ggplot2` `MASS` `sfsmisc` `stargazer` `lme4`
