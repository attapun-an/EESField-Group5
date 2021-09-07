# Script to analyse data from field images
# Based on HemiphotTest.R Hans ter Steege (2018)
# attapun-an

# Working directory is set to root directory (one above the script folder)
rm(list=ls())  # clear memory


# load libraries and source files ----
library(jpeg)
source("scripts/Hemiphot/Hemiphot.R")



im <- readJPEG("images/field/EE5_2907.JPG", native = F)        # this turns into array

# not sure what the difference is

## convert to class HemiphotImage - adding a circle
im.hemi = Image2Hemiphot(im)
PlotHemiImage(image = im.hemi, draw.circle = T, channel = "")   #note that east and west are reversed as the image looks upward

# adjusting image circle
im.hemi = SetCircle(im.hemi, cx = 237, cy = 237, cr = 228)
PlotHemiImage(im.hemi)


# check channels and choose one with most contrast
PlotHemiImage(im.hemi, draw.circle = T, channel = "R")
PlotHemiImage(im.hemi, draw.circle = T, channel = "G")
PlotHemiImage(im.hemi, draw.circle = T, channel = "B")
# they all look the same -_- (they said blue might be best?)

im.blue = SelectRGB(im.hemi, "B")
PlotHemiImage(im.blue, draw.circle = T)

# Threshold the image ----
threshold = 0.55  # value between 0 and 1
image.th = ThresholdImage(image = im.blue, th = threshold, draw.image = T)

# Calculations of Hemiphot ----

## canopy openess is calculated over 89 circles, each 360 points
## these points can be visualized with draw.circles()

PlotHemiImage(image.th, draw.circle = T) 
DrawCircles(image.th[[2]], image.th[[3]], image.th[[4]])

PlotHemiImage(image.th, draw.circle = T) 


## calculate canopy cover based on canopy openess of the 89 circles
## the openess by circle is stored in gap.fractions
gap.fractions = CalcGapFractions(image.th)                                      # this needs to run before other calcs
canopy.openness = CalcOpenness(fractions = gap.fractions); canopy.openness      # the colon and repeat just displays what it is


## calculate LAI according to Licor's LAI Analyzer 
canopy.LAI = CalcLAI(fractions = gap.fractions, width = 6); canopy.LAI # dependent on gap fractions


# Try to run batch code ----

all.images = list.files("images/test/",pattern = ".jpg")                        # list all images in a directory
nr.images = length(all.images); nr.images                                       # number of images


## Create data frame to hold all results
all.data = data.frame(matrix(0, nr.images, 7))
names(all.data) = c("File", "CanOpen", "LAI",
                    "DirectAbove", "DiffAbove",
                    "DirectBelow", "DiffBelow")
all.data[,1] = all.images

# The batch code ----

## determine in Hemiphot.R and fill in here for batch processing
location.cx         = 237             # x coordinate of center
location.cy         = 237             # y coordinate of center
location.cr         = 228             # radius of circle
location.threshold  = 0.55


for(i in 1:nr.images){  # 1:nr.images = 1 to nr.images (i.e. [1:3, ]) it's just telling it to start at 1 (which I thought it does automatically)                              
  ## read file
  image = readJPEG(paste("images/test/",all.images[i],sep = ""), native = F)     #if native = T creates a raster, else an array
  
  ## conver to Hemi image
  image = Image2Hemiphot(image)
  
  ## set cirlce parameters
  image = SetCircle(image, cx = location.cx, cy = location.cy, cr = location.cr)
  
  ## select blue channel
  image = SelectRGB(image, "B")
  
  #threshold
  image = ThresholdImage(im = image, th = location.threshold, draw.image = F)
  
  # canopy openness
  gap.fractions = CalcGapFractions(image)
  all.data[i,2] = CalcOpenness(fractions = gap.fractions)
  
  ## calculate LAI according to Licor's LAI Analyzer 
  all.data[i,3] = CalcLAI(fractions = gap.fractions)
}

head(all.data) 

# save data
write.table(all.data, "output/test-script_output.csv", sep = ",")
