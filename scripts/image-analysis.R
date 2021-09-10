# Script to analyse data from field images
# Based on HemiphotTest.R Hans ter Steege (2018)
# attapun-an

# Working directory is set to root directory (one above the script folder)
rm(list=ls())  # clear memory


# load libraries and source files ----
library(jpeg)
source("scripts/Hemiphot/Hemiphot.R")
source("scripts/fov_func.R")


# Process sample image ---- 

# read a sample image file
im <- readJPEG("images/field/EE5_2907.JPG", native = F) 

# convert to class HemiphotImage and plotting it
im.hemi = Image2Hemiphot(im) 
PlotHemiImage(image = im.hemi, draw.circle = T, channel = "")   #note that east and west are reversed as the image looks upward
# note: this takes a while to run

# Calculating image circle to 60 degrees
radius <- fov.px(30, 8, 5.95)  # this function was found on the HemiPhoto Guide, 
# our FOV is 60 degrees, we divide by 2 to get degrees theta
# 8 mm is the focal length of our lens
# 5.95 is the pixel pitch of the Nikon D750

# adjusting image circle to 60 degrees FOV
im.hemi = SetCircle(im.hemi, cr = radius)
PlotHemiImage(im.hemi)   # preview the plot


# check channels and output images to output/image_test
# output to image because previewing in R takes too long
jpeg("output/image_test/Channel_Red.jpg", width = 1500, height = 1080, units = "px", pointsize = 32)
Channel_R <- PlotHemiImage(im.hemi, draw.circle = T, channel = "R")
dev.off()

jpeg("output/image_test/Channel_Green.jpg", width = 1500, height = 1080, units = "px", pointsize = 32)
Channel_G <- PlotHemiImage(im.hemi, draw.circle = T, channel = "G")
dev.off()

jpeg("output/image_test/Channel_Blue.jpg", width = 1500, height = 1080, units = "px", pointsize = 32)
Channel_B <- PlotHemiImage(im.hemi, draw.circle = T, channel = "B")
dev.off()
# blue has the best contrast

# Store blue channel
im.blue = SelectRGB(im.hemi, "B")
PlotHemiImage(im.blue, draw.circle = T)

# Threshold the image at 3 levels and see which one works best----
threshold_1 = 0.40 
threshold_2 = 0.50
threshold_3 = 0.60

jpeg("output/image_test/t1.jpg", width = 1500, height = 1080, units = "px", pointsize = 32)
image.th_1= ThresholdImage(image = im.blue, th = threshold_1, draw.image = T)
text(x = 100, y = 500,labels = paste(threshold_1), pos = 4, col = "#FFFFFF" )
dev.off()

jpeg("output/image_test/t2.jpg", width = 1500, height = 1080, units = "px", pointsize = 32)
image.th_2= ThresholdImage(image = im.blue, th = threshold_2, draw.image = T)
text(x = 100, y = 500,labels = paste(threshold_2), pos = 4, col = "#FFFFFF" )
dev.off()

jpeg("output/image_test/t3.jpg", width = 1500, height = 1080, units = "px", pointsize = 32)
image.th_3= ThresholdImage(image = im.blue, th = threshold_3, draw.image = T)
text(x = 100, y = 500,labels = paste(threshold_3), pos = 4, col = "#FFFFFF" )
dev.off()

# BATCH CODE - variables ----
# run this before running any of the batch covdes

all.images = list.files("images/field/",pattern = ".JPG")                        # list all images in a directory
nr.images = length(all.images); nr.images                                       # number of images


## Create data frame to hold all results
all.data = data.frame(matrix(0, nr.images, 7))
names(all.data) = c("File", "CanOpen", "LAI",
                    "DirectAbove", "DiffAbove",
                    "DirectBelow", "DiffBelow")
all.data[,1] = all.images


## determine in Hemiphot.R and fill in here for batch processing
location.cr         = 696             # radius of circle as calculated befrore
location.threshold  = 0.70

# BATCH CODE - calculate LAI ----

for(i in 1:nr.images){  # 1:nr.images = 1 to nr.images (i.e. [1:3, ]) it's just telling it to start at 1 (which I thought it does automatically)                              
  ## read file
  image = readJPEG(paste("images/field/",all.images[i],sep = ""), native = F)     #if native = T creates a raster, else an array
  
  ## conver to Hemi image
  image = Image2Hemiphot(image)
  
  ## set cirlce parameters
  image = SetCircle(image, cr = location.cr)
  
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
write.csv(all.data, "output/image-analysis-output.csv")


# BATCH GENERATE IMAGES ----

for(i in 1:nr.images){  
  ## read file
  image_origin = readJPEG(paste("images/field/",all.images[i],sep = ""), native = F)     #if native = T creates a raster, else an array
  
  ## conver to Hemi image
  image = Image2Hemiphot(image_origin)
  
  ## set cirlce parameters
  image = SetCircle(image, cr = location.cr)
  
  ## select blue channel
  image = SelectRGB(image, "B")
  
  #threshold
  image = ThresholdImage(im = image, th = location.threshold, draw.image = F)
  
  # canopy openness
  gap.fractions = CalcGapFractions(image)
  canopy_openness = CalcOpenness(fractions = gap.fractions)
  
  ## calculate LAI according to Licor's LAI Analyzer 
  lai = CalcLAI(fractions = gap.fractions)
  
  # generate images
  jpeg(paste("output/thresholds/", all.images[i], "thresh.jpg"), width = 1500, height = 1080, units = "px", pointsize = 32)
  ThresholdImage(im = image, th = location.threshold, draw.image = T)
  text(x = 100, y = 500,labels = paste("CO:", round(canopy_openness,4),
                                       "\n","LAI:", round(lai, 4), sep = ""), pos = 4, col = "#FFFFFF" )
  dev.off()
  
  jpeg(paste("output/thresholds/", all.images[i], "blue.jpg"), width = 1500, height = 1080, units = "px", pointsize = 32)
  blue = Image2Hemiphot(image_origin)
  blue = SetCircle(blue, cr = location.cr)
  PlotHemiImage(image = blue, draw.circle = T, channel = "B")
  dev.off()
}

