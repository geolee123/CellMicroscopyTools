# clear workspace
rm(list=ls())

# load libraries
library(EBImage)
library(xlsx)
source("measure_cell_area.r")

# ---------------------- main code ------------------------

# single run
MPP<-500/156 # microns per pixel conversation # will output pixels as default
dmin<-200
sdradiusmax<-50 # max standard deviation (um)
area<-getarea(filename = 'example1.tif', dmin=dmin, sdradiusmax= sdradiusmax, MPP=MPP)

area<-getarea(filename = 'example1.tif')

area<-getarea(filename = 'example2.tif', inverse=T)


# batch run
# get list of images for processing
main_dir <- getwd() # select directory where all your images reside
images<-list.files(main_dir,pattern='.tif$',recursive=TRUE)
imagepath<-file.path(main_dir, images)

areas<-lapply(imagepath,getarea, dmin=dmin, sdradiusmax=sdradiusmax,MPP=MPP)

# wrap results up in an excel doc
plated_cell_areas<-data.frame(imagename = imagepath, area = unlist(areas))
write.xlsx2(plated_cell_areas,file.path(main_dir,'plated_cell_areas.xls'), sheet = 'Day5', append = T)