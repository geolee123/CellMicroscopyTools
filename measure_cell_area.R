getarea <- function(filename,MPP,dmin=75, sdradiusmax=50,inverse=F){
  # -------------------------------------------------------------------------------
  # function measure_cell_area
  #
  # Description: 
  #   Designed to take in microscope image tif and measure the cell area of interest in field of view
  # 
  # Inputs:
  #   filename: image tif to be read
  #   MPP: 1 (default) microns per pixel conversation. 
  #        if undefined, by default will interpret image and parameters in terms of pixels instead of microns (um)
  #   dmin: 100 (default), identify object of interest by min diameter (pixels) unless MPP is defined, in which case (um).  
  #   sdradiusmax: 25 (default), identify object of interest by max standard deviation (pixels) unless MPP is defined, in which case (um).  
  #   inverse: false (default)
  # Outputs:
  #   area: area in pixels unless MPP is defined, in which case microns squared (um2)
  #
  # Author: George Lee
  # Date: February 24, 2018
  # Contact: georgeleephd@gmail.com
  # -------------------------------------------------------------------------------
  
  require(EBImage)
  options(EBImage.display = "raster")
  
  
  if(missing(MPP)) {
    MPP=1
    unit= 'pxl'
  }else{
    unit= 'um'
  }
  
  
  print(filename)
  origimg = readImage(filename)
  

  # can crop image if you want to
  #newstri<- dim(img)[1]*0
  #newstrj<- dim(img)[2]*0
  #newendi<- dim(img)[1]*.7
  #newendj<- dim(img)[2]*.8
  newstri<- 1
  newstrj<- 1
  newendi<- dim(origimg)[1]
  newendj<- dim(origimg)[2]
  origimg = origimg[newstri:newendi, newstrj:newendj,]
  
  # making the image to around 500 pxls on the horizontal axis to speed things up
  downsample = round(dim(origimg)[1]/500)
  resizedimg = resize(origimg, w=(newendi-newstri+1)/downsample, h=(newendj-newstrj+1)/downsample)
  MPP <- downsample*MPP
  
  rgbimg <- rgbImage(resizedimg) # this one is just for plotting purposes
  display(resizedimg)
  
  # initializing a few variables
  area = NA
  edge_strength = -6
  
  while (is.na(area)){
    
    print(filename)
    print(paste0('Edge_strength:',edge_strength))
    img<-resizedimg
    
    colorMode(img) = Grayscale
    
    if (inverse == T){
      img = max(img) - img
    }
    
    # high pass filter - edge detection
    fhi = matrix(1, nrow = 3, ncol = 3)
    fhi[2, 2] = edge_strength #-6.7
    img = filter2(img, fhi)
    display(img)
    
    # clean up artifacts
    img= opening(img, makeBrush(1, shape='disc'))
    #logo_closing = closing(img, makeBrush(30, shape='disc'))
    logo_closing = closing(img, makeBrush(  0.01*dim(resizedimg)[1], shape='disc'))
    
    
    # otsu's thresholding
    threshold = otsu(logo_closing)
    nuc_th = combine( mapply(function(frame, th) frame > th, getFrames(logo_closing), threshold, SIMPLIFY=FALSE) )
    display(nuc_th)
    
    # draw negative border to allow holes to fill properly  
    nuc_th[1:length(resizedimg[,1,1]), 1,] = FALSE #top
    nuc_th[1,1:length(resizedimg[1,,1]),] = FALSE #left
    nuc_th[length(resizedimg[,1,1]),1:length(resizedimg[1,,1]),] = FALSE #right
    nuc_th[1:length(resizedimg[,1,1]),length(resizedimg[1,,1]),] = FALSE #bottom
    display(nuc_th)
    
    # fill holes
    nmask = fillHull(nuc_th)
    nmask = bwlabel(nmask)
    
    display(nmask)
    
    # overlay segmentation
    segmented = paintObjects(nmask, rgbimg, col = c('green',NA))
    
    # filter based on roundness and size measures
    features<-computeFeatures.shape(nmask[,,1])
    
    sdradius<-features[, colnames(features)[4]]
    farea<-features[, colnames(features)[1]]
    print(farea)
    print(sdradius)
    
    # can set different thresholds for acceptable segmentations
    sdpxlradiusmax<-sdradiusmax/MPP
    minpxlarea <- ((dmin/2)^2)*pi/MPP
    temparea<-features[farea > minpxlarea & 
                         sdradius < sdpxlradiusmax, colnames(features)[1]]
    
    
    print(temparea)
    
    # goes through a couple different edge strength values and stops when a reasonable segmentation is done  
    if (length(temparea) == 0 & edge_strength > -7.5){
      pxlarea = NA
      edge_strength = edge_strength - 0.3
    }  else{
      if (length(temparea) == 0){
        pxlarea = NA
      }else{
        pxlarea = temparea
      }
      break
    }
    
  }
  
  # convert pixel area to um
  pxldiameter=sqrt(pxlarea/pi)*2
  diameter = pxldiameter*MPP
  area = pi*(diameter/2)^2
  
  # save image with segmentation overlay along with resulting area
  display(segmented,method="raster")
  text(x = 20, y = 20, label = paste0('Area: ', format(area, digits=3),' ', unit,'2', 
                                      '\nDiameter: ', format(diameter, digits=3), ' ',unit),
       adj = c(0,1), col = "orange", cex = 2)
  
  pdir <-file.path(dirname(filename), 'processed')
  dir.create(pdir)
  filename = file.path(pdir, paste0('processed_', basename(filename)))
  dev.print(tiff, filename = filename , width = dim(segmented)[1], height = dim(segmented)[2])
  
  if (is.na(area)){
    warning(paste('No objects found under current object selection critera diameter dmin > ', dmin,'and sdradiusmax < ', sdradiusmax,'. Suggest new parameters.'))
  }
  
  return(area)
}