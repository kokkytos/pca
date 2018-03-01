setwd(dirname(rstudioapi::getSourceEditorContext()$path))#for Rscript inside RStudio


## Libraries ----------------------------------------------------------------------
library(raster)
library(RStoolbox)
library(MODIS)
MODISoptions(MODISserverOrder = c("LPDAAC","LAADS")) #run lpdaacLogin(server = "LPDAAC") first, saves credentials in ~/.netrc, https://www.rdocumentation.org/packages/MODIS/versions/1.1.0/topics/lpdaacLogin

#set CRS definitions
wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
moll<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


satDN<-63

## city settings -----------------------------------------------------------

#citydata<-list(name='Athens', ext=extent(1991932,2087932,4523978,4602978))
citydata<-list(name='Milan', ext=extent(672756,798897,5338012,5450956))#extents (in World Mollweide coords)

ext<-citydata$ext
city<-citydata$name


## Dates -------------------------------------------------------------------
startdate<-"2013-01-01"
enddate<-"2013-12-31"



## DMSP/OLS -----------------------------------------------------------------
OLS_FILE<-('/home/leonidas/Documents/phd/EARSeL/OLS/FIL2013.tif')
ols <- crop(raster(OLS_FILE), ext, snap='near',datatype ="INT1U")


## DMSP/OLS (radiance calibrated)-----------------------------------------------------------------
OLS_FILE_CAL<-'/home/leonidas/Documents/phd/sanui/OLS_CAL/F16_20100111-20101209_rad_v4.avg_vis.tif'
rin <- raster(OLS_FILE_CAL)
projection(rin) <- CRS(wgs84)
ols_cal<-projectRaster(
  from      = rin,
  to        = ols,
  crs       = moll, 
  method    = 'ngb', 
  alignOnly = FALSE, 
  over      = FALSE,
  dataType  = 'INT1U',
  overwrite = TRUE
)



## VIIRS ------------------------------------------------------------------------
VIIRS_FILE<-'/home/leonidas/Documents/phd/sanui/VIIRS/SVDNB_npp_20140101-20140131_75N060W_vcmcfg_v10_c201506171538.avg_rade9h.tif'
rin <- raster(VIIRS_FILE)
projection(rin) <- CRS(wgs84)

viirs<-projectRaster(
  from      = rin,
  to        = ols,
  crs       = moll, 
  method    = 'ngb', 
  alignOnly = FALSE, 
  over      = FALSE,
  dataType  = 'INT1U',
  overwrite = TRUE
)


## Seawinds------------------------------------------------------------------------
SeaWinds<-raster('/home/leonidas/Documents/phd/EARSeL/SEAWINDS/Global_quev_2009_JFM_PR.tif')#todo: find file for 2013

SeaWinds <- projectRaster(
  from = SeaWinds,
  to = ols,
  crs       = moll,
  method    = 'ngb',
  alignOnly = FALSE,
  over      = FALSE
)

SeaWinds <- SeaWinds/maxValue(SeaWinds)#normalize Seawinds



## NDVI -----------------------------------------------------------------------------------------
product<-"MOD13A2" #Vegetation Indices 16-Day L3 Global 1km: https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13a2
SDSstring="1"
outDirPath = "NDVI"
scale.factor =0.0001

cll <- getCollection(product = product, forceCheck = TRUE)

ndvi.modis<-MODIS::runGdal(
  job = city,
  product = product,
  extent =  ols,
  SDSstring=SDSstring,
  collection = cll,
  begin = startdate,
  end = enddate,
  outDirPath = outDirPath,
  overwrite= TRUE,
  checkIntegrity = TRUE,
  #quiet=TRUE,
  wait = 20
)

ndvi.rasters<- lapply(ndvi.modis[[1]], sapply, function(x) raster(x)*scale.factor) #generate rasters (scaled by factor)


years<-split(x=ndvi.rasters, f=format(as.Date(as.character(names(ndvi.rasters)), format = "%Y-%m-%d"),"%Y"))#split by year

months<-lapply(years, function (x) split(x, f=format(as.Date(as.character(names(x)), format = "%Y-%m-%d"),"%m")))
ndvi.months<-lapply(months, sapply,function(x) mean(stack(unlist(x)),na.rm=T))

ndvi.years.mean<-lapply(years, function(x) mean(stack(unlist(x)),na.rm=T))#calculate annual mean ndvi
ndvi.years.max<-lapply(years, function(x) max(stack(unlist(x),na.rm=T)))#calculate annual max ndvi.

#make a list with appropriate structure/data
#ndvi<-list(annual.mean=ndvi.years.mean,annual.max=ndvi.years.max, monthly.mean=ndvi.months, days16=ndvi.rasters)



## LST -----------------------------------------------------------------------------------------
#settings for LST
product <- "MOD11A2" #Land Surface Temperature and Emissivity 8-Day L3 Global 1km, https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod11a2
SDSstring <- "100010000000"
outDirPath <- "LST"
scale.factor =0.02 

cll <- getCollection(product = product, forceCheck = TRUE)

lst.modis<-MODIS::runGdal(
  job = city,
  product = product,
  extent =  ols,
  SDSstring = SDSstring,
  collection = cll,
  begin = startdate,
  end = enddate,
  outDirPath = outDirPath,
  overwrite= TRUE,
  checkIntegrity = TRUE,
  wait = 20
)

lst.modis$MOD11A2.006<-lst.modis$MOD11A2.006[names(ndvi.modis$MOD13A2.006)] #keep only 16-days, subset with ndvi.modis dates.

lst.rasters<- lapply(lst.modis[[1]], sapply, function(x) raster(x)*scale.factor) #generate rasters (scaled by factor)
lst.days<- lapply(lst.rasters,function(x) mean(stack(x),na.rm=T))#mean by day

years<-split(x=lst.days, f=format(as.Date(as.character(names(lst.days)), format = "%Y-%m-%d"),"%Y"))#split by year
months<-lapply(years, function (x) split(x, f=format(as.Date(as.character(names(x)), format = "%Y-%m-%d"),"%m")))

lst.months<-lapply(months, sapply,function(x) mean(stack(unlist(x)),na.rm=T))
lst.years.mean<-lapply(years, function(x) mean(stack(x),na.rm=T))#calculate annual mean temp.


#make a list with appropriate structure/data
#lst<-list(annual=lst.years.mean, monthly=lst.months, days=lst.days)


## TVANUI -------------------------------------------------------------
# data normalization 
ols[ols>satDN]<-satDN #set max DN=63
ols.norm<-ols/satDN #normalize ols

lst.norm<-(lst.years.mean$`2013`-minValue(lst.years.mean$`2013`))/(maxValue(lst.years.mean$`2013`)-minValue(lst.years.mean$`2013`)) #norm lst

ndvipositive<-ndvi.years.max$`2013` #NDVI max for 2013
ndvipositive[ndvipositive<0]<-NA #valid range for NDVI [0-1]



#' Calculates TVANUI
#' [Zhang2018] https://www.sciencedirect.com/science/article/pii/S0924271617303611
#' @param ndvi normalized ndvi raster without negative values 
#' @param ols  normalized ols raster 
#' @param lst  normalized lst raster 
#'
#' @return tvanui raster
#' @export
#'
#' @examples
ftvanui<-function(ndvi,ols, lst){
  if (length(ndvi[ndvi<0]>0)){
    stop("NDVI contains negative values..." )
  }
  
  if (maxValue(ols)>1||maxValue(ndvi)>1||maxValue(lst)>1){
    stop("Input rasters are not normalized..." )
  }
  tvanui<-((atan(lst/ndvi))/(pi/2))*ols #return value
}


tvanui<-ftvanui(ndvipositive,ols.norm,lst.norm)


## SANUI ------------------------------------------------------------------------
ndviPositive<-ndvi.years.mean$`2013` #mean NDVI
ndviPositive[ndviPositive<0]<-NA#keep only positive values of NDVI, set negative values to NA
sanui <- (1-ndviPositive)*SeaWinds*ols.norm

## VANUI ------------------------------------------------------------------------

#' Calculates VANUI
#'
#' @param ndvi normalized ndvi raster without negative values 
#' @param ols  normalized ols raster
#'
#' @return VANUI raster
#' @export
#'
#' @examples
fvanui<-function(ndvi,ols){
  if (length(ndvi[ndvi<0]>0)){
    stop("ndvi contains negative values" )
  }
  
  if (minValue(ols)<0 || maxValue(ols) > 1){
    stop("NTL raster is not [0-1] normalized" )
  }
  
  if (minValue(ndvi)<0 || maxValue(ndvi) > 1){
    stop("NDVI raster is not [0-1] normalized" )
  }
  
  
  vanui<-(1-ndvi)*(ols)
}

vanui<-fvanui(ndviPositive, ols.norm)

## Pearson correlation coefficient  --------------------------------------------------------------------------------------

layers<-stack(ols, ols_cal, viirs, sanui, vanui, tvanui)
names(layers)<-c('ols', 'ols_cal', 'viirs', 'sanui', 'vanui', 'tvanui')

cor<-layerStats(layers,stat ='pearson', na.rm=T)

write.csv(as.data.frame(cor$`pearson correlation coefficient`), "pearson.csv",row.names=TRUE)


## PCA -----------------------------------------------------------------------------------------------
img=brick(lst.years.mean$`2013`/maxValue(lst.years.mean$`2013`),ndvi.years.max$`2013`,ols.norm)

pca<-rasterPCA(img)
summary(pca$model)

layers<-stack(viirs, ols_cal, pca$map$PC1)
names(layers)<-c('viirs', 'ols_cal', 'PC1')

cor<-layerStats(layers,stat ='pearson', na.rm=T)
write.csv(as.data.frame(cor$`pearson correlation coefficient`), "pearson.PC1.csv",row.names=TRUE)



##  L*(1-variance(V))*(1-variance(T)) ----------------------------------------------------------------

var.method<-ols*(1-calc(stack(unlist(ndvi.rasters)), fun=var))*(1-calc(stack(sapply(unlist(lst.days), function(x) x/maxValue(x))), fun=var))

layers<-stack(viirs, ols_cal, var.method)
names(layers)<-c('viirs', 'ols_cal', 'var.method')

cor<-layerStats(layers,stat ='pearson', na.rm=T)
write.csv(as.data.frame(cor$`pearson correlation coefficient`), "pearson.VAR.csv",row.names=TRUE)
