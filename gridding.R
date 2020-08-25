# gridding functions

library('Rgrib2')
library('sp')
library('fastgrid')

# GRIB Lambert projection
crs.lambert <- CRS('+ellps=sphere +a=6367470 +b=6367470 +proj=lcc +lon_0=15.0 +lat_0=0.0 +x_0=0.0 +y_0=0 +lat_1=63.3 +lat_2=63.3 +no_defs')
crs.lonlat <- CRS("+init=epsg:4326")

# Transform one x y point from coordinate system p1 to p2
transpoint<- function(x,y,p1,p2) {
  xy <- SpatialPoints(matrix(c(x,y),nrow=1),proj4string = p1)
  coordinates(spTransform(xy,p2))
}

# Read valid times from a grib file and return as POSIXct
getfcdates <- function(f1,tz='UTC') {
  g <- Gopen(f1)
  # take 3 number time values into account
  a <- format(strptime(sprintf('%04d', g$validityTime), format='%H%M'), '%H%M') 
  validtime <- as.POSIXct(paste(g$validityDate,a),tz=tz,format="%Y%m%d %H%M")
  return(validtime)
}

# return the grib parameter name
getfcparam <- function(f1) {
  g <- Gopen(f1)
  param <- g$parameterNumber[1]
  return(param)
}

# read one field from MEPS grib file in Lambert projection
# returns output as SpatialGridDataFrame
readgrib <- function(f1,msg=1,gcrs=crs.lambert,ocrs=crs.lonlat) {
  
  g <- Gopen(f1)
  gh <- Rgrib2::Ghandle(g,msg)
  gdat  <- Rgrib2::Gdec(gh)
  gdom <- Rgrib2::Gdomain(gh)
  
  IntPar <- c("Nx", "Ny", "iScansNegatively", "jScansPositively",
              "jPointsAreConsecutive",
              "alternativeRowScanning","missingValue", "numberOfMissing")
  StrPar <- c("units")
  
  ginf <- Rgrib2::Ginfo(gh, IntPar = IntPar, StrPar = StrPar)
  
  dx = gdom$dx
  dy = gdom$dy
  nx = gdom$nx
  ny = gdom$ny
  lon0 <- gdom$SW[1]
  lat0 <- gdom$SW[2]
  if (lon0 > 180) lon0 <-lon0-360
  
  xy0 <- transpoint(lon0,lat0,ocrs,gcrs)
  x0<-xy0[1]
  y0<-xy0[2]
  
  #x1 <- x0+(nx-1)*dx
  #y1 <- y0+(ny-1)*dy
  #ll2 = transpoint(x1,y1,p1,p0)
  #x <- seq(x0,x1,len=nx)
  #y <- seq(y0,y1,len=ny)
  
  if (ginf$jScansPositively==1){ # check this!!!
    gdat <- gdat[,dim(gdat)[2]:1]
  }
  
  gt <- sp::GridTopology(cellcentre.offset = c(x0,y0),
                         cellsize = c(dx,dy),
                         cells.dim = c(nx,ny))
  
  out<-sp::SpatialGridDataFrame(gt, data.frame(VAR1=as.vector(gdat)),proj4string = gcrs)
  sp::coordnames(out) <- c('x','y')
  sp::gridded(out)<-TRUE
  sp::fullgrid(out) <- TRUE
  
  GhandleFree(gh)
  
  return(out)
}

# copy msg from gin to gout and replace data with newdata
#Gmod(gh,data=newdata,IntPar=list(centre=89,productDefinitionTemplateNumber=0))
savegrib <- function(gin,gout,msg=1,newdata=NULL,append=FALSE) {
  g <- Gopen(gin) # open the original grib file
  gh <- Ghandle(g,msg)
  if (!is.null(newdata)) {
    dim(newdata) <- c(g$Nx[msg],g$Ny[msg]) # reshape
    newdata <- newdata[,dim(newdata)[2]:1] # swap column order
    Gmod(gh,data=newdata,IntPar=list(centre=86,productDefinitionTemplateNumber=70,generatingProcessIdentifier=207)) # replace grib data with new
    # Modify grib meta-data generatingProcessIndentifier=207 is "MNWC Himan" producer
  }
  Gwrite(gh,filename = gout, append = append) # save to file
  GhandleFree(gh)
  invisible(NULL)
}

# gridding point observations to background grid using 'fastgrid' package
gridobs <- function(obs,grid, variable="VAR1", sigma = 5.0, nugget=0.2, clen=1000*10, lsm=NULL) {
  
  covpars <- c(sigma,clen,nugget)
  trend_model <- as.formula(paste(variable,'~ -1'))
  coordnames(grid) <- c('longitude','latitude')
  var.pred <- fastkriege(trend_model = trend_model, obs, grid, cov.pars = covpars, 
                         lsm=lsm,lsmy=obs$lsm, alt=NULL, alty=NULL,
                         bg=grid, variable=variable, LapseRate = 0.0,
                         method='nearest')
  
  return(var.pred)
}

# quality check for corrected forecast 
qcheck <- function(fcvalue,param,grib_paramnb) {
  if(param==grib_paramnb$parnumb[2]) { # rh
    fcvalue[fcvalue>1]<-1
    fcvalue[fcvalue<0]<-0
  }
  if(param==grib_paramnb$parnumb[3]) { # ws
    fcvalue[fcvalue<0]<-0
  }
  return(fcvalue)
}

# obs processing
obs_prepare <- function(obs, fc_time, e=NULL, lsm=NULL) {
  # convert to MEPS coordinate system
  obsx <- spTransform(obs,crs.lambert)
  obsx <- obsx[obsx$time==fc_time,] # select ftime only
  obsx$VAR1 <- obsx$observation # name has to match that of the FC field
  obsx <- obsx[complete.cases(obsx$VAR1),] # remove NaN values
  # crop needs the raster package
  # e <- raster::extent(out)
  if (!is.null(e))
    obsx <- raster::crop(obsx,e)
  # add lsm to observation locations
  if (!is.null(lsm))
    obsx$lsm <- grid2points(lsm,obsx,method='nearest',variable='lsm')
  return(obsx)
}
