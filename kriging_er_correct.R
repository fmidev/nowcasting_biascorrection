# Error correction of 1-5h forecasts for Metcoop nowcast (meps control) model for parameters t2m, rh2m and ws10m
# model data: 2t in Kelvins, Rh 0-1
# obs: 2t in Celsius, Rh 0-100
# return input fields, if no obs available 
#
# library(devtools)
# install_github("harphub/Rgrib2")
library('Rgrib2')
library('httr')
library('sp')
library('fastgrid')
library('rgeos')
library('raster')
source('gridding.R') # subroutines
source('smartmet_obs.R') # obs fetch from smartmet server
source('file-access.R') # wrapper for reading/writing files from/to s3

args = commandArgs(trailingOnly=TRUE)
#args[1] = input-file
#args[2] = output-file
#args[3] = Netatmo TRUE/FALSE

if (length(args) == 0) {
  quit(status=1)
}

g_in <- read_s3(args[1])
g_out <- strip_protocol(args[2])
use_NetA <- args[3] # TRUE if NetAtmo data is used, FALSE if not

# data.frame to map grib input-file parameterNumber to corresponding smartmet server OBS name and error weights 
grib_paramnb <- data.frame('parname'=c('2t','rh','ws'),'parnumb'=c(0,192,1),'obs_parm'=c('TA_PT1M_AVG','RH_PT1M_AVG','WS_PT10M_AVG'),'w1'=c(0.9,0.9,0.8),'w2'=c(0.9,0.8,0.7),'w3'=c(0.8,0.7,0.6),'w4'=c(0.7,0.7,0.6),stringsAsFactors=FALSE)

# get the parameter based on grib-input parameterNumber
param <- getfcparam(g_in) # t2m=0,rh=192,ws=1
# select the row with extra info needed for correct parameter
par.row <- which(grib_paramnb$parnumb==param)

# Use this if time is set based on sys.time 
# # define lates full hour in UTC (the time error is calculated from)
# #local.time <- Sys.time()
# local.time <- as.POSIXct('2020-07-28 13:00:00 UTC') # use this for test data! 
# attr(local.time, "tzone") <- "UTC"
# current.hour <- round(as.POSIXct(local.time, format="%H:%M:%S", tz="UTC"), units="hours")
# rm(local.time)
# fc_hours <- current.hour + c(0,1,2,3,4)*60*60

# for test data
# g_in <- 'smnwc-T-K.grib2' # temperature test dataset  
# g_in <- 'smnwc-RH-0TO1.grib2' # relative humidity test dataset
# g_in <- 'smnwc-FF-MS.grib2' # wind speed test dataset

# correlation length km
clen <- 50 
clenx <- 10 # smaller correlation length used for NetAtmo data
# land sea mask used
LSM <- readRDS('MEPS_lsm_sea.Rds') # only sea (+Vänern and Wettern)
coordnames(LSM) <- c('longitude','latitude') # needed for gridding (should be fixed!)

# Forecast leadtimes 15min/1h (depends on the data) steps in Metcoop nowcast
mtimes <- getfcdates(g_in) # in grib files times <10 with 3 numbers, this function works also when 00:00 --> 0!

atime <- mtimes[1] # analysis times
fc_hours <- atime + c(1,2,3,4,5)*60*60 # 1h,2h,3h,4h,5h

# find indexes (=message numbers) in grib forecast times
msgs <- sapply(fc_hours, function(x) which(mtimes==x)) 

# start with 1h fc and do correction against observations
m <- msgs[1] 
t1 <- fc_hours[1] # 1h forecast of Metcoop NWC to used in error correction
out <- readgrib(g_in,msg=m)
coordnames(out) <- c('longitude','latitude') # needed for gridding

# define corresponding obs parameter name for smartmet server
obs_param <- grib_paramnb$obs_parm[par.row]

# obs from smartmet server
# keyword for smartmet server obs retrieval: snwc includes the stations in ErrCorrectStations.csv
obs <- readobs_all(t1,t1,obs_param,spatial = TRUE)

# if wind speed, then use potential wind speed opervations when available (for Finland)
if(param==1)  {
  # retrieve WSP obs data
  obsPT <- readobs_all(t1,t1,"WSP_PT10M_AVG",spatial = TRUE)
  # define the indexes for which there's WSP available 
  ind_fmisid <- which(obs$fmisid %in% obsPT$fmisid)
  # replace original obs values wiht WSP when available
  obs$observation <- replace(obs$observation,ind_fmisid,obsPT$observation)
}

if(!is.null(obs)) { # do error correction if there's obs available, if not return input fields 
  # prepare obs
  obsx <- obs_prepare(obs,t1,raster::extent(out), LSM)
  # obs for 2t in C --> K in model data
  # obs for rh 0-100 --> 0-1 in model data
  if (param==grib_paramnb$parnumb[1]){ # temperature from K --> C
    obsx$VAR1 <- obsx$VAR1 + 273.15
  }
  if (param==grib_paramnb$parnumb[2]){ # RH from 0-1 --> 0-100
    obsx$VAR1 <- obsx$VAR1/100
  }

  # gridding
  var.pred <- gridobs(obsx,out,clen=1000*clen, lsm=LSM$lsm)
  
  # diff = modified - original --> error correction 
  fcerr <- var.pred$diff  # error correction

  # quality control:
  # RH<0-->0 & RH>1-->1
  # ws<0-->0
  fc.mod <- qcheck(var.pred$VAR1,param,grib_paramnb)

  # plot corrected & diff field 
  #var.pred$VAR1 <- fc.mod
  #MOSplotting::MOS_plot_field(var.pred,layer = "VAR1", shapetrans = TRUE,cmin=min(out$VAR1), cmax = max(out$VAR1),
  #                              stations = obsx,main=paste(fc_hours[1], 'clen =',clen,'km'),pngfile=paste("mod_var",m,".png",sep=""))
  # 
  #MOSplotting::MOS_plot_field(var.pred,layer = "diff", shapetrans = TRUE,cmin=(-7), cmax = 7,
  #                             stations = obsx,main=paste(fc_hours[1], 'clen =',clen,'km'),pngfile=paste("SYN_",m,".png",sep=""))
  
  # if temperature then add NetAtmo QC corrected obs to bias correction
  if (use_NetA == TRUE & param==grib_paramnb$parnumb[1]){ 
    var.pred$VAR1 <- fc.mod
    alku <- t1-(10*60) # NetAtmo data is taken from 10 min time interval xx:50-t1
    obsNetA <- readobs_all(alku,t1,parname = 'NetAtmo',spatial = TRUE) # fetch NetAtm QC corrected obs
    names(obsNetA)<-c('name','fmisid','time','observation')
    if(!is.null(obsNetA)) { # do error correction if there's NetAtmo obs available, if not return SYNOP corrected fields  
      obsNetA$observation <- obsNetA$observation + 273.15
      # prepare obs
      obsNetAx <- obs_prepareNetA(obsNetA,t1,raster::extent(out),LSM,grid=var.pred)
      # coordnames(obsNetAx) <- c('longitude','latitude')
      var.predX <- gridobs(obsNetAx,var.pred,clen=1000*clenx,lsm=LSM$lsm)
      # diff = modified - original --> error correction 
      fcerr <- var.predX$diff  # error correction
      fc.mod <- qcheck(var.predX$VAR1,param,grib_paramnb)
      
  #    MOSplotting::MOS_plot_field(var.predX,layer = "diff", shapetrans = TRUE,cmin=(-5), cmax = 5,
  #                            main=paste(fc_hours[1], 'clen =',clenx,'km'),pngfile=paste("mod_NA",m,".png",sep=""))
  #    MOSplotting::MOS_plot_field(var.predX,layer = "VAR1", shapetrans = TRUE,cmin=(250), cmax = 290,
  #                                stations = obsNetAx,main=paste(fc_hours[1], 'clen =',clen,'km'),pngfile=paste("mod_VAR_NA",m,".png",sep=""))
  #    MOSplotting::MOS_plot_field(var.pred,layer = "VAR1", shapetrans = TRUE,cmin=(250), cmax = 290,
  #                                main=paste(fc_hours[1], 'clen =',clen,'km'),pngfile=paste("mod_",m,".png",sep=""))
  #    MOSplotting::MOS_plot_field(out,layer = "VAR1", shapetrans = TRUE,cmin=(250), cmax = 290,
  #                                main=paste(fc_hours[1], 'clen =',clen,'km'),pngfile=paste("mod_",m,".png",sep=""))
      
    }
  }
  savegrib(g_in,g_out,msg = m, newdata = fc.mod, append=FALSE)

  # now add the 1h correction to 2h,3h,4h and 5h
  wcorr <- as.vector(t(grib_paramnb[par.row,c('w1','w2','w3','w4')])) # choose the relevant error weights 
  for (m in 1:(length(msgs)-1)) {
    mm <- msgs[2:length(msgs)][m] # index of grib message
    out2 <- readgrib(g_in,msg=mm) # raw model data 
    tmp_fc.mod2 <- out2$VAR1 + wcorr[m] * fcerr # error correction using varying weights for different forecast times
    fc.mod2 <- qcheck(tmp_fc.mod2,param,grib_paramnb)
    savegrib(g_in,g_out,msg = mm, newdata = fc.mod2, append=TRUE)
  }
} else { # if obs == NULL save just the input fields 
  for (m in 1:(length(msgs))) {
    mm <- msgs[m] # index of grib message
    out2 <- readgrib(g_in,msg=mm) # raw model data 
    fc.mod2 <- out2$VAR1
    #fc.mod2 <- qcheck(tmp_fc.mod2,param,grib_paramnb)
    savegrib(g_in,g_out,msg = mm, newdata = fc.mod2, append=TRUE)
  }
}

write_s3(args[2])

# extra plotting
# out2$mod <- fc.mod2 # corrected forecast
# out2$diff <- out2$mod - out2$VAR1 # difference to original
# 
# MOSplotting::MOS_plot_field(out2,layer = "mod", shapetrans = TRUE,cmin=min(out2$VAR1), cmax = max(out2$VAR1),
#                            stations = obsx,main=paste(fc_hours[2], 'clen =',clen,'km'),pngfile="testi.png")
# 
# MOSplotting::MOS_plot_field(out2,layer = "diff", shapetrans = TRUE,cmin=min(out2$diff), cmax = max(out2$diff),
#                            stations = obsx,main=paste(fc_hours[2], 'clen =',clen,'km'))
