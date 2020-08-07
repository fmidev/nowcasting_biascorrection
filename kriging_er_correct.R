# Error correction of 1-5h forecasts for Metcoop nowcast (meps control) model for parameters t2m, rh2m and ws10m
# model data: 2t in Kelvins, Rh 0-1
# obs: 2t in Celsius, Rh 0-100
# 
# install_github("harphub/Rgrib2")
library('Rgrib2')
library('httr')
library('sp')
library('fastgrid')
source('gridding.R') # subroutines
source('smartmet_obs.R') # obs fetch from smartmet server

# read obs station list used in error correction from csv.file (subset of MOS station list)
stlist <- read.csv("MNWCstlist.csv")

# grib parameterNumber to map the parameter 
grib_paramnb <- data.frame('parname'=c('2t','rh','ws'),'parnumb'=c(0,192,1),'obs_parm'=c('TA_PT1M_AVG','RH_PT1M_AVG','WS_PT10M_AVG'),'w1'=c(0.9,0.9,0.8),'w2'=c(0.9,0.8,0.7),'w3'=c(0.8,0.7,0.6),'w4'=c(0.7,0.7,0.6),stringsAsFactors=FALSE)

#g_in <- 'smnwc-T-K.grib2' # temperature test dataset  
#g_in <- 'error_correction/smnwc-RH-0TO1.grib2' # relative humidity test dataset
g_in <- 'smnwc-FF-MS.grib2' # wind speed test dataset

# correlation length km
clen <- 50 
# land sea mask used
LSM <- readRDS('MEPS_lsm_sea.Rds') # only sea (+Vänern and Wettern)
coordnames(LSM) <- c('longitude','latitude') # needed for gridding (should be fixed!)

# Forecast leadtimes 15min/1h (depends on the data) steps in Metcoop nowcast
mtimes <- getfcdates(g_in) # in grib files times <10 with 3 numbers, check how works at 00:00 --> 0!

# get the grib parameterNumber
param <- getfcparam(g_in) # t2m=0,rh=192,ws=1
# select the row with extra info needed for correct parameter
par.row <- which(grib_paramnb$parnumb==param)

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

# obs from smartmet server -> replace by keyword etc ASAP
# fmisid numbers as character, have to use loop for fetching the data since ~400fmisid/url is the max
fmisid <- paste(stlist$fmisid[1:400], sep="", collapse=",")
obs <- readobs_all(t1,t1,fmisid,obs_param,spatial = TRUE)
for(i in 1:3){
  fmisid <- paste(stlist$fmisid[(i*400+1):((i+1)*400)], sep="", collapse=",")
  tmp_obs <- readobs_all(t1,t1,fmisid,obs_param,spatial = TRUE)
  obs <- rbind(obs,tmp_obs)
}
fmisid <- paste(stlist$fmisid[(4*400+1):nrow(stlist)], sep="", collapse=",")
tmp_obs <- readobs_all(t1,t1,fmisid,obs_param,spatial = TRUE)
obs <- rbind(obs,tmp_obs)
rm(tmp_obs)
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

# plot corrected field
# var.pred$VAR1 <- fc.mod
# MOSplotting::MOS_plot_field(var.pred,layer = "VAR1", shapetrans = TRUE,cmin=min(out$VAR1), cmax = max(out$VAR1),
#                             stations = obsx,main=paste(fc_hours[1], 'clen =',clen,'km'),pngfile=paste("mod_",m,".png",sep=""))

# create grib with first modified field
# g_out needs to have name that separates corrected grib files for different parameters automatically
# g_out should have some grib 
g_out <- paste(grib_paramnb$parname[par.row],'_fix.grib2',sep="")
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

# extra plotting
# out2$mod <- fc.mod2 # corrected forecast
# out2$diff <- out2$mod - out2$VAR1 # difference to original

# MOSplotting::MOS_plot_field(out2,layer = "mod", shapetrans = TRUE,cmin=min(out2$VAR1), cmax = max(out2$VAR1),
#                            stations = obsx,main=paste(fc_hours[2], 'clen =',clen,'km'),pngfile="testi.png")
# erotus
# MOSplotting::MOS_plot_field(out2,layer = "diff", shapetrans = TRUE,cmin=-10, cmax = +10,
#                            stations = obsx,main=paste(fc_hours[2], 'clen =',clen,'km'))

