# fetch obs from smartmet server
# Used in kriging_test2.R error correction of short range MNWC forecast
# Station_list defined by fmisid numbers
# The first guess station_list is MOS station_list

library('httr')

# Read observations from smartmet server
readobs <- function(starttime, endtime, fmisid, parname,
  parameters = c('name','fmisid','latitude','longitude','time'),
  query = list(),
  spatial=FALSE) {

  url <- 'http://smartmet.fmi.fi/timeseries'
  sep <- I(';') # csv separator I()
  
  # default query parameters
  defquery = list(    
    format='ascii',
    separator=sep,
    timeformat='sql',
    tz='gmt',
    starttime=format(as.POSIXct(starttime),'%Y%m%d%H%M'),
    endtime=format(as.POSIXct(endtime),'%Y%m%d%H%M'),
    #keyword='synop_fi',
    fmisid=fmisid,
    timestep='1h',
    precision='full',
    #param=paste(parameters,parname,collapse=I(','),sep=''),
    param=paste(c(parameters,parname),collapse=I(','),sep=''),
    producer='observations_fmi'    
  )

  # modify default query from command line arguments
  query <- modifyList(defquery,query)
  
  outfile <- tempfile(fileext = ".csv")
  httr::GET(url=url, query=query, httr::write_disk(outfile,overwrite = TRUE))
  
  obs <- read.csv2(outfile,sep=sep,header=FALSE,col.names=c(parameters,'observation'),dec='.',
                   stringsAsFactors=FALSE)
  if(nrow(obs)>0){
    obs$time <- as.POSIXct(obs$time,tz='UTC')
  
    # convert to SpatialPointsDataFrame
    if (spatial) {
      obs <- obs[complete.cases(obs$latitude),]
      obs <- obs[obs$latitude>-90,] # exclude south pole!
    
      sp::coordinates(obs) <- c('longitude','latitude')
      sp::proj4string(obs) <- sp::CRS("+init=epsg:4326")
    }
  
    file.remove(outfile)
    return(obs)
  }
}

# return Finnish and foreign and road weather stations
readobs_all <- function(starttime, endtime=NULL, fmisid, parname,
                        parameters = c('name','fmisid','latitude','longitude','time'),
                        spatial=FALSE) {
  if (is.null(endtime))
    endtime <- starttime
  obs1 <- readobs(starttime, endtime, fmisid, parname, parameters = parameters, spatial = spatial)
  obs2 <- readobs(starttime, endtime, fmisid, parname,
                 query=list(producer='foreign'),
                 parameters = parameters, spatial = spatial)
  obs3 <- readobs(starttime, endtime, fmisid, parname,
                  query=list(producer='road'),
                  parameters = parameters, spatial = spatial)
  tmp_list <- list(obs1,obs2,obs3)
  tmp_list <- tmp_list[lengths(tmp_list) != 0]
  #joku = tmp_list[-which(sapply(tmp_list, is.null))]
  #tmp_list=tmp_list[-(which(sapply(tmp_list,is.null),arr.ind=TRUE))] #remove NULL
  obs <- do.call("rbind", tmp_list)
  #obs <- obs[complete.cases(obs@data$TA_PT1M_AVG),] # Remove if obs for T is Nan!!!

  return(obs)
}
