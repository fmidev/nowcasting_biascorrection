# fetch obs from smartmet server
# Station_list used is keyword=snwc
# The first guess station_list is MOS station_list, only SYNOP stations

library('httr')

# Do conversion to spatial object if server returns numbers, if not then return NULL  
obs_spatial <- function(obs) {
  tryCatch(
    {
    obs <- obs[complete.cases(obs$latitude),]
    obs <- obs[obs$latitude>-90,] # exclude south pole!
    sp::coordinates(obs) <- c('longitude','latitude')
    sp::proj4string(obs) <- sp::CRS("+init=epsg:4326")
    return(obs)
    },
    # how to handle warnings 
    warning = function(cond) {
      message(cond)
      obs = NULL
      return(obs)
    },
    # how to handle errors
    error = function(cond) {
      message(cond)
      obs = NULL
      return(obs)
    }
  )
}

# Read observations from smartmet server
readobs <- function(starttime, endtime, parname,
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
    keyword='snwc',
    timestep='1h',
    precision='full',
    #data_quality='1',
    #param=paste(parameters,parname,collapse=I(','),sep=''),
    param=paste(c(parameters,parname),collapse=I(','),sep=''),
    producer='observations_fmi'    
  )

  # modify default query from command line arguments
  query <- modifyList(defquery,query)
  outfile <- tempfile(fileext = ".csv")
  
  # if no connection to server (times out) then return NULL
  tryCatch(
    {
    a <- Sys.time()  
    httr::GET(url=url, query=query, httr::write_disk(outfile,overwrite = TRUE),timeout(20))
    print(paste('Retrieving ',query$producer,' obs for ',parname,' takes: ', round(Sys.time()-a,digits=2),'s',sep=''))
    },
    # how to handle warnings 
    warning = function(cond) {
      message(cond)
    },
    # how to handle errors
    error = function(cond) {
    message(cond)
    }
  )
  
  obs <- read.csv2(outfile,sep=sep,header=FALSE,col.names=c(parameters,'observation'),dec='.',
                   stringsAsFactors=FALSE)
  if (nrow(obs)>0) {
    obs$time <- as.POSIXct(obs$time,tz='UTC')
    # add QC for obs: based on static thresholds and on distribution (chi-sq test)
    if (parname=='NetAtmo' | parname=='TA_PT1M_AVG') { # for temperature [-40,40]
      obs <- obs[(obs$observation>=(-40) & obs$observation<=40),]
      outliers <- scores(obs$observation, type="chisq", prob=0.995) # chi-sq scores => (x - mean(x))^2/var(x)
      obs <- obs[outliers==FALSE,]
    }
    if (parname=='WS_PT10M_AVG' | parname=='WSP_PT10M_AVG') { # for wind speed [0,40]
      obs <- obs[(obs$observation>=0 & obs$observation<=40),]
      outliers <- scores(obs$observation, type="chisq", prob=0.9999) 
      obs <- obs[outliers==FALSE,]
    } 
    if (parname=='RH_PT1M_AVG') { # for relative humidity [0,100]
      obs <- obs[(obs$observation>=0 & obs$observation<=100),]
      outliers <- scores(obs$observation, type="chisq", prob=0.9999) 
      obs <- obs[outliers==FALSE,]
    } 
    # convert to SpatialPointsDataFrame
    if (spatial) {
      obs <- obs_spatial(obs)
      # obs <- obs[complete.cases(obs$latitude),]
      # obs <- obs[obs$latitude>-90,] # exclude south pole!
      # sp::coordinates(obs) <- c('longitude','latitude')
      # sp::proj4string(obs) <- sp::CRS("+init=epsg:4326")
    }
  
    file.remove(outfile)
    return(obs)
  }
}

# return Finnish and foreign stations (road weather stations)
readobs_all <- function(starttime, endtime=NULL, parname,
                        parameters = c('name','fmisid','latitude','longitude','time'),
                        spatial=FALSE) {
  if (is.null(endtime))
    endtime <- starttime
  if (parname!='NetAtmo'){ # if parameter is not NetAtmo data
    obs1 <- readobs(starttime, endtime, parname, parameters = parameters, spatial = spatial)
    obs2 <- readobs(starttime, endtime, parname,
                   query=list(producer='foreign'),
                   parameters = parameters, spatial = spatial)
    #obs3 <- readobs(starttime, endtime, fmisid, parname,  # road weather station
    #                query=list(producer='road'),
    #                parameters = parameters, spatial = spatial)
    #tmp_list <- list(obs1,obs2,obs3)
    tmp_list <- list(obs1,obs2)
    tmp_list <- tmp_list[lengths(tmp_list) != 0]
    #joku = tmp_list[-which(sapply(tmp_list, is.null))]
    #tmp_list=tmp_list[-(which(sapply(tmp_list,is.null),arr.ind=TRUE))] #remove NULL
    obs <- do.call("rbind", tmp_list)
    #obs <- obs[complete.cases(obs@data$TA_PT1M_AVG),] # Remove if obs for T is Nan!!!
  } else if (parname=='NetAtmo') {
    #obs <- fread(paste("http://smartmet.fmi.fi/timeseries?producer=NetAtmo&tz=gmt&precision=full&starttime=",alku1,"&endtime=",loppu1,"&param=data,station_id,longitude,latitude,utctime,temperature&format=ascii&data_quality=1&bbox=7,55,32,70",sep=""))
    obs <- readobs(starttime, endtime, parname='temperature',
                    query=list(producer='NetAtmo',data_quality='1'), 
                    parameters = c('name','station_id','latitude','longitude','time'), spatial = spatial)
  }
    
  return(obs)
}
