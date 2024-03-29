# Bias correction for short range deterministic weather forecasts
This bias correction (BC) can be used to correct the forecast errors in deterministic weather model forecasts up to 1-5 hour leadtimes. BC uses model forecasts in grids and point observations. The input data is the model grids (grib2-files) and the output is bias corrected model fields (grib2) for forecast hours 1,2,3,4,5h (when 0h is the model analysis time). BC retrieves observation information from Smartmet server for observation points defined in file ErrCorrectStations.csv, where fmisids are used as indentifiers for the stations. The current list includes obs stations for producers: "foreign" and "synop_fi" and it's a subset of MOS station list for Scandinavian area. BC calculates the model error in obs points for forecast time 1h (for hourly updating Metcoop nowcast model, that is available in 1h delay, this is the latest model forecast hour with observations available). Calculated errors for points are gridded and the 1h model forecast grid is corrected using gridded error correction information. 1h gridded error field is used to correct the forecast times 2-5h using error weights loosely based on 1 year of model forecast error information. BC currently works for parameters 2m temperature (K), 2m relative humidity (0-1) and wind speed (m/s).  

## Usage 
Rscript kriging_er_correct.R input.grib2 output.grib2 yes/no(=option to use NetAtmo obs for T2m)
* Error correction is calculated for time: input.grib2 analysis time+1h
* Gridded error correction field is used to correct forecast hours 2,3,4,5 
* output.grib2 has forecasts times 1,2,3,4,5h, does not include the analysis time 0h!
* Run time of ~20min past recommended to include sufficient amount of observations  

## Running in a container

Build container image:

```
docker build -t nowcasting_biascorrection .
```

Give input and output files as variables to container.

```
docker run --rm \
    -e INPUT=s3://.../in.grib2 \
    -e OUTPUT=s3://.../out.grib2 \
    -t nowcasting_biascorrection:latest
```

Local files can also be used instead of S3-based files. In this case it's useful to mount a host directory
where the input and output are stored.

```
docker run --rm \
    -v /tmp/nowcasting:/tmp/nowcasting \
    -e INPUT=/tmp/nowcasting/in.grib2 \
    -e OUTPUT=/tmp/nowcasting/out.grib2 \
    -t nowcasting_biascorrection:latest
```


## Authors
marko.laine@fmi.fi leila.hieta@fmi.fi mikko.partio@fmi.fi

## Known issues, features and development ideas
* Keyword "snwc" used to fetch obs data from smartmet server (station list ErrCorrectStations.csv)   
* SYNOP observations used for error correction, for temperature also NetAtmo is possible   
* Output data is used as input data for nowcasting_fcst
* Output data grib meta-data generatingProcessIndentifier=207 ("MNWC_Himan")
* Kriging uses both land sea mask (MEPS_lsm.Rds) & altitude (MEPS_alt.Rds) information from model data in gridding
* NetAtmo station altitudes are interpolated from digital elevation map (not included to this repository) 
* For NetAtmo obs the mean of all the values within a grid is used in bias correction 
