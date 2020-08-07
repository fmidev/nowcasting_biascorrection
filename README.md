# Bias correction for short range deterministic weather forecasts
This bias correction (BC) can be used to correct the forecast errors in deterministic weather model forecasts up to 1-5 hour leadtimes. BC uses model forecasts in grids and point observations. The input data is the model grids (grib2-files) and the output is bias corrected model fields (grib2) for forecast hours 1,2,3,4,5h (when 0h is the model analysis time). BC retrieves observation information from Smartmet server for observation points defined in file MNWCstlist.csv, where fmisids are used as indentifiers for the stations. The current list includes obs stations for producers: "road","foreign" and "synop_fi" and it's a subset of MOS station list for Scandinavian area. BC calculates the model error in obs points for forecast time 1h (for hourly updating Metcoop nowcast model, that is available in 1h delay, this is the latest model forecast hour with observations available). Calculated errors for points are gridded and the 1h model forecast grid is corrected using gridded error correction information. 1h gridded error field is used to correct the forecast times 2-5h using error weights loosely based on 1 year of model forecast error information. BC currently works for parameters 2m temperature (K), 2m relative humidity (0-1) and wind speed (m/s).  

---

## Authors
marko.laine@fmi.fi leila.hieta@fmi.fi

---

## Known issues, features and development ideas
* Stationlist should be replaced by "keyword". Stationlist should be relatively easily modified
* Road weather stations do not have good enough QC, high risk of erroneous results
* BC should take the current time into account and not relay on just model information for times
* Currently only works with test data (real time data not available)
* Have to check that 00:00 times work
* Output grid metadata have to include the info that they are corrected fields
* Output data is used as input data for nowcasting_fcst
