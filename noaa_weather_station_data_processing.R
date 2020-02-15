# NOAA Weather Data Station file processing
# Miklós Bán, banm@vocs.unideb.hu, 2019.02.08
# Example running
# source('noaa_weather_station_data_processing.R')
# ws_data.df <- processWSFile('128300-99999-2019.gz')
#
# temperatue formatting and rescaling
# as.numeric(paste(substr((ws_data.df$air_temp),1,1),as.numeric(substr((ws_data.df$air_temp),2,5)),sep='')) / 10

# Simple Mapping WS
# lon <- as.numeric(paste(substr((ws_data.df$lon),1,1),as.numeric(substr((ws_data.df$lon),2,7)),sep='')) / 1000
# lat <- as.numeric(paste(substr((ws_data.df$lat),1,1),as.numeric(substr((ws_data.df$lat),2,7)),sep='')) / 1000

# library(rworldmap)
# newmap <- getMap(resolution = "low")
# plot(newmap, xlim = c(-20, 59), ylim = c(-35, 71), asp = 1)
# points(lon, lat, col = "black", bg= "red", cex = 1.2, pch=23)



# processing gzipped weather station file
processWSFile = function(filepath) {
    #con = file(filepath, "r") # non gzipped
    con <- gzcon(file(filepath,open="rb"))
    is.ws.df <- FALSE
    while ( TRUE ) {
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) {
            break
        }
        if (!is.ws.df) {
            ws.df <- processWSDataLine(line)
            is.ws.df <- TRUE
        } else
            ws.df <- rbind(ws.df,processWSDataLine(line))
    }
    close(con)
    return(ws.df)
}

# processing weather station data line
processWSDataLine = function(line) {

    # example weather station data line
    # 0099128300999992019010100004+47067+017833FM-12+028199999V0203401N002019999999N999999999+00011-00341103231ADDMA1999999099661MD1210051+9999REMSYN05812830 36/// /3402 10001 21034 39966 40323 52005 555 10077=
    # see more details in https://www1.ncdc.noaa.gov/pub/data/noaa/ish-format-document.docx

    # Control data
    tot <- substr(line,1,4) # TOTAL-VARIABLE-CHARACTERS
    usaf <- substr(line,5,10) # FIXED-WEATHER-STATION USAF MASTER STATION CATALOG identifier
    wban <- substr(line,11,15) # FIXED-WEATHER-STATION NCEI WBAN identifier
    datum <- substr(line,16,23) # GEOPHYSICAL-POINT-OBSERVATION date
    utc_time <- substr(line,24,27) # GEOPHYSICAL-POINT-OBSERVATION time
    sflag <- substr(line,28,28) # GEOPHYSICAL-POINT-OBSERVATION data source flag
    lat <- substr(line,29,34) # GEOPHYSICAL-POINT-OBSERVATION latitude coordinate
    lon <- substr(line,35,41) # GEOPHYSICAL-POINT-OBSERVATION longitude coordinate
    greptc <- substr(line,42,46) # GEOPHYSICAL-REPORT-TYPE code
    eled <- substr(line,47,51) # GEOPHYSICAL-POINT-OBSERVATION elevation dimension
    wscl <- substr(line,52,56) # FIXED-WEATHER-STATION call letter identifier
    mpo <- substr(line,57,60) # METEOROLOGICAL-POINT-OBSERVATION quality control process name

    # Weather data
    wind_dir <- substr(line,61,63) # WIND-OBSERVATION direction angle
    wind_dir_qc <- substr(line,64,64) # WIND-OBSERVATION direction quality code
    wind_tc <- substr(line,65,65) # WIND-OBSERVATION type code
    wind_sp_rate <- substr(line,66,69) # WIND-OBSERVATION speed rate
    wind_sp_qc <- substr(line,70,70) # WIND‑OBSERVATION speed quality code
    sky_chd <- substr(line,71,75) # SKY-CONDITION-OBSERVATION ceiling height dimension
    sky_c_qc <- substr(line,76,76) # SKY-CONDTION-OBSERVATION ceiling quality code
    sky_cdc <- substr(line,77,77) # SKY-CONDITION-OBSERVATION ceiling determination code
    sky_cc <- substr(line,78,78) # SKY-CONDITION-OBSERVATION CAVOK code 
    vis_dd <- substr(line,79,84) # VISIBILITY-OBSERVATION distance dimension
    vis_qc <- substr(line,85,85) # VISIBILITY-OBSERVATION distance quality code
    vis_vc <- substr(line,86,86) # VISIBILITY-OBSERVATION variability code
    vis_qvc <- substr(line,87,87) # VISIBILITY-OBSERVATION quality variability code
    air_temp <- substr(line,88,92) # AIR-TEMPERATURE-OBSERVATION air temperature
    air_temp_qc <- substr(line,93,93) # AIR-TEMPERATURE-OBSERVATION air temperature quality code
    air_dewp_temp <- substr(line,94,98) # AIR‑TEMPERATURE‑OBSERVATION dew point temperature
    air_dewp_qc <- substr(line,99,99) # AIR‑TEMPERATURE‑OBSERVATION dew point quality code
    atm_see_pres <- substr(line,100,104) # ATMOSPHERIC-PRESSURE-OBSERVATION sea level pressure
    atm_see_pres_qc <- substr(line,105,105) # ATMOSPHERIC-PRESSURE-OBSERVATION sea level pressure quality code
    
    ws_line <- data.frame(tot,usaf,wban,datum,utc_time,sflag,lat,lon,greptc,eled,wscl,mpo,wind_dir,wind_dir_qc,wind_tc,wind_tc,wind_sp_rate,wind_sp_qc,sky_chd,sky_c_qc,sky_cdc,sky_cc,vis_dd,vis_qc,vis_vc,vis_qvc,air_temp,air_temp_qc,air_dewp_temp,air_dewp_qc,atm_see_pres,atm_see_pres_qc)
    
    return(ws_line)
}

