#'@title Extract Data and Tidy
#'
#'@description This function simply extracts and tidies hurricane wind data from Colorado State University.
#'
#'@param file a character string specifying the name of a .txt file from which to read some data
#'
#'@return a data frame with a cleaned version of the input file data
#'
#'@importFrom readr read_fwf
#'@importFrom readr fwf_widths
#'@importFrom magrittr %>%
#'@importFrom dplyr mutate
#'@importFrom dplyr select
#'@importFrom tidyr gather
#'@importFrom tidyr spread
#'@importFrom tidyr separate
#'@importFrom stringr str_to_title
#'@importFrom lubridate ymd_h
#'
#'@export
get_stormdata<-function(file="ebtrk_atlc_1988_2015.txt"){
    ext_tracks_widths<-c(7,10,2,2,3,5,5,6,4,5,4,4,5,3,4,3,3,3,4,3,3,3,4,3,3,3,2,6,1) #pre-formatting

    ext_tracks_colnames<-c("storm_id","storm_name","month","day","hour","year","latitude","longitude",
                           "max_wind","min_pressure","rad_max_wind","eye_diameter","pressure_1","pressure_2",
                           paste("radius_34",c("ne","se","sw","nw"),sep="_"),
                           paste("radius_50",c("ne","se","sw","nw"),sep="_"),
                           paste("radius_64",c("ne","se","sw","nw"),sep="_"),
                           "storm_type","distance_to_land","final")

    ext_tracks<-readr::read_fwf(file,readr::fwf_widths(ext_tracks_widths,ext_tracks_colnames),na="-99")

    ext_tracks_cleaned<-ext_tracks%>%
        dplyr::mutate(storm_id=paste0(stringr::str_to_title(storm_name),"-",year), #create storm id that includes year
                      longitude=180-((longitude+180)%%360), #change longitude to different scale
                      date=lubridate::ymd_h(paste(year,month,day,hour,sep="_")))%>% #condense date/time info
        dplyr::select(storm_id,date,latitude,longitude,radius_34_ne:radius_64_nw)%>%
        tidyr::gather(key=variable,value=radius,dplyr::starts_with("radius"))%>% #condense wind radius info
        tidyr::separate(col=variable,into=c("suffix","wind_speed","quadrant"),sep="_")%>% #split wind speed from distance
        tidyr::spread(key=quadrant,value=radius)%>% #spread distance info out again
        dplyr::select(-suffix)

    return(ext_tracks_cleaned)
}
