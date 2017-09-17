#'@title Geocode Polygon Shapes
#'
#'@description This function takes a point in space, given by a longitude and latitude, and projects out the
#'             outline of a radial polygon originating from the point. The outline it projects is made up of
#'             hundreds of additional points. The distance in projects it is based on the radius provided,
#'             and the extent of the polygon is determined by the degrees input into the variable "arc".
#'             If the radius is in any length of measurement other than meters, please also provide a
#'             convertion factor.
#'
#'@param point a numeric vector containing the longitude and latitude of the startin point of a polygon radius
#'@param arc a numeric vector containing the range of degrees over which to draw additional polygon points
#'@param radius a numeric object specifying the length of the radius of a polygon
#'@param conv a numeric object specifying a conversion factor, if the radius is not in meters
#'@param scale a numeric object between 0 and 1, specifying a scale by which to reduce the radius, if any
#'
#'@return a data frame of geocoded longitudes and latitude points, specifying the outline of a polygon
#'
#'@importFrom geosphere destPoint
#'
#'@export
geocode_radii<-function(point,arc=0:360,radius=1,conv=1,scale=1){
    polygon_points<-geosphere::destPoint(p=point,b=arc,d=radius*conv*scale) #generates polyon point projections
    data<-data.frame(x=c(polygon_points[,"lon"],point[1]), #save polygon point projections to a dataframe
                     y=c(polygon_points[,"lat"],point[2]))
    return(data)
}

#'@title Hurricane Geom
#'
#'@description This is the Hurricane Wind Speed geom, which underlies the Hurricane Geom Layer function.
#'             Important parameters for this geom are x (longitude), y (latitude), r_ne, r_se, r_nw, r_sw
#'             (corresponding to wind distances in different directional quadrants), and scale_radii (which
#'             allows the user to scale down the wind effects, though this is optional).
#'
#'@section Aesthetics:
#' \code{geom_hurricane} understands the following aesthetics (required in bold):
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{y}
#'   \item \strong{r_ne}
#'   \item \strong{r_se}
#'   \item \strong{r_nw}
#'   \item \strong{r_sw}
#'   \item color
#'   \item fill
#'   \item size
#'   \item linetype
#'   \item alpha
#'   \item scale_radii
#' }
#'
#'@inheritParams layer
#'
#'@importFrom ggplot2 ggproto
#'@importFrom ggplot2 Geom
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 draw_key_polygon
#'@importFrom grid polygonGrob
#'@importFrom grid gpar
#'
#'@export
GeomHurricane<-ggplot2::ggproto("GeomHurricane",ggplot2::Geom,
                                required_aes=c("x","y","r_ne","r_se","r_sw","r_nw"), #required input parameters
                                default_aes=ggplot2::aes(fill="grey",color="black",size=0.5, #additional parameters
                                                         alpha=0.8,linetype=1,scale_radii=1),
                                draw_key=ggplot2::draw_key_polygon, #ensures that legend is included
                                draw_group = function(data,panel_scales,coord){

                                    point<-c(data[1,]$x,data[1,]$y) #initialize parameters
                                    scale_radii<-data[1,]$scale_radii
                                    naut<-1852

                                    data_ne<-geocode_radii(point,0:90,data[1,]$r_ne,naut,scale_radii) #get geocodes polygon projections
                                    data_se<-geocode_radii(point,90:180,data[1,]$r_se,naut,scale_radii)
                                    data_sw<-geocode_radii(point,180:270,data[1,]$r_sw,naut,scale_radii)
                                    data_nw<-geocode_radii(point,270:360,data[1,]$r_nw,naut,scale_radii)

                                    data_compass<-rbind(data_ne,data_se,data_sw,data_nw) #combine 4 quadrants into one polygon projection
                                    coords<-coord$transform(data_compass,panel_scales)

                                    color<-data[1,]$color #initialize additional parameters
                                    fill<-data[1,]$fill
                                    alpha<-data[1,]$alpha

                                    grid::polygonGrob(x=coords$x,y=coords$y, #build polygon and fill in sections
                                                      gp=grid::gpar(col=color,fill=fill,alpha=alpha))
                                })

#'@title Hurricane Geom Layer
#'
#'@description This function creates a Hurricane Wind Geom on a given ggmap plot. It does so by accessing the
#'             HurricaneGeom object, and providing a framework to plug in its parameters.
#'
#'@param mapping Set of aesthetic mappings created by aes or aes_. If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#'@param data The data to be displayed in this layer. There are three options:
#'
#'            If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#'
#'            A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify for which variables will be created.
#'
#'            A function will be called with a single argument, the plot data. The return value must be a data.frame., and will be used as the layer data.
#'@param stat The statistical transformation to use on the data for this layer, as a string.
#'@param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#'@param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#'@param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#'@param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#'@param ... other arguments passed on to layer. These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3. They may also be parameters to the paired geom/stat.
#'
#'@section Aesthetics:
#' \code{geom_hurricane} understands the following aesthetics (required in bold):
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{y}
#'   \item \strong{r_ne}
#'   \item \strong{r_se}
#'   \item \strong{r_nw}
#'   \item \strong{r_sw}
#'   \item color
#'   \item fill
#'   \item size
#'   \item linetype
#'   \item alpha
#'   \item scale_radii
#'
#'@return a ggplot2 graphical object
#'
#'@importFrom ggplot2 layer
#'
#'@export
geom_hurricane<-function(mapping=NULL,data=NULL,stat="identity",position="identity",
                         show.legend=NA,inherit.aes=TRUE,na.rm=FALSE,...){
    ggplot2::layer(geom=GeomHurricane,mapping=mapping,data=data,stat=stat,position=position, #create geom layer
                   show.legend=show.legend,inherit.aes=inherit.aes,params=list(na.rm=na.rm,...))
}
