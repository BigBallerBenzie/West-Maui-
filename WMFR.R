library(rayshader)
library(sp)
library(raster)
library(scales)
library(rgdal)
memory.limit(size = 56000)

#import elevation
elevation1 = raster::raster("") #path to .hgt file
elevation2 = raster::raster("") #path to .hgt file


wmfr_elevation = raster::merge(elevation1,elevation2)

height_shade(raster_to_matrix(wmfr_elevation)) %>%
  plot_map()

#import raster
wmfr_r = raster::raster("") #path to b4 image 
wmfr_g = raster::raster("") #path to b3 image 
wmfr_b = raster::raster("") #path to b2 image 

wmfr_rgb = raster::stack(wmfr_r, wmfr_g, wmfr_b)
raster::plotRGB(wmfr_rgb, scale=255^2)

#correct color
wmfr_rgb_corrected = sqrt(raster::stack(wmfr_r, wmfr_g, wmfr_b))
raster::plotRGB(wmfr_rgb_corrected)

#fix coordnates 
raster::crs(wmfr_r)
raster::crs(wmfr_elevation)
crs(wmfr_r)
wmfr_elevation_utm = raster::projectRaster(wmfr_elevation, crs = crs(wmfr_r), method = "bilinear")
crs(wmfr_elevation_utm)

#crop by coordnates 
bottom_left = c(y=-156.728728, x=20.750757)
top_right   = c(y=-156.456428, x=21.067780)

extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm = sp::spTransform(extent_latlong, raster::crs(wmfr_elevation_utm))

e = raster::extent(extent_utm)
e
wmfr_rgb_cropped = raster::crop(wmfr_rgb_corrected, e)
elevation_cropped = raster::crop(wmfr_elevation_utm, e)

names(wmfr_rgb_cropped) = c("r","g","b")

wmfr_r_cropped = rayshader::raster_to_matrix(wmfr_rgb_cropped$r)
wmfr_g_cropped = rayshader::raster_to_matrix(wmfr_rgb_cropped$g)
wmfr_b_cropped = rayshader::raster_to_matrix(wmfr_rgb_cropped$b)

wmfrel_matrix = rayshader::raster_to_matrix(elevation_cropped)

wmfr_rgb_array = array(0,dim=c(nrow(wmfr_r_cropped),ncol(wmfr_r_cropped),3))

wmfr_rgb_array[,,1] = wmfr_r_cropped/255 #Red layer
wmfr_rgb_array[,,2] = wmfr_g_cropped/255 #Blue layer
wmfr_rgb_array[,,3] = wmfr_b_cropped/255 #Green layer

wmfr_rgb_array = aperm(wmfr_rgb_array, c(2,1,3))
plot_map(wmfr_rgb_array)

#fix color
wmfr_rgb_rescale =scales::rescale_max(wmfr_rgb_array, to = c(.02,.98))
plot_map(wmfr_rgb_rescale)

wmfr_rgb_contrast = scales::rescale(wmfr_rgb_rescale,to=c(.15,1))
plot_map(wmfr_rgb_contrast)

#3d plot
plot_3d(wmfr_rgb_contrast, wmfrel_matrix, windowsize = c(1100,900), zscale = 15, shadowdepth = -50,
        zoom=0.5, phi=65,theta=0,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")

render_snapshot(title_text = "West Maui Forest Reserve, Hawaii | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha 
)

render_movie(filename = "west_maui.mp4", type = "orbit",
             phi = 40,theta = 0,frames = 1440, fps = 60)
                