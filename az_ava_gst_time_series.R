

#  This code produces a time series graph of growing season 
#  temperature for the three American Viticultural Areas (AVAs)
#  in Arizona, as used in the 2019 November issue of the 
#  Climate Viticulture Newsletter:
#  https://cals.arizona.edu/research/climategem/content/climate-viticulture-newsletter-2019-november#gst

#  Growing season temperature (GST) is defined as the average 
#  temperature from April 1 through October 31.

#  GST data are based on daily PRISM data: 
#  (http://www.prism.oregonstate.edu/).
#  For simplicity, the PRISM-derived, preprocessed GST data are
#  included and used in this script.


#####  SETUP


#  Load required libraries.
library( "rgdal" )
library( "raster" )
library( "ggplot2" )

#  Set the first and last years of the overall analysis period,
#  which is based on daily PRISM data availability.
yr_start <- 1981
yr_end <- 2019

#  Set the years of the climatological, or baseline, period.
yr_start_clim <- 1981
yr_end_clim <- 2010

#  Load latitude and longitude values for the state that 
#  correspond to the gridcell centers of the 4-km PRISM grid mesh.
#  Then, create vectors containing unique latitude and longitude values.
#  Sort the latitude vector from largest to smallest values in order to
#  match reading direction of data with rasters.
latlon_state <- read.csv( file = "prism_mesh_gcsna1983_az_bndbox.csv",
                          header = TRUE )
lon_state <- unique( latlon_state$x_center )
lat_state <- sort( x = unique( latlon_state$y_center ),
                   decreasing = TRUE )
rm( latlon_state )

#  Load rasters of annual GST values.
for ( yr in yr_start:yr_end ) {
  
  #  Set the raster name of current year GST data.
  raster_name <- paste( "gst_data",
                        paste0( "gst",
                                "_",
                                "az",
                                yr,
                                ".tif" ),
                        sep = "/" )
  
  #  Create a raster layer object from current year GST data.
  annual_raster <- raster( raster_name )
  
  #  Initiate or add to annual raster stack that will contain data
  #  from all years in the analysis period.
  if ( yr == yr_start ) {
    prism_stack_az <- annual_raster
  } else {
    prism_stack_az <- stack( prism_stack_az,annual_raster )
  }
  
  rm( annual_raster,raster_name )
  
}
rm( yr )


#####  CALCULATE GST TIME SERIES FOR AVAS


#  SONOITA AVA


#  Set AVA name.
ava_name <- "sonoita_ava"

#  Load latitude and longitude values for the AVA that correspond 
#  to the gridcell centers of the 4-km PRISM grid mesh.
latlon_ava <- read.csv( paste0( "prism_mesh_gcsna1983_",
                                ava_name,
                                ".csv" ) )

#  Initialize a dataframe that will take annual GST values for
#  each PRISM gridcell within the AVA.
ava_data <- as.data.frame( matrix( data = NA,
                                   nrow = length( yr_start:yr_end ),
                                   ncol = nrow( latlon_ava ) ) )

#  For each PRISM gridcell overlaying the AVA, extract data from
#  the PRISM-based GST raster stack.
for ( gridcell in 1:nrow( latlon_ava ) ) {
  
  ava_data[ ,gridcell ] <- as.numeric( prism_stack_az[ 
    #  Find matching latitude between AVA and state values.
    which( lat_state == latlon_ava$y_center[ gridcell ],
           arr.ind = FALSE ),
    #  find matching longitude between AVA and state values.
    which( lon_state == latlon_ava$x_center[ gridcell ],
           arr.ind = FALSE ) ] )
  
  #  Set a column name to latitude and longitude values of current
  #  gridcell.
  gridcell_colname <- paste( as.character( latlon_ava$y_center[ gridcell ] ),
                             as.character( latlon_ava$x_center[ gridcell ] ),
                             sep = "_" )
  
  #  Iteratively define column names for dataframe with annual GST 
  #  values.
  if ( gridcell == 1 ) {
    gridcell_colname_list <- gridcell_colname
  } else {
    gridcell_colname_list <- c( gridcell_colname_list,gridcell_colname )
  }
  
  #  Write column names after data for all gridcells has been
  #  extracted.
  if ( gridcell == nrow( latlon_ava ) ) {
    colnames( ava_data ) <- gridcell_colname_list
  }
  
  rm( gridcell_colname )
}
rm( gridcell,gridcell_colname_list,latlon_ava )

#  Initialize a dataframe that will take GST summary statistics
#  for the AVA.
ava_data_summary <- as.data.frame( matrix( data = NA,
                                           nrow = length( yr_start:yr_end ),
                                           ncol = 3 ) )
colnames( ava_data_summary ) <- c( "average","perc_5","perc_95" )

#  Calculate average, 5th percentile, and 95th percentile GST 
#  values for each year of the analysis period based on data from 
#  across the AVA.
ava_data_summary$average <- rowMeans( ava_data )
for ( i in 1:length( yr_start:yr_end ) ) {
  ava_data_summary$perc_5[ i ] <- as.numeric( quantile( ava_data[ i, ],0.05 ) )
  ava_data_summary$perc_95[ i ] <- as.numeric( quantile( ava_data[ i, ],0.95 ) )
}
rm( i )

#  Add the average GST value of the climatology period to 
#  summary statistics dataframe.
x <- ava_data[ which( yr_start:yr_end==yr_start_clim ):
                 which( yr_start:yr_end==yr_end_clim ), ]
ava_data_summary[ "clim" ] <- mean( rowMeans( x ) )
rm( x )

#  Convert GST values from degrees C to degrees F.
ava_data_summary <- ( ava_data_summary * ( 9/5 ) ) + 32

#  Add additional variables that will aid in time series graphic.
ava_data_summary[ "year" ] <- yr_start:yr_end
ava_data_summary[ "ava" ] <- "Sonoita"

#  Rename AVA data and summary statistics.
ava_data_sonoita <- ava_data
ava_data_summary_sonoita <- ava_data_summary

rm( ava_data,ava_data_summary,ava_name )


#  PROPOSED VERDE VALLEY AVA


#  Set AVA name.
ava_name <- "verde_valley_ava"

#  Load latitude and longitude values for the AVA that correspond 
#  to the gridcell centers of the 4-km PRISM grid mesh.
latlon_ava <- read.csv( paste0( "prism_mesh_gcsna1983_",
                                ava_name,
                                ".csv" ) )

#  Initialize a dataframe that will take annual GST values for
#  each PRISM gridcell within the AVA.
ava_data <- as.data.frame( matrix( data = NA,
                                   nrow = length( yr_start:yr_end ),
                                   ncol = nrow( latlon_ava ) ) )

#  For each PRISM gridcell overlaying the AVA, extract data from
#  the PRISM-based GST raster stack.
for ( gridcell in 1:nrow( latlon_ava ) ) {
  
  ava_data[ ,gridcell ] <- as.numeric( prism_stack_az[ 
    #  Find matching latitude between AVA and state values.
    which( lat_state == latlon_ava$y_center[ gridcell ],
           arr.ind = FALSE ),
    #  find matching longitude between AVA and state values.
    which( lon_state == latlon_ava$x_center[ gridcell ],
           arr.ind = FALSE ) ] )
  
  #  Set a column name to latitude and longitude values of current
  #  gridcell.
  gridcell_colname <- paste( as.character( latlon_ava$y_center[ gridcell ] ),
                             as.character( latlon_ava$x_center[ gridcell ] ),
                             sep = "_" )
  
  #  Iteratively define column names for dataframe with annual GST 
  #  values.
  if ( gridcell == 1 ) {
    gridcell_colname_list <- gridcell_colname
  } else {
    gridcell_colname_list <- c( gridcell_colname_list,gridcell_colname )
  }
  
  #  Write column names after data for all gridcells has been
  #  extracted.
  if ( gridcell == nrow( latlon_ava ) ) {
    colnames( ava_data ) <- gridcell_colname_list
  }
  
  rm( gridcell_colname )
}
rm( gridcell,gridcell_colname_list,latlon_ava )

#  Initialize a dataframe that will take GST summary statistics
#  for the AVA.
ava_data_summary <- as.data.frame( matrix( data = NA,
                                           nrow = length( yr_start:yr_end ),
                                           ncol = 3 ) )
colnames( ava_data_summary ) <- c( "average","perc_5","perc_95" )

#  Calculate average, 5th percentile, and 95th percentile GST 
#  values for each year of the analysis period based on data from 
#  across the AVA.
ava_data_summary$average <- rowMeans( ava_data )
for ( i in 1:length( yr_start:yr_end ) ) {
  ava_data_summary$perc_5[ i ] <- as.numeric( quantile( ava_data[ i, ],0.05 ) )
  ava_data_summary$perc_95[ i ] <- as.numeric( quantile( ava_data[ i, ],0.95 ) )
}
rm( i )

#  Add the average GST value of the climatology period to 
#  summary statistics dataframe.
x <- ava_data[ which( yr_start:yr_end==yr_start_clim ):
                 which( yr_start:yr_end==yr_end_clim ), ]
ava_data_summary[ "clim" ] <- mean( rowMeans( x ) )
rm( x )

#  Convert GST values from degrees C to degrees F.
ava_data_summary <- ( ava_data_summary * ( 9/5 ) ) + 32

#  Add additional variables that will aid in time series graphic.
ava_data_summary[ "year" ] <- yr_start:yr_end
ava_data_summary[ "ava" ] <- "proposed Verde Valley AVA"

#  Rename AVA data and summary statistics.
ava_data_verde_valley <- ava_data
ava_data_summary_verde_valley <- ava_data_summary

rm( ava_data,ava_data_summary,ava_name )


#  WILLCOX AVA


#  Set AVA name.
ava_name <- "willcox_ava"

#  Load latitude and longitude values for the AVA that correspond 
#  to the gridcell centers of the 4-km PRISM grid mesh.
latlon_ava <- read.csv( paste0( "prism_mesh_gcsna1983_",
                                ava_name,
                                ".csv" ) )

#  Initialize a dataframe that will take annual GST values for
#  each PRISM gridcell within the AVA.
ava_data <- as.data.frame( matrix( data = NA,
                                   nrow = length( yr_start:yr_end ),
                                   ncol = nrow( latlon_ava ) ) )

#  For each PRISM gridcell overlaying the AVA, extract data from
#  the PRISM-based GST raster stack.
for ( gridcell in 1:nrow( latlon_ava ) ) {
  
  ava_data[ ,gridcell ] <- as.numeric( prism_stack_az[ 
    #  Find matching latitude between AVA and state values.
    which( lat_state == latlon_ava$y_center[ gridcell ],
           arr.ind = FALSE ),
    #  find matching longitude between AVA and state values.
    which( lon_state == latlon_ava$x_center[ gridcell ],
           arr.ind = FALSE ) ] )
  
  #  Set a column name to latitude and longitude values of current
  #  gridcell.
  gridcell_colname <- paste( as.character( latlon_ava$y_center[ gridcell ] ),
                             as.character( latlon_ava$x_center[ gridcell ] ),
                             sep = "_" )
  
  #  Iteratively define column names for dataframe with annual GST 
  #  values.
  if ( gridcell == 1 ) {
    gridcell_colname_list <- gridcell_colname
  } else {
    gridcell_colname_list <- c( gridcell_colname_list,gridcell_colname )
  }
  
  #  Write column names after data for all gridcells has been
  #  extracted.
  if ( gridcell == nrow( latlon_ava ) ) {
    colnames( ava_data ) <- gridcell_colname_list
  }
  
  rm( gridcell_colname )
}
rm( gridcell,gridcell_colname_list,latlon_ava )

#  Initialize a dataframe that will take GST summary statistics
#  for the AVA.
ava_data_summary <- as.data.frame( matrix( data = NA,
                                           nrow = length( yr_start:yr_end ),
                                           ncol = 3 ) )
colnames( ava_data_summary ) <- c( "average","perc_5","perc_95" )

#  Calculate average, 5th percentile, and 95th percentile GST 
#  values for each year of the analysis period based on data from 
#  across the AVA.
ava_data_summary$average <- rowMeans( ava_data )
for ( i in 1:length( yr_start:yr_end ) ) {
  ava_data_summary$perc_5[ i ] <- as.numeric( quantile( ava_data[ i, ],0.05 ) )
  ava_data_summary$perc_95[ i ] <- as.numeric( quantile( ava_data[ i, ],0.95 ) )
}
rm( i )

#  Add the average GST value of the climatology period to 
#  summary statistics dataframe.
x <- ava_data[ which( yr_start:yr_end==yr_start_clim ):
                 which( yr_start:yr_end==yr_end_clim ), ]
ava_data_summary[ "clim" ] <- mean( rowMeans( x ) )
rm( x )

#  Convert GST values from degrees C to degrees F.
ava_data_summary <- ( ava_data_summary * ( 9/5 ) ) + 32

#  Add additional variables that will aid in time series graphic.
ava_data_summary[ "year" ] <- yr_start:yr_end
ava_data_summary[ "ava" ] <- "Willcox AVA"

#  Rename AVA data and summary statistics.
ava_data_willcox <- ava_data
ava_data_summary_willcox <- ava_data_summary

rm( ava_data,ava_data_summary,ava_name )


#####  CREATE AND SAVE THE AVA GST TIME SERIES FIGURE


#  FIGURE SETUP


#  To help with setting axis breaks on the figures, create a
#  function that rounds numbers to the nearest specified base.
myround <- function( x,base ) {
  base*round( x/base )
}

#  Bring in the 'multiplot' function to combine separate graphs
#  in the same figure.
source( "multiplot.R" )


#  SONOITA AVA


#  Create a ggplot object for the GST data.
t1 <- ggplot( data = ava_data_summary_sonoita ) +
  #  Add the GST climatology data as a time series.
  geom_line( aes( x = year,
                  y = clim ),
             color = "#66c2a5",
             linetype = "dashed",
             size = 1.0 ) +
  #  Set the 5th/95th percentile shading in the background.
  geom_ribbon( aes( x = year,
                    ymin = perc_5,
                    ymax = perc_95 ),
               alpha = 0.33,
               fill = "#66c2a5" ) +
  #  Add the annual GST data as a time series.
  geom_line( aes( x = year,
                  y = average ),
             color = "#66c2a5",
             size = 2.0 ) +
  #  Add the graph title.
  ggtitle( "Growing Season Temperature - Sonoita AVA" ) +
  #  Add the subtitle, x/y axis labels, and caption.
  labs( subtitle = paste0( "1981-2010 average: ",
                           round( ava_data_summary_sonoita$clim[ 1 ],1 ),
                           "°F" ),
        x = "year",
        y = "°F",
        caption = "\ndata source: PRISM (prism.oregonstate.edu)" ) +
  #  Specify the breaks, gridlines, and limits of both plot axes.
  scale_x_continuous( breaks = seq( myround( min( ava_data_summary_sonoita$year ),10 ),
                                    myround( max( ava_data_summary_sonoita$year ),10 ),
                                    by = 5 ),
                      limits = c( ( min( ava_data_summary_sonoita$year ) ),
                                  ( max( ava_data_summary_sonoita$year ) ) ),
                      minor_breaks = seq( min( ava_data_summary_sonoita$year ),
                                          max( ava_data_summary_sonoita$year ),
                                          by = 1 ) ) +
  scale_y_continuous( breaks = seq( myround( floor( min( ava_data_summary_sonoita$perc_5 ) ),1 ),
                                    myround( ceiling( max( ava_data_summary_sonoita$perc_95 ) ),1 ),
                                    by = 1 ),
                      limits = c( ( min( ava_data_summary_sonoita$perc_5 ) ),
                                  ( max( ava_data_summary_sonoita$perc_95 ) ) ) ) +
  #  Specify the ggplot theme, or overall appearance, of the
  #  graph with the font set to 'mono'.
  theme_light( base_family = "mono" ) +
  #  Further customize the appearance of the graph.
  theme( axis.line = element_blank(),
         axis.text.x = element_text( color = "gray30",
                                     size = 10 ),
         axis.text.y = element_text( color="gray30",
                                     size = 10 ),
         axis.ticks.length = unit( 0.0,"mm" ),
         axis.title.x = element_text( color = "gray30",
                                      face = "bold",
                                      size = 10 ),
         axis.title.y = element_text( color = "gray30",
                                      face = "bold",
                                      size = 10 ),
         legend.title = element_text( color = "black",
                                      size = 8 ),
         legend.position = "none",
         panel.border = element_blank(),
         panel.grid.major.x = element_line( color = "gray60",
                                            size = 0.3 ),
         panel.grid.major.y = element_line( color = "gray60",
                                            size = 0.3 ),
         panel.grid.minor.x = element_line( color = "gray60",
                                            size = 0.3 ),
         panel.grid.minor.y = element_blank(),
         plot.caption = element_text( hjust = 0 ),
         plot.margin = unit( c( 2,2,2,2 ),"mm" ),
         plot.subtitle = ( element_text( size = 10 ) ), 
         plot.title = ( element_text( face = "bold",
                                      size = 12 ) ) )


#  WILLCOX AVA


#  Create a ggplot object for the GST data.
t2 <- ggplot( data = ava_data_summary_willcox ) +
  #  Add the GST climatology data as a time series.
  geom_line( aes( x = year,
                  y = clim ),
             color = "#fc8d62",
             linetype = "dashed",
             size = 1.0 ) +
  #  Set the 5th/95th percentile shading in the background.
  geom_ribbon( aes( x = year,
                    ymin = perc_5,
                    ymax = perc_95 ),
               alpha = 0.33,
               fill = "#fc8d62" ) +
  #  Add the annual GST data as a time series.
  geom_line( aes( x = year,
                  y = average ),
             color = "#fc8d62",
             size = 2.0 ) +
  #  Add the graph title.
  ggtitle( "Growing Season Temperature - Willcox AVA" ) +
  #  Add the subtitle, x/y axis labels, and caption.
  labs( subtitle = paste0( "1981-2010 average: ",
                           round( ava_data_summary_willcox$clim[ 1 ],1 ),
                           "°F" ),
        x = "year",
        y = "°F",
        caption = "\ndata source: PRISM (prism.oregonstate.edu)" ) +
  #  Specify the breaks, gridlines, and limits of both plot axes.
  scale_x_continuous( breaks = seq( myround( min( ava_data_summary_willcox$year ),10 ),
                                    myround( max( ava_data_summary_willcox$year ),10 ),
                                    by = 5 ),
                      limits = c( ( min( ava_data_summary_willcox$year ) ),
                                  ( max( ava_data_summary_willcox$year ) ) ),
                      minor_breaks = seq( min( ava_data_summary_willcox$year ),
                                          max( ava_data_summary_willcox$year ),by = 1 ) ) +
  scale_y_continuous( breaks = seq( myround( floor( min( ava_data_summary_willcox$perc_5 ) ),1 ),
                                    myround( ceiling( max( ava_data_summary_willcox$perc_95 ) ),1 ),
                                    by = 1 ),
                      limits = c( ( min( ava_data_summary_willcox$perc_5 ) ),
                                  ( max( ava_data_summary_willcox$perc_95 ) ) ) ) +
  #  Specify the ggplot theme, or overall appearance, of the
  #  graph with the font set to 'mono'.
  theme_light( base_family = "mono" ) +
  #  Further customize the appearance of the graph.
  theme( axis.line = element_blank(),
         axis.text.x = element_text( color = "gray30",
                                     size = 10 ),
         axis.text.y = element_text( color="gray30",
                                     size = 10 ),
         axis.ticks.length = unit( 0.0,"mm" ),
         axis.title.x = element_text( color = "gray30",
                                      face = "bold",
                                      size = 10 ),
         axis.title.y = element_text( color = "gray30",
                                      face = "bold",
                                      size = 10 ),
         legend.title = element_text( color = "black",
                                      size = 8 ),
         legend.position = "none",
         panel.border = element_blank(),
         panel.grid.major.x = element_line( color = "gray60",
                                            size = 0.3 ),
         panel.grid.major.y = element_line( color = "gray60",
                                            size = 0.3 ),
         panel.grid.minor.x = element_line( color = "gray60",
                                            size = 0.3 ),
         panel.grid.minor.y = element_blank(),
         plot.caption = element_text( color = "white",
                                      hjust = 0 ),
         plot.margin = unit( c( 2,2,2,2 ),"mm" ),
         plot.subtitle = ( element_text( size = 10 ) ), 
         plot.title = ( element_text( face = "bold",
                                      size = 12 ) ) )


#  PROPOSED VERDE VALLEY AVA


#  Create a ggplot object for the GST data.
t3 <- ggplot( data = ava_data_summary_verde_valley ) +
  #  Add the GST climatology data as a time series.
  geom_line( aes( x = year,
                  y = clim ),
             color = "#8da0cb",
             linetype = "dashed",
             size = 1.0 ) +
  #  Set the 5th/95th percentile shading in the background.
  geom_ribbon( aes( x = year,
                    ymin = perc_5,
                    ymax = perc_95 ),
               alpha = 0.33,
               fill = "#8da0cb" ) +
  #  Add the annual GST data as a time series.
  geom_line( aes( x = year,
                  y = average ),
             color = "#8da0cb",
             size = 2.0 ) +
  #  Add the graph title.
  ggtitle( "Growing Season Temperature - proposed Verde Valley AVA" ) +
  #  Add the subtitle, x/y axis labels, and caption.
  labs( subtitle = paste0( "1981-2010 average: ",
                           round( ava_data_summary_verde_valley$clim[ 1 ],1 ),
                           "°F" ),
        x = "year",
        y = "°F",
        caption = "\ndata source: PRISM (prism.oregonstate.edu)" ) +
  #  Specify the breaks, gridlines, and limits of both plot axes.
  scale_x_continuous( breaks = seq( myround( min( ava_data_summary_verde_valley$year ),10 ),
                                    myround( max( ava_data_summary_verde_valley$year ),10 ),
                                    by = 5 ),
                      limits = c( ( min( ava_data_summary_verde_valley$year ) ),
                                  ( max( ava_data_summary_verde_valley$year ) ) ),
                      minor_breaks = seq( min( ava_data_summary_verde_valley$year ),
                                          max( ava_data_summary_verde_valley$year ),by = 1 ) ) +
  scale_y_continuous( breaks = seq( myround( floor( min( ava_data_summary_verde_valley$perc_5 ) ),1 ),
                                    myround( ceiling( max( ava_data_summary_verde_valley$perc_95 ) ),1 ),
                                    by = 1 ),
                      limits = c( ( min( ava_data_summary_verde_valley$perc_5 ) ),
                                  ( max( ava_data_summary_verde_valley$perc_95 ) ) ) ) +
  #  Specify the ggplot theme, or overall appearance, of the
  #  graph with the font set to 'mono'.
  theme_light( base_family = "mono" ) +
  #  Further customize the appearance of the graph.
  theme( axis.line = element_blank(),
         axis.text.x = element_text( color = "gray30",
                                     size = 10 ),
         axis.text.y = element_text( color="gray30",
                                     size = 10 ),
         axis.ticks.length = unit( 0.0,"mm" ),
         axis.title.x = element_text( color = "gray30",
                                      face = "bold",
                                      size = 10 ),
         axis.title.y = element_text( color = "gray30",
                                      face = "bold",
                                      size = 10 ),
         legend.title = element_text( color = "black",
                                      size = 8 ),
         legend.position = "none",
         panel.border = element_blank(),
         panel.grid.major.x = element_line( color = "gray60",
                                            size = 0.3 ),
         panel.grid.major.y = element_line( color = "gray60",
                                            size = 0.3 ),
         panel.grid.minor.x = element_line( color = "gray60",
                                            size = 0.3 ),
         panel.grid.minor.y = element_blank(),
         plot.caption = element_text( color = "white",
                                      hjust = 0 ),
         plot.margin = unit( c( 2,2,2,2 ),"mm" ),
         plot.subtitle = ( element_text( size = 10 ) ), 
         plot.title = ( element_text( face = "bold",
                                      size = 12 ) ) )


#  PLOT THE THREE GRAPHS IN A VERTICAL STACKAND SAVE FIGURE


#  Combine the three time series plots into one graphic.
t123 <- multiplot( t3,t2,t1,cols=1 )  

#  Save the figure as a .png file.
ggsave( "gst-time-series-stack.png",
        plot=multiplot( t3,t2,t1,cols = 1 ),
        device = "png",
        path = NULL,
        scale = 1,
        width = 6.0,
        height = 8.0,
        units = "in",
        dpi = 600 )


#####

