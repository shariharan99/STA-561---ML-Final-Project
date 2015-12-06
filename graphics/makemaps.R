#!/usr/bin/Rscript
# This code was written by Azeem Zaman
# please contact [first] . [last] @ duke (dot) edu with questions

# load packages
library(rgdal)
library(maptools)
# this is a look-up table used to convert each state to its FIPS code
# it will be used to remove Alaska and Hawaii, which improves the 
# project and thus the plot
state.to.fip <- c("Alabama" = '01',
                  "Alaska" = '02',
                  "Arizona" = '04',
                  "Arkansas" = '05',
                  "California" = '06',
                  "Colorado" = '08',
                  "Connecticut" = '09',
                  "Delaware" = '10',
                  "District of Columbia" = '11',
                  "Florida" = '12',
                  "Georgia" = '13',
                  "Hawaii" = '15',
                  "Idaho" = '16',
                  "Illinois" = '17',
                  "Indiana" = '18',
                  "Iowa" = '19',
                  "Kansas" = '20',
                  "Kentucky" = '21',
                  "Louisiana" = '22',
                  "Maine" = '23',
                  "Maryland" = '24',
                  "Massachusetts" = '25',
                  "Michigan" = '26',
                  "Minnesota" = '27',
                  "Mississippi" = '28',
                  "Missouri" = '29',
                  "Montana" = '30',
                  "Nebraska" = '31',
                  "Nevada" = '32',
                  "New Hampshire" = '33',
                  "New Jersey" = '34',
                  "New Mexico" = '35',
                  "New York" = '36',
                  "North Carolina" = '37',
                  "North Dakota" = '38',
                  "Ohio" = '39',
                  "Oklahoma" = '40',
                  "Oregon" = '41',
                  "Pennsylvania" = '42',
                  "Rhode Island" = '44',
                  "South Carolina" = '45',
                  "South Dakota" = '46',
                  "Tennessee" = '47',
                  "Texas" = '48',
                  "Utah" = '49',
                  "Vermont" = '50',
                  "Virginia" = '51',
                  "Washington" = '53',
                  "West Virginia" = '54',
                  "Wisconsin" = '55',
                  "Wyoming" = '56')

# the heat map code is based on a function by Eduardo Leoni
# found on stack overflow
# http://stackoverflow.com/questions/1260965/developing-geographic-thematic-maps-with-r

# this reads in the shapefile with the US school district boundaries
my.map <- readShapePoly('Shiny/fullUS')


plot.heat <- function(state.map, 
                      col.var, 
                      breaks = 5, 
                      plot.legend = TRUE, 
                      title = NULL,
                      width = 1){
###############################################################################
# This function creates a heat map for a given variable
#
# Inputs
#  -state.map:  a shapefile with data on the states to be plotted
#  -col.var: the name of the variable that is being head mapped (colored)
#  -break:  the number of colors to be used in the heat map
#  -plot.legend:  turns legend on and off
#  -title: can be used to specify main title for heat map
#  -width:  the width of the lines used to plot district boundaries (can be NA)
#
# Outputs
#  A plot of the specified states with the specified paramters.  The variable
#  col.var will be plotted as a heat map on the states
###############################################################################
  # breaks col.var into categories
  state.map@data$col.var.cat <- cut(state.map@data[,col.var],
                                    breaks,
                                    include.lowest=TRUE)
  # gets the end points of intervals for the heat levels
  cutpoints <- levels(state.map@data$col.var.cat)
  # gets a vector of colors to be used in heat map
  col.vec <- heat.colors(length(levels(state.map@data$col.var.cat)))
  # assigns the heat colors to the levels 
  levels(state.map@data$col.var.cat) <- col.vec
  # plots the desired states
  plot(state.map,
       border=NA, # here we have turned borders 
       axes = FALSE,
       lwd = width,
       las = 1,
       col=as.character(state.map@data$col.var.cat))
  if (plot.legend){ # add legend to plot
    legend("bottomleft", 
           cutpoints, 
           fill = col.vec,
           bty="n",
           title=title,
           cex=1)
  }
}
# make pdf plot of diversity for continetnal US
pdf("USdiverplot.pdf")    
plot.heat(my.map[!(my.map$STATEFP %in% c('02','15')),],
              'diver', width = .1)
dev.off()
# make pdf plot of graduation rate for continental US
pdf("USrateplot.pdf")    
plot.heat(my.map[!(my.map$STATEFP %in% c('02','15')),],
          'ALL_RAT', width = .001)
dev.off()
