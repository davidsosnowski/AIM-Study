##################################
# ACEs, Identity & Morality Study
# Data Preparation and QC
# Author: David W. Sosnowski
# Email: dsosnow1@jhu.edu
##################################

getwd()

# load necessary packages
library( haven )

# import data
fall <- read_sav( "AIM Data Fall Original.sav" )
spring <- read_sav( "AIM Data Spring.sav" )

# view data
View( fall )
View( spring )


                          #######################
                          ### QC of Fall data ###
                          #######################




