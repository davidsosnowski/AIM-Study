#######################################
# AIM Study
# Perceptions manuscript
# Data analysis
# David W. Sosnowski, PhD
#######################################

getwd()

# load necessary packages
library( tidyverse )
library( psych )
library( jtools )

# load data
df1 <- read_csv( "/Users/david/Desktop/aim_study/perceptions_ms/perceptions_fall_spring_cleaned.csv" )


###################################
# Model 1
# Outcome: GAD-7 total score
###################################

summ( m5.adj <- lm( GAD7.rtot ~ Age + Race.b + m0f1 + factor( ses1 ) + 
                      ACE.percep + ACE.count + ACE.percep:ACE.count, 
                    data = df1 ) )


#################################
# Model 2
# Outcome: PHQ-9
# Predictor: ACE score*Impact
#################################

#PHQ-9 total score: adjusted model
summ( m6.adj <- lm( PHQ9.rtot ~ Age + Race.b + m0f1 + factor( ses1 ) + 
                      ACE.percep + ACE.count + ACE.percep:ACE.count, 
                    data = df1 ) )


