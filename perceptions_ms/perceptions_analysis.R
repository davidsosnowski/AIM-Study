############################################################################
# Perceived Negative Effects of Adverse Childhood Experiences 
# as a Predictor of Depressive and Anxiety Symptoms Among College Students
############################################################################

getwd()

# load necessary packages
library( tidyverse )
library( psych )
library( jtools )

# load data
df1 <- read_csv( "/Users/david/Desktop/aim_study/perceptions_ms/perceptions_fall_spring_cleaned.csv" )


#############################
# Model 1
# Outcome: GAD-7 total score
#############################

summ( m1.adj <- lm( GAD7.rtot ~ Age + Race.b + m0f1 + factor( ses1 ) + 
                      ACE.percep + ACE.count + ACE.percep:ACE.count, 
                    data = df1 ) )


#############################
# Model 2
# Outcome: PHQ-9 total score
#############################

#PHQ-9 total score: adjusted model
summ( m2.adj <- lm( PHQ9.rtot ~ Age + Race.b + m0f1 + factor( ses1 ) + 
                      ACE.percep + ACE.count + ACE.percep:ACE.count, 
                    data = df1 ) )



###############################
# Sensitivity Analysis:
# Inclusion of COVID covariate
###############################

#############################
# Model 3
# Outcome: GAD-7 total score
#############################

summ( m3.adj <- lm( GAD7.rtot ~ Age + Race.b + m0f1 + factor( ses1 ) + COVID5 + 
                      ACE.percep + ACE.count + ACE.percep:ACE.count, data = df1 ) )


#############################
# Model 4
# Outcome: PHQ-9 total score
#############################

summ( m4.adj <- lm( PHQ9.rtot ~ Age + Race.b + m0f1 + factor( ses1 ) + COVID6 + 
                      ACE.percep + ACE.count + ACE.percep:ACE.count, data = df1 ) )

