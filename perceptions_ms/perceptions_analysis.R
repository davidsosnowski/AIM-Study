################################################################################
# ACEs, Identity & Morality (AIM) Study
# Data Analysis
# Author: David W. Sosnowski
# Email: dsosnow1@jhu.edu
# Manuscript: Perceived Negative Effects of Adverse Childhood Experiences as a 
# Predictor of Depressive and Anxiety Symptoms Among College Students
################################################################################

getwd()

### Load necessary packages
library( tidyverse )
library( psych )
library( jtools )
library( broom )

### Load data
df1 <- read_csv( "data_for_analysis.csv" )


#############################
# Model 1
# Outcome: GAD-7 total score
#############################

### Define & run model
m1 <- lm( GAD7.rtot ~ Age + Race.b + m0f1 + factor( ses1 ) + ACE.percep + 
           ACE.count + ACE.percep:ACE.count, data = df1 )

### Model summary
summ( m1 )

### Look at fitted values and residuals 
### for significant predictor (ACE.percep)
m1.diag <- augment( m1 )
head( m1.diag )
ggplot( m1.diag, aes( ACE.percep, GAD7.rtot ) ) +
  geom_point() +
  stat_smooth( method = lm, se = F ) +
  geom_segment( aes( xend = ACE.percep, yend = .fitted ), color = "red", size = 0.3 )

### Checking model assumptions
par( mfrow = c( 2, 2 ) )
plot( m1 )
# Assumptions appear to be met, but want to check for possible outliers

### Add observations indices to diagnostics tibble
m1.diag <- m1.diag %>%
  mutate( index = 1:nrow( m1.diag ) ) %>%
  select( index, everything() )
head( m1.diag, 4 )

### Identify potential outliers
m1.diag %>% top_n( 10, wt = .cooksd )
# Some cases with low GAD-7 scores and high ACEs (and vice verse), but
# no particular reason to think these responses are unreliable.
# Keeping all cases.


#############################
# Model 2
# Outcome: PHQ-9 total score
#############################

### Define & run model
m2 <- lm( PHQ9.rtot ~ Age + Race.b + m0f1 + factor( ses1 ) + ACE.percep + 
            ACE.count + ACE.percep:ACE.count, data = df1 )

### Model summary
summ( m2 )

### Look at fitted values and residuals 
### for significant predictors (ACE.count, ACE.percep)
m2.diag <- augment( m2 )
head( m2.diag )

#ACE.percep
ggplot( m2.diag, aes( ACE.percep, PHQ9.rtot ) ) +
  geom_point() +
  stat_smooth( method = lm, se = F ) +
  geom_segment( aes( xend = ACE.percep, yend = .fitted ), color = "red", size = 0.3 )

#ACE.count
ggplot( m2.diag, aes( ACE.count, PHQ9.rtot ) ) +
  geom_point() +
  stat_smooth( method = lm, se = F ) +
  geom_segment( aes( xend = ACE.count, yend = .fitted ), color = "red", size = 0.3 )

### Checking model assumptions
par( mfrow = c( 2, 2 ) )
plot( m2 )
# Assumptions appear to be met, though may have an issue with heteroscedasticity

### Check this assumption
par( mfrow = c( 1,2 ) )

#View raw values
hist( df1$PHQ9.rtot )
hist( sqrt( df1$PHQ9.rtot ) ) # closer approximation to normal distribution

#Re-run model with sqrt transformation
m2.log <-lm( sqrt( PHQ9.rtot ) ~ Age + Race.b + m0f1 + factor( ses1 ) + ACE.percep + 
               ACE.count + ACE.percep:ACE.count, data = df1 )
summ( m2.log ) # ACE.percep and ACE.count are still significant, but stronger

#Plot residuals & fitted values
plot( m2, 3 )
plot( m2.log, 3 ) # Scale-Location plot doesn't look that much better
plot( m2.log ) # Q-Q plot looks worse
# Use original model

### Add observations indices to diagnostics
m2.diag <- m2.diag %>%
  mutate( index = 1:nrow( m2.diag ) ) %>%
  select( index, everything() )
head( m2.diag, 4 )

### Identify potential outliers
m2.diag %>% top_n( 10, wt = .cooksd )
# Some cases with low PHQ-9 scores and high ACEs (and vice verse), but
# no particular reason to think these responses are unreliable.
# Keeping all cases.



############################################
##            Sensitivity Analyses        ## 
## Remove cases with short study duration ##
############################################

### Remove cases with study duration < 20 minutes
df2 <- df1[ which( df1$Duration.M > 20 ), ]


#############################
# Model 3
# Outcome: GAD-7 total score
#############################

### Define & run model
m3 <- lm( GAD7.rtot ~ Age + Race.b + m0f1 + factor( ses1 ) + ACE.percep + 
            ACE.count + ACE.percep:ACE.count, data = df2 )

### Model summary
summ( m3 ) # similar results

### Look at fitted values and residuals 
### for significant predictor (ACE.percep)
m3.diag <- augment( m3 )
head( m3.diag )
ggplot( m3.diag, aes( ACE.percep, GAD7.rtot ) ) +
  geom_point() +
  stat_smooth( method = lm, se = F ) +
  geom_segment( aes( xend = ACE.percep, yend = .fitted ), color = "red", size = 0.3 )

### Checking model assumptions
par( mfrow = c( 2, 2 ) )
plot( m3 )
# Assumptions appear to be met


#############################
# Model 4
# Outcome: PHQ-9 total score
#############################

### Define & run model
m4 <- lm( PHQ9.rtot ~ Age + Race.b + m0f1 + factor( ses1 ) + ACE.percep + 
            ACE.count + ACE.percep:ACE.count, data = df2 )

### Model summary
summ( m4 ) # similar results

### Look at fitted values and residuals 
### for significant predictors (ACE.count, ACE.percep)
m4.diag <- augment( m4 )
head( m4.diag )

#ACE.percep
ggplot( m4.diag, aes( ACE.percep, PHQ9.rtot ) ) +
  geom_point() +
  stat_smooth( method = lm, se = F ) +
  geom_segment( aes( xend = ACE.percep, yend = .fitted ), color = "red", size = 0.3 )

#ACE.count
ggplot( m4.diag, aes( ACE.count, PHQ9.rtot ) ) +
  geom_point() +
  stat_smooth( method = lm, se = F ) +
  geom_segment( aes( xend = ACE.count, yend = .fitted ), color = "red", size = 0.3 )

### Checking model assumptions
par( mfrow = c( 2, 2 ) )
plot( m4 )
# Assumptions appear to be met

### Results and conclusions are similar after sensitivity analysis

# DONE
################################################################################