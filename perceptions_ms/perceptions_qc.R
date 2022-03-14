################################################################################
# ACEs, Identity & Morality (AIM) Study
# Data Preparation and QC
# Author: David W. Sosnowski
# Email: dsosnow1@jhu.edu
# Manuscript: Perceived Negative Effects of Adverse Childhood Experiences as a 
# Predictor of Depressive and Anxiety Symptoms Among College Students
################################################################################

getwd()

### Load necessary packages
library( tidyverse )

### Load data
fall <- read_csv( "AIM_data_fall.csv" )
spring <- read_csv( "AIM_data_spring.csv" )

### Prep fall data for merging
### Subset necessary variables
names( fall )
fall2 <- fall %>% select( ID, Duration.M, Age, Sex, Race1, Race2, Race3, Race4, 
                          Race5, Race6, Race7, Race8, Race_Other, ses1, ses2, 
                          ACE1.1, ACE1.5, ACE2.1, ACE2.5, ACE3.1, ACE3.5, 
                          ACE4.1, ACE4.5, ACE5.1, ACE5.5, ACE6.1, ACE6.5, 
                          ACE7.1, ACE7.5, ACE8.1, ACE8.5, ACE9.1, ACE9.5, 
                          ACE10.1, ACE10.5, ACE11.1, ACE11.5, ACE12.1, ACE12.5,
                          ACE13.1, ACE13.5, ACE14.1, ACE14.5, ACE15.1, ACE15.5,
                          GAD1, GAD2, GAD3, GAD4, GAD5, GAD6, GAD7, PHQ1, PHQ2,
                          PHQ3, PHQ4, PHQ5, PHQ6, PHQ7, PHQ8, PHQ9 )

### Recode sex
fall2$m0f1 <- ifelse( fall2$Sex == 2, 0, fall2$Sex )
table( fall2$m0f1, exclude = NULL )
#  0 (male)   1 (female)
#    65           196 

### Recode race
# 1 (Alaskan, Hawaiian, or American Native )
# 2 (Latina/o/x, Chicana/o, Hispanic, or Spanish Origin)
# 3 (African or Caribbean)
# 4 (Middle Eastern or West Asian)
# 5 (East Asian)
# 6 (Pacific Islander)
# 7 (Caucasian or European American)
# 8 (other) - most responses were Black/African American
fall2$Race.b <- ifelse( fall2$Race7 == 1 & is.na( fall2$Race1 ) & is.na( fall2$Race2 ) & is.na( fall2$Race3 ) & 
                           is.na( fall2$Race4 ) & is.na( fall2$Race5 ) & is.na( fall2$Race6 ) & is.na( fall2$Race8 ), 1, 0 )
table( fall2$Race.b, exclude = NULL )
#  0 (other)   1 (Caucasian/European American)
#     174         87 

### Recode ACEs to 0 = No, 1 = Yes
fall2$ACE1.1 <- ifelse( fall2$ACE1.1 == 5, 1, 0 )
fall2$ACE2.1 <- ifelse( fall2$ACE2.1 == 5, 1, 0 )
fall2$ACE3.1 <- ifelse( fall2$ACE3.1 == 5, 1, 0 )
fall2$ACE4.1 <- ifelse( fall2$ACE4.1 == 5, 1, 0 )
fall2$ACE5.1 <- ifelse( fall2$ACE5.1 == 5, 1, 0 )
fall2$ACE6.1 <- ifelse( fall2$ACE6.1 == 5, 1, 0 )
fall2$ACE7.1 <- ifelse( fall2$ACE7.1 == 5, 1, 0 )
fall2$ACE8.1 <- ifelse( fall2$ACE8.1 == 5, 1, 0 )
fall2$ACE9.1 <- ifelse( fall2$ACE9.1 == 5, 1, 0 )
fall2$ACE10.1 <- ifelse( fall2$ACE10.1 == 5, 1, 0 )
fall2$ACE11.1 <- ifelse( fall2$ACE11.1 == 5, 1, 0 )
fall2$ACE12.1 <- ifelse( fall2$ACE12.1 == 23, 1, 0 )
fall2$ACE13.1 <- ifelse( fall2$ACE13.1 == 1, 1, 0 )
fall2$ACE14.1 <- ifelse( fall2$ACE14.1 == 1, 1, 0 )
fall2$ACE15.1 <- ifelse( fall2$ACE15.1 == 23, 1, 0 )

### Create cohort variable for merging
fall2$cohort <- 1


### Prep spring data for merging
### Subset necessary variables
names( spring )
spring2 <- spring %>% select( ID, Duration.M, Age, Sex, Race1, Race2, Race3, Race4, 
                          Race5, Race6, Race7, Race8, Race_Other, ses1, ses2, 
                          ACE1.1, ACE1.5, ACE2.1, ACE2.5, ACE3.1, ACE3.5, 
                          ACE4.1, ACE4.5, ACE5.1, ACE5.5, ACE6.1, ACE6.5, 
                          ACE7.1, ACE7.5, ACE8.1, ACE8.5, ACE9.1, ACE9.5, 
                          ACE10.1, ACE10.5, ACE11.1, ACE11.5, ACE12.1, ACE12.5,
                          ACE13.1, ACE13.5, ACE14.1, ACE14.5, ACE15.1, ACE15.5,
                          GAD1, GAD2, GAD3, GAD4, GAD5, GAD6, GAD7, PHQ1, PHQ2,
                          PHQ3, PHQ4, PHQ5, PHQ6, PHQ7, PHQ8, PHQ9, COVID1,
                          COVID2, COVID3, COVID4, COVID5, COVID6, COVID7, 
                          COVID8, COVID9 )

### Recode sex
spring2$m0f1 <- ifelse( spring2$Sex == 2, 0, spring2$Sex )
table( spring2$m0f1, exclude = NULL )
#  0 (male)   1 (female)
#    85           279 

### Recode race (see codes above)
spring2$Race.b <- ifelse( spring2$Race7 == 1 & is.na( spring2$Race1 ) & is.na( spring2$Race2 ) & is.na( spring2$Race3 ) & 
                          is.na( spring2$Race4 ) & is.na( spring2$Race5 ) & is.na( spring2$Race6 ) & is.na( spring2$Race8 ), 1, 0 )
table( spring2$Race.b, exclude = NULL )
#  0 (other)   1 (Caucasian/European American)
#     257         107 

### Recode ACEs
spring2$ACE1.1 <- ifelse( spring2$ACE1.1 == 5, 1, 0 )
spring2$ACE2.1 <- ifelse( spring2$ACE2.1 == 5, 1, 0 )
spring2$ACE3.1 <- ifelse( spring2$ACE3.1 == 5, 1, 0 )
spring2$ACE4.1 <- ifelse( spring2$ACE4.1 == 5, 1, 0 )
spring2$ACE5.1 <- ifelse( spring2$ACE5.1 == 5, 1, 0 )
spring2$ACE6.1 <- ifelse( spring2$ACE6.1 == 5, 1, 0 )
spring2$ACE7.1 <- ifelse( spring2$ACE7.1 == 5, 1, 0 )
spring2$ACE8.1 <- ifelse( spring2$ACE8.1 == 5, 1, 0 )
spring2$ACE9.1 <- ifelse( spring2$ACE9.1 == 5, 1, 0 )
spring2$ACE10.1 <- ifelse( spring2$ACE10.1 == 5, 1, 0 )
spring2$ACE11.1 <- ifelse( spring2$ACE11.1 == 5, 1, 0 )
spring2$ACE12.1 <- ifelse( spring2$ACE12.1 == 23, 1, 0 )
spring2$ACE13.1 <- ifelse( spring2$ACE13.1 == 1, 1, 0 )
spring2$ACE14.1 <- ifelse( spring2$ACE14.1 == 1, 1, 0 )
spring2$ACE15.1 <- ifelse( spring2$ACE15.1 == 24, 1, 0 )

### Create cohort variable for merging
spring2$cohort <- 2

### Check if column names match
stopifnot( identical( colnames( fall2 ), colnames( spring2 ) ) )
# COVID items not in fall cohort

### Merge tibbles
df1 <- bind_rows( fall2[ 1:261, 1:64 ], spring2[ 1:364, 1:73 ] )

### Clean up environment
rm( list = c( "fall", "fall2", "spring", "spring2" ) )

# Study specific variables ready for QC #
################################################################################


#################################
## Participant inclusion check ##
#################################

### Check for and remove older (i.e. non-traditional) college students
table( df1$Age ) 

### Remove individuals over age 24 (n = 13)
### Justification: 25+ traditionally used to define "non-traditional" college students 
### See: (https://nces.ed.gov/pubs/web/97578e.asp)
df1 <- df1[ which( df1$Age < 25 ), ]

### Look at study duration
psych::describe( df1$Duration.M )
### Long study duration for some participants, but that's OK
### However, may want to conduct sensitivity analysis
### excluding those who took <= 20 (?) minutes
sum( df1$Duration.M > 20 )


################################################################################

###################################
##   Cleaning outcome variables  ##
## Anxiety & Depressive Symptoms ##
###################################

### Create GAD-7 total scores
df1$GAD7tot <- df1$GAD1 + df1$GAD2 + df1$GAD3 + df1$GAD4 + df1$GAD5 + df1$GAD6 + df1$GAD7

### Check reliability
psych::alpha( df1[ ,c( 46:52 ) ] ) # .89

### Update GAD scoring
### Should be 4-point Likert (not at all, several days, more than half the days, every day)
### We (accidentally) have a 6-point Likert (Never, Almost never, Once in a while, Some days, Most days, Every day)
df1$GAD1.r <- ifelse( df1$GAD1 < 3, 0, df1$GAD1 )
df1$GAD1.r <- ifelse( df1$GAD1 == 3 | df1$GAD1 == 4, 1, df1$GAD1.r )
df1$GAD1.r <- ifelse( df1$GAD1 == 5, 2, df1$GAD1.r )
df1$GAD1.r <- ifelse( df1$GAD1 == 6, 3, df1$GAD1.r )

df1$GAD2.r <- ifelse( df1$GAD2 < 3, 0, df1$GAD2 )
df1$GAD2.r <- ifelse( df1$GAD2 == 3 | df1$GAD2 == 4, 1, df1$GAD2.r )
df1$GAD2.r <- ifelse( df1$GAD2 == 5, 2, df1$GAD2.r )
df1$GAD2.r <- ifelse( df1$GAD2 == 6, 3, df1$GAD2.r )

df1$GAD3.r <- ifelse( df1$GAD3 < 3, 0, df1$GAD3 )
df1$GAD3.r <- ifelse( df1$GAD3 == 3 | df1$GAD3 == 4, 1, df1$GAD3.r )
df1$GAD3.r <- ifelse( df1$GAD3 == 5, 2, df1$GAD3.r )
df1$GAD3.r <- ifelse( df1$GAD3 == 6, 3, df1$GAD3.r )

df1$GAD4.r <- ifelse( df1$GAD4 < 3, 0, df1$GAD4 )
df1$GAD4.r <- ifelse( df1$GAD4 == 3 | df1$GAD4 == 4, 1, df1$GAD4.r )
df1$GAD4.r <- ifelse( df1$GAD4 == 5, 2, df1$GAD4.r )
df1$GAD4.r <- ifelse( df1$GAD4 == 6, 3, df1$GAD4.r )

df1$GAD5.r <- ifelse( df1$GAD5 < 3, 0, df1$GAD5 )
df1$GAD5.r <- ifelse( df1$GAD5 == 3 | df1$GAD5 == 4, 1, df1$GAD5.r )
df1$GAD5.r <- ifelse( df1$GAD5 == 5, 2, df1$GAD5.r )
df1$GAD5.r <- ifelse( df1$GAD5 == 6, 3, df1$GAD5.r )

df1$GAD6.r <- ifelse( df1$GAD6 < 3, 0, df1$GAD6 )
df1$GAD6.r <- ifelse( df1$GAD6 == 3 | df1$GAD6 == 4, 1, df1$GAD6.r )
df1$GAD6.r <- ifelse( df1$GAD6 == 5, 2, df1$GAD6.r )
df1$GAD6.r <- ifelse( df1$GAD6 == 6, 3, df1$GAD6.r )

df1$GAD7.r <- ifelse( df1$GAD7 < 3, 0, df1$GAD7 )
df1$GAD7.r <- ifelse( df1$GAD7 == 3 | df1$GAD7 == 4, 1, df1$GAD7.r )
df1$GAD7.r <- ifelse( df1$GAD7 == 5, 2, df1$GAD7.r )
df1$GAD7.r <- ifelse( df1$GAD7 == 6, 3, df1$GAD7.r )

### Create revised GAD-7 total scores
df1$GAD7.rtot <- df1$GAD1.r + df1$GAD2.r + df1$GAD3.r + df1$GAD4.r + 
  df1$GAD5.r + df1$GAD6.r + df1$GAD7.r

### Check reliability
psych::alpha( df1[ ,c( 75:81 ) ] ) # .87

### Create clinical cut-offs
df1$GAD.clinical <- cut(
  df1$GAD7.rtot,
  breaks = c( 0, 5, 10, 15, Inf ),
  labels = c( "Minimal", "Mild", "Moderate", "Severe" ),
  right  = FALSE
)

table( df1$GAD.clinical )
prop.table( table( df1$GAD.clinical ) )*100


### Create PHQ-9 total scores
df1$PHQtot <- df1$PHQ1 + df1$PHQ2 + df1$PHQ3 + df1$PHQ4 + df1$PHQ5 + df1$PHQ6 + 
  df1$PHQ7 + df1$PHQ8 + df1$PHQ9

### Check reliability
psych::alpha( df1[ ,c( 53:61 ) ] ) # .90

### Update PHQ scoring
### Should be 4-point Likert (not at all, several days, more than half the days, every day)
### We (accidentally) have a 6-point Likert (Never, Almost never, Once in a while, Some days, Most days, Every day)
df1$PHQ1.r <- ifelse( df1$PHQ1 < 3, 0, df1$PHQ1 )
df1$PHQ1.r <- ifelse( df1$PHQ1 == 3 | df1$PHQ1 == 4, 1, df1$PHQ1.r )
df1$PHQ1.r <- ifelse( df1$PHQ1 == 5, 2, df1$PHQ1.r )
df1$PHQ1.r <- ifelse( df1$PHQ1 == 6, 3, df1$PHQ1.r )

df1$PHQ2.r <- ifelse( df1$PHQ2 < 3, 0, df1$PHQ2 )
df1$PHQ2.r <- ifelse( df1$PHQ2 == 3 | df1$PHQ2 == 4, 1, df1$PHQ2.r )
df1$PHQ2.r <- ifelse( df1$PHQ2 == 5, 2, df1$PHQ2.r )
df1$PHQ2.r <- ifelse( df1$PHQ2 == 6, 3, df1$PHQ2.r )

df1$PHQ3.r <- ifelse( df1$PHQ3 < 3, 0, df1$PHQ3 )
df1$PHQ3.r <- ifelse( df1$PHQ3 == 3 | df1$PHQ3 == 4, 1, df1$PHQ3.r )
df1$PHQ3.r <- ifelse( df1$PHQ3 == 5, 2, df1$PHQ3.r )
df1$PHQ3.r <- ifelse( df1$PHQ3 == 6, 3, df1$PHQ3.r )

df1$PHQ4.r <- ifelse( df1$PHQ4 < 3, 0, df1$PHQ4 )
df1$PHQ4.r <- ifelse( df1$PHQ4 == 3 | df1$PHQ4 == 4, 1, df1$PHQ4.r )
df1$PHQ4.r <- ifelse( df1$PHQ4 == 5, 2, df1$PHQ4.r )
df1$PHQ4.r <- ifelse( df1$PHQ4 == 6, 3, df1$PHQ4.r )

df1$PHQ5.r <- ifelse( df1$PHQ5 < 3, 0, df1$PHQ5 )
df1$PHQ5.r <- ifelse( df1$PHQ5 == 3 | df1$PHQ5 == 4, 1, df1$PHQ5.r )
df1$PHQ5.r <- ifelse( df1$PHQ5 == 5, 2, df1$PHQ5.r )
df1$PHQ5.r <- ifelse( df1$PHQ5 == 6, 3, df1$PHQ5.r )

df1$PHQ6.r <- ifelse( df1$PHQ6 < 3, 0, df1$PHQ6 )
df1$PHQ6.r <- ifelse( df1$PHQ6 == 3 | df1$PHQ6 == 4, 1, df1$PHQ6.r )
df1$PHQ6.r <- ifelse( df1$PHQ6 == 5, 2, df1$PHQ6.r )
df1$PHQ6.r <- ifelse( df1$PHQ6 == 6, 3, df1$PHQ6.r )

df1$PHQ7.r <- ifelse( df1$PHQ7 < 3, 0, df1$PHQ7 )
df1$PHQ7.r <- ifelse( df1$PHQ7 == 3 | df1$PHQ7 == 4, 1, df1$PHQ7.r )
df1$PHQ7.r <- ifelse( df1$PHQ7 == 5, 2, df1$PHQ7.r )
df1$PHQ7.r <- ifelse( df1$PHQ7 == 6, 3, df1$PHQ7.r )

df1$PHQ8.r <- ifelse( df1$PHQ8 < 3, 0, df1$PHQ8 )
df1$PHQ8.r <- ifelse( df1$PHQ8 == 3 | df1$PHQ8 == 4, 1, df1$PHQ8.r )
df1$PHQ8.r <- ifelse( df1$PHQ8 == 5, 2, df1$PHQ8.r )
df1$PHQ8.r <- ifelse( df1$PHQ8 == 6, 3, df1$PHQ8.r )

df1$PHQ9.r <- ifelse( df1$PHQ9 < 3, 0, df1$PHQ9 )
df1$PHQ9.r <- ifelse( df1$PHQ9 == 3 | df1$PHQ9 == 4, 1, df1$PHQ9.r )
df1$PHQ9.r <- ifelse( df1$PHQ9 == 5, 2, df1$PHQ9.r )
df1$PHQ9.r <- ifelse( df1$PHQ9 == 6, 3, df1$PHQ9.r )

### Create revised PHQ-9 total scores
df1$PHQ9.rtot <- df1$PHQ1.r + df1$PHQ2.r + df1$PHQ3.r + df1$PHQ4.r + df1$PHQ5.r + 
  df1$PHQ6.r + df1$PHQ7.r + df1$PHQ8.r + df1$PHQ9.r

### Check reliability
psych::alpha( df1[ ,c( 85:93 ) ] ) # .89

### Create clinical cut-offs
df1$PHQ.clinical <- cut(
  df1$PHQ9.rtot,
  breaks = c( 0, 5, 10, 15, Inf ),
  labels = c( "Minimal", "Mild", "Moderate", "Severe" ),
  right  = FALSE
)

table( df1$PHQ.clinical )
prop.table( table( df1$PHQ.clinical ) )*100


### Visualize data
ggplot( df1, aes( x = as.factor( GAD.clinical ), fill = as.factor( GAD.clinical ) ) ) + 
  geom_bar( ) + geom_text( stat = "count", aes( label = ..count.., vjust = -.3 ) ) +
  scale_fill_brewer( palette = "Set2" ) +
  theme( legend.position = "none" ) + ggtitle( "Bar Chart of GAD-7 Clinical Cut-offs" ) +
  xlab( "Clinical Cut-off" ) + ylab( "Frequency" )

ggplot( df1, aes( x = as.factor( PHQ.clinical ), fill = as.factor( PHQ.clinical ) ) ) + 
  geom_bar( ) + geom_text( stat = "count", aes( label = ..count.., vjust = -.3 ) ) +
  scale_fill_brewer( palette = "Set2" ) +
  theme( legend.position = "none" ) + ggtitle( "Bar Chart of PHQ-9 Clinical Cut-offs" ) +
  xlab( "Clinical Cut-off" ) + ylab( "Frequency" )

### Are anxiety and depressive symptoms correlated?
cor.test( df1$GAD7.rtot, df1$PHQ9.rtot )
# Yes, strongly

# Outcome variables cleaned #
################################################################################


###################################
##          Predictors           ##
## Adverse Childhood Experiences ##
#   Perceived Negative Effect    ##
###################################

### Create cumulative ACE score
ACES <- df1 %>% select( ID, cohort, ACE1.1, ACE2.1, ACE3.1, ACE4.1, ACE5.1, 
                           ACE6.1, ACE7.1, ACE8.1, ACE9.1, ACE10.1, ACE11.1, 
                           ACE12.1, ACE13.1, ACE14.1, ACE15.1 )

### Create summary score
ACES$ACE.count <- rowSums( ACES[ ,3:17 ] )

### Describe ACE summary score
psych::describe( ACES$ACE.count )
table( ACES$ACE.count )

### Visualize data - full sample
ggplot( ACES, aes( x = ACE.count, fill = as.factor( ACE.count ) ) ) + geom_bar( ) +
  geom_text( stat = "count", aes( label = ..count.., vjust = -1 ) ) + 
  theme( legend.position = "none", panel.border = element_blank(), panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), axis.line = element_line( colour = "black" ), 
         panel.background = element_rect( fill = "transparent" ), 
         plot.background = element_rect( fill = "transparent", color = NA ) ) + 
  scale_x_continuous( name = "Number of Adverse Childhood Experiences", breaks = seq( 0,13,1 ) ) + 
  scale_y_continuous( name = "Frequency", expand = expansion( mult = c( 0, .1 ) ) )


### Facet for separate cohorts
ACES$cohort <- factor( ACES$cohort, levels = c( 1, 2 ), labels = c( "Fall", "Spring" ) )
ggplot( ACES, aes( x = ACE.count, fill = as.factor( ACE.count ) ) ) + geom_bar( ) +
  geom_text( stat = "count", aes( label = ..count.., vjust = -1 ) ) + 
  theme( legend.position = "none", panel.border = element_blank(), panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), axis.line = element_line( colour = "black" ), 
         panel.background = element_rect( fill = "transparent" ), 
         plot.background = element_rect( fill = "transparent", color = NA ) ) + 
  scale_x_continuous( name = "Number of Adverse Childhood Experiences", breaks = seq( 0,13,1 ) ) + 
  scale_y_continuous( name = "Frequency", expand = expansion( mult = c( 0, .1 ) ) ) + 
  facet_grid( . ~ cohort )

### Merge ACE data with full data set
ACES <- ACES[ ,c( 1, 18 ) ]
df2 <- merge( df1, ACES, by = "ID" )


### Look at perceptions data
ACE.percep <- df2 %>% select( ID, cohort, ACE1.5, ACE2.5, ACE3.5, ACE4.5, ACE5.5, 
                              ACE6.5, ACE7.5, ACE8.5, ACE9.5, ACE10.5, ACE11.5, 
                              ACE12.5, ACE13.5, ACE14.5, ACE15.5 )
names( ACE.percep ) <- c( "ID", "Cohort", "Alcohol", "Drugs", "PsychDx", 
                          "Suicide", "Death", "Jail", "FamViolence", "Divorce",
                          "SexAbuse", "PhysAbuse", "EmotAbuse", "Neglect",
                          "Bully", "Discrim", "CommViolence" )

### Visualize data
ACE.percep.long <- ACE.percep %>%
  pivot_longer( 3:17, names_to = "question", values_to = "response" )

library( plyr )
means.all <- ddply( ACE.percep.long, .( question ), summarise, 
                    response = mean( response, na.rm = T ) )

### Full sample
ACE.percep %>%
  pivot_longer( 3:17, names_to = "question", values_to = "response" ) %>%
  ggplot( aes( y = response, x = question ) ) +
  geom_boxplot() +
  labs( x = "Adverse Childhood Experience", y = "Perceived Negative Impact" ) +
  geom_text( data = means.all, aes( x = question, y = response, label = round( response, digits = 2 ) ), 
             size = 3, vjust = 0 )

### Fall
ACE.percep.fall <- ACE.percep[ which( ACE.percep$Cohort == 1 ), ]

ACE.percep.fall.long <- ACE.percep.fall %>%
  pivot_longer( 3:17, names_to = "question", values_to = "response" )

means.fall <- ddply( ACE.percep.fall.long, .( question ), summarise, 
                     response = mean( response, na.rm = T ) )

ACE.percep.fall %>%
  pivot_longer( 3:17, names_to = "question", values_to = "response" ) %>%
  ggplot( aes( y = response, x = question ) ) +
  geom_boxplot() +
  labs( x = "Adverse Childhood Experience", y = "Perceived Negative Impact" ) +
  geom_text( data = means.fall, aes( x = question, y = response, label = round( response, digits = 2 ) ), 
             size = 3, vjust = 0 )

### Spring
ACE.percep.spring <- ACE.percep[ which( ACE.percep$Cohort == 2 ), ]

ACE.percep.spring.long <- ACE.percep.spring %>%
  pivot_longer( 3:17, names_to = "question", values_to = "response" )

means.spring <- ddply( ACE.percep.spring.long, .( question ), summarise, 
                       response = mean( response, na.rm = T ) )

ACE.percep.spring %>%
  pivot_longer( 3:17, names_to = "question", values_to = "response" ) %>%
  ggplot( aes( y = response, x = question ) ) +
  geom_boxplot() +
  labs( x = "Adverse Childhood Experience", y = "Perceived Negative Impact" ) +
  geom_text( data = means.spring, aes( x = question, y = response, label = round( response, digits = 2 ) ), 
             size = 3, vjust = 0 )

### Create ACE perceptions score
df2$ACE.percep <- rowMeans( 
  df2[ ,c( 17,19,21,23,25,27,29,31,33,35,37,39,41,43,45 ) ], na.rm = T )
df2$ACE.percep <- ifelse( is.nan( df2$ACE.percep ), NA, df2$ACE.percep )
psych::describe( df2$ACE.percep )

### Visualize perception scores
df3 <- df2[ which( !is.na( df2$ACE.percep ) ), ] # remove NA for plotting
ggplot( df3, aes( x = ACE.percep ) ) + geom_histogram( color = "darkblue", fill = "lightblue" ) + 
  geom_vline( aes( xintercept = mean( ACE.percep ) ), color = "blue", linetype = "dashed", size = 1 )

### Facet by cohort
df3$cohort <- df3$cohort <- factor( df3$cohort, levels = c( 1, 2 ), labels = c( "Fall", "Spring" ) )
ggplot( df3, aes( x = ACE.percep ) ) + geom_histogram( color = "darkblue", fill = "lightblue" ) + 
  geom_vline( aes( xintercept = mean( ACE.percep ) ), color = "blue", linetype = "dashed", size = 1 ) +
  facet_grid( . ~ cohort )

### Clean up environment
rm( list = c( "means.all", "means.fall", "means.spring", "ACES", 
              "ACE.percep", "ACE.percep.fall", "ACE.percep.fall.long", 
              "ACE.percep.long", "ACE.percep.spring", "ACE.percep.spring.long", 
              "df1", "df3" ) )

# Predictor varialbes cleaned #
################################################################################

################
## Covariates ##
################

### Socioeconomic status (while growing up)
#In terms of finances, which of the following best describes you/your family's 
#situation prior to age 18? Would you say you/your family had:

#A lot more money than you need; A little more money than you need
#Just enough to meet your needs; Not enough to meet your needs
table( df2$ses1 )


#In terms of finances, which of the following best describes you/your family’s 
#situation prior to age 18 compared to other families?  Would you say you/your family was:

#Better off than most other families; About the same as most other families;
#Worse off than most other families
table( df2$ses2 ) 

df2$ses1.f <- factor( df2$ses1, levels = c( 1, 2, 3, 4 ), labels = c( "A lot more money than needed", 
                                                                    "A little more money than needed",
                                                                    "Just enough to meet needs",
                                                                    "Not enough to meet needs" ) )

df2$ses2.f <- factor( df2$ses2, levels = c( 1, 2, 3 ), labels = c( "Better off than most families", 
                                                                    "About the same as most families",
                                                                    "Worse off than most families" ) )

### Visualize data
ggplot( df2, aes( x = as.factor( ses1.f ), fill = as.factor( ses1.f ) ) ) + 
  geom_bar( ) + geom_text( stat = "count", aes( label = ..count.., vjust = -.3 ) ) +
  scale_fill_brewer( palette = "Set2" ) +
  theme( legend.position = "none" ) + ggtitle( "Family Finances When Growing Up" ) +
  xlab( "Response" ) + ylab( "Frequency" )

ggplot( df2, aes( x = as.factor( ses2.f ), fill = as.factor( ses2.f ) ) ) + 
  geom_bar( ) + geom_text( stat = "count", aes( label = ..count.., vjust = -.3 ) ) +
  scale_fill_brewer( palette = "Set2" ) +
  theme( legend.position = "none" ) + ggtitle( "Family Situation When Growing Up" ) +
  xlab( "Response" ) + ylab( "Frequency" )


### COVID items (available in spring cohort only)
#1. How much has the COVID-19 pandemic impacted your feelings of anxiety?
#2. How much has the COVID-19 pandemic impacted your feelings of depression?
#Response Options: Made it a lot better; Made it a little better; Didn't really change;
# Made it a little worse; Made it a lot worse

#Anxiety
table( df2$COVID5 )

#Depression
table( df2$COVID6 )

### Visualize responses
df2$COVID5.f <- factor( df2$COVID5, levels = c( 1, 2, 3, 4, 5 ), labels = c( "Made it a lot better", 
                                                                           "Made it a little better",
                                                                           "Didn't really change", 
                                                                           "Made it a little worse",
                                                                           "Made it a lot worse" ) )

df2$COVID6.f <- factor( df2$COVID6, levels = c( 1, 2, 3, 4, 5 ), labels = c( "Made it a lot better", 
                                                                           "Made it a little better",
                                                                           "Didn't really change", 
                                                                           "Made it a little worse",
                                                                           "Made it a lot worse" ) )
temp <- df2[ which( !is.na( df2$COVID5 ) ), ]

ggplot( temp, aes( x = as.factor( COVID5.f ), fill = as.factor( COVID5.f ) ) ) + 
  geom_bar( ) + geom_text( stat = "count", aes( label = ..count.., vjust = -.3 ) ) +
  scale_fill_brewer( palette = "Set2" ) +
  theme( legend.position = "none" ) + ggtitle( "COVID-19 Effects on Anxiety" ) +
  xlab( "Response" ) + ylab( "Frequency" )

ggplot( temp, aes( x = as.factor( COVID6.f ), fill = as.factor( COVID6.f ) ) ) + 
  geom_bar( ) + geom_text( stat = "count", aes( label = ..count.., vjust = -.3 ) ) +
  scale_fill_brewer( palette = "Set2" ) +
  theme( legend.position = "none" ) + ggtitle( "COVID-19 Effects on Depression" ) +
  xlab( "Response" ) + ylab( "Frequency" )

### Distributions are not as expected (i.e., very few reported worsening anxiety & depression)
### May not be able to use these variables as-is (i.e., may need to combine latter two responses)

### Clean up environment
rm( list = c( "df3", "temp" ) )

# Covariates cleaned #
################################################################################

##############################
## Extra data visualization ##
##############################

### Examine correlations among study variables
### Use revised scoring versions of PHQ9 and GAD7
library( Hmisc )

### Function for heat map
cors <- function( df ) { 
  M <- Hmisc::rcorr( as.matrix( df ) )
  Mdf <- map( M, ~data.frame( .x ) )
}

### Subset variables
corrs <- df2[ ,c( "ACE.count", "ACE.percep", "Age", "PHQ9.rtot", "GAD7.rtot",
                 "Race.b", "m0f1", "ses1", "ses2", "COVID5", "COVID6" ) ]
names( corrs ) <- c( "ACE_Score", "Perceived_Effect", "Age", "PHQ-9", "GAD-7", 
                     "Race", "Sex", "Family_Finances", "Family_Situation", 
                     "COVID_Anxiety", "COVID_Depression" )

### Generate Heatmap
cors( corrs ) %>%
  map( ~rownames_to_column( .x, var = "measure1" ) ) %>%
  # format each data set (r,P,n) long 
  map( ~pivot_longer( .x, -measure1, "measure2" ) ) %>%
  # merge our three list elements by binding the rows
  bind_rows( .id = "id" ) %>%
  pivot_wider( names_from = id, values_from = value ) %>%
  mutate( sig_p = ifelse( P <.05, T, F ), p_if_sig = ifelse( P <.05, P, NA ),
          r_if_sig = ifelse( P <.05, r, NA ) ) %>% 
  ggplot( aes( measure1, measure2, fill = r, label = round( r_if_sig, 2 ) ) ) +
  geom_tile() +
  labs( x = NULL, y = NULL, fill = "Pearson's\nCorrelation", 
        title = "Heatmap of Core Study Variables", 
        subtitle = "Only significant Pearson's correlation coefficients shown" ) + 
  scale_fill_gradient2( mid = "#FBFEF9", low = "#0C6291", high = "#A63446", 
                        limits = c( -1,1 ) ) +
  geom_text() +
  theme_classic() +
  scale_x_discrete( expand = c( 0,0 ) ) +
  scale_y_discrete( expand = c( 0,0 ) ) +
  theme( text = element_text( family = "Times", size = 18 ) ) +
  theme( axis.text.x = element_text( angle = -45, margin = margin( 0,0,0,0 ), hjust = .05, vjust = .5 ) )

### COVID anxiety and depression items, respectively, are negatively associated 
### with anxiety and depression. So, those with low anxiety & depression saw
### increases in these symptoms during COVID, but the opposite may not be true?
### Considering the distribution and availability of this item for only a portion
### of spring participants, will exclude from downstream analyses.

### Wrrite out file for analysis
write_csv( df2, file = "data_for_analysis.csv" )

