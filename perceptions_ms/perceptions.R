#######################################
# AIM Study
# Sum Scores vs. Perception ms
# David W. Sosnowski, PhD
#####################################

getwd()

# load necessary packages
library( tidyverse )

# load data
fall <- read_csv( "/Users/david/Desktop/aim_study/raw_data/raw_data_fall.csv" )
spring <- read_csv( "/Users/david/Desktop/aim_study/raw_data/raw_data_spring.csv" )

### prep fall data for merging
# subset necessary variables from data files
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

# recode data
fall2$m0f1 <- ifelse( fall2$Sex == 2, 0, fall2$Sex )
table( fall2$m0f1, exclude = NULL )
#  0 (male)   1 (female)
#    65           196 

fall2$Race.b <- ifelse( fall2$Race7 == 1 & is.na( fall2$Race1 ) & is.na( fall2$Race2 ) & is.na( fall2$Race3 ) & 
                           is.na( fall2$Race4 ) & is.na( fall2$Race5 ) & is.na( fall2$Race6 ) & is.na( fall2$Race8 ), 1, 0 )
table( fall2$Race.b, exclude = NULL )
#  0 (other)   1 (white)
#     174         87 

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

# create cohort variable for post-merge
fall2$cohort <- 1


### prep spring data for merging
# subset necessary variables from data files
names( spring )
spring2 <- spring %>% select( ID, Duration.M, Age, Sex, Race1, Race2, Race3, Race4, 
                          Race5, Race6, Race7, Race8, Race_Other, ses1, ses2, 
                          ACE1.1, ACE1.5, ACE2.1, ACE2.5, ACE3.1, ACE3.5, 
                          ACE4.1, ACE4.5, ACE5.1, ACE5.5, ACE6.1, ACE6.5, 
                          ACE7.1, ACE7.5, ACE8.1, ACE8.5, ACE9.1, ACE9.5, 
                          ACE10.1, ACE10.5, ACE11.1, ACE11.5, ACE12.1, ACE12.5,
                          ACE13.1, ACE13.5, ACE14.1, ACE14.5, ACE15.1, ACE15.5,
                          GAD1, GAD2, GAD3, GAD4, GAD5, GAD6, GAD7, PHQ1, PHQ2,
                          PHQ3, PHQ4, PHQ5, PHQ6, PHQ7, PHQ8, PHQ9 )

# recode data
spring2$m0f1 <- ifelse( spring2$Sex == 2, 0, spring2$Sex )
table( spring2$m0f1, exclude = NULL )
#  0 (male)   1 (female)
#    85           279 

spring2$Race.b <- ifelse( spring2$Race7 == 1 & is.na( spring2$Race1 ) & is.na( spring2$Race2 ) & is.na( spring2$Race3 ) & 
                          is.na( spring2$Race4 ) & is.na( spring2$Race5 ) & is.na( spring2$Race6 ) & is.na( spring2$Race8 ), 1, 0 )
table( spring2$Race.b, exclude = NULL )
#  0 (other)   1 (white)
#     257         107 

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
spring2$ACE15.1 <- ifelse( spring2$ACE15.1 == 23, 1, 0 )

# create cohort variable for post-merge
spring2$cohort <- 2

# check if column names match
stopifnot( identical( colnames( fall2 ), colnames( spring2 ) ) )

# merge tibbles
fall.spring <- bind_rows( fall2[ 1:261, 1:64 ], spring2[ 1:364, 1:64 ] )

# create GAD-7 total scores
fall.spring$GAD7tot <- fall.spring$GAD1 + fall.spring$GAD2 + fall.spring$GAD3 +
  fall.spring$GAD4 + fall.spring$GAD5 + fall.spring$GAD6 + fall.spring$GAD7

# check reliability
GAD <- fall.spring[ ,c( 46:52 ) ]
psych::alpha( GAD ) # .89

# update GAD scoring
# should be 4-point Likert (not at all, several days, more than half the days, every day)
# we have a 6-point Likert (Never, Almost never, Once in a while, Some days, Most days, Every day)
fall.spring$GAD1.r <- ifelse( fall.spring$GAD1 < 3, 0, fall.spring$GAD1 )
fall.spring$GAD1.r <- ifelse( fall.spring$GAD1 == 3 | fall.spring$GAD1 == 4, 1, fall.spring$GAD1.r )
fall.spring$GAD1.r <- ifelse( fall.spring$GAD1 == 5, 2, fall.spring$GAD1.r )
fall.spring$GAD1.r <- ifelse( fall.spring$GAD1 == 6, 3, fall.spring$GAD1.r )

fall.spring$GAD2.r <- ifelse( fall.spring$GAD2 < 3, 0, fall.spring$GAD2 )
fall.spring$GAD2.r <- ifelse( fall.spring$GAD2 == 3 | fall.spring$GAD2 == 4, 1, fall.spring$GAD2.r )
fall.spring$GAD2.r <- ifelse( fall.spring$GAD2 == 5, 2, fall.spring$GAD2.r )
fall.spring$GAD2.r <- ifelse( fall.spring$GAD2 == 6, 3, fall.spring$GAD2.r )

fall.spring$GAD3.r <- ifelse( fall.spring$GAD3 < 3, 0, fall.spring$GAD3 )
fall.spring$GAD3.r <- ifelse( fall.spring$GAD3 == 3 | fall.spring$GAD3 == 4, 1, fall.spring$GAD3.r )
fall.spring$GAD3.r <- ifelse( fall.spring$GAD3 == 5, 2, fall.spring$GAD3.r )
fall.spring$GAD3.r <- ifelse( fall.spring$GAD3 == 6, 3, fall.spring$GAD3.r )

fall.spring$GAD4.r <- ifelse( fall.spring$GAD4 < 3, 0, fall.spring$GAD4 )
fall.spring$GAD4.r <- ifelse( fall.spring$GAD4 == 3 | fall.spring$GAD4 == 4, 1, fall.spring$GAD4.r )
fall.spring$GAD4.r <- ifelse( fall.spring$GAD4 == 5, 2, fall.spring$GAD4.r )
fall.spring$GAD4.r <- ifelse( fall.spring$GAD4 == 6, 3, fall.spring$GAD4.r )

fall.spring$GAD5.r <- ifelse( fall.spring$GAD5 < 3, 0, fall.spring$GAD5 )
fall.spring$GAD5.r <- ifelse( fall.spring$GAD5 == 3 | fall.spring$GAD5 == 4, 1, fall.spring$GAD5.r )
fall.spring$GAD5.r <- ifelse( fall.spring$GAD5 == 5, 2, fall.spring$GAD5.r )
fall.spring$GAD5.r <- ifelse( fall.spring$GAD5 == 6, 3, fall.spring$GAD5.r )

fall.spring$GAD6.r <- ifelse( fall.spring$GAD6 < 3, 0, fall.spring$GAD6 )
fall.spring$GAD6.r <- ifelse( fall.spring$GAD6 == 3 | fall.spring$GAD6 == 4, 1, fall.spring$GAD6.r )
fall.spring$GAD6.r <- ifelse( fall.spring$GAD6 == 5, 2, fall.spring$GAD6.r )
fall.spring$GAD6.r <- ifelse( fall.spring$GAD6 == 6, 3, fall.spring$GAD6.r )

fall.spring$GAD7.r <- ifelse( fall.spring$GAD7 < 3, 0, fall.spring$GAD7 )
fall.spring$GAD7.r <- ifelse( fall.spring$GAD7 == 3 | fall.spring$GAD7 == 4, 1, fall.spring$GAD7.r )
fall.spring$GAD7.r <- ifelse( fall.spring$GAD7 == 5, 2, fall.spring$GAD7.r )
fall.spring$GAD7.r <- ifelse( fall.spring$GAD7 == 6, 3, fall.spring$GAD7.r )


# create revised GAD-7 total scores
fall.spring$GAD7.rtot <- fall.spring$GAD1.r + fall.spring$GAD2.r + fall.spring$GAD3.r +
  fall.spring$GAD4.r + fall.spring$GAD5.r + fall.spring$GAD6.r + fall.spring$GAD7.r

# check reliability
GAD.r <- fall.spring[ ,c( 66:72 ) ]
psych::alpha( GAD.r ) # .87

# create clinical cut-offs
fall.spring$GAD.clinical <- cut(
  fall.spring$GAD7.rtot,
  breaks = c( 0, 5, 10, 15, Inf ),
  labels = c( "minimal", "mild", "moderate", "severe" ),
  right  = FALSE
)

table( fall.spring$GAD.clinical )
prop.table( table( fall.spring$GAD.clinical ) )*100


# create PHQ-9 total scores
fall.spring$PHQtot <- fall.spring$PHQ1 + fall.spring$PHQ2 + fall.spring$PHQ3 +
  fall.spring$PHQ4 + fall.spring$PHQ5 + fall.spring$PHQ6 + fall.spring$PHQ7 + 
  fall.spring$PHQ8 + fall.spring$PHQ9

# check reliability
PHQ <- fall.spring[ ,c( 53:61 ) ]
psych::alpha( PHQ ) # .90


# update PHQ scoring
# should be 4-point Likert (not at all, several days, more than half the days, every day)
# we have a 6-point Likert (Never, Almost never, Once in a while, Some days, Most days, Every day)
fall.spring$PHQ1.r <- ifelse( fall.spring$PHQ1 < 3, 0, fall.spring$PHQ1 )
fall.spring$PHQ1.r <- ifelse( fall.spring$PHQ1 == 3 | fall.spring$PHQ1 == 4, 1, fall.spring$PHQ1.r )
fall.spring$PHQ1.r <- ifelse( fall.spring$PHQ1 == 5, 2, fall.spring$PHQ1.r )
fall.spring$PHQ1.r <- ifelse( fall.spring$PHQ1 == 6, 3, fall.spring$PHQ1.r )

fall.spring$PHQ2.r <- ifelse( fall.spring$PHQ2 < 3, 0, fall.spring$PHQ2 )
fall.spring$PHQ2.r <- ifelse( fall.spring$PHQ2 == 3 | fall.spring$PHQ2 == 4, 1, fall.spring$PHQ2.r )
fall.spring$PHQ2.r <- ifelse( fall.spring$PHQ2 == 5, 2, fall.spring$PHQ2.r )
fall.spring$PHQ2.r <- ifelse( fall.spring$PHQ2 == 6, 3, fall.spring$PHQ2.r )

fall.spring$PHQ3.r <- ifelse( fall.spring$PHQ3 < 3, 0, fall.spring$PHQ3 )
fall.spring$PHQ3.r <- ifelse( fall.spring$PHQ3 == 3 | fall.spring$PHQ3 == 4, 1, fall.spring$PHQ3.r )
fall.spring$PHQ3.r <- ifelse( fall.spring$PHQ3 == 5, 2, fall.spring$PHQ3.r )
fall.spring$PHQ3.r <- ifelse( fall.spring$PHQ3 == 6, 3, fall.spring$PHQ3.r )

fall.spring$PHQ4.r <- ifelse( fall.spring$PHQ4 < 3, 0, fall.spring$PHQ4 )
fall.spring$PHQ4.r <- ifelse( fall.spring$PHQ4 == 3 | fall.spring$PHQ4 == 4, 1, fall.spring$PHQ4.r )
fall.spring$PHQ4.r <- ifelse( fall.spring$PHQ4 == 5, 2, fall.spring$PHQ4.r )
fall.spring$PHQ4.r <- ifelse( fall.spring$PHQ4 == 6, 3, fall.spring$PHQ4.r )

fall.spring$PHQ5.r <- ifelse( fall.spring$PHQ5 < 3, 0, fall.spring$PHQ5 )
fall.spring$PHQ5.r <- ifelse( fall.spring$PHQ5 == 3 | fall.spring$PHQ5 == 4, 1, fall.spring$PHQ5.r )
fall.spring$PHQ5.r <- ifelse( fall.spring$PHQ5 == 5, 2, fall.spring$PHQ5.r )
fall.spring$PHQ5.r <- ifelse( fall.spring$PHQ5 == 6, 3, fall.spring$PHQ5.r )

fall.spring$PHQ6.r <- ifelse( fall.spring$PHQ6 < 3, 0, fall.spring$PHQ6 )
fall.spring$PHQ6.r <- ifelse( fall.spring$PHQ6 == 3 | fall.spring$PHQ6 == 4, 1, fall.spring$PHQ6.r )
fall.spring$PHQ6.r <- ifelse( fall.spring$PHQ6 == 5, 2, fall.spring$PHQ6.r )
fall.spring$PHQ6.r <- ifelse( fall.spring$PHQ6 == 6, 3, fall.spring$PHQ6.r )

fall.spring$PHQ7.r <- ifelse( fall.spring$PHQ7 < 3, 0, fall.spring$PHQ7 )
fall.spring$PHQ7.r <- ifelse( fall.spring$PHQ7 == 3 | fall.spring$PHQ7 == 4, 1, fall.spring$PHQ7.r )
fall.spring$PHQ7.r <- ifelse( fall.spring$PHQ7 == 5, 2, fall.spring$PHQ7.r )
fall.spring$PHQ7.r <- ifelse( fall.spring$PHQ7 == 6, 3, fall.spring$PHQ7.r )

fall.spring$PHQ8.r <- ifelse( fall.spring$PHQ8 < 3, 0, fall.spring$PHQ8 )
fall.spring$PHQ8.r <- ifelse( fall.spring$PHQ8 == 3 | fall.spring$PHQ8 == 4, 1, fall.spring$PHQ8.r )
fall.spring$PHQ8.r <- ifelse( fall.spring$PHQ8 == 5, 2, fall.spring$PHQ8.r )
fall.spring$PHQ8.r <- ifelse( fall.spring$PHQ8 == 6, 3, fall.spring$PHQ8.r )

fall.spring$PHQ9.r <- ifelse( fall.spring$PHQ9 < 3, 0, fall.spring$PHQ9 )
fall.spring$PHQ9.r <- ifelse( fall.spring$PHQ9 == 3 | fall.spring$PHQ9 == 4, 1, fall.spring$PHQ9.r )
fall.spring$PHQ9.r <- ifelse( fall.spring$PHQ9 == 5, 2, fall.spring$PHQ9.r )
fall.spring$PHQ9.r <- ifelse( fall.spring$PHQ9 == 6, 3, fall.spring$PHQ9.r )


# create revised PHQ-9 total scores
fall.spring$PHQ9.rtot <- fall.spring$PHQ1.r + fall.spring$PHQ2.r + fall.spring$PHQ3.r +
  fall.spring$PHQ4.r + fall.spring$PHQ5.r + fall.spring$PHQ6.r + fall.spring$PHQ7.r +
  fall.spring$PHQ8.r + fall.spring$PHQ9.r

# check reliability
PHQ.r <- fall.spring[ ,c( 76:84 ) ]
psych::alpha( PHQ.r ) # .89

# create clinical cut-offs
fall.spring$PHQ.clinical <- cut(
  fall.spring$PHQ9.rtot,
  breaks = c( 0, 5, 10, 15, Inf ),
  labels = c( "minimal", "mild", "moderate", "severe" ),
  right  = FALSE
)

table( fall.spring$PHQ.clinical )
prop.table( table( fall.spring$PHQ.clinical ) )*100


# write out merged and cleaned file
write_csv( fall.spring, file = "perceptions_fall_spring.csv" )

################################################################################
#####################
## cumulative ACEs ##
#####################

# clean up environment
df1 <- fall.spring
rm( list = c( "fall", "fall.spring", "fall2", "GAD", "GAD.r", "PHQ", "PHQ.r",
              "spring", "spring2" ) )

# subset ACE data
ACE.sum <- df1[ ,c( 1,64,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44 ) ]

# create summary score
ACE.sum$ACE.count <- rowSums( ACE.sum[ ,3:17 ] )

# describe ACE summary score
describe( ACE.sum$ACE.count )
table( ACE.sum$ACE.count )

# plot data - full sample
p <- ggplot( ACE.sum, aes( x = ACE.count, fill = as.factor( ACE.count ) ) ) + geom_bar( ) +
  geom_text( stat = "count", aes( label = ..count.., vjust = -1 ) ) + 
  theme( legend.position = "none", panel.border = element_blank(), panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), axis.line = element_line( colour = "black" ), 
         panel.background = element_rect( fill = "transparent" ), 
         plot.background = element_rect( fill = "transparent", color = NA ) ) + 
  scale_x_continuous( name = "Number of Adverse Childhood Experiences", breaks = seq( 0,11,1 ) ) + 
  scale_y_continuous( name = "Frequency", expand = expansion( mult = c( 0, .1 ) ) )
p
ggsave( p, filename = "ACE_Frequency_All.png", bg = "transparent" )


# facet for separate cohorts
ACE.sum$cohort <- factor( ACE.sum$cohort, levels = c( 1, 2 ), labels = c( "Fall", "Spring" ) )
p <- ggplot( ACE.sum, aes( x = ACE.count, fill = as.factor( ACE.count ) ) ) + geom_bar( ) +
  geom_text( stat = "count", aes( label = ..count.., vjust = -1 ) ) + 
  theme( legend.position = "none", panel.border = element_blank(), panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), axis.line = element_line( colour = "black" ), 
         panel.background = element_rect( fill = "transparent" ), 
         plot.background = element_rect( fill = "transparent", color = NA ) ) + 
  scale_x_continuous( name = "Number of Adverse Childhood Experiences", breaks = seq( 0,11,1 ) ) + 
  scale_y_continuous( name = "Frequency", expand = expansion( mult = c( 0, .1 ) ) )
p2 <- p + facet_grid( . ~ cohort )
ggsave( p2, filename = "ACE_Frequency_by_Cohort.png", bg = "transparent" )



# merge with full data set
ACE.sum <- ACE.sum[ ,c( 1, 18 ) ]
df2 <- merge( df1, ACE.sum, by = "ID" )

# look at perceptions data
ACE.percep <- df1[, c( 1,64,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45 ) ]
names( ACE.percep ) <- c( "ID", "Cohort", "Alcohol", "Drugs", "Psych Dx", 
                          "Suicide", "Death", "Jail", "Fam Violence", "Divorce",
                          "Sex Abuse", "Phys Abuse", "Emot Abuse", "Neglect",
                          "Bully", "Discrimination", "Comm Violence" )
 
# plot data
ACE.percep.long <- ACE.percep %>%
  pivot_longer( 3:17, names_to = "question", values_to = "response" )

library( plyr )
means.all <- ddply( ACE.percep.long, .( question ), summarise, 
                response = mean( response, na.rm = T ) )

# full sample
ACE.percep %>%
  pivot_longer( 3:17, names_to = "question", values_to = "response" ) %>%
  ggplot( aes( y = response, x = question ) ) +
  geom_boxplot() +
  labs( x = "Adverse Childhood Experience", y = "Perceived Negative Impact" ) +
  geom_text( data = means.all, aes( x = question, y = response, label = round( response, digits = 2 ) ), 
             size = 3, vjust = 0 )

# fall
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

# spring
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
