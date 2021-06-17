##############################################
# ACEs, Identity & Morality (AIM) Study
# ACEs data QC
# Authors: David W. Sosnowski & Casey Burton
# Email: dsosnow1@jhu.edu; burtoncd@vcu.edu
##############################################

getwd()

# load necessary packages
library( tidyverse )

# load spring data
df1 <- read_csv( "raw_data_spring.csv" )

# recode ACE y/n to 0 = No, 1 = Yes
df1$ACE1.1 <- ifelse( df1$ACE1.1 == 5, 1, 0 )
df1$ACE2.1 <- ifelse( df1$ACE2.1 == 5, 1, 0 )
df1$ACE3.1 <- ifelse( df1$ACE3.1 == 5, 1, 0 )
df1$ACE4.1 <- ifelse( df1$ACE4.1 == 5, 1, 0 )
df1$ACE5.1 <- ifelse( df1$ACE5.1 == 5, 1, 0 )
df1$ACE6.1 <- ifelse( df1$ACE6.1 == 5, 1, 0 )
df1$ACE7.1 <- ifelse( df1$ACE7.1 == 5, 1, 0 )
df1$ACE8.1 <- ifelse( df1$ACE8.1 == 5, 1, 0 )
df1$ACE9.1 <- ifelse( df1$ACE9.1 == 5, 1, 0 )
df1$ACE10.1 <- ifelse( df1$ACE10.1 == 5, 1, 0 )
df1$ACE11.1 <- ifelse( df1$ACE11.1 == 5, 1, 0 )
df1$ACE12.1 <- ifelse( df1$ACE12.1 == 23, 1, 0 )
df1$ACE13.1 <- ifelse( df1$ACE13.1 == 1, 1, 0 )
df1$ACE14.1 <- ifelse( df1$ACE14.1 == 1, 1, 0 )
df1$ACE15.1 <- ifelse( df1$ACE15.1 == 23, 1, 0 )


# recode frequency and duration data to numeric 
df1$ACE1.frq.2 <- ifelse( df1$ACE1.frq.2 == "day", 1, df1$ACE1.frq.2 )
df1$ACE1.frq.2 <- ifelse( df1$ACE1.frq.2 == "week", 7, df1$ACE1.frq.2 )
df1$ACE1.frq.2 <- ifelse( df1$ACE1.frq.2 == "month", 30.42, df1$ACE1.frq.2 ) 
df1$ACE1.frq.2 <- ifelse( df1$ACE1.frq.2 == "year", 365, df1$ACE1.frq.2 )

df1$ACE2.frq.2 <- ifelse( df1$ACE2.frq.2 == "day", 1, df1$ACE2.frq.2 )
df1$ACE2.frq.2 <- ifelse( df1$ACE2.frq.2 == "week", 7, df1$ACE2.frq.2 )
df1$ACE2.frq.2 <- ifelse( df1$ACE2.frq.2 == "month", 30.42, df1$ACE2.frq.2 ) 
df1$ACE2.frq.2 <- ifelse( df1$ACE2.frq.2 == "year", 365, df1$ACE2.frq.2 )

df1$ACE7.frq.2 <- ifelse( df1$ACE7.frq.2 == "day", 1, df1$ACE7.frq.2 )
df1$ACE7.frq.2 <- ifelse( df1$ACE7.frq.2 == "week", 7, df1$ACE7.frq.2 )
df1$ACE7.frq.2 <- ifelse( df1$ACE7.frq.2 == "month", 30.42, df1$ACE7.frq.2 ) 
df1$ACE7.frq.2 <- ifelse( df1$ACE7.frq.2 == "year", 365, df1$ACE7.frq.2 )

df1$ACE9.frq.2 <- ifelse( df1$ACE9.frq.2 == "day", 1, df1$ACE9.frq.2 )
df1$ACE9.frq.2 <- ifelse( df1$ACE9.frq.2 == "week", 7, df1$ACE9.frq.2 )
df1$ACE9.frq.2 <- ifelse( df1$ACE9.frq.2 == "month", 30.42, df1$ACE9.frq.2 ) 
df1$ACE9.frq.2 <- ifelse( df1$ACE9.frq.2 == "year", 365, df1$ACE9.frq.2 )

df1$ACE10.frq.2 <- ifelse( df1$ACE10.frq.2 == "day", 1, df1$ACE10.frq.2 )
df1$ACE10.frq.2 <- ifelse( df1$ACE10.frq.2 == "week", 7, df1$ACE10.frq.2 )
df1$ACE10.frq.2 <- ifelse( df1$ACE10.frq.2 == "month", 30.42, df1$ACE10.frq.2 ) 
df1$ACE10.frq.2 <- ifelse( df1$ACE10.frq.2 == "year", 365, df1$ACE10.frq.2 )

df1$ACE11.frq.2 <- ifelse( df1$ACE11.frq.2 == "day", 1, df1$ACE11.frq.2 )
df1$ACE11.frq.2 <- ifelse( df1$ACE11.frq.2 == "week", 7, df1$ACE11.frq.2 )
df1$ACE11.frq.2 <- ifelse( df1$ACE11.frq.2 == "month", 30.42, df1$ACE11.frq.2 ) 
df1$ACE11.frq.2 <- ifelse( df1$ACE11.frq.2 == "year", 365, df1$ACE11.frq.2 )

df1$ACE12.frq.2 <- ifelse( df1$ACE12.frq.2 == "day", 1, df1$ACE12.frq.2 )
df1$ACE12.frq.2 <- ifelse( df1$ACE12.frq.2 == "week", 7, df1$ACE12.frq.2 )
df1$ACE12.frq.2 <- ifelse( df1$ACE12.frq.2 == "month", 30.42, df1$ACE12.frq.2 ) 
df1$ACE12.frq.2 <- ifelse( df1$ACE12.frq.2 == "year", 365, df1$ACE12.frq.2 )

df1$ACE13.frq.2 <- ifelse( df1$ACE13.frq.2 == "day", 1, df1$ACE13.frq.2 )
df1$ACE13.frq.2 <- ifelse( df1$ACE13.frq.2 == "week", 7, df1$ACE13.frq.2 )
df1$ACE13.frq.2 <- ifelse( df1$ACE13.frq.2 == "month", 30.42, df1$ACE13.frq.2 ) 
df1$ACE13.frq.2 <- ifelse( df1$ACE13.frq.2 == "year", 365, df1$ACE13.frq.2 )

df1$ACE14.frq.2 <- ifelse( df1$ACE14.frq.2 == "day", 1, df1$ACE14.frq.2 )
df1$ACE14.frq.2 <- ifelse( df1$ACE14.frq.2 == "week", 7, df1$ACE14.frq.2 )
df1$ACE14.frq.2 <- ifelse( df1$ACE14.frq.2 == "month", 30.42, df1$ACE14.frq.2 ) 
df1$ACE14.frq.2 <- ifelse( df1$ACE14.frq.2 == "year", 365, df1$ACE14.frq.2 )

df1$ACE15.frq.2 <- ifelse( df1$ACE15.frq.2 == "day", 1, df1$ACE15.frq.2 )
df1$ACE15.frq.2 <- ifelse( df1$ACE15.frq.2 == "week", 7, df1$ACE15.frq.2 )
df1$ACE15.frq.2 <- ifelse( df1$ACE15.frq.2 == "month", 30.42, df1$ACE15.frq.2 ) 
df1$ACE15.frq.2 <- ifelse( df1$ACE15.frq.2 == "year", 365, df1$ACE15.frq.2 )


df1$ACE1.frq.4 <- ifelse( df1$ACE1.frq.4 == "days", 1, df1$ACE1.frq.4 )
df1$ACE1.frq.4 <- ifelse( df1$ACE1.frq.4 == "weeks", 7, df1$ACE1.frq.4 )
df1$ACE1.frq.4 <- ifelse( df1$ACE1.frq.4 == "months", 30.42, df1$ACE1.frq.4 ) 
df1$ACE1.frq.4 <- ifelse( df1$ACE1.frq.4 == "years", 365, df1$ACE1.frq.4 )

df1$ACE2.frq.4 <- ifelse( df1$ACE2.frq.4 == "days", 1, df1$ACE2.frq.4 )
df1$ACE2.frq.4 <- ifelse( df1$ACE2.frq.4 == "weeks", 7, df1$ACE2.frq.4 )
df1$ACE2.frq.4 <- ifelse( df1$ACE2.frq.4 == "months", 30.42, df1$ACE2.frq.4 ) 
df1$ACE2.frq.4 <- ifelse( df1$ACE2.frq.4 == "years", 365, df1$ACE2.frq.4 )

df1$ACE7.frq.4 <- ifelse( df1$ACE7.frq.4 == "days", 1, df1$ACE7.frq.4 )
df1$ACE7.frq.4 <- ifelse( df1$ACE7.frq.4 == "weeks", 7, df1$ACE7.frq.4 )
df1$ACE7.frq.4 <- ifelse( df1$ACE7.frq.4 == "months", 30.42, df1$ACE7.frq.4 ) 
df1$ACE7.frq.4 <- ifelse( df1$ACE7.frq.4 == "years", 365, df1$ACE7.frq.4 )

df1$ACE9.frq.4 <- ifelse( df1$ACE9.frq.4 == "days", 1, df1$ACE9.frq.4 )
df1$ACE9.frq.4 <- ifelse( df1$ACE9.frq.4 == "weeks", 7, df1$ACE9.frq.4 )
df1$ACE9.frq.4 <- ifelse( df1$ACE9.frq.4 == "months", 30.42, df1$ACE9.frq.4 ) 
df1$ACE9.frq.4 <- ifelse( df1$ACE9.frq.4 == "years", 365, df1$ACE9.frq.4 )

df1$ACE10.frq.4 <- ifelse( df1$ACE10.frq.4 == "days", 1, df1$ACE10.frq.4 )
df1$ACE10.frq.4 <- ifelse( df1$ACE10.frq.4 == "weeks", 7, df1$ACE10.frq.4 )
df1$ACE10.frq.4 <- ifelse( df1$ACE10.frq.4 == "months", 30.42, df1$ACE10.frq.4 ) 
df1$ACE10.frq.4 <- ifelse( df1$ACE10.frq.4 == "years", 365, df1$ACE10.frq.4 )

df1$ACE11.frq.4 <- ifelse( df1$ACE11.frq.4 == "days", 1, df1$ACE11.frq.4 )
df1$ACE11.frq.4 <- ifelse( df1$ACE11.frq.4 == "weeks", 7, df1$ACE11.frq.4 )
df1$ACE11.frq.4 <- ifelse( df1$ACE11.frq.4 == "months", 30.42, df1$ACE11.frq.4 ) 
df1$ACE11.frq.4 <- ifelse( df1$ACE11.frq.4 == "years", 365, df1$ACE11.frq.4 )

df1$ACE12.frq.4 <- ifelse( df1$ACE12.frq.4 == "days", 1, df1$ACE12.frq.4 )
df1$ACE12.frq.4 <- ifelse( df1$ACE12.frq.4 == "weeks", 7, df1$ACE12.frq.4 )
df1$ACE12.frq.4 <- ifelse( df1$ACE12.frq.4 == "months", 30.42, df1$ACE12.frq.4 ) 
df1$ACE12.frq.4 <- ifelse( df1$ACE12.frq.4 == "years", 365, df1$ACE12.frq.4 )

df1$ACE13.frq.4 <- ifelse( df1$ACE13.frq.4 == "days", 1, df1$ACE13.frq.4 )
df1$ACE13.frq.4 <- ifelse( df1$ACE13.frq.4 == "weeks", 7, df1$ACE13.frq.4 )
df1$ACE13.frq.4 <- ifelse( df1$ACE13.frq.4 == "months", 30.42, df1$ACE13.frq.4 ) 
df1$ACE13.frq.4 <- ifelse( df1$ACE13.frq.4 == "years", 365, df1$ACE13.frq.4 )

df1$ACE14.frq.4 <- ifelse( df1$ACE14.frq.4 == "days", 1, df1$ACE14.frq.4 )
df1$ACE14.frq.4 <- ifelse( df1$ACE14.frq.4 == "weeks", 7, df1$ACE14.frq.4 )
df1$ACE14.frq.4 <- ifelse( df1$ACE14.frq.4 == "months", 30.42, df1$ACE14.frq.4 ) 
df1$ACE14.frq.4 <- ifelse( df1$ACE14.frq.4 == "years", 365, df1$ACE14.frq.4 )

df1$ACE15.frq.4 <- ifelse( df1$ACE15.frq.4 == "days", 1, df1$ACE15.frq.4 )
df1$ACE15.frq.4 <- ifelse( df1$ACE15.frq.4 == "weeks", 7, df1$ACE15.frq.4 )
df1$ACE15.frq.4 <- ifelse( df1$ACE15.frq.4 == "months", 30.42, df1$ACE15.frq.4 ) 
df1$ACE15.frq.4 <- ifelse( df1$ACE15.frq.4 == "years", 365, df1$ACE15.frq.4 )

# convert variables to numeric
df1 <- as.data.frame( df1 )
df1$ACE1.frq.2 <- as.numeric( df1$ACE1.frq.2 )
df1$ACE2.frq.2 <- as.numeric( df1$ACE2.frq.2 )
df1$ACE7.frq.2 <- as.numeric( df1$ACE7.frq.2 )
df1$ACE9.frq.2 <- as.numeric( df1$ACE9.frq.2 )
df1$ACE10.frq.2 <- as.numeric( df1$ACE10.frq.2 )
df1$ACE11.frq.2 <- as.numeric( df1$ACE11.frq.2 )
df1$ACE12.frq.2 <- as.numeric( df1$ACE12.frq.2 )
df1$ACE13.frq.2 <- as.numeric( df1$ACE13.frq.2 )
df1$ACE14.frq.2 <- as.numeric( df1$ACE14.frq.2 )
df1$ACE15.frq.2 <- as.numeric( df1$ACE15.frq.2 )

df1$ACE1.frq.4 <- as.numeric( df1$ACE1.frq.4 )
df1$ACE2.frq.4 <- as.numeric( df1$ACE2.frq.4 )
df1$ACE7.frq.4 <- as.numeric( df1$ACE7.frq.4 )
df1$ACE9.frq.4 <- as.numeric( df1$ACE9.frq.4 )
df1$ACE10.frq.4 <- as.numeric( df1$ACE10.frq.4 )
df1$ACE11.frq.4 <- as.numeric( df1$ACE11.frq.4 )
df1$ACE12.frq.4 <- as.numeric( df1$ACE12.frq.4 )
df1$ACE13.frq.4 <- as.numeric( df1$ACE13.frq.4 )
df1$ACE14.frq.4 <- as.numeric( df1$ACE14.frq.4 )
df1$ACE15.frq.4 <- as.numeric( df1$ACE15.frq.4 )

# create frequency x duration score, weighted by multiple daily occurrences
df1$ACE1.fxd.w <- ( ( df1$ACE1.frq.4 / df1$ACE1.frq.2 )*df1$ACE1.frq.3 )*df1$ACE1.frq.1
df1$ACE2.fxd.w <- ( ( df1$ACE2.frq.4 / df1$ACE2.frq.2 )*df1$ACE2.frq.3 )*df1$ACE2.frq.1
df1$ACE7.fxd.w <- ( ( df1$ACE7.frq.4 / df1$ACE7.frq.2 )*df1$ACE7.frq.3 )*df1$ACE7.frq.1
df1$ACE9.fxd.w <- ( ( df1$ACE9.frq.4 / df1$ACE9.frq.2 )*df1$ACE9.frq.3 )*df1$ACE9.frq.1
df1$ACE10.fxd.w <- ( ( df1$ACE10.frq.4 / df1$ACE10.frq.2 )*df1$ACE10.frq.3 )*df1$ACE10.frq.1
df1$ACE11.fxd.w <- ( ( df1$ACE11.frq.4 / df1$ACE11.frq.2 )*df1$ACE11.frq.3 )*df1$ACE11.frq.1
df1$ACE12.fxd.w <- ( ( df1$ACE12.frq.4 / df1$ACE12.frq.2 )*df1$ACE12.frq.3 )*df1$ACE12.frq.1
df1$ACE13.fxd.w <- ( ( df1$ACE13.frq.4 / df1$ACE13.frq.2 )*df1$ACE13.frq.3 )*df1$ACE13.frq.1
df1$ACE14.fxd.w <- ( ( df1$ACE14.frq.4 / df1$ACE14.frq.2 )*df1$ACE14.frq.3 )*df1$ACE14.frq.1
df1$ACE15.fxd.w <- ( ( df1$ACE15.frq.4 / df1$ACE15.frq.2 )*df1$ACE15.frq.3 )*df1$ACE15.frq.1

# create frequency x duration score, not weighted by multiple daily occurrences
df1$ACE1.fxd <- df1$ACE1.fxd.w 
df1$ACE1.fxd <- ifelse( df1$ACE1.frq.2 == 1, 
                        ( ( df1$ACE1.frq.4 / df1$ACE1.frq.2 )*df1$ACE1.frq.3 ), 
                        df1$ACE1.fxd )

df1$ACE2.fxd <- df1$ACE2.fxd.w 
df1$ACE2.fxd <- ifelse( df1$ACE2.frq.2 == 1, 
                        ( ( df1$ACE2.frq.4 / df1$ACE2.frq.2 )*df1$ACE2.frq.3 ), 
                        df1$ACE2.fxd )

df1$ACE7.fxd <- df1$ACE7.fxd.w 
df1$ACE7.fxd <- ifelse( df1$ACE7.frq.2 == 1, 
                        ( ( df1$ACE7.frq.4 / df1$ACE7.frq.2 )*df1$ACE7.frq.3 ), 
                        df1$ACE7.fxd )

df1$ACE9.fxd <- df1$ACE9.fxd.w 
df1$ACE9.fxd <- ifelse( df1$ACE9.frq.2 == 1, 
                        ( ( df1$ACE9.frq.4 / df1$ACE9.frq.2 )*df1$ACE9.frq.3 ), 
                        df1$ACE9.fxd )

df1$ACE10.fxd <- df1$ACE10.fxd.w 
df1$ACE10.fxd <- ifelse( df1$ACE10.frq.2 == 1, 
                        ( ( df1$ACE10.frq.4 / df1$ACE10.frq.2 )*df1$ACE10.frq.3 ), 
                        df1$ACE10.fxd )

df1$ACE11.fxd <- df1$ACE11.fxd.w 
df1$ACE11.fxd <- ifelse( df1$ACE11.frq.2 == 1, 
                        ( ( df1$ACE11.frq.4 / df1$ACE11.frq.2 )*df1$ACE11.frq.3 ), 
                        df1$ACE11.fxd )

df1$ACE12.fxd <- df1$ACE12.fxd.w 
df1$ACE12.fxd <- ifelse( df1$ACE12.frq.2 == 1, 
                        ( ( df1$ACE12.frq.4 / df1$ACE12.frq.2 )*df1$ACE12.frq.3 ), 
                        df1$ACE12.fxd )

df1$ACE13.fxd <- df1$ACE13.fxd.w 
df1$ACE13.fxd <- ifelse( df1$ACE13.frq.2 == 1, 
                        ( ( df1$ACE13.frq.4 / df1$ACE13.frq.2 )*df1$ACE13.frq.3 ), 
                        df1$ACE13.fxd )

df1$ACE14.fxd <- df1$ACE14.fxd.w 
df1$ACE14.fxd <- ifelse( df1$ACE14.frq.2 == 1, 
                        ( ( df1$ACE14.frq.4 / df1$ACE14.frq.2 )*df1$ACE14.frq.3 ), 
                        df1$ACE14.fxd )

df1$ACE15.fxd <- df1$ACE15.fxd.w 
df1$ACE15.fxd <- ifelse( df1$ACE15.frq.2 == 1, 
                        ( ( df1$ACE15.frq.4 / df1$ACE15.frq.2 )*df1$ACE15.frq.3 ), 
                        df1$ACE15.fxd )


### write out file for sharing
write_csv( df1, file = "spring_2021_aces_cleaned.csv" )





