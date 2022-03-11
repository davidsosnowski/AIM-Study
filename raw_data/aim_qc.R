########################################
# ACEs, Identity & Morality (AIM) Study
# Cleaning raw data for sharing
# Author: David W. Sosnowski
# Email: dsosnow1@jhu.edu
########################################

getwd()

### Load necessary packages
library( haven )
library( psych )
library( tidyverse )

### Import data
fall <- read_sav( "AIM_Data_Fall.sav" )
spring <- read_sav( "AIM_Data_Spring.sav" )



                          #######################
                          ### QC of Fall data ###
                          #######################

### Remove participants who did not consent to the study
fall <- fall[ which( fall$Q1.1 == 1 ), ] # 3 participants removed

### Remove variables containing identifiable or unnecessary information
fall <- fall %>% select( -StartDate, -EndDate, -Status, -IPAddress, -RecordedDate,
                            -RecipientLastName, -RecipientFirstName, -RecipientEmail,
                            -ExternalReference, -LocationLatitude, -LocationLongitude,
                            -DistributionChannel, -UserLanguage, -Q1.1, -Q19.1, -Q19.2,
                            -Create_New_Field_or_Choose_From_Dropdown..., -id,
                            -Q2.34___Parent_Topics, -Q2.34___Topics )


### Create variable names
names( fall ) <- c( "Progress", "Duration", "Finished", "ID", "Age", "Sex", 
                       "Gender", "Gender_Other", "Feminimity", "Masculinity", 
                       "SexOrient", "SexOrient_Other", "Race1", "Race2", "Race3", 
                       "Race4", "Race5", "Race6", "Race7", "Race8", "Race_Other", 
                       "PCG", "PCG_Other", "PGC_EDU", "PGC_EDU_Other", "ses1", "ses2", 
                       "Dem", "Repub", "Ind", "Liber", "Soc", "PolAffil", 
                       "PolAffil_Other", "Religion", "Religion_Othr", 
                       "ACE1.1", "ACE1.2", "ACE1.3", "ACE1.4", "ACE1.5", 
                       "ACE2.1", "ACE2.2", "ACE2.3", "ACE2.4", "ACE2.5",
                       "ACE3.1", "ACE3.2", "ACE3.3", "ACE3.4", "ACE3.5", 
                       "ACE4.1", "ACE4.2", "ACE4.3", "ACE4.4", "ACE4.5", 
                       "ACE5.1", "ACE5.2", "ACE5.3", "ACE5_COD", "ACE5.4", "ACE5.5", 
                       "ACE6.1", "ACE6.2", "ACE6.3", "ACE6.4", "ACE6.5", 
                       "ACE7.1", "ACE7.4", "ACE7.5", 
                       "ACE8.1", "ACE8.4", "ACE8.5", 
                       "ACE9.1", "ACE9.2", "ACE9.3", "ACE9.4", "ACE9.4.1", "ACE9.5", 
                       "ACE10.1", "ACE10.2", "ACE10.3", "ACE10.4", "ACE10.4.1", "ACE10.5",
                       "ACE11.1", "ACE11.2", "ACE11.3", "ACE11.4", "ACE11.4.1", "ACE11.5",
                       "ACE12.1", "ACE12.5",
                       "ACE13.1", "ACE13.4.1", "ACE13.5",
                       "ACE14.1", "ACE14.4.1", "ACE14.5", "ACE15.1", "ACE15.5", 
                       "PhysHealth", "PsycHealth", "Smoke", "AgeSmoke", "IH1", 
                       "IH2", "IH3", "IH4", "IH5", "IH6", "IH7", "IH8", "IH9", 
                       "IH10", "IH11", "IH12", "valid1", "IH13", "IH14", "IH15", "IH16", 
                       "IH17", "IH18", "SE1", "SE2", "SE3", "SE4", "SE5", "SE6", 
                       "SE7", "SE8", "SE9", "SE10", "SE11", "SE12", "SE13", "SE14", "SE15",
                       "IFC1", "IFC2", "IFC3", "IFC4", "IFC5", "IFC6", "IFC7", 
                       "IFC8", "IFC9", "IFC10", "IFC11", "IFC12", "IFC13", "IFC14",
                       "IFC15", "IFC16", "IFC17", "IFC18", "IFC19", "IFC20", 
                       "IFC21", "IFC22", "IFC23", "IFC24", "IFC25", "IFC26", "IFC27",
                       "MFQ1", "MFQ2", "MFQ3", "MFQ4", "MFQ5", "MFQ6", "MFQ7", 
                       "MFQ8", "MFQ9", "MFQ10", "MFQ11", "MFQ12", "MFQ13", "MFQ14", 
                       "MFQ15", "MFQ16", "MFQ17", "MFQ18", "MFQ19", "MFQ20", "valid2", 
                       "BOM1", "BOM2", "BOM3", "BOM4", "BOM5", "BOM6", "BOM7", 
                       "BOM8", "BOM9", "BOM10", "BOM11", "BOM12", "BOM13", "BOM14", 
                       "BOM15", "BOM16", "BOM17", "BOM18", "BOM19","BOM20", "valid3",
                       "SLS1", "SLS2", "SLS3", "SLS4", "SLS5", 'valid4', 
                       "GAD1", "GAD2", "GAD3", "GAD4", "GAD5", "GAD6", "GAD7", 
                       "PHQ1", "PHQ2", "PHQ3", "PHQ4", "PHQ5", "PHQ6", "PHQ7", 
                       "PHQ8", "PHQ9", "valid5" )

### Check validity questions
table( fall$valid1 ) # correct answer = 1 (n = 244)
table( fall$valid2 ) # correct answer = 6 (n = 271)
table( fall$valid3 ) # correct answer = 1 (n = 147)
table( fall$valid4 ) # correct answer = 1 (n = 271)
table( fall$valid5 ) # correct answer = 1 (n = 234)

fall$keep <- ifelse( fall$valid1 == 1 & fall$valid2 == 6 & 
                          fall$valid3 == 6 & fall$valid4 == 1 & 
                          fall$valid5 == 1, 1, 0 )

table( fall$keep, exclude = NULL ) # 6 valid participants

### The following quality assurance questions may not be valid:
# I get paid bi-weekly by unicorns
# All of my friends say that I would make a great poodle
# All of my friends are space aliens

### Only use quality assurance items that ask, "Please choose 'strongly agree/disagree'"
fall$keep <- ifelse( fall$valid2 == 6 & fall$valid4 == 1, 1, 0 )

table( fall$keep, exclude = NULL ) # 261 valid participants

fall <- fall[ which( fall$keep == 1 ), ]


### Explore study completion time
fall$Duration.M <- fall$Duration / 60 # convert from seconds to minutes
describe( fall$Duration.M )
#  n    mean    sd     median    min     max   
# 261  88.11  326.27   25.40    9.25   3095.20

### May want to consider removing participants below a certain time cut-off

### Write out data file for data sharing
write_csv( fall, "AIM_data_fall.csv" )




                       #########################
                       ### QC of Spring data ###
                       #########################

### Remove participants who did not consent to the study
spring <- spring[ which( spring$Q1.1 == 1 ), ] # 3 participants removed

### Remove variables containing identifiable or unnecessary information
spring <- spring %>% select( -StartDate, -EndDate, - Status, -RecordedDate, 
                             -DistributionChannel, -UserLanguage, -Q1.1,
                             -Q19.1, -Q19.2, -Create_New_Field_or_Choose_From_Dropdown...,
                             -id, -Q2.34___Parent_Topics, -Q2.34___Topics )

### Create variable names
names( spring ) <- c( "Progress", "Duration", "Finished", "ID", "Age", "Sex", 
                       "Gender", "Gender_Other", "Feminimity", "Masculinity", 
                       "SexOrient", "SexOrient_Other", "Race1", "Race2", "Race3", 
                       "Race4", "Race5", "Race6", "Race7", "Race8", "Race_Other", 
                       "PCG", "PCG_Other", "PGC_EDU", "PGC_EDU_Other", "ses1", "ses2", 
                       "SimRace", "SimRace_Other", "Learn", "PolAffil", 
                       "PolAffil_Other", "Religion", "Religion_Othr", 
                       "ACE1.1", "ACE1.2", "ACE1.3", "ACE1.4", "ACE1.frq.1", 
                       "ACE1.frq.2", "ACE1.frq.3", "ACE1.frq.4", "ACE1.5", 
                       "ACE2.1", "ACE2.2", "ACE2.3", "ACE2.4", "ACE2.frq.1", 
                       "ACE2.frq.2", "ACE2.frq.3", "ACE2.frq.4", "ACE2.5",
                       "ACE3.1", "ACE3.2", "ACE3.3", "ACE3.4", "ACE3.5", 
                       "ACE4.1", "ACE4.2", "ACE4.3", "ACE4.4", "ACE4.frq.1",
                       "ACE4.frq.2",  "ACE4.5", 
                       "ACE5.1", "ACE5.2", "ACE5.3", "ACE5_COD", "ACE5.4", "ACE5.5", 
                       "ACE6.1", "ACE6.2", "ACE6.3", "ACE6.4", "ACE6.frq.1", 
                       "ACE6.frq.2", "ACE6.5", 
                       "ACE7.1", "ACE7.4", "ACE7.frq.1", "ACE7.frq.2", "ACE7.frq.3",
                       "ACE7.frq.4", "ACE7.5", 
                       "ACE8.1", "ACE8.4", "ACE8.5", 
                       "ACE9.1", "ACE9.2", "ACE9.3", "ACE9.4", "ACE9.frq.1", "ACE9.frq.2",
                       "ACE9.frq.3", "ACE9.frq.4", "ACE9.5", 
                       "ACE10.1", "ACE10.2", "ACE10.3", "ACE10.4", "ACE10.frq.1",
                       "ACE10.frq.2", "ACE10.frq.3", "ACE10.frq.4", "ACE10.5",
                       "ACE11.1", "ACE11.2", "ACE11.3", "ACE11.4", "ACE11.frq.1",
                       "ACE11.frq.2", "ACE11.frq.3", "ACE11.frq.4", "ACE11.5",
                       "ACE12.1", "ACE12.frq.1", "ACE12.frq.2", "ACE12.frq.3", 
                       "ACE12.frq.4", "ACE12.5",
                       "ACE13.1", "ACE13.frq.1", "ACE13.frq.2", "ACE13.frq.3", 
                       "ACE13.frq.4", "ACE13.5",
                       "ACE14.1", "ACE14.frq.1", "ACE14.frq.2", "ACE14.frq.3", 
                       "ACE14.frq.4", "ACE14.5",
                       "ACE15.1", "ACE15.frq.1", "ACE15.frq.2", "ACE15.frq.3", 
                       "ACE15.frq.4", "ACE15.5", "Therapy", "Therapy_Age",
                       "Therapy_Help", "PhysHealth", "PsycHealth", "Smoke", "AgeSmoke", 
                       "IH1", "IH2", "IH3", "IH4", "IH5", "IH6", "IH7", "IH8", "IH9", 
                       "IH10", "IH11", "IH12", "valid1", "IH13", "IH14", "IH15", "IH16", 
                       "IH17", "IH18", "SE1", "SE2", "SE3", "SE4", "SE5", "SE6", 
                       "SE7", "SE8", "SE9", "SE10", "SE11", "SE12", "SE13", "SE14", "SE15",
                       "IFC1", "IFC2", "IFC3", "IFC4", "IFC5", "IFC6", "IFC7", 
                       "IFC8", "IFC9", "IFC10", "IFC11", "IFC12", "IFC13", "IFC14",
                       "IFC15", "IFC16", "IFC17", "IFC18", "IFC19", "IFC20", 
                       "IFC21", "IFC22", "IFC23", "IFC24", "IFC25", "IFC26", "IFC27",
                       "MFQ1", "MFQ2", "MFQ3", "MFQ4", "MFQ5", "MFQ6", "MFQ7", 
                       "MFQ8", "MFQ9", "MFQ10", "MFQ11", "MFQ12", "MFQ13", "MFQ14", 
                       "MFQ15", "MFQ16", "MFQ17", "MFQ18", "MFQ19", "MFQ20", "valid2", 
                       "BOM1", "BOM2", "BOM3", "BOM4", "BOM5", "BOM6", "BOM7", 
                       "BOM8", "BOM9", "BOM10", "BOM11", "BOM12", "BOM13", "BOM14", 
                       "BOM15", "BOM16", "BOM17", "BOM18", "BOM19","BOM20", "valid3",
                       "SLS1", "SLS2", "SLS3", "SLS4", "SLS5", 'valid4', 
                       "GAD1", "GAD2", "GAD3", "GAD4", "GAD5", "GAD6", "GAD7", 
                       "PHQ1", "PHQ2", "PHQ3", "PHQ4", "PHQ5", "PHQ6", "PHQ7", 
                       "PHQ8", "PHQ9", "valid5", "COVID1", "COVID2", "COVID3", "COVID4", 
                       "COVID5", "COVID6", "COVID7", "COVID8", "COVID9" )

### Remove participants with incorrect responses to any quality assurance questions
### All of these items ask for participants to choose "strongly agree/disagree"
table( spring$valid1 ) # correct answer = 1 (n = 451)
table( spring$valid2 ) # correct answer = 6 (n = 427)
table( spring$valid3 ) # correct answer = 6 (n = 431)
table( spring$valid4 ) # correct answer = 1 (n = 419)
table( spring$valid5 ) # correct answer = 1 (n = 443)

spring$keep <- ifelse( spring$valid1 == 1 & spring$valid2 == 6 & 
                            spring$valid3 == 6 & spring$valid4 == 1 & 
                            spring$valid5 == 1, 1, 0 )

table( spring$keep, exclude = NULL ) # 364 valid participants

spring <- spring[ which( spring$keep == 1 ), ]


### Explore study completion time
spring$Duration.M <- spring$Duration / 60 # convert from seconds to minutes
describe( spring$Duration.M )
#  n    mean    sd     median   min    max   
# 364  153.66  783.42  31.73  11.72  11661.02

### May want to consider removing participants below a certain time cut-off

### Write out raw data file for data sharing
write_csv( spring, "AIM_data_spring.csv" )
