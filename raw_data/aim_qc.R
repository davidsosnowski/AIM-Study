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


                          #######################
                          ### QC of Fall data ###
                          #######################

# remove participants who did not consent to the study
fall <- fall[ which( fall$Q1.1 == 1 ), ] # 3 participants removed

# remove variables containing identifiable or unnecessary information
fall.qc <- fall[ ,-c( 1:4, 8, 10:18, 246:251 ) ]

# create variable names
names( fall.qc ) <- c( "Progress", "Duration", "Finished", "ID", "Age", "Sex", 
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
                       "ACE13.1", "ACE13.4", "ACE13.4.1", "ACE13.5",
                       "ACE.14.1", "ACE14.4.1", "ACE15.1", "AC15.5", 
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

# write out raw data file for data sharing
write.csv( fall.qc, "raw_data_fall.csv", row.names = F, quote = F )




                       #########################
                       ### QC of Spring data ###
                       #########################

# remove participants who did not consent to the study
spring <- spring[ which( spring$Q1.1 == 1 ), ] # 3 participants removed

# remove variables containing identifiable or unnecessary information
spring.qc <- spring[ ,-c( 1:3, 7, 9:11, 288:293 ) ]

# create variable names
names( spring.qc ) <- c( "Progress", "Duration", "Finished", "ID", "Age", "Sex", 
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

# write out raw data file for data sharing
write.csv( spring.qc, "raw_data_spring.csv", row.names = F, quote = F )
