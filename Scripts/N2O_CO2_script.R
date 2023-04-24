#############################
#Script to calculate N2O and CO2 concentrations (in ppm and nM) 
#Using the headspace equilibration technique and output from a GC (Maranger lab, UdeM)
#Author: Cynthia Soued, modified by Morgan Botrel
#############################

#Clean workspace
rm(list=ls()) 

#load the file

source("scripts/N2O_CO2_functions.R")

#Open libraries and file
file=read.csv("Data/Raw/carbon/Brandon/CO2/co2_fall_2022.csv")

#Remove bad quality samples
file=file[!file$Quality=="Bad",]

##====================##
##N2O AND CO2 ANALYSIS##
##====================##

#Divide by run in a list
run <- list()
for(i in 1:length(unique(file$Run))){
  run[[i]] <- file[file$Run==seq(1,max(unique(file$Run)),1)[i],] }

#Calibrate the values for N2O
for(i in 1:length(run)){
  run[[i]] <- cal.N2O(run[[i]],seq(1,length(run),1)[i])
}

#Define if the values should be calibrated using the linear or polynomial calibration
#Choose highest R2
for(i in 1:length(run)) {
  run[[i]]$pN2O.meas=run[[i]]$cal.values.poly
  run[[i]]$pN2O.head=run[[i]]$cal.head.poly
}

#Calibrate the values for CO2
for(i in 1:length(run)){
  run[[i]] <- cal.CO2(run[[i]],seq(1,length(run),1)[i])
}

#Define if the values should be calibrated using the linear or polynomial calibration
#Choose highest R2
for(i in 1:length(run)) {
  run[[i]]$pCO2.meas=run[[i]]$cal.values.poly
  run[[i]]$pCO2.head=run[[i]]$cal.head.poly
}

#Calculate real concentrations of the samples in a new list
results <- list()
for(i in 1:length(run)) {
  results[[i]] <- head.calc.N2O.CO2(run[[i]][run[[i]]$Type=="Sample",])
}

#Combine all the results in one data.frame
output=plyr::rbind.fill(results)
output=output[,c("Date","Time","Sample.Id","pN2O.insitu","pN2O.insitu.error","conc.N2O.insitu","conc.N2O.insitu.error","delta.N2O",
                 "pCO2.insitu", "pCO2.insitu.error", "conc.CO2.insitu", "conc.CO2.insitu.error", "delta.CO2", "pCO2.head", "pN2O.head")]
colnames(output)[1] <- 'gc_analysis_date'


#######################
#Writting output csv file
# Donner un nom au fichier de sortie (je mets souvent le même nom que pour celui d'entrée en changer input pour output comme ça je sais à quel entrée correspond quelle sortie et ça permet de pouvoir faire des vérifications plus facilement)
write.csv(output,"Data/Raw/carbon/Brandon/CO2/co2_fall_2022_calculs.csv")



