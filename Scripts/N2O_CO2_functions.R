##########
#N2O and CO2 processing data functions
##########


##########
#calibration regression functions
##########

cal.N2O=function(run, run_no){
  
  #Create the data.frame with calibration values
  cal=run[run$Type=="STD.N2O",c("N2O","Cal.value")]
  
  #Linear calibration
  reg.cal.lin=lm(cal$Cal.value~cal$N2O)
  C1.lin=reg.cal.lin$coefficient[2]
  Int.lin=reg.cal.lin$coefficient[1]
  R2.lin=summary(reg.cal.lin)$adj.r.squared
  #Polynomial calibration
  reg.cal.poly=lm(cal$Cal.value~cal$N2O+I(cal$N2O^2))
  C1.poly=reg.cal.poly$coefficient[2]
  C2.poly=reg.cal.poly$coefficient[3]
  Int.poly=reg.cal.poly$coefficient[1]
  R2.poly=summary(reg.cal.poly)$adj.r.squared
  
  plot(cal$N2O,cal$Cal.value,pch=16,xlab="N2O GC value",ylab="N2O (ppm)",
       title(main=paste("Calibration","  ","R2.lin","=",round(R2.lin,5),"  ","R2.poly","=",round(R2.poly,5),sep=" ")),
       ylim=c(0,3.2))
  abline(reg.cal.lin,col="red",lwd=2)
  legend('bottomright',paste('run', run_no, sep = ' '), bty = 'n')
  
  #Calibrating samples values using both calibrations
  run$cal.values.lin=run$N2O*C1.lin+Int.lin
  run$cal.values.poly=C2.poly*(run$N2O)^2+run$N2O*C1.poly+Int.poly
  run$cal.head.lin=run$pN2O.head*C1.lin+Int.lin
  run$cal.head.poly=C2.poly*(run$pN2O.head)^2+run$pN2O.head*C1.poly+Int.poly
  
  return(run)
  
}

cal.CO2=function(run, run_no){
  
  #Create the data.frame with calibration values
  cal=run[run$Type=="STD.CO2",c("CO2.TCD","Cal.value")]
  
  #Linear calibration
  reg.cal.lin=lm(cal$Cal.value~cal$CO2.TCD)
  C1.lin=reg.cal.lin$coefficient[2]
  Int.lin=reg.cal.lin$coefficient[1]
  R2.lin=summary(reg.cal.lin)$adj.r.squared
  #Polynomial calibration
  reg.cal.poly=lm(cal$Cal.value~cal$CO2.TCD+I(cal$CO2.TCD^2))
  C1.poly=reg.cal.poly$coefficient[2]
  C2.poly=reg.cal.poly$coefficient[3]
  Int.poly=reg.cal.poly$coefficient[1]
  R2.poly=summary(reg.cal.poly)$adj.r.squared
  
  plot(cal$CO2.TCD,cal$Cal.value,pch=16,xlab="CO2 GC value",ylab="CO2 (ppm)",
       title(main=paste("Calibration","  ","R2.lin","=",round(R2.lin,5),"  ","R2.poly","=",round(R2.poly,5),sep=" ")),
       ylim=c(0,12000))
  abline(reg.cal.lin,col="red",lwd=2)
  legend('bottomright',paste('run', run_no, sep = ' '), bty = 'n')
  
  #Calibrating samples values using both calibrations
  run$cal.values.lin=run$CO2.TCD*C1.lin+Int.lin
  run$cal.values.poly=C2.poly*(run$CO2.TCD)^2+run$CO2.TCD*C1.poly+Int.poly
  run$cal.head.lin=run$pCO2.head*C1.lin+Int.lin
  run$cal.head.poly=C2.poly*(run$pCO2.head)^2+run$pCO2.head*C1.poly+Int.poly
  
  return(run)
  
}

################################################################### 
# The following function head.calc.N2O back Calculates N2O and CO2 concentrations (in ppm and nmol/L)
# in the in situ water body from a sample extracted using the headspace technique.
# In order to run the function you need to import a csv or text file containing the input variables 
# used in the calculation (see below).
###################################################################


#Function start
##############################################################
head.calc.N2O.CO2.single = function(run) {
  
  #Defining input table columns
  # If you import a table format with other column names than the ones below
  # then you must change them accordingly after the dollar sign in each row.
  # IMPORTANT!! Verify that your inputs variables are in the rignt units (indicated below for each column)
  
  Sample.Id = as.character(run$Sample.Id) #Sample ID
  pN2O.meas = run$pN2O.meas #N2O (ppm) measured in the headspace after shaking (value given by the GC and calibration step)
  pN2O.head = run$pN2O.head #N2O (ppm) in the headspace before shaking
  pCO2.meas = run$pCO2.meas #CO2 (ppm) measured in the headspace after shaking (value given by the GC and calibration step)
  pCO2.head = run$pCO2.head #CO2 (ppm) in the headspace before shaking
  vol.tot = run$vol.tot #total volume of the bottle/syringe in L
  vol.head = run$vol.head #Volume of the headspace in L (water removed from the bottle)
  temp.in = run$temp.in #in situ temperature (before shaking) in degree celsius
  temp.sam = run$temp.sam #sampling temperature (after shaking) in degree celsius
  atm = run$atm #Atmospheric pressure in kPa
  sal = run$sal #Salinity, considered 0 in freshwater systems
  
  #Define the error for gas concentrations and isotopic signature
  pN2O.error=abs(0.1*pN2O.meas) #Typical CV of a replicate for N2O gas concentration (here it's set to 10%)
  pCO2.error=abs(0.1*pCO2.meas) #Typical CV of a replicate for N2O gas concentration (here it's set to 10%)
  
  # Calculating intermediate terms
  # mol.vol = Molar volume
  # ratio = Headspace ratio (air/water)
  # sol.N2O.in = solubility of N2O at in situ temperature 
  # sol.N2O.sam = solubility of N2O at sampling temperature
  # sol.CO2.in = solubility of CO2 at in situ temperature 
  # sol.CO2.sam = solubility of CO2 at sampling temperature
  
  mol.vol = (0.082057*(temp.sam+273.15))*(101.325/atm)
  ratio =  vol.head / (vol.tot-vol.head)
  sol.N2O.in = exp(-62.7062+97.3066*(100/(temp.in+273.15))+24.1406*log((temp.in+273.15)/100)+sal*(-0.05842+0.033193*((temp.in+273.15)/100)+(-0.0051313)*((temp.in+273.15)/100)^2))
  sol.N2O.sam = exp(-62.7062+97.3066*(100/(temp.sam+273.15))+24.1406*log((temp.sam+273.15)/100)+sal*(-0.05842+0.033193*((temp.sam+273.15)/100)+(-0.0051313)*((temp.in+273.15)/100)^2))
  sol.CO2.in = 10^-(-((9345.17/(temp.in + 273.15))-60.2409+23.3585*log((273.15+temp.in)/100)+sal*(0.023517-0.023656*((273.15+temp.in)/100)+0.0047036*((273.15+temp.in)/100)^2))/log(10))
  sol.CO2.sam = 10^-(-((9345.17/(temp.sam + 273.15))-60.2409+23.3585*log((273.15+temp.sam)/100)+sal*(0.023517-0.023656*((273.15+temp.sam)/100)+0.0047036*((273.15+temp.sam)/100)^2))/log(10))
  
  #Generate random vectors with a normal distribution based on error for gas concentration and isotopic signature for simulations
  pN2O.random=rnorm(10000,mean=pN2O.meas,sd=pN2O.error)
  pCO2.random=rnorm(10000,mean=pCO2.meas,sd=pCO2.error)
  
  # Calculating in situ N2O and CO2 concentrations
  # pN2O.insitu = in situ N2O concentration in the water (in ppm)
  # conc.N2O.insitu = in situ CO2 concentration in the water (in nmol/L)
  # delta.N2O = difference between in situ observed N2O concentration and concentration at equilibrium with the atmosphere
  # pCO2.insitu = in situ CO2 concentration in the water (in ppm)
  # conc.CO2.insitu = in situ CO2 concentration in the water (in nmol/L)
  # delta.CO2 = difference between in situ observed CO2 concentration and concentration at equilibrium with the atmosphere
  
  pN2O.insitu = ((pN2O.meas-pN2O.head)*ratio/(mol.vol)+(pN2O.meas*sol.N2O.sam))/sol.N2O.in
  conc.N2O.insitu = pN2O.insitu * sol.N2O.in*1000
  delta.N2O = conc.N2O.insitu - pN2O.head*sol.N2O.in*1000
  pCO2.insitu = ((pCO2.meas-pCO2.head)*ratio/(mol.vol)+(pCO2.meas*sol.CO2.sam))/sol.CO2.in
  conc.CO2.insitu = pCO2.insitu * sol.CO2.in*1000
  delta.CO2 = conc.CO2.insitu - pCO2.head*sol.CO2.in*1000
  
  #Calculating error on gas concentration using Monte Carlo simulation
  pN2O.insitu.error = sd(unlist(lapply(1:length(pN2O.random),function(i){ ((pN2O.random[i]-pN2O.head)*ratio/(mol.vol)+(pN2O.random[i]*sol.N2O.sam))/sol.N2O.in})))
  conc.N2O.insitu.error = pN2O.insitu.error * sol.N2O.in *1000
  pCO2.insitu.error = sd(unlist(lapply(1:length(pCO2.random),function(i){ ((pCO2.random[i]-pCO2.head)*ratio/(mol.vol)+(pCO2.random[i]*sol.CO2.sam))/sol.CO2.in})))
  conc.CO2.insitu.error = pCO2.insitu.error * sol.CO2.in *1000
  
  # Compiling the results in a data table
  # Here you can custumize the output of the function by adding/removing/changing the order
  # of the output columns below in the parenthesis. 
  output.single=cbind(run,pN2O.insitu,pN2O.insitu.error,conc.N2O.insitu,conc.N2O.insitu.error,delta.N2O,
                      pCO2.insitu,pCO2.insitu.error,conc.CO2.insitu,conc.CO2.insitu.error,delta.CO2,pN2O.head,pCO2.head)
  
  # Defining the object to return
  return(output.single)
  
}

head.calc.N2O.CO2=function(run){
  output = lapply(1:dim(run)[1],function(i){head.calc.N2O.CO2.single(run[i,])})
  col.names=colnames(data.frame(output[1]))
  output=do.call(rbind.data.frame, output)
  output$Sample.Id=as.character(output$Sample.Id)
  output$Date=as.character(output$Date)
  output$Time=as.character(output$Time)
  output$File.Name=as.character(output$File.Name)
  output$Method.Name=as.character(output$Method.Name)
  output$User.Name=as.character(output$User.Name)
  output$Quality=as.character(output$Quality)
  output$Type=as.character(output$Type)
  colnames(output)=col.names
  return(output)
}


#End of function
#######################################################


#######################################################
#Please document any changes to the function below
#######################################################

#July 11 2018: function trasferred to labo Maranger
#July 20 2018: modified by MBotrel, 
  #added a number of run argument to the cal.N2O function
  #added a CO2 calibration function (cal.CO2)
  #modification of the solubility formula in the head.calc.N2O funtion (formaly there was an error and the formula was for CO2)
  #modification of head.calc.N2O function to include CO2 calculation (name changed to head.calc.N2O.CO2)


