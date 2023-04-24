% make_meteo_file_single_point
% This script writes ERA5 reanalysis data in an ascii file that can be
% read by MyLake (standard input).

% This script combines data from specified years.
% This script produces only one file (single location).

% Claude Bélanger, October 2022

clear all;

% *** To be specified before running **************************************

firstyear = 2015;
lastyear =  2022;

lake = 'CRO';

fileout = 'input_meteo_era5_CRO_20152022.txt';

% *************************************************************************

% *** Combining n years of data (from firstyear to lastyear) **************

for year = firstyear:lastyear
    
    yearstr = num2str(year); 
    disp(yearstr);
    filein = ['era5_' lake '_' yearstr '_daily.mat'];
    load(filein);
    
    if year == firstyear
        allairtem = airtem;
        %airtem(10,10,end)
        allatmpre = atmpre;
        allglorad = glorad;
        allclocov = clocov;
        allwindsp = windsp;
        allprecip = precip;
        allrelhum = relhum;
        alltime   = time;
    else
        allairtem = [allairtem; airtem];
        allatmpre = [allatmpre; atmpre];
        allglorad = [allglorad; glorad];
        allclocov = [allclocov; clocov];
        allwindsp = [allwindsp; windsp];
        allprecip = [allprecip; precip];
        allrelhum = [allrelhum; relhum];
        alltime   = [alltime; time];
    end
    
    clear 'airtem' 'atmpre' 'glorad' 'clocov' 'windsp' 'precip' 'relhum';
end
%allairtem(10,10,365)
%allairtem(10,10,end)

% *** Creating the meteo file at lat, lon ********

header2a = 'year month day global_rad(MJ/m2) cloud_cov(-) air_temp(C) rel_hum(%%) air_press(hPa) wind_sp(m/s) precip(mm/day) ';
header2b = 'inflow(m3/day) inflow_T(C) inflow_C(-) inflow_S(kg/m3) inflow_TP(mg/m3) inflow_DOP(mg/m3) inflow_chla(mg/m3) inflow_DOC(mg/m3) \n';
header2 = [header2a header2b];

yy = datestr(alltime,'yyyy'); mm = datestr(alltime,'mm'); dd = datestr(alltime,'dd');
yy = str2num(yy); mm = str2num(mm); dd = str2num(dd);

% Specifying inflow values 
[m,n] = size(alltime);
inflow_flux(1:m) = 0;       % inflow (m3/day)
inflow_T(1:m) = 4.0;        % inflow T (C)
inflow_C(1:m) = 0.5;        % inflow C (-)
inflow_S(1:m) = 0.01;       % inflow S (kg/m3)
inflow_TP(1:m) = 50;        % inflow TP (mg/m3)
inflow_DOP(1:m) = 7;        % inflow DOP (mg/m3)
inflow_chla(1:m) = 0.1;     % inflow chla (mg/m3)
inflow_DOC(1:m) = 3000;     % inflow DOC (mg/m3)

% *************************************************************************

disp(['processing ' lake]);
    
header1 = ['-999 ',num2str(latitude),' ',num2str(longitude),'\n'];
%header1
        
inglorad = allglorad;   % GR
inclocov = allclocov;   % CC
inairtem = allairtem;   % air T
inrelhum = allrelhum;   % RH
inatmpre = allatmpre;   % atm P
inwindsp = allwindsp;   % WS
inprecip = allprecip;   % precip
        
output = [yy mm dd ...
    inglorad ...      % GR
    inclocov ...      % CC
    inairtem ...      % air T
    inrelhum ...      % RH
    inatmpre ...      % atm P
    inwindsp ...      % WS
    inprecip ...      % precip
    inflow_flux' inflow_T' inflow_C' inflow_S' ...
    inflow_TP' inflow_DOP' inflow_chla' inflow_DOC'];
        
fid = fopen(fileout,'w');
fprintf(fid,header1);
fprintf(fid,header2);
fprintf(fid,'%4.0f %2.0f %2.0f %6.3f %4.3f %6.2f %6.2f %7.2f %6.3f %6.3f %6.1f %3.1f %3.1f %4.2f %5.1f %3.1f %3.1f %6.1f\n',output');
fclose(fid);

disp('done!');

        
        

