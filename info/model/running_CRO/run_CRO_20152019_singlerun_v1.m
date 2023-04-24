% Script to run MyLake (v_12) for Lac Croche, bassin est (CRO)
% also calculate RMSD
% by CB, Feb. 2017

clear;
path(path,'C:\Users\brand\OneDrive - Universite de Montreal\Bureau\Maitrise\Projet\model\Mylake_v12_package\air_sea') %path for air-sea toolbox
path(path,'C:\Users\brand\OneDrive - Universite de Montreal\Bureau\Maitrise\Projet\model\Mylake_v12_package\v12')     %path for MyLake model code

global ies80 Eevapor;
test_time=0;
Eevapor=0;



% ***** to be specified before running ************************************

lake = 'Lac Croche';
year = 2015;
m_start = [2015, 5, 14]; 
m_stop =  [2022,12, 31];
initfile =  'C:\Users\brand\OneDrive - Universite de Montreal\Bureau\Maitrise\Projet\model\running_CRO\1_input_files_CRO\CRO_init_20152019_start_14may.xlsx';
inputfile = 'C:\Users\brand\OneDrive - Universite de Montreal\Bureau\Maitrise\Projet\model\running_CRO\1_input_files_CRO\CRO_input_meteo_era5_20152022_start_14may_v2.xlsx';
parafile =  'C:\Users\brand\OneDrive - Universite de Montreal\Bureau\Maitrise\Projet\model\running_CRO\1_input_files_CRO\CRO_para_20152019_v2.xlsx';

fileout = 'C:\Users\brand\OneDrive - Universite de Montreal\Bureau\Maitrise\Projet\model\running_CRO\3_output\CRO_20152019_out.mat';
save_out = 1; % 0 if do not save, 1 if save

save_jpg = 0; % 0 if not saving as jpg, 1 if saved as jpg
save_eps = 0; % 0 if not saving as eps, 1 if saved as eps
figname = 'plot_mod_vs_obs_CRO_2022_singlerun_v1';

% ***** initiating 
runno = 0;

% ***** run MyLake **************************************************
                        
runno = runno + 1;
disp(' ');
disp('x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x');
processing_txt = 'calling solvemodel_v12_2017a';
disp(processing_txt);
disp(' ');

[zz,Az,Vz,tt,Qst,Kzt,Tzt,Czt,Szt,Pzt,Chlzt,PPzt,DOPzt,DOCzt,Qzt_sed,lambdazt,...
P3zt_sed,P3zt_sed_sc,His,DoF,DoM,MixStat,Wt,Phys_par,Bio_par]...
= solvemodel_v12_2017a(m_start,m_stop,initfile,'lake',inputfile,'timeseries', parafile,'lake');

 
Kz_ak_plot = Phys_par(2); % open water diffusion parameter (-)
Kz_ak_ice_plot = Phys_par(3); % under ice diffusion parameter (-)
Kz_N0_plot = Phys_par(4); % min. stability frequency (s-2)
C_shelter_plot = Phys_par(5); % wind shelter parameter (-)
lat_plot = Phys_par(6); %latitude (decimal degrees)
lon_plot = Phys_par(7); %longitude (decimal degrees)
alb_melt_ice_plot = Phys_par(8);   % albedo of melting ice (-)
alb_melt_snow_plot = Phys_par(9); % albedo of melting snow (-)
swa_b0_plot = Bio_par(1); % non-PAR light extinction coefficient
swa_b1_plot = Bio_par(2); % PAR light extinction coefficient

       
DoF_realtime=DoF+tt(1)-1; %TSA, antatt at tidsteg er 1 dag
DoM_realtime=DoM+tt(1)-1; %TSA
DoF_plottime=DoF+tt(1)-1-datenum(year,1,1); %TSA, antatt at tidsteg er 1 dag
DoM_plottime=DoM+tt(1)-1-datenum(year,1,1); %TSA
tt_mod = tt - datenum(year,1,1); %time now scaled so that it begins from the 1 january of the "year" (=0)

% *****

%plot_mod_vs_obs_CRO_20152019_singlerun_v2(runno,tt,Tzt,Kz_ak_plot,Kz_ak_ice_plot,Kz_N0_plot,C_shelter_plot,alb_melt_ice_plot,alb_melt_snow_plot);
%plot_mod_vs_obs_CRO_20152019_singlerun_v3(runno,m_start,m_stop,tt,Tzt,Kz_ak_plot,Kz_ak_ice_plot,Kz_N0_plot,C_shelter_plot,alb_melt_ice_plot,alb_melt_snow_plot);
plot_mod_vs_obs_CRO_2021_singlerun_v1(runno,m_start,m_stop,tt,Tzt,Kz_ak_plot,Kz_ak_ice_plot,Kz_N0_plot,C_shelter_plot,alb_melt_ice_plot,alb_melt_snow_plot,...
    swa_b0_plot,swa_b1_plot);
% ***************

if save_out == 1
     % Define the CSV file path
    save (fileout, 'zz', 'tt', 'Tzt', 'His');
        % Define the CSV file paths for depth and time
    depthCsvFile = 'C:\Users\brand\OneDrive - Universite de Montreal\Bureau\Maitrise\Projet\model\running_CRO\3_output\depth_data.csv';
    timeCsvFile = 'C:\Users\brand\OneDrive - Universite de Montreal\Bureau\Maitrise\Projet\model\running_CRO\3_output\time_data.csv';

    % Save 'zz' (depth) and 'tt' (time) in separate CSV files
    writematrix(zz, depthCsvFile); % Depth data
    writematrix(tt_mod, timeCsvFile);  % Time data

    % Define the CSV file path for 'Tzt' (temperature data)
    temperatureCsvFile = 'C:\Users\brand\OneDrive - Universite de Montreal\Bureau\Maitrise\Projet\model\running_CRO\3_output\temperature_data.csv';

    % Save 'Tzt' as a matrix in a CSV file
    writematrix(Tzt, temperatureCsvFile);

    disp(['Data saved to ' depthCsvFile]);
    disp(['Data saved to ' timeCsvFile]);
    disp(['Data saved to ' temperatureCsvFile]);
end

if save_jpg == 1
    %print ('-djpeg100','-painters',figname);
    print ('-djpeg100',figname);
end

if save_eps == 1
    print ('-depsc','-tiff',figname);
end


