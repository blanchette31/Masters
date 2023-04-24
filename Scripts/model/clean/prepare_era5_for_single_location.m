% Script for preparing ERA5 reanalysis data to be used by MyLake
% at a single location.
% This script ...
% 1) converts from ERA5 time stamps (0 is 0h 01 JAN 1900)
%   to Matlab time stamps (1 is 0h 01 JAN 0000)
% 2) converts from time zone UTC to time zone EST,
% 3) converts the hourly values to the units used by MyLake
% 4) calculates daily values from hourly values,
% 5) reads the hourly data in NetCDF format and saves the daily data in Matlab format

% This version proceeds one year at the time.
% Wind speed is calculated from u-wind and v-wind at 10 m.
% Relative humidity is calculated from dewpoint and air temperature.

% suffix 2: hourly, EST, MyLake units, single point
% suffix 3: daily, EST, MyLake units, single point

% Claude Bélanger, Oct. 2022

clear;

% *** input file ***
year1 = 2022;   % year to be processed
type1 = 1;      % 1 for validated data, 2 for temporary data (ERA5T)
year2 = 2022;   % following year (for conversion from UTC to EST)
type2 = 1;      % 1 for validated data, 2 for temporary data (ERA5T)

yearstr1 = num2str(year1);
filein1 = ['CRO_era5_' yearstr1 '.nc'];
%filein1
%ncdisp(filein1,'/','min');
ncdisp(filein1);

yearstr2 = num2str(year2);
filein2 = ['CRO_era5_' yearstr2 '.nc'];
%filein2
%ncdisp(filein2,'/','min');
ncdisp(filein2);


% *** output file ***

fileout = ['era5_CRO_' yearstr1 '_daily.mat'];

% *** create new time vector (using matlab date number)
% new time: time1 (8760 x 1 on normal years, 8784 x 1 on leap years)

%time = ncread(filein,'time');
%size(time)
%clear time;

year = year1;
first_time = datenum(year,1,1,0,0,0);
time1 = ncread(filein1,'time');
nhours = max( size(time1) );
last_time = first_time + (nhours/24) -1 ;
time2 = (first_time:1:last_time);
time2 = time2';
%size(time2)
%datestr(time1(end))


% *** reading and converting from UTC to EST ****************************** 

disp(' ')
disp(' reading air temperature, converting to EST, converting to °C ')
t2m1 = ncread(filein1,'t2m');
size(t2m1)
if type1 == 1
    t2m1 = squeeze(t2m1);
else
    t2m1 = squeeze(t2m1);
    nhours = max(size(t2m1));
    for i = 1:nhours
        if isnan(t2m1(1,i)); t2m1(1,i) = t2m1(2,i); end
        if isnan(t2m1(2,i)); t2m1(2,i) = t2m1(1,i); end
    end
    t2m1 = t2m1(2,:); % that is when using ERA5T data (not validated)
    t2m1 = squeeze(t2m1);
    t2m1 = t2m1';
end
size(t2m1)
t2m1 = t2m1(6:end); % temperature 1st part
size(t2m1)

t2m2 = ncread(filein2,'t2m');
size(t2m2)
if type2 == 1
    t2m2 = squeeze(t2m2);
else
    t2m2 = squeeze(t2m2);
    nhours = max(size(t2m2));
    for i = 1:nhours
        if isnan(t2m2(1,i)); t2m2(1,i) = t2m2(2,i); end
        if isnan(t2m2(2,i)); t2m2(2,i) = t2m2(1,i); end
    end
    t2m2 = t2m2(2,:); % that is when using ERA5T data (not validated)
    t2m2 = squeeze(t2m2);
    t2m2 = t2m2';
end
size(t2m2)
t2m2 = t2m2(1:5);   % temperature 2nd part
size(t2m2)

airtem1 = [t2m1; t2m2];  % temperature EST (K)
size(airtem1)
airtem2 = airtem1 - 273.15;  % temperature EST (C)
size(airtem2)

disp(' reading surface pressure, converting to EST, converting to hPa ')
sp1 = ncread(filein1,'sp');
if type1 == 1
    sp1 = squeeze(sp1);
else
    sp1 = squeeze(sp1);
    nhours = max(size(sp1));
    for i = 1:nhours
        if isnan(sp1(1,i)); sp1(1,i) = sp1(2,i); end
        if isnan(sp1(2,i)); sp1(2,i) = sp1(1,i); end
    end
    sp1 = sp1(2,:); % that is when using ERA5T data (not validated)
    sp1 = squeeze(sp1);
    sp1 = sp1';
end
sp1 = sp1(6:end); % surface pressure 1st part

sp2 = ncread(filein2,'sp');
if type2 == 1
    sp2 = squeeze(sp2);
else
    sp2 = squeeze(sp2);
    nhours = max(size(sp2));
    for i = 1:nhours
        if isnan(sp2(1,i)); sp2(1,i) = sp2(2,i); end
        if isnan(sp2(2,i)); sp2(2,i) = sp2(1,i); end
    end
    sp2 = sp2(2,:); % that is when using ERA5T data (not validated)
    sp2 = squeeze(sp2);
    sp2 = sp2';
end
sp2 = sp2(1:5);   % surface pressure 2nd part

atmpre1 = [sp1; sp2];       % surface pressure EST (Pa)
atmpre2 = atmpre1 / 100.0;  % surface pressure EST (hPa)
%size(atmpre2)

disp(' reading solar radiation, converting to EST, converting to MJ/m2 ')
ssrd1 = ncread(filein1,'ssrd');
if type1 == 1
    ssrd1 = squeeze(ssrd1);
else
    ssrd1 = squeeze(ssrd1);
    nhours = max(size(ssrd1));
    for i = 1:nhours
        if isnan(ssrd1(1,i)); ssrd1(1,i) = ssrd1(2,i); end
        if isnan(ssrd1(2,i)); ssrd1(2,i) = ssrd1(1,i); end
    end
    ssrd1 = ssrd1(2,:); % that is when using ERA5T data (not validated)
    ssrd1 = squeeze(ssrd1);
    ssrd1 = ssrd1';
end
ssrd1 = ssrd1(6:end); % solar radiation 1st part

ssrd2 = ncread(filein2,'ssrd');
if type2 == 1
    ssrd2 = squeeze(ssrd2);
else
    ssrd2 = squeeze(ssrd2);
    nhours = max(size(ssrd2));
    for i = 1:nhours
        if isnan(ssrd2(1,i)); ssrd2(1,i) = ssrd2(2,i); end
        if isnan(ssrd2(2,i)); ssrd2(2,i) = ssrd2(1,i); end
    end
    ssrd2 = ssrd2(2,:); % that is when using ERA5T data (not validated)
    ssrd2 = squeeze(ssrd2);
    ssrd2 = ssrd2';
end
ssrd2 = ssrd2(1:5);   % solar radiation 2nd part

glorad1 = [ssrd1; ssrd2];   % surface solar radiation downwards EST (J/m2)
glorad2 = glorad1 / 1000000.0; % surface solar radiation downwards EST (MJ/m2)
%size(glorad2)

disp(' reading cloud cover, converting to EST ')
tcc1 = ncread(filein1,'tcc');
if type1 == 1
    tcc1 = squeeze(tcc1);
else
    tcc1 = squeeze(tcc1);
    nhours = max(size(tcc1));
    for i = 1:nhours
        if isnan(tcc1(1,i)); tcc1(1,i) = tcc1(2,i); end
        if isnan(tcc1(2,i)); tcc1(2,i) = tcc1(1,i); end
    end
    tcc1 = tcc1(2,:); % that is when using ERA5T data (not validated)
    tcc1 = squeeze(tcc1);
    tcc1 = tcc1';
end
tcc1 = tcc1(6:end); % cloud cover 1st part

tcc2 = ncread(filein2,'tcc');
if type2 == 1
    tcc2 = squeeze(tcc2);
else
    tcc2 = squeeze(tcc2);
    nhours = max(size(tcc2));
    for i = 1:nhours
        if isnan(tcc2(1,i)); tcc2(1,i) = tcc2(2,i); end
        if isnan(tcc2(2,i)); tcc2(2,i) = tcc2(1,i); end
    end
    tcc2 = tcc2(2,:); % that is when using ERA5T data (not validated)
    tcc2 = squeeze(tcc2);
    tcc2 = tcc2';
end
tcc2 = tcc2(1:5);   % cloud cover 2nd part

clocov2 = [tcc1; tcc2]; % cloud cover EST (0-1)
%size(clocov2)

disp(' reading wind u component, converting to EST ')
u1 = ncread(filein1,'u10');
if type1 == 1
    u1 = squeeze(u1);
else
    u1 = squeeze(u1);
    nhours = max(size(u1));
    for i = 1:nhours
        if isnan(u1(1,i)); u1(1,i) = u1(2,i); end
        if isnan(u1(2,i)); u1(2,i) = u1(1,i); end
    end
    u1 = u1(2,:); % that is when using ERA5T data (not validated)
    u1 = squeeze(u1);
    u1 = u1';
end
u1 = u1(6:end); % wind u 1st part

u2 = ncread(filein2,'u10');
if type2 == 1
    u2 = squeeze(u2);
else
    u2 = squeeze(u2);
    nhours = max(size(u2));
    for i = 1:nhours
        if isnan(u2(1,i)); u2(1,i) = u2(2,i); end
        if isnan(u2(2,i)); u2(2,i) = u2(1,i); end
    end
    u2 = u2(2,:); % that is when using ERA5T data (not validated)
    u2 = squeeze(u2);
    u2 = u2';
end
u2 = u2(1:5);    % wind u 2nd part

windcu2 = [u1; u2]; % wind component u EST (m/s) 


disp(' reading wind v component, converting to EST ')
v1 = ncread(filein1,'v10');
if type1 == 1
    v1 = squeeze(v1);
else
    v1 = squeeze(v1);
    nhours = max(size(v1));
    for i = 1:nhours
        if isnan(v1(1,i)); v1(1,i) = v1(2,i); end
        if isnan(v1(2,i)); v1(2,i) = v1(1,i); end
    end
    v1 = v1(2,:); % that is when using ERA5T data (not validated)
    v1 = squeeze(v1);
    v1 = v1';
end
v1 = v1(6:end); % wind v 1st part

v2 = ncread(filein2,'v10');
if type2 == 1
    v2 = squeeze(v2);
else
    v2 = squeeze(v2);
    nhours = max(size(v2));
    for i = 1:nhours
        if isnan(v2(1,i)); v2(1,i) = v2(2,i); end
        if isnan(v2(2,i)); v2(2,i) = v2(1,i); end
    end
    v2 = v2(2,:); % that is when using ERA5T data (not validated)
    v2 = squeeze(v2);
    v2 = v2';
end
v2 = v2(1:5);    % wind v 2nd part

windcv2 = [v1; v2]; % wind component v EST (m/s) 

disp(' calculating wind speed from u and v ')
windsp2 = ((windcu2.^2)+(windcv2.^2)).^0.5;
%size(windsp2)

disp(' calculating wind direction from u and v ')
wdir = 90.0-(atan2(windcv2,windcu2)*180.0/pi);
negatif = wdir < 0.0;
wdir = wdir + (negatif * 360.0); % +360 if dir < 0
%size(wdir)

disp(' reading precipitation, converting to EST, converting to mm ')
tp1 = ncread(filein1,'tp');
if year==1979; tp1(:,:,1:7) = 0; end % to correct for 7 NaN at the beginning of 1979
if type1 == 1
    tp1 = squeeze(tp1);
else
    tp1 = squeeze(tp1);
    nhours = max(size(tp1));
    for i = 1:nhours
        if isnan(tp1(1,i)); tp1(1,i) = tp1(2,i); end
        if isnan(tp1(2,i)); tp1(2,i) = tp1(1,i); end
    end
    tp1 = tp1(2,:); % that is when using ERA5T data (not validated)
    tp1 = squeeze(tp1);
    tp1 = tp1';
end
tp1 = tp1(6:end); % precipitation 1st part

tp2 = ncread(filein2,'tp');
if type2 == 1
    tp2 = squeeze(tp2);
else
    tp2 = squeeze(tp2);
    nhours = max(size(tp2));
    for i = 1:nhours
        if isnan(tp2(1,i)); tp2(1,i) = tp2(2,i); end
        if isnan(tp2(2,i)); tp2(2,i) = tp2(1,i); end
    end
    tp2 = tp2(2,:); % that is when using ERA5T data (not validated)
    tp2 = squeeze(tp2);
    tp2 = tp2';
end
tp2 = tp2(1:5); % precipitation 2nd part

precip1 = [tp1; tp2];   % precipitation EST (m per hour)
precip2 = precip1 * 1000.0; % precipitation EST (mm per hour)
%size(precip2)


disp(' reading dew point temmperature, converting to EST and °C, calculating relative humidity ')
d2m1 = ncread(filein1,'d2m');
if type1 == 1
    d2m1 = squeeze(d2m1);
else
    d2m1 = squeeze(d2m1);
    nhours = max(size(d2m1));
    for i = 1:nhours
        if isnan(d2m1(1,i)); d2m1(1,i) = d2m1(2,i); end
        if isnan(d2m1(2,i)); d2m1(2,i) = d2m1(1,i); end
    end
    d2m1 = d2m1(2,:); % that is when using ERA5T data (not validated)
    d2m1 = squeeze(d2m1);
    d2m1 = d2m1';
end
d2m1 = d2m1(6:end); % dewpoint 1st part

d2m2 = ncread(filein2,'d2m');
if type2 == 1
    d2m2 = squeeze(d2m2);
else
    d2m2 = squeeze(d2m2);
    nhours = max(size(d2m2));
    for i = 1:nhours
        if isnan(d2m2(1,i)); d2m2(1,i) = d2m2(2,i); end
        if isnan(d2m2(2,i)); d2m2(2,i) = d2m2(1,i); end
    end
    d2m2 = d2m2(2,:); % that is when using ERA5T data (not validated)
    d2m2 = squeeze(d2m2);
    d2m2 = d2m2';
end
d2m2 = d2m2(1:5); % dewpoint 2nd part

dewtem1 = [d2m1; d2m2];  % dewpoint temperature EST (K)
dewtem2 = dewtem1 - 273.15;  % dewpoint temperature EST (C)
%size(dewtem2)

nhours = max(size(dewtem2));
relhum2 = ones(nhours,1);
a = 17.625;
b = 243.04;
for k = 1:nhours
    tem = airtem2(k);
    dew = dewtem2(k);
    relhum2(k) = 100*( exp((a*dew)/(b+dew)) / exp((a*tem)/(b+tem))); % hourly relative humidity EST (%)
end
%size(relhum2)


% *** from hourly to daily (daily averages) *******************************

disp(' ')
disp(' averaging air temperature '); % *** 2 meter temperature ***
ndays = max(size(airtem2)) / 24;
airtem3 = ones(ndays,1);

for day = 1:ndays           
    t1 = ((day-1)*24)+1; 
    t2 = ((day-1)*24)+24;
    %if (i==10) && (j==10)
    %    disp(['dayno = ' num2str(day) ' t1 = ' num2str(t1) ' t2 = ' num2str(t2)]);
    %end         
    airtem3(day) = mean( airtem2(t1:t2) ); % daily temperature EST (C)
end
%size(airtem3)

disp(' averaging surface pressure ') % *** surface pressure ***
ndays = max(size(atmpre2)) / 24;
atmpre3 = ones(ndays,1);

for day = 1:ndays           
    t1 = ((day-1)*24)+1; 
    t2 = ((day-1)*24)+24;        
    atmpre3(day) = mean( atmpre2(t1:t2) ); % daily surface pressure EST (hPa)
end
%size(atmpre3)

disp(' calculating daily solar radiation from hourly solar radiation ') % *** surface solar radiation downwards ***
ndays = max(size(glorad2)) / 24;
glorad3 = ones(ndays,1);

for day = 1:ndays           
    t1 = ((day-1)*24)+1; 
    t2 = ((day-1)*24)+24;       
    glorad3(day) = sum( glorad2(t1:t2) ); % daily solar radiation EST (MJ/m2)
end
%size(glorad3)

disp(' averaging cloud cover ') % *** total cloud cover ***
ndays = max(size(clocov2)) / 24;
clocov3 = ones(ndays,1);

for day = 1:ndays           
    t1 = ((day-1)*24)+1; 
    t2 = ((day-1)*24)+24;       
    clocov3(day) = mean( clocov2(t1:t2) ); % daily total cloud cover EST (0-1)
end
%size(clocov3)

disp(' averaging wind speed ') % *** wind speed ***
ndays = max(size(windsp2)) / 24;
windsp3 = ones(ndays,1);

for day = 1:ndays           
    t1 = ((day-1)*24)+1; 
    t2 = ((day-1)*24)+24;        
    windsp3(day) = mean( windsp2(t1:t2) ); % daily wind speed EST (m/s)
end
%size(windsp3)

disp(' calculating daily precipitation from hourly precipation ') % *** precipitation ***
ndays = max(size(precip2)) / 24;
precip3 = ones(ndays,1);

for day = 1:ndays           
    t1 = ((day-1)*24)+1; 
    t2 = ((day-1)*24)+24;      
    precip3(day) = sum( precip2(t1:t2) ); % daily precipitation EST (mm)
end
%size(precip3)

disp(' averaging relative humidity ') % *** relative humidity ***
ndays = max(size(relhum2)) / 24;
relhum3 = ones(ndays,1);

for day = 1:ndays           
    t1 = ((day-1)*24)+1; 
    t2 = ((day-1)*24)+24;       
    relhum3(day) = mean( relhum2(t1:t2) ); % daily relative humidity EST (%)
end
%size(relhum3)

% **********************************************

disp(' ')
lon1 = ncread(filein1,'longitude');
lat1 = ncread(filein1,'latitude');

disp(' ')
disp(' saving the daily variables ')
units     =[{'air temperature: °C'};{'atm. pressure: hPa'};{'solar radiation: MJ/m2'};...
    {'cloud coverage: 0-1'};{'wind speed: m/s'};{'precipitation: mm'};{'relative humidity: %'}];
time      = time2;
longitude = lon1;
latitude  = lat1;

airtem = airtem3;
atmpre = atmpre3;
glorad = glorad3;
clocov = clocov3;
windsp = windsp3;
precip = precip3;
relhum = relhum3;

save(fileout,'time','latitude','longitude','airtem','atmpre','glorad',...
    'clocov','windsp','precip','relhum','units');
disp(' done!')

