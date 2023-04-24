clf;
clear;

% *** to be specified before runnning ***

lake = 'CRO';
filein = 'input_meteo_era5_CRO_20152022.txt';

mdata = importdata(filein, ' ', 2); % meteo data

mintime = [2015  1  1  0  0  0];
maxtime = [2022 12 31  0  0  0];

minglorad =  0.0;       maxglorad =  35.0;
minclocov =  0.0;       maxclocov =  1.0;
minairtem =  -30.0;     maxairtem = 30.0;
minwindsp =  0.0;       maxwindsp =  10.0;
minprecip =  0.0;       maxprecip =  50.0;

% ***************************************

ndays = max(size(mdata.data));
for i = 1:ndays
    yy = mdata.data(i,1);
    mm = mdata.data(i,2);
    dd = mdata.data(i,3);
    time(i) = datenum(yy,mm,dd);
end

ticks(1)=datenum(2015,1,1,0,0,0);
ticks(2)=datenum(2016,1,1,0,0,0);
ticks(3)=datenum(2017,1,1,0,0,0);
ticks(4)=datenum(2018,1,1,0,0,0);
ticks(5)=datenum(2019,1,1,0,0,0);
ticks(6)=datenum(2020,1,1,0,0,0);
ticks(7)=datenum(2021,1,1,0,0,0);
ticks(8)=datenum(2022,1,1,0,0,0);

% ******

color1 = [0 0 128]/255;         % navy
color2 = [255 140 0]/255;       % darkorange
color3 = [220 20 60]/255;       % crimson

%color1 = [255 140 0]/255;       % darkorange
%color2 = [238 130 98]/255;      % salmon2
%color3 = [0 0 128]/255;         % navy
%color4 = [180 82 205]/255;      % mediumorchid3
%color5 = [67 110 238]/255;      % royalblue2
%color6 = [220 20 60]/255;       % crimson
%color7 = [0 205 102]/255;       % springgreen2
%color8 = [205 186 150]/255;    % wheat3
%color9 = [162 205 90]/255;      % darkolivegreen3
%color10 = [142 142 142]/255;    % sgigray56
%color11 = [255 215 0]/255;       % gold1
color0 = [0 0 0]/255;          % black
%color99 = [255 102 178]/255;      % pink

ytiks = [0 0.25 0.50 0.75 1.0];
lw = 2;

glorad = mdata.data(:,4);
clocov = mdata.data(:,5);
airtem = mdata.data(:,6);
windsp = mdata.data(:,9);
precip = mdata.data(:,10);

mintime = datenum(mintime(1),mintime(2),mintime(3),mintime(4),mintime(5),mintime(6));
maxtime = datenum(maxtime(1),maxtime(2),maxtime(3),maxtime(4),maxtime(5),maxtime(6));
mintime
maxtime

% *** subplot no.1 (glorad) ***********************************************
%subplot(5,1,1)
axes('Position',[0.1,0.81,0.8,0.15],'Box','on');
plot(time,glorad,'linewidth',lw,'color',color1);
hold on;

axis([mintime maxtime minglorad maxglorad]);
%set(gca,'ytick',ytiks);
set(gca,'xtick',ticks);
%datetick('x',1,'keeplimits','keepticks');
%datetick('x','mmm','keeplimits');
set(gca,'xticklabel',[]);
set(gca,'fontsize',9);
title(['Lac ' lake ', daily global radiation (MJ/m^2)']);
ylabel('glorad (MJ/m^2)');
grid on;

%yletter=minuw+(maxuw-minuw)*ratio4letter;
%text(xletter,yletter,'(a)','fontsize',10);

%x1 = mintime+(0.009*(maxtime-mintime)/0.8);
%y1 = 0.85;

%txtlatlon = ['(a) ' num2str(lat1),' N, ',num2str(lon),' O'];
%text(x1, y1, txtlatlon,'fontsize',12,'color',color0);

%xx1 = mintime+(0.071*(maxtime-mintime)/0.8);
%xx2 = mintime+(0.136*(maxtime-mintime)/0.8);
%xx3 = mintime+(0.203*(maxtime-mintime)/0.8);
%yy1 = 0.60;

% *** subplot no.2 (clocov) ***********************************************
hold on;
%subplot(5,1,2)
axes('Position',[0.1,0.62,0.8,0.15],'Box','on');
plot(time,clocov,'linewidth',lw,'color',color1);
hold on;

axis([mintime maxtime minclocov maxclocov]);
%set(gca,'ytick',ytiks);
set(gca,'xtick',ticks);
%datetick('x',1,'keeplimits','keepticks');
%datetick('x','mmm','keeplimits');
set(gca,'xticklabel',[]);
set(gca,'fontsize',9);
title(['Lac ' lake ', daily cloud cover (0-1)']);
ylabel('clocov (0-1)');
%txtlatlon = ['(b) ' num2str(lat2),' N, ',num2str(lon),' O'];
%text(x1, y1, txtlatlon,'fontsize',12,'color',color0);
grid on;


% *** subplot no.3 (airtem) ***********************************************
hold on;
%subplot(5,1,3)
axes('Position',[0.1,0.43,0.8,0.15],'Box','on');
plot(time,airtem,'linewidth',lw,'color',color1);
hold on;

axis([mintime maxtime minairtem maxairtem]);
%set(gca,'ytick',ytiks);
set(gca,'xtick',ticks);
%datetick('x',1,'keeplimits','keepticks');
%datetick('x','mmm','keeplimits');
set(gca,'xticklabel',[]);
set(gca,'fontsize',9);
title(['Lac ' lake ', daily air temperature (°C)']);
ylabel('airtem (°C)');
%txtlatlon = ['(c) ' num2str(lat3),' N, ',num2str(lon),' O'];
%text(x1, y1, txtlatlon,'fontsize',12,'color',color0);
grid on;

% *** subplot no.4 (windsp) ***********************************************
hold on;
%subplot(5,1,4)
axes('Position',[0.1,0.24,0.8,0.15],'Box','on');
plot(time,windsp,'linewidth',lw,'color',color1);
hold on;

axis([mintime maxtime minwindsp maxwindsp]);
%set(gca,'ytick',ytiks);
set(gca,'xtick',ticks);
%datetick('x',1,'keeplimits','keepticks');
%datetick('x','mmm','keeplimits');
set(gca,'xticklabel',[]);
set(gca,'fontsize',9);
title(['Lac ' lake ', daily wind speed (m/s)']);
ylabel('windsp (m/s)');
%txtlatlon = ['(d) ' num2str(lat4),' N, ',num2str(lon),' O'];
%text(x1, y1, txtlatlon,'fontsize',12,'color',color0);
grid on;

% *** subplot no.5 (precip) ***********************************************
hold on;
%subplot(5,1,5)
axes('Position',[0.1,0.05,0.8,0.15],'Box','on');
plot(time,precip,'linewidth',lw,'color',color1);
hold on;

axis([mintime maxtime minprecip maxprecip]);
%set(gca,'ytick',ytiks);
set(gca,'xtick',ticks);
datetick('x','dd mmm yyyy','keeplimits','keepticks');
%datetick('x','mmm','keeplimits');
%datetick('x',1,'keeplimits','keepticks');
%%set(gca,'xticklabel',[]);
set(gca,'fontsize',9);
title(['Lac ' lake ', daily precipitations (mm/day)']);
ylabel('precip (mm/day)');
%txtlatlon = ['(e) ' num2str(lat5),' N, ',num2str(lon),' O'];
%text(x1, y1, txtlatlon,'fontsize',12,'color',color0);
grid on;

%***************