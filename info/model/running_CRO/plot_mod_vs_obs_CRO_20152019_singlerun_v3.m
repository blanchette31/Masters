% plot_mod_vs_obs
%function plot_mod_vs_obs_ETL_19801984_singlerun(figno,depobs1,depobs2,tt,Tzt,Kz_ak,Kz_ak_ice,Kz_N0,C_shelter,alb_melt_ice,alb_melt_snow,rms1,rms2,rmsavg)
function plot_mod_vs_obs_CRO_20152019_singlerun_v3(figno,m_start,m_stop,tt,Tzt,Kz_ak,Kz_ak_ice,Kz_N0,C_shelter,alb_melt_ice,alb_melt_snow)

clf;

% *** to be specified before runnning ***

obstemfile = 'CRO_obstemp_daily_alldepths_2016_2019.txt'; % file temperature observations
temdata = readmatrix(obstemfile,'NumHeaderLines',1); % temperature data

%dep1txt = '0.5 m';dep2txt = '2.5 m';dep3txt = '4.5 m';dep4txt = '7.5 m';dep5txt = '10.5 m';dep6txt = '13.5 m';
%dep1txt = '1.0 m';dep2txt = '2.0 m';dep3txt = '4.0 m';dep4txt = '6.0 m';dep5txt = '8.0 m';dep6txt = '10.0 m';
dep1txt = '0.5 m';dep2txt = '1.0 m';dep3txt = '4.5 m';dep4txt = '6.5 m';dep5txt = '8.5 m';dep6txt = '10.5 m';

mintime = [2015  5  1  0  0  0];
maxtime = [2019 12 31  0  0  0];
mintem =  0.0;
maxtem =  30.0;

% day start of run
%yystart = 2015; mmstart = 5; ddstart = 14;

x4dep = 0.008;
x4rmse = 0.12;
ytext2 = 15;

% **********
[nobs, ncol] = size(temdata);
%nobs
%ncol
[ndep, ndays] = size(Tzt);
indexnan = nan(ndays,1); % all simulated days are nan

m_start
for i = 1:nobs
    dateday = datenum(temdata(i,1), temdata(i,2), temdata(i,3));
    %indexday = dateday - datenum(m_yystart,mmstart,ddstart);
    indexday = dateday - datenum(m_start(1),m_start(2),m_start(3));
    indexnan(indexday) = 1;
end
%size(indexnan)
indexnan = isnan(indexnan);
%size(indexnan)

% **********

tem0p5m = (temdata(:,5) + temdata(:,6)) / 2;  % temp. at 0.5 m
tem1p0m = temdata(:,7);  % temp. at 1.0 m
tem4p5m = temdata(:,9);
tem6p5m = temdata(:,11);
tem8p5m = temdata(:,13);
tem10p5m = temdata(:,14);

ndays = max( size(temdata) ); % convert dates to serial dates
dates2 = zeros(1,ndays);
for i=1:ndays
    dates2(i) = datenum(temdata(i,1),temdata(i,2),temdata(i,3));
end
dates2 = dates2';

mintime = datenum(mintime);
maxtime = datenum(maxtime);

color1 = [67 110 238]/255;      % royalblue2
color2 = [0 0 128]/255;         % navy

color3 = [180 82 205]/255;      % mediumorchid3
color4 = [0 205 102]/255;       % springgreen2
%color4 = [67 110 238]/255;      % royalblue2
color5 = [162 205 90]/255;      % darkolivegreen3
color6 = [255 215 0]/255;       % gold1
color7 = [255 140 0]/255;       % darkorange
color8 = [220 20 60]/255;       % crimson

year = 1979;
for k = 1:2:100
    year = year + 1;
    ticks(k)=datenum(year,1,1,0,0,0);
    ticks(k+1)=datenum(year,7,1,0,0,0);
end

w4black = 1.5;
w4blue = 2.0;

ticks_y = [0 10 20 30];

mod_temp = Tzt;

% *** subplot no.1 - temperature at 0.5 m *********************************
subplot(6,1,1)
%axes('Position',[0.1,0.5,0.8,0.3],'Box','on');
%hold on;
%plot(dates2,tem0p5m,'color',color2,'linewidth',w4blue);
plot(dates2,tem0p5m,'o','markerfacecolor',color2,'markeredgecolor',color2,'markersize',1.5);
hold on;
plot(tt,mod_temp(1,:),'color',color4,'linewidth',w4blue);
%plot(tt,(mod_temp(1,:)+mod_temp(2,:))/2,'color',color4,'linewidth',w4blue);
hold on;
%size(dates2)
%size(tem02m)

axis([mintime maxtime mintem maxtem]);
box on;
set(gca,'ytick',ticks_y);
set(gca,'xtick',ticks);
datetick('x',1,'keeplimits','keepticks');
set(gca,'xticklabel',[]);
set(gca,'fontsize',10);
ylabel('T (°C)');
%grid on;
set(gca,'ygrid','on');
%xtext = datenum(2011,5,1,0,0,0);
xtext = mintime + (maxtime-mintime)*x4dep;
ytext = 15;
text(xtext,ytext,dep1txt,'fontsize',12,'color',color2);

dataobs = tem0p5m;
%'flag1'
%size(dataobs)
datamod = mod_temp(1,:); datamod = datamod';
%'flag2'
%size(datamod)
datamod(indexnan) = [];
%'flag3'
%size(datamod)

rmse_obsmod = sqrt(immse(dataobs,datamod));
disp(' ');
disp(['root-mean-square error at 0.5 m ' num2str(rmse_obsmod)]);

xtext2 = mintime + (maxtime-mintime)* x4rmse;
%ytext2 = 6;
textrsme = ['rmse = ' num2str(rmse_obsmod,'%4.2f')];
text(xtext2,ytext2,textrsme,'fontsize',10,'color','k');

% *** subplot no.2 - temperature at 1.0 m *********************************
subplot(6,1,2)

%plot(dates2,tem1p0m,'color',color2,'linewidth',w4blue);
plot(dates2,tem1p0m,'o','markerfacecolor',color2,'markeredgecolor',color2,'markersize',1.5);
hold on;
%plot(tt,mod_temp(4,:),'color',color4,'linewidth',w4blue);
plot(tt,(mod_temp(1,:)+mod_temp(2,:))/2,'color',color4,'linewidth',w4blue);
hold on;

axis([mintime maxtime mintem maxtem]);
box on;
set(gca,'ytick',ticks_y);
set(gca,'xtick',ticks);
datetick('x',1,'keeplimits','keepticks');
set(gca,'xticklabel',[]);
set(gca,'fontsize',10);
ylabel('T (°C)');
%grid on;
set(gca,'ygrid','on');
%xtext = datenum(2011,5,1,0,0,0);
xtext = mintime + (maxtime-mintime)*x4dep;
ytext = 15;
text(xtext,ytext,dep2txt,'fontsize',12,'color',color2);

dataobs = tem1p0m;
datamod = (mod_temp(1,:)+mod_temp(2,:))/2; datamod = datamod';
datamod(indexnan) = [];

rmse_obsmod = sqrt(immse(dataobs,datamod));
disp(' ');
disp(['root-mean-square error at 1.0 m ' num2str(rmse_obsmod)]);

xtext2 = mintime + (maxtime-mintime)* x4rmse;
%ytext2 = 6;
textrsme = ['rmse = ' num2str(rmse_obsmod,'%4.2f')];
text(xtext2,ytext2,textrsme,'fontsize',10,'color','k');

% *** subplot no.3 - temperature at 4.5 m *********************************
subplot(6,1,3)

%plot(dates2,tem4p5m,'color',color2,'linewidth',w4blue);
plot(dates2,tem4p5m,'o','markerfacecolor',color2,'markeredgecolor',color2,'markersize',1.5);
hold on;
plot(tt,mod_temp(5,:),'color',color4,'linewidth',w4blue);
%plot(tt,(mod_temp(4,:)+mod_temp(5,:))/2,'color',color4,'linewidth',w4blue);
hold on;

axis([mintime maxtime mintem maxtem]);
box on;
set(gca,'ytick',ticks_y);
set(gca,'xtick',ticks);
datetick('x',1,'keeplimits','keepticks');
set(gca,'xticklabel',[]);
set(gca,'fontsize',10);
ylabel('T (°C)');
%grid on;
set(gca,'ygrid','on');
%xtext = datenum(2011,5,1,0,0,0);
xtext = mintime + (maxtime-mintime)*x4dep;
ytext = 15;
text(xtext,ytext,dep3txt,'fontsize',12,'color',color2);

dataobs = tem4p5m;
datamod = mod_temp(5,:); datamod = datamod';
datamod(indexnan) = [];

rmse_obsmod = sqrt(immse(dataobs,datamod));
disp(' ');
disp(['root-mean-square error at 4.5 m ' num2str(rmse_obsmod)]);

xtext2 = mintime + (maxtime-mintime)* x4rmse;
%ytext2 = 6;
textrsme = ['rmse = ' num2str(rmse_obsmod,'%4.2f')];
text(xtext2,ytext2,textrsme,'fontsize',10,'color','k');

% *** subplot no.4 - temperature at 6.5 m *********************************
subplot(6,1,4)

%plot(dates2,tem6p5m,'color',color2,'linewidth',w4blue);
plot(dates2,tem6p5m,'o','markerfacecolor',color2,'markeredgecolor',color2,'markersize',1.5);
hold on;
plot(tt,mod_temp(7,:),'color',color4,'linewidth',w4blue);
%plot(tt,(mod_temp(6,:)+mod_temp(7,:))/2,'color',color4,'linewidth',w4blue);
hold on;

axis([mintime maxtime mintem maxtem]);
box on;
set(gca,'ytick',ticks_y);
set(gca,'xtick',ticks);
datetick('x',1,'keeplimits','keepticks');
set(gca,'xticklabel',[]);
set(gca,'fontsize',10);
ylabel('T (°C)');
%grid on;
set(gca,'ygrid','on');
%xtext = datenum(2011,5,1,0,0,0);
xtext = mintime + (maxtime-mintime)*x4dep;
ytext = 15;
text(xtext,ytext,dep4txt,'fontsize',12,'color',color2);

dataobs = tem6p5m;
datamod = mod_temp(7,:); datamod = datamod';
datamod(indexnan) = [];

rmse_obsmod = sqrt(immse(dataobs,datamod));
disp(' ');
disp(['root-mean-square error at 6.5 m ' num2str(rmse_obsmod)]);

xtext2 = mintime + (maxtime-mintime)* x4rmse;
%ytext2 = 6;
textrsme = ['rmse = ' num2str(rmse_obsmod,'%4.2f')];
text(xtext2,ytext2,textrsme,'fontsize',10,'color','k');

% *** subplot no.5 - temperature at 8.5 m ********************************
subplot(6,1,5)

%plot(dates2,tem8p5m,'color',color2,'linewidth',w4blue);
plot(dates2,tem8p5m,'o','markerfacecolor',color2,'markeredgecolor',color2,'markersize',1.5);
hold on;
plot(tt,mod_temp(9,:),'color',color4,'linewidth',w4blue);
%plot(tt,(mod_temp(8,:)+mod_temp(9,:))/2,'color',color4,'linewidth',w4blue);
hold on;

axis([mintime maxtime mintem maxtem]);
box on;
set(gca,'ytick',ticks_y);
set(gca,'xtick',ticks);
datetick('x',1,'keeplimits','keepticks');
set(gca,'xticklabel',[]);
set(gca,'fontsize',10);
ylabel('T (°C)');
%grid on;
set(gca,'ygrid','on');
%xtext = datenum(2011,5,1,0,0,0);
xtext = mintime + (maxtime-mintime)*x4dep;
ytext = 15;
text(xtext,ytext,dep5txt,'fontsize',12,'color',color2);

dataobs = tem8p5m;
datamod = mod_temp(9,:); datamod = datamod';
datamod(indexnan) = [];

rmse_obsmod = sqrt(immse(dataobs,datamod));
disp(' ');
disp(['root-mean-square error at 8.5 m ' num2str(rmse_obsmod)]);

xtext2 = mintime + (maxtime-mintime)* x4rmse;
%ytext2 = 6;
textrsme = ['rmse = ' num2str(rmse_obsmod,'%4.2f')];
text(xtext2,ytext2,textrsme,'fontsize',10,'color','k');

% *** subplot no.6 - temperature at 10.5 m ********************************
subplot(6,1,6)

%plot(dates2,tem10p5m,'color',color2,'linewidth',w4blue);
plot(dates2,tem10p5m,'o','markerfacecolor',color2,'markeredgecolor',color2,'markersize',1.5);
hold on;
plot(tt,mod_temp(11,:),'color',color4,'linewidth',w4blue);
%plot(tt,(mod_temp(10,:)+mod_temp(11,:))/2,'color',color4,'linewidth',w4blue);
hold on;

axis([mintime maxtime mintem maxtem]);
box on;
set(gca,'ytick',ticks_y);
set(gca,'xtick',ticks);
datetick('x',1,'keeplimits','keepticks');
%set(gca,'xticklabel',[]);
set(gca,'fontsize',10);
ylabel('T (°C)');
%grid on;
set(gca,'ygrid','on');
%xtext = datenum(2011,5,1,0,0,0);
xtext = mintime + (maxtime-mintime)*x4dep;
ytext = 15;
text(xtext,ytext,dep6txt,'fontsize',12,'color',color2);

dataobs = tem10p5m;
datamod = mod_temp(11,:); datamod = datamod';
datamod(indexnan) = [];

rmse_obsmod = sqrt(immse(dataobs,datamod));
disp(' ');
disp(['root-mean-square error at 10.5 m ' num2str(rmse_obsmod)]);

xtext2 = mintime + (maxtime-mintime)* x4rmse;
%ytext2 = 6;
textrsme = ['rmse = ' num2str(rmse_obsmod,'%4.2f')];
text(xtext2,ytext2,textrsme,'fontsize',10,'color','k');

%***************

ytext = -15;
text_para1 = ['Kz ak ',num2str(Kz_ak),'     Kz ak ice ',num2str(Kz_ak_ice),'     Kz N0 ',num2str(Kz_N0), ...
    '     C shelter ',num2str(C_shelter), '     alb melt ice ',num2str(alb_melt_ice),'     alb melt snow ',num2str(alb_melt_snow)];
text(xtext,ytext,text_para1);

%figname = ['fig_',num2str(figno),'.jpg'];
%print ('-djpeg100',figname);

%***************




