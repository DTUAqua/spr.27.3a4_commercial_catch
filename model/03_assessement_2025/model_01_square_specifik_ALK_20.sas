/**/
libname in 'C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\data';
libname out 'C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\model';

proc format;

   VALUE $ibts
  "31F1"="5"
  "31F2" -"31F3"="6"
  "32F0" -"32F1"="5"
  "32F2" -"32F3"="6"
  "33F0" -"33F2"="5"
  "33F3" -"33F5"="6"
  "34F0" -"34F2"="5"
  "34F3" -"34F5"="6"
  "35F0" -"35F2"="5"
  "35F3" -"35F7"="6"
  "36E8" -"36F0"="4"
  "36F1" -"36F2"="5"
  "36F3" -"36F9"="6"
  "37E9" -"37F1"="4"
  "37F2" -"37F8"="6"
  "38E8" -"38F1"="4"
  "38F2" -"38F8"="6"
  "39E8" -"39F0"="4"
  "39F1" -"39F2"="2"
  "39F3" -"39F8"="6"
  "40E7" -"40E9"="4"
  "40F0" -"40F4"="2"
  "40F5" -"40F8"="7"
  "41E6" -"41E9"="3"
  "41F0" -"41F4"="2"
  "41F5" -"41F8"="7"
  "41G0" -"41G2"="9"
  "42E6" -"42E9"="3"
  "42F0" -"42F4"="2"
  "42F5" -"42F8"="7"
  "42G0" -"42G2"="9"
  "43E7" -"43E9"="3"
  "43F0" -"43F4"="2"
  "43F8" -"43F9"="8"
  "43F5" -"43F7"="7"
  "43G0" -"43G2"="9"
  "44E6" -"44E9"="3"
  "44F0" -"44F1"="1"
  "44F2" -"44F4"="2"
  "44F5"        ="7"
  "44F8" -"44G1"="8"
  "45E6" -"45E9"="3"
  "45F0" -"45F4"="1"
  "45F8" -"45G1"="8"
  "46E6" -"46E9"="3"
  "46F0" -"46F3"="1"
  "46F9" -"46G1"="8"
  "47E6" -"47E7"="3"
  "47E8" -"47F3"="1"
  "48E6" -"48F3"="1"
  "49E6" -"49F3"="1"
  "50E6" -"50F3"="1"
  "51E6" -"51F3"="1"
  "52E6" -"52F3"="1"

  "9999"="MANGLER"
  other='OUT'
  ;
run;

data no1;
set in.norwegian_alk_2024;
do age=0,1,2,3,4;
output;
end;
run;

data no2;
set no1;
sum=noage0+noage1+noage2+noage3+noage4_;
if sum=0 then delete;
if age=0 then number=noage0;
if age=1 then number=noage1;
if age=2 then number=noage2;
if age=3 then number=noage3;
if age=4 then number=noage4_;
day=date-100*floor(date/100);
month=(date-10000*floor(date/10000)-day)/100;
year=floor(date/10000)+2000;
sampleid=date*100000+compress(station, "SWE");
if sampleid = . then sampleid=date*100000+compress(station, "FO");
*sampleid = strip(station)||strip(date)||strip(country);
stat='             ';
length=scm*5;
statisticalrectangle=icessq;
stat=station;
country='NOR';
speciescode = 'BRS';

drop station date icessq sum;
run;

data in1;
set in.age_including_survey no2;
n0=0;
n1=0;
n2=0;
n3=0;
n4=0;

if speciescode ne 'BRS' then delete;

if country ne 'NOR' then month=month(date);

if  country ne 'NOR' then day=day(dategearstart);

if station='' then station=stat;

if month le 3 and age=0 then age=1;  **0-årige i jan-mar omdøbes til 1-årige*;

if age=0 then n0=number;
if age=1 then n1=number;
if age=2 then n2=number;
if age=3 then n3=number;
if age ge 4 then n4=number;

length=0.5*floor(length*0.2);
if length gt 20 then delete;
if length lt 3 then delete;
intsq='    ';
intsq=statisticalrectangle;

roundfish=put(intsq,$ibts.);
if roundfish in ('OUT','','MANGLER') then delete;

area3='    ';
area3='IV';
*if roundfish in ('8','9') then area3='IIIa';
if intsq in ('45F7', '45F8', '46F8', '46F9', '47F9', '46G0', '47G0', '48G0', '47G1') then area3='NO';
lat=substr(intsq,1,2)/2+35.75;
x1=substr(intsq,3,1);
x2=substr(intsq,4,1);
if x1='E' then lon=-(10-x2)+0.5;
if x1='F' then lon=x2+0.5;
if x1='G' then lon=10+x2+0.5;

lat=floor(lat);
lon=2*floor(lon/2);

latlon=lat*100+lon;

area=latlon;
area2=200*floor(lat/2)+4*floor(lon/4);
quarter=floor((month-1)/3)+1;
run;

proc sort data=in1 out=l2 nodupkey;
by year quarter area3 area sampleid;
run;

proc summary data=l2;
var sampleid;
by year quarter area3 area;
output out=l4 (drop=_type_ _freq_) n()=n_samples;
run;

data out.age_samples;
set l4;
run;

proc summary data=out.age_samples;
var n_samples;
by year;
output out=a2 sum()=n_samples;
run;

proc gplot data=a2;
plot n_samples*year;
run;

proc sort data=in1;
 by year quarter area3 country  lat lon agereadid length;
 run;

 proc summary data=in1;
 var n0-n4;
 by year quarter area3 country  lat lon agereadid length;
 output out=in2 sum()=;
run;

data in4;
set in2;
do age=0,1,2,3,4;
output;
end;
run;

data in4a;
set in4;
do length=1 to 30 by 0.5;  
output;
end;
run;

data in4b;
set in4a;
n0=0;
n1=0;
n2=0;
n3=0;
n4=0;
run;

data in5;
set in4 in4b;

sum=n0+n1+n2+n3+n4;
p0=n0/sum;
p1=n1/sum;
p2=n2/sum;
p3=n3/sum;
p4=n4/sum;

if age=0 then n=n0;
if age=1 then n=n1;
if age=2 then n=n2;
if age=3 then n=n3;
if age=4 then n=n4;

s0=0;
s1=0;
s2=0;
s3=0;
s4=0;

if age=0 then s0=n0+n1+n2+n3+n4; *sx angiver antallet af fisk der er alder x og ældre;
if age=1 then s1=n1+n2+n3+n4;
if age=2 then s2=n2+n3+n4;
if age=3 then s3=n3+n4;
if age=4 then s4=n4;

if age ne 0 then n0=0; 
if age ne 1 then n1=0; 
if age ne 2 then n2=0; 
if age ne 3 then n3=0; 
if age ne 4 then n4=0; 

if age=0 then p=p0;
if age=1 then p=p1;
if age=2 then p=p2;
if age=3 then p=p3;
if age=4 then p=p4;

if age=0 then logit0=log(p/(1-p));
if age=1 then logit1=log(p/(p2-p3-p4));
if age=2 then logit2=log(p/(p3-p4));
if age=3 then logit3=log(p/(p4));

dec=10*floor(year/10);
scm=length;


latlon=lat*100+lon;

area=latlon;
area2=200*floor(lat/2)+4*floor(lon/4);

if year lt 2023 or year gt 2025 then delete; *******Opdater;

run;

proc sort data=in5;
by year quarter area3 latlon age length;
run;
/*
proc gplot data=in5;
plot  (logit0-logit3)*length=quarter;
by year ;
symbol1 v=plus i=r c=red;
symbol2 v=triangle i=r c=red;
symbol3 v=square i=r c=red;
symbol4 v=plus i=r c=blue;
symbol5 v=triangle i=r c=blue;
symbol6 v=square i=r c=blue;
symbol7 v=plus i=r c=green;
symbol8 v=triangle i=r c=green;
symbol9 v=square i=r c=green;
symbol10 v=plus i=r c=black;
symbol11 v=triangle i=r c=black;
symbol12 v=square i=r c=black;

run;
*/
proc sort data=in5;
by year quarter area3 area area2  scm;
run;


proc summary data=in5; 
var n0-n4 s0-s4;
by year quarter area3 area area2  scm;
output out=out.in5 sum()=;
run;

*******  Det følgende program kører på flere niveauer (se note-dokument)  ****;

%inc 'C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\model_scripts\alk_level1.sas';
%inc 'C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\model_scripts\alk_level2.sas';
%inc 'C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\model_scripts\alk_level3.sas';
%inc 'C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\model_scripts\alk_level4.sas';
%inc 'C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\model_scripts\alk_level5.sas';

data l1;
set in.length;

month=month(date);
day=day(date);
intsq='    ';
intsq=statisticalrectangle;

roundfish=put(intsq,$ibts.);
if roundfish in ('OUT','','MANGLER') then delete;

if speciescode ne 'BRS' then delete;

area3='    ';
area3='IV';
*if roundfish in ('8','9') then area3='IIIa';
if intsq in ('45F7', '45F8', '46F8', '46F9', '47F9', '46G0', '47G0', '48G0', '47G1') then area3='NO';

lat=substr(intsq,1,2)/2+35.75;
x1=substr(intsq,3,1);
x2=substr(intsq,4,1);
if x1='E' then lon=-(10-x2)+0.5;
if x1='F' then lon=x2+0.5;
if x1='G' then lon=10+x2+0.5;

lat=floor(lat);
lon=2*floor(lon/2);

quarter=floor((month-1)/3)+1;

latlon=lat*100+lon;
scm=length;

area=latlon;
area2=200*floor(lat/2)+4*floor(lon/4);
run;

*****************Til square specifik alk med alle squares****************;
proc sort data=l1 (keep=intsq area area2 area3) out=l3b nodupkey;
by intsq area area2 area3;
run;

data l3c;
set l3b;
do year=2023 to 2025 by 1; ****** Opdater;
output;
end;
run;

data l3e;
set l3c;
do quarter=1,2,3,4;
output;
end;
run;

data l3f;
set l3e;
do age=0 to 3 by 1;
output;
end;
run;

data l3g;
set l3f;
do scm=2 to 30 by 0.5;
output;
end;
run;

data l4;
set l3g;
hy=floor((quarter-1)/2)+1;
run;

***Nu tildeles en ALK fra det lavest mulige niveau****;

proc sort data=l4;
by year quarter area3 area age scm;
run;

data l83a;
set out.alk_level1;
drop area2;
run;

proc sort data=l83a out=l83 (drop=pi level);
by year quarter area3 area age scm;
run;

data l93;
merge l4 l83;
by year quarter area3 area age scm;
run;


data l103;
set l93;

level=1;
if age ne 0 and pred ne . and maxscm ne . and maxscm lt 18 and scm gt maxscm+2 then pred=0;
if pred ne . and maxscm ne . and maxscm=19 and scm gt maxscm+3 then pred=0;
if pred ne . and maxscm ne . and maxscm gt 19 and scm gt maxscm+4 then pred=0;
if pred ne . and minscm ne . and minscm lt 20 and scm lt minscm-2 then pred=1;
if pred ne . and minscm ne . and minscm=22 and scm lt minscm-3 then pred=1;
if pred ne . and minscm ne . and minscm ge 24 and scm lt minscm-4 then pred=1;
pi=pred;
if pi=. then level=.;
drop pred n s maxscm minscm lower upper minp maxp prange;
run;

proc sort data=l103;
by year quarter area3 area2  age scm;
run;

data l84a;
set out.alk_level2;
run;

proc sort data=l84a out=l84 (drop=pi level);
by year quarter area3 area2 age scm;
run;

data l94;
merge l103 l84;
by year quarter area3 area2  age scm;
run;

data l104;
set l94;
if pi=. and level=. then level=2;
if pred ne . and maxscm ne . and maxscm lt 18 and scm gt maxscm+2 then pred=0;
if pred ne . and maxscm ne . and maxscm=19 and scm gt maxscm+3 then pred=0;
if pred ne . and maxscm ne . and maxscm gt 19 and scm gt maxscm+4 then pred=0;
if pred ne . and minscm ne . and minscm lt 20 and scm lt minscm-2 then pred=1;
if pred ne . and minscm ne . and minscm=22 and scm lt minscm-3 then pred=1;
if pred ne . and minscm ne . and minscm ge 24 and scm lt minscm-4 then pred=1;
if pi=. then pi=pred;
if pi=. then level=.;
drop  pred n s maxscm minscm lower upper minp maxp prange;
run;

proc sort data=l104;
by year quarter area3 age scm;
run;

data l85a;
set out.alk_level3;
run;

proc sort data=l85a out=l85 (drop=pi level);
by year quarter area3 age scm;
run;

data l95;
merge l104 l85;
by year quarter area3 age scm;
run;

data l105;
set l95;
if pi=. and level=. then level=3;
if pred ne . and maxscm ne . and maxscm lt 18 and scm gt maxscm+2 then pred=0;
if pred ne . and maxscm ne . and maxscm=19 and scm gt maxscm+3 then pred=0;
if pred ne . and maxscm ne . and maxscm gt 19 and scm gt maxscm+4 then pred=0;
if pred ne . and minscm ne . and minscm lt 20 and scm lt minscm-2 then pred=1;
if pred ne . and minscm ne . and minscm=22 and scm lt minscm-3 then pred=1;
if pred ne . and minscm ne . and minscm ge 24 and scm lt minscm-4 then pred=1;
if pi=. then pi=pred;
if pi=. then level=.;
drop pred n s maxscm minscm lower upper minp maxp prange;
run;

data l105b;
set l105;
if level=. then delete;
run;

proc sort data=l105;
by year area3 hy age scm;
run;

proc sort data=out.alk_level4 out=l86 (drop=pi level);
by year area3 hy age scm;
run;

data l96;
merge l105 l86;
by year area3 hy age scm;
run;

data l106;
set l96;

if pi=. and level=. then level=4;
if pred ne . and maxscm ne . and maxscm lt 18 and scm gt maxscm+2 then pred=0;
if pred ne . and maxscm ne . and maxscm=19 and scm gt maxscm+3 then pred=0;
if pred ne . and maxscm ne . and maxscm gt 19 and scm gt maxscm+4 then pred=0;
if pred ne . and minscm ne . and minscm lt 20 and scm lt minscm-2 then pred=1;
if pred ne . and minscm ne . and minscm=22 and scm lt minscm-3 then pred=1;
if pred ne . and minscm ne . and minscm ge 24 and scm lt minscm-4 then pred=1;
if pi=. then pi=pred;
if pi=. then level=.;
drop  pred n s maxscm minscm lower upper minp maxp prange;
run;

data l106b;
set l106;
if level=. then delete;
run;


proc sort data=l106;
by year area3 age scm;
run;

proc sort data=out.alk_level5 out=l87 (drop=pi level);
by year area3 age scm;
run;

data l97;
merge l106 l87;
by year area3 age scm;
run;

data l110;
set l97;

if pi=. and level=. then level=5;
if pred ne . and maxscm ne . and maxscm lt 18 and scm gt maxscm+2 then pred=0;
if pred ne . and maxscm ne . and maxscm=19 and scm gt maxscm+3 then pred=0;
if pred ne . and maxscm ne . and maxscm gt 19 and scm gt maxscm+4 then pred=0;
if pred ne . and minscm ne . and minscm lt 20 and scm lt minscm-2 then pred=1;
if pred ne . and minscm ne . and minscm=22 and scm lt minscm-3 then pred=1;
if pred ne . and minscm ne . and minscm ge 24 and scm lt minscm-4 then pred=1;
if quarter in (1,2) and age=0 then pred=0;
if pi=. then pi=pred;
if pi=. then level=.;
if pi=. then pi=1;
drop pred n s maxscm minscm lower upper minp maxp prange;
run;


data l11;
set l110;
if age=0 then pi0=pi;
if age=1 then pi1=pi;
if age=2 then pi2=pi;
if age=3 then pi3=pi;
if age=0 then l0=level;
if age=1 then l1=level;
if age=2 then l2=level;
if age=3 then l3=level;
run;

proc sort data=l11;
by year quarter area3 area intsq scm;
run;

proc summary data=l11;
var pi0-pi3 l0-l3;
by year quarter area3 area intsq scm;
output out=l12 max=;
run;

*******  Unconditioned probabilities of being age 1, age 2, 3 and age 4+   ***********
*******  are calculated and plotted                                        ***********;

data l12b;
set l12;
do age=' 0',' 1',' 2',' 3',' 4';
output;
end;
run;

data l13;
set l12b;
if intsq='' then delete;

if pi3=. then pi3=1;

if age=' 0' then p=pi0;
if age=' 1' then p=(1-pi0)*pi1;
if age=' 2' then p=(1-pi0-(1-pi0)*pi1)*pi2;
if age=' 3' then p=(1-pi0-(1-pi0)*pi1-(1-pi0-(1-pi0)*pi1)*pi2)*pi3;
if age=' 4' then p=(1-pi0-(1-pi0)*pi1-(1-pi0-(1-pi0)*pi1)*pi2-(1-pi0-(1-pi0)*pi1-(1-pi0-(1-pi0)*pi1)*pi2)*pi3);
keep year area3 area intsq quarter  age p scm l0-l3 pi0-pi3;
run;

data l14;
set l13;
*if 1*month lt 7 then halfyear=1;
*if 1*month ge 7 then halfyear=2;

if age=' 0' then p0=p;
if age=' 1' then p1=p;
if age=' 2' then p2=p;
if age=' 3' then p3=p;
if age=' 4' then p4=p;
*if aar ne 1976 and aar ne 1977 then delete;
*if PP_ar_tx in ('SH','','999') then delete;
run;

proc sort data=l14;
by year quarter area3 area intsq scm;
run;

proc summary data=l14;
var p0-p4 pi0-pi3 l0-l3;
by year quarter area3 area intsq scm;
output out=l15 max=;
run;

data l16;
set in.alk23_intsq;
if year gt 2022 then delete;
run;

data out.alk24_intsq;
set l15 l16;
if intsq='' then delete;
drop _type_ _freq_;
run;

title '';

data l14a;
set l14;
*if year lt 2016 then delete;
if area3='NO' then delete;
*if quarter ne 1 then delete;
run;

proc sort data=l14a;
by year quarter area3 area intsq scm;
run;

proc gplot data=l14a;
plot p*scm=age/overlay haxis=axis1 vaxis=axis1;
by year quarter area3;
symbol1 v=plus value=star     height=0.3 cm i=join l=1 c=black;
symbol2 v=none value=circle   height=0.3 cm i=join l=1 c=red;
symbol3 v=none value=triangle height=0.3 cm i=join l=1 c=blue;
symbol4 v=none value=square   height=0.3 cm i=join l=1 c=orange;
symbol5 v=none value=plus     height=0.3 cm i=join l=1 c=purple;
symbol6 v=none value=triangle height=0.3 cm i=join l=1 c=yellow;
symbol7 v=none value=square   height=0.3 cm i=join l=1 c=green;
symbol8 v=none value=plus     height=0.3 cm i=join l=1 c=brown;
*title1 'plot of aglg19 - plot p*scm=age/overlay haxis=axis1 vaxis=axis1';
*title2 'by cruise surv_loc_new';

run;
quit;


*****************************************************************************************;
**************************************ALK slut ******************************************;
*****************************************************************************************;
