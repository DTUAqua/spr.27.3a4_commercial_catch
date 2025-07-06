/**/
libname in 'C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\data';
libname out 'C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\model';

%let path = C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\model;

*libname dis 'c:\ar\sas\coddist.'; *Not used;

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

************Der er to input data sæt: havret tilbage til 1989 og logbog fra 1982-1988. ************
************Før 1982 bruges gennesmnitlig fordeling 1982-1988*******************************'******;

data a01;
set in.Dk_spr_catch_82_88;
aar=year+1900;
kv=quarter;
if aar=1989 then delete;
ton=brs_ton;
sq=square;
run;

proc sort data=a01;
by kv sq;
run;

proc summary data=a01;
var ton;
by kv sq;
output out=a02 (drop=_type_ _freq_) mean()=;
run;

data a0;
set a02;
do aar=1963 to 1981 by 1;
output;
end;
run;

data a1;
set in.Dk_spr_catch_82_88;
aar=year+1900;
kv=quarter;
if aar=1989 then delete;
ton=brs_ton;
sq=square;
run;

data a2a1;
set in.havr89_og_frem_alle_arter;
if art not in ('BRS') then delete;
if aar lt 2012 then delete;
if aar gt 2013 then delete;
run;

data a2a2;
set in.dk_spr_catch_89_11;
if aar ge 2012 then delete;
run;

data a2a3;
set in.Havr89_og_frem_alle_arter;
if art not in ('BRS') then delete;
if aar lt 2014 then delete;
run;

data a2a;
set a2a3 a2a2 a2a1 a1 a0;

year=aar;
quarter=kv;
intsq='    ';
intsq=sq;
roundfish=put(sq,$ibts.);
if roundfish in ('OUT','','MANGLER') then delete;
div='    ';
div='IV';
if roundfish in ('8','9') then div='IIIa';

if year gt 2018 then delete;

keep year quarter intsq ton div;
run;

proc sort data=a2a;
by year quarter div;
run;

proc summary data=a2a;
var ton;
by year quarter div ;
output out=t4x (drop=_type_ _freq_) sum()=;
run;

/*
proc export data=a2a
   outfile='c:\ar\sprat\age_distributions\danish_catches.csv'
   dbms=csv 
   replace;
run;
quit;
*/

data a2b;
set in.catch_square_2002_2025;
intsq='    ';
intsq=square;
ton=catch_in_ton;
if country in ('DEN','DK') and year lt 2019 then delete;

if intsq = '31F0' and year ge 2023 then intsq = '31F1';

****************REMOVE IN 2024************************;
****************Temporary fix for low catches and no samples**********;
if year=2025 and quarter=1 then do;
	quarter=4; year=2024;
	end;
run;

data a2c;
set a2a a2b;
if quarter=2 then quarter=3;
run;

proc sort data=a2c;
by year quarter intsq;
run;

proc summary data=a2c;
var ton;
by year quarter intsq;
output out=a2 (drop=_type_ _freq_) sum()=;
run;

proc sort data=a2;
by year quarter intsq;
run;

proc sort data=out.mean_weight_and_n_per_kg_2024 out=a3;
by year quarter intsq;
run;

/*
data a3;
set a3;

output;

if year = 2021 and quarter = 4 then do;
	year = 2022; quarter = 1; hy = 1; mark = 'Imputed'; output;
end;

run;
*/

proc sort data = a3;
by year quarter intsq;
run;

data a4;
merge a2 a3;
by year quarter intsq;
run;

proc sql;
create table out.y_q_sq as
select distinct year, quarter, intsq, ton, area3, area2, n0_per_kg
from a4;


proc sort data=a4 out=x4;
by quarter year;
run;

proc corr data=x4 outp=x5;
var n_samples ton;
by quarter year;
run;

proc summary data=x4;
var ton n_samples;
by quarter year;
output out=x4a sum()=;
run;

*proc export data=x4a
   outfile='C:\ar\sprat\age_distributions\quarterly_catches.csv'
   dbms=csv 
   replace;
*run;
*quit;

data x6;
set x5;
if _type_ ne 'CORR' then delete;
if _name_ ne 'ton' then delete;
if year lt 1991 then delete;
run;

proc corr data=x6;
var year n_samples;
by quarter;
run;

proc summary data=x6;
var n_samples;
by quarter;
output out=x7 (drop=_type_ _freq_) p90()=p90 p10()=p10 mean()=mean max()=max min()=min;
run;

*proc export data=x7
   outfile='C:\ar\sprat\age_distributions\correlation_n_samples_ton.csv'
   dbms=csv 
   replace;
*run;
*quit;

proc gplot data=x6;
plot n_samples*year=quarter;
symbol1 v=plus i=join;
symbol2 v=plus i=join;
symbol3 v=plus i=join;
symbol4 v=plus i=join;
run;


data a2d;
set a2;

roundfish=put(intsq,$ibts.);

div='    ';
div='IV';
*if roundfish in ('8','9') then div='IIIa';

if intsq in ('45F7', '45F8', '46F8', '46F9', '47F9', '46G0', '47G0', '48G0', '47G1') 
then div='NO';

run;

proc sort data=a2d;
by year div roundfish;
run;

proc summary data=a2d;
var ton;
by year div;
output out=t4 (drop=_type_ _freq_) sum()=;
run;

data t5;
set in.sms_ns_2011;
if species ne 'Sprat' then delete;
sms_ton=yield__sop_;
if year gt 1964 then delete;
run;

proc sort data=t5;
by year;
run;

proc summary data=t5;
var sms_ton;
by year;
output out=t6 (drop=_type_ _freq_) sum()=IV;
run;

data t7a;
set t6 in.ices_catch ;
if year lt 1965 then IV=IV/1000;
if year ge 1966 then IV=IV+IIIa;
run;

data t7b;
merge t7a t4;
by year;
run;


***************UPDATE HERE AND UNQUOTE*****************************;


data t8a;
set t7b;
if year=2012 then IV=85.564+10.416;

if year=2013 then IV=60.934+3.75;

if year=2014 then IV=140.384+18.58;

if year=2015 then IV=290.380+13.27;

if year=2016 then IV=240.673+8.204;

if year=2017 then IV=128.660+1.418;

if year=2018 then IV=187.216+3.969;

if year=2019 then IV=23.386+123.082;

*if year=2020 then IV=179.843+0.478; *2021;
if year=2020 then IV=182.654;
if year=2021 then IV=80.761;
if year=2022 then IV=89.721+0.384; *OK;

if year=2023 then IV=91.420; *2025 - updated and only 2023 landings;

if year=2024 then IV=84.970 + 0.002; *2025 - updated 2024+2025 landings;

if year=2025 then IV=0; *2025 run - moved to Q4 2024;


*if year=2021 then IV=0.478;

****************REMOVE 478 t from 2020 IN 2022************************;
****************Temporary fix for low catches and no samples**********;

if div='IV' then faktorIV=1000*IV/ton;
if div='NO' then faktorNO=1;
keep year faktorIV faktorNO;
run;

proc summary data=t8a;
var faktorIV faktorNO;
by year;
output out=t8 (drop=_type_ _freq_) sum()=;
run;

data out.factor;
set t8;
run;


proc gplot data=t8;
plot (faktorIV faktorNO)*year/overlay;
run;

proc sort data=a4;
by year;
run;

data cb9;
merge a4 t8;
by year;
run; 

data cb10;
set cb9;
if ton=. then ton=0;
roundfish=put(intsq,$ibts.);
div='    ';
div='IV';
if intsq in ('45F7', '45F8', '46F8', '46F9', '47F9', '46G0', '47G0', '48G0', '47G1') then div='NO';

if div='IV' then ton=faktorIV*ton;
if div='NO' then ton=faktorNO*ton;

n0=ton*n0_per_kg;
n1=ton*n1_per_kg;
n2=ton*n2_per_kg;
n3=ton*n3_per_kg;
n4=ton*n4_per_kg;
wmw0=n0*mw0;
wmw1=n1*mw1;
wmw2=n2*mw2;
wmw3=n3*mw3;
wmw4=n4*mw4;
if quarter=2 then wmw0=mw0;
if quarter=2 then wmw1=mw1;
if quarter=2 then wmw2=mw2;
if quarter=2 then wmw3=mw3;
if quarter=2 then wmw4=mw4;

run;

proc sort data=cb10 out=cb11;
by year quarter div;
run;

proc sort data=cb10 out=cb11x;
by div year intsq;
run;


proc summary data=cb11x;
var ton n_samples;
by div year intsq;
output out=cb12x sum()= ;
run;

proc export data=cb12x (drop= _type_ _freq_)
   outfile="&path.\square_based_cathes.csv"
   dbms=csv 
   replace;
run;
quit;

proc summary data=cb11;
var n0-n4 wmw0-wmw4 ton n_samples 
;
by year quarter div;
output out=cb12 sum()= mean(wmw0)=w2mw0 mean(wmw1)=w2mw1 mean(wmw2)=w2mw2 mean(wmw3)=w2mw3 mean(wmw4)=w2mw4;
run;

data cb13;
set cb12;
mw0=wmw0/n0;
mw1=wmw1/n1;
mw2=wmw2/n2;
mw3=wmw3/n3;
mw4=wmw4/n4;
if quarter=2 then mw0=w2mw0;
if quarter=2 then mw1=w2mw1;
if quarter=2 then mw2=w2mw2;
if quarter=2 then mw3=w2mw3;
if quarter=2 then mw4=w2mw4;

*keep aar area ton n0-n4 mw0-mw4 n_samples;
run;

*****************Indsæt 0-år og middelvægt for hele perioden hvor W=.;

proc sort data=cb13 out=m14 (keep=year quarter div) nodupkey;
by quarter;
run;

data m15a;
set m14;
do year=1974 to 2025 by 1;
output;
end;
run;

data m15;
set m15a;
do div='IV', 'NO';
output;
end;
run;

proc sort data=m15;
by year quarter div;
run;

data m16;
merge cb13 m15;
by year quarter div;
run;

data m17;
set m16;
if n0=. then n0=0;
if n1=. then n1=0;
if n2=. then n2=0;
if n3=. then n3=0;
if n4=. then n4=0;

if ton=. then ton=0;
if year=. then delete;

run;

proc sort data=m17;
by quarter div;
run;

proc summary data=m17;
var mw0-mw4;
by quarter div;
output out=m18 (drop=_type_ _freq_) mean(mw0)=mmw0 mean(mw1)=mmw1 
mean(mw2)=mmw2 mean(mw3)=mmw3 mean(mw4)=mmw4
;
run;

data m19;
merge m17 m18;
by quarter div;
run;

data m20;
set m19;
if mw0=. then mw0=mmw0;
if mw1=. then mw1=mmw1;
if mw2=. then mw2=mmw2;
if mw3=. then mw3=mmw3;
if mw4=. then mw4=mmw4;

if n_samples lt 5  then mw0=mmw0;
if n_samples lt 5  then mw1=mmw1;
if n_samples lt 5  then mw2=mmw2;
if n_samples lt 5  then mw3=mmw3;
if n_samples lt 5  then mw4=mmw4;

n0_per_ton=n0/ton;
n1_per_ton=n1/ton;
n2_per_ton=n2/ton;
n3_per_ton=n3/ton;
n4_per_ton=n4/ton;

n0_n1=n0/n1;
n1_n2=n1/n2;
n2_n3=n2/n3;
n3_n4=n3/n4;

*if n_samples lt 5 then delete;

drop mmw0-mmw4;
run;

proc sort data=m19;
by quarter div;
run;

proc gplot data=m20;
plot (n0-n4)*year/overlay;
by quarter div;
symbol1 v=plus i=join c=black;
symbol2 v=plus i=join c=red;
symbol3 v=plus i=join c=blue;
symbol4 v=plus i=join c=green;
symbol5 v=plus i=join c=orange;
run;

proc gplot data=m19;
plot (mw0-mw4)*year/overlay;
by quarter;
run;

proc gplot data=m20;
plot (n0_per_ton n1_per_ton n2_per_ton n3_per_ton n4_per_ton)*year=div;
by quarter;
run;

********************************Check for consistency*********************************;

data m21;
set m20;
do age=0,1,2,3,4;
output;
end;
run;

data m22;
set m21;
if age=0 then n_per_ton=log(n0_per_ton);
if age=1 then n_per_ton=log(n1_per_ton);
if age=2 then n_per_ton=log(n2_per_ton);
if age=3 then n_per_ton=log(n3_per_ton);

if age=1 then  ratio=log(n1/n0);
if age=2 then  ratio=log(n2/n1);
if age=3 then  ratio=log(n3/n2);

if n_samples lt 5 then delete;
 keep year quarter age n_per_ton ratio n_samples mw0-mw4 div;
run;

***********************Check for consistens internt********************************************;
data m23;
set m22;
*quarter=quarter+1;
*if quarter gt 4 then year=year+1;
*if quarter gt 4 then age=age+1;
*if quarter gt 4 then quarter=quarter-4;
year=year+1;
age=age+1;
lagn_per_ton=n_per_ton;
lag_ratio=ratio;


keep year quarter age lagn_per_ton lag_ratio div;
run;

proc sort data=m22;
by div year quarter age;
run;

proc sort data=m23;
by div year quarter age;
run;

data m24;
merge m22 m23;
by div year quarter age;
run;


*proc export data=m24
   outfile='C:\ar\sprat\age_distributions\catch_ratio_consistency.csv'
   dbms=csv 
   replace;
*run;
*quit;



data m24a;
set m24;
if age=5 then delete;
if age=0 then delete;
if quarter=2 then delete;
dec=floor((year*1)/20);
run;

proc sort data=m24a;
by div quarter age dec;
run;

proc corr data=m24a outp=m24b;
var year ratio lag_ratio mw0-mw4;
by div quarter age ;
run;

data m24c;
set m24b;
if _type_ ne 'CORR' then delete;
if _name_ ne 'ratio' then delete;
run;

*proc export data=m24b
   outfile='C:\ar\sprat\age_distributions\ratio_lag_correlations.csv'
   dbms=csv 
   replace;
*run;
*quit;

proc gplot data=m24c;
plot (lag_ratio)*dec;
by div   ;
symbol1 v=plus i=r c=red;
symbol2 v=plus i=j c=yellow;
symbol3 v=plus i=j c=green;
symbol4 v=plus i=j c=blue;
run;

*****************Check for ekstern med SMS**************************;
proc sort data=m21 out=sms1;
by year age;
run;

data sms1b;
set sms1;
if age=0 then n=n0;
if age=1 then n=n1;
if age=2 then n=n2;
if age=3 then n=n3;
if age=4 then n=n4;
if div='IIIa' then delete;
if n=0 then delete;
run;

proc summary data=sms1b;
var n ton n_samples n3 n4;
by year age;
output out=sms1c (drop=_type_ _freq_) sum()=;
run;

data sms1x;
set sms1c;
prop=n3/(n3+n4);
run;

proc summary data=sms1x;
var prop;
output out=sms1x2 mean()=;
run;

proc gplot data=sms1x;
plot prop*year;
run;

data sms2;
set in.sms_ns_2011;
if species ne 'Sprat' then delete;
sms_ton=yield__sop_;
sms_per_ton=c/sms_ton;
sms_n=c;
run;

proc sort data=sms2;
by year age;
run;

proc summary data=sms2;
var sms_n ;
by year age;
output out=sms2a (drop=_type_ _freq_) sum()=;
run;

proc summary data=sms2;
var sms_ton;
by year ;
output out=sms2b (drop=_type_ _freq_) sum()=;
run;

data sms3a;
merge sms1c sms2a;
by year  age;
run;

data sms3;
merge sms3a sms2b;
by year;
run;

data sms4;
set sms3;
n_per_ton=n/ton;
*if n_samples lt 10 then delete;
sms_n_per_ton=sms_n/sms_ton;
*if age=0 then delete;
if year in (1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1985,1986) then delete;;
run;

proc sort data=sms4;
by age;
run;

proc gplot data=sms4;
plot sms_n_per_ton*n_per_ton/overlay;
*by age;
symbol1 v=plus i=r c=black;
symbol2 v=plus i=j c=red;
symbol3 v=plus i=r c=blue;
symbol4 v=plus i=r c=purple;
run;

proc corr data=sms4;
var sms_n_per_ton n_per_ton;
*by age;
run;


proc sort data=sms4;
by year age;
run;

data sms5;
set sms4;
age=age+1;
sms_n2=n_per_ton;
keep year age sms_n2;
run;

data sms6;
merge sms4 sms5;
by year age;
run;

data sms7;
set sms6;
ratio=log(n_per_ton/sms_n2);
run;

data sms8;
set sms7;
year=year+1;
age=age+1;
lagn_per_ton=n_per_ton;
lag_ratio=ratio;
keep year  age lagn_per_ton lag_ratio ;
run;

proc sort data=sms7;
by  year  age;
run;

proc sort data=sms8;
by  year  age;
run;

data sms9;
merge sms7 sms8;
by  year  age;
run;


*proc export data=m24
   outfile='C:\ar\sprat\age_distributions\catch_ratio_consistency.csv'
   dbms=csv 
   replace;
*run;
*quit;

data sms10;
set sms9;
if age=5 then delete;
if age=0 then delete;
if year le 1975 then delete;
if year in (1985,1986,1987,1988,1989,1991) then delete;
if year ge 2011 then delete;
run;

proc sort data=sms10;
by age ;
run;

proc corr data=sms10 outp=sms11;
var year ratio lag_ratio ;
by age ;
run;

proc gplot data=sms10;
plot ratio*lag_ratio;
by age;
symbol1 v=plus i=j;
run;

data m24c;
set m24b;
if _type_ ne 'CORR' then delete;
if _name_ ne 'ratio' then delete;
run;

*proc export data=m24b
   outfile='C:\ar\sprat\age_distributions\ratio_lag_correlations.csv'
   dbms=csv 
   replace;
*run;
*quit;

proc gplot data=m24c;
plot (lag_ratio)*dec;
by div   ;
symbol1 v=plus i=r c=red;
symbol2 v=plus i=j c=yellow;
symbol3 v=plus i=j c=green;
symbol4 v=plus i=j c=blue;
run;

proc sort data=m24;
by age quarter ;
run;

proc gplot data=m24;
plot ratio*lag_ratio=div;
by age quarter;
symbol1 v=plus i=r c=black;
symbol2 v=plus i=r c=red;
symbol3 v=plus i=r c=blue;
symbol4 v=plus i=r c=green;
symbol5 v=plus i=r c=orange;
run;
**
*****************In 1985, there are no 3-year olds observed, in 1986 there are no samples*************;
****************Before 1974, there are no samples and SMS data are used*******************************;

data s1;
set in.sms_ns_2011;
if species ne 'Sprat' then delete;
sms_ton=yield__sop_;
sms_per_ton=c/sms_ton;
if quarter ne 3 then delete;
if age=0 then smsn0=c;
if age=1 then smsn1=c;
if age=2 then smsn2=c;
if age=3 then smsn3=0.91*c;
if age=3 then smsn4=0.09*c;
if age=0 then smsw0=weca;
if age=1 then smsw1=weca;
if age=2 then smsw2=weca;
if age=3 then smsw3=weca;
if age=3 then smsw4=weca;
run;

proc sort data=s1;
by year;
run;

proc summary data=s1;
var smsn0-smsn4 smsw0-smsw4;
by year;
output out=s2 (drop=_type_ _freq_) sum()=;
run;

proc summary data=s1;
var sms_ton;
by year ;
output out=s3 (drop=_type_ _freq_) sum()=;
run;

proc sort data=m20;
by year;
run;

data s4;
 merge m20 s2 s3;
 by year;
 run;

data s5;
set s4;
if year in (1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1985,1986) and div='IV' then id=1;
if id=1 then mw0=smsw0;
if id=1 then mw1=smsw1;
if id=1 then mw2=smsw2;
if id=1 then mw3=smsw3;
if id=1 then mw4=smsw4;
if id=1 and quarter ne 4 then n0=0;
if id=1 and quarter=4 then n0=smsn0;
if id=1 then n1=ton*smsn1/(sms_ton-mw0*n0);
if id=1 then n2=ton*smsn2/(sms_ton-mw0*n0);
if id=1 then n3=ton*smsn3/(sms_ton-mw0*n0);
if id=1 then n4=ton*smsn4/(sms_ton-mw0*n0);
if id=1 and quarter=4 then n1=(ton-smsn0*mw0)*smsn1/(sms_ton-mw0*n0);
if id=1 and quarter=4 then n2=(ton-smsn0*mw0)*smsn2/(sms_ton-mw0*n0);
if id=1 and quarter=4 then n3=(ton-smsn0*mw0)*smsn3/(sms_ton-mw0*n0);
if id=1 and quarter=4 then n4=(ton-smsn0*mw0)*smsn4/(sms_ton-mw0*n0);
run;

proc sort data=s4;
by quarter;
run;

proc gplot data=s4;
plot mw2*smsw2=quarter;
*by quarter;
run;


*proc sort data=s5;
*by year quarter;
*run;

*proc summary data=s5;
*var n0-n4;
*by year;
*output out=s6 sum()=;
*run;

**************Adding final year q1 catches*******************;

/*
data s6;
set s5;
if year=2019 then ton=.;
if year=2019 then n0=0;
if year=2019 then n1=0;
if year=2019 then n2=0;
if year=2019 then n3=0;
if year=2019 then n4=0;

run;


data s7;
set s6;
q12=0;
q34=0;
if quarter=1 then q12=ton;
if quarter=2 then q12=ton;
if quarter=3 then q34=ton;
if quarter=4 then q34=ton;
if quarter in (1,2) then year=year-1;
if year lt 2014 then delete;
if year ne 2017 and quarter ne 4 then n0=.;
if year ne 2017 and quarter ne 4 then n1=.;
if year ne 2017 and quarter ne 4 then n2=.;
if year ne 2017 and quarter ne 4 then n3=.;
if year ne 2017 and quarter ne 4 then n4=.;
run;

proc sort data=s7;
by div year;
run;

proc summary data=s7;
var n0-n4 q12 q34;
by div year;
output out=s8 sum()=;
run;

data s9;
set s8;
if year=2017 then q12=.;
ratio=q12/q34;
if year ne 2017 then q34=.;
run;

proc summary data=s9;
var n0-n4 ratio q34;
by div;
output out=s10 mean()=;
run;

data s11;
set s10;
n1last=n0;
n2last=n1;
n3last=n2;
n4last=n3+n4;
tonlast=q34*ratio;
year=2018;
keep div n1last n2last n3last n4last tonlast year;
run;

proc sort data=s5;
by div year;
run;

data s12;
merge s5 s11;
by div year;
run;

data s13;
set s12;
if year=2018 then quarter=1;
if year=2018 and quarter=1 then ton=tonlast;  ** replaced by TAC 2017 for area 4 below***;
*if year=2018 and quarter=1 and div='IV' then ton=33830;
if year=2018 and quarter=1 then n0=0;
if year=2018 and quarter=1 then n1=n1last;
if year=2018 and quarter=1 then n2=n2last;
if year=2018 and quarter=1 then n3=n3last;
if year=2018 and quarter=1 then n4=n4last;
run;
*/
********SOP korrektion af mw***************************************************;
data m25;
set s5;
if quarter in (1,2) then sopcorr=ton/(n1*mw1+n2*mw2+n3*mw3+n4*mw4);
if quarter in (3,4) then sopcorr=ton/(n0*mw0+n1*mw1+n2*mw2+n3*mw3+n4*mw4);
if ton=0 then sopcorr=1;

mw0=sopcorr*mw0;
mw1=sopcorr*mw1;
mw2=sopcorr*mw2;
mw3=sopcorr*mw3;
mw4=sopcorr*mw4;
if quarter=. then delete;
sop=n0*mw0+n1*mw1+n2*mw2+n3*mw3+n4*mw4;
keep year quarter n0-n4 mw0-mw4 ton n_samples div;
run;

data m26;
set s5;
if quarter in (1,2) then sopcorr=ton/(n1*mw1+n2*mw2+n3*mw3+n4*mw4);
if quarter in (3,4) then sopcorr=ton/(n0*mw0+n1*mw1+n2*mw2+n3*mw3+n4*mw4);
if ton=0 then sopcorr=1;

if quarter ne 2 then mw1=sopcorr*mw1;
if quarter ne 2 then mw2=sopcorr*mw2;
if quarter ne 2 then mw3=sopcorr*mw3;
if quarter ne 2 then mw4=sopcorr*mw4;
*if year lt 1991 then delete;
if quarter=. then delete;
sop=n0*mw0+n1*mw1+n2*mw2+n3*mw3+n4*mw4;

wmw0=n0*mw0;
wmw1=n1*mw1;
wmw2=n2*mw2;
wmw3=n3*mw3;
wmw4=n4*mw4;
if quarter=2 then wmw1=mw1;
if quarter=2 then wmw2=mw2;
if quarter=2 then wmw3=mw3;
if quarter=2 then wmw4=mw4;
*if quarter in (1,2) then year=year-1;
*keep year quarter n0-n4 mw0-mw4 ton n_samples div;
run;

proc sort data=m26;
by div year quarter;
run;

proc summary data=m26;
var n_samples n0-n4 wmw0-wmw4 ton;
by div year quarter;
output out=m27 sum()= mean(wmw1)=w2mw1 mean(wmw2)=w2mw2 mean(wmw3)=w2mw3 mean(wmw4)=w2mw4;
run;

*Opdater nedestående år til sidste år i tidsserien, så indeværende år;

data m28;
set m27;
if year ne 2025 then mw0=wmw0/n0; *Her gives et gennemsnit over div, year og kvartal - undtagen for det specificerede år, der ende med at få et gennemsnit over år og div;
if year ne 2025 then mw1=wmw1/n1;
if year ne 2025 then mw2=wmw2/n2;
if year ne 2025 then mw3=wmw3/n3;
if year ne 2025 then mw4=wmw4/n4;
if quarter=2 then mw1=w2mw1;
if quarter=2 then mw2=w2mw2;
if quarter=2 then mw3=w2mw3;
if quarter=2 then mw4=w2mw4;
*if quarter=2 then mw0=wmw0;
*if quarter=2 then mw1=wmw1;
*if quarter=2 then mw2=wmw2;
*if quarter=2 then mw3=wmw3;
*if quarter=2 then mw4=wmw4;
*if year lt 1982 then delete;
*keep year quarter n0-n4 mw0-mw4 ton n_samples div;
run;

proc sort data=m28;
by div quarter;
run;

proc summary data=m28;
var mw0-mw4;
by div quarter;
output out=m29 mean(mw0)=mmw0 mean(mw1)=mmw1 mean(mw2)=mmw2 mean(mw3)=mmw3 mean(mw4)=mmw4;
run;

data m30;
merge m28 m29;
by div quarter;
run;

data m31;
set m30;
*if year=2018 then mw0=.;
*if year=2018 then mw1=.;
*if year=2018 then mw2=.;
*if year=2018 then mw3=.;
*if year=2018 then mw4=.;
if mw0=. then mw0=mmw0;
if mw1=. then mw1=mmw1;
if mw2=. then mw2=mmw2;
if mw3=. then mw3=mmw3;
if mw4=. then mw4=mmw4;
*sop2017=ton/(n1*mw1+n2*mw2+n3*mw3+n4*mw4);
*if year=2018 then n1=n1*sop2017;
*if year=2018 then n2=n2*sop2017;
*if year=2018 then n3=n3*sop2017;
*if year=2018 then n4=n4*sop2017;
keep year quarter n0-n4 mw0-mw4 ton n_samples div;
run;

proc sort data=m31;
by div quarter;
run;

proc gplot data=m31;
plot (mw0-mw4)*year/overlay;
by div quarter;
symbol1 v=plus i=join c=black;
symbol2 v=plus i=join c=red;
symbol3 v=plus i=join c=blue;
symbol4 v=plus i=join c=green;
symbol5 v=plus i=join c=red;

run;

******Eksporter csv**;

proc sort data=m31;
by div year quarter;
run;

data miv;
set m31;
if div ne 'IV' then delete;
run;

proc export data=miv
   outfile="&path.\Total_catch_in_numbers_and_mean_weight_benchmark_IV_no_Q2_2024.csv"
   dbms=csv 
   replace;
run;
quit;

data miiia;
set m31;
if div ne 'NO' then delete;
if year lt 1974 then delete;
run;

proc export data=miiia
   outfile="&path.\Total_catch_in_numbers_and_mean_weight_benchmark_NO_no_Q2_2024.csv"
   dbms=csv 
   replace;
run;
quit;


data m26;
set s5;
if quarter in (1,2) then sopcorr=ton/(n1*mw1+n2*mw2+n3*mw3+n4*mw4);
if quarter in (3,4) then sopcorr=ton/(n0*mw0+n1*mw1+n2*mw2+n3*mw3+n4*mw4);
if ton=0 then sopcorr=1;

mw0=sopcorr*mw0;
mw1=sopcorr*mw1;
mw2=sopcorr*mw2;
mw3=sopcorr*mw3;
mw4=sopcorr*mw4;

if quarter=. then delete;
if quarter in (1,2) then year=year-1;


if quarter in (1,2) then n0=n1;
if quarter in (1,2) then n1=n2;
if quarter in (1,2) then n2=n3;
if quarter in (1,2) then n3=n4;
if quarter in (3,4) then n3=n3+n4;

if quarter in (1,2) then mw0=mw1;
if quarter in (1,2) then mw1=mw2;
if quarter in (1,2) then mw2=mw3;
if quarter in (1,2) then mw3=mw4;
if quarter in (3,4) then mw3=(n3*mw3+n4*mw4)/(n3+n4);

wmw0=n0*mw0;
wmw1=n1*mw1;
wmw2=n2*mw2;
wmw3=n3*mw3;

if quarter=2 then wmw0=mw0;
if quarter=2 then wmw1=mw1;
if quarter=2 then wmw2=mw2;
if quarter=2 then wmw3=mw3;
if quarter=2 then wmw4=mw4;

quarter=quarter-2;
if quarter lt 1 then quarter=quarter+4;

*keep year quarter n0-n4 mw0-mw4 ton n_samples div;
run;

proc sort data=m26;
by div year quarter;
run;

proc summary data=m26;
var n_samples n0-n3 wmw0-wmw3 ton;
by div year quarter;
output out=m27 sum()= mean(wmw1)=w2mw1 mean(wmw2)=w2mw2 mean(wmw3)=w2mw3 mean(wmw0)=w2mw0;
run;

data m28;
set m27;
if year lt 2026 then mw0=wmw0/n0;
if year lt 2026 then mw1=wmw1/n1;
if year lt 2026 then mw2=wmw2/n2;
if year lt 2026 then mw3=wmw3/n3;
if quarter in (1,2) then  mw0=wmw0/n0;
if quarter in (1,2) then  mw1=wmw1/n1;
if quarter in (1,2) then  mw2=wmw2/n2;
if quarter in (1,2) then  mw3=wmw3/n3;
if quarter in (4) then  mw0=w2mw0;
if quarter in (4) then  mw1=w2mw1;
if quarter in (4) then  mw2=w2mw2;
if quarter in (4) then  mw3=w2mw3;
if quarter in (4) then  mw4=w2mw4;
*if year lt 1982 then delete;
*keep year quarter n0-n3 mw0-mw3 ton n_samples div;
run;

proc sort data=m28;
by div quarter;
run;

proc summary data=m28;
var mw0-mw3;
by div quarter ;
output out=m29 mean(mw0)=mmw0 mean(mw1)=mmw1 mean(mw2)=mmw2 mean(mw3)=mmw3;
run;

data m30;
merge m28 m29;
by div quarter;
run;

data m31;
set m30;
*if year=2020 and quarter=3 then mw0=.;
*if year=2020 and quarter=3  then mw1=.;
*if year=2020 and quarter=3  then mw2=.;
*if year=2020 and quarter=3  then mw3=.;
if mw0=. then mw0=mmw0;
if mw1=. then mw1=mmw1;
if mw2=. then mw2=mmw2;
if mw3=. then mw3=mmw3;
*sop2017=ton/(n0*mw0+n1*mw1+n2*mw2+n3*mw3);
*if year=2017 and quarter=3  then n0=n0*sop2017;
*if year=2017 and quarter=3  then n1=n1*sop2017;
*if year=2017 and quarter=3  then n2=n2*sop2017;
*if year=2017 and quarter=3  then n3=n3*sop2017;
nperton0=n0/ton;
nperton1=n1/ton;
nperton2=n2/ton;
nperton3=n3/ton;
if div='NO' and year lt 1974 then delete;
keep year quarter n0-n4 mw0-mw4 ton n_samples div;
run;

proc sort data=m31;
by div quarter;
run;

proc gplot data=m31;
plot (nperton0-nperton3)*year/overlay;
by div quarter;
symbol1 v=plus i=join c=black;
symbol2 v=plus i=join c=red;
symbol3 v=plus i=join c=blue;
symbol4 v=plus i=join c=green;
symbol5 v=plus i=join c=red;

run;
*nperton0-nperton3 mw0-mw3;
******Eksporter csv**;

proc sort data=m31;
by div year quarter;
run;

data miv;
set m31;
if div ne 'IV' then delete;
run;

proc export data=miv
   outfile="&path.\July_to_june_quarterly_catch_in_numbers_and_mw_benchmark_IV_no_Q2_2024.csv"
   dbms=csv 
   replace;
run;
quit;

data miiia;
set m31;
if div ne 'NO' then delete;
if year lt 1974 then delete;
run;

proc export data=miiia
   outfile="&path.\July_to_june_quarterly_catch_in_numbers_and_mw_benchmark_NO_no_Q2_2024.csv"
   dbms=csv 
   replace;
run;
quit;
