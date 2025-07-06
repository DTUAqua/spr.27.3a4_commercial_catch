/**/
libname in 'C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\data';
libname out 'C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\model';

%let path = C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\stock_coord_work\spr.27.3a4\2025_spr.27.3a4_RDBES_combined\model;

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

data nl1;
set in.norwegian_length_2024;
day=date-100*floor(date/100);
month=(date-10000*floor(date/10000)-day)/100;
year=floor(date/10000)+2000;
sampleid=date*100000+compress(station, "SWE");
if sampleid=. then sampleid=date;
stat='             ';
length=scm*5;
statisticalrectangle=icessq;
stat=station;
country='NOR';
speciescode = 'BRS';
number=total_number_length_class ;
drop station date icessq scm total_number_length_class;
run;

data nl2;
set in.norwegian_alk_2024;
day=date-100*floor(date/100);
month=(date-10000*floor(date/10000)-day)/100;
year=floor(date/10000)+2000;
sampleid=date*100000+compress(station, "SWE");
if sampleid=. then sampleid=date;
stat='             ';
length=scm*5;
statisticalrectangle=icessq;
stat=station;
country='NOR';
speciescode = 'BRS';
weight=mean_weight/1000;
drop station date icessq scm total_number_length_class noage0-noage3 noage4_ propaged total_weight_length_class mean_weight;
run;

data in1;
set in.length_including_survey nl1 nl2;

if country ne 'NOR' then month=month(date);
if country ne 'NOR' then day=day(date);

if cruise in ('IYFS','IBTS','IBTS-1','IBTS-2','IBTS-2.1','IBTS-2.2','BITS-1','BITS-2','1-IYFS') then delete;

if speciescode ne 'BRS' then delete;

length=0.5*floor(length*0.2);
if length gt 20 then delete;
if length lt 3 then delete;
scm=length;

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

area=lat*100+lon;
area2=200*floor(lat/2)+4*floor(lon/4);

quarter=floor((month-1)/3)+1;
lnl=log(length);
if year in (1979,1980,1981,1982,1983) then m_vgt=(weight_new);
if year not in (1979,1980,1981,1982,1983) then m_vgt=(weight_new/number);
if country='' then country='DEN';
if country='NOR' then m_vgt=weight;
logw=log(m_vgt);
c=logw-3.309*lnl;
dec=floor(year/10);

*if year lt 2020 then delete;

run;

proc sort data=in1 out=l1;
by quarter;
run;

proc gplot data=l1;
plot c*year=country;
by quarter;
symbol1 v=plus i=r;
symbol2 v=plus i=r;
run;

*****************Antal prøver pr area pr. quarter******************************************;

proc sort data=l1 out=l2;
by year quarter intsq sampleid;
run;

proc summary data=l2;
var number;
by year  quarter intsq sampleid;
output out=l4a (drop=_type_ _freq_) sum()=sumn;
run;

data l14b;
set l4a;
if sumn lt 25 then delete;
run;

proc summary data=l14b;
var sampleid;
by year  quarter intsq;
output out=l4 (drop=_type_ _freq_) n()=n_samples;
run;

data out.n_samples;
set l4; 
run;

proc summary data=out.n_samples;
var n_samples;
by year;
output out=a2 sum()=n_samples;
run;

proc gplot data=a2;
plot n_samples*year;
run;

******************Weight outliers removed*****************;

proc sort data=in1;
by  quarter scm;
run;

proc genmod data=in1;
class scm;
model m_vgt=lnl/dist=gamma link=log;
output out=l5f pred=pred2;
*by quarter;
run;

proc sort data=l5f nodupkey;
by  quarter scm;
run;

data l5e;
merge in1 l5f;
by  quarter scm;
run;

data l5e1;
set l5e;
if pred2 ne . and m_vgt gt 1.5*pred2 then m_vgt=.;
if pred2 ne . and m_vgt lt 0.5*pred2 then m_vgt=.;
run;

proc sort data=l5e1;
by year quarter scm;
run;

proc genmod data=l5e1;
class scm;
model m_vgt=lnl/dist=gamma link=log;
output out=l5b pred=pred upper=upper lower=lower;
by year quarter;
run;

proc sort data=l5b out=l5d nodupkey;
by year quarter scm pred;
run;

data l5h;
merge l5e1 l5d;
by year quarter scm;
run;
 
data l7b;
set l5h;
if m_vgt=. then m_vgt=pred;
if m_vgt=. then m_vgt=pred2;
vgt=number*m_vgt;
ratio=m_vgt/pred2;
logw=log(m_vgt);
run;

proc sort data=l7b;
by year quarter sampleid scm;
run;

proc gplot data=l7b;
plot number*scm;
by year quarter;
symbol v=plus i=join;
run;

data l7;
set l7b;
run;

proc sort data=l7;
by year intsq quarter scm;
run;

proc sort data=out.alk24_intsq;
by year intsq quarter scm;
run;

data l8;
merge l7 out.alk24_intsq;
by year intsq quarter scm;
run;

data l9;
set l8;
if number=. then delete;
if scm lt 2 then delete;

if quarter in (1,2) and p0 ne 0 then p1=p0+p1;
if quarter in (1,2) and p0 ne 0 then p0=0;

if p0 lt 0.000000000001 then p0=0;
if p1 lt 0.000000000001 then p1=0;
if p2 lt 0.000000000001 then p2=0;
if p3 lt 0.000000000001 then p3=0;
if p4 lt 0.000000000001 then p4=0;

n0=number*p0;
n1=number*p1;
n2=number*p2;
n3=number*p3;
n4=number*p4;


nsum=n0+n1+n2+n3+n4;

w0=vgt*p0;
w1=vgt*p1;
w2=vgt*p2;
w3=vgt*p3;
w4=vgt*p4;

nl0=scm*n0;
nl1=scm*n1;
nl2=scm*n2;
nl3=scm*n3;
nl4=scm*n4;

b=3.309;

nc0=n0*1000*vgt/(number*scm**b);
nc1=n1*1000*vgt/(number*scm**b);
nc2=n2*1000*vgt/(number*scm**b);
nc3=n3*1000*vgt/(number*scm**b);
nc4=n4*1000*vgt/(number*scm**b);

roundfish=put(intsq,$ibts.);
if roundfish in ('OUT','','MANGLER') then delete;
area3='    ';
area3='IV';
*if roundfish in ('8','9') then area3='IIIa';
if intsq in ('45F7', '45F8', '46F8', '46F9', '47F9', '46G0', '47G0', '48G0', '47G1') then area3='NO';
run;

proc sort data=l9;
by year quarter intsq area3 area area2 sampleid;
run;

proc summary data=l9;
var n0-n4 w0-w4 nl0-nl4 nc0-nc4 number nsum vgt;
by year quarter intsq area3 area area2 sampleid ;
output out=l10 sum()=;
run;

data l11;
set l10;

*if year lt 1991 then delete;

if nsum lt 25 then delete;
*if (vgt*0.95) lt vgt0 lt (vgt*1.05) then vgt=vgt0;
*if (vgt*0.95) lt vgt1 lt (vgt*1.05) then vgt=vgt1;
if n0 lt 0.000000000001 then n0=0;
if n1 lt 0.000000000001 then n1=0;
if n2 lt 0.000000000001 then n2=0;
if n3 lt 0.000000000001 then n3=0;
if n4 lt 0.000000000001 then n4=0;
n0_per_kg=n0/vgt;
n1_per_kg=n1/vgt;
n2_per_kg=n2/vgt;
n3_per_kg=n3/vgt;
n4_per_kg=n4/vgt;
mw0=w0/n0;
mw1=w1/n1;
mw2=w2/n2;
mw3=w3/n3;
mw4=w4/n4;
ml0=nl0/n0;
ml1=nl1/n1;
ml2=nl2/n2;
ml3=nl3/n3;
ml4=nl4/n4;

mc0=nc0/n0;
mc1=nc1/n1;
mc2=nc2/n2;
mc3=nc3/n3;
mc4=nc4/n4;

p0=n0_per_kg/(nsum/vgt);
p1=n1_per_kg/(nsum/vgt);
p2=n2_per_kg/(nsum/vgt);
p3=n3_per_kg/(nsum/vgt);
p4=n4_per_kg/(nsum/vgt);
hy=1;
if quarter ge 3 then hy=2;
run;

proc sort data=l11;
by quarter year;
run;

proc summary data=l11;
var p0 p1 p2 p3 p4 n0_per_kg n1_per_kg n2_per_kg n3_per_kg n4_per_kg
mw0 mw1 mw2 mw3 mw4 ml0 ml1 ml2 ml3 ml4 mc0 mc1 mc2 mc3 mc4;
by quarter year;
output out=l11a mean()=;
run;

proc gplot data=l11a;
plot (p0 p1 p2 p3 p4)*year/overlay;
by quarter;
symbol1 v=plus c=black i=join;
symbol2 v=plus c=red i=join;
symbol3 v=plus c=blue i=join;
symbol4 v=plus c=green i=join;
symbol5 v=plus c=purple i=join;
run;

proc gplot data=l11a;
plot (n0_per_kg n1_per_kg n2_per_kg n3_per_kg n4_per_kg)*year/overlay;
by quarter;
run;

proc gplot data=l11a;
plot (mw0 mw1 mw2 mw3 mw4)*year/overlay;
by quarter;
run;

proc gplot data=l11a;
plot (ml0 ml1 ml2 ml3 ml4)*year/overlay;
by quarter;
run;

proc gplot data=l11a;
plot (mc0 mc1 mc2 mc3 mc4)*year/overlay;
by quarter;
run;

proc corr data=l11a;
var year p0 p1 p2 p3 p4 n0_per_kg n1_per_kg n2_per_kg n3_per_kg n4_per_kg
mw0 mw1 mw2 mw3 mw4 ml0 ml1 ml2 ml3 ml4;
by quarter;
run;


proc gchart data=l11a;
vbar mc0 mc1 mc2 mc3 mc4/subgroup=year;
by quarter;
run;

data i1;
set in.new_sandeel_areas_incl_3a;
intsq='    ';
intsq=square;
keep intsq;
run;

proc sort data=i1 out=i2 nodupkey;
by intsq;
run;

data i3;
set i2;
do year=1974 to 2025 by 1;
output;
end;
run;

data i4;
set i3;
do quarter=1 to 4 by 1;
output;
end;
run;

data m0;
set i4;

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

area=lat*100+lon;
area2=200*floor(lat/2)+4*floor(lon/4);
hy=1;
if quarter ge 3 then hy=2;
run;

proc sort data=m0;
by year quarter area3 area;
run;


*************level 1*****************;

proc sort data=l11;
by year quarter area3 area;
run;

proc summary data=l11;
var n0_per_kg n1_per_kg n2_per_kg n3_per_kg n4_per_kg mw0-mw4 ml0-ml4 mc0-mc4 p0-p4 n0-n4 number;
by year quarter area3 area;
output out=l12 mean()= n(number)=nsamples sum(n0)=sn0 sum(n1)=sn1 sum(n2)=sn2 sum(n3)=sn3
sum(n4)=sn4;
run;

data l13;
set l12;
if nsamples lt 5 then delete;
level=1;
if sn0 lt 10 then mw0=.;
if sn0 lt 10 then ml0=.;
if sn0 lt 10 then mc0=.;
if sn1 lt 10 then mw1=.;
if sn1 lt 10 then ml1=.;
if sn1 lt 10 then mc1=.;
if sn2 lt 10 then mw2=.;
if sn2 lt 10 then ml2=.;
if sn2 lt 10 then mc2=.;
if sn3 lt 10 then mw3=.;
if sn3 lt 10 then ml3=.;
if sn3 lt 10 then mc3=.;
if sn4 lt 10 then mw4=.;
if sn4 lt 10 then ml4=.;
if sn4 lt 10 then mc4=.;
drop _type_ _freq_ n0-n4 sn0-sn4 nsamples number;
run;

proc sort data=l13;
by  year quarter area3 area;
run;

data m1;
merge m0 l13;
by  year quarter area3 area;
run;


*************level 2*****************;

proc sort data=l11;
by  year quarter area3 area2;
run;

proc summary data=l11;
var n0_per_kg n1_per_kg n2_per_kg n3_per_kg n4_per_kg mw0-mw4 ml0-ml4 mc0-mc4 p0-p4 n0-n4 number;
by  year quarter area3 area2;
output out=l12 mean()= n(number)=nsamples sum(n0)=sn0 sum(n1)=sn1 sum(n2)=sn2 sum(n3)=sn3
sum(n4)=sn4;
run;

data l13;
set l12;
if nsamples lt 5 then delete;
lnew=2;

if sn0 lt 10 then mw0=.;
if sn0 lt 10 then ml0=.;
if sn0 lt 10 then mc0=.;
if sn1 lt 10 then mw1=.;
if sn1 lt 10 then ml1=.;
if sn1 lt 10 then mc1=.;
if sn2 lt 10 then mw2=.;
if sn2 lt 10 then ml2=.;
if sn2 lt 10 then mc2=.;
if sn3 lt 10 then mw3=.;
if sn3 lt 10 then ml3=.;
if sn3 lt 10 then mc3=.;
if sn4 lt 10 then mw4=.;
if sn4 lt 10 then ml4=.;
if sn4 lt 10 then mc4=.;

newn0=n0_per_kg;
newn1=n1_per_kg;
newn2=n2_per_kg;
newn3=n3_per_kg;
newn4=n4_per_kg;
newmw0=mw0;
newmw1=mw1;
newmw2=mw2;
newmw3=mw3;
newmw4=mw4;
newml0=ml0;
newml1=ml1;
newml2=ml2;
newml3=ml3;
newml4=ml4;
newmc0=mc0;
newmc1=mc1;
newmc2=mc2;
newmc3=mc3;
newmc4=mc4;
newp0=p0;
newp1=p1;
newp2=p2;
newp3=p3;
newp4=p4;
drop _type_ _freq_ nsamples n0_per_kg n1_per_kg n2_per_kg n3_per_kg 
n4_per_kg mw0-mw4 ml0-ml4 mc0-mc4 p0-p4 n0-n4 sn0-sn4 number;
run;

proc sort data=m1;
by  year quarter area3 area2;
run;


proc sort data=l13;
by  year quarter area3 area2;
run;


data m2;
merge l13 m1;
by  year quarter area3 area2;
run;

data m3;
set m2;
if level=. then level=lnew;


if n0_per_kg=. then n0_per_kg=newn0;
if n1_per_kg=. then n1_per_kg=newn1;
if n2_per_kg=. then n2_per_kg=newn2;
if n3_per_kg=. then n3_per_kg=newn3;
if n4_per_kg=. then n4_per_kg=newn4;
if mw0=. then mw0=newmw0;
if mw1=. then mw1=newmw1;
if mw2=. then mw2=newmw2;
if mw3=. then mw3=newmw3;
if mw4=. then mw4=newmw4;

if ml0=. then ml0=newml0;
if ml1=. then ml1=newml1;
if ml2=. then ml2=newml2;
if ml3=. then ml3=newml3;
if ml4=. then ml4=newml4;

if mc0=. then mc0=newmc0;
if mc1=. then mc1=newmc1;
if mc2=. then mc2=newmc2;
if mc3=. then mc3=newmc3;
if mc4=. then mc4=newmc4;

if p0=. then p0=newp0;
if p1=. then p1=newp1;
if p2=. then p2=newp2;
if p3=. then p3=newp3;
if p4=. then p4=newp4;
if p1=. then level=.;

drop newn0-newn4 newmw0-newmw4 newml0-newml4 newmc0-newmc4 newp0-newp4 lnew;
run;

*************level 3*****************;

proc sort data=l11;
by  year quarter area3;
run;

proc summary data=l11;
var n0_per_kg n1_per_kg n2_per_kg n3_per_kg n4_per_kg mw0-mw4 ml0-ml4 mc0-mc4 p0-p4 n0-n4 number;
by  year quarter area3;
output out=l12 mean()= n(number)=nsamples sum(n0)=sn0 sum(n1)=sn1 sum(n2)=sn2 sum(n3)=sn3
sum(n4)=sn4;
run;

data l13;
set l12;
if nsamples lt 5 then delete;
lnew=3;
if sn0 lt 10 then mw0=.;
if sn0 lt 10 then ml0=.;
if sn0 lt 10 then mc0=.;
if sn1 lt 10 then mw1=.;
if sn1 lt 10 then ml1=.;
if sn1 lt 10 then mc1=.;
if sn2 lt 10 then mw2=.;
if sn2 lt 10 then ml2=.;
if sn2 lt 10 then mc2=.;
if sn3 lt 10 then mw3=.;
if sn3 lt 10 then ml3=.;
if sn3 lt 10 then mc3=.;
if sn4 lt 10 then mw4=.;
if sn4 lt 10 then ml4=.;
if sn4 lt 10 then mc4=.;
newn0=n0_per_kg;
newn1=n1_per_kg;
newn2=n2_per_kg;
newn3=n3_per_kg;
newn4=n4_per_kg;
newmw0=mw0;
newmw1=mw1;
newmw2=mw2;
newmw3=mw3;
newmw4=mw4;
newml0=ml0;
newml1=ml1;
newml2=ml2;
newml3=ml3;
newml4=ml4;
newmc0=mc0;
newmc1=mc1;
newmc2=mc2;
newmc3=mc3;
newmc4=mc4;
newp0=p0;
newp1=p1;
newp2=p2;
newp3=p3;
newp4=p4;
drop _type_ _freq_ nsamples n0_per_kg n1_per_kg n2_per_kg n3_per_kg 
n4_per_kg mw0-mw4 ml0-ml4 mc0-mc4 p0-p4 n0-n4 sn0-sn4 number;
run;

proc sort data=m3;
by  year quarter area3;
run;

proc sort data=l13;
by  year quarter area3;
run;

data m4;
merge l13 m3;
by  year quarter area3;
run;

data m5;
set m4;
if level=. then level=lnew;
if n0_per_kg=. then n0_per_kg=newn0;
if n1_per_kg=. then n1_per_kg=newn1;
if n2_per_kg=. then n2_per_kg=newn2;
if n3_per_kg=. then n3_per_kg=newn3;
if n4_per_kg=. then n4_per_kg=newn4;
if mw0=. then mw0=newmw0;
if mw1=. then mw1=newmw1;
if mw2=. then mw2=newmw2;
if mw3=. then mw3=newmw3;
if mw4=. then mw4=newmw4;

if ml0=. then ml0=newml0;
if ml1=. then ml1=newml1;
if ml2=. then ml2=newml2;
if ml3=. then ml3=newml3;
if ml4=. then ml4=newml4;

if mc0=. then mc0=newmc0;
if mc1=. then mc1=newmc1;
if mc2=. then mc2=newmc2;
if mc3=. then mc3=newmc3;
if mc4=. then mc4=newmc4;

if p0=. then p0=newp0;
if p1=. then p1=newp1;
if p2=. then p2=newp2;
if p3=. then p3=newp3;
if p4=. then p4=newp4;
if p1=. then level=.;

drop newn0-newn4 newmw0-newmw4 newml0-newml4 newmc0-newmc4 newp0-newp4 lnew;
run;

*************level 4*****************;

proc sort data=l11;
by  year area3 hy;
run;

proc summary data=l11;
var n0_per_kg n1_per_kg n2_per_kg n3_per_kg n4_per_kg mw0-mw4 ml0-ml4 mc0-mc4 p0-p4 n0-n4 number;
by  year area3 hy;
output out=l12 mean()= n(number)=nsamples sum(n0)=sn0 sum(n1)=sn1 sum(n2)=sn2 sum(n3)=sn3
sum(n4)=sn4;
run;

data l13;
set l12;
if nsamples lt 5 then delete;
lnew=4;
if sn0 lt 10 then mw0=.;
if sn0 lt 10 then ml0=.;
if sn0 lt 10 then mc0=.;
if sn1 lt 10 then mw1=.;
if sn1 lt 10 then ml1=.;
if sn1 lt 10 then mc1=.;
if sn2 lt 10 then mw2=.;
if sn2 lt 10 then ml2=.;
if sn2 lt 10 then mc2=.;
if sn3 lt 10 then mw3=.;
if sn3 lt 10 then ml3=.;
if sn3 lt 10 then mc3=.;
if sn4 lt 10 then mw4=.;
if sn4 lt 10 then ml4=.;
if sn4 lt 10 then mc4=.;
newn0=n0_per_kg;
newn1=n1_per_kg;
newn2=n2_per_kg;
newn3=n3_per_kg;
newn4=n4_per_kg;
newmw0=mw0;
newmw1=mw1;
newmw2=mw2;
newmw3=mw3;
newmw4=mw4;
newml0=ml0;
newml1=ml1;
newml2=ml2;
newml3=ml3;
newml4=ml4;
newmc0=mc0;
newmc1=mc1;
newmc2=mc2;
newmc3=mc3;
newmc4=mc4;
newp0=p0;
newp1=p1;
newp2=p2;
newp3=p3;
newp4=p4;
drop _type_ _freq_ nsamples n0_per_kg n1_per_kg n2_per_kg n3_per_kg 
n4_per_kg mw0-mw4 ml0-ml4 mc0-mc4 p0-p4 n0-n4 sn0-sn4 number;
run;

proc sort data=m5;
by  year area3 hy;
run;

proc sort data=l13;
by  year  area3 hy;
run;


data m6;
merge l13 m5;
by  year area3 hy;
run;

data m7;
set m6;
if level=. then level=lnew;
if n0_per_kg=. then n0_per_kg=newn0;
if n1_per_kg=. then n1_per_kg=newn1;
if n2_per_kg=. then n2_per_kg=newn2;
if n3_per_kg=. then n3_per_kg=newn3;
if n4_per_kg=. then n4_per_kg=newn4;
if mw0=. then mw0=newmw0;
if mw1=. then mw1=newmw1;
if mw2=. then mw2=newmw2;
if mw3=. then mw3=newmw3;
if mw4=. then mw4=newmw4;

if ml0=. then ml0=newml0;
if ml1=. then ml1=newml1;
if ml2=. then ml2=newml2;
if ml3=. then ml3=newml3;
if ml4=. then ml4=newml4;

if mc0=. then mc0=newmc0;
if mc1=. then mc1=newmc1;
if mc2=. then mc2=newmc2;
if mc3=. then mc3=newmc3;
if mc4=. then mc4=newmc4;

if p0=. then p0=newp0;
if p1=. then p1=newp1;
if p2=. then p2=newp2;
if p3=. then p3=newp3;
if p4=. then p4=newp4;
if p1=. then level=.;

drop newn0-newn4 newmw0-newmw4 newml0-newml4 newmc0-newmc4 newp0-newp4 lnew;
run;


*************level 5*****************;

proc sort data=l11;
by  year area3 ;
run;

proc summary data=l11;
var n0_per_kg n1_per_kg n2_per_kg n3_per_kg n4_per_kg mw0-mw4 ml0-ml4 mc0-mc4 p0-p4 n0-n4 number;
by  year area3;
output out=l12 mean()= n(number)=nsamples sum(n0)=sn0 sum(n1)=sn1 sum(n2)=sn2 sum(n3)=sn3
sum(n4)=sn4;
run;

data l12a;
set l12;
do quarter=1,2,3,4;
output;
end;
run;

data l13;
set l12a;


faktor=0;
if n3_per_kg=0 then mw3=0;
if n4_per_kg=0 then mw4=0;
if n0_per_kg ne 0 then faktor=n0_per_kg*mw0/(n0_per_kg*mw0+n1_per_kg*mw1+n2_per_kg*mw2+n3_per_kg*mw3+n4_per_kg*mw4);
if mw3=0 then mw3=.;
if mw4=0 then mw4=.;
if quarter in (1,2) then n0_per_kg=0;
if quarter in (1,2) then n1_per_kg=n1_per_kg/(1-faktor);
if quarter in (1,2) then n2_per_kg=n2_per_kg/(1-faktor);
if quarter in (1,2) then n3_per_kg=n3_per_kg/(1-faktor);
if quarter in (1,2) then n4_per_kg=n4_per_kg/(1-faktor);

if nsamples lt 5 then delete;
lnew=5;
if sn0 lt 10 then mw0=.;
if sn0 lt 10 then ml0=.;
if sn0 lt 10 then mc0=.;
if sn1 lt 10 then mw1=.;
if sn1 lt 10 then ml1=.;
if sn1 lt 10 then mc1=.;
if sn2 lt 10 then mw2=.;
if sn2 lt 10 then ml2=.;
if sn2 lt 10 then mc2=.;
if sn3 lt 10 then mw3=.;
if sn3 lt 10 then ml3=.;
if sn3 lt 10 then mc3=.;
if sn4 lt 10 then mw4=.;
if sn4 lt 10 then ml4=.;
if sn4 lt 10 then mc4=.;
newn0=n0_per_kg;
newn1=n1_per_kg;
newn2=n2_per_kg;
newn3=n3_per_kg;
newn4=n4_per_kg;
newmw0=mw0;
newmw1=mw1;
newmw2=mw2;
newmw3=mw3;
newmw4=mw4;

newml0=ml0;
newml1=ml1;
newml2=ml2;
newml3=ml3;
newml4=ml4;

newmc0=mc0;
newmc1=mc1;
newmc2=mc2;
newmc3=mc3;
newmc4=mc4;

newp0=p0;
newp1=p1;
newp2=p2;
newp3=p3;
newp4=p4;

drop _type_ _freq_ nsamples n0_per_kg n1_per_kg n2_per_kg n3_per_kg 
n4_per_kg mw0-mw4 ml0-ml4 mc0-mc4 p0-p4 n0-n4 sn0-sn4 number faktor;
run;

proc sort data=m7;
by  year quarter area3;
run;

proc sort data=l13;
by  year quarter area3;
run;

data m8;
merge l13 m7;
by  year quarter area3;
run;

data m9;
set m8;
if level=. then level=lnew;
if n0_per_kg=. then n0_per_kg=newn0;
if n1_per_kg=. then n1_per_kg=newn1;
if n2_per_kg=. then n2_per_kg=newn2;
if n3_per_kg=. then n3_per_kg=newn3;
if n4_per_kg=. then n4_per_kg=newn4;
if mw0=. then mw0=newmw0;
if mw1=. then mw1=newmw1;
if mw2=. then mw2=newmw2;
if mw3=. then mw3=newmw3;
if mw4=. then mw4=newmw4;

if ml0=. then ml0=newml0;
if ml1=. then ml1=newml1;
if ml2=. then ml2=newml2;
if ml3=. then ml3=newml3;
if ml4=. then ml4=newml4;

if mc0=. then mc0=newmc0;
if mc1=. then mc1=newmc1;
if mc2=. then mc2=newmc2;
if mc3=. then mc3=newmc3;
if mc4=. then mc4=newmc4;

if p0=. then p0=newp0;
if p1=. then p1=newp1;
if p2=. then p2=newp2;
if p3=. then p3=newp3;
if p4=. then p4=newp4;
if p1=. then level=.;

if year le 2022 then delete;

drop newn0-newn4 newmw0-newmw4 newml0-newml4 newmc0-newmc4 newp0-newp4 lnew;
run;

data m16;
set out.n_samples;
if year le 2022 then delete;
run;


proc sort data=m9;
by year quarter intsq;
run;

data m17;
merge m9 m16;
by year quarter intsq;
run; 

data m17a;
set in.mean_weight_and_n_per_kg_2023;
if year ge 2023 then delete;
run;

data out.mean_weight_and_n_per_kg_2024;
set m17 m17a;
if n_samples=. then n_samples=0;
if n1_per_kg=. then delete;

run;

proc export data=out.mean_weight_and_n_per_kg_2024
   outfile="&path.\numbers_at_age_per_kg_and_mean_weight_2024.csv"
   dbms=csv 
   replace;
run;
quit;

proc sort data=out.mean_weight_and_n_per_kg_2024 out=m17;
by year quarter;
run;

data m17a;
set m17;
if n_samples=0 then delete;
lat=substr(intsq,1,2)/2+35.75;
x1=substr(intsq,3,1);
x2=substr(intsq,4,1);
if x1='E' then lon=-(10-x2)+0.5;
if x1='F' then lon=x2+0.5;
if x1='G' then lon=10+x2+0.5;

roundfish=put(intsq,$ibts.);

*if year lt 1991 then delete;
*if roundfish not in (4,5,6,7) then delete;
*if n0_per_kg gt 100 then n0_per_kg=.;
*if lat gt 56.5 then delete;
run;

proc gchart data=m17a;
vbar mw0 mw1 mw2 mw3 mw4 
n0_per_kg n1_per_kg n2_per_kg n3_per_kg n4_per_kg/subgroup=year;
run;

proc gplot data=m17a;
plot (mc0 mc1 mc2 mc3 mc4)*year/overlay;
run;

proc summary data=m17;
var n_samples;
by year;
output out=m18 sum()=;
run;

proc print data=m18;
var year n_samples;
run;

proc summary data=m17a;
var n0_per_kg n1_per_kg n2_per_kg n3_per_kg n4_per_kg mw0 mw1 mw2 mw3 mw4 n_samples;
by year quarter;
output out=m18 mean()= sum(n_samples)=samples;
run;

proc print data=m18;
var year quarter samples mw0 mw1 mw2 mw3 mw4;
run;

proc sort data=m18;
by year quarter;
run;

data m19;
set m18;
*if samples le 5 then delete;
if area3='NO' then delete;
run;


proc sort data=m19;
by quarter;
run;

proc gplot data=m19;
plot (n0_per_kg n1_per_kg n2_per_kg n3_per_kg n4_per_kg)*year/overlay;
by quarter;
run;
/*
data m19a;
set m19;
do age=0 to 4 by 1;
output;
end;
run;

data m19b;
set m19a;
if age=0 then n=n0_per_kg;
if age=1 then n=n1_per_kg/n2_per_kg;
if age=2 then n=n2_per_kg/n3_per_kg;
if age=3 then n=n3_per_kg/n4_per_kg;
if age=4 then n=n4_per_kg;
keep year quarter age n;
run;

data m20;
set m19b;
n_lag=n;
*age=age+1;
*year=year+1;
if quarter ge 4 then year=year+1;
if quarter ge 4 then age=age+1;
quarter=quarter+1;
if quarter ge 5 then quarter=quarter-4;
keep year quarter age n_lag;
run;

data m21;
merge m19b m20;
by year quarter age;
run;

proc sort data=m21;
by age quarter;
run;

proc corr data=m21;
var year n n_lag ;
by age quarter;
run;

proc sort data=m21;
by quarter;
run;

proc gplot data=m21;
plot n*n_lag=age/overlay;
by quarter;
symbol1 v=0 i=r;
symbol2 v=1 i=r;
symbol3 v=2 i=r;
symbol4 v=3 i=r;
symbol5 v=4 i=r;
run;
*/
proc sort data=m19;
by quarter;
run;

proc gplot data=m19;
plot (mw0 mw1 mw2 mw3 mw4)*year/overlay;
by quarter;
symbol1 v=0 i=join;
symbol2 v=1 i=join;
symbol3 v=2 i=join;
symbol4 v=3 i=join;
symbol5 v=4 i=join;
run;

