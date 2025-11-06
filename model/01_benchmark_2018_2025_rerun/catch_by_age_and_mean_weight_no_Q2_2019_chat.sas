
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
   outfile='C:\Users\kibi\OneDrive - Danmarks Tekniske Universitet\gits\spr.27.3a4_commercial_catch\output\01_benchmark_2018_2025_rerun\square_based_cathes.csv'
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

*****************Inds�t 0-�r og middelv�gt for hele perioden hvor W=.;

proc sort data=cb13 out=m14 (keep=year quarter div) nodupkey;
by quarter;
run;

data m15a;
set m14;
do year=1974 to 2020 by 1;
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
