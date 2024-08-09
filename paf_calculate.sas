
libname cnhsdata '********'; run;
options formchar=' _________';

proc format;
value Canfoodgroup
                   1='Diet low in fruits'    2='Diet low in vegetables'           3='Nut(g/day)'
		   		       				             4='Diet high in red meat' 5='Diet high in processed meat'
                   6='dairy(g/day)'    7='Ate pickled vegetables(mg/day)'  8='Alcohol consumption'
                   9='Fiber (g/day)'   10='Calcium (mg/day)' 11='Diet low in whole grain'
                  12='Diet low in bean products' 13='High BMI'    14='Ever smoking'  15='Physical inactive'
                    16=' Self-reported diabetes' 17=' Hypertension' 18='HBV infection' 19='HCV infection' 20='HIV'21='H.pylori infection' 22='HPV infection'23='PM2.5'24='Second-hand smoking';
value agegroup 20='20-29' 30='30-39' 40='40-49' 50='50-59' 60='60-69' 70='70+';
value sex      1='male' 2='female';
value urbanrural 1='urban' 2='rural';
run;
/*The paf was calculated by the new method for continuous variables, using quantiles*/

data a; set cnhsdata.group20210308ur cnhsdata.group13bmi cnhsdata.group11wg; 
newgroup=wave*10000+urbanrural*1000+agegroup*10+sex;
if measurement<0 then measurement=0;
if group=1 or group=2 or group=4 or group=5 or group=11 or group=12 or group=13 then output;
run;
proc sort data=a;by  group wave urbanrural agegroup sex;run;
proc univariate data=a;
    var measurement;
	by group wave urbanrural agegroup sex;
    output out=percentile_data
    pctlpts = 2.5, 7.5, 12.5,17.5,22.5,27.5,32.5,37.5,42.5,47.5,
              52.5,57.5,62.5,67.5,72.5,77.5,82.5,87.5,92.5,97.5
    pctlpre = P_;
run;
proc sort data=a;by  group wave sex;run;
proc univariate data=a;
    var measurement;
	by group wave sex;
    output out=stable1
    pctlpts = 25,50,75
    pctlpre = P_;
run;
proc sort data=a;by  group wave;run;
proc univariate data=a;
    var measurement;
	by group wave;
    output out=table1
    pctlpts = 25,50,75
    pctlpre = P_;
run;
proc print;run;

proc transpose data=percentile_data out=percentile_data_tans1;
by group wave urbanrural agegroup sex;run;

proc import datafile="D:\PAF\lj\RR\RRcancerpw.xlsx"
        out=rrdata
        dbms=excel replace;sheet=rr;
     getnames=yes;
run;

proc print data = rrdata;
run;
data rr;set rrdata; if group=1 or group=2 or group=4 or group=5  or group=11 or group=12 or group=13 then output;run;

proc sort data=percentile_data_tans1;by  group sex ;run;proc sort data=rr;by  group sex ;run;
data percentile_data_tans2;
merge percentile_data_tans1 rr;by group sex;
p=0.05;
newgroup=wave*10000+urbanrural*1000+agegroup*10+sex;run;

data calculation;
set percentile_data_tans2;
var_tm=mean_tmred;
if group in (1,2,11) then do;
								       if col1>var_tm then do;
RRlung=1;	RRliver=1;	RRgastric=1	;RRcrc=1;	RReso=1;	RRprostate=1;	RRlym=1;	RRova=1;	RRbreast=1;	RRcervical=1;	RRleu=1;RRpan=1;
                                                       end;
  else do;
RRlung=exp((var_tm-col1)*beta_lung) ;    RRliver= exp((var_tm-col1)*beta_liv);      RRgastric=exp((var_tm-col1)*beta_gas);                                         
RRcrc=exp((var_tm-col1)*beta_crc);RReso=exp((var_tm-col1)*beta_eso);RRprostate=exp((var_tm-col1)*beta_pro);
RRlym=exp((var_tm-col1)*beta_lym); RRova= exp((var_tm-col1)*beta_ova); RRbreast=exp((var_tm-col1)*beta_br);
RRcervical=exp((var_tm-col1)*beta_cer); RRleu=exp((var_tm-col1)*beta_leu);RRpan=exp((var_tm-col1)*beta_pan);
											                                   end;
															         end;
if group in (4,5,13) then do;

										       if col1<var_tm then do;
RRlung=1;	RRliver=1;	RRgastric=1	;RRcrc=1;	RReso=1;	RRprostate=1;	RRlym=1;	RRova=1;	RRbreast=1;	RRcervical=1;	RRleu=1;RRpan=1;
                                                       end;
                                                               else do;

RRlung=exp((col1-var_tm)*beta_lung) ;    RRliver= exp((col1-var_tm)*beta_liv);      RRgastric=exp((col1-var_tm)*beta_gas);                                         
RRcrc=exp((col1-var_tm)*beta_crc);RReso=exp((col1-var_tm)*beta_eso);RRprostate=exp((col1-var_tm)*beta_pro);
RRlym=exp((col1-var_tm)*beta_lym); RRova= exp((col1-var_tm)*beta_ova); RRbreast=exp((col1-var_tm)*beta_br);
RRcervical=exp((col1-var_tm)*beta_cer); RRleu=exp((col1-var_tm)*beta_leu);RRpan=exp((col1-var_tm)*beta_pan);
											                                   end;
																												      end;
p_rr_lung=p*(RRlung-1);
p_rr_liv=p*(RRliver-1);
p_rr_gas=p*(RRgastric-1);
p_rr_crc=p*(RRcrc-1);
p_rr_eso=p*(RReso-1);
p_rr_pro=p*(RRprostate-1);
p_rr_lym=p*(RRlym-1);
p_rr_ova=p*(RRova-1);
p_rr_bre=p*(RRbreast-1);
p_rr_cer=p*(RRcervical-1);
p_rr_leu=p*(RRleu-1);
p_rr_pan=p*(RRpan-1);

proc sort;
by group newgroup;
run;


data pafdata;
set calculation;
by group newgroup;
if first.newgroup then do;sum_lung=0;sum_liv=0;sum_gas=0;
                             sum_crc=0; sum_eso=0;sum_pro=0;sum_lym=0;sum_ova=0;sum_bre=0;sum_cer=0;sum_leu=0;sum_pan=0;
																       pcheck=0;
																             end;
sum_lung+p_rr_lung;
sum_liv+p_rr_liv;
sum_gas+p_rr_gas;
sum_crc+p_rr_crc;
sum_eso+p_rr_eso;
sum_pro+p_rr_pro;
sum_lym+p_rr_lym;
sum_ova+p_rr_ova;
sum_bre+p_rr_bre;
sum_cer+p_rr_cer;
sum_leu+p_rr_leu;
sum_pan+p_rr_pan;
pcheck+p;

if last.newgroup then output;
keep newgroup sum_lung sum_liv sum_gas sum_crc  sum_eso sum_pro sum_lym sum_ova sum_bre sum_cer sum_leu sum_pan pcheck group wave urbanrural agegroup sex;
run;

data PAF2035;
set pafdata;
paf_lung=put(sum_lung/(sum_lung+1)*100, 4.1);
paf_liv=put(sum_liv/(sum_liv+1)*100, 4.1);
paf_gas=put(sum_gas/(sum_gas+1)*100, 4.1);
paf_crc=put(sum_crc/(sum_crc+1)*100, 4.1);
paf_eso=put(sum_eso/(sum_eso+1)*100, 4.1);
paf_pro=put(sum_pro/(sum_pro+1)*100, 4.1);
paf_lym=put(sum_lym/(sum_lym+1)*100, 4.1);
paf_ova=put(sum_ova/(sum_ova+1)*100, 4.1);
paf_bre=put(sum_bre/(sum_bre+1)*100, 4.1);
paf_cer=put(sum_cer/(sum_cer+1)*100, 4.1);
paf_leu=put(sum_leu/(sum_leu+1)*100, 4.1);
paf_pan=put(sum_pan/(sum_pan+1)*100, 4.1);
year = wave;
keep newgroup group  year  urbanrural agegroup sex  paf_lung paf_liv paf_gas paf_crc paf_eso paf_pro paf_lym paf_ova paf_bre paf_cer paf_leu paf_pan;
proc sort;
by group year sex;
proc print;
run;


**********************PAF for binary exposure ********************;
data rrsmkpasveg;
set rrdata;
if group in (14,15,8) then output; keep group sex beta_lung 	beta_liv	beta_gas	beta_crc	beta_eso	beta_pro	beta_lym	beta_ova	beta_br	beta_cer	beta_leu beta_pan
;

proc sort;
by group sex;
proc print;
run;

data psmkpasveg;
set cnhsdata.meansdforlj;
if group in (14,15,8) then output;
proc sort;
by group sex;
run;

data smkpasvegdata;
merge psmkpasveg(in=msta) rrsmkpasveg;
by group sex;
if msta;
percent=mean;
proc print;
run;


data pafsmkpasveg;
set smkpasvegdata;
p_rr_lung=(percent/100)*(exp(beta_lung)-1);
p_rr_liv=(percent/100)*(exp(beta_liv)-1);
p_rr_gas=(percent/100)*(exp(beta_gas)-1);
p_rr_crc=(percent/100)*(exp(beta_crc)-1);
p_rr_eso=(percent/100)*(exp(beta_eso)-1);
p_rr_pro=(percent/100)*(exp(beta_pro)-1);
p_rr_lym=(percent/100)*(exp(beta_lym)-1);
p_rr_ova=(percent/100)*(exp(beta_ova)-1);
p_rr_bre=(percent/100)*(exp(beta_br)-1);
p_rr_cer=(percent/100)*(exp(beta_cer)-1);
p_rr_leu=(percent/100)*(exp(beta_leu)-1);
p_rr_pan=(percent/100)*(exp(beta_pan)-1);
paf_lung=put(p_rr_lung/(p_rr_lung+1)*100, 4.1);
paf_liv=put(p_rr_liv/(p_rr_liv+1)*100, 4.1);
paf_gas=put(p_rr_gas/(p_rr_gas+1)*100, 4.1);
paf_crc=put(p_rr_crc/(p_rr_crc+1)*100, 4.1);
paf_eso=put(p_rr_eso/(p_rr_eso+1)*100, 4.1);
paf_pro=put(p_rr_pro/(p_rr_pro+1)*100, 4.1);
paf_lym=put(p_rr_lym/(p_rr_lym+1)*100, 4.1);
paf_ova=put(p_rr_ova/(p_rr_ova+1)*100, 4.1);
paf_bre=put(p_rr_bre/(p_rr_bre+1)*100, 4.1);
paf_cer=put(p_rr_cer/(p_rr_cer+1)*100, 4.1);
paf_leu=put(p_rr_leu/(p_rr_leu+1)*100, 4.1);
paf_pan=put(p_rr_pan/(p_rr_pan+1)*100, 4.1);
year = wave;
keep year group urbanrural sex agegroup paf_lung paf_liv paf_gas paf_crc paf_eso paf_pro paf_lym paf_ova paf_bre paf_cer paf_leu paf_pan;
proc sort;
by  group urbanrural sex agegroup year;
proc print;
run;


*********************************************** overall paf *******************************************;

data pafall;
set paf2035  pafsmkpasveg;
drop newgroup;
proc freq;
tables group;
proc print;
run;


/*PAF was combined by gender, age and region*/
data cnhsdata.pafall; set pafall; run;
%macro pafpre(ur, sex, age);

data paf&ur&sex&age;
set pafall;
if agegroup=&age and sex=&sex and urbanrural=&ur;
paf_lung&ur&sex&age=paf_lung;
paf_liv&ur&sex&age=paf_liv;
paf_gas&ur&sex&age=paf_gas;
paf_crc&ur&sex&age=paf_crc;
paf_eso&ur&sex&age=paf_eso;
paf_pro&ur&sex&age=paf_pro;
paf_lym&ur&sex&age=paf_lym;
paf_ova&ur&sex&age=paf_ova;
paf_bre&ur&sex&age=paf_bre;
paf_cer&ur&sex&age=paf_cer;
paf_leu&ur&sex&age=paf_leu;
paf_pan&ur&sex&age=paf_pan;
keep year group paf_lung&ur&sex&age paf_liv&ur&sex&age paf_gas&ur&sex&age paf_crc&ur&sex&age  paf_eso&ur&sex&age paf_pro&ur&sex&age 
paf_lym&ur&sex&age paf_ova&ur&sex&age paf_bre&ur&sex&age paf_cer&ur&sex&age   paf_leu&ur&sex&age paf_pan&ur&sex&age;
proc sort;
by group year ;
run;

%mend;

%pafpre(1,1,20);%pafpre(1,1,30);%pafpre(1,1,40);%pafpre(1,1,50);%pafpre(1,1,60);%pafpre(1,1,70);%pafpre(1,2,20);%pafpre(1,2,30);%pafpre(1,2,40);%pafpre(1,2,50);
%pafpre(1,2,60);%pafpre(1,2,70);%pafpre(2,1,20);%pafpre(2,1,30);%pafpre(2,1,40);%pafpre(2,1,50);%pafpre(2,1,60);%pafpre(2,1,70);%pafpre(2,2,20);%pafpre(2,2,30);
%pafpre(2,2,40);%pafpre(2,2,50);%pafpre(2,2,60);%pafpre(2,2,70);

data overallpaf;
merge paf1120 paf1130 paf1140 paf1150 paf1160 paf1170 paf1220 paf1230 paf1240 paf1250 paf1260 paf1270 paf2120 paf2130 paf2140 paf2150 paf2160
paf2170 paf2220 paf2230 paf2240 paf2250 paf2260 paf2270;
by  group year;
yearpaf_lung=
(paf_lung1120*0.0640749792291515+paf_lung1130*0.0632016056709848+paf_lung1140*0.0589767962053254+paf_lung1150*0.0389303923883878+paf_lung1160*0.0214615354617439+
paf_lung1170*0.012917252516362+paf_lung1220*0.0628548673915807+paf_lung1230*0.0604839901664346+paf_lung1240*0.0553121677405731+paf_lung1250*0.0378057263805841+
paf_lung1260*0.0214891833993558+paf_lung1270*0.0143280580472796+paf_lung2120*0.0490522296935186+paf_lung2130*0.0496180341400287+paf_lung2140*0.0556350747096706+
paf_lung2150*0.0432399414312312+paf_lung2160*0.0282056505673727+paf_lung2170*0.019914850763446+paf_lung2220*0.0491085726792354+paf_lung2230*0.0474503134338851+
paf_lung2240*0.0549308979277063+paf_lung2250*0.0414098083440804+paf_lung2260*0.0266205001344798+paf_lung2270*0.0229775715775817);
yearpaf_liv=
(paf_liv1120*0.0640749792291515+paf_liv1130*0.0632016056709848+paf_liv1140*0.0589767962053254+paf_liv1150*0.0389303923883878+paf_liv1160*0.0214615354617439
+paf_liv1170*0.012917252516362+paf_liv1220*0.0628548673915807+paf_liv1230*0.0604839901664346+paf_liv1240*0.0553121677405731+paf_liv1250*0.0378057263805841+
paf_liv1260*0.0214891833993558+paf_liv1270*0.0143280580472796+paf_liv2120*0.0490522296935186+paf_liv2130*0.0496180341400287+paf_liv2140*0.0556350747096706+
paf_liv2150*0.0432399414312312+paf_liv2160*0.0282056505673727+paf_liv2170*0.019914850763446+paf_liv2220*0.0491085726792354+paf_liv2230*0.0474503134338851+
paf_liv2240*0.0549308979277063+paf_liv2250*0.0414098083440804+paf_liv2260*0.0266205001344798+paf_liv2270*0.0229775715775817);
yearpaf_gas=
(paf_gas1120*0.0640749792291515+paf_gas1130*0.0632016056709848+paf_gas1140*0.0589767962053254+paf_gas1150*0.0389303923883878+paf_gas1160*0.0214615354617439+
paf_gas1170*0.012917252516362+paf_gas1220*0.0628548673915807+paf_gas1230*0.0604839901664346+paf_gas1240*0.0553121677405731+paf_gas1250*0.0378057263805841+
paf_gas1260*0.0214891833993558+paf_gas1270*0.0143280580472796+paf_gas2120*0.0490522296935186+paf_gas2130*0.0496180341400287+paf_gas2140*0.0556350747096706+
paf_gas2150*0.0432399414312312+paf_gas2160*0.0282056505673727+paf_gas2170*0.019914850763446+paf_gas2220*0.0491085726792354+paf_gas2230*0.0474503134338851+
paf_gas2240*0.0549308979277063+paf_gas2250*0.0414098083440804+paf_gas2260*0.0266205001344798+paf_gas2270*0.0229775715775817);
yearpaf_crc=
(paf_crc1120*0.0640749792291515+paf_crc1130*0.0632016056709848+paf_crc1140*0.0589767962053254+paf_crc1150*0.0389303923883878+paf_crc1160*0.0214615354617439+
paf_crc1170*0.012917252516362+paf_crc1220*0.0628548673915807+paf_crc1230*0.0604839901664346+paf_crc1240*0.0553121677405731+paf_crc1250*0.0378057263805841+
paf_crc1260*0.0214891833993558+paf_crc1270*0.0143280580472796+paf_crc2120*0.0490522296935186+paf_crc2130*0.0496180341400287+paf_crc2140*0.0556350747096706+
paf_crc2150*0.0432399414312312+paf_crc2160*0.0282056505673727+paf_crc2170*0.019914850763446+paf_crc2220*0.0491085726792354+paf_crc2230*0.0474503134338851+
paf_crc2240*0.0549308979277063+paf_crc2250*0.0414098083440804+paf_crc2260*0.0266205001344798+paf_crc2270*0.0229775715775817);

yearpaf_eso=
(paf_eso1120*0.0640749792291515+paf_eso1130*0.0632016056709848+paf_eso1140*0.0589767962053254+paf_eso1150*0.0389303923883878+paf_eso1160*0.0214615354617439+
paf_eso1170*0.012917252516362+paf_eso1220*0.0628548673915807+paf_eso1230*0.0604839901664346+paf_eso1240*0.0553121677405731+paf_eso1250*0.0378057263805841+
paf_eso1260*0.0214891833993558+paf_eso1270*0.0143280580472796+paf_eso2120*0.0490522296935186+paf_eso2130*0.0496180341400287+paf_eso2140*0.0556350747096706+
paf_eso2150*0.0432399414312312+paf_eso2160*0.0282056505673727+paf_eso2170*0.019914850763446+paf_eso2220*0.0491085726792354+paf_eso2230*0.0474503134338851+
paf_eso2240*0.0549308979277063+paf_eso2250*0.0414098083440804+paf_eso2260*0.0266205001344798+paf_eso2270*0.0229775715775817);

yearpaf_pro=
(paf_pro1120*0.0640749792291515+paf_pro1130*0.0632016056709848+paf_pro1140*0.0589767962053254+paf_pro1150*0.0389303923883878+paf_pro1160*0.0214615354617439+
paf_pro1170*0.012917252516362+paf_pro1220*0.0628548673915807+paf_pro1230*0.0604839901664346+paf_pro1240*0.0553121677405731+paf_pro1250*0.0378057263805841+
paf_pro1260*0.0214891833993558+paf_pro1270*0.0143280580472796+paf_pro2120*0.0490522296935186+paf_pro2130*0.0496180341400287+paf_pro2140*0.0556350747096706+
paf_pro2150*0.0432399414312312+paf_pro2160*0.0282056505673727+paf_pro2170*0.019914850763446+paf_pro2220*0.0491085726792354+paf_pro2230*0.0474503134338851+
paf_pro2240*0.0549308979277063+paf_pro2250*0.0414098083440804+paf_pro2260*0.0266205001344798+paf_pro2270*0.0229775715775817);

yearpaf_lym=
(paf_lym1120*0.0640749792291515+paf_lym1130*0.0632016056709848+paf_lym1140*0.0589767962053254+paf_lym1150*0.0389303923883878+paf_lym1160*0.0214615354617439+
paf_lym1170*0.012917252516362+paf_lym1220*0.0628548673915807+paf_lym1230*0.0604839901664346+paf_lym1240*0.0553121677405731+paf_lym1250*0.0378057263805841+
paf_lym1260*0.0214891833993558+paf_lym1270*0.0143280580472796+paf_lym2120*0.0490522296935186+paf_lym2130*0.0496180341400287+paf_lym2140*0.0556350747096706+
paf_lym2150*0.0432399414312312+paf_lym2160*0.0282056505673727+paf_lym2170*0.019914850763446+paf_lym2220*0.0491085726792354+paf_lym2230*0.0474503134338851+
paf_lym2240*0.0549308979277063+paf_lym2250*0.0414098083440804+paf_lym2260*0.0266205001344798+paf_lym2270*0.0229775715775817);

yearpaf_ova=
(paf_ova1120*0.0640749792291515+paf_ova1130*0.0632016056709848+paf_ova1140*0.0589767962053254+paf_ova1150*0.0389303923883878+paf_ova1160*0.0214615354617439+
paf_ova1170*0.012917252516362+paf_ova1220*0.0628548673915807+paf_ova1230*0.0604839901664346+paf_ova1240*0.0553121677405731+paf_ova1250*0.0378057263805841+
paf_ova1260*0.0214891833993558+paf_ova1270*0.0143280580472796+paf_ova2120*0.0490522296935186+paf_ova2130*0.0496180341400287+paf_ova2140*0.0556350747096706+
paf_ova2150*0.0432399414312312+paf_ova2160*0.0282056505673727+paf_ova2170*0.019914850763446+paf_ova2220*0.0491085726792354+paf_ova2230*0.0474503134338851+
paf_ova2240*0.0549308979277063+paf_ova2250*0.0414098083440804+paf_ova2260*0.0266205001344798+paf_ova2270*0.0229775715775817);

yearpaf_bre=
(paf_bre1120*0.0640749792291515+paf_bre1130*0.0632016056709848+paf_bre1140*0.0589767962053254+paf_bre1150*0.0389303923883878+paf_bre1160*0.0214615354617439+
paf_bre1170*0.012917252516362+paf_bre1220*0.0628548673915807+paf_bre1230*0.0604839901664346+paf_bre1240*0.0553121677405731+paf_bre1250*0.0378057263805841+
paf_bre1260*0.0214891833993558+paf_bre1270*0.0143280580472796+paf_bre2120*0.0490522296935186+paf_bre2130*0.0496180341400287+paf_bre2140*0.0556350747096706+
paf_bre2150*0.0432399414312312+paf_bre2160*0.0282056505673727+paf_bre2170*0.019914850763446+paf_bre2220*0.0491085726792354+paf_bre2230*0.0474503134338851+
paf_bre2240*0.0549308979277063+paf_bre2250*0.0414098083440804+paf_bre2260*0.0266205001344798+paf_bre2270*0.0229775715775817);

yearpaf_cer=
(paf_cer1120*0.0640749792291515+paf_cer1130*0.0632016056709848+paf_cer1140*0.0589767962053254+paf_cer1150*0.0389303923883878+paf_cer1160*0.0214615354617439+
paf_cer1170*0.012917252516362+paf_cer1220*0.0628548673915807+paf_cer1230*0.0604839901664346+paf_cer1240*0.0553121677405731+paf_cer1250*0.0378057263805841+
paf_cer1260*0.0214891833993558+paf_cer1270*0.0143280580472796+paf_cer2120*0.0490522296935186+paf_cer2130*0.0496180341400287+paf_cer2140*0.0556350747096706+
paf_cer2150*0.0432399414312312+paf_cer2160*0.0282056505673727+paf_cer2170*0.019914850763446+paf_cer2220*0.0491085726792354+paf_cer2230*0.0474503134338851+
paf_cer2240*0.0549308979277063+paf_cer2250*0.0414098083440804+paf_cer2260*0.0266205001344798+paf_cer2270*0.0229775715775817);

yearpaf_leu=
(paf_leu1120*0.0640749792291515+paf_leu1130*0.0632016056709848+paf_leu1140*0.0589767962053254+paf_leu1150*0.0389303923883878+paf_leu1160*0.0214615354617439+
paf_leu1170*0.012917252516362+paf_leu1220*0.0628548673915807+paf_leu1230*0.0604839901664346+paf_leu1240*0.0553121677405731+paf_leu1250*0.0378057263805841+
paf_leu1260*0.0214891833993558+paf_leu1270*0.0143280580472796+paf_leu2120*0.0490522296935186+paf_leu2130*0.0496180341400287+paf_leu2140*0.0556350747096706+
paf_leu2150*0.0432399414312312+paf_leu2160*0.0282056505673727+paf_leu2170*0.019914850763446+paf_leu2220*0.0491085726792354+paf_leu2230*0.0474503134338851+
paf_leu2240*0.0549308979277063+paf_leu2250*0.0414098083440804+paf_leu2260*0.0266205001344798+paf_leu2270*0.0229775715775817);
yearpaf_pan=
(paf_pan1120*0.0640749792291515+paf_pan1130*0.0632016056709848+paf_pan1140*0.0589767962053254+paf_pan1150*0.0389303923883878+paf_pan1160*0.0214615354617439+
paf_pan1170*0.012917252516362+paf_pan1220*0.0628548673915807+paf_pan1230*0.0604839901664346+paf_pan1240*0.0553121677405731+paf_pan1250*0.0378057263805841+
paf_pan1260*0.0214891833993558+paf_pan1270*0.0143280580472796+paf_pan2120*0.0490522296935186+paf_pan2130*0.0496180341400287+paf_pan2140*0.0556350747096706+
paf_pan2150*0.0432399414312312+paf_pan2160*0.0282056505673727+paf_pan2170*0.019914850763446+paf_pan2220*0.0491085726792354+paf_pan2230*0.0474503134338851+
paf_pan2240*0.0549308979277063+paf_pan2250*0.0414098083440804+paf_pan2260*0.0266205001344798+paf_pan2270*0.0229775715775817);
yearpaf_lungmale=
(paf_lung1120*0.127323757+paf_lung1130*0.120990504+paf_lung1140*0.11861608+paf_lung1150*0.076167283+paf_lung1160*0.042954094+paf_lung1170*0.03182169+
paf_lung2120*0.09758599+paf_lung2130*0.094259239+paf_lung2140*0.111266908+paf_lung2150*0.083334135+paf_lung2160*0.056105734+paf_lung2170*0.039574587);

yearpaf_lungfemale=
(paf_lung1220*0.127199741+paf_lung1230*0.118280623+paf_lung1240*0.113474275+paf_lung1250*0.075315266+paf_lung1260*0.043942022+paf_lung1270*0.036119564+
paf_lung2220*0.099522223+paf_lung2230*0.09181453+paf_lung2240*0.112015402+paf_lung2250*0.081619425+paf_lung2260*0.054262919+paf_lung2270*0.046434011);

yearpaf_gasmale=
(paf_gas1120*0.127323757+paf_gas1130*0.120990504+paf_gas1140*0.11861608+paf_gas1150*0.076167283+paf_gas1160*0.042954094+paf_gas1170*0.03182169+
paf_gas2120*0.09758599+paf_gas2130*0.094259239+paf_gas2140*0.111266908+paf_gas2150*0.083334135+paf_gas2160*0.056105734+paf_gas2170*0.039574587);

yearpaf_gasfemale=
(paf_gas1220*0.127199741+paf_gas1230*0.118280623+paf_gas1240*0.113474275+paf_gas1250*0.075315266+paf_gas1260*0.043942022+paf_gas1270*0.036119564+
paf_gas2220*0.099522223+paf_gas2230*0.09181453+paf_gas2240*0.112015402+paf_gas2250*0.081619425+paf_gas2260*0.054262919+paf_gas2270*0.046434011);

yearpaf_livmale=
(paf_liv1120*0.127323757+paf_liv1130*0.120990504+paf_liv1140*0.11861608+paf_liv1150*0.076167283+paf_liv1160*0.042954094+paf_liv1170*0.03182169+
paf_liv2120*0.09758599+paf_liv2130*0.094259239+paf_liv2140*0.111266908+paf_liv2150*0.083334135+paf_liv2160*0.056105734+paf_liv2170*0.039574587);

yearpaf_livfemale=
(paf_liv1220*0.127199741+paf_liv1230*0.118280623+paf_liv1240*0.113474275+paf_liv1250*0.075315266+paf_liv1260*0.043942022+paf_liv1270*0.036119564+
paf_liv2220*0.099522223+paf_liv2230*0.09181453+paf_liv2240*0.112015402+paf_liv2250*0.081619425+paf_liv2260*0.054262919+paf_liv2270*0.046434011);

yearpaf_crcmale=
(paf_crc1120*0.127323757+paf_crc1130*0.120990504+paf_crc1140*0.11861608+paf_crc1150*0.076167283+paf_crc1160*0.042954094+paf_crc1170*0.03182169+
paf_crc2120*0.09758599+paf_crc2130*0.094259239+paf_crc2140*0.111266908+paf_crc2150*0.083334135+paf_crc2160*0.056105734+paf_crc2170*0.039574587);

yearpaf_crcfemale=
(paf_crc1220*0.127199741+paf_crc1230*0.118280623+paf_crc1240*0.113474275+paf_crc1250*0.075315266+paf_crc1260*0.043942022+paf_crc1270*0.036119564+
paf_crc2220*0.099522223+paf_crc2230*0.09181453+paf_crc2240*0.112015402+paf_crc2250*0.081619425+paf_crc2260*0.054262919+paf_crc2270*0.046434011);

yearpaf_esomale=
(paf_eso1120*0.127323757+paf_eso1130*0.120990504+paf_eso1140*0.11861608+paf_eso1150*0.076167283+paf_eso1160*0.042954094+paf_eso1170*0.03182169+
paf_eso2120*0.09758599+paf_eso2130*0.094259239+paf_eso2140*0.111266908+paf_eso2150*0.083334135+paf_eso2160*0.056105734+paf_eso2170*0.039574587);

yearpaf_esofemale=
(paf_eso1220*0.127199741+paf_eso1230*0.118280623+paf_eso1240*0.113474275+paf_eso1250*0.075315266+paf_eso1260*0.043942022+paf_eso1270*0.036119564+
paf_eso2220*0.099522223+paf_eso2230*0.09181453+paf_eso2240*0.112015402+paf_eso2250*0.081619425+paf_eso2260*0.054262919+paf_eso2270*0.046434011);
yearpaf_promale=
(paf_pro1120*0.127323757+paf_pro1130*0.120990504+paf_pro1140*0.11861608+paf_pro1150*0.076167283+paf_pro1160*0.042954094+paf_pro1170*0.03182169+
paf_pro2120*0.09758599+paf_pro2130*0.094259239+paf_pro2140*0.111266908+paf_pro2150*0.083334135+paf_pro2160*0.056105734+paf_pro2170*0.039574587);

yearpaf_profemale=
(paf_pro1220*0.127199741+paf_pro1230*0.118280623+paf_pro1240*0.113474275+paf_pro1250*0.075315266+paf_pro1260*0.043942022+paf_pro1270*0.036119564+
paf_pro2220*0.099522223+paf_pro2230*0.09181453+paf_pro2240*0.112015402+paf_pro2250*0.081619425+paf_pro2260*0.054262919+paf_pro2270*0.046434011);

yearpaf_lymmale=
(paf_lym1120*0.127323757+paf_lym1130*0.120990504+paf_lym1140*0.11861608+paf_lym1150*0.076167283+paf_lym1160*0.042954094+paf_lym1170*0.03182169+
paf_lym2120*0.09758599+paf_lym2130*0.094259239+paf_lym2140*0.111266908+paf_lym2150*0.083334135+paf_lym2160*0.056105734+paf_lym2170*0.039574587);

yearpaf_lymfemale=
(paf_lym1220*0.127199741+paf_lym1230*0.118280623+paf_lym1240*0.113474275+paf_lym1250*0.075315266+paf_lym1260*0.043942022+paf_lym1270*0.036119564+
paf_lym2220*0.099522223+paf_lym2230*0.09181453+paf_lym2240*0.112015402+paf_lym2250*0.081619425+paf_lym2260*0.054262919+paf_lym2270*0.046434011);

yearpaf_bremale=
(paf_bre1120*0.127323757+paf_bre1130*0.120990504+paf_bre1140*0.11861608+paf_bre1150*0.076167283+paf_bre1160*0.042954094+paf_bre1170*0.03182169+
paf_bre2120*0.09758599+paf_bre2130*0.094259239+paf_bre2140*0.111266908+paf_bre2150*0.083334135+paf_bre2160*0.056105734+paf_bre2170*0.039574587);

yearpaf_brefemale=
(paf_bre1220*0.127199741+paf_bre1230*0.118280623+paf_bre1240*0.113474275+paf_bre1250*0.075315266+paf_bre1260*0.043942022+paf_bre1270*0.036119564+
paf_bre2220*0.099522223+paf_bre2230*0.09181453+paf_bre2240*0.112015402+paf_bre2250*0.081619425+paf_bre2260*0.054262919+paf_bre2270*0.046434011);

yearpaf_ovamale=
(paf_ova1120*0.127323757+paf_ova1130*0.120990504+paf_ova1140*0.11861608+paf_ova1150*0.076167283+paf_ova1160*0.042954094+paf_ova1170*0.03182169+
paf_ova2120*0.09758599+paf_ova2130*0.094259239+paf_ova2140*0.111266908+paf_ova2150*0.083334135+paf_ova2160*0.056105734+paf_ova2170*0.039574587);

yearpaf_ovafemale=
(paf_ova1220*0.127199741+paf_ova1230*0.118280623+paf_ova1240*0.113474275+paf_ova1250*0.075315266+paf_ova1260*0.043942022+paf_ova1270*0.036119564+
paf_ova2220*0.099522223+paf_ova2230*0.09181453+paf_ova2240*0.112015402+paf_ova2250*0.081619425+paf_ova2260*0.054262919+paf_ova2270*0.046434011);

yearpaf_cermale=
(paf_cer1120*0.127323757+paf_cer1130*0.120990504+paf_cer1140*0.11861608+paf_cer1150*0.076167283+paf_cer1160*0.042954094+paf_cer1170*0.03182169+
paf_cer2120*0.09758599+paf_cer2130*0.094259239+paf_cer2140*0.111266908+paf_cer2150*0.083334135+paf_cer2160*0.056105734+paf_cer2170*0.039574587);

yearpaf_cerfemale=
(paf_cer1220*0.127199741+paf_cer1230*0.118280623+paf_cer1240*0.113474275+paf_cer1250*0.075315266+paf_cer1260*0.043942022+paf_cer1270*0.036119564+
paf_cer2220*0.099522223+paf_cer2230*0.09181453+paf_cer2240*0.112015402+paf_cer2250*0.081619425+paf_cer2260*0.054262919+paf_cer2270*0.046434011);

yearpaf_leumale=
(paf_leu1120*0.127323757+paf_leu1130*0.120990504+paf_leu1140*0.11861608+paf_leu1150*0.076167283+paf_leu1160*0.042954094+paf_leu1170*0.03182169+
paf_leu2120*0.09758599+paf_leu2130*0.094259239+paf_leu2140*0.111266908+paf_leu2150*0.083334135+paf_leu2160*0.056105734+paf_leu2170*0.039574587);

yearpaf_leufemale=
(paf_leu1220*0.127199741+paf_leu1230*0.118280623+paf_leu1240*0.113474275+paf_leu1250*0.075315266+paf_leu1260*0.043942022+paf_leu1270*0.036119564+
paf_leu2220*0.099522223+paf_leu2230*0.09181453+paf_leu2240*0.112015402+paf_leu2250*0.081619425+paf_leu2260*0.054262919+paf_leu2270*0.046434011);

yearpaf_panmale=
(paf_pan1120*0.127323757+paf_pan1130*0.120990504+paf_pan1140*0.11861608+paf_pan1150*0.076167283+paf_pan1160*0.042954094+paf_pan1170*0.03182169+
paf_pan2120*0.09758599+paf_pan2130*0.094259239+paf_pan2140*0.111266908+paf_pan2150*0.083334135+paf_pan2160*0.056105734+paf_pan2170*0.039574587);

yearpaf_panfemale=
(paf_pan1220*0.127199741+paf_pan1230*0.118280623+paf_pan1240*0.113474275+paf_pan1250*0.075315266+paf_pan1260*0.043942022+paf_pan1270*0.036119564+
paf_pan2220*0.099522223+paf_pan2230*0.09181453+paf_pan2240*0.112015402+paf_pan2250*0.081619425+paf_pan2260*0.054262919+paf_pan2270*0.046434011);
/*** standardized by proportion of Census2010 ***/
run;



/*CNHS_PAF*/
data cnhsdata.paflevel;
set  overallpaf;
if group=23 then delete;
if year in (2023,2025,2027,2029,2031,2033,2035) then delete;
keep  year group yearpaf_lung yearpaf_gas yearpaf_liv yearpaf_crc yearpaf_eso yearpaf_pro yearpaf_lym yearpaf_bre yearpaf_ova yearpaf_cer yearpaf_leu yearpaf_pan
yearpaf_lungmale yearpaf_lungfemale yearpaf_gasmale yearpaf_gasfemale yearpaf_livmale yearpaf_livfemale yearpaf_crcmale yearpaf_crcfemale
yearpaf_esomale yearpaf_esofemale yearpaf_promale yearpaf_profemale yearpaf_lymmale yearpaf_lymfemale yearpaf_bremale yearpaf_brefemale yearpaf_ovamale
yearpaf_ovafemale yearpaf_cermale yearpaf_cerfemale yearpaf_leumale yearpaf_leufemale yearpaf_panmale yearpaf_panfemale;

proc sort;
by  group year;
proc print;
run;
/*Other factors were calculated in the same way as the risk factors in CNHS*/
data pafall;
set cnhsdata.paflevel paf_other;
array numtmp _numeric_;
        do over numtmp;
                numtmp=coalesce(numtmp,0);
        end;
proc print;
run;
data cnhsdata.pafallgroup; set pafall;
proc print;run;
data pafall1; set cnhsdata.pafallgroup;
lagyear=year+15;
drop year;
run;
data cnhsdata.pafallgroup1; set pafall1;
year=lagyear;drop lagyear;
if group=3 or  group=10 or  group=6 or  group=9 or  group=20 or  group=7 or group=16 or group=17 then delete;
if year in (2001,2003,2037,2039,2041,2043,2045) then delete;
run;
PROC EXPORT DATA= cnhsdata.pafallgroup1
            OUTFILE= '********'
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
/*Output the transpose file*/
data a1 ; set cnhsdata.pafallgroup1; 
drop yearpaf_lung yearpaf_liv	yearpaf_gas	yearpaf_crc	yearpaf_eso	yearpaf_pro	yearpaf_lym	yearpaf_ova	yearpaf_bre	yearpaf_cer	yearpaf_leu	
yearpaf_pan;
if year in (2006,2008,2037,2039,2041,2043,2045) then delete;
proc sort;
by group;
run;
PROC TRANSPOSE DATA=a1 OUT=pafall1;
  BY group;   
  ID year;
  VAR 	yearpaf_lungmale	yearpaf_lungfemale	yearpaf_gasmale	yearpaf_gasfemale	yearpaf_livmale	yearpaf_livfemale	yearpaf_crcmale	
yearpaf_crcfemale	yearpaf_esomale	yearpaf_esofemale	yearpaf_promale	yearpaf_profemale	yearpaf_lymmale	yearpaf_lymfemale	yearpaf_bremale	
yearpaf_brefemale	yearpaf_ovamale	yearpaf_ovafemale	yearpaf_cermale	yearpaf_cerfemale	yearpaf_leumale	yearpaf_leufemale	yearpaf_panmale	
yearpaf_panfemale
;
proc print;run;
data a2;set pafall1; 
if _2012=0 and _2024=0 then delete;
array b[9] _2012 _2015 _2019 _2021 _2024 _2026 _2030 _2032 _2035;  
   do i=1 to 9;    
      b(i)=abs(b(i));
   end;
   run;
proc sort ; by  _NAME_  descending _2012 ;
proc print;
run;
ods rtf close;
PROC EXPORT DATA=a2
            OUTFILE= 'overallpaf1.csv'
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;



*male;
%macro paffood(food);
data pafs&food;
set cnhsdata.pafgroup;
if group=&food;
paf_lung&food=ABS(yearpaf_lungmale*0.01);
paf_liv&food=ABS(yearpaf_livmale*0.01);
paf_gas&food=ABS(yearpaf_gasmale*0.01);
paf_crc&food=ABS(yearpaf_crcmale*0.01);
paf_eso&food=ABS(yearpaf_esomale*0.01);
paf_pro&food=ABS(yearpaf_promale*0.01);
paf_lym&food=ABS(yearpaf_lymmale*0.01);
paf_ova&food=ABS(yearpaf_ovamale*0.01);
paf_bre&food=ABS(yearpaf_bremale*0.01);
paf_cer&food=ABS(yearpaf_cermale*0.01);
paf_leu&food=ABS(yearpaf_leumale*0.01);
paf_pan&food=ABS(yearpaf_panmale*0.01);
keep year  paf_lung&food paf_liv&food paf_gas&food paf_crc&food paf_eso&food paf_pro&food  paf_lym&food paf_bre&food paf_cer&food paf_leu&food paf_ova&food paf_pan&food; 
proc sort;
by year;
run;
%mend;

%paffood(1); %paffood(2); %paffood(4);%paffood(5);
%paffood(8);%paffood(11);%paffood(12);%paffood(13);%paffood(14);%paffood(15);
%paffood(18);%paffood(19);%paffood(21);%paffood(22);%paffood(23);;%paffood(24);

data pafallmale;
merge pafs1 pafs2 pafs4 pafs5 pafs24 pafs8  pafs11 pafs12 pafs13 pafs14 pafs15 pafs18 pafs19  pafs21 pafs22 pafs23;
by year;
paf_lung_all=(1-(1-paf_lung1)*(1-paf_lung2)*(1-paf_lung4)*(1-paf_lung5)*(1-paf_lung24)*(1-paf_lung8)*((1-paf_lung11)*(1-paf_lung12)*
(1-paf_lung13)*(1-paf_lung14)*(1-paf_lung15)*(1-paf_lung18) *(1-paf_lung19)*(1-paf_lung21)*
(1-paf_lung22) *(1-paf_lung23)))*100 ;
paf_liv_all=(1-(1-paf_liv1)*(1-paf_liv2)*(1-paf_liv4)*(1-paf_liv5)*(1-paf_liv24)*(1-paf_liv8)
              *(1-paf_liv11)*(1-paf_liv12)*(1-paf_liv13)*(1-paf_liv14)*(1-paf_liv15)*
              (1-paf_liv18) *(1-paf_liv19)*(1-paf_liv21)*(1-paf_liv22) *(1-paf_liv23))*100 ;
paf_gas_all=(1-(1-paf_gas1)*(1-paf_gas2)*(1-paf_gas4)*(1-paf_gas5*0.5659)*(1-paf_gas24)*(1-paf_gas8)
              *(1-paf_gas11)*(1-paf_gas12)*(1-paf_gas13*0.4341)*(1-paf_gas14)*(1-paf_gas15)*
			   *(1-paf_gas18) *(1-paf_gas19)*(1-paf_gas21)*(1-paf_gas22) *(1-paf_gas23))*100 ;
paf_crc_all=(1-(1-paf_crc1)*(1-paf_crc2)*(1-paf_crc4)*(1-paf_crc5)*(1-paf_crc24)*(1-paf_crc8)
               *(1-paf_crc11)*(1-paf_crc12)*(1-paf_crc13)*(1-paf_crc14)*(1-paf_crc15)
			   *(1-paf_crc18) *(1-paf_crc19)*(1-paf_crc21)*(1-paf_crc22) *(1-paf_crc23))*100 ;
paf_eso_all=(1-(1-paf_eso1*0.8642)*(1-paf_eso2)*(1-paf_eso4)*(1-paf_eso5*0.8642)*(1-paf_eso24)*(1-paf_eso8)
               *(1-paf_eso11)*(1-paf_eso12)*(1-paf_eso13*0.1045)*(1-paf_eso14)*(1-paf_eso15)
			   *(1-paf_eso18) *(1-paf_eso19)*(1-paf_eso21)*(1-paf_eso22) *(1-paf_eso23))*100 ;
paf_pro_all=(1-(1-paf_pro1)*(1-paf_pro2)*(1-paf_pro4)*(1-paf_pro5)*(1-paf_pro24)*(1-paf_pro8)
               *(1-paf_pro11)*(1-paf_pro12)*(1-paf_pro13)*(1-paf_pro14)*(1-paf_pro15)
			   *(1-paf_pro18) *(1-paf_pro19)*(1-paf_pro21)*(1-paf_pro22) *(1-paf_pro23))*100 ;
paf_lym_all=(1-(1-paf_lym1)*(1-paf_lym2)*(1-paf_lym4)*(1-paf_lym5)*(1-paf_lym24)*(1-paf_lym8)
               *(1-paf_lym11)*(1-paf_lym12)*(1-paf_lym13)*(1-paf_lym14)*(1-paf_lym15)
			   *(1-paf_lym18) *(1-paf_lym19)*(1-paf_lym21) *(1-paf_lym22) *(1-paf_lym23))*100 ;
paf_ova_all=(1-(1-paf_ova1)*(1-paf_ova2)*(1-paf_ova4)*(1-paf_ova5)*(1-paf_ova24)*(1-paf_ova8)
               *(1-paf_ova11)*(1-paf_ova12)*(1-paf_ova13)*(1-paf_ova14)*(1-paf_ova15)
			   *(1-paf_ova18) *(1-paf_ova19)*(1-paf_ova21)*(1-paf_ova22) *(1-paf_ova23))*100 ;
paf_bre_all=(1-(1-paf_bre1)*(1-paf_bre2)*(1-paf_bre4)*(1-paf_bre5)*(1-paf_bre24)*(1-paf_bre8)
              *(1-paf_bre11)*(1-paf_bre12)*(1-paf_bre13)*(1-paf_bre14)*(1-paf_bre15)
			   *(1-paf_bre18) *(1-paf_bre19)*(1-paf_bre21)*(1-paf_bre22) *(1-paf_bre23))*100 ;
paf_cer_all=(1-(1-paf_cer1)*(1-paf_cer2)*(1-paf_cer4)*(1-paf_cer5)*(1-paf_cer24)*(1-paf_cer8)
              *(1-paf_cer11)*(1-paf_cer12)*(1-paf_cer13)*(1-paf_cer14)*(1-paf_cer15)
			   *(1-paf_cer18) *(1-paf_cer19)*(1-paf_cer21)*(1-paf_cer22) *(1-paf_cer23))*100 ;
paf_leu_all=(1-(1-paf_leu1)*(1-paf_leu2)*(1-paf_leu4)*(1-paf_leu5)*(1-paf_leu24)*(1-paf_leu8)
               *(1-paf_leu11)*(1-paf_leu12)*(1-paf_leu13)*(1-paf_leu14)*(1-paf_leu15)
			   *(1-paf_leu18) *(1-paf_leu19)*(1-paf_leu21)*(1-paf_leu22) *(1-paf_leu23))*100 ;
paf_pan_all=(1-(1-paf_pan1)*(1-paf_pan2)*(1-paf_pan4)*(1-paf_pan5)*(1-paf_pan24)*(1-paf_pan8)
               *(1-paf_pan11)*(1-paf_pan12)*(1-paf_pan13)*(1-paf_pan14)*(1-paf_pan15)
			   *(1-paf_pan18) *(1-paf_pan19)*(1-paf_pan21)*(1-paf_pan22) *(1-paf_pan23))*100 ;
keep year paf_lung_all paf_liv_all paf_gas_all paf_crc_all paf_eso_all paf_pro_all paf_lym_all paf_ova_all paf_bre_all paf_cer_all
paf_leu_all paf_pan_all;
run;
proc print;
run;
ods rtf close;
PROC EXPORT DATA= pafallmale
            OUTFILE= '*******'
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
