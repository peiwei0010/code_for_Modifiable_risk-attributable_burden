
libname cnhsdata '**********'; run;
options formchar=' _________';

proc format;
value Canfoodgroup
                   1='Fruit(g/day)'    2='Vegetable(g/day)'           3='Nut(g/day)'
		   		       				             4='Red meat(g/day)' 5='Processed meat(g/day)'
                   6='dairy(g/day)'    7='Ate pickled vegetables(mg/day)'  8='alcohol (g/day)'
                   9='Fiber (g/day)'   10='Calcium (mg/day)' 11='Whole grain(g/day)'
                  12='bean(g/day)' 13='bmi(kg/m2)'    14='Ever smoking(%)'  15='physical inactive(%)'
                    16=' diabete(%)' 17=' HIGH BLOOD PRESSURE(%)';
value agegroup 20='20-29' 30='30-39' 40='40-49' 50='50-59' 60='60-69' 70='70+';
value sex      1='male' 2='female';
value urbanrural 1='urban' 2='rural';
run;


**********************************************************************************************
*                                   Food/Nutrients 7-17                                      *
**********************************************************************************************;
***********************************
*            fct  dataset         *
***********************************;
proc import datafile="***********"
out=cnhsdata.fct91
dbms=xlsx replace;
run;

data fct91;
set cnhsdata.fct91;/*Food composition table data information*/
kcal=Energ_kc;
carbo=cho;                        /* carbohydrate including fiber*/
fiber=Fiber;
if alc_v=. then alc_v=0;
if alc_w=. then alc_w=0;
array check{8} edible kcal protein fat carbo na ca fiber;
do i=1 to 8; if check{i}=. then check{i}=0; end;        /*new array*/

keep code edible kcal protein fat carbo alc_v alc_w na ca fiber;
proc sort;
by code;
run; 

proc import datafile="D:\PAF\lj\data\2002fct.xlsx"
out=cnhsdata.fct02
dbms=xlsx replace;
run;
data fct02;
set cnhsdata.fct02;/*Food composition table data information*/
kcal=Energ_kc;
carbo=cho;                        /* carbohydrate including fiber*/
fiber=Fiber;
if alc_v=. then alc_v=0;
if alc_w=. then alc_w=0;
array check{8} edible kcal protein fat carbo na ca fiber;
do i=1 to 8; if check{i}=. then check{i}=0; end;         /*give na 0*/

keep newcode edible kcal protein fat carbo alc_v alc_w na ca fiber;
proc sort;
by newcode;
run;

***********************************
*            food dataset         *
***********************************;
/*The combined dietary data were chnsdata.nutr*/


proc sort data=cnhsdata.nutr3_00 out=nutr;
by HHID COMMID WAVE LINE;run;

data food;
set nutr;
if wave in (1997,2000,2004,2006,2009,2011);
drop t1 t2;/*t1 province t2 region*/
proc sort;
by wave IDIND;
run;



***********************************
*            main dataset         *
***********************************;
/***read in sex, age, urban and rural***/
data a1; set cnhsdata.mast_pub_12; keep IDind gender; proc sort; by  IDind; run;
data a2; set cnhsdata.surveys_pub_12; keep IDind wave age urban t1 t2;  proc sort;  by IDind; run;
data basic;
merge a2(in=mst) a1;
by IDind;
if wave in (1997,2000,2004,2006,2009,2011);
urbanrural=t2;
proc freq;
tables t1 t2 gender urbanrural;                    /*Province, urban and rural situation, and gender were displayed*/
proc sort;
by  wave IDind;
run;


****************************************
*            foods for >=20yrs         *
****************************************;

data foodmain;           /*The original nutr and mast_pub_12 and surveys_pub_12 data were merged*/
merge food(in=mst) basic;
by wave IDind;
if mst;
proc freq;
tables t1 t2 gender urbanrural;
run;

data food20;
set foodmain;
if age>=20;
if age<30 then agegroup=20;
else if age<40 then agegroup=30;
else if age<50 then agegroup=40;
else if age<60 then agegroup=50;
else if age<70 then agegroup=60;
else agegroup=70;
proc freq;
tables wave;
tables gender;
tables agegroup;
run;


****************************************
*            merge datasets            *
****************************************;

data fooda;
set food20;
if wave in (1997,2000);
if 1000<foodcode<28015;
code=foodcode;
if code=. then delete;
proc sort;
by code;
run;

data fcta;
set fct91;
if code=. then delete;
proc sort;
by code;
run;

data foodanutra;
merge fooda(in=msta) fcta;
by code;
if msta;
run;


data foodb;
set food20;
if wave in (2004,2006,2009,2011);
if 10000<foodcode<220000;
newcode=foodcode;
if newcode=. then delete;
proc sort;
by newcode;
run;


data fctb;
set fct02;
if newcode=. then delete;
proc sort;
by newcode;
run;

data foodanutrb;
merge foodb(in=msta) fctb;
by newcode;
if msta;
run;


data foodfct;
set foodanutra foodanutrb;/**/
RUN;
/*V35:TOTAL # OF "PERSON-DAYS" FOR THE 3 DAYS*/
/*V35a:# OF "PERSON-DAYS" FOR THIS DAY (VD)*/
/*V39:AMOUNT EATEN, MEASURE VARIES BY SURVEY YR*/
data  foodfct1;
set foodfct;
if wave in (1997,2000) then foodamounts=v39*50;/*1Á½=50¿Ë*/
if wave in (2004,2006,2009,2011)  then foodamounts=v39;
if v35a=. or v35a=0 then personday=1;
else personday=v35a;      
run;

proc freq data=foodfct1;table v35a;table personday;run;


data foods;
set foodfct1;

IDDAY=IDind*10+vd;
day=vd;
if carbo=0 then carbo=.; /***for whole grain, no zero for dividing*/
foodwt=foodamounts;

if wave=2011 then do;
                    if v39b=1 and 0<EDIBLE=<100 then edit=EDIBLE/100; else edit=1;
		       	      	   end;
						 else if 0<EDIBLE=<100 then edit=EDIBLE/100; else edit=1;

if wave in (1997,2000)                       then do;

if 10001=<foodcode=<10012 or 10013=<foodcode=<10025 or 10027=<foodcode=<10029 or 10032=<foodcode=<10035 or 10039=<foodcode=<10043 or 10046=foodcode or 10049=<foodcode=<10062
or 10064=<foodcode=<10105 or 10108=<foodcode=<10165 or 10169=<foodcode=<10183 or 10187=<foodcode=<10205
or 10207=<foodcode=<10235 or 10239=<foodcode=<10250
or 6001=<foodcode=<6005 or foodcode=6027 or foodcode=6054 or foodcode=6071 or foodcode=6069 or foodcode=6070 or  6104=<foodcode=<6127 or foodcode=6136
        then foodgroup=1; *fruit;              


   else if 3001=<foodcode=<3087 or 4010=<foodcode=<4017  or 
        4042=<foodcode=<4177 or 5001=<foodcode=<5331 or 6006=<foodcode=<6027 or 6028=<foodcode=<6053 or 6055=<foodcode=<6068
or 6072=<foodcode=<6103 or 6129=<foodcode=<6135 or
        7001=<foodcode=<7067 
        then foodgroup=2; *vegetable;

   else if 11001=<foodcode=<11068
        then foodgroup=3; *nut;

   else if code=12030 or code=12031 or 12038=<code=<12041 or 12044=<code=<12088or 12091=<code=<12110 or 12114=<code=<12168 or
 12170=<code=<12260 or 12268=<code=<12315 
        then foodgroup=4;  *red meat;

   else if 12001=<code=<12029 or 12032=<code=<12037 or code=12042 or code=12043 or code=12089 or   code=12090 or 12111=<code=<12113 or code=12169  or 12261=<code=<12267 or code=12128 or 12136=<code=<12138
or code=13015  or code=13024 or code=13052 or code=13068 or code=13073
		   then foodgroup=5; *processed meat;

    else if (14000=<foodcode=<14006 or 14008=<foodcode<15000)
         then do; foodgroup=6; foodwt=foodamounts*protein/3.0; end; *dairy, convert to weight of milk equivalent;

	 else if 8001=<foodcode=<8999 then foodgroup=7; *salted vegetables;

	 else if 23000=<foodcode=<23100 then do; foodgroup=8;foodwt=foodamounts*alc_w/100; end; *alcohol;
	 else if 1001=<foodcode=<1011 or foodcode=1013 or foodcode=1019 or foodcode=1050 or 
          1069=<foodcode=<1075 or foodcode=1081 or 1088=<foodcode=<1090 or 1092=<foodcode=<1094 or 
          1096=<foodcode=<1104 or foodcode=1111 or 1117=<foodcode=<1120 or 1125=<foodcode=<1126 or 
          1128=<foodcode=<1129 or 1136=<foodcode=<1138 or 1141=<foodcode=<1147 or 1150=<foodcode=<1152 or 
          foodcode=1167 or foodcode=1175 or 1178=<foodcode=<1185 or 1188=<foodcode=<1207 or 
          1209=<foodcode=<1212 or foodcode=1214 or 1216=<foodcode=<1218 or 1222=<foodcode=<1226 or 
          1229 =<foodcode=<1232 or 1235=<foodcode=<1268 or foodcode=1270 then foodgroup=11; *whole grain;

	 else if 2001=<foodcode=<2011 or 2015=<foodcode=<2076 or 2091=<foodcode=<2126 or 2133=<foodcode=<2149 or 2151=<foodcode=<2175  
or 2187=<foodcode=<2217  then foodgroup=12; *bean*;
   else foodgroup=22;
                                                  end;


if wave in (2004,2006,2009,2011)                 then do;

    if 61101=<foodcode=<61118 or 61201=<foodcode=<61227 or foodcode=61301 or 61903=<foodcode=<61906 or 62101=<foodcode=<62113 or 62201=<foodcode=<62204
         or foodcode=62301 or 62304=<foodcode=<62308 or 62901=<foodcode=<62903 or 63101=<foodcode=<63106 or 63201=<foodcode=<63204
	      or 63901=<foodcode=<63903 or 63907=<foodcode=<63910 or 64101=<foodcode=<64210 or foodcode=64301 or foodcode=64302 or
	           65001=<foodcode=<65006 or 65008=<foodcode=<65018 or 66101=<foodcode=<66205
         then foodgroup=1; /*fruit*/

    else if 41101=<foodcode=<46009 or 48001=<foodcode=<48080 then foodgroup=2;/*vegetable*/

    else if 71001=<foodcode<80000
         then foodgroup=3; *nuts;

    else if 81101=<foodcode=<81209 or foodcode=81211 or foodcode=81212 or 81310=<foodcode=<81313 or 82101=<foodcode=<82108 or foodcode=82205 or 82206=<foodcode=<82209
         or 83101=<foodcode=<83110 or 83205=<foodcode=<83209 or foodcode=84101 or foodcode=84201
	     or foodcode=85101 or foodcode=85201 or foodcode=89001 or foodcode=89004 or foodcode=89005
         then foodgroup=4; *redmeat;

    else if 81301=<foodcode<81422 or 82301=<foodcode=<82306 or 83301=<foodcode=83308 or 84301=<foodcode=<84303 or foodcode=85301
    	  then foodgroup=5; *processed meat;

    else if (101100=<foodcode<101300 or 102100=<foodcode<110000)
         then do; foodgroup=6; foodwt=foodamounts*protein/3.0; end; /*dairy convert to weight of milk equivalent */

    else if 205001=<foodcode=<205035 then foodgroup=7; /* salted vegetable */

    else if 171000=<foodcode=<173111 then do; foodgroup=8; foodwt=foodamounts*alc_w/100; end; *alcohol;
	else if foodcode=11101 or foodcode=11201 or 11203=<foodcode=<11206 or foodcode=11302 or
          foodcode=11306 or foodcode=11311 or 11314=<foodcode=<11317or foodcode=11401 or 
          11403=<foodcode=<11408 or 11410=<foodcode=<11411 or foodcode=11502 or foodcode=12106 or
          foodcode=12108 or foodcode=12204 or 12211=<foodcode=<12212 or 12214=<foodcode=<12215 or 
          12303=<foodcode=<12304 or foodcode=12408 or 12410=<foodcode=<12411 or 13101=<foodcode=<13110 
          or foodcode=13201 or foodcode=14101 or 14201=<foodcode=<14202 or foodcode=15101 or 
          foodcode=15104 or 15201=<foodcode=<15202 or 19001=<foodcode=<19011 then foodgroup=11;

 else if 31101=<foodcode=<39902 then foodgroup=12; *bean*;
    else foodgroup=22;
                                                  end;


if kcal=. then kcal=0;
if poly=. then poly=0;
if na=. then na=0;
if ca=. then ca=0;
if fiber=. then fiber=0;
if somega3=. then somega3=0;
if poly=. then poly=0;

foodkcal=foodamounts/personday*edit*kcal/100;
foodpoly=foodamounts/personday*edit*poly/100;
foodna=foodamounts/personday*edit*na/100;
foodca=foodamounts/personday*edit*ca/100;
foodsomega3=foodamounts/personday*edit*somega3/100;
foodfiber=foodamounts/personday*edit*fiber/100;
foodweight=foodwt/personday*edit;

proc sort;
by wave IDDAY;
run;



************************************************************
*                foods for each day for 1-3 days           *
************************************************************;


data g;
set foods;
by wave IDDAY;                                    /*temporary variable*/
if first.IDDAY then do; foodday=0;               
                        kcalday=0;
                        polyday=0;
                        somega3day=0;
                        fiberday=0;
							naday=0;
												caday=0;
															 end;
foodday+foodweight;
kcalday+foodkcal;
polyday+foodpoly;
somega3day+foodsomega3;
fiberday+foodfiber;
naday+foodna;
caday+foodca;
if last.IDDAY then output;
format foodday 6.0 kcalday 6.0 polyday 5.1 somega3day 5.2 fiberday 5.1 naday 6.0 caday 6.0;
run;

proc means;
var foodday kcalday polyday somega3day fiberday naday caday;/*varible*/
class wave;
run;

data nutr20160526;
set g;
naday=naday/1000;
keep wave IDDAY  IDind day age agegroup t1 t2 gender urbanrural
     foodday kcalday polyday  somega3day  fiberday naday caday;
run;

data ddd;
set foods;
proc sort;
by wave IDDAY  foodgroup;
run;

proc freq data=ddd;
tables foodgroup;
run;

data dd;
set ddd;
by wave IDDAY foodgroup;
if first.foodgroup then do; groupfood=0; groupkcal=0; end; 
groupfood+foodweight;
groupkcal+foodkcal;
if last.foodgroup then output;
run;

%macro food(a); 
data food&a;
set dd;
if foodgroup=&a;
foodday&a=groupfood;
kcalday&a=groupkcal;
format foodday&a 7.1 kcalday&a 7.1;
keep wave IDDAY foodday&a kcalday&a;
proc sort;
by wave IDDAY;
run;
%mend;

%food(1);%food(2);%food(3);%food(4);%food(5);%food(6);%food(7);%food(8);%food(11); %food(12); %food(22) ;


data kcal;
set nutr20160526;
proc sort;
by wave IDDAY;
run;

data foodnutr20160526;
merge kcal(in=mst) food1 food2 food3 food4 food5 food6 food7 food8 food11 food12 food22;
by  wave IDDAY;
if mst;
if foodday1=.  then foodday1=0;
if foodday2=.  then foodday2=0;
if foodday3=.  then foodday3=0;
if foodday4=.  then foodday4=0;
if foodday5=.  then foodday5=0;
if foodday6=.  then foodday6=0;
if foodday7=.  then foodday7=0;
if foodday8=.  then foodday8=0;
if foodday11=.  then foodday11=0;
if foodday12=.  then foodday12=0;
if foodday22=. then foodday22=0;

if kcalday1=.  then kcalday1=0;
if kcalday2=.  then kcalday2=0;
if kcalday3=.  then kcalday3=0;
if kcalday4=.  then kcalday4=0;
if kcalday5=.  then kcalday5=0;
if kcalday6=.  then kcalday6=0;
if kcalday7=.  then kcalday7=0;
if kcalday8=.  then kcalday8=0;
if kcalday11=.  then kcalday11=0;
if kcalday12=.  then kcalday12=0;
if kcalday22=. then kcalday22=0;

check=foodday1+foodday2+foodday3+foodday4+foodday5+foodday6+foodday7+foodday8+foodday11+foodday12+foodday22;
check2=kcalday1+kcalday2+kcalday3+kcalday4+kcalday5+kcalday6+kcalday7+kcalday8+kcalday11+kcalday12+kcalday22;

foodday7=foodday7*1000; *g to mg;
proc means mean std;
var check foodday check2 kcalday;
run;

proc freq;
tables kcalday caday naday fiberday foodday1 foodday2 foodday3 foodday4 foodday5 foodday6 foodday7 foodday8 foodday11 foodday12;
run;


data foodnutr20160526;
set foodnutr20160526;

if kcalday<100   then delete;            /* 0.22% */
if kcalday>10000 then delete;            /* 0.4% */

if caday>5000    then naday=5000;        /*0.11%*/
if naday>100     then naday=100;         /*0.33%*/
if fiberday>200  then fiberday=200;      /*0.06%*/
if foodday1>1000 then foodday1=1000;     /* fruit 0.08%*/
if foodday2>2000 then foodday2=2000;     /* vegetable 0.15%*/
if foodday3>300  then foodday3=300;      /* nuts 0.08%*/
if foodday4>1000 then foodday4=1000;     /* redmeat 0.11%*/
if foodday5>200  then foodday5=200;      /* processedmeat 0.21%*/
if foodday6>1000 then foodday6=1000;     /* dairy 0.08%*/
if foodday7>250 then foodday7=250;     /* pivkled vegetable 0.1%*/
if foodday8>200  then foodday8=200;      /*alcohol 0.13%*/
if foodday11>1000 then foodday11=1000;     /* whole grain 0.27%*/
if foodday12>1000 then foodday12=1000;
somega3day=somega3day*1000;
pufaday=polyday*9/kcalday*100;
run;

data cnhsdata.foodnutr20200506; set foodnutr20160526; run;


%macro group(a,b,c);
data group&a;
set cnhsdata.foodnutr20200506;
foodnutr=&b;
group=&a;
format foodnutr &c;
keep  wave t1 IDDAY  IDind day foodday kcalday foodnutr group age agegroup gender urbanrural;
run;

%mend;

%group(1,foodday1,6.0);   %group(2,foodday2,6.0);   %group(3,foodday3, 5.1);
%group(4,foodday4,6.0);   %group(5,foodday5,5.1);
%group(6,foodday6,6.1);    %group(7,foodday7,6.1);   %group(8,foodday8, 6.1);
%group(9,fiberday,5.1);   %group(10, caday, 6.0); %group(11, foodday11, 6.1);%group(12, foodday12, 6.1);

data nutrfood20200506;
set group1 group2 group3 group4 group5 group6 group7 group8 group9 group10 group11 group12;
proc means;
var foodnutr;
class group;
format group Canfoodgroup.;
run;

proc freq ;
tables group;
run;

data cnhsdata.nutrfood20200506;set nutrfood20200506;run;
proc means data= cnhsdata.nutrfood20200506;
var foodnutr;
class group;
format group Canfoodgroup.;
run;

****************************************
*                energy adjust         *
****************************************;

data bb;
set cnhsdata.nutrfood20200506;
run;

%macro r(a,b);

data r&a&b;
set bb;
if gender=&a and group=&b;
proc reg data=r&a&b outest=est;
model foodnutr=kcalday;
output out=cc&a&b RESIDUAL=resid;
run;

data est&a&b;
set est;
gender=&a;
group=&b;
beta=kcalday;
keep gender group intercept beta;
proc print;
run;
%mend;

%r(1,1);  %r(1,2);  %r(1,3);  %r(1,4);  %r(1,5);  %r(1,6); %r(1,7);  %r(1,8);  %r(1,9); %r(1,10);%r(1,11);%r(1,12);
%r(2,1);  %r(2,2);  %r(2,3);  %r(2,4);  %r(2,5);  %r(2,6);  %r(2,7); %r(2,8);  %r(2,9); %r(2,10);%r(2,11);%r(2,12);

data residual;
set  cc11 cc12 cc13 cc14 cc15 cc16 cc17 cc18 cc19 cc110 cc111 cc112
      cc21 cc22 cc23 cc24 cc25 cc26 cc27 cc28 cc29 cc210 cc211 cc212;
proc sort;
by gender group;
run;

data betase;
set  est11 est12 est13 est14 est15 est16 est17 est18 est19 est110 est111 est112
      est21 est22 est23 est24 est25 est26 est27 est28 est29 est210 est211 est212;
proc sort;
by gender group;
run;

proc freq data=residual; tables gender*group;run;
proc freq data=betase; tables gender*group;run;

data residuals;
merge residual(in=mst) betase;
by gender group;
if mst;
adjusted=resid+intercept+beta*2000;
run;

proc means mean std;
var adjusted foodnutr;
class group;
format group Canfoodgroup.;
RUN;/*** very close***/

data residua20160526;
set residuals;
if adjusted<0 then adjusted=0;
run;

data cnhsdata.residua20200506; set residua20160526;run;

****************************************
*     average foodnutr per day         *
****************************************;

data residual;
set cnhsdata.residua20200506;
proc sort;
by wave group IDind;
run;

data daynutrfood20160526;
set residual;
by wave group IDind;
if first.IDind then do;
                       daynutrfoodall=0;
						   daynutrfoodadjall=0;
										   daykcalall=0;
														   ndays=0;
															   end;
daynutrfoodall+foodnutr;
daynutrfoodadjall+adjusted;
ndays+1;
daykcalall+kcalday;
if last.IDind then output;
keep wave t1 IDind group age agegroup gender urbanrural daynutrfoodall daynutrfoodadjall ndays;
run;

proc freq data=daynutrfood20160526;
tables ndays;
run;

data daynutrfood20160526;
set daynutrfood20160526;
daynutrfood=daynutrfoodall/ndays;              /* crude intake */
daynutrfoodadj=daynutrfoodadjall/ndays;        /* energy adjusted intake */
sex=gender;
keep wave t1 IDind group age agegroup sex urbanrural daynutrfood daynutrfoodadj;
run;

proc freq data=daynutrfood20160526;tables t1; tables group;run;



proc sort data=daynutrfood20160526;
by group wave;
proc means mean std data=daynutrfood20160526;
var daynutrfood;
by group wave;
format group Canfoodgroup.;
RUN;

data cnhsdata.daynutrfood20200506;set daynutrfood20160526; measurement=daynutrfoodadj; run;

proc means n nmiss min mean max data=cnhsdata.daynutrfood20200506;
var measurement;
class group sex;
format group Canfoodgroup.;
run;


*****************************
*           dataset         *
*****************************;

data a0; set cnhsdata.pexam_pub_12; proc sort; by wave IDind; run; proc freq; tables wave*U24A; run;

data a1; set cnhsdata.mast_pub_12; keep IDind gender ; proc sort; by  IDind; run;

data cnhs2; set cnhsdata.surveys_pub_12; keep IDind wave age urban t1; proc sort;  by wave IDind; run;

data cnhspa; set cnhsdata.Nutr2_00; keep IDind wave V29; proc sort;  by wave IDind; run;

data a;
merge a0(in=a0) cnhs2 cnhspa;
by wave IDind;
if a0;
run;

proc sort data=a; by IDind; run;

data aaa;
merge a(in=mst) a1;
by IDind;
if mst;
if wave in (1991,1993,1997, 2000,2004,2006,2009,2011,2015);
if age>=20;
if age<30 then agegroup=20;
else if age<40 then agegroup=30;
else if age<50 then agegroup=40;
else if age<60 then agegroup=50;
else if age<70 then agegroup=60;
else agegroup=70;
urbanrural=t2;
sex=gender;
if u25=1 then eversmk1=1; else eversmk1=0;/*smoking and smoking cessation*/
if u29>0 then eversmk2=1; else eversmk2=0;
run;
proc freq ;
tables v29;
run;

*****************************
*             BMI           *
*****************************;

data bmi;
set aaa;
if height<100 then height=.; /**one height=60 for adults**/
if weight=. or height=. then delete;
bmi=weight/height/height*10000;
group=13;
measurement=bmi;
keep IDind group wave t1 sex agegroup urbanrural measurement age;
proc sort;by wave;
proc means n nmiss min mean max;
var measurement;
by t1;
run;


********************************************************
*                 physical inactivity                  *
********************************************************;

data pal;
set aaa;
if v29 in (1,2) then palyn=1;
else if v29 in (4,5,3) then palyn=0;
else palyn=.;
if palyn=. then delete;
group=14;
measurement=palyn;
keep IDind group wave t1 sex agegroup urbanrural measurement age;
proc sort;
by sex agegroup urbanrural t1;
proc freq ;
tables measurement;
run;
proc sort;by wave;
proc means n nmiss min mean max;
var measurement;
by wave;
run;





                                                     ***
                                                ***      ***
                                            ***              ***
                                        *** means and predicting ***
                                        ***  by age, sex and UR ,t1 ***
                                            ***              ***
                                                ***      ***
                                                     ***;

/******predicting for all besides continuous variable£¬group1-10******/
data group20210308;
set cnhsdata.group20210308;
groupidind=group*10000000000000+IDind;
if group=7 then delete;
if group=8 then delete;
if group=9 then delete;
if group=11 then delete;
if group=17 then delete;
if group=14 then delete;
if group=13 then delete;
if group=15 then delete;
if group=16 then delete;
year=wave-1991;           /*tweak model*/
sqrtyear=sqrt(year);      /*tweak model*/                
proc means min median max;
var groupidind;
run;
proc sort;
by group sex agegroup urbanrural;
run;

proc mixed data=group20210308 noclprint covtest;
class IDind;
model measurement=sqrtyear/solution ddfm=bw notest outpred=pred20210308;  
repeated IDind/sub=IDind  type=cs;  
ods output SolutionF=mixedest20210308;
by group sex agegroup urbanrural ;
run;
/******predicting data******/
data int;
set mixedest20210308;
if effect='Intercept';
intercept=estimate;
keep  group sex agegroup urbanrural intercept;
proc sort;
by group sex agegroup urbanrural;
run;

data betawave;
set mixedest20210308;
if effect='sqrtyear';
betawave=estimate;
keep  group sex agegroup urbanrural betawave;
proc sort;
by group sex agegroup urbanrural;
run;

data resid;
set pred20210308;
if wave=2011;
keep IDind group sex agegroup urbanrural resid;
proc sort;
by group sex agegroup urbanrural;
run;

data pred;
merge resid(in=mst) int betawave;
by group sex agegroup urbanrural;
if mst;
run;

%macro midyear(year);
data mid&year;
set pred;
wave=&year;
year=wave-1991;         
sqrtyear=sqrt(wave-1991);   
measurement=intercept+betawave*sqrtyear+resid;  
keep IDind wave agegroup urbanrural sex group measurement;
run;
%mend;

 %midyear(2015); %midyear(2017);  %midyear(2020) ;%midyear(2023); %midyear(2025); %midyear(2027); %midyear(2029);  %midyear(2031); %midyear(2033); %midyear(2035);

data groups20210308ur;
set mid2015 mid2017 mid2020 mid2023 mid2025 mid2027 mid2029 mid2031 mid2033 mid2035
    group20210308;
drop age;
run;

proc print data =pred (obs=5);
  title 'check prediction1';
run;

proc print data =groups20210308ur (obs=5);
  title 'check prediction2';
run;

data cnhsdata.group20210308ur; set groups20210308ur; run;

proc means data=cnhsdata.group20210308ur;
var measurement;
class wave group sex agegroup urbanrural;
output out=nutrmeansd mean=nutrmean std=nutrstd n=n;
run;

data meansd20210308ur;
set nutrmeansd;
if wave ne . and group ne . and sex ne . and agegroup ne . and urbanrural ne .;
mean=nutrmean;
std=nutrstd;
see=std/sqrt(n);
if group in (7,8,9,11,13,15,17,14,16) then delete;
keep wave group sex agegroup urbanrural mean std n see;
proc sort;
by group sex agegroup urbanrural wave;
proc print;
run;
proc means data=meansd20210308ur;
var mean;
class wave group sex;
run;

/*bmi*/

data group13;
set cnhsdata.group20210308;
groupidind=group*10000000000000+IDind;
if group^=13 then delete;
year=wave-1991;           /*tweak model*/
sqrtyear=sqrt(year);      /*tweak model*/
proc sort;
by group sex agegroup urbanrural;
run;

proc mixed data=group13 noclprint covtest;
class IDind;
model measurement=sqrtyear/solution ddfm=bw notest outpred=pred13;  
repeated IDind/sub=IDind  type=cs;
ods output SolutionF=mixedest13;
by group sex agegroup urbanrural;
run;

********************************************************************************************************
*                                          predicting for smoking                                      *
********************************************************************************************************;

********************prevalence*************************;

data eversmoking;
set aaa;
if (u27=9 or u27=.) and (u25=9 or u25=.) then delete;
if t1=11 or t1=31 or t1=55 then delete;/*Beijing Shanghai Chongqing*/
if u27=1 and u25=1 then eversmk=1; if u27=0 and u25=1 then eversmk=0; /*25EVER SMOKED CIGARETTES,27STILL SMOKES CIGARETTES*/
if u25=0 then eversmk=0;
if eversmk=. then delete;
domainurbansexage=urbanrural*1000+sex*100+agegroup;
proc sort;
by wave;
run;
proc freq; table eversmk;run;

ods output CrossTabFreqs=smkpct;
PROC freq data=eversmoking;
TABLES  domainurbansexage*eversmk;
by wave;
run;
ods output close;
data eversmkp; /*** prevalence of current smoking ***/
set smkpct;
if domainurbansexage=. then delete;
if eversmk=1;
if rowpercent=. then rowpercent=0;
year=wave;
keep wave year domainurbansexage  rowpercent;
proc sort;
by wave domainurbansexage;
proc print;
run;
**********************predicting***************************;
ods output ParameterEstimates =smkest;
proc sort data=eversmoking;
   by domainurbansexage;
   run;
proc genmod data=eversmoking;
   model eversmk = wave age/ type3 dist=poisson;
   by domainurbansexage;
   run;
ods output close;

data intercept;
set smkest;
if Parameter='Intercept';
intercept=estimate;
keep domainurbansexage intercept;
proc sort;
by domainurbansexage;
run;

data betawave;
set smkest;
if Parameter='WAVE';
betawave=estimate;
keep domainurbansexage betawave;
proc sort;
by domainurbansexage;
run;

data betaage;
set smkest;
if Parameter='age';
betaage=estimate;
keep domainurbansexage betaage;
proc sort;
by domainurbansexage;
run;

data smoking;
merge intercept betawave betaage;
by domainurbansexage;
agemed=(domainurbansexage-int(domainurbansexage/100)*100)+5;
run;

%macro midyear(year);
data mid&year;
set smoking;
year=&year;
rowpercent=exp(intercept+betawave*&year+betaage*agemed)*100;
keep  domainurbansexage year rowpercent;
run;
%mend;

%midyear(2017);   %midyear(2020);%midyear(2023); %midyear(2025); %midyear(2027); %midyear(2029);  %midyear(2031); %midyear(2033); %midyear(2035);

data prevalence;
set mid2017 mid2020 mid2023 mid2025 mid2027 mid2029 mid2031 mid2033 mid2035
    eversmkp;
group=14;
urbanrural=int(domainurbansexage/1000);
sex=int((domainurbansexage-urbanrural*1000)/100);
agegroup=domainurbansexage-urbanrural*1000-sex*100;
if wave=. then wave=year;
proc sort;
by group  sex agegroup urbanrural wave;
run;
proc print;run;

data cnhsdata.smkpct20210308ur;
set prevalence;
mean=rowpercent;
keep group wave sex agegroup urbanrural mean;
proc sort;
by group wave agegroup sex urbanrural ;
run;


********************************************************************************************************
*                               mean & SD/SEE by urban rural                                           *
********************************************************************************************************;

ods rtf file='*******';
data cnhsdata.meansdforlj;
set cnhsdata.meansd20210308conur cnhsdata.smkpct20210308ur  cnhsdata.palpct20210308ur cnhsdata.drinkpct20210308ur;
if group in (14,15,8) then do; see=0;std=0; end;
drop n;
format group Canfoodgroup.  agegroup agegroup. sex sex. urbanrural urbanrural.;
proc sort;
by group sex agegroup urbanrural wave;
run;
* ods rtf close;
proc means data=cnhsdata.meansdforlj;
var mean;
class wave group sex;
run;

PROC EXPORT DATA=cnhsdata.meansdforlj
            OUTFILE= '*****'
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

data mean;
set cnhsdata.meansdforlj;run;

%macro datapre(ur, sex, age);

data data&ur&sex&age;
set  mean;
if agegroup=&age and sex=&sex and urbanrural=&ur;
mean&ur&sex&age=mean;
see&ur&sex&age=see;
year=wave;
keep year group mean&ur&sex&age see&ur&sex&age;
proc sort;
by group year;
run;

%mend;

%datapre(1,1,20);
%datapre(1,1,30);
%datapre(1,1,40);
%datapre(1,1,50);
%datapre(1,1,60);
%datapre(1,1,70);
%datapre(1,2,20);
%datapre(1,2,30);
%datapre(1,2,40);
%datapre(1,2,50);
%datapre(1,2,60);
%datapre(1,2,70);
%datapre(2,1,20);
%datapre(2,1,30);
%datapre(2,1,40);
%datapre(2,1,50);
%datapre(2,1,60);
%datapre(2,1,70);
%datapre(2,2,20);
%datapre(2,2,30);
%datapre(2,2,40);
%datapre(2,2,50);
%datapre(2,2,60);
%datapre(2,2,70);


data mean1;
merge
data1120
data1130
data1140
data1150
data1160
data1170
data1220
data1230
data1240
data1250
data1260
data1270
data2120
data2130
data2140
data2150
data2160
data2170
data2220
data2230
data2240
data2250
data2260
data2270
;
by group year;
yearmean=
(mean1120*0.0640749792291515+
mean1130*0.0632016056709848+
mean1140*0.0589767962053254+
mean1150*0.0389303923883878+
mean1160*0.0214615354617439+
mean1170*0.012917252516362+
mean1220*0.0628548673915807+
mean1230*0.0604839901664346+
mean1240*0.0553121677405731+
mean1250*0.0378057263805841+
mean1260*0.0214891833993558+
mean1270*0.0143280580472796+
mean2120*0.0490522296935186+
mean2130*0.0496180341400287+
mean2140*0.0556350747096706+
mean2150*0.0432399414312312+
mean2160*0.0282056505673727+
mean2170*0.019914850763446+
mean2220*0.0491085726792354+
mean2230*0.0474503134338851+
mean2240*0.0549308979277063+
mean2250*0.0414098083440804+
mean2260*0.0266205001344798+
mean2270*0.0229775715775817);
;
/*** standardized by proportion of Census2010 ***/

yearsee=sqrt((
see1120*see1120+
see1130*see1130+
see1140*see1140+
see1150*see1150+
see1160*see1160+
see1170*see1170+
see1220*see1220+
see1230*see1230+
see1240*see1240+
see1250*see1250+
see1260*see1260+
see1270*see1270+
see2120*see2120+
see2130*see2130+
see2140*see2140+
see2150*see2150+
see2160*see2160+
see2170*see2170+
see2220*see2220+
see2230*see2230+
see2240*see2240+
see2250*see2250+
see2260*see2260+
see2270*see2270
)/24);

yearmeanurban=
(mean1120*0.124525121+
mean1130*0.118331076+
mean1140*0.116008842+
mean1150*0.074493091+
mean1160*0.042009943+
mean1170*0.031122234+
mean1220*0.122050285+
mean1230*0.113492242+
mean1240*0.108880471+
mean1250*0.072266261+
mean1260*0.042163107+
mean1270*0.034657327);

yearmeanrural=
(mean2120*0.101799795+
mean2130*0.098329394+
mean2140*0.116071461+
mean2150*0.08693254+
mean2160*0.058528404+
mean2170*0.041283435+
mean2220*0.101855514+
mean2230*0.093967115+
mean2240*0.114641595+
mean2250*0.083532987+
mean2260*0.055535109+
mean2270*0.047522653);

yearmeanmale=
(mean1120*0.127323757+
mean1130*0.120990504+
mean1140*0.11861608+
mean1150*0.076167283+
mean1160*0.042954094+
mean1170*0.03182169+
mean2120*0.09758599+
mean2130*0.094259239+
mean2140*0.111266908+
mean2150*0.083334135+
mean2160*0.056105734+
mean2170*0.039574587);

yearmeanfemale=
(mean1220*0.127199741+
mean1230*0.118280623+
mean1240*0.113474275+
mean1250*0.075315266+
mean1260*0.043942022+
mean1270*0.036119564+
mean2220*0.099522223+
mean2230*0.09181453+
mean2240*0.112015402+
mean2250*0.081619425+
mean2260*0.054262919+
mean2270*0.046434011);

yearmeanage20=
(mean1120*0.284622568+
mean1220*0.278965927+
mean2120*0.218146053+
mean2220*0.218265452);

yearmeanage30=
(mean1130*0.287135889+
mean1230*0.275394231+
mean2130*0.223696979+
mean2230*0.213772901);

yearmeanage40=
(mean1140*0.26294461+
mean1240*0.246787506+
mean2140*0.246653183+
mean2240*0.243614701);

yearmeanage50=
(mean1150*0.242983321+
mean1250*0.235719795+
mean2150*0.265846491+
mean2250*0.255450393);

yearmeanage60=
(mean1160*0.219818751+
mean1260*0.22062019+
mean2160*0.28712263+
mean2260*0.272438428);

yearmeanage70=
(mean1170*0.208820108+
mean1270*0.232539438+
mean2170*0.259696124+
mean2270*0.29894433);
/*** standardized by proportion of Census2010 ***/

yearseeurban=sqrt((
see1120*see1120+
see1130*see1130+
see1140*see1140+
see1150*see1150+
see1160*see1160+
see1170*see1170+
see1220*see1220+
see1230*see1230+
see1240*see1240+
see1250*see1250+
see1260*see1260+
see1270*see1270
)/12);

yearseerural=sqrt((
see2120*see2120+
see2130*see2130+
see2140*see2140+
see2150*see2150+
see2160*see2160+
see2170*see2170+
see2220*see2220+
see2230*see2230+
see2240*see2240+
see2250*see2250+
see2260*see2260+
see2270*see2270
)/12);

yearseemale=sqrt((
see1120*see1120+
see1130*see1130+
see1140*see1140+
see1150*see1150+
see1160*see1160+
see1170*see1170+
see2120*see2120+
see2130*see2130+
see2140*see2140+
see2150*see2150+
see2160*see2160+
see2170*see2170
)/12);

yearseefemale=sqrt((
see1220*see1220+
see1230*see1230+
see1240*see1240+
see1250*see1250+
see1260*see1260+
see1270*see1270+
see2220*see2220+
see2230*see2230+
see2240*see2240+
see2250*see2250+
see2260*see2260+
see2270*see2270
)/12);

yearseeage20=sqrt((
see1120*see1120+
see1220*see1220+
see2120*see2120+
see2220*see2220
)/4);

yearseeage30=sqrt((
see1130*see1130+
see1230*see1230+
see2130*see2130+
see2230*see2230
)/4);

yearseeage40=sqrt((
see1140*see1140+
see1240*see1240+
see2140*see2140+
see2240*see2240
)/4);

yearseeage50=sqrt((
see1150*see1150+
see1250*see1250+
see2150*see2150+
see2250*see2250
)/4);

yearseeage60=sqrt((
see1160*see1160+
see1260*see1260+
see2160*see2160+
see2260*see2260
)/4);

yearseeage70=sqrt((
see1170*see1170+
see1270*see1270+
see2170*see2170+
see2270*see2270
)/4);
run;


ods rtf file='*******';

title 'overall mean levels by year and ursexage';

/*Intake Results*/
data meanlevelursexage;
set  mean1;
keep group year yearmean yearmeanurban yearmeanrural yearmeanmale yearmeanfemale
yearmeanage20 yearmeanage30 yearmeanage40 yearmeanage50 yearmeanage60 yearmeanage70
yearseeurban yearseerural yearseemale yearseefemale yearsee
yearseeage20 yearseeage30 yearseeage40 yearseeage50 yearseeage60 yearseeage70;
format group Canfoodgroup.;
proc sort;
by group year;
proc print;
run;
/*Intake Results*/
ods rtf close;

PROC EXPORT DATA=meanlevelursexage
            OUTFILE= '******'
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
/*table1*/
data yearmean;
set meanlevelursexage;
keep group year yearmean yearsee;
run;
proc transpose data=yearmean out=table1;
by group;
var yearmean yearsee;
id year;
proc print;
run;
PROC EXPORT DATA=table1
            OUTFILE= '***********'
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


