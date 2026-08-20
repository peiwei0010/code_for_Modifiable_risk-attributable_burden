/* Adapted from 1.code_for_chnsRF.sas (peiwei0010/code_for_Modifiable_risk-attributable_burden).
   The original script builds PROC FORMAT value labels (Canfoodgroup / agegroup / sex /
   urbanrural) and then runs PROC FREQ / PROC MEANS over CHNS survey extracts read from a
   private `cnhsdata` libname. That libname isn't available outside the authors' environment,
   so this bundle keeps their format style and PROC MEANS/FREQ/SORT/PRINT flow, and runs it
   against a real inline sample of the burden data the repo ships (4.female_all.csv) instead
   of the unavailable libname read. */

options formchar=' _________';

proc format;
value cancergrp
    1='Breast cancer'  2='Lung cancer'  3='Liver cancer';
run;

/* real rows lifted from 4.female_all.csv (year, DALY, YLL, YLD, cancer_1, stdrate) */
data burden;
length cancer_1 $20;
input year DALY YLL YLD cancer_1 $ stdrate;
if cancer_1='Breast_cancer' then do; cancergroup=1; cancer_1='Breast cancer'; end;
else if cancer_1='Lung_cancer' then do; cancergroup=2; cancer_1='Lung cancer'; end;
else if cancer_1='Liver_cancer' then do; cancergroup=3; cancer_1='Liver cancer'; end;
datalines;
2007 1665563.791 1553961.215 111602.5762 Breast_cancer 8.313657769
2008 1806075.364 1686015.743 120059.6213 Breast_cancer 8.820709512
2009 1864061.771 1734806.295 129255.4766 Breast_cancer 8.913708233
2010 1999797.876 1861376.817 138421.0584 Breast_cancer 9.100928471
2011 2119139.669 1971553.425 147586.244  Breast_cancer 9.209121578
2012 1988831.499 1832092.228 156739.2711 Breast_cancer 8.490171643
2007 3517261.373 3483336.231 33925.14217 Lung_cancer   25.25670351
2008 3359060.257 3323046.77  36013.48698 Lung_cancer   23.35621728
2009 3646760.412 3608295.887 38464.52447 Lung_cancer   24.75163073
2010 3650535.038 3609580.379 40954.65922 Lung_cancer   24.03540917
2011 3863414.926 3819502.852 43912.07451 Lung_cancer   24.36147527
2012 4029895.216 3982597.34  47297.87613 Lung_cancer   24.81588899
2007 1839895.656 1820345.291 19550.36523 Liver_cancer  11.84380472
2008 1859423.861 1839767.548 19656.31307 Liver_cancer  11.49610291
2009 1965361.484 1945501.627 19859.85698 Liver_cancer  11.76370759
2010 2081048.913 2060728.986 20319.92704 Liver_cancer  12.12399098
2011 2068837.244 2047857.864 20979.37937 Liver_cancer  11.67057573
2012 2058390.095 2036630.893 21759.20116 Liver_cancer  11.39126117
;
run;

proc sort data=burden;
by cancergroup year;
run;

proc freq data=burden;
tables cancergroup year;
run;

proc means data=burden n nmiss min mean max std;
var DALY YLL YLD stdrate;
class cancergroup;
format cancergroup cancergrp.;
run;

proc print data=burden (obs=10);
var year cancer_1 DALY YLL YLD stdrate;
title 'Female cancer burden sample (adapted from 1.code_for_chnsRF.sas)';
run;
