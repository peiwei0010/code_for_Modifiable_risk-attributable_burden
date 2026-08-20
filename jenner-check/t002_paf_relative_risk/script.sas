/* Adapted from 2.paf_calculate.sas (peiwei0010/code_for_Modifiable_risk-attributable_burden).
   The original computes population-attributable-fraction (PAF) values per cancer site from
   a relative-risk (RR) coefficient table imported via PROC IMPORT from a local .xlsx
   (D:\PAF\lj\RR\RRcancerpw.xlsx) and CHNS exposure summaries read from a private `cnhsdata`
   libname. Neither is available outside the authors' environment, so this bundle keeps
   their PAF formula exactly as written --
       RR = exp(exposure * beta)
       PAF = put(sum_RR / (sum_RR + 1) * 100, 4.1)
   -- and its PROC SORT / BY-group accumulation pattern, applied to a small mock RR
   coefficient table plus a real inline sample of the burden data the repo ships
   (5.male_all.csv), instead of the unavailable Excel/CHNS inputs. */

options formchar=' _________';

proc format;
value cancergrp
    1='Lung cancer'  2='Colorectal cancer';
run;

/* real rows lifted from 5.male_all.csv (year, cancer_1, stdrate) */
data burden;
length cancer_1 $20;
input year cancer_1 $ stdrate;
if cancer_1='Lung_cancer' then do; group=1; cancer_1='Lung cancer'; end;
else if cancer_1='CRC_cancer' then do; group=2; cancer_1='Colorectal cancer'; end;
datalines;
2007 Lung_cancer 52.99283183
2008 Lung_cancer 50.76718948
2009 Lung_cancer 51.18308583
2010 Lung_cancer 50.51021823
2011 Lung_cancer 51.66566551
2012 Lung_cancer 50.94315432
2007 CRC_cancer  13.53109299
2008 CRC_cancer  12.56892818
2009 CRC_cancer  13.06328798
2010 CRC_cancer  12.73063703
2011 CRC_cancer  12.79959299
2012 CRC_cancer  11.99090586
;
run;

/* mock relative-risk coefficients, standing in for the authors' RRcancerpw.xlsx import */
data rr;
input group beta_lung beta_crc;
datalines;
1 0.014 0.006
2 0.009 0.011
;
run;

proc sort data=burden; by group year; run;
proc sort data=rr; by group; run;

data merged;
merge burden(in=mst) rr;
by group;
if mst;
run;

/* relative risk and PAF contribution, following the RR/PAF formula from 2.paf_calculate.sas */
data calculation;
set merged;
var_tm = stdrate;
p = 0.05;
if group=1 then RRcancer = exp(var_tm * beta_lung);
else if group=2 then RRcancer = exp(var_tm * beta_crc);
p_rr = p * (RRcancer - 1);
run;

proc sort data=calculation;
by group year;
run;

data pafdata;
set calculation;
by group year;
if first.group then sum_rr=0;
sum_rr + p_rr;
if last.group then output;
keep group year sum_rr;
run;

data paf;
set pafdata;
paf_pct = put(sum_rr / (sum_rr + 1) * 100, 4.1);
format group cancergrp.;
run;

proc print data=paf;
title 'PAF by cancer site (adapted from 2.paf_calculate.sas)';
run;

proc means data=calculation mean std min max;
var RRcancer p_rr;
class group;
format group cancergrp.;
run;
