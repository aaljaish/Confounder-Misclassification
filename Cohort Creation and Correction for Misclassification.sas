libname input  "C:\Users\Admin\..."; /* REPLACE ME WITH A VALID PATH*/
libname output "C:\Users\Admin\..."; /* REPLACE ME WITH A VALID PATH*/
%let seed=110899; 
%put WARNING: The seed used for this program is: &seed;

***************************************************
***************************************************
***************************************************;

/* The RanMBin macro can be downloaded from: https://support.sas.com/kb/66/969.html
PURPOSE: The RanMBin macro generates values from multiple binary variables with specified means and correlation matrix. 
It creates an output data set with the specified number of observations and arranged either in wide or long format. 
Exchangeable and autoregressive AR(1) correlation structures are supported as well as unstructured correlation matrices. 
Banded correlation structures with autoregressive, power-decaying correlations are also easily handled.
*/
%include "C:\Users\Admin\...\RanMBin.sas"; /* REPLACE ME WITH A VALID PATH*/
%RanMBin(means=.47 .34 .09 .15 .09 .29 .18 .27 .55, exch=.08, n=10000, seed=&seed)


data output.Cohort;
set mbin;
rename 
	Y1=confounder1 
	Y2=confounder2
	Y3=confounder3
	Y4=confounder4
	Y5=confounder5
	Y6=confounder6
	Y7=confounder7
	Y8=confounder8
	Y9=confounder9;
drop id;
run;

***************************************************
***************************************************
***************************************************;

/****This sets confounder association with the outcome/exposure****/
data output.Cohort;
	set output.Cohort;
	call streaminit(&seed);
	linpred_exp=
		-1.1465
		+log(0.7)*confounder1 
		+log(2.0)*confounder2
		+log(2.0)*confounder3
		+log(2.0)*confounder4 
		+log(2.0)*confounder5 
		+log(2.0)*confounder6
		+log(2.0)*confounder7
		+log(2.0)*confounder8
		+log(2.0)*confounder9;  /*linear prediction model*/
	exposure_sim=rand("binomial",1/(1+exp(-linpred_exp)),1);
	linpred= 		
		-3.7228
		+log(0.7)*confounder1 
		+log(2.0)*confounder2
		+log(2.0)*confounder3
		+log(2.0)*confounder4 
		+log(2.0)*confounder5 
		+log(2.0)*confounder6
		+log(2.0)*confounder7
		+log(2.0)*confounder8
		+log(2.0)*confounder9;  /*linear prediction model*/
	outcome_HR=exp(-linpred);
	Outcome_sim=rand("binomial",1/(1+outcome_HR),1);
	PatientN=_N_;
	SiteN=1;
	drop linpred linpred_exp outcome_HR;
run;

***************************************************
***************************************************
***************************************************;
%let indep1=confounder1 confounder2 confounder3 confounder4 confounder5 confounder6 confounder7 confounder8 confounder9;
TITLE "Association of confounding variables with the outcome";

proc Logistic data=input.Cohort;
	model outcome_sim=&indep1;
run;

TITLE "Association of confounding variables with the exposure";

proc Logistic data=input.Cohort;
	model exposure_sim=&indep1;
run;

TITLE "Association of exposure with the outcome (unadjusted)";

proc Logistic data=input.Cohort;
	class exposure_sim (ref='1');
	model outcome_sim=exposure_sim;
run;

TITLE "Association of exposure with the outcome (fully adjusted)";

proc Logistic data=input.Cohort;
	class exposure_sim (ref='0');
	model outcome_sim=exposure_sim &indep1;
run;

TITLE;

***************************************************
***************************************************
***************************************************;

/*Operating characteristics of confounding variables*/
data Op_char;
	call streaminit(&seed);

	do i=1 to 9;
		sensitivity=rand('uniform',0.6,0.90);
		specificty =rand('uniform',0.80,0.95);
		sensitivity_var=cat('','sens',i);
		specificty_var =cat('','spec',i);
		call symput(compress(sensitivity_var),sensitivity);
		call symput(compress(specificty_var),specificty);
		output;
	end;
run;

%macro repeat (n);
	%let var1=confounder1;
	%let var2=confounder2;
	%let var3=confounder3;
	%let var4=confounder4;
	%let var5=confounder5;
	%let var6=confounder6;
	%let var7=confounder7;
	%let var8=confounder8;
	%let var9=confounder9;

	%do i=1 %to &n;
		%put &&Var&i with a sensivitiy of &&sens&i;
		%put &&Var&i with a specificity of %SYSEVALF(1-&&spec&i);

		data output.Cohort;
			set input.Cohort;
			call streaminit(&seed+&i);

			if &&Var&i=1 then
				&&Var&i.._misc=RAND("Bernoulli", &&sens&i);
			else &&Var&i.._misc=RAND("Bernoulli", 1-&&spec&i);
		run;

	%end;
%mend;

%repeat(9);

***************************************************
***************************************************
***************************************************;
data inset;
	set input.cohort;
run;

TITLE "Sensitivity and Specificity of confounding variables";

proc freq data=inset;
	tables confounder1_misc*confounder1 
		confounder2_misc*confounder2 
		confounder3_misc*confounder3 
		confounder4_misc*confounder4 
		confounder5_misc*confounder5 
		confounder6_misc*confounder6 
		confounder7_misc*confounder7 
		confounder8_misc*confounder8 
		confounder9_misc*confounder9/norow nopercent;
run;

TITLE;

***************************************************
***************************************************
***************************************************;

/* The Misclassification macro can be downloaded from: : https://github.com/aaljaish/Confounder-Misclassification

This SAS Macro used a probabilistic method for conducting sensitivity analyses to produce a "Percent Adjustment" factor that corrects for 
the effect of residual confounding due to measurement error of confounding variables. The Percent Adjustment is the amount of confounding 
bias removed using the misclassified confounders relative to the total confounding bias. Our method uses Monto Carlo simulations to estimate 
the Percent Adjustment and corrects the observed exposure-outcome odds ratio (OR). The user specifies ranges of sensitivity and specificity 
parameters for each confounder to yield confidence intervals for the exposure-outcome OR that incorporates both random and systematic error.

*/
%include "C:\Users\Admin\...\Correction for non-differential misclassification.sas"; /* REPLACE ME WITH A VALID PATH*/
%Misclassification (
	inset=inset,
	exp=exposure_sim,
	depend=outcome_sim,
	libname=C:\Users\Admin\..., /* REPLACE ME WITH A VALID PATH*/
	startover=yes,

	Variable1=confounder1_misc, Sens_min1=0.7720528778,Sens_max1=0.8720528778,spec_min1=0.845173584,spec_max1=0.945173584,
	Variable2=confounder2_misc, Sens_min2=0.594810531,Sens_max2=0.694810531,spec_min2=0.7674928221,spec_max2=0.8674928221,
	Variable3=confounder3_misc, Sens_min3=0.5594159383,Sens_max3=0.6594159383,spec_min3=0.7640770607,spec_max3=0.8640770607,
	Variable4=confounder4_misc, Sens_min4=0.8199756649,Sens_max4=0.9199756649,spec_min4=0.8384644508,spec_max4=0.9384644508,
	Variable5=confounder5_misc, Sens_min5=0.8419508274,Sens_max5=0.9419508274,spec_min5=0.8328404458,spec_max5=0.9328404458,
	Variable6=confounder6_misc, Sens_min6=0.6676323245,Sens_max6=0.7676323245,spec_min6=0.8586862725,spec_max6=0.9586862725,
	Variable7=confounder7_misc, Sens_min7=0.6688511737,Sens_max7=0.7688511737,spec_min7=0.8069124417,spec_max7=0.9069124417,
	Variable8=confounder8_misc, Sens_min8=0.8379739543,Sens_max8=0.9379739543,spec_min8=0.8632480895,spec_max8=0.9632480895,
	Variable9=confounder9_misc, Sens_min9=0.6756563795,Sens_max9=0.7756563795,spec_min9=0.796513874,spec_max9=0.896513874,

	indep=confounder1_misc confounder2_misc confounder3_misc confounder4_misc confounder5_misc confounder6_misc confounder7_misc confounder8_misc confounder9_misc,
	N_Misc_var=9,
	print_plots=yes,
	TOTALREPS=1000

	);