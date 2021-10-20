
title "Sashelp.bweight --- Infant Birth Weight";

proc contents data=x1 varnum;
ods select position;
run;


data x1;
set sashelp.bweight;

* Creating a binary outcome variable;
* A baby weighing less or equal to 2500 grams is considered "low birth weight";
LBW=Weight lt 2500; 

* Creating a binary exposure variable;
* A mom that smokes at least 5 cigarettes per day is considered a frequent smoker;
FreqSmoker=CigsPerDay ge 5; 

* Creating a binary confounder variable;
* A mom gained at least 0 pounds;
WtGain=MomWtGain GE 10;

/*Misclassifying the Married variable with 75% Sensitivity and 90% Specificity*/
if Married=1 then Married_mis=rand('Uniform')<0.75;
if Married=0 then Married_mis=rand('Uniform')>0.90;

/*Misclassifying the Weight gain variable with 70% Sensitivity and 85% Specificity*/
if WtGain=1 then WtGain_mis=rand('Uniform')<0.70;
if WtGain=0 then WtGain_mis=rand('Uniform')>0.85;

run;

Title "Model 1: Fully adjusted and with no misclassification";
ods graphics off;            /* or use the %ODSOff macro */
ods exclude ModelInfo ResponseProfile ClassLevelInfo ConvergenceStatus FitStatistics GlobalTests
			NObs ParameterEstimates Association GoodnessOfFit;             /* suspend all open destinations */
proc logistic data=x1;
class lbw (ref="0") FreqSmoker (ref="0") Married (ref="0") boy (ref="0") WtGain (ref="1");
model lbw=FreqSmoker Married WtGain;
run;
ods exclude none;            /* or use the %ODSOn macro */

%include "C:\PATH\misclass.sas";
%Misclassification (
	inset=x1,
	exp=FreqSmoker,
	depend=lbw,
	libname=C:\PATH,
	startover=yes,

	Variable1=Married_mis, Sens_min1=0.70, sens_mod_1=0.72, sens_mod2_1=0.77, Sens_max1=0.80, 
						  Spec_min1=0.85, spec_mod_1=0.87, spec_mod2_1=0.92, Spec_max1=0.95,

	Variable2=WtGain_mis, Sens_min2=0.65, sens_mod_2=0.67, sens_mod2_2=0.72, Sens_max2=0.75, 
					   Spec_min2=0.80, spec_mod_2=0.82, spec_mod2_2=0.87, Spec_max2=0.90,


	indep=Married_mis WtGain_mis,
	N_Misc_var=2,
	print_plots=yes,
	TOTALREPS=1000

	);
