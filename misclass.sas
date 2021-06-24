*****     delete existing datasets from previous runs     *****
**************************************************************;
%Macro Misclassification (
			libname, /* Specify the path to the folder that contains the "inset" dataset on your computer.*/
			inset, /* Specify the name of the dataset on which to perform the analysis. This parameter must be specified, and the appropriate dataset must exist in the specified library.  */
			exp, /* The exposure variable must be specified and exist in the "inset" dataset. */
			depend, /* The dependent variable must be specified and exist in the "inset" dataset. */
			print_plots, /* The user can request that the distribution of the specificity and specificity for each confounder be plotted. The default value is yes (without quotations) to print all plots.

			Possible values: yes, no
			*/ 
			indep, /*This parameter specifies the variables entered into the logistic regression model for adjustment. The adjustment variables must include at least all misclassified variables. The macro will stop executing if the user enters the "exp" or "depend" variable(s) in the "indep" string. No quotations or commas are needed to separate variables. If the indep string is not specified, the macro will by default enter all misclassified variables.  */
			N_Misc_var, /* The user must specify the number of misclassified confounding variables (i.e., a number between 1 and 20).*/
			TOTALREPS, /*The user can specify the number of iterations. If not specified, the macro will iterate 1000 times.*/
			startover, /* The user can request that all existing datasets generated from previous runs be deleted. The default value is no (without quotations); that is, all new observations will be appended to the existing datasets.

Possible values: yes, no
*/

			/* Here the user defines which variables are misclassified. The SAS macro can handle up to 20 misclassified variables. Variable1 parameter is required, but the remaining parameters are optional.*/
			/* Sens_minX & Spec_minX:  The user must specify the minimum sensitivity and specificity parameter for each misclassified variable. */
			/* Sens_mod_X to Spec_mod_X: The user can specify the first mode for the sensitivity and specificity parameters for each misclassified variable. If the first mode is not specified, the parameter distribution is assumed to be uniform.*/
			/* Sens_mod2_X to Spc_mod_X: The user can specify a second mode for the sensitivity and specificity parameters for each misclassified variable. If the first mode is specified and the second mode is not specified, then the parameter distribution is assumed to be triangular. If both the first and second modes are specified and the second mode is greater than the first mode, then the parameter distribution is assumed to be trapezoidal.*/
			/* Sens_maxX to Spec_maxX: The user must specify the maximum sensitivity and specificity parameter for each misclassified variable. */
			
			Variable1,  Sens_min1,  sens_mod_1,  sens_mod2_1,  Sens_max1,  spec_min1,  spec_mod_1,  spec_mod2_1,  spec_max1,
			Variable2,  Sens_min2,  sens_mod_2,  sens_mod2_2,  Sens_max2,  spec_min2,  spec_mod_2,  spec_mod2_2,  spec_max2,
			Variable3,  Sens_min3,  sens_mod_3,  sens_mod2_3,  Sens_max3,  spec_min3,  spec_mod_3,  spec_mod2_3,  spec_max3,
			Variable4,  Sens_min4,  sens_mod_4,  sens_mod2_4,  Sens_max4,  spec_min4,  spec_mod_4,  spec_mod2_4,  spec_max4,
			Variable5,  Sens_min5,  sens_mod_5,  sens_mod2_5,  Sens_max5,  spec_min5,  spec_mod_5,  spec_mod2_5,  spec_max5,
			Variable6,  Sens_min6,  sens_mod_6,  sens_mod2_6,  Sens_max6,  spec_min6,  spec_mod_6,  spec_mod2_6,  spec_max6,
			Variable7,  Sens_min7,  sens_mod_7,  sens_mod2_7,  Sens_max7,  spec_min7,  spec_mod_7,  spec_mod2_7,  spec_max7, 
			Variable8,  Sens_min8,  sens_mod_8,  sens_mod2_8,  Sens_max8,  spec_min8,  spec_mod_8,  spec_mod2_8,  spec_max8,
			Variable9,  Sens_min9,  sens_mod_9,  sens_mod2_9,  Sens_max9,  spec_min9,  spec_mod_9,  spec_mod2_9,  spec_max9,
			Variable10, Sens_min10, sens_mod_10, sens_mod2_10, Sens_max10, spec_min10, spec_mod_10, spec_mod2_10, spec_max10,
			Variable11, Sens_min11, sens_mod_11, sens_mod2_11, Sens_max11, spec_min11, spec_mod_11, spec_mod2_11, spec_max11,
			Variable12, Sens_min12, sens_mod_12, sens_mod2_12, Sens_max12, spec_min12, spec_mod_12, spec_mod2_12, spec_max12,
			Variable13, Sens_min13, sens_mod_13, sens_mod2_13, Sens_max13, spec_min13, spec_mod_13, spec_mod2_13, spec_max13,
			Variable14, Sens_min14, sens_mod_14, sens_mod2_14, Sens_max14, spec_min14, spec_mod_14, spec_mod2_14, spec_max14,
			Variable15, Sens_min15, sens_mod_15, sens_mod2_15, Sens_max15, spec_min15, spec_mod_15, spec_mod2_15, spec_max15,
			Variable16, Sens_min16, sens_mod_16, sens_mod2_16, Sens_max16, spec_min16, spec_mod_16, spec_mod2_16, spec_max16,
			Variable17, Sens_min17, sens_mod_17, sens_mod2_17, Sens_max17, spec_min17, spec_mod_17, spec_mod2_17, spec_max17,
			Variable18, Sens_min18, sens_mod_18, sens_mod2_18, Sens_max18, spec_min18, spec_mod_18, spec_mod2_18, spec_max18,
			Variable19, Sens_min19, sens_mod_19, sens_mod2_19, Sens_max19, spec_min19, spec_mod_19, spec_mod2_19, spec_max19,
			Variable20, Sens_min20, sens_mod_20, sens_mod2_20, Sens_max20, spec_min20, spec_mod_20, spec_mod2_20, spec_max20 
			);
	/*Change SAS System Options for print the log*/
	options nosource nosource2 noserror nonotes;

	* create libname;
	libname sensdir "&libname";

	%do i=1 %to &N_Misc_var;
		%let var&i=&&Variable&i;
	%end;

	%let binary_mis=&indep;
	%Let N=&totalreps;

	/*******************************/
	/*If requested, delete exisiting datasets*/
	/*******************************/
	/*******************************/
	/*Creating temporary datasets for manipulation*/
	/*******************************/
	%tempdata;

	/*******************************/
	/*ERROR CHECKING MACRO */
	/*******************************/
	%checkerr;
	ods select none;

	%if &quitmac=1 %then
		%do;
			%goto stoper;
		%end;

	/*******************************/
	/*misclassifying the input datasets */
	/*******************************/
	%repeat1 (&N);

	/*******************************/
	/*Preparing the results for printing */
	/*******************************/
	%inputparam;
	%printresults;

	/*** Merge the misclassification parameter estimates with the ***/
	/*** distribution of sensitivity and specificity ***/
	proc Sql;
		create table Misclassification(drop=tmpid p pt) as
			select a.*,b.*
				from Corrected as a left join SN_SP(rename=(RunId=tmpid)) as b
					on a.runid=b.tmpid;
	quit;

	/*******************************/
	/*Plot distribution of sensitivity and specificity */
	/*******************************/
	/* Only print the plots when specified by the user  */
	%if %upcase(&print_plots) = YES %then
		%do;
			%graphs();
		%end;

%stoper:
	;
	ods select none;

	%if &quitmac=1 %then
		%do;
			%PUT ERROR: Misclassification macro stopped due to errors;
		%end;

	******* close opened dataset and delete temporary datasets *******;
	%close_all_dsid;

	/* Delete all global macro variables created */
	%SYMDEL  quitmac  Percent_thrownouts Percent_thrownout
		sp1 sp2 sp3 sp4 sp5 sp6 sp7 sp8 sp9 sp10 
		sn1 sn2 sn3 sn4 sn5 sn6 sn7 sn8 sn9 sn10 footnote / nowarn;

	/* Clear all libnames */
	libname _all_ clear;

	/* Reset the ods statment to print all results*/
	ods select all;

	/* resent all SAS system options */
	options source source2 serror notes;
	%put END TIME: %sysfunc(datetime(),datetime14.);
%mend Misclassification;

*****************************************************************************************;
*****************************************************************************************;
*****************************************************************************************;
*****************************************************************************************;
****************************** End Misclassification macro ******************************;
*****************************************************************************************;
*****************************************************************************************;
*****************************************************************************************;
*****************************************************************************************;

************************************************************;
******* Creating temporary datasets for manipulation *******;
************************************************************;
%macro tempdata();

	data sim1;
		set &inset;
	run;

%mend tempdata;

********************************************************************;
*********************** ERROR CHECKING MACRO ***********************;
********************************************************************;
%macro checkerr();
	* create global macro variable to decide termination and set to 0;
	%global quitmac  Percent_thrownouts Percent_thrownout
		convBETA convVAR sys_parm A B C D datetime_start
		sp1 sp2 sp3 sp4 sp5 sp6 sp7 sp8 sp9 sp10
		sn1 sn2 sn3 sn4 sn5 sn6 sn7 sn8 sn9 sn10
		err1 err2 err3 err4 err5 err6 err7 err8 err9 err10 
		LCL_exp_boot UCL_exp_boot;
	%let datetime_start = %sysfunc(TIME());
	%put START TIME: %sysfunc(datetime(),datetime14.);
	%let quitmac=0;

	%do i=1 %to 10;
		%let var&i=&&Variable&i;
		%let sp&i=0;
		%let sn&i=0;
		%let err&i=0;
	%end;

	%let datlen=%sysfunc(open(misclassification,i));

	*When requested by the user, delete exisiting data sets;
	%if %upcase(&startover) = YES and datlen>0 %then
		%do;

			proc datasets lib=work;
				delete misclassification corrected excluded;
			run;

		%end;

	%do i=1 %to 100;
		%let rc=%sysfunc(close(&i));
	%end;

	data Excluded;
		RunID=0;
		or_c=1;
		or_pa=1;
		or_pa_2=1;
		adjustment=1;
		or_ca=1;
		throwout=0;
	run;

	* determine if data set and exposure and outcome variables exist;
	%let datlen=%sysfunc(open(&inset,i));

	%if &datlen=0 %then
		%do;
			%put ERROR:  DATASET &inset DOES NOT EXIST;
			%let quitmac=1;
			%goto stoper;
		%end;

	%let explen=%sysfunc(varnum(&datlen,&exp));

	%if &explen=0 %then
		%do;
			%put ERROR:  VARIABLE &exp DOES NOT EXIST;
			%let quitmac=1;
			%goto stoper;
		%end;

	%let deplen=%sysfunc(varnum(&datlen,&depend));

	%if &deplen=0 %then
		%do;
			%put ERROR:  VARIABLE &depend DOES NOT EXIST;
			%let quitmac=1;
			%goto stoper;
		%end;

	* if no libname specified, terminate;
	%if %length(&libname)=0 %then
		%do;
			%put ERROR:  NO LIBNAME SPECIFIED;
			%let quitmac=1;
			%goto stoper;
		%end;

	* if input dataset not given, terminate;
	%if %length(&inset)=0 %then
		%do;
			%put ERROR:  NO INPUT DATASET SPECIFIED;
			%let quitmac=1;
			%goto stoper;
		%end;

	* if dependent variable not given, terminate;
	%if %length(&depend)=0 %then
		%do;
			%put ERROR:  NO DEPENDENT VARIABLE SPECIFIED;
			%let quitmac=1;
			%goto stoper;
		%end;

	* if predictor variable not given, terminate;
	%if %length(&exp)=0 %then
		%do;
			%put ERROR:  NO EXPOSURE VARIABLE SPECIFIED;
			%let quitmac=1;
			%goto stoper;
		%end;

	* if no misclassification variable is specified, use exp;
	%if %length(&Variable1)=0 %then
		%do;
			%put WARNING:  NO MISCLASSIFICATION VARIABLE SPECIFIED, SETTING = &exp;
			%let Variable1=&exp;
		%end;

	%if %length(&print_plots)=0 %then
		%do;
			%let print_plots = YES;
			%put WARNING: print_plots NOT SPECIFIED, SETTING = YES;
		%end;

	%let mvarlen=%sysfunc(varnum(&datlen,&Variable1));

	%if &mvarlen=0 %then
		%do;
			%put ERROR:  VARIABLE &Variable1 DOES NOT EXIST;
			%let quitmac=1;
			%goto stoper;
		%end;

* If other misclassified variables are specified, then ensure they exist in the data set;
%do i=2 %to &N_Misc_var;
	%if %length(&&Variable&i)>0 %then
		%do;
			%let var&i.len=%sysfunc(varnum(&datlen,&&Variable&i));

			%if &&var&i.len=0 %then
				%do;
					%put ERROR:  VARIABLE &&Variable&i DOES NOT EXIST;
					%let quitmac=1;
					%goto stoper;
				%end;
		%end;
%end;

	* make sure all misclassification parameters are specified;
	* is min sensitivity specified?;
%do i=1 %to &N_Misc_var;
	%if %length(&&sens_min&i)=0 %then
		%do;
			%put ERROR:  NO MINIMUM SENSITIVITY FOR &&Variable&i SPECIFIED;
			%let quitmac=1;
			%goto stoper;
		%end;

	* is max sensitivity specified?;
	%if %length(&&sens_max&i)=0 %then
		%do;
			%put ERROR:  NO MAXIMUM SENSITIVITY FOR &&Variable&i SPECIFIED;
			%let quitmac=1;
			%goto stoper;
		%end;

	* is min specificity specified?;
	%if %length(&&spec_min&i)=0 %then
		%do;
			%put ERROR:  NO MINIMUM SPECIFICITY FOR &&Variable&i SPECIFIED;
			%let quitmac=1;
			%goto stoper;
		%end;

	* is max specificity specified?;
	%if %length(&&spec_max&i)=0 %then
		%do;
			%put ERROR:  NO MAXIMUM SPECIFICITY FOR &&Variable&i SPECIFIED;
			%let quitmac=1;
			%goto stoper;
		%end;
%end;

	* make sure exp is not in the indep;
	%let cov_pred=%index(%upcase(&indep),%upcase(&exp));

	%if &cov_pred > 0 %then
		%do;
			%put ERROR: YOU CANNOT INCLUDE THE EXPOSURE ("exp") VARIABLE IN THE "indep" STRING;
			%let quitmac=1;
			%goto stoper;
		%end;

	* make sure depend is not in the indep;
	%let cov_out=%index(%upcase(&indep),%upcase(&depend));

	%if &cov_out > 0 %then
		%do;
			%put ERROR: YOU CANNOT INCLUDE THE OUTCOME ("depend") VARIABLE IN THE "indep" STRING. IF THE "depend" VARIABLE HAS A SIMILAR NAME TO ANOTHER VARIABLE IN THE "indpend" STRING, CONSIDER RE-NAMEING ONE OF THE VARIABLES;
			%let quitmac=1;
			%goto stoper;
		%end;

	* make sure that the variable is in predictors or is one of the main variables;
%do i=1 %to &N_Misc_var;
	%let mis1=%eval(%index(%upcase(&indep),%upcase(&&Variable&i))>0);
	%let mis2=%eval(%index(%upcase(&depend),%upcase(&&Variable&i))>0);
	%let mis3=%eval(%index(%upcase(&exp),%upcase(&&Variable&i))>0);

	%let mistake=%eval(&mis1+&mis2+&mis3);

	%if &mistake<1 %then %do;
			%put ERROR: AT LEAST ONE MISCLASSIFIED VARIABLE IS NOT INCLUDED IN indep, depend, OR exp STRINGS;
			%let quitmac=1;
			%goto stoper;
	%end;
%end;


	* if no totalreps specified, then specify 1000 repetitions;
	%if %length(&totalreps)=0 %then
		%do;
			%let totalreps=1000;
			%put WARNING: TOTALREPS NOT SPECIFIED, WILL LOOP TO 1000 REPETITIONS;
		%end;

	* Check on the type of distribution for misclassified variables;
%do i=1 %to &N_Misc_var;
	%if %length(&&sens_mod_&i)=0 and %length(&&sens_mod2_&i)=0 %then
		%do;
			%let sens_mod_&i=&&sens_min&i;
			%let sens_mod2_&i=&&sens_max&i;
			%put WARNING: Sensitivity Uniform Distribution for &&Variable&i;
		%end;
	%else %if %length(&&sens_mod2_&i)=0 %then
		%do;
			%let sens_mod2_&i=&&sens_mod_&i;
			%put WARNING: Sensitivity Triangular Distribution for &&Variable&i;
		%end;

	%if %length(&&spec_mod_&i)=0 and %length(&&spec_mod2_&i)=0 %then
		%do;
			%let spec_mod_&i=&&spec_min&i;
			%let spec_mod2_&i=&&spec_max&i;
			%put WARNING: Specificity Uniform Distribution for &&Variable&i;
		%end;
	%else %if %length(&&spec_mod2_&i)=0 %then
		%do;
			%let spec_mod2_&i=&&spec_mod_&i;
			%put WARNING: Specificity Triangular Distribution for &&Variable&i;
		%end;


	* make sure that the min is not greater than the max, etc;
	%if &&sens_min&i > &&sens_max&i or &&sens_min&i > &&sens_mod_&i or &&sens_min&i > &&sens_mod2_&i
		or &&sens_mod_&i > &&sens_mod2_&i or &&sens_mod_&i > &&sens_max&i or &&sens_mod2_&i > &&sens_max&i %then
		%do;
			%put ERROR: SENSITIVITY PARAMETERS ORDER for &&variable&i IS WRONG;
			%let quitmac=1;
			%goto stoper;
		%end;

	* make sure that the min is not greater than the max, etc;
	%if &&spec_min&i > &&spec_max&i or &&spec_min&i > &&spec_mod_&i or &&spec_min&i > &&spec_mod2_&i
		or &&spec_mod_&i > &&spec_mod2_&i or &&spec_mod_&i > &&spec_max&i or &&spec_mod2_&i > &&spec_max&i %then
		%do;
			%put ERROR: Specificity PARAMETERS ORDER for &&variable&i IS WRONG;
			%let quitmac=1;
			%goto stoper;
		%end;
%end;

	proc Printto log="&libname\misclassifaction_log.txt";
	run;

%do i=1 %to &N_Misc_var;
	* Check that the misclassified variables are a 0/1 dummy variables;
	proc Means noprint data=&inset noprint;
		var &&variable&i;
		output out=test2set min=min max=max;
	run;

	data test2set;
		set test2set;

		if min or max ne 1 then
			error=1;
		else error=0;
		call symput("err&i",error);
	run;

%end;

	proc Printto;
	run;

%do i=1 %to &N_Misc_var;
		%if &&err&i=1 %then %do;
			%put ERROR: &&variable&i MISCLASSIFIED VARIABLE IS NOT A 0/1 DUMMY VARIABLE;
			%let quitmac=1;
		%end;
%end;

%stoper:
	;
%mend checkerr;

*********************************************************************************;
********************** Creating misclassification estimates *********************;
*********************************************************************************;
data sn_sp_inputs;
	format Variable $char24. SN_MIN SN_MOD1 SN_MOD2 SN_MAX SP_MIN SP_MOD1 SP_MOD2 SP_MAX best8.;
run;

%macro inputparam();
	%do iii=1 %to &N_Misc_var;

		data sn_sp_input;
			format Variable $char24. SN_MIN SN_MOD1 SN_MOD2 SN_MAX SP_MIN SP_MOD1 SP_MOD2 SP_MAX best8.;
			Variable="&&variable&iii";
			SN_MIN=&&sens_min&iii;
			SN_MOD1=&&sens_mod_&iii;
			SN_MOD2=&&sens_mod2_&iii;
			SN_MAX=&&sens_max&iii;
			SP_MIN=&&spec_min&iii;
			SP_MOD1=&&spec_mod_&iii;
			SP_MOD2=&&spec_mod2_&iii;
			SP_MAX=&&spec_max&iii;
		run;

		proc Append data=sn_sp_input base=sn_sp_inputs;
		run;

	%end;

	data sn_sp_inputs;
		set sn_sp_inputs;

		if not missing(SP_MAX);
	run;

%mend inputparam;

%macro repeat1 (loopn);
	%let pp=0;

	%do %while (&pp < &loopn);
%top:
		;
		%let pp=%eval(&pp+1);

		data SN_SP0;
			RunID=&PP;
		run;

		*******************Distribution for Sensitivity and Specificity******************;
%macro SN_SP_repeat();
	%do aaa=1 %to &N_Misc_var;

		data SN_SP0;
			set SN_SP0;
			sens_min&aaa=&&sens_min&aaa;
			sens_mod_&aaa=&&sens_mod_&aaa;
			sens_mod2_&aaa=&&sens_mod2_&aaa;
			sens_max&aaa=&&sens_max&aaa;
			spec_min&aaa=&&spec_min&aaa;
			spec_mod_&aaa=&&spec_mod_&aaa;
			spec_mod2_&aaa=&&spec_mod2_&aaa;
			spec_max&aaa=&&spec_max&aaa;
		run;

	%end;
%mend;

%SN_SP_repeat;

/*Putting a range around the sensitivity and specificity*/
data SN_SP0;
	set SN_SP0;
	array Sens_min [*] Sens_min1-Sens_min&N_Misc_var;
	array Sens_mod_ [*] Sens_mod_1-Sens_mod_&N_Misc_var;
	array Sens_mod2_ [*] Sens_mod2_1-Sens_mod2_&N_Misc_var;
	array Sens_max [*] Sens_max1-Sens_max&N_Misc_var;
	array Spec_min [*] Spec_min1-Spec_min&N_Misc_var;
	array Spec_mod_ [*] Spec_mod_1-Spec_mod_&N_Misc_var;
	array Spec_mod2_ [*] Spec_mod2_1-Spec_mod2_&N_Misc_var;
	array Spec_max [*] Spec_max1-Spec_max&N_Misc_var;
	array SN [&N_Misc_var];
	array SP [&N_Misc_var];

	do ii=1 to &N_Misc_var;
		* create a trapezoidal distribution based on input parameters and choose;
		* sensitivity from it;
		p=ranuni(-1);
		SN[ii] =  (p*(sens_max[ii]+sens_mod2_[ii]-sens_min[ii]-SENS_MOD_[ii]) + (sens_min[ii] + SENS_MOD_[ii]))/2;

		if SN[ii] < SENS_MOD_[ii] then
			do;
				SN[ii]  = sens_min[ii] + sqrt((SENS_MOD_[ii]-sens_min[ii])*(2*SN[ii]-sens_min[ii]-SENS_MOD_[ii]));
			end;
		else if SN[ii] > sens_mod2_[ii] then
			do;
				SN[ii] = sens_max[ii] - sqrt(2*(sens_max[ii]-sens_mod2_[ii])*(SN[ii]-sens_mod2_[ii]));
			end;

		* create a trapezoidal distribution based on input parameters and choose;
		* specificity from it;
		pt=ranuni(-1);
		SP[ii] =  (pt*(spec_max[ii]+spec_mod2_[ii]-spec_min[ii]-SPEC_MOD_[ii]) + (spec_min[ii] + SPEC_MOD_[ii]))/2;

		if SP[ii] < SPEC_MOD_[ii] then
			do;
				SP[ii]  = spec_min[ii] + sqrt((SPEC_MOD_[ii]-spec_min[ii])*(2*SP[ii]-spec_min[ii]-SPEC_MOD_[ii]));
			end;
		else if SP[ii] > spec_mod2_[ii] then
			do;
				SP[ii] = spec_max[ii] - sqrt(2*(spec_max[ii]-spec_mod2_[ii])*(SP[ii]-spec_mod2_[ii]));
			end;
	end;

	drop spec_min: sens_min: SPEC_MOD_: SENS_MOD_: SPEC_MOD2_: SENS_MOD2_: spec_max: sens_max:;
run;

%macro SN_SP_repeat2();
	%do bbb=1 %to &N_Misc_var;

		Data SN_SP0;
			set SN_SP0;
			label 
				SN&BBB="Sensitivity of &&Variable&BBB"
				SP&BBB="Specificity of &Variable&BBB";
			call symput("SN&BBB",SN&BBB);
			call symput("SP&BBB",SP&BBB);
		run;

	%end;
%mend;

%SN_SP_repeat2;

/*Misclassifying the data set to obtain an additional confounded effect*/
%macro looping2 (q);

	data sim2;
		set sim1;
	run;

	%do j=1 %to &q;

		data sim2;
			set sim2;

			*call streaminit(1234);
			if &&var&j.=1 then
				&&var&j.=rand("binomial",&&SN&j.,1);
			else if &&var&j.=0 then
				&&var&j.=rand("binomial",1-(&&SP&j.),1);
		run;

	%end;
%mend looping2;

%looping2 (q=&N_Misc_var);

/*Obtaining the PARTIALLY adjusted estimate for the exposure variable with one order of misclassification*/
* pull out parameters for conventional analysis;
ods output ParameterEstimates=OR_PA;

proc Logistic data=sim1 descending covout outest=Conventional00;
	* note that there is no class statement, so all variables must be either
	  dummy variables or continuous;
	model &depend = &exp &indep;
run;

data Conventional00;
	set Conventional00;

	if _TYPE_="PARMS" then
		call symput("convBETA",&exp);

	if _TYPE_="COV" and lowcase(_name_)=lowcase("&exp") then
		call symput("convVAR",&exp);
run;

/*Obtaining the PARTIALLY adjusted estimate for the exposure variable with two orders of misclassification*/
ods output ParameterEstimates=OR_PA_2;

proc Logistic data=sim2;
	model &depend(event=last)=&exp &binary_mis;
run;

/*Obtaining the UNADJUSTED and FULLY CONFOUNDED estimate for the exposure variable with two orders of misclassification*/
ods output ParameterEstimates=OR_C;

proc Logistic data=&inset;
	model &depend(event=last)=&exp;
run;

/*Local macro for partially adjusted with 1 order of misclassifcation*/
data _NULL_;
	set OR_PA;

	if variable="&exp";
	LCL=(exp(estimate-1.96*StdErr));
	UCL=(exp(estimate+1.96*StdErr));
	estimate=exp(estimate);
	call symput("OR_PA",estimate);
	call symput("LCL_OR_PA",LCL);
	call symput("UCL_OR_PA",UCL);
run;

/*Local macro for partially adjust with 2 orders of misclassifcation*/
data _NULL_;
	set OR_PA_2;

	if variable="&exp";
	estimate=exp(estimate);
	call symput("OR_PA_2",estimate);
run;

/*Local macro for unadjusted estimate*/
data _NULL_;
	set OR_C;

	if variable="&exp";
	LCL=(exp(estimate-1.96*StdErr));
	UCL=(exp(estimate+1.96*StdErr));
	estimate=exp(estimate);
	call symput("OR_C",estimate);
	call symput("LCL_OR_C",LCL);
	call symput("UCL_OR_C",UCL);
run;

/*Getting the percent adjustment using Savitz et al. formula into a local macro*/
data adjust;
	throwout=0;
	adjustment=(((&OR_PA_2)-(&OR_C))/((&OR_PA)-(&OR_C)));
	call symput("Adjustment",adjustment);
run;

proc Sql;
	create table adjusted as
		select distinct 
			&pp as RunID, &OR_C as OR_C label="Fully confounded estimate", 
			&OR_PA as OR_PA label="Partially adjusted estimate with 1 order of misclassifcation", 
			&OR_PA_2 as OR_PA_2 label="Partially adjusted estimate with 2 orders of misclassifcation",
			&adjustment as adjustment Label="Percent Adjustment",
			((((&OR_PA)-(&OR_C))/(&adjustment))+(&OR_C)) as OR_CA label="Adjusted Estimate after adjustment", 
		case
			when calculated OR_CA < 0 then 1
			else 0
		end 
	as throwout
		from OR_C;
	Select throwout, OR_CA into :throwout, :sys_parm
		from adjusted;
quit;

%if &throwout=1 %then
	%do;

		proc Append data=adjusted base=Excluded;
		run;

		%goto top;
	%end;

proc Append data=SN_SP0 base=SN_SP;
run;

* merge the two parameter datasets;
data combineb;
	set adjusted (keep=RunID adjustment or_pa_2 or_c);

	* initialize the random number generator seeds;
	retain zns1 zns2;

	if zns1 eq . then
		do;
			zns1 = -1;
			zns2 = -1;
		end;

	* create a distribution to represent result without sensitivity analysis;
	znor1=rannor(zns1);
	znor2=rannor(zns2);
	conv_var=&convVAR;

	* create a random error estimate using the conventional variance;
	conv_parm = &convBETA - znor1*sqrt(&convVAR);

	* create a distribution to represent systematic error;
	sys_parm = log(&sys_parm);

	* create a distribution to represent approximation to bootstrapping analysis;
	boot_parm = sys_parm - znor2*sqrt(&convVAR);

	* create odds ratio, systematic error;
	exp_sys = &sys_parm;

	* create odds ratio, conventional analysis;
	exp_conv = exp(conv_parm);

	* create odds ratio, boot approx;
	exp_boot = exp(boot_parm);
	label 	conv_parm='Beta RANDOM ERROR'
		boot_parm='Beta SYSTEMATIC and RANDOM ERROR'
		sys_parm='Beta SYSTEMATIC' 
		znor1="Standard Normal Deviate for RANDOM ERROR"
		znor2="Standard Normal Deviate for BOOT APPROX"
		exp_sys = 'Odds Ratio, Systematic Error'
		exp_conv = 'Odds Ratio, Conventional Analysis'
		exp_boot = 'Odds Ratio, Total Error Analysis'
		conv_var = 'Conventional Variance';
	drop zns1 zns2 znor1 znor2;
run;

proc Append data=combineb base=Corrected;
run;

proc Sql;
	select distinct count(*) into :thrownout
		from Excluded
			where throwout=1;
quit;

Data _null_;
	complete=Round((&PP/&N)*100,0.1);
	complete1=cat(complete,"%");
	Percent_thrownout=Round((&thrownout/(&pp+&thrownout))*100,0.1);
	Percent_thrownout1=cat(Percent_thrownout,"%");
	call symput("complete",COMPRESS(complete1));
	call symput("Percent_thrownout",COMPRESS(Percent_thrownout1));
	call symput("Percent_thrownouts",Percent_thrownout);
run;

%Put Run=&PP     Percent Complete=&complete     Percent Thrown out=&Percent_thrownout     PROCESSING TIME:  %sysfunc(putn(%sysevalf(%sysfunc(TIME())-&datetime_start.),mmss.)) (mm:ss);
%end;
%mend;

**************************************************************;
**************************************************************;

***** close opened dataset and delete temporary datasets *****
**************************************************************;
%macro close_all_dsid;
	%local i rc;

	%do i=1 %to 100;
		%let rc=%sysfunc(close(&i));
	%end;

	proc Datasets lib=work;
		delete sim2 adjust OR_C OR_PA OR_PA_2 OR ADJUSTED Confounded Conventional 
			percentiles1 systematic bootstrap TEST2SET adjusted SN_SP SN_SP0
			Conventional00 exp_sys exp_conv exp_boot combineb sn_sp_input;
	run;

%mend close_all_dsid;

/**************************************************************************/
/******************* Preparing the results for printing *******************/
/**************************************************************************/
/*Local macro for partially adjust with 1 order of misclassifcation*/
%macro printresults();

	data _NULL_;
		set OR_PA;

		if variable="&exp";
		LCL=(exp(estimate-1.96*StdErr));
		UCL=(exp(estimate+1.96*StdErr));
		estimate=exp(estimate);
		call symput("OR_PA",estimate);
		call symput("LCL_OR_PA",LCL);
		call symput("UCL_OR_PA",UCL);
	run;

	/*Local macro for unadjust estimate*/
	data _NULL_;
		set OR_C;

		if variable="&exp";
		N_sim="  -  ";
		LCL=(exp(estimate-1.96*StdErr));
		UCL=(exp(estimate+1.96*StdErr));
		estimate=exp(estimate);
		call symput("OR_C",estimate);
		call symput("LCL_OR_C",LCL);
		call symput("UCL_OR_C",UCL);
	run;

	ods select none;

	proc Sql;
		Select distinct (select count(*) from corrected where exp_conv LE 1)/count(*) as conv,
		(select count(*) from corrected where exp_sys LE 1)/count(*) as systematic,
		(select count(*) from corrected where exp_boot LE 1)/count(*) as bootstrap
			into :conv, :systematic, :bootstrap
				from corrected;
	quit;

	proc Univariate data=corrected;
		var adjustment;
		output out=percentiles1 median=median pctlpts=2.5 97.5 pctlpre=P;
	run;

	proc Univariate data=corrected;
		var exp_sys;
		output out=exp_sys median=median pctlpts=2.5 97.5 pctlpre=P;
	run;

	proc Univariate data=corrected;
		var exp_conv;
		output out=exp_conv median=median pctlpts=2.5 97.5 pctlpre=P;
	run;

	proc Univariate data=corrected;
		var exp_boot;
		output out=exp_boot median=median pctlpts=2.5 97.5 pctlpre=P;
	run;

	proc Sql;
		select count(*) as countN format=best5.
			into :countN
				from corrected;
	quit;

	ods select all;

	Data Confounded;
		format method $char60.;

		Method="Unadjusted Estimate (Fully Confounded)";
		N_sim="  -  ";
		Estimate=&OR_C;
		LCL=&LCL_OR_C;
		UCL=&UCL_OR_C;
		CL_Width=&UCL_OR_C-&LCL_OR_C;
		P_LT_1=.;
	run;

	Data Conventional;
		set exp_conv;
		format method $char60. P_LT_1 percent10.;

		Method="Conventional Analysis (Partial Adjustment for Confounding)";
		N_sim="&countN";
		CL_Width=p97_5-p2_5;
		P_LT_1=&conv;
		rename median=Estimate p2_5=LCL p97_5=UCL;
	run;

	Data Systematic;
		set exp_sys;
		format method $char60.;

		Method="Systematic Error Analysis (only systematic error)";
		N_sim="&countN";
		CL_Width=p97_5-p2_5;
		P_LT_1=&systematic;
		rename median=Estimate p2_5=LCL p97_5=UCL;
	run;

	Data bootstrap;
		set exp_boot;
		format method $char60.;

		Method="Total Error Analysis (both random and systematic errors)";
		N_sim="&countN";
		CL_Width=p97_5-p2_5;
		P_LT_1=&bootstrap;
		rename median=Estimate p2_5=LCL p97_5=UCL;
		call symput("LCL_exp_boot",STRIP(round(p2_5,0.01)));
		call symput("UCL_exp_boot",STRIP(round(p97_5,0.01)));
	run;

	Data percentiles1;
		set percentiles1;
		format method $char60.;

		Method="Percent adjustment using the misclassified variables";
		N_sim="&countN";
		CL_Width=p97_5-p2_5;
		rename median=Estimate p2_5=LCL p97_5=UCL;
	run;

	data results;
		set Confounded Conventional percentiles1 systematic bootstrap;
	run;

	Title1 "SENSITIVITY ANALYSIS RESULTS";
	Title2 "Correcting for &N_Misc_var misclassified variable(s)";

	proc Print data=results label;
		var Method N_sim Estimate lcl ucl CL_Width p_lt_1;
		label N_sim="N Simulations" Estimate="Estimate" lcl="2.5% LCL"
			ucl="97.5% UCL" CL_Width="CL Width" p_lt_1="% OR < 1";
	run;

	Title "Input parameters for sensitivity and specificity";

	Proc Print data=sn_sp_inputs;
	run;

	title;
	Title "proportion of deleted iterations";

	proc Sql;
		select distinct &Percent_thrownouts as Percent_thrownout label="Percent of simulations removed"
			from corrected;
	quit;

	title;
	%let foot= .;

	%if &Percent_thrownouts > 0 %then
		%do;
			%let footnote=&Percent_thrownout of simulations had an odds ratio less than zero and were removed from the results. Caution should be excercised when the proportion of removed simulations exceeds 10%.;
			%put WARNING: &footnote;
		%end;

	Title1;
	Title2;
%mend printresults;

***************************************************;

***** Print sensitivity and specificity plots *****
***************************************************;
%macro graphs ();

	proc Sgplot data=misclassification;
		title1 "Distribution of Odds Ratios";
		density exp_boot / SCALE=PERCENT legendlabel="Total Error Analysis" type=kernel name="plot1" lineattrs=(color=black);
		density exp_conv / SCALE=PERCENT legendlabel="Conventional  Analysis" type=kernel name="plot2" lineattrs=(color=red Pattern=LONGDASH);
		density exp_sys  / SCALE=PERCENT legendlabel="Systematic Error Analysis" type=kernel name="plot3" lineattrs=(color=orange PATTERN=dashdashdot);
		*xaxis label="Odds Ratio"  min=0.5 max=2;
		yaxis min=0 max=40;
		keylegend  "plot1" "plot2" "plot3" / location=outside position=bottom;
	run;

	proc Sgplot data=misclassification noautolegend;
		title1 "Distribution of Odds Ratios";
		title2 "Total Error Analysis";
		histogram exp_boot / fillattrs=(color=black) name="plot1" transparency=0.5;
		density exp_boot / legendlabel="Total Error Analysis" type=kernel lineattrs=(color=black) name="plot2";
		xaxis label="Odds Ratio";
		refline &LCL_exp_boot /axis=x label="Lower 95% CL = &LCL_exp_boot" lineattrs=(color=black PATTERN=dashdashdot);
		refline &UCL_exp_boot /axis=x label="Upper 95% CL = &UCL_exp_boot" lineattrs=(color=black PATTERN=dashdashdot);
	run;

	title1;
	title2;

	* options for the y-axis;
	axis1 c=bl label=(A=90 F=swiss H=16 pt J=center 'Frequency') 
		major=(W=2) offset=(0,0) width=4 value=(F=swiss H=12 pt);

	* options for the x-axis;
	axis2 c=bl label=(A=0 F=swiss H=16 pt J=center) value=(F=swiss H=12 pt) 
		offset=(0 cm,0 cm) width=4 value=(F=swiss H=12 pt);

	* plot sensitivity and specificity;
	proc Gchart data=SN_SP;
		vbar sn1-sn&N_Misc_var sp1-sp&N_Misc_var /raxis=axis1 maxis=axis2;
		title "Simulated Sensitivity and Specificity Plots";
	run;

	title;
	quit;

%mend graphs;
