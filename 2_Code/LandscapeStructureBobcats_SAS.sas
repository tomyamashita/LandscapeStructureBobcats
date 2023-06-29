dm "output;clear;log;clear";
ods listing; ods html close; ods graphics off; 

options nocenter;

ods pdf file = 'F:\TAMUK\Data_and_Analyses\Chapter_RemoteSensing\Analysis_SAS\GLMM_all_20230406.pdf'; 

proc datasets lib=work kill memtype=data;
run; 

proc import out=ds
	datafile="F:\TAMUK\Data_and_Analyses\Chapter_RemoteSensing\Analysis_SAS\AllData_Regression_20230330.xlsx" dbms=xlsx replace;
run;

proc print data=ds; 
run; 

proc standard data = ds out = dsn mean = 0 std = 1; 
var pland--m3_0; 
run; 

proc print data = dsn; 
run; 

proc means mean var data = dsn; 
run; 

*PCA on structure variables; 

proc princomp data = dsn out = PC_config prefix = PCconfig; 
var pland PD LPI ED LSI AREA_MN ENN_MN AI; 
run; 

*PCA on lidar variables; 

proc princomp data = dsn out = PC_lidar prefix = PClidar; 
var m0_5 m1_0 m1_5 m2_0 m2_5 m3_0; 
run; 

*Combine data into a single dataset; 

data dsall; 
ID = _n_; 
merge PC_config PC_lidar; 
keep ID det_av Site yearmonth PLAND PD LPI ED LSI AREA_MN ENN_MN AI m0_5 m1_0 m1_5 m2_0 m2_5 m3_0 CanopyH_MN PCconfig1 PCconfig2 PClidar1; 
run; 

proc print data=dsall; 
run; 


* Test for error correlation structure for different sets of predictors;
**Configuration models; 
%macro glmm_config (type = , fit = , status = ); 
	proc glimmix data = dsall method = mmpl pconv = 0.001; 
		class site yearmonth; 
		model det_av = PCconfig1 PCconfig2 CanopyH_MN/solution dist = negbin; 
		random _residual_/subject = site type = &type; 
		ods output fitstatistics = &fit convergencestatus = &status; 
%mend glmm_config; 

data structures; 
	input model structure $char9.; 
	cards; 
		1 vc
		2 cs
		3 csh
		4 ar(1)
		5 arh(1)
		6 toep
		7 toeph
		8 arma(1,1)
		9 un
;

data _null_; 
	if 0 then set structures nobs = n; 
	call symputx('nn',n); 
	stop; 
run; 
%put nobs = &nn; 

%macro assemble (all, conv, best); 
	%macro ml (n); 
		%do i = 1 %to &n; 
			data fit&i; set fit&i; 
			model = &i; 
			if descr = '-2 Log Pseudo-Likelihood';

			data status&i; set status&i; 
			model = &i; 
		%end; 
	; 
	%mend; 
	%ml(&nn); 

	%macro allset (n); 
		data allfit; 
		set fit1
		%do i = 2 %to &n; 
			fit&i
		%end; 
	;
	%mend; 
	%allset (&nn); 

	%macro allset (n); 
		data allstatus; 
		set status1 
		%do i = 2 %to &n; 
			status&i
		%end; 
	;
	%mend; 
	%allset (&nn); 

	data &all; 
		merge structures allfit allstatus; by model; 
		drop descr; 
		rename Value = AICc; 
	run; 

	proc sort data = &all; by AICc; 

	data &conv; 
		set &all; 
		if status = 1 then delete; 

	data &best; 
		set &conv (firstobs = 1 obs = 1); 
;
%mend assemble; 

*Test different correlation strutures for configuration; 
%glmm_config(type = vc,           fit = fit1, status = status1);
%glmm_config(type = cs,           fit = fit2, status = status2);
%glmm_config(type = csh,          fit = fit3, status = status3);
%glmm_config(type = ar(1),        fit = fit4, status = status4); 
%glmm_config(type = arh(1),       fit = fit5, status = status5);
%glmm_config(type = toep,         fit = fit6, status = status6);
%glmm_config(type = toeph,        fit = fit7, status = status7);
%glmm_config(type = arma(1,1),    fit = fit8, status = status8);
%glmm_config(type = un,           fit = fit9, status = status9);

*Compile all the fit statistics into a single dataset; 
%assemble (all_config, conv_config, best_config); 

proc print data = all_config; 
run; 

proc print data = conv_config; 
run; 

proc print data = best_config; 
run; 

*Identify and run the best model; 

data best; 
	set best_config; 
	type = structure; 
	call symputx('type', type); 
run; 
%put best structure is &type; 

proc glimmix data = dsall method = mmpl pconv = 0.001;
	class site yearmonth;
	model det_av =  PCconfig1 PCconfig2 CanopyH_MN/solution dist = negbin;
	random _residual_/subject = site type = &type;
	ods output fitstatistics = fit_config; 
run;
quit; 


**LiDAR Models;
proc print data = dsall; 
run; 

%macro glmm_lidar (type = , fit = , status = ); 
	proc glimmix data = dsall method = mmpl pconv = 0.001; 
		class site yearmonth; 
		model det_av = PClidar1 CanopyH_MN/solution dist = negbin; 
		random _residual_/subject = site type = &type; 
		ods output fitstatistics = &fit convergencestatus = &status; 
%mend glmm_lidar; 

data structures; 
	input model structure $char9.; 
	cards; 
		1 vc
		2 cs
		3 csh
		4 ar(1)
		5 arh(1)
		6 toep
		7 toeph
		8 arma(1,1)
		9 un
;

data _null_; 
	if 0 then set structures nobs = n; 
	call symputx('nn',n); 
	stop; 
run; 
%put nobs = &nn; 

%macro assemble (all, conv, best); 
	%macro ml (n); 
		%do i = 1 %to &n; 
			data fit&i; set fit&i; 
			model = &i; 
			if descr = '-2 Log Pseudo-Likelihood';

			data status&i; set status&i; 
			model = &i; 
		%end; 
	; 
	%mend; 
	%ml(&nn); 

	%macro allset (n); 
		data allfit; 
		set fit1
		%do i = 2 %to &n; 
			fit&i
		%end; 
	;
	%mend; 
	%allset (&nn); 

	%macro allset (n); 
		data allstatus; 
		set status1 
		%do i = 2 %to &n; 
			status&i
		%end; 
	;
	%mend; 
	%allset (&nn); 

	data &all; 
		merge structures allfit allstatus; by model; 
		drop descr; 
		rename Value = AICc; 
	run; 

	proc sort data = &all; by AICc; 

	data &conv; 
		set &all; 
		if status = 1 then delete; 

	data &best; 
		set &conv (firstobs = 1 obs = 1); 
;
%mend assemble; 

%glmm_lidar(type = vc,           fit = fit1, status = status1);
%glmm_lidar(type = cs,           fit = fit2, status = status2);
%glmm_lidar(type = csh,          fit = fit3, status = status3);
%glmm_lidar(type = ar(1),        fit = fit4, status = status4); 
%glmm_lidar(type = arh(1),       fit = fit5, status = status5);
%glmm_lidar(type = toep,         fit = fit6, status = status6);
%glmm_lidar(type = toeph,        fit = fit7, status = status7);
%glmm_lidar(type = arma(1,1),    fit = fit8, status = status8);
*%glmm_lidar(type = un,           fit = fit9, status = status9);

%assemble(all_lidar, conv_lidar, best_lidar)

proc print data = all_lidar; 
run; 

proc print data = conv_lidar; 
run; 

proc print data = best_lidar; 
run; 

data best; 
	set best_lidar; 
	type = structure; 
	call symputx('type', type); 
run; 
%put best structure is &type; 

proc glimmix data = dsall method = mmpl pconv = 0.001;
	class site yearmonth;
	model det_av =  PClidar1 CanopyH_MN/solution dist = negbin;
	random _residual_/subject = site type = &type;
	ods output fitstatistics = fit_lidar;
run;
quit; 


** Combined Models;
proc print data = dsall; 
run; 

%macro glmm_all (type = , fit = , status = ); 
	proc glimmix data = dsall method = mmpl pconv = 0.001; 
		class site yearmonth; 
		model det_av = PCconfig1 PCconfig2 PClidar1 CanopyH_MN/solution dist = negbin; 
		random _residual_/subject = site type = &type; 
		ods output fitstatistics = &fit convergencestatus = &status; 
%mend glmm_all; 

data structures; 
	input model structure $char9.; 
	cards; 
		1 vc
		2 cs
		3 csh
		4 ar(1)
		5 arh(1)
		6 toep
		7 toeph
		8 arma(1,1)
		9 un
;

data _null_; 
	if 0 then set structures nobs = n; 
	call symputx('nn',n); 
	stop; 
run; 
%put nobs = &nn; 

%macro assemble (all, conv, best); 
	%macro ml (n); 
		%do i = 1 %to &n; 
			data fit&i; set fit&i; 
			model = &i; 
			if descr = '-2 Log Pseudo-Likelihood';

			data status&i; set status&i; 
			model = &i; 
		%end; 
	; 
	%mend; 
	%ml(&nn); 

	%macro allset (n); 
		data allfit; 
		set fit1
		%do i = 2 %to &n; 
			fit&i
		%end; 
	;
	%mend; 
	%allset (&nn); 

	%macro allset (n); 
		data allstatus; 
		set status1 
		%do i = 2 %to &n; 
			status&i
		%end; 
	;
	%mend; 
	%allset (&nn); 

	data &all; 
		merge structures allfit allstatus; by model; 
		drop descr; 
		rename Value = AICc; 
	run; 

	proc sort data = &all; by AICc; 

	data &conv; 
		set &all; 
		if status = 1 then delete; 

	data &best; 
		set &conv (firstobs = 1 obs = 1); 
;
%mend assemble; 


%glmm_all(type = vc,           fit = fit1, status = status1);
%glmm_all(type = cs,           fit = fit2, status = status2);
%glmm_all(type = csh,          fit = fit3, status = status3);
%glmm_all(type = ar(1),        fit = fit4, status = status4); 
%glmm_all(type = arh(1),       fit = fit5, status = status5);
%glmm_all(type = toep,         fit = fit6, status = status6);
%glmm_all(type = toeph,        fit = fit7, status = status7);
%glmm_all(type = arma(1,1),    fit = fit8, status = status8);
%glmm_all(type = un,           fit = fit9, status = status9);

%assemble(all_full, conv_full, best_full)

proc print data = all_full; 
run; 

proc print data = conv_full; 
run; 

proc print data = best_full; 
run; 

data best; 
	set best_full; 
	type = structure; 
	call symputx('type', type); 
run; 
%put best structure is &type; 

*Full model with the the PCA variables as the main variables; 

proc glimmix data = dsall method = mmpl pconv = 0.001;
	class site yearmonth;
	model det_av = PCconfig1 PCconfig2 PClidar1 CanopyH_MN/solution dist = negbin covb;
	estimate 'yhat at -2' intercept 1 PCconfig1 -2 PCconfig2 0 PClidar1 0 CanopyH_MN 0.3173078/ilink cl;
	estimate 'yhat at -1' intercept 1 PCconfig1 -1 PCconfig2 0 PClidar1 0 CanopyH_MN 0.3173078/ilink cl;
	estimate 'yhat at 0' intercept 1 PCconfig1 0 PCconfig2 0 PClidar1 0 CanopyH_MN 0.3173078/ilink cl;
	estimate 'yhat at 1' intercept 1 PCconfig1 1 PCconfig2 0 PClidar1 0 CanopyH_MN 0.3173078/ilink cl;
	estimate 'yhat at 2' intercept 1 PCconfig1 2 PCconfig2 0 PClidar1 0 CanopyH_MN 0.3173078/ilink cl;
	random _residual_/subject = site type = &type;
	
	ods output fitstatistics = fit_ful1 parameterestimates = betahat covb = covb;
run;quit; 


** Interaction Models;
proc print data = dsall; 
run; 

%macro glmm_int (type = , fit = , status = ); 
	proc glimmix data = dsall method = mmpl pconv = 0.001; 
		class site yearmonth; 
		model det_av = PCconfig1 PCconfig2 PClidar1 PCconfig1*PClidar1 PCconfig2*PClidar1 CanopyH_MN/solution dist = negbin; 
		random _residual_/subject = site type = &type; 
		ods output fitstatistics = &fit convergencestatus = &status; 
%mend glmm_int; 

data structures; 
	input model structure $char9.; 
	cards; 
		1 vc
		2 cs
		3 csh
		4 ar(1)
		5 arh(1)
		6 toep
		7 toeph
		8 arma(1,1)
		9 un
;

data _null_; 
	if 0 then set structures nobs = n; 
	call symputx('nn',n); 
	stop; 
run; 
%put nobs = &nn; 

%macro assemble (all, conv, best); 
	%macro ml (n); 
		%do i = 1 %to &n; 
			data fit&i; set fit&i; 
			model = &i; 
			if descr = '-2 Log Pseudo-Likelihood';

			data status&i; set status&i; 
			model = &i; 
		%end; 
	; 
	%mend; 
	%ml(&nn); 

	%macro allset (n); 
		data allfit; 
		set fit1
		%do i = 2 %to &n; 
			fit&i
		%end; 
	;
	%mend; 
	%allset (&nn); 

	%macro allset (n); 
		data allstatus; 
		set status1 
		%do i = 2 %to &n; 
			status&i
		%end; 
	;
	%mend; 
	%allset (&nn); 

	data &all; 
		merge structures allfit allstatus; by model; 
		drop descr; 
		rename Value = AICc; 
	run; 

	proc sort data = &all; by AICc; 

	data &conv; 
		set &all; 
		if status = 1 then delete; 

	data &best; 
		set &conv (firstobs = 1 obs = 1); 
;
%mend assemble; 

%glmm_int(type = vc,           fit = fit1, status = status1);
%glmm_int(type = cs,           fit = fit2, status = status2);
%glmm_int(type = csh,          fit = fit3, status = status3);
%glmm_int(type = ar(1),        fit = fit4, status = status4); 
%glmm_int(type = arh(1),       fit = fit5, status = status5);
%glmm_int(type = toep,         fit = fit6, status = status6);
%glmm_int(type = toeph,        fit = fit7, status = status7);
%glmm_int(type = arma(1,1),    fit = fit8, status = status8);
%glmm_int(type = un,           fit = fit9, status = status9);

%assemble(all_int, conv_int, best_int)

proc print data = all_int; 
run; 

proc print data = conv_int; 
run; 

proc print data = best_int; 
run; 

data best; 
	set best_int; 
	type = structure; 
	call symputx('type', type); 
run; 
%put best structure is &type; 

proc glimmix data = dsall method = mmpl pconv = 0.001;
	class site yearmonth;
	model det_av =  PCconfig1 PCconfig2 PClidar1 PCconfig1*PClidar1 PCconfig2*PClidar1 CanopyH_MN/solution dist = negbin;
	random _residual_/subject = site type = &type;
	ods output fitstatistics = fit_int;
run;
quit; 


*Additive full model to assess additional effects of including LiDAR; 
**Compare a full model to a structures only model; 

data fit_full; 
	set fit_ful1; 
	if descr = '-2 Log Pseudo-Likelihood'; 
	rename value = Minus2LogLFull; 
run; 

data fit_red; 
	set fit_config; 
	if descr = '-2 Log Pseudo-Likelihood'; 
	rename value = Minus2LogLRed; 
run; 

data fit; 
	merge fit_full fit_red; 
	chisqcalc = Minus2LogLFull - Minus2LogLRed; 
	pvalue = 1 - probchi(chisqcalc,1); 
run; 

proc print data = fit; 
run; 

proc print data=betahat;
run; 

proc print data=covb;
run;

proc export data = dsall
outfile = "F:\TAMUK\Data_and_Analyses\Chapter_RemoteSensing\Analysis_SAS\data_PCA_20230406.csv" dbms = csv replace;
run; 

proc export data = betahat
outfile = "F:\TAMUK\Data_and_Analyses\Chapter_RemoteSensing\Analysis_SAS\betahats_20230406.csv" dbms = csv replace;
run;

proc export data = covb
outfile = "F:\TAMUK\Data_and_Analyses\Chapter_RemoteSensing\Analysis_SAS\varcov_20230406.csv" dbms = csv replace;
run;


ods pdf close; 
