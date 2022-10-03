/* import data */
proc import datafile="/home/u45009217/SMU/DS6372/Data/cars_no_na.csv" dbms=dlm 
		out=cars_no_na replace;
	delimiter=',';
	getnames=yes;
run;

/* get rid of 'var1' column - was created when imported */
proc print data=cars_no_na;
run;

data cars_no_na (keep=Make Model Year Engine_Fuel_Type Engine_HP 
		Engine_Cylinders Transmission_Type Driven_Wheels Number_of_Doors 
		Market_Category Vehicle_Size Vehicle_Style highway_MPG city_mpg Popularity 
		MSRP combined_mpg BeforeAfter2000);
	set cars_no_na;
run;

/* Corr plot */
proc corr data=cars_no_na;
run;

/* scatterplots of numeric columns */
symbol c=blue v=dot;

proc sgscatter data=cars_no_na;
	matrix MSRP Engine_HP Popularity Year city_mpg combined_mpg highway_MPG;
run;

/* transform some variables to test*/
data log_data;
	set cars_no_na;
	logMSRP=log(MSRP);
	logHP=log(Engine_HP);
run;

/* plot the log data */
symbol c=blue v=dot;

proc sgscatter data=log_data;
	matrix MSRP Engine_HP logMSRP logHP;
run;

/* get column names */
proc contents data=cars_no_na out=meta (keep=NAME);
run;

proc print data=meta;
run;

/* forward selection */
proc glmselect data=cars_no_na plots=all seed=777;
	partition fraction(test=.1 validate=.1);
	class Make Model Engine_Fuel_Type Engine_Cylinders Transmission_Type 
		Driven_Wheels Driven_Wheels Market_Category Vehicle_Size Vehicle_Style 
		Number_Of_Doors BeforeAfter2000;
	model MSRP=Driven_Wheels Engine_Cylinders Engine_Fuel_Type Engine_HP Make 
		Market_Category Model Number_of_Doors Popularity Transmission_Type 
		Vehicle_Size Vehicle_Style Year city_mpg combined_mpg highway_MPG / 
		selection=forward(stop=cv) cvmethod=random(5) stats=adjrsq cvdetails;
run;

/* backward selection */
proc glmselect data=cars_no_na plots=all seed=777;
	partition fraction(test=.1 validate=.1);
	class Make Model Engine_Fuel_Type Engine_Cylinders Transmission_Type 
		Driven_Wheels Driven_Wheels Market_Category Vehicle_Size Vehicle_Style 
		Number_Of_Doors BeforeAfter2000;
	model MSRP=Driven_Wheels Engine_Cylinders Engine_Fuel_Type Engine_HP Make 
		Market_Category Model Number_of_Doors Popularity Transmission_Type 
		Vehicle_Size Vehicle_Style Year city_mpg combined_mpg highway_MPG / 
		selection=backward(stop=cv) cvmethod=random(5) stats=adjrsq;
run;

/* stepwise selection */
proc glmselect data=cars_no_na plots=all seed=777;
	partition fraction(test=.1 validate=.1);
	class Make Model Engine_Fuel_Type Engine_Cylinders Transmission_Type 
		Driven_Wheels Driven_Wheels Market_Category Vehicle_Size Vehicle_Style 
		Number_Of_Doors BeforeAfter2000;
	model MSRP=Driven_Wheels Engine_Cylinders Engine_Fuel_Type Engine_HP Make 
		Market_Category Model Number_of_Doors Popularity Transmission_Type 
		Vehicle_Size Vehicle_Style Year city_mpg combined_mpg highway_MPG / 
		selection=stepwise(stop=cv) cvmethod=random(5) stats=adjrsq;
run;

/* LASSO selection */
proc glmselect data=cars_no_na plots=all seed=777;
	partition fraction(test=.1 validate=.1);
	class Make Model Engine_Fuel_Type Engine_Cylinders Transmission_Type 
		Driven_Wheels Driven_Wheels Market_Category Vehicle_Size Vehicle_Style 
		Number_Of_Doors BeforeAfter2000;
	model MSRP=Driven_Wheels Engine_Cylinders Engine_Fuel_Type Engine_HP Make 
		Market_Category Model Number_of_Doors Popularity Transmission_Type 
		Vehicle_Size Vehicle_Style Year city_mpg combined_mpg highway_MPG 
		BeforeAfter2000 / selection=LASSO(stop=cv) cvmethod=random(5) stats=adjrsq;
run;

/* elastic net selection */
proc glmselect data=cars_no_na plots=all seed=777;
	partition fraction(test=.1 validate=.1);
	class Make Model Engine_Fuel_Type Engine_Cylinders Transmission_Type 
		Driven_Wheels Driven_Wheels Market_Category Vehicle_Size Vehicle_Style 
		Number_Of_Doors BeforeAfter2000;
	model MSRP=Driven_Wheels Engine_Cylinders Engine_Fuel_Type Engine_HP Make 
		Market_Category Model Number_of_Doors Popularity Transmission_Type 
		Vehicle_Size Vehicle_Style Year city_mpg combined_mpg highway_MPG 
		BeforeAfter2000 / selection=elasticnet(stop=cv) cvmethod=random(5) 
		stats=adjrsq;
run;

/* import file with added variable */
proc import datafile="/home/u45009217/SMU/DS6372/Data/beforeAfter2000.csv" 
		dbms=dlm out=idk replace;
	delimiter=',';
	getnames=yes;
run;

/* Create distinct test, training, validation sets */
data idk;
	set idk;
	n=ranuni(100);
run;

proc sort data=idk;
	by n;

data training testing validation;
	set idk nobs=nobs;

	if _n_<=.8*nobs then
		output training;
	else if _n_<=.9*nobs then
		output testing;
	else
		output validation;
run;

/* test to see if it worked */
proc print data=validation;
run;

/* create new scaled MSRP variable */
data cars_no_na2;
	set idk;
	MSRP2=MSRP/10000;
run;

/* stepwise selection */
proc glmselect data=cars_no_na2 plots=all seed=777;
	partition fraction(test=.1 validate=.1);
	class Make Engine_Fuel_Type Engine_Cylinders Market_Category Beforeafter2000;
	model MSRP2=Market_Category Make Engine_HP Engine_Cylinders BeforeAfter2000 
		Engine_Fuel_Type Engine_HP*Engine_Cylinders Engine_HP*Market_Category / 
		stats=adjrsq cvdetails;
run;

/* Stepwise Selection */
proc glmselect data=cars_no_na2 plots=all seed=777;
	partition fraction(test=.1 validate=.1);
	class Make Engine_Fuel_Type Engine_Cylinders Market_Category beforeafter2000;
	model MSRP2=Market_Category Make Engine_HP Engine_Cylinders BeforeAfter2000 
		Engine_Fuel_Type / stats=adjrsq cvdetails;
run;
