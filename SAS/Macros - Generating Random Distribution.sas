*Macro to generate a distribution of random numbers;

%MACRO randgen(type , param1 , param2 );
	%IF &type = EXPONENTIAL | &type = POISSON %THEN %DO;
		DATA &type;
		call streaminit(3456);
			do i = 1 to 1000;
				x = rand("&type", &param1);
				output;
			end;
		RUN;
		PROC SGPLOT DATA = &type;
		  TITLE "&type PLOT";
		  HISTOGRAM x/ transparency=0.75 fillattrs=(color=blue);
		  density x / type=normal lineattrs=(color=red) legendlabel='&type';
		RUN ;
	%END;
   %ELSE %DO;
	DATA &type;
	call streaminit(3456);
		do i = 1 to 1000;
			x = rand("&type", &param1, &param2);
			output;
		end;
	RUN;
		PROC SGPLOT DATA = &type;
		  TITLE "&type PLOT";
		  HISTOGRAM x/ transparency=0.75 fillattrs=(color=blue);
		  density x / type=normal lineattrs=(color=red) legendlabel='&type';
		RUN ;
	%END;
%MEND randgen;

%randgen(NORMAL,90,8)
%randgen(EXPONENTIAL,8)
%randgen(BINOMIAL,0.2,200) 
%randgen(POISSON,8)
%randgen(GAMMA,3,8)

