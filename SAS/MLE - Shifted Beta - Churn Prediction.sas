data Survival;
input year Customers;
datalines;
0	1000
1	869
2	743
3	653
4	593
5	551
6	517
7	491
;
run;

*customers lost;
data survived;
set Survival;
Lost = -dif(Customers);
if year >= 1 then output;
run;


*MLE - Shifted Beta 
PROC NLMIXED DATA=Survived;
  RETAIN remaining;
  PARMS alpha=0.1 beta = 0.1;
  IF Year <= 7 THEN 
    DO;
prob = (beta(alpha+1,beta+year-1))/(beta(alpha,beta));
      ll = lost*log(prob);
      remaining = customers;
	IF Year = 7 THEN DO ;
prob2 = beta(alpha, beta+year)/beta(alpha, beta);
	ll4 = remaining*log(prob2);
	ll = ll+ll4;
	END;
    END;
  MODEL lost ~ general(ll);
RUN;

DATA simulated (drop=x y);
  CALL streaminit(345); 
  DO i=1 to 1000;	/* simulate 1000 customers */
  y = rand('BETA', 0.668, 3.806);	
    DO t=1 to 12;	/* t represents period */
      x=rand('BERNOULLI',y);	
      IF x=1 THEN leave;
    END;
    OUTPUT;
  END;
RUN;

/* Aggregate Simulated Data */

PROC sql;  /* count how many customers leave in each period */
create table sumtable as
  select t as period, count(i) as lost from simulated
    group by t;
QUIT;
DATA sumtable2;
  set sumtable;
  retain remain 1000;
  remain = remain - lost;
RUN;


/* Graph Population-level Survival */

PROC sgplot data=sumtable2;
  scatter x = period y = remain;
RUN;
