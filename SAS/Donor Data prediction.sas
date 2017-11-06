libname asd "C:\Education\Courses\predictive using sas\project";

proc sql;
create table asd.tab as 
select a.donor_id, a.appeal_id, a.appeal_date, a.appeal_cost, 
b.gift_amount, b.gift_date, b.zipcode, b.first_gift_date
from asd.appeals a 
full join asd.donations b 
on a.donor_id = b.donor_id and a.appeal_id = b.appeal_id
;
quit;

proc sql;
select count(*), count(distinct donor_id), count(distinct appeal_id)from asd.Appeals;
select count(*), count(distinct donor_id), count(distinct appeal_id)from asd.Donations;
select count(*),  count(distinct donor_id), count(distinct appeal_id)from asd.Tab;
quit;

******************** QUESTION 1 ******************************** ;
data asd.tab2;
set asd.tab;
gift_ind = 1;
if gift_amount = . then gift_ind = 0;
year = year(appeal_date);
appeal_mon = month(appeal_date);
gift_mon = month(gift_date);
gift_year = year(gift_date);
row_num = _n_;
run;

proc sql;
create table appeal1 as 
select donor_id, sum(gift_ind) as gifted_y, count(appeal_id) as appeal from asd.tab2
group by donor_id

;quit;
*create table appeal2 as  /*where donor_id is not null
*select donor_id , case when gift_ind =1 then cnt as gifted_y,
*case when gift_ind = 0 then cnt as gifted_n;
/*proc sql;
select * from asd.tab where donor_id = 8798746;
quit;
*/
proc corr data = appeal1 output = corr_appeal1;
var gifted_y appeal;
run;

proc sgplot data = appeal1;
scatter x = appeal y = gifted_y;
run;

******************** QUESTION 2 b ******************************** ;

proc sql;
create table appeal2a as 
select appeal_mon,sum(gift_ind) as gifted_y, count(appeal_id) as gifted_n from asd.tab2
where donor_id is not null
group by appeal_mon
;
quit;

proc export 
  data=appeal2a
  dbms=xlsx 
  outfile="C:\Education\Courses\predictive using sas\project\appeal2a.xlsx" 
  replace;
run;
 
proc sql;
create table appeal2c as 
select appeal_mon, donor_id, sum(gift_ind) as gifted_y, count(appeal_id) as appeal_y from asd.tab2
where donor_id is not null
group by appeal_mon, donor_id
;
quit;

data appeal2d;
set appeal2c;
ratio = gifted_y/appeal_y;
run;

proc ANOVA data=appeal2d;
	class appeal_mon;
	model ratio = appeal_mon;
	means appeal_mon / SNK aplha = 0.05;
	run;

proc ANOVA data=appeal2d  ;
	class appeal_mon;
	model ratio = appeal_mon;
	means appeal_mon / tukey;
	run;

	*to plot it;
PROC SGPLOT DATA = Weather;
 SERIES X = Month Y = BRain;
 SERIES X = Month Y = VRain;
 SERIES X = Month Y = LRain;
 TITLE 'Average Monthly Rainfall in Olympic Cities';
RUN; 

******************** QUESTION 2a ******************************** ;

proc sql;
create table appeal2e as 
select donor_id, avg(appeal_cost) as avg_app_cost, sum(gift_ind) as gifted_y, count(appeal_id) as appeal_y from asd.tab2
where donor_id is not null
group by donor_id
;
quit;

data appeal2f;
set appeal2e;
ratio = gifted_y/appeal_y;
run;

proc reg data = appeal2f;
model ratio = avg_app_cost;
run;

proc corr data = appeal2f;
var ratio avg_app_cost;
run;

proc sgplot data = appeal2f;
scatter x = avg_app_cost y = ratio;
run;


****** Recency ***********;
*-	Is past donation amount of a donor a good predictor of the future donation amount by that donor?;
data reg;
set asd.tab2;
keep donor_id gift_amount gift_date first_gift_date row_num appeal_date;
if donor_id ~= . & gift_amount ~= .;
run;

proc sort data = reg;
by donor_id gift_date appeal_date;
run;

Data reg2 ;
	Set reg;
	By donor_id; /* Table has to be sorted on the by-group variable - id in this case - for this to work correctly */
		If first.donor_id then  output;/* If first observation for this id, then reset counter and total */
run;

proc sort data = reg;
by row_num;
run;

data reg3;
merge reg (in = x) reg2 (in = y);
by row_num ;
if x = 1 and y = 0;
run;

Data reg4 ;
	Set reg3;
	By donor_id; /* Table has to be sorted on the by-group variable - id in this case - for this to work correctly */
		If first.donor_id then  output;/* If first observation for this id, then reset counter and total */
run;

proc sql;
create table reg5 as 
select a.donor_id, a.gift_amount as last_gift_amt, b.gift_amount as pre_gift_amt
from reg2 a left join reg4 b on a.donor_id = b.donor_id;
quit;

/*proc reg data=reg5;
  model last_gift_amt = pre_gift_amt;
run;

proc corr data = reg5;
var last_gift_amt pre_gift_amt;
run;

proc sgplot data = reg5;
scatter x = pre_gift_amt y = last_gift_amt;
run;
*/
proc sort data = reg3;
by row_num;
run;

proc sort data = reg4;
by row_num;
run;

* to see if last gift amount is influenced by all previous periods;
data reg6;
merge reg3 (in = x) reg4 (in = y);
by row_num ;
if x = 1 and y = 0;
run;

proc sort data = reg6;
by donor_id gift_date appeal_date;
run;

*use this for join;
Data reg7 ;
	Set reg6;
	By donor_id; /* Table has to be sorted on the by-group variable - id in this case - for this to work correctly */
		If first.donor_id then  output;/* If first observation for this id, then reset counter and total */
run;

proc sort data = reg6;
by row_num;
run;

proc sort data = reg7;
by row_num;
run;

data reg8;
merge reg6 (in = x) reg7 (in = y);
by row_num ;
if x = 1 and y = 0;
run;


proc sort data = reg8;
by donor_id gift_date appeal_date;
run;

*use this for join;
Data reg9 ;
	Set reg8;
	By donor_id; /* Table has to be sorted on the by-group variable - id in this case - for this to work correctly */
		If first.donor_id then  output;/* If first observation for this id, then reset counter and total */
run;

proc sort data = reg8;
by row_num;
run;

proc sort data = reg9;
by row_num;
run;

data reg10;
merge reg8 (in = x) reg9 (in = y);
by row_num ;
if x = 1 and y = 0;
run;


proc sort data = reg10;
by donor_id gift_date appeal_date;
run;

*use this for join;
Data reg11 ;
	Set reg10;
	By donor_id; /* Table has to be sorted on the by-group variable - id in this case - for this to work correctly */
		If first.donor_id then  output;/* If first observation for this id, then reset counter and total */
run;


proc sort data = reg10;
by row_num;
run;

proc sort data = reg11;
by row_num;
run;

data reg12;
merge reg10 (in = x) reg11 (in = y);
by row_num ;
if x = 1 and y = 0;
run;


proc sort data = reg12;
by donor_id gift_date appeal_date;
run;

*use this for join;
Data reg13 ;
	Set reg12;
	By donor_id; /* Table has to be sorted on the by-group variable - id in this case - for this to work correctly */
		If first.donor_id then  output;/* If first observation for this id, then reset counter and total */
run;


proc sort data = reg12;
by row_num;
run;

proc sort data = reg13;
by row_num;
run;

data reg14;
merge reg12 (in = x) reg13 (in = y);
by row_num ;
if x = 1 and y = 0;
run;

proc sort data = reg14;
by donor_id gift_date appeal_date;
run;

*use this for join;
Data reg15 ;
	Set reg14;
	By donor_id; /* Table has to be sorted on the by-group variable - id in this case - for this to work correctly */
		If first.donor_id then  output;/* If first observation for this id, then reset counter and total */
run;

proc sql;
create table asd.past_reg as 
select 
	a.donor_id, 
	a.gift_amount as donation_t, 
	b.gift_amount as donation_t_1,
	c.gift_amount as donation_t_2,
	d.gift_amount as donation_t_3,
	e.gift_amount as donation_t_4,
	f.gift_amount as donation_t_5,
	g.gift_amount as donation_t_6
from reg2 a 
left join reg4 b on a.donor_id = b.donor_id
left join reg7 c on a.donor_id = c.donor_id
left join reg9 d on a.donor_id = d.donor_id
left join reg11 e on a.donor_id = e.donor_id
left join reg13 f on a.donor_id = f.donor_id
left join reg15 g on a.donor_id = g.donor_id
;
quit;

proc reg data=asd.past_reg;
  model donation_t = donation_t_1 donation_t_2;
run;

proc corr data = asd.past_reg;
run;

/*
proc sql;
create table reg2 as
select donor_id, max(gift_date) as latest from Reg
group by donor_id;
create table reg3 as 
select a.donor_id, a.latest, b.gift_amount, b.row_num, b.appeal_date
from Reg2 a inner join Reg b on a.donor_id = b.donor_id and a.latest = b.gift_date;
create table reg4 as
select donor_id, min(appeal_date) as appeal from reg3
group by donor_id;
create table reg5 as 
select a.*
from reg3 a inner join reg4 b on a.donor_id = b.donor_id and a.appeal_date = b.appeal;
quit;
*/
proc sql;
*select count(*), count(distinct donor_id) from asd.tab2;
*select count(*), count(distinct donor_id) from reg;
*select count(*), count(distinct donor_id) from reg2;
*select count(*), count(distinct donor_id) from reg3;
*select count(*), count(distinct donor_id) from reg4;
*select count(*), count(distinct donor_id) from reg5;
*select count(*), count(distinct donor_id) from reg6;
*select count(*), count(distinct donor_id) from reg7;
*select count(*), count(distinct donor_id) from reg8;
*select count(*), count(distinct donor_id) from reg9;
select count(*), count(distinct donor_id) from reg10;
select count(*), count(distinct donor_id) from reg11;
select count(*), count(distinct donor_id) from reg12;
*select count(*), count(distinct donor_id) from reg13;
*select * from asd.tab2 where gift_date = 16863 and donor_id = 8203945;
*select * from asd.tab2 where gift_date = 17049 and donor_id = 9060476;
*select * from reg5 where donor_id in (select donor_id from (select donor_id, count(*) as cnt from reg5 group by donor_id having cnt > 1) );
quit;

proc sql;
create table reg4 as 
select a.*, max(gift_date) as second_latest
from reg3 a left join reg b on a.donor_id = b.donor_id and a.latest > b.gift_date
group by donor_id, latest, gift_amount;
quit;











