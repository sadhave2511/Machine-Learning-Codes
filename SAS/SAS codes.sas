
*separating new and existing customers;

 *new customers;
 proc sql;
 create table new_cust as 
 select HH_Id, count(*) as cnt from Final_table
 group by HH_ID
 having count(*) = 1;
 run;

 proc sql;
 create table single_trans as 
 select 
	a.HH_ID, 
	case when Gift_Cert_Ind = "Y" then 1 else 0 end as Gift_redeemed,
	case when Copn_Redem_Ind = "Y" then 1 else 0 end as Coup_redeemed
 from Final_table a inner join New_cust b 
 on a.HH_ID = b.HH_ID;
 run;

proc freq data = single_trans;
tables Gift_redeemed*Coup_redeemed;
run;
*only 55 out of 7178 have made transactions with coupons and gift certificates;

*repeat customers;
 proc sql;
 create table rpt_cust as 
 select HH_Id, count(*) as cnt from Final_table
 group by HH_ID
 having count(*) > 1;
 run;

 proc sql;
 create table repeat_trans as 
 select 
	a.HH_ID, 
	case when Gift_Cert_Ind = "Y" then 1 else 0 end as Gift_redeemed,
	case when Copn_Redem_Ind = "Y" then 1 else 0 end as Coup_redeemed
 from Final_table a inner join rpt_cust b 
 on a.HH_ID = b.HH_ID;
 run;

proc freq data = repeat_trans;
tables Gift_redeemed*Coup_redeemed;
run;
*only 28 out of 6863 have made transactions with coupons and gift certificates;

*engagement segment of customers;
proc sql;
create table max_order_dt as 
select HH_ID, max(order_date) as max_order_dt from (select * from final_table where order_date <= '31MAR2009'D)
group by HH_Id;
run;

data max_order_dt;
set max_order_dt;
format max_order_dt mmddyy10.;
run;

proc sql;
create table seg2 as
select HH_ID,count(*) as cnt
from final_table
group by HH_ID;
run;

proc freq data = seg2;
tables cnt;
run;



proc sql;
create table eng_seg_2008 as 
select a.HH_Id,
	case when max_order_dt > '31MAR2008'D then 'Active'
		when max_order_dt between '31MAR2008'D and '31MAR2007'D then 'Churn'
		when cnt = 1 then 'Single'
		when max_order_dt <= '31MAR2007'D then 'Inactive'
	else 'Inactive'
	end as Eng_Seg
from max_order_dt a left join seg2 b 
on 
	a.HH_ID = b.HH_ID
;
run;

proc freq data = eng_seg_2008;
tables Eng_seg;
run;

*Which region has most shipping cost;
proc means data = final_table;
class Region;
var Ship_Handl;
run;

proc ANOVA data=final_table;
	class Region;
	model Ship_handl = Region;
	means Region / SNK aplha = 0.05;
	run;
* Midwest has significantly differnt means compared to the others;

*gifting with region;
proc freq data = Final_table;
tables Gift_Ind*Region/ chisq;
run;

*do customers gift low value products;
proc ttest data= final_table sides = 2 alpha =0.05 h0=0;
class Gift_Ind;
var Gross_Pdt_Rev;
run;

*yes. gift items mean is only $50. non gift items are $57;

*does shopping amout vary with predominant age group?;
proc import datafile = 'C:\Education\Courses\predictive using sas\assignment\zipwithage.csv' out = zipwithage dbms = csv replace;
getnames = yes;
run;

proc sql; 
create table datawithage as 
	select
		a.Gross_Pdt_Rev, b.dominant_age
	from 
		Final_table a 
	inner join 
		Zipwithage b 
	on 
		a.zip = b.zip;
run;

 proc ANOVA data=datawithage;
	class dominant_age;
	model Gross_Pdt_Rev = dominant_age;
	means dominant_age / SNK alpha = 0.05;
	run;

*no significant difference;

*More sales in high income region??;
proc import datafile = 'C:\Education\Courses\predictive using sas\assignment\zipwihtincome.csv'
	out = zipwithincome dbms = csv replace;
getnames = yes;
run;


proc sql; 
create table datawithinc as 
	select
		a.Gross_Pdt_Rev, b.Segment_of_max_income
	from 
		Final_table a 
	inner join 
		Zipwithincome b 
	on 
		a.zip = b.zip;
run;


 proc ANOVA data=datawithinc;
	class Segment_of_max_income;
	model Gross_Pdt_Rev = Segment_of_max_income;
	means Segment_of_max_income / SNK alpha = 0.05;
	run;

 proc ANOVA data=datawithinc;
	class Segment_of_max_income;
	model Gross_Pdt_Rev = Segment_of_max_income;
	means Segment_of_max_income / tukey;
	run;

	proc means data = datawithinc;
	class Segment_of_max_income;
	var Gross_Pdt_Rev ;
	run;

	*yes. more sales in high income region;

/*Logistic regresstion **************************************************/
*running a regression to find characteristics of a customer with repeat transaction;

 proc sql;
 create table rpt_ind as 
 select HH_Id, min(order_date) as frst_order, min(Order_Number) as Order_no, count(*) as cnt from Final_table
 group by HH_ID;
 select count(*), count(distinct HH_ID) from rpt_ind;
 run;


proc sql;
 create table reg as 
 select a.*,b.cnt from Final_table a
 inner join rpt_ind b on a.order_date = b.frst_order
 and a.HH_ID = b.HH_ID and a.Order_Number = b.Order_no;
 run;
 select count(*), count(distinct HH_ID) from reg;
 select * from reg where Hh_ID in (Select HH_ID from (select Hh_ID, count(*) as cnt from reg group by HH_ID having cnt >=2));
 run;

proc freq data = reg;
tables subscr_ind;
run;

data reg_data;
set reg;
month = month(order_date);
year = year(order_date);
drop zip HH_ID refund_date refund_status refund_type refund_reason Order_Number Order_date subscr_ind subscr_qty Offer_code pay_status Inv_ID Order_Rank Refunddate Refund_amt return_amt place state stateabb county; 
if cnt = 1 then cnt = 0; else cnt = 1;
if refund_status = ' ' then refund = 0; else refund = 1;
if gift_cert_ind = ' ' then gift_cert_ind = 'N';
if copn_redem_ind = ' ' then copn_redem_ind = 'N';
if zip in (831, 966, 951) then region = 'West';
if Addn_chg_code = ' ' then Addn_chg_code = 'NA';
if zip > 0;
run;

*splitting to train and test data;
proc surveyselect data = reg_data method= srs seed=43543 outall
  samprate=0.75 out=subsets;

data training;
   set subsets;
   if Selected=1;
/*
Proc export data= training
   outfile= "C:\Education\Courses\predictive using sas\assignment\orderdata - Minimal Working Example\training.csv"
   dbms=csv 
   replace;
 run;
*/  
  
data test;
   set subsets;
   if Selected=0;

*running regression ;
proc logistic data= training descending;
  class cat_itm_ind
  	Offer_type 
	Offer_drop
	Gift_cert_ind
	copn_redem_ind Gift_ind pay_type pay_cat Order_type anc_ind Addn_chg_code web_itm_ind div_code region month year refund / param=ref ;
  model cnt = 
Cat_Itm_Ind	Cat_Itm_Qty	Num_Recip_Qty	Gross_Pdt_Rev	Ship_Handl	Sales_Tax	Cancel_Amt	Returned_Amt	Receivable	Offer_Type	Offer_Drop	Gift_Cert_Ind	Gift_Cert_Amt	Copn_Redem_Ind	Copn_Redem_Amt	Gift_Ind	Pay_Type	Pay_Cat	Order_Type	Anc_Ind	Anc_Qty	Addn_Chg	Addn_Chg_Code	Web_Itm_Ind	Web_Itm_Qty	Write_Off	Div_Code	net_refund_amt	Region	month	year	refund
/ rsq 
;
run;

*r-square is 0.07 - Not enough explanatory variables;


*comparing web and catalog sales;
data hypotest (keep = Order_category Gross_Pdt_Rev);
set Final_table;
Order_category = 'Cat';
if Web_Itm_Ind = 'Y' OR Cat_Itm_Ind = 'Y';
if Web_Itm_Ind = 'Y' then Order_category = 'Web';
run;


proc ttest data = hypotest sides = 2 alpha = 0.05 h0 = 0;
class Order_category;
var Gross_Pdt_Rev;
run;

*comparing web and catalog order qty;
data hypotest2 (keep = Order_category Qty);
set Final_table;
if Web_Itm_Ind = 'Y' OR Cat_Itm_Ind = 'Y';
if Web_Itm_Ind = 'Y' then Order_category = 'Web'; else Order_category = 'Cat';
if Web_Itm_Ind = 'Y' then Qty = Web_Itm_Qty; else Qty = cat_itm_qty;
run;


proc ttest data = hypotest2 sides = 2 alpha = 0.05 h0 = 0;
class Order_category;
var Qty;
run;


*comparing web and catalog order expensive or not;
data hypotest2 (keep = Order_category Qty);
set Final_table;
if Web_Itm_Ind = 'Y' OR Cat_Itm_Ind = 'Y';
if Web_Itm_Ind = 'Y' then Order_category = 'Web'; else Order_category = 'Cat';
if Web_Itm_Ind = 'Y' then Qty = Web_Itm_Qty; else Qty = cat_itm_qty;
run;


proc ttest data = hypotest2 sides = 2 alpha = 0.05 h0 = 0;
class Order_category;
var Qty;
run;

*comparing web and catalog order expensive or not;
data hypotest3 (keep = Order_category expense);
set Final_table;
if Web_Itm_Ind = 'Y' OR Cat_Itm_Ind = 'Y';
if Web_Itm_Ind = 'Y' then Order_category = 'Web'; else Order_category = 'Cat';
if Web_Itm_Ind = 'Y' then Qty = Web_Itm_Qty; else Qty = cat_itm_qty;
expense = gross_pdt_rev/qty;
run;


proc ttest data = hypotest3 sides = 2 alpha = 0.05 h0 = 0;
class Order_category;
var expense;
run;
