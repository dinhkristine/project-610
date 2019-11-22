
filename house '/folders/myfolders/data/house_data1.csv';

proc import datafile=house
  out=house
  dbms=csv
  replace;
run;

proc print data=house (obs=10);
run;



*proc import datafile = 'c:/Users/kdinh/Desktop/kc_house_data1.csv'
 out = house
 dbms = CSV
 ;
*run;

*ods rtf file='c:/Users/kdinh/Desktop/house.rtf' startpage = never;

proc print data = house (obs = 10); 
var price logprice bathrooms floors view condition grade sqft_living1000;
run; 

proc reg; 
model logprice = sqft_living1000 floors view condition grade / vif; 
run;
quit;

proc reg; 
model logprice = bathrooms sqft_living1000 floors view condition grade / selection = stepwise; 
run;
quit;

proc reg; 
model logprice = bathrooms sqft_living1000 floors view condition grade / selection = cp adjrsq sse; 
run;
quit;


proc corr; 
run; 
