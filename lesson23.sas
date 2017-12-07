
* make a copy to WORK;

data helpmkh;
  set library.helpmkh;
  run;

* Encoding: UTF-8.
* =================================================.
* N736 Lesson 23 - repeated measures ANOVA
* this time we'll also add a BETWEEN group
  variable treat for the treatment group
  this is sometimes called MIXED ANOVA since there
  is both a WITHIN and a BETWEEN group variable
*
* dated 11/14/2017
* Melinda Higgins, PhD
* =================================================.

* =================================================.
* In the HELP dataset there are 5 time points
* baseline and 4 follow-up time points at 6m, 12m, 18m and 24m
*
* for today's lesson we will be working with the PCS
* physical component score for the SF36 quality of life tool
* let's look at how these 5 PCS measurements are
* correlated across time.
* =================================================;

proc corr data=helpmkh;
  var pcs pcs1 pcs2 pcs3 pcs4;
  run;

* =================================================.
* notice that most of these correlations have r>0.4 indicating
* moderate to large correlation across time
* this makes sense since particpants scores probably
* do not change a lot every 6 months and will tend to be 
* similar to each other WITHIN each particpant
* more so than pcs scores BETWEEN participants
* =================================================;

* repeated measures ANOVA
  add plots=all to get basically
  the proc univariate plots for each
  pcs at each time point
  the printe option gets use the sphericity test
  add the treat group;

* run time as continuous and treat as "factor";

proc glm data=helpmkh;
  class treat;
  model pcs pcs1 pcs2 pcs3 pcs4 = treat / nouni;
  repeated time 5 / mean printe;
  run;

* transpose the data into a long format;
* first sort by ID;

proc sort data=helpmkh out=helpmkh_sortid;
  by id;
  run;

proc transpose data=helpmkh_sortid out=helpmkh_pcslong;
  var pcs pcs1 pcs2 pcs3 pcs4;
  by id treat;
  run;

proc contents data=helpmkh_pcslong; run;

proc freq data=helpmkh_pcslong;
  table _name_;
  run;

data help_pcslong;
  set helpmkh_pcslong;
  rename _label_ = pcslabel;
  rename _name_ = pcstime;
  rename col1 = pcs;
  run;

proc contents data=help_pcslong; run;

data help2;
  set help_pcslong;
  if pcstime='pcs'  then time=0;
  if pcstime='pcs1' then time=1;
  if pcstime='pcs2' then time=2;
  if pcstime='pcs3' then time=3;
  if pcstime='pcs4' then time=4;
  run;

proc contents data=help2; run;

* we can use the LONG format dataset
  to make some error bar plots
  see the links for the example code below;

* see http://support.sas.com/kb/42/515.html;

proc means data=help2 mean std;
  class time;
  var pcs;
  output out=meansout mean=mean stderr=stderr;
run;

data reshape(keep=time pcs mean);                                                                                                      
   set meansout;                                                                                                                        
   pcs=mean;  
   output;                                                                                                                              
                                                                                                                                        
   pcs=mean - stderr;                                                                                                                  
   output;                                                                                                                              
                                                                                                                                        
   pcs=mean + stderr;                                                                                                                  
   output;                                                                                                                              
run;                                                                                                                                    
 
/* Define the title */
   title1 'Plot Means with Standard Error Bars from Calculated Data'; 

/* Define the axis characteristics */
   axis1 offset=(5,5) minor=none;                                                                                                                  
   axis2 label=(angle=90);                                                                                                              

/* Define the symbol characteristics */
   symbol1 interpol=hiloctj color=blue line=2;                                                                                          
   symbol2 interpol=none color=blue value=dot height=1.5;
                                                                                                                                       
/* Plot the error bars using the HILOCTJ interpolation */                                                                               
/* and overlay symbols at the means. */                                                                                                 

proc gplot data=reshape;                                                                                                                
   plot pcs*time mean*time / overlay haxis=axis1 vaxis=axis2;     
run;  

* do the error bar plot again
  but by treat group;

* see http://support.sas.com/kb/50/217.html;

proc sort data=help2 out=help2_sort;
  by treat time;
  run;

proc means data=help2_sort noprint;                                                                                                           
   by treat time;                                                                                                                    
   var pcs;                                                                                                                            
   output out=meansout(drop=_type_ _freq_) mean=mean stderr=stderr;                                                                     
run; 

/* Reshape the data to contain three Y values for */                                                                                    
/* each X for use with the HILOC interpolation.   */                                                                                    
data reshape(keep=treat time pcs mean);                                                                                             
   set meansout;                                                                                                                        
   by treat time;                                                                                                                    
                                                                                                                                        
/* Offset the X values to display two groups */                                                                                         
   if treat=0 then time=time - 0.08;                                                                                               
   else if treat=1 then time=time + 0.08;                                                                                          
                                                                                                                                        
   pcs=mean;                                                                                                                           
   output;                                                                                                                              
                                                                                                                                        
   pcs=mean - stderr;                                                                                                                  
   output;                                                                                                                              
                                                                                                                                        
   pcs=mean + stderr;                                                                                                                  
   output;                                                                                                                              
run;                                                                                                                                    
                                                                                                                                        
/* Define the title */                                                                                                                  
   title1 'Plot Means with Standard Error Bars from Calculated Data for Groups';                                                        
                                                                                                                                        
/* Define the axis characteristics */                                                                                                   
   axis1 offset=(0,0) minor=none value=(t=1 ' ' t=7 ' ');                                                                                                       
   axis2 label=(angle=90) order=(46 to 52 by 1) minor=(n=0.2);                                                                                                                   
                                                                                                                                        
/* Define the symbol characteristics */                                                                                                 
   symbol1 interpol=hiloctj color=vibg line=1;                                                                                          
   symbol2 interpol=hiloctj color=depk line=2;                                                                                          
                                                                                                                                        
   symbol3 interpol=none color=vibg value=dot height=1.5;                                                                               
   symbol4 interpol=none color=depk value=dot height=1.5;                                                                               
                                                                                                                                        
/* Define the legend characteristics */                                                                                                 
   legend1 label=('Group:') frame;                                                                                                      
                                                                                                                                        
/* Plot the error bars using the HILOCTJ interpolation */                                                                               
/* and overlay symbols at the means. */                                                                                                 
proc gplot data=reshape;                                                                                                                
   plot pcs*time=treat / haxis=axis1 vaxis=axis2 legend=legend1;                                                                    
   plot2 mean*time=treat / vaxis=axis2 noaxis nolegend;                                                                              
run;               

* proc mixed - random intercepts
  first set time to continuous
  and treat as a "factor";

* NOTE: read me on ML versus REML
  and read more on type - covariance structures
  which really only apply with random slopes;

* notice that the sample size here is
  much larger n=1424 whereas before we
  had 98*5 = 490 subjects*time - so this
  approach gives us 2.9 times more data;

proc mixed data=help2 method=REML covtest;
  class treat;
  model pcs = treat time treat*time / solution;
  random intercept /  subject=id type=vc;
  lsmeans treat;
  run;
  
* treat both time and treat as "factor"
  run all pair wise comparisons;

proc mixed data=help2 method=REML covtest plots=all;
  class treat time;
  model pcs = treat time treat*time;
  random intercept /  subject=id type=vc;
  lsmeans treat time treat*time / adjust=sidak;
  run;



