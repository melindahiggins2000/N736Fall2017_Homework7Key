* Encoding: UTF-8.
* ==================================================
* N736 Homework 07 - Answer Key
* Repeated Measures and Multilevel (MIXED) Linear Models (MLM)
*
* by Melinda Higginsm PhD
* dated 11/23/2017
* ==================================================.

* Look at distribution of the 5 CESD measurements
* notice the changing sample size.

FREQUENCIES VARIABLES=cesd cesd1 cesd2 cesd3 cesd4
  /FORMAT=NOTABLE
  /NTILES=4
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /HISTOGRAM
  /ORDER=ANALYSIS.

* we can also use the explore tool in SPSS
* to apply listwise deletion to view
* complete cases only
* and we'll look at these by treatment group.

EXAMINE VARIABLES=cesd cesd1 cesd2 cesd3 cesd4
  /PLOT BOXPLOT HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

EXAMINE VARIABLES=cesd cesd1 cesd2 cesd3 cesd4 BY treat
  /PLOT BOXPLOT HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

* compute the number of missing measurements
* of CESD over the 5 time points.

COMPUTE nmiss_cesd=nmiss(cesd, cesd1, cesd2, cesd3, cesd4).
EXECUTE.

FREQUENCIES VARIABLES=nmiss_cesd
  /ORDER=ANALYSIS.

* create an indicator variable to compared
* subjects who completed all versus those with
* missing time points.

COMPUTE cesd_nonemissing=nmiss_cesd < 1.
EXECUTE.

* RM-ANOVA
* uses listwise deletion
* TIME main WITHIN effect
* TREAT is the main BETWEEN effect.

* NOTE: The 2 lines of code
*  /EMMEANS=TABLES(treat*time) COMPARE(treat) ADJ(SIDAK)
*  /EMMEANS=TABLES(treat*time) COMPARE(time) ADJ(SIDAK)
* were added in manually - these have to be typed in and modified
* from the PASTE command from the GUI menus in SPSS.

GLM cesd cesd1 cesd2 cesd3 cesd4 BY treat
  /WSFACTOR=time 5 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(time*treat)
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(treat) COMPARE ADJ(SIDAK)
  /EMMEANS=TABLES(time) COMPARE ADJ(SIDAK)
  /EMMEANS=TABLES(treat*time) COMPARE(treat) ADJ(SIDAK)
  /EMMEANS=TABLES(treat*time) COMPARE(time) ADJ(SIDAK)
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=time 
  /DESIGN=treat.

* flip treat effect and run again to check interaction term.

compute treat_flip = treat=0.
execute.

GLM cesd cesd1 cesd2 cesd3 cesd4 BY treat_flip
  /WSFACTOR=time 5 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(time*treat_flip)
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(treat_flip) COMPARE ADJ(SIDAK)
  /EMMEANS=TABLES(time) COMPARE ADJ(SIDAK)
  /EMMEANS=TABLES(treat_flip*time) COMPARE(treat_flip) ADJ(SIDAK)
  /EMMEANS=TABLES(treat_flip*time) COMPARE(time) ADJ(SIDAK)
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=time 
  /DESIGN=treat_flip.

* error-bar plot of CESD at 5 time points
* listwise deletion used as in RM-ANOVA
* the PANEL option is used to see the differences by group.

GRAPH
  /ERRORBAR(CI 95)=cesd cesd1 cesd2 cesd3 cesd4
  /PANEL COLVAR=treat COLOP=CROSS
  /MISSING=LISTWISE.

* ==========================================
* RESTRUCTURE DATA
*
* for this next set of code, we need to restructure the data
* from a wide format where cesd, cesd1, cesd2, cesd3, cesd4 are all
* in different columns, to instead "stack" these 5 variables
* on top of one another - so each row is a different time point
* ==========================================

VARSTOCASES
  /MAKE cesd FROM cesd cesd1 cesd2 cesd3 cesd4
  /INDEX=Index1(5) 
  /KEEP=id treat age female pss_fr racegrp homeless a15a a15b d1 e2b g1b i1 i2 pcs mcs f1a f1b f1c 
    f1d f1e f1f f1g f1h f1i f1j f1k f1l f1m f1n f1o f1p f1q f1r f1s f1t indtot drugrisk sexrisk satreat 
    substance drinkstatus daysdrink anysubstatus daysanysub linkstatus dayslink e2b1 g1b1 i11 pcs1 mcs1 
    indtot1 drugrisk1 sexrisk1 pcrec1 e2b2 g1b2 i12 pcs2 mcs2 indtot2 drugrisk2 sexrisk2 pcrec2 e2b3 
    g1b3 i13 pcs3 mcs3 indtot3 drugrisk3 sexrisk3 pcrec3 e2b4 g1b4 i14 pcs4 mcs4 indtot4 drugrisk4 
    sexrisk4 pcrec4 nmiss_cesd cesd_nonemissing 
  /NULL=KEEP
  /COUNT=countCases.

* save the restructured data.

SAVE OUTFILE='C:\MyGithub\N736Fall2017_Homework7Key_draft\helpmkh_cesdlong.sav'
  /COMPRESSED.

* the INDEX created starts at 1 by default
* we need to change this to start at 0.

COMPUTE time0=Index1-1. 
EXECUTE.

* if we had complete data on all 453 subjects at all 5 times points
* we should have 2265 data points.
* this new LONG format file does have 2265 rows which is correct
* However, there are NOT 2265 values for the pcs.
* here are the summary stats over all 5 time points for 
* everyone who had CESD data.

FREQUENCIES VARIABLES=cesd
  /NTILES=4
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /HISTOGRAM
  /ORDER=ANALYSIS.

* there are 1422 CESD data points and 843 missing out of the 2265 possible 
* 1422/2265 = 62.8% have data, but 843/2265 = 37.2% is missing.

* This is still much better than only have 98/453 = 21.6% who
* had complete data at all 5 time points.
* to maximize the use of ALL available data, we are better
* off using MLM - multilevel (Mixed) linear modeling

* run MLM for PCS by treatment group
* treat time as coninuous
* only the intercept is random
* and VC variance components was used

* treat fixed, time continuous.

MIXED cesd BY treat WITH time0
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=treat time0 treat*time0 | SSTYPE(3)
  /METHOD=REML
  /PRINT=CPS CORB COVB DESCRIPTIVES G  LMATRIX R SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(VC)
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(treat) .

* run with treat_flip.

MIXED cesd BY treat_flip WITH time0
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=treat_flip time0 treat_flip*time0 | SSTYPE(3)
  /METHOD=REML
  /PRINT=CPS CORB COVB DESCRIPTIVES G  LMATRIX R SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(VC)
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(treat_flip) .

* make an error bar plot of cesd over time by group.
* use the long dataset.

GRAPH
  /ERRORBAR(CI 95)=cesd BY time0
  /PANEL COLVAR=treat COLOP=CROSS.
