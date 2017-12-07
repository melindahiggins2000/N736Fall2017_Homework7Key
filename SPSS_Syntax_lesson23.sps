* Encoding: UTF-8.
* ======================================.
* N736 - LESSON 23 - Repeated Measures ANOVA
* with a between group variable - we'll use treat
*
* Melinda Higgins
* dated Nov 15, 2017
*
* We're working with the HELP dataset
* we'll focus on the 5 PCS measurements over time
* and we'll include the treat variable for treatment group
* to see if there are differences between the 2 groups
* over time. This is sometimes called Mixed ANOVA
* because you have both a between and a within group variable.
* ======================================.

* Look at distribution of the 5 PCS measurements
* LISTWISE deletion is used here since
* that is what is assumed in the repeated measures ANOVA procedue.

EXAMINE VARIABLES=pcs pcs1 pcs2 pcs3 pcs4 BY treat
  /PLOT BOXPLOT HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

* RM-ANOVA
* uses listwise deletion
* TIME main WITHIN effect
* TREAT is the main BETWEEN effect.

* NOTE: The 2 lines of code
*  /EMMEANS=TABLES(treat*time) COMPARE(treat) ADJ(SIDAK)
*  /EMMEANS=TABLES(treat*time) COMPARE(time) ADJ(SIDAK)
* were added in manually - these have to be typed in and modified
* from the PASTE command from the GUI menus in SPSS.

GLM pcs pcs1 pcs2 pcs3 pcs4 BY treat
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

* error-bar plot of pcs at 5 time points
* listwise deletion used as in RM-ANOVA
* the PANEL option is used to see the differences by group.

GRAPH
  /ERRORBAR(CI 95)=pcs pcs1 pcs2 pcs3 pcs4
  /PANEL COLVAR=treat COLOP=CROSS
  /MISSING=LISTWISE.

* ==========================================
* RESTRUCTURE DATA
*
* for this next set of code, we need to restructure the data
* from a wide format where pcs,pcs1,pcs2,pcs3,pcs4 are all
* in different columns, to instead "stack" these 5 variables
* on top of one another - so each row is a different time point
* ==========================================

VARSTOCASES
  /MAKE pcs FROM pcs pcs1 pcs2 pcs3 pcs4
  /INDEX=Time "Time Points"(5) 
  /KEEP=id treat age female pss_fr racegrp homeless a15a a15b d1 e2b g1b i1 i2 mcs f1a f1b f1c f1d 
    f1e f1f f1g f1h f1i f1j f1k f1l f1m f1n f1o f1p f1q f1r f1s f1t cesd indtot drugrisk sexrisk 
    satreat substance drinkstatus daysdrink anysubstatus daysanysub linkstatus dayslink e2b1 g1b1 i11 
    mcs1 cesd1 indtot1 drugrisk1 sexrisk1 pcrec1 e2b2 g1b2 i12 mcs2 cesd2 indtot2 drugrisk2 sexrisk2 
    pcrec2 e2b3 g1b3 i13 mcs3 cesd3 indtot3 drugrisk3 sexrisk3 pcrec3 e2b4 g1b4 i14 mcs4 cesd4 indtot4 
    drugrisk4 sexrisk4 pcrec4 
  /NULL=KEEP
  /COUNT=countCases.

* the INDEX created starts at 1 by default
* we need to change this to start at 0.

COMPUTE time0=Time-1. 
EXECUTE.

* save the long format as another filename.

SAVE OUTFILE='C:\MyGithub\N736Fall2017_lesson23\helpmkh_pcslong.sav'
  /COMPRESSED.

* if we had complete data on all 453 subjects at all 5 times points
* we should have 2265 data points.
* this new LONG format file does have 2265 rows which is correct
* However, there are NOT 2265 values for the pcs.
* here are the summary stats over all 5 time points for 
* everyone who had PCS data.

FREQUENCIES VARIABLES=pcs
  /NTILES=4
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /HISTOGRAM
  /ORDER=ANALYSIS.

* there are 1424 PCS data points and 841 missing out of the 2265 possible 
* 1424/2265 = 62.9% have data, but 841/2265 = 37.1% is missing.

* This is still much better than only have 98/453 = 21.6% who
* had complete data at all 5 time points.
* to maximize the use of ALL available data, we are better
* off using MLM - multilevel (Mixed) linear modeling

* Run the MIXED procedure for the 5 time points.
* this has a random intercept by subject (ID)
* This model treats TIME as a factor.

MIXED pcs BY time0
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=time0 | SSTYPE(3)
  /METHOD=REML
  /PRINT=CPS CORB COVB DESCRIPTIVES G  LMATRIX R SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(VC)
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(time0) COMPARE ADJ(SIDAK).

* we can also run TIME as a continuous covariate
* like doing regression - assumes linear affect of time.

MIXED pcs WITH time0
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=time0 | SSTYPE(3)
  /METHOD=REML
  /PRINT=CPS CORB COVB DESCRIPTIVES G  LMATRIX R SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(VC)
  /EMMEANS=TABLES(OVERALL).

* select a few of the first cases IDs.

USE ALL.
COMPUTE filter_$=(id < 11).
VARIABLE LABELS filter_$ 'id < 11 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

* plot these PCS time profiles for these few subjects.

GRAPH
  /SCATTERPLOT(BIVAR)=time0 WITH pcs BY id
  /MISSING=LISTWISE.

FILTER OFF.
USE ALL.
EXECUTE.

* the resulting plots were edited in the OUTPUT window
* and interpolation lines and "fitted" lines were overlaid
* you can view these plots to get an idea of the variability
* of the time trends for PCS between the different subjects
* and you can see the intermittent missing data

* run MLM for PCS by treatment group
* treat time as a factor
* again the post hoc tests for the interaction
* effect - edited manually like above.

MIXED pcs BY treat time0
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=treat time0 treat*time0 | SSTYPE(3)
  /METHOD=REML
  /PRINT=CPS CORB COVB DESCRIPTIVES G  LMATRIX R SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(VC)
  /EMMEANS=TABLES(OVERALL)
  /EMMEANS=TABLES(treat) COMPARE ADJ(SIDAK)
  /EMMEANS=TABLES(time0) COMPARE ADJ(SIDAK)
  /EMMEANS=TABLES(treat*time0) COMPARE(treat) ADJ(SIDAK)
  /EMMEANS=TABLES(treat*time0) COMPARE(time0) ADJ(SIDAK).

* there is ALOT more to learn with MLM
* the examples above only had a random intercept
* we could also have random slopes
* and we could change the variance structure
* from "variance components" to many other options
* for covariance structures - see HELP.

* treat fixed, time continuous.

MIXED pcs BY treat WITH time0
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=treat time0 treat*time0 | SSTYPE(3)
  /METHOD=REML
  /PRINT=CPS CORB COVB DESCRIPTIVES G  LMATRIX R SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(UN)
  /EMMEANS=TABLES(treat) COMPARE ADJ(SIDAK)
  /EMMEANS=TABLES(OVERALL).

* for the interaction - flip the treat group coding
* and recheck the p-values.

COMPUTE treat_flip=treat=0.
EXECUTE.

MIXED pcs BY treat_flip WITH time0
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=treat_flip time0 treat_flip*time0 | SSTYPE(3)
  /METHOD=REML
  /PRINT=CPS CORB COVB DESCRIPTIVES G  LMATRIX R SOLUTION TESTCOV
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(UN)
  /EMMEANS=TABLES(treat_flip) COMPARE ADJ(SIDAK)
  /EMMEANS=TABLES(OVERALL).



