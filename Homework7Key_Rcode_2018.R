# ==================================
# N736 Homework 7 - RM-ANOVA and MLM
# ANSWER KEY
#
# updated 12/14/2018
# Melinda Higgins, PhD
# 
# ==================================

# ==================================
# we're be working with the 
# helpmkh dataset
# ==================================

library(tidyverse)
library(haven)

helpdat <- haven::read_spss("helpmkh.sav")

# ============================================.
# For this homework we'll use the helpmkh dataset
#
# In the HELP dataset there are 5 time points
# baseline and 4 follow-up time points 
# at 6m, 12m, 18m and 24m
#
# for Homework 7 we will be working with the MCS
# mental component score for the SF36 quality of life tool
# let's look at how these 5 MCS measurements are
# correlated across time
#
# and we'll also look at the treat group.
# ============================================.

# keep subject id also
h1 <- helpdat %>%
  select(id, treat, mcs, mcs1, mcs2, mcs3, mcs4)

# compute number of missing values
# across the 5 time points of cesd measurements
h1$nmiss_mcs <- rowSums(is.na(h1[,3:7]))
table(h1$nmiss_mcs)

# see http://psych.wisc.edu/moore/Rpdf/610-R11_MixedDesign.pdf
# keep only the 98 cases with complete data
# where nmiss_cesd equals 0 (none missing)
h198 <- h1 %>%
  filter(nmiss_mcs==0)

# to do these analyses in R, we first
# have to reshape the data from WIDE
# to long

h198long <- h198 %>%
  gather(key=item,
         value=value,
         -c(id,treat,nmiss_mcs))

# now you have 98*5 = 490 cases
# add a time variable to long format
h198long <- h198long %>%
  mutate(time=c(rep(0,98),
                rep(1,98),
                rep(2,98),
                rep(3,98),
                rep(4,98)))

# we can use the aov() function but we
# need to first convert these to factors

h198long$id <- factor(h198long$id) # subject id
h198long$treat <- factor(h198long$treat) # group - between factor
h198long$time <- factor(h198long$time) # time - within factor

# rename the variables
names(h198long) <- c("id","treat","nmiss_mcs",
                   "mcsitem","mcsvalue","time")

m1 <- aov(mcsvalue ~ treat*time + Error(id/time), 
          data=h198long) 
summary(m1)

# a plot approach
with(h198long, 
     interaction.plot(time, treat, mcsvalue,
                      type="b", col=c("red","blue"), pch=c(16,18),
                      main="Interaction Plot for Treatment Group and Time"))

# MLM Approach
# use nlme package
library(nlme)

lme1 <- nlme::lme(mcsvalue ~ time*treat,
                  data=h198long,
                  random= ~1 | id,
                  method="REML",
                  na.action=na.omit)
# get summary - model coefficients
# tests coefficients not equal to 0
summary(lme1)

# get anova tables - both
# of these yield type III Sums of Squares
anova.lme(lme1, type="marginal")
car::Anova(lme1, type="III")


# =========================================
# run again with all cases with data for the mcs

# to do these analyses in R, we first
# have to reshape the data from WIDE
# to long

h1long <- h1 %>%
  gather(key=item,
         value=value,
         -c(id,treat,nmiss_mcs))

# now we have 453*5=2265 rows
# add a time variable to long format
h1long <- h1long %>%
  mutate(time=c(rep(0,453),
                rep(1,453),
                rep(2,453),
                rep(3,453),
                rep(4,453)))

# we can use the aov() function but we
# need to first convert these to factors

h1long$id <- factor(h1long$id) # subject id
h1long$treat <- factor(h1long$treat) # group - between factor
h1long$time <- factor(h1long$time) # time - within factor

# rename the variables
names(h1long) <- c("id","treat","nmiss_mcs",
                   "mcsitem","mcsvalue","time")

m2 <- aov(mcsvalue ~ treat*time + Error(id/time), 
          data=h1long) 
summary(m2)

# MLM Approach
# use nlme package
library(nlme)

lme1 <- nlme::lme(mcsvalue ~ time*treat,
                  data=h1long,
                  random= ~1 | id,
                  method="REML",
                  na.action=na.omit)
# get summary - model coefficients
# tests coefficients not equal to 0
summary(lme1)

# get anova tables - both
# of these yield type III Sums of Squares
anova.lme(lme1, type="marginal")
car::Anova(lme1, type="III")

# you can also use the lme4 package
# read more at https://freshbiostats.wordpress.com/2013/07/28/mixed-models-in-r-lme4-nlme-both/
# and just google nlme vs lme4 package

# flip treat and run again
h1long <- h1long %>%
  mutate(treat_flip = as.numeric(treat==0))

lme2 <- nlme::lme(mcsvalue ~ time*treat_flip,
                  data=h1long,
                  random= ~1 | id,
                  method="REML",
                  na.action=na.omit)
# get summary - model coefficients
# tests coefficients not equal to 0
summary(lme2)

# get anova tables - both
# of these yield type III Sums of Squares
anova.lme(lme2, type="marginal")
car::Anova(lme2, type="III")