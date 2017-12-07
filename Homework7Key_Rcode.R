# ==================================
# N736 Homework 7 Answer Key - Repeated Measures ANOVA
# add a BETWEEN group variable treat
# and look at multilevel (mixed) model approach
#
# dated 11/24/2017
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

h1 <- helpdat %>%
  select(id, treat, cesd, cesd1, cesd2, cesd3, cesd4)

# compute number of missing values
# across the 5 time points of cesd measurements
h1$nmiss_cesd <- rowSums(is.na(h1[,3:7]))
table(h1$nmiss_cesd)

# see http://psych.wisc.edu/moore/Rpdf/610-R11_MixedDesign.pdf
# keep only the 98 cases with complete data
# where nmiss_cesd equals 0 (none missing)
h198 <- h1 %>%
  filter(nmiss_cesd==0)

# put into a long format

h198long <- h198 %>%
  gather(key=item,
         value=value,
         -c(id,treat,nmiss_cesd)) 

names(h198long) <- c("id","treat",
                     "nmiss_cesd","cesditem","cesdvalue")

# add a time variable to long format
h198long <- h198long %>%
  mutate(time=c(rep(0,98),
                rep(1,98),
                rep(2,98),
                rep(3,98),
                rep(4,98)))

h198long$id <- factor(h198long$id) # subject id
h198long$treat <- factor(h198long$treat) # group - between factor
h198long$time <- factor(h198long$time) # time - within factor

m2 <- aov(cesdvalue ~ treat*time + Error(id/time), 
          data=h198long) 
summary(m2)

# Graphs - Main effects and interaction graphs
par(mfrow=c(2, 2), cex=0.6, mar=c(4, 4, 4, 2), mex=0.8) 
# sets up for 4 graphs on a page. The next 4 statements ask for main effect means, then the GxT interaction.
plot(h198long$cesdvalue ~ h198long$treat,
     main="HELP Dataset -- Treatment Group Between Subjects Effect", 
     xlab="Treatment Group", ylab="CESD")
plot(h198long$cesdvalue ~ h198long$time,
     main="HELP Dataset -- Time Within Subjects Effect", 
     xlab="Time",ylab="CESD")
plot(h198long$cesdvalue ~ h198long$id,
     main="HELP Dataset -- CESD by Subjects", 
     xlab="Subjects",ylab="CESD")
interaction.plot(h198long$time,h198long$treat,h198long$cesdvalue,
                 main="HELP Dataset - CESD over Time by Treatment Group", 
                 ylab="CESD")

#------------------------------

# use all available data

# restructure into long format

h1long <- h1 %>%
  gather(key=item,
         value=value,
         -c(id,treat),
         na.rm=FALSE)

names(h1long) <- c("id","treat","cesditem","cesdvalue")

# add a time variable to long format
h1long <- h1long %>%
  mutate(time=c(rep(0,453),
                rep(1,453),
                rep(2,453),
                rep(3,453),
                rep(4,453)))

# Error Bar Plots

# from the cookbook for R
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

h1long_nomiss <- na.omit(h1long)
table(h1long_nomiss$time)

h1se <- summarySE(h1long_nomiss, 
                  measurevar="cesdvalue", 
                  groupvars=c("time","treat"))

ggplot(h1se, aes(x=time, y=cesdvalue)) + 
  geom_errorbar(aes(ymin=cesdvalue-se, ymax=cesdvalue+se), 
                width=.1) +
  geom_line() +
  geom_point() +
  xlab("Time Points") +
  ylab("Depressive Symptoms (CESD)") +
  ggtitle("CESD Means and CI's Over Time") +
  facet_wrap(~treat)

# MLM Approach

# use nlme package
library(nlme)

lme1 <- lme(cesdvalue ~ time*treat,
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

lme2 <- lme(cesdvalue ~ time*treat_flip,
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