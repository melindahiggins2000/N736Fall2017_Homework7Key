# ==================================
# N736 Lesson 22 - Repeated Measures ANOVA
#
# dated 11/12/2017
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
  select(id, pcs, pcs1, pcs2, pcs3, pcs4)

# see https://gribblelab.wordpress.com/2009/03/09/repeated-measures-anova-using-r/
# also see https://biostats.w.uib.no/test-for-sphericity-mauchly-test/

h2 <- h1[,2:6]
h2 <- as.matrix(h2)

m1 <- lm(h2 ~ 1)
m1

tfactor <- factor(c("t0","t1","t2","t3","t4"))

library(car)

m1.aov <- Anova(m1, 
                idata = data.frame(tfactor),
                idesign = ~tfactor, 
                type="III")
#summary(m1.aov, multivariate=FALSE)
summary(m1.aov, multivariate=TRUE)

# restructure into long format

h1long <- h1 %>%
  gather(key=item,
         value=value,
         -c(id))

names(h1long) <- c("id","pcsitem","pcsvalue")

# add a time variable to long format
h1long <- h1long %>%
  mutate(time=c(rep(0,453),
                rep(1,453),
                rep(2,453),
                rep(3,453),
                rep(4,453)))

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
                  measurevar="pcsvalue", 
                  groupvars=c("time"))

ggplot(h1se, aes(x=time, y=pcsvalue)) + 
  geom_errorbar(aes(ymin=pcsvalue-se, ymax=pcsvalue+se), width=.1) +
  geom_line() +
  geom_point() +
  xlab("Time Points") +
  ylab("Physical Component Score (SF-36 PCS)") +
  ggtitle("PCS Means and CI's Over Time")