##%##########################################################################%##
#
# Milking Genetic Potential: Structural Change in the U.S. Dairy Industry
# 
# Alexander S. Hopkins and Tristan M. Hanon
# 
# Filename: Structural-Break-Analysis.R
#
##%##########################################################################%##

# Load Packages
library(strucchange)
library(tidyverse)


##%##########################################################################%##
# Test for evidence of structural change in mean                            ####
##%##########################################################################%##

# Create time series objects
mpc_yy_ts <- ts(change_data$mpc_yy, start = c(2001, 1), frequency = 12)
fpc_yy_ts <- ts(change_data$fpc_yy, start = c(2001, 1), frequency = 12)
ppc_yy_ts <- ts(change_data$ppc_yy, start = c(2001, 1), frequency = 12)

# Perform recursive estimates tests
mpc_re <- efp(mpc_yy_ts ~ 1, type = "RE")
sctest(mpc_re)
#plot(mpc_re)

fpc_re <- efp(fpc_yy_ts ~ 1, type = "RE")
sctest(fpc_re)
#plot(fpc_re)

ppc_re <- efp(ppc_yy_ts ~ 1, type = "RE")
sctest(ppc_re)
#plot(ppc_re)

# Perform tests on F-statistics
mpc_fstats <- Fstats(mpc_yy_ts ~ 1)
sctest(mpc_fstats)
#plot(mpc_fstats)

fpc_fstats <- Fstats(fpc_yy_ts ~ 1)
sctest(fpc_fstats)
#plot(fpc_fstats)

ppc_fstats <- Fstats(ppc_yy_ts ~ 1)
sctest(ppc_fstats)
#plot(ppc_fstats)


##%##########################################################################%##
# Find optimal breakpoints and plot                                         ####
##%##########################################################################%##

# Milk per cow
mpc_bp <- breakpoints(mpc_yy_ts ~ 1)
summary(mpc_bp)
#plot(mpc_bp)
coef(mpc_bp, breaks = 3)
confint(mpc_bp, breaks = 3)

# Fat per cow
fpc_bp <- breakpoints(fpc_yy_ts ~ 1)
summary(fpc_bp)
#plot(fpc_bp)
coef(fpc_bp, breaks = 2)
confint(fpc_bp, breaks = 2)

# Protein per cow
ppc_bp <- breakpoints(ppc_yy_ts ~ 1)
summary(ppc_bp)
#plot(ppc_bp)
