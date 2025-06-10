##############################################################################
# load packages

source("./Code/functions.R")  # load my own support functions

# you may need to install the packages before you load the packages below
# to install the packages, run the following lines once
# install.packages("entropy")
# install.packages("lmerTest")

require(entropy)  # for calculating participants' "rate"

require(lmerTest)  # package for linear mixed effects modeling
# this is the same as lme4, but includes Satterthwaite approximation
# to give you degrees of freedom and p values per parameter

##############################################################################

# all references to Sarah's github repository below refer to
# github.com/smarzen/resource-rational-prediction

##############################################################################
# subset the dataframe

# subset by condition
np <- subset(df,input_type=="NoisyPeriodic")
dp <- subset(df,input_type=="DoubleProcess")
ep <- subset(df,input_type=="EvenProcess")

# validate that the correct number of rows are in each subset
nrow(np) # 44
nrow(dp) # 40
nrow(ep) # 56

# subset that took the working memory test
wm <- subset(df,WM=="TRUE")

npw <- subset(wm,input_type=="NoisyPeriodic")
dpw <- subset(wm,input_type=="DoubleProcess")
epw <- subset(wm,input_type=="EvenProcess")

# validate that the correct number of rows are in each subset
nrow(npw) # 25
nrow(dpw) # 21
nrow(epw) # 26


##############################################################################
# Participants learned to predict each timeseries
##############################################################################

# average predictive accuracy by condition

mean(np$total_accuracy)  # 0.880118
mean(dp$total_accuracy)  # 0.648222
mean(ep$total_accuracy)  # 0.5882267

# ceilings on predictive accuracy can be read off of the PRA curves
max(np_pra$accuracy)  # 0.95
max(dp_pra$accuracy)  # 0.8125
max(ep_pra$accuracy)  # 0.7692308


##############################################################################
# Figure 1 (panels a through c)
##############################################################################

# compute the binomial 95% confidence interval (the blue zone on Figure 1)
# on the cumulative accuracy of participants' responses

# example:
# compute the upper and lower bounds on the confidence interval at N = 100 trials

x <- prop.test(x = 50, n = 100, conf.level = .95, correct = FALSE)
x
lower <- x$conf.int[1]  # 0.4038315 % of trials correct
upper <- x$conf.int[2]  # 0.5961685 % of trials correct

# interpretation:
# if the participant had been guessing at chance levels (50%) and completed 100 trials
# we would expect their score to be between 0.4038315-0.5961685 95% of the time
# any scores outside of this range suggest that the participant was not guessing

# now write a function that computes the binomial 95% confidence interval at each trial

##############################################################################
# get_ci()
# get the upper and lower limits on the confidence interval per trial
# for the range of 1 through t trials

# input:  t trials (the max of the range you want to compute for)
# output: a list of two arrays of length t
#         the first array contains the lower limit of the confidence interval per trial
#         the second array contains the upper limit of the confidence interval per trial

get_ci <- function(t) {
    lowers <- c()
    uppers <- c()
    for (t in 1:t) {
        suppressWarnings(x <- prop.test(x = t/2, n = t, conf.level = .95, correct = FALSE))
        lowers[t] <- x$conf.int[1]
        uppers[t] <- x$conf.int[2]
    }
    return(list(lowers,uppers))
}

# example usage:
result <- get_ci(100)  # for trials 1-100
lowers <- result[[1]]
uppers <- result[[2]]

##############################################################################
# cumulative_accuracy()
# get the cumulative accuracy score per trial for one row in df

cumulative_accuracy <- function(df,row) {
    cumulative_accs <- c() 
    accs <- get_data(df,row)[5][[1]]
    trials <- length(accs)
    
    for (t in 1:trials) {
        ans <- sum(accs[0:t])/length(accs[0:t])
        cumulative_accs <- c(cumulative_accs,ans)
    }
    
    return(cumulative_accs)
}

# example usage:
cumulative_accuracy(df,1)


##############################################################################
# create the cumulative accuracy plots in Figure 1

plot_cumulative_accuracy <- function(df,max_trial,max_acc,title) {
    
    # initialize the plot
    plot(1, type="n", las=1, ylim=c(0,1), xlim=c(0,max_trial), xlab="", ylab="",main=title)
    
    # plot polygon for 95% confidence interval
    t <- max_trial+50  # add 50 so polygon runs past the max xlim like the data lines do
    x1 <- c(1:t)
    result <- get_ci(t)
    lowers <- result[[1]]
    uppers <- result[[2]]
    polygon(c(x1, rev(x1)), c(uppers, rev(lowers)), col="lightblue")
    
    line_width <- 0.4
    for (r in 1:nrow(df)) {	# for each participant in the data frame
        cumulative_accs <- cumulative_accuracy(df,r)
        lines(cumulative_accs, type="l", lwd=line_width)
    }
}

# take a look at the plots
# in the paper we zoomed in on the x axis to look at trials 1 through 1200
# but here you can look at all of the trials by setting max_trial to 2700

# create plots showing the max number of trials (2700)
plot_cumulative_accuracy(subset(df,input_type=="NoisyPeriodic"),2700,0.95,title="NP")
plot_cumulative_accuracy(subset(df,input_type=="DoubleProcess"),2700,0.8125,title="DP")
plot_cumulative_accuracy(subset(df,input_type=="EvenProcess"),2700,0.7692308,title="EP")

# for the plots to include in the paper, zoom in on the first couple hundred trials
# to make it easier to see the learning trajectories at the start of the experiment

# save figures to the Plots folder
# note: these are already in the Plots folder in the github repository, 
# so running these lines will overwrite those files

pdf(file = "./Plots/cumulative_accuracy_NoisyPeriodic.pdf", width = 6, height = 6) # The height of the plot in inches
plot_cumulative_accuracy(subset(df,input_type=="NoisyPeriodic"),600,0.95,"Noisy Periodic")
dev.off()

pdf(file = "./Plots/cumulative_accuracy_DoubleProcess.pdf", width = 6, height = 6) # The height of the plot in inches
plot_cumulative_accuracy(subset(df,input_type=="DoubleProcess"),1200,0.8125,"Double Process")
dev.off()

pdf(file = "./Plots/cumulative_accuracy_EvenProcess.pdf", width = 6, height = 6) # The height of the plot in inches
plot_cumulative_accuracy(subset(df,input_type=="EvenProcess"),1200,0.7692308,"Even Process")
dev.off()

# note:
# you'll see that many of the trajectories in Noisy Periodic (NP) all end on trial 350.
# this is because the participants in all of these sessions learned NP within the first 50 trials,
# progressed to the bonus round on trial 50 (everyone had to complete 50 trials before progressing),
# and then completed all 300 trials of the bonus round.

##############################################################################
# get the number of trajectories that exited the blue zone

# compute the blue zone uppers and lowers for all trials
result <- get_ci(2700)
lowers <- result[[1]]
uppers <- result[[2]]

# example 1:
# get the first session's cumulative accuracy trajectory
cac <- cumulative_accuracy(df,r=1)
# check if the trajectory went above the upper bounds, and on which trials
# the trajectory is above the blue zone where all trials are "TRUE" 
cac > uppers[1:length(cac)]
# this session clearly exited the blue zone and we can say this participant learned something

# example 2:
# trajectories that go below the lower bound are also evidence of learning
# but means they are selecting the incorrect response more than expected by chance
# there was one person who exhibited this behavior for an extended period
cac <- cumulative_accuracy(df,r=113)
cac < lowers[1:length(cac)]
# this participant could have misunderstood the instructions
# and thought that the click sound meant their response was wrong

# NOTE:
# the first few trials aren't reliable indicators of learning since the session just started
# so we don't want to say a trajectory exited the zone if it only popped out a few times at the start

# for a rough indicator of how many participants learned
# let's just tally up all of the sessions that ended outside of the blue zone
# because looking at the plots, nearly all of the trajectories that left the zone ended outside of it

# see which rows of the df had trajectories that ended outside of the blue zone
exited <- c()
for (r in 1:nrow(df)) {
    # for the current session, get the cumulative accuracy on each trial
    cac <- cumulative_accuracy(df,r)
    
    # pull out the last trial
    max_trial <- length(cac)
    ca <- cac[max_trial]
    
    # see if it was within the blue zone or not
    low <- lowers[max_trial]
    high <- uppers[max_trial]
    
    # TRUE means it was in the blue zone, FALSE means it was outside
    exited[r] <- ca < high && ca > low
    
    #myprint("----------------")
    #myprint(r)
    #myprint(head(cac < lowers[1:max_trial],100))
}

# this many sessions ended IN the blue zone
sum(exited)  # 13

# this many sessions ended OUT of the blue zone
sum(exited==FALSE)  # 127


##############################################################################
# Participants used resource-rational prediction strategies
##############################################################################

# compute rate and accuracy from participants' prediction behavior

# these values are already stored in df under these columns:
df$total_rate
df$total_accuracy
df$rate_2ndhalf
df$accuracy_2ndhalf

# here is how they were computed:
# this is my own R implementation of how Sarah calculated these numbers
# see Sarah's github repository for her calculations in python

total_rate <- c()      # store rate for each participant here, maintaining row order in df
total_accuracy <- c()  # store accuracy for each participant here, maintaining row order in df

# for each row in the data frame
for (r in 1:nrow(df)) {
    
    # grab the relevant data
    d <- get_data(df,r)
    states <- d[2][[1]]
    predictions <- d[4][[1]]
    accs <- d[5][[1]]
    
    # put all states and predictions into a table
    VSP <- table(states,predictions) 
    
    # estimate the mutual information
    mi <- mi.Dirichlet(VSP,0,unit="log2")  # 0 pseudocount per bin
    total_rate <- c(total_rate,mi)
    
    # calculate the accuracy
    ac <- sum(accs)/length(accs)
    total_accuracy <- c(total_accuracy,ac)
}  

# check that your values are identical to those in df
# round() is used because numeric precision changes when data are read in from a csv
unique(round(total_rate,10) == round(df$total_rate,10))          # should be TRUE
unique(round(total_accuracy,10) == round(df$total_accuracy,10))  # should be TRUE


# these are the same calculations as above, 
# but computed over the second half of the trials only
rate_2ndhalf <- c()      
accuracy_2ndhalf <- c()

# for each row in the data frame
for (r in 1:nrow(df)) {
    
    # get data from the second half of the participant's trials
    n_trials <- nchar(df[r,]$output_sequence)
    n_trials_2ndhalf <- round(n_trials/2)  # round up when odd
    d <- get_data(df,r)
    states <- tail(d[2][[1]],n_trials_2ndhalf)
    predictions <- tail(d[4][[1]],n_trials_2ndhalf)
    accs <- tail(d[5][[1]],n_trials_2ndhalf)
    
    # put all states and predictions into a table
    VSP <- table(states,predictions) 
    
    # estimate the mutual information
    mi <- mi.Dirichlet(VSP,0,unit="log2")  # 0 pseudocount per bin
    rate_2ndhalf <- c(rate_2ndhalf,mi)
    
    # calculate the accuracy
    ac <- sum(accs)/length(accs)
    accuracy_2ndhalf <- c(accuracy_2ndhalf,ac)
}

# check that your values are identical to those in df
unique(round(rate_2ndhalf,10) == round(df$rate_2ndhalf,10))          # should be TRUE
unique(round(accuracy_2ndhalf,10) == round(df$accuracy_2ndhalf,10))  # should be TRUE

# check that they're close to how Sarah computed them
# see Sarah's github repository for her calculations in python
round(np$rate_2ndhalf,2) - round(np_exp$rate,2)  # should all be zero
round(dp$rate_2ndhalf,2) - round(dp_exp$rate,2)
round(ep$rate_2ndhalf,2) - round(ep_exp$rate,2)


##############################################################################
# compute the PRA curves

# see Sarah's python github repository for these computations

# PRA.Rdata contains the PRA curve output data from Sarah's python code
# see load_data.R to load PRA.Rdata
# see npz_converter.R for the code that created this Rdata file 
# from Sarah's npz files

# here's how you can plot the PRA curves
plot(np_pra$rate,np_pra$accuracy,type="l",las=1,main="Noisy Periodic PRA curve",xlab="rate",ylab="accuracy")
plot(dp_pra$rate,dp_pra$accuracy,type="l",las=1,main="Double Process PRA curve",xlab="rate",ylab="accuracy")
plot(ep_pra$rate,ep_pra$accuracy,type="l",las=1,main="Even Process PRA curve",xlab="rate",ylab="accuracy")


##############################################################################
# compute participants' distance from the curve

# see Sarah's python github repository for these computations
# distance from the PRA curve is computed along the accuracy (y-axis) dimension

# these values are already stored in df under this column:
df$PRA_distance


##############################################################################
# mean distance from the PRA curve by condition

# mean distance by condition
round(mean(np$PRA_distance),2)  # 0
round(mean(dp$PRA_distance),2)  # 0.07
round(mean(ep$PRA_distance),2)  # 0.03


##############################################################################
# linear mixed-effects regression analysis

# data points are not independent - they're grouped by participant
# so enter participantID as a random effect

full <- lmer(PRA_distance ~ input_type + (1|participantID), data = df, REML=F)
r1 <- lmer(PRA_distance ~ 1 + (1|participantID), data = df, REML=F)
anova(full,r1)  
"     npar     AIC     BIC logLik -2*log(L)  Chisq Df Pr(>Chisq)    
r1      3 -406.17 -397.34 206.08   -412.17                         
full    5 -437.75 -423.04 223.87   -447.75 35.582  2  1.877e-08 ***"  # full is better

# relevel so Noisy Periodic is the reference condition
df <- within(df, input_type <- relevel(input_type, ref = "NoisyPeriodic"))
full <- lmer(PRA_distance ~ input_type + (1|participantID), data = df, REML=F)
summary(full)
"                          Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)              -0.002914   0.007426 109.995284  -0.392  0.69554       <- NP vs optimal
input_typeDoubleProcess   0.068530   0.010700 138.440070   6.405 2.19e-09 ***   <- DP vs NP
input_typeEvenProcess     0.031385   0.009856 139.898267   3.184  0.00179 **    <- EP vs NP"   

# relevel so Double Process is the reference condition
df <- within(df, input_type <- relevel(input_type, ref = "DoubleProcess"))
full <- lmer(PRA_distance ~ input_type + (1|participantID), data = df, REML=F)
summary(full)
"                          Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)               0.065616   0.007767 126.525004   8.448 5.97e-14 ***   <- DP vs optimal
input_typeNoisyPeriodic  -0.068530   0.010700 138.440070  -6.405 2.19e-09 ***
input_typeEvenProcess    -0.037145   0.010130 139.817456  -3.667 0.000348 ***"

# relevel so Even Process is the reference condition
df <- within(df, input_type <- relevel(input_type, ref = "EvenProcess"))
full <- lmer(PRA_distance ~ input_type + (1|participantID), data = df, REML=F)
summary(full)
"                          Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)               0.028471   0.006578 112.747015   4.328 3.27e-05 ***   <- EP vs optimal
input_typeDoubleProcess   0.037145   0.010130 139.817456   3.667 0.000348 ***   <- DP vs EP
input_typeNoisyPeriodic  -0.031385   0.009856 139.898267  -3.184 0.001789 **"

# notes:
# PRA_distance is distance below the curve, so a significantly positive Estimate denotes sub-optimality
# the "<-" notes to the right of the results indicate the values reported in the manuscript

# examples:
# DP is sub-optimal with an estimated distance of 0.065616 accuracy points below the curve
# DP is less optimal than NP with an estimated distance of 0.068530 accuracy points from NP

# DP is 2.3 times less optimal than EP
round(0.065616/0.028471,2)  # 2.3

# ranking:
# (most optimal) NP > EP > DP (least optimal)


##############################################################################
# Figure 1 (panels d through e)
##############################################################################

# get the ceiling on accuracy for each condition
np_ideal_acc <- max(np_pra$accuracy)  # 0.95
dp_ideal_acc <- max(dp_pra$accuracy)  # 0.8125
ep_ideal_acc <- max(ep_pra$accuracy)  # 0.7692308

np_ideal_rat <- np_pra$rate[which.max(np_pra$accuracy)]  # 1
dp_ideal_rat <- dp_pra$rate[which.max(dp_pra$accuracy)]  # 0.6962123
ep_ideal_rat <- ep_pra$rate[which.max(ep_pra$accuracy)]  # 0.7793498


# Noisy Periodic
plot(np_exp$rate, np_exp$accuracy, las=1, pch=16, xlab="rate (bits)", ylab="accuracy", main="NP")  
lines(np_pra$rate, np_pra$accuracy, type="l")
points(np_ideal_rat, np_ideal_acc, pch=8,col="red")  # plot location of ideal strategy

# Double Process
plot(dp_exp$rate, dp_exp$accuracy, las=1, pch=16, xlab="rate (bits)", ylab="accuracy", main="DP")  
lines(dp_pra$rate, dp_pra$accuracy, type="l")
points(dp_ideal_rat, dp_ideal_acc, pch=8,col="red")  # plot location of ideal strategy

# Even Process
plot(ep_exp$rate, ep_exp$accuracy, las=1, pch=16, xlab="rate (bits)", ylab="accuracy",ylim=c(.45,.8),xlim=c(0,.8), main="EP")  
lines(ep_pra$rate, ep_pra$accuracy, type="l")
points(ep_ideal_rat, ep_ideal_acc, pch=8,col="red")  # plot location of ideal strategy


# save figures to the Plots folder
# note: these are already in the Plots folder in the github repository, 
# so running these lines will overwrite those files

pdf(file = "./Plots/PRA_NoisyPeriodic.pdf", width = 6, height = 6)
plot(np_exp$rate, np_exp$accuracy, las=1, pch=16, xlab="rate (bits)", ylab="accuracy", main="Noisy Periodic")  
lines(np_pra$rate, np_pra$accuracy, type="l")
points(np_ideal_rat, np_ideal_acc, pch=8,col="red")  
dev.off()

pdf(file = "./Plots/PRA_DoubleProcess.pdf", width = 6, height = 6)
plot(dp_exp$rate, dp_exp$accuracy, las=1, pch=16, xlab="rate (bits)", ylab="accuracy", main="Double Process")  
lines(dp_pra$rate, dp_pra$accuracy, type="l")
points(dp_ideal_rat, dp_ideal_acc, pch=8,col="red")  
dev.off()

pdf(file = "./Plots/PRA_EvenProcess.pdf", width = 6, height = 6)
plot(ep_exp$rate, ep_exp$accuracy, las=1, pch=16, xlab="rate (bits)", ylab="accuracy",ylim=c(.45,.8),xlim=c(0,.8), main="Even Process")  
lines(ep_pra$rate, ep_pra$accuracy, type="l")
points(ep_ideal_rat, ep_ideal_acc, pch=8,col="red")
dev.off()


##############################################################################
# A traditional analysis of prediction performance
##############################################################################

# do a traditional analysis where you just look at performance relative to the 
# maximum possible accuracy per condition
# without accounting for the participant's demonstrated rate

# store the new values you compute here on another dataframe called df2

# Noisy Periodic
of_maxacc <- np_ideal_acc-np$accuracy_2ndhalf
np2 <- cbind(np,of_maxacc)
ncol(np)+1 == ncol(np2)  # should be TRUE

# Double Process
of_maxacc <- dp_ideal_acc-dp$accuracy_2ndhalf
dp2 <- cbind(dp,of_maxacc)
ncol(dp)+1 == ncol(dp2)  # should be TRUE

# Even Process
of_maxacc <- ep_ideal_acc-ep$accuracy_2ndhalf
ep2 <- cbind(ep,of_maxacc)
ncol(ep)+1 == ncol(ep2)  # should be TRUE


# mean distance of participants' accuracy to the max accuracy
mean(np2$of_maxacc)  # 0.048222
mean(dp2$of_maxacc)  # 0.1476041
mean(ep2$of_maxacc)  # 0.1800286

# re-frame the same numbers as mean percentage of max accuracy achieved
mean(1-np2$of_maxacc)  # 0.951778
mean(1-dp2$of_maxacc)  # 0.8523959
mean(1-ep2$of_maxacc)  # 0.8199714


# bind these by-condition calculations together into one dataframe again for the LMER
df2 <- rbind(np2,dp2,ep2)

# relevel so Noisy Periodic is the reference condition
df2 <- within(df2, input_type <- relevel(input_type, ref = "NoisyPeriodic"))
full <- lmer(of_maxacc ~ input_type + (1|participantID), data = df2, REML=F)
summary(full)
"                         Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)               0.05546    0.01373 132.91985   4.041 8.97e-05 ***   <- NP vs ceiling
input_typeDoubleProcess   0.09344    0.01915 139.27110   4.879 2.87e-06 ***   <- DP vs NP
input_typeEvenProcess     0.12644    0.01752 137.15618   7.216 3.31e-11 ***   <- EP vs NP"

# relevel so Double Process is the reference condition
df2 <- within(df2, input_type <- relevel(input_type, ref = "DoubleProcess"))
full <- lmer(of_maxacc ~ input_type + (1|participantID), data = df2, REML=F)
summary(full)
"                         Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)               0.14890    0.01419 136.47403  10.493  < 2e-16 ***   <- DP vs ceiling
input_typeNoisyPeriodic  -0.09344    0.01915 139.27110  -4.879 2.87e-06 ***
input_typeEvenProcess     0.03299    0.01805 138.61999   1.828   0.0698 .     <- DP vs EP"

# relevel so Even Process is the reference condition
df2 <- within(df2, input_type <- relevel(input_type, ref = "EvenProcess"))
full <- lmer(of_maxacc ~ input_type + (1|participantID), data = df2, REML=F)
summary(full)
"                         Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)               0.18190    0.01219 128.82113  14.920  < 2e-16 ***   <- EP vs ceiling
input_typeDoubleProcess  -0.03299    0.01805 138.61999  -1.828   0.0698 .  
input_typeNoisyPeriodic  -0.12644    0.01752 137.15618  -7.216 3.31e-11 ***"

# notes:
# the "<-" notes to the right of the results indicate the values reported in the manuscript

# ranking:
# (most optimal) NP > DP > EP (least optimal)

# but the difference between DP and EP was not significant (it was marginal)
# so the ranking to report is
# (most optimal) NP > DP = EP (least optimal)


##############################################################################
# and then of course the worst-possible comparison you could make would be to 
# directly compare the accuracy scores across conditions without accounting for 
# the fact that each HMM has a different ceiling on predictive accuracy

# straightup accuracies
mean(np$accuracy_2ndhalf)  # 0.901778
mean(dp$accuracy_2ndhalf)  # 0.6648959
mean(ep$accuracy_2ndhalf)  # 0.5892022

# relevel so Noisy Periodic is the reference condition
df <- within(df, input_type <- relevel(input_type, ref = "NoisyPeriodic"))
full <- lmer(accuracy_2ndhalf ~ input_type + (1|participantID), data = df, REML=F)
summary(full)
"                         Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)               0.89454    0.01373 132.91985   65.17   <2e-16 ***
input_typeEvenProcess    -0.30721    0.01752 137.15618  -17.53   <2e-16 ***
input_typeDoubleProcess  -0.23094    0.01915 139.27110  -12.06   <2e-16 ***"

# relevel so Double Process is the reference condition
df <- within(df, input_type <- relevel(input_type, ref = "DoubleProcess"))
full <- lmer(accuracy_2ndhalf ~ input_type + (1|participantID), data = df, REML=F)
summary(full)
"                         Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)               0.66360    0.01419 136.47403  46.762  < 2e-16 ***
input_typeNoisyPeriodic   0.23094    0.01915 139.27110  12.059  < 2e-16 ***
input_typeEvenProcess    -0.07626    0.01805 138.61999  -4.225 4.31e-05 ***"

# relevel so Even Process is the reference condition
df <- within(df, input_type <- relevel(input_type, ref = "EvenProcess"))
full <- lmer(accuracy_2ndhalf ~ input_type + (1|participantID), data = df, REML=F)
summary(full)
"                         Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)               0.58733    0.01219 128.82113  48.175  < 2e-16 ***
input_typeDoubleProcess   0.07626    0.01805 138.61999   4.225 4.31e-05 ***
input_typeNoisyPeriodic   0.30721    0.01752 137.15618  17.533  < 2e-16 ***"

# ranking:
# (most optimal) NP > DP > EP (least optimal)
# here the differences appear to be largest and they are all significant

# and that just ranks them by the max accuracy of the HMM
max(np_pra$accuracy)  # 0.95
max(dp_pra$accuracy)  # 0.8125
max(ep_pra$accuracy)  # 0.7692308


##############################################################################
# Working memory is related to rate
##############################################################################

# linear mixed-effects regression analysis

# data points are not independent - they're grouped by participant and input type
# so enter participantID and input_type as random effects

full <- lmer(total_rate ~ WM_score + (1|participantID) + (1|input_type), data = wm, REML=FALSE)
r1 <- lmer(total_rate ~ 1 + (1|participantID) + (1|input_type), data = wm, REML=FALSE)

anova(full,r1)
"     npar     AIC     BIC logLik -2*log(L)  Chisq Df Pr(>Chisq)  
r1      4 -19.725 -10.619 13.863   -27.725                       
full    5 -22.113 -10.730 16.056   -32.113 4.3878  1     0.0362 *"

summary(full)
"            Estimate Std. Error       df t value Pr(>|t|)  
(Intercept) -0.97722    0.62058 45.74753  -1.575    0.122  
WM_score     0.03250    0.01492 42.01881   2.179    0.035 *"

# interpretation:
# each point increase in WM score gives you a 0.03250 increase in rate
# (that's a 1/42 = 0.0238 increase in working memory score)
# and the result is significant at the p = 0.05 level


##############################################################################
# Prediction algorithm results
##############################################################################

# see Sarah's github repository for her and Amy's analysis in python


##############################################################################
# END
##############################################################################

# run specifications
"
> version
_                           
platform       x86_64-apple-darwin20       
arch           x86_64                      
os             darwin20                    
system         x86_64, darwin20            
status                                     
major          4                           
minor          5.0                         
year           2025                        
month          04                          
day            11                          
svn rev        88135                       
language       R                           
version.string R version 4.5.0 (2025-04-11)
nickname       How About a Twenty-Six"
