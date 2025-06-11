##############################################################################
# this code takes the .npz files from Sarah's python github repo
# and converts them into an Rdata file
# for usage with the R code in Vanessa's github repo

# apologies for the bilingual archiving ...this is our reality

##############################################################################
# load packages

# install.packages("reticulate")
library(reticulate)

np <- import("numpy")

##############################################################################
# load data

# load the python data files from Sarah's repository
# (loads from copies I downloaded in May 2025 and archived in my repo)
npz <- np$load("./Data/NoisyPeriodic_PRA.npz")
dpz <- np$load("./Data/DoubleProcess_PRA.npz")
epz <- np$load("./Data/EvenProcess_PRA.npz")
dis <- np$load("./Data/PRA_Distances.npz")

# the download links are:
# https://github.com/smarzen/resource-rational-prediction/blob/main/NoisyPeriodic_PRA.npz
# https://github.com/smarzen/resource-rational-prediction/blob/main/Double_PRA.npz
# https://github.com/smarzen/resource-rational-prediction/blob/main/EvenProcess_PRA.npz
# https://github.com/smarzen/resource-rational-prediction/blob/main/PRA_Distances.npz

##############################################################################
# access the data in the npz files

# see what variables the file contains
npz$files
# "Rs" "Ds" "R_participant" "D_participant"

# access each variable like this
npz$f[["Rs"]]
npz$f[["Ds"]]
npz$f[["R_participant"]]
npz$f[["D_participant"]]

# this is the PRA curve for the Noisy Periodic Process
PRA_rate <- npz$f[["Rs"]]
PRA_accuracy <- npz$f[["Ds"]]
plot(PRA_rate,PRA_accuracy,las=1)

# this is the participants' data in the Noisy Periodic condition
exp_rate <- npz$f[["R_participant"]]
exp_accuracy <- npz$f[["D_participant"]]
plot(exp_rate, exp_accuracy, las=1, pch=16)
lines(PRA_rate, PRA_accuracy)

# get the length of each variable
length(PRA_rate)      # 999
length(PRA_accuracy)  # 999
length(exp_rate)      # 44 - that's the number of sessions completed in NP
length(exp_accuracy)  # 44

# this file contains the distance of participants' session coordinates
# from the curve along 3 dimensions
dis$files

# this is the distance along the accuracy dimension (y-axis)
PRA_distance <- dis$f[["distances_a"]]

# we decided a-priori to analyze distance along the accuracy dimension
# but Sarah has included these other distance measures as well
dis$f[["distances_r"]]  # is distance along the rate dimension (x-axis)
dis$f[["distances_orthogonal"]]  # is orthogonal distance (to nearest x,y point on the line)

# PRA_distance contains 140 values, one per session, maintaining the same row
# order as the entries in experiment.csv, so it can be added on as a new column

# PRA_distance has already been added to experiment.csv, check that they are identical
# round() is used because numeric precision changes when data are read in from a csv
unique(round(df$PRA_distance,10) == round(PRA_distance,10))  # should be TRUE

##############################################################################
# put everything into an Rdata file

# create 6 dataframes, two per experimental condition:
# one with the theoretical PRA curve coordinates
# and another with the participants' data points

# NP
rate <- npz$f[["Rs"]]
accuracy <- npz$f[["Ds"]]
np_pra <- data.frame(rate,accuracy)

rate <- npz$f[["R_participant"]]
accuracy <- npz$f[["D_participant"]]
np_exp <- data.frame(rate,accuracy)

# DP
rate <- dpz$f[["Rs"]]
accuracy <- dpz$f[["Ds"]]
dp_pra <- data.frame(rate,accuracy)

rate <- dpz$f[["R_participant"]]
accuracy <- dpz$f[["D_participant"]]
dp_exp <- data.frame(rate,accuracy)

# EP
rate <- epz$f[["Rs"]]
accuracy <- epz$f[["Ds"]]
ep_pra <- data.frame(rate,accuracy)

rate <- epz$f[["R_participant"]]
accuracy <- epz$f[["D_participant"]]
ep_exp <- data.frame(rate,accuracy)

# save these all in one Rdata file called "PRA.Rdata"
save(np_pra,np_exp,dp_pra,dp_exp,ep_pra,ep_exp,file="./Data/PRA.Rdata")

# here's how to load it back in
# load("./Data/PRA.Rdata")

##############################################################################
# END
##############################################################################