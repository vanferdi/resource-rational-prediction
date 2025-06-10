##############################################################################
# load the experimental data

df <- read.csv("./Data/experiment.csv",
               colClasses=c(input_sequence="character",
               output_sequence="character",
               hidden_sequence="character",
               accuracy="character",
               input_type="factor"))

# check that it loaded
nrow(df)  # there should be 140 rows
ncol(df)  # and 34 columns
df[1,1]   # this is the first participant: "bjiij645459N"


##############################################################################
# load the PRA curves

load("./Data/PRA.Rdata")

# loads 6 dataframes:
np_pra  # contains the PRA curve for the Noisy Periodic (NP) condition
np_exp  # contains participants' rate and accuracy per NP session as Sarah calculated it

dp_pra  # contains the PRA curve for the Double Process (DP) condition
dp_exp  # contains participants' rate and accuracy per DP session as Sarah calculated it

ep_pra  # contains the PRA curve for the Even Process (EP) condition
ep_exp  # contains participants' rate and accuracy per EP session as Sarah calculated it


##############################################################################
# END
##############################################################################