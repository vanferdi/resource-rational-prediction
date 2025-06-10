##############################################################################
# basic participation info

# total number of participants who took the experiment
uniqueIDs <- unique(df$participantID)
length(uniqueIDs)  # 93 participants

# total number of times the experiment was taken
nrow(df)  # 140 sessions

# sessions per experimental condition
table(df$input_type)
"DoubleProcess   EvenProcess NoisyPeriodic 
            40            56            44 "

# histogram trials completed
hist(df$trials_completed, las=1, breaks = 10)

# trials completed by condition:

# subset the dataframe by condition
np <- subset(df,input_type=="NoisyPeriodic")
dp <- subset(df,input_type=="DoubleProcess")
ep <- subset(df,input_type=="EvenProcess")

# NP condition
min(np$trials_completed)          # 123
round(mean(np$trials_completed))  # 494
max(np$trials_completed)          # 2700

# DP condition
min(dp$trials_completed)          # 323
round(mean(dp$trials_completed))  # 901
max(dp$trials_completed)          # 2700

# EP condition
min(ep$trials_completed)          # 56
round(mean(ep$trials_completed))  # 907
max(ep$trials_completed)          # 2700


##############################################################################
# return vs one-off participants

# get the number of sessions completed per participant
table(df$participantID)
n <- as.numeric(table(df$participantID))
rev(sort(n)) # 7 6 5 5 4 ...
# each entry is the number of sessions for one participant, sorted highest to lowest

# validate that these sum to 140 sessions
sum(n)  # 140

# how many participants took the experiment only once?
sum(table(df$participantID) == 1)  # 71 took it only once
sum(table(df$participantID) > 1)   # 22 took it more than once

# how many participants took the experiment n times?
sum(table(df$participantID) == 2)  # 12 took it 2x
sum(table(df$participantID) == 3)  #  3 took it 3x
sum(table(df$participantID) == 4)  #  2 took it 4x
sum(table(df$participantID) == 5)  #  3 took it 5x
sum(table(df$participantID) == 6)  #  1 took it 6x
sum(table(df$participantID) == 7)  #  1 took it 7x

# get the list of all participants who took it once
oneoffIDs <- names((table(df$participantID) == 1)[table(df$participantID) == 1])
length(oneoffIDs)  # 71

# get the list of all participants who took it more than once
returnIDs <- names((table(df$participantID) > 1)[table(df$participantID) > 1])
length(returnIDs)  # 22

# next let's compute the percentage of sessions taken by return participants

# return the total number of sessions taken by any subset of participant IDs
get_sessions <- function(IDs) {
	sessions <- c()
	for (id in IDs) {
		sub <- subset(df,participantID == id)
		sessions <- c(sessions,nrow(sub))
	}
	return(sessions)
}

# validation tests
unique(get_sessions(oneoffIDs)==1)  # all values should be = 1
unique(get_sessions(returnIDs)>1)  # all values should be > 1
unique(sort(get_sessions(returnIDs)) == (sort(n))[sort(n)>1])  # all of these should be equal

# get the total number of sessions taken by one-off participants
# (this will be equal to the number of one-off participants)
oneoff_df <- subset(df,participantID %in% oneoffIDs)
nrow(oneoff_df)  # 71, correct

# get the total number of sessions taken by return participants
return_df <- subset(df,participantID %in% returnIDs)
nrow(return_df)  # 69
sum(get_sessions(returnIDs))  # check that number another way - it's also 69

# make sure these two numbers sum to the total number of sessions
nrow(oneoff_df)+nrow(return_df)   # 140, correct

# get the percentage of sessions completed by one-off participants
71/140  # 0.5071429

# get the percentage of sessions completed by return participants
69/140  # 0.4928571

# sessions were approx 50/50 one-offs vs repeat participants


##############################################################################
# return participants by condition

table(oneoff_df$input_type)
"DoubleProcess   EvenProcess NoisyPeriodic 
            23            26            22"

# create a table showing how the sessions of return participants were divided across conditions

tab <- table(return_df$participantID,return_df$input_type)
tab <- tab[,c(3,1,2)]  # change column order to match order in paper
colnames(tab) <- c("NP","DP","EP")
tab
"               NP DP EP
  bjiij645459N  4  0  0
  egkdu989924I  0  0  2
  gedwx424171B  2  0  1
  jalpp395314D  1  1  0
  jkouu878410T  1  0  1
  jodzw609607J  0  0  2
  kvgjv670707S  2  1  2
  lnmld144115K  1  0  4
  lvoaf406127I  1  1  0
  mfgjn358614M  0  2  2
  nhctv7082N    3  0  3
  olpaq745103X  0  1  1
  pdyeo705367C  0  0  2
  pkowk188097E  1  0  1
  ppfqx39138S   1  0  1
  qefkp644319D  0  0  2
  rdeiw90318W   0  2  0
  subwn512365U  1  1  1
  ufopp989065S  1  1  1
  ydvyc213678S  0  1  1
  ygcdt430284G  3  3  1
  yogba802779Q  0  3  2"

# how many of these 22 repeat participants took 1, 2, or all 3 of the conditions?

# compute the answer to that question using the table
result <- c()
for (i in 1:nrow(tab)) {
	vals <- as.numeric(tab[i,])
	if (sum(vals == 0) == 0) { result[i] <- 3 } # no zeros means they took all conditions
	if (sum(vals == 0) == 1) { result[i] <- 2 } # one zero means they took two conditions
	if (sum(vals == 0) == 2) { result[i] <- 1 } # two zeros means they took one condition
	
	# for people who only took one condition, print the number of sessions they did
	if (sum(vals == 0) == 2) { print(max(vals)) }
}

table(result)
" 1  2  3 
  6 12  4 "

# this table means:
# 6 of the repeat participants took one condition
# 12 of the repeat participants took two conditions
# 4 of the repeat participants took all three conditions

# for the 6 people who only took one condition, these are the conditions they took:
# 1 person took NP 4x
# 1 person took DP 2x
# 4 people took EP 2x

# take home message:
# most people repeated the experiment twice and took two different conditions


# Create latex table of these stats for the Supplemental Information

# sort the columns into some form of a logical order
tab2 <- as.data.frame(cbind(tab,result))
tab2 <- tab2[order(tab2$result),]
# manually refine the order
ord <- c(6,1,2,3,4,5,14,7,13,15,8,16,18,9,11,12,10,17,21,19,20,22)
tab2 <- cbind(tab2,ord)
tab2 <- tab2[order(tab2$ord),]
tab2

# print one row per condition to paste into latex table code
printer <- function(array) {
	result <- c()
	for (i in array) {
		result <- c(result,"&",i)
	}
	result <- paste0(result,collapse=" ")
	return(result)
}

printer(tab2$NP)  # & 0 & 0 & 0 & 0 & 0 & 4 & 1 & 1 & 0 & 0 & 1 & 1 & 1 & 2 & 1 & 0 & 0 & 3 & 1 & 1 & 2 & 3
printer(tab2$DP)  # & 0 & 0 & 0 & 0 & 2 & 0 & 1 & 1 & 1 & 1 & 0 & 0 & 0 & 0 & 0 & 2 & 3 & 0 & 1 & 1 & 1 & 3
printer(tab2$EP)  # & 2 & 2 & 2 & 2 & 0 & 0 & 0 & 0 & 1 & 1 & 1 & 1 & 1 & 1 & 4 & 2 & 2 & 3 & 1 & 1 & 2 & 1


##############################################################################
# working memory participants by condition

# make a dataframe with only the participants who took the working memory test
wm <- subset(df,WM=="TRUE")

# get the number of working memory participants and their number of sessions
length(unique(wm$participantID))  # 37 participants
nrow(wm)                          # 72 sessions

# how many one-off participants took the WM test?
length(unique(subset(oneoff_df,WM=="TRUE")$participantID))  # 24 participants
nrow(subset(oneoff_df,WM=="TRUE"))                          # 24 sessions

# how many return participants took the WM test?
length(unique(subset(return_df,WM=="TRUE")$participantID))  # 13 participants
nrow(subset(return_df,WM=="TRUE"))                          # 48 sessions

# sessions across conditions
table(wm$input_type)
"DoubleProcess   EvenProcess NoisyPeriodic 
            21            26            25 "


# how many one-off participants were there per condition?
oneoff_NP <- subset(oneoff_df,input_type=="NoisyPeriodic")
length(unique(subset(oneoff_NP,WM=="TRUE")$participantID))  # 8 one-off participants

oneoff_DP <- subset(oneoff_df,input_type=="DoubleProcess")
length(unique(subset(oneoff_DP,WM=="TRUE")$participantID))  # 10 one-off participants

oneoff_EP <- subset(oneoff_df,input_type=="EvenProcess")
length(unique(subset(oneoff_EP,WM=="TRUE")$participantID))  # 6 one-off participants


# how many return participants per condition?
return_NP <- subset(return_df,input_type=="NoisyPeriodic")
length(unique(subset(return_NP,WM=="TRUE")$participantID))  # 9 return participants

return_DP <- subset(return_df,input_type=="DoubleProcess")
length(unique(subset(return_DP,WM=="TRUE")$participantID))  # 7 return participants

return_EP <- subset(return_df,input_type=="EvenProcess")
length(unique(subset(return_EP,WM=="TRUE")$participantID))  # 11 return participants

# Summary:
# 37 participants took the working memory test.
# These 37 people completed a total of 72 experimental sessions.
# 24 of these 37 participants took the experiment one time. 
# The remaining 13 participants took the experiment more than once.
# These 13 people completed a total of 48 experimental sessions.
# So 1/3 of the sessions with WM scores were from 24 people and 2/3 were from 13 people.


##############################################################################
# END
##############################################################################