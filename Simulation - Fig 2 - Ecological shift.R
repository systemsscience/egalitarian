#------------------------------------------------------------------------------------
# Set up model ----------------------------------------------------------------------
#------------------------------------------------------------------------------------

# Load packages and set working directory -------------------------------------------

# Uncomment these to install them for the first time
#install.packages("gtools")
#install.packages("plyr")

library(gtools)
library(plyr)

# Set working directory
setwd("~/Documents/R/egalitarian")

# Define parameters -----------------------------------------------------------------

# Define Hawk-Dove and Prisoner's Dilemma parameters BEFORE and AFTER the parameter shift
vrange <- c(1,1)
drange <- c(1,1)
brange <- c(1,2)
crange <- c(1,1)
wrange <- c(0.9,0.9)

# Evolutionary parameters
startingfractionHawkDefect <- 1
mutationprobability <- 0.01
BaselineFitness <- 10

# Run parameters
maxiterations <- 100000
repetitions <- 1
epoch <- 10000
xminmax <- c(-100,200)
time <- 1:(epoch*2)-epoch

# Define strategies ----------------------------------------------------------------

# The strategy set is defined by the matrix S
# Each row is a unique strategy, with behavior defined by the 0-or-1 values in the columns
S <- as.data.frame(permutations(2,8,c(0,1),repeats.allowed=T))
names(S) <- c("DOVE","COOPERATOR","CONDITIONALDOVE","CONDITIONALCOOPERATOR","LEVELLER","COMPLYTOCONDITIONALDOVE","COMPLYTOCONDITIONALCOOPERATOR","COMPLYTOLEVELLER")

# Filter out meaningless/impossible strategies
throwout <- which(
	(S$COOPERATOR==0 & (S$CONDITIONALCOOPERATOR==1 | S$LEVELLER==1)) # Has to be a cooperator to withdraw cooperation
	|(S$DOVE==0 & (S$CONDITIONALDOVE==1 | S$LEVELLER==1)) # Has to be a dove to withdraw playing dove
	|(S$DOVE==1 & (S$COMPLYTOCONDITIONALDOVE==1 | S$COMPLYTOLEVELLER==1)) # Compliance to punishment is not relevant for true doves
	|(S$COOPERATOR==1 & S$COMPLYTOCONDITIONALCOOPERATOR==1) # Compliance to punishment is not relevant for true cooperators
	|(S$DOVE==1 & S$CONDITIONALDOVE==1 | S$COMPLYTOCONDITIONALDOVE==1) # Ignore conditional doves for the current purpose
)
S <- as.matrix(S)
S <- as.data.frame(S[-throwout,])
S # This filtering will leave 14 unique strategies (rows)


# Initialize some accounting variables -----------------------------------------------

# Create matrices to store the frequencies of strategies and behavior through tim
HistoryOfFrequencies <- matrix(nrow=epoch*2,ncol=dim(S)[1])


#------------------------------------------------------------------------------------
# Complete one run for this parameter combination -----------------------------------
#------------------------------------------------------------------------------------

v <- vrange[1]
d <- drange[1]
b <- brange[1]
c <- crange[1]
w <- wrange[1]

# Compute payoff matrix------------------------------------------

# This is done once for each parameter combination, even if there are multiple runs.
# The payoff matrix PAYOFFS has one row per strategy, one column per strategy.
# It gives the payoff to the row strategy when facing the column strategy.
PAYOFFS <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])

# These record the sequence of Hawk/Dove and Cooperate/Defect moves 
# by the row strategy when facing the column strategy
RowPlaysDove <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])
RowPlaysCooperate <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])
ColumnPlaysDove <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])
ColumnPlaysCooperate <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])

# These record the average number of times row plays Dove and Cooperate
AvgRoundsRowPlaysDove <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])
AvgRoundsRowPlaysCooperate <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])

# For each i row strategy and j column strategy in the strategy matrix
for(i in 1:dim(S)[1]) {
	for(j in 1:dim(S)[1]) {
		
		# How many rounds do we need to account for?  This number just needs to be high
		# enough for all transient behavioral interactions within pairs to settle down.
		maxrounds <- 10 
		
		# Create accounting variables to record 
		# Hawk-Dove & Cooperate-Defect behavior across rounds
		RowPlaysDoveInRound <- rep(NA, maxrounds)
		RowPlaysCooperateInRound <- rep(NA, maxrounds)		
		ColumnPlaysDoveInRound <- rep(NA, maxrounds)
		ColumnPlaysCooperateInRound <- rep(NA, maxrounds)
		
		# Create accounting variables to record
		# admonishment by punishing strategies
		RowConditionalDoveAdmonishes <- rep(0, maxrounds)
		RowLevellerAdmonishes <- rep(0, maxrounds)
		RowConditionalCooperatorAdmonishes <- rep(0, maxrounds)
		ColumnConditionalDoveAdmonishes <- rep(0, maxrounds)
		ColumnLevellerAdmonishes <- rep(0, maxrounds)
		ColumnConditionalCooperatorAdmonishes <- rep(0, maxrounds)
		
		# Create accounting variables to record payoffs
		RowHDPayoff <- rep(NA, maxrounds)
		RowPDPayoff <- rep(NA, maxrounds)
		ColumnHDPayoff <- rep(NA, maxrounds)
		ColumnPDPayoff <- rep(NA, maxrounds)
		
		# What is the behavior of row and column in the first and subsequent rounds?
		
		# 1st Round - Players have no info, they follow their 1st nature
		RowPlaysDoveInRound[1] <- S[i,]$DOVE 
		RowPlaysCooperateInRound[1] <- S[i,]$COOPERATOR 
		ColumnPlaysDoveInRound[1] <- S[j,]$DOVE 
		ColumnPlaysCooperateInRound[1] <- S[j,]$COOPERATOR 
		
		# Round 2 through maxrounds
		for (round in 2:maxrounds) {
			
			# Set the default behavior according to the basic propensities of the strategies
			RowPlaysDoveInRound[round] <- S[i,]$DOVE 
			RowPlaysCooperateInRound[round] <- S[i,]$COOPERATOR 
			ColumnPlaysDoveInRound[round] <- S[j,]$DOVE 
			ColumnPlaysCooperateInRound[round] <- S[j,]$COOPERATOR 
			
			# If row has been admonished by a conditional dove column and is compliant, row plays dove
			if (S[i,]$COMPLYTOCONDITIONALDOVE==1 & max(ColumnConditionalDoveAdmonishes[1:(round-1)])==1) {
				RowPlaysDoveInRound[round] <- 1
			}
			# If row has been admonished by a column leveller and is compliant, row plays dove
			if (S[i,]$COMPLYTOLEVELLER==1 & max(ColumnLevellerAdmonishes[1:(round-1)])==1) {
				RowPlaysDoveInRound[round] <- 1
			}
			
			# If row is a conditional dove and column played hawk last round, row maybe punishes with hawk
			if (S[i,]$CONDITIONALDOVE==1 & ColumnPlaysDoveInRound[round-1]==0) {
				# Column played hawk against a conditional dove row
				if (sum(RowConditionalDoveAdmonishes)==0) { 
					# Row hasn't admonished before
					# The conditional dove row plays hawk and admonishes
					RowPlaysDoveInRound[round] <- 0 
					RowConditionalDoveAdmonishes[round] <- 1
				} else {
					# Row has admonished as a conditional dove before
					if (RowConditionalDoveAdmonishes[round-1]==1 & sum(RowConditionalDoveAdmonishes)==1) { 
						# Row just admonished once, last round. Don't punish, as column
						# may come around this round based on last round's admonishment.
					} else { # Column has no excuse. Row punishes as a conditional dove.
						RowPlaysDoveInRound[round] <- 0
						RowConditionalDoveAdmonishes[round] <- 1
					}
				}
			}
			
			# Row may cooperate if is compliant to conditional cooperators
			if (S[i,]$COMPLYTOCONDITIONALCOOPERATOR==1 & max(ColumnConditionalCooperatorAdmonishes[1:(round-1)])==1) {
				# Row is compliant to conditional cooperators, and has been punished by column
				# in the past. Therefore row plays cooperate.
				RowPlaysCooperateInRound[round] <- 1
			}
			
			# If row is a conditional cooperator and column defected last round, row considers punishment
			if (S[i,]$CONDITIONALCOOPERATOR==1 & ColumnPlaysCooperateInRound[round-1]==0) {
				# If row is compliant to leveller and column admonished row as a leveller last round, don't treat it as a defection
				if (ColumnLevellerAdmonishes[round-1]==1 & S[i,]$COMPLYTOLEVELLER==1) {
					# Do nothing
				} else {										
					if (sum(RowConditionalCooperatorAdmonishes)==0) { 
						# Row hasn't punished column before. Row defects and admonishes.
						RowPlaysCooperateInRound[round] <- 0
						RowConditionalCooperatorAdmonishes[round] <- 1
					} else {
						if (RowConditionalCooperatorAdmonishes[round-1]==1 & sum(RowConditionalCooperatorAdmonishes)==1) { 
							# Row just admonished once, last round. Don't punish, as column
							# may come around this round based on last round's admonishment.
						} else { # Column has no excuse. Row punishes as a conditional cooperator.
							RowPlaysCooperateInRound[round] <- 0
							RowConditionalCooperatorAdmonishes[round] <- 1
						}
					}
				}
			}
			
			# If row is a leveller and column played dove last round, row considers punishment.
			if (S[i,]$LEVELLER==1 & ColumnPlaysDoveInRound[round-1]==0) {
				if (sum(RowLevellerAdmonishes)==0) { 
					# Row hasn't punished column before. Row defects and admonishes.
					RowPlaysCooperateInRound[round] <- 0
					RowLevellerAdmonishes[round] <- 1
				} else {
					if (RowLevellerAdmonishes[round-1]==1 & sum(RowLevellerAdmonishes)==1) { 
						# Row just admonished once, last round. Don't punish, as column
						# may come around this round based on last round's admonishment.
					} else { # Column has no excuse. Row punishes as a conditional cooperator.
						RowPlaysCooperateInRound[round] <- 0
						RowLevellerAdmonishes[round] <- 1
					}
				}
			}
			
			# If column complies to conditional dove and has been admonished, column plays dove
			if (S[j,]$COMPLYTOCONDITIONALDOVE==1 & max(RowConditionalDoveAdmonishes[1:(round-1)])==1) {
				ColumnPlaysDoveInRound[round] <- 1
			}
			# If column complies to leveller and has been admonished, column plays dove
			if (S[j,]$COMPLYTOLEVELLER==1 & max(RowLevellerAdmonishes[1:(round-1)])==1) {
				ColumnPlaysDoveInRound[round] <- 1
			}
			
			# If column is a conditional dove and row played hawk last round, column considers punishing
			if (S[j,]$CONDITIONALDOVE==1 & RowPlaysDoveInRound[round-1]==0) {
				if (sum(ColumnConditionalDoveAdmonishes)==0) {
					# Column hasn't punished row before. Column defects and admonishes.
					ColumnPlaysDoveInRound[round] <- 0
					ColumnConditionalDoveAdmonishes[round] <- 1
				} else {
					if (ColumnConditionalDoveAdmonishes[round-1]==1 & sum(ColumnConditionalDoveAdmonishes)==1) { 
						# Column just admonished once, last round. Don't punish, as row
						# may come around this round based on last round's admonishment.
					} else { # Row has no excuse. Row punishes as a conditional dove..
						ColumnPlaysDoveInRound[round] <- 0
						ColumnConditionalDoveAdmonishes[round] <- 1
					}
				}
			}
			
			# If column complies to conditional cooperators and has been admonished, column cooperates
			if (S[j,]$COMPLYTOCONDITIONALCOOPERATOR==1 & max(RowConditionalCooperatorAdmonishes[1:(round-1)])==1) {
				ColumnPlaysCooperateInRound[round] <- 1
			}
			
			# If column is a conditional cooperator and row defects, column considers punishment
			if (S[j,]$CONDITIONALCOOPERATOR==1 & RowPlaysCooperateInRound[round-1]==0) {
				# If column is compliant to leveller and row admonished column as a leveller last round, don't treat it as a defection
				if (RowLevellerAdmonishes[round-1]==1 & S[j,]$COMPLYTOLEVELLER==1) {
					# Do nothing
				} else {
					if (sum(ColumnConditionalCooperatorAdmonishes)==0) { 
						# Column hasn't punished row before. Column defects and admonishes.
						ColumnPlaysCooperateInRound[round] <- 0
						ColumnConditionalCooperatorAdmonishes[round] <- 1
					} else {
						if (ColumnConditionalCooperatorAdmonishes[round-1]==1 & sum(ColumnConditionalCooperatorAdmonishes)==1) { 
							# Column just admonished once, last round. Don't punish, as row
							# may come around this round based on last round's admonishment.
						} else { # Row has no excuse. Row punishes as a conditional coopreator.
							ColumnPlaysCooperateInRound[round] <- 0
							ColumnConditionalCooperatorAdmonishes[round] <- 1
						}
					}
				}
			}
			
			# If column is a leveller and row played hawk last round, column considers punishment
			if (S[j,]$LEVELLER==1 & RowPlaysDoveInRound[round-1]==0) {
				if (sum(ColumnLevellerAdmonishes)==0) { 
					# Column hasn't punished row before. Column defects and admonishes.
					ColumnPlaysCooperateInRound[round] <- 0
					ColumnLevellerAdmonishes[round] <- 1
				} else {
					if (ColumnLevellerAdmonishes[round-1]==1 & sum(ColumnLevellerAdmonishes)==1) { 														# Column just admonished once, last round. Don't punish, as row
						# may come around this round based on last round's admonishment.
					} else { # Row has no excuse. Row punishes as leveller.
						ColumnPlaysCooperateInRound[round] <- 0
						ColumnLevellerAdmonishes[round] <- 1
					}
				}
			}
		}
		
		# Add up payoffs -----------
		for (round in 1:maxrounds) {
			# Row payoff from hawk-dove
			if(RowPlaysDoveInRound[round]==1 & ColumnPlaysDoveInRound[round]==1) {RowHDPayoff[round] <- v/2}
			if(RowPlaysDoveInRound[round]==1 & ColumnPlaysDoveInRound[round]==0) {RowHDPayoff[round] <- 0}
			if(RowPlaysDoveInRound[round]==0 & ColumnPlaysDoveInRound[round]==1) {RowHDPayoff[round] <- v}
			if(RowPlaysDoveInRound[round]==0 & ColumnPlaysDoveInRound[round]==0) {RowHDPayoff[round] <- (v-d)/2}
			# weight the payoff by the probability of reaching that round
			RowHDPayoff[round] <- RowHDPayoff[round] * w ^ (round-1) 
			
			# Row payoff from prisoner's dilemma
			if(RowPlaysCooperateInRound[round]==1 & ColumnPlaysCooperateInRound[round]==1) {RowPDPayoff[round] <- b-c}
			if(RowPlaysCooperateInRound[round]==1 & ColumnPlaysCooperateInRound[round]==0) {RowPDPayoff[round] <- -c}
			if(RowPlaysCooperateInRound[round]==0 & ColumnPlaysCooperateInRound[round]==1) {RowPDPayoff[round] <- b}
			if(RowPlaysCooperateInRound[round]==0 & ColumnPlaysCooperateInRound[round]==0) {RowPDPayoff[round] <- 0}
			# weight the payoff by the probability of reaching that round
			RowPDPayoff[round] <- RowPDPayoff[round] * w ^ (round-1)
			
			# Column payoff from hawk-dove
			if(ColumnPlaysDoveInRound[round]==1 & RowPlaysDoveInRound[round]==1) {ColumnHDPayoff[round] <- v/2}
			if(ColumnPlaysDoveInRound[round]==1 & RowPlaysDoveInRound[round]==0) {ColumnHDPayoff[round] <- 0}
			if(ColumnPlaysDoveInRound[round]==0 & RowPlaysDoveInRound[round]==1) {ColumnHDPayoff[round] <- v}
			if(ColumnPlaysDoveInRound[round]==0 & RowPlaysDoveInRound[round]==0) {ColumnHDPayoff[round] <- (v-d)/2}
			# weight the payoff by the probability of reaching that round
			ColumnHDPayoff[round] <- ColumnHDPayoff[round] * w ^ (round-1)
			
			# Column payoff from prisoner's dilemma
			if(ColumnPlaysCooperateInRound[round]==1 & RowPlaysCooperateInRound[round]==1) {ColumnPDPayoff[round] <- b-c}
			if(ColumnPlaysCooperateInRound[round]==1 & RowPlaysCooperateInRound[round]==0) {ColumnPDPayoff[round] <- -c}
			if(ColumnPlaysCooperateInRound[round]==0 & RowPlaysCooperateInRound[round]==1) {ColumnPDPayoff[round] <- b}
			if(ColumnPlaysCooperateInRound[round]==0 & RowPlaysCooperateInRound[round]==0) {ColumnPDPayoff[round] <- 0}
			# weight the payoff by the probability of reaching that round
			ColumnPDPayoff[round] <- ColumnPDPayoff[round] * w ^ (round-1)	
		}
		# weight the last round payoff so that it covers all expected subsequent rounds
		RowHDPayoff[maxrounds] <- RowHDPayoff[maxrounds] / (1-w)
		RowPDPayoff[maxrounds] <- RowPDPayoff[maxrounds] / (1-w)
		
		# Save a record of the sequence of plays in each game as a string of 1s and 0s
		RowPlaysDove[i,j] <- paste(RowPlaysDoveInRound,sep="",collapse="")
		RowPlaysCooperate[i,j] <- paste(RowPlaysCooperateInRound,sep="",collapse="")
		ColumnPlaysDove[i,j] <- paste(ColumnPlaysDoveInRound,sep="",collapse="")
		ColumnPlaysCooperate[i,j] <- paste(ColumnPlaysCooperateInRound,sep="",collapse="")
		
		# Calculate the fraction of rounds that row plays dove and cooperate
		wgts <- 1:maxrounds - 1
		wgts <- w ^ wgts
		wgts[maxrounds] <- wgts[maxrounds] / (1-w)
		AvgRoundsRowPlaysDove[i,j] <- sum(RowPlaysDoveInRound * wgts)
		AvgRoundsRowPlaysCooperate[i,j] <- sum(RowPlaysCooperateInRound * wgts)
		
		# Save the payoffs in the payoff matrix
		PAYOFFS[i,j] <- sum(RowHDPayoff,RowPDPayoff)
		PAYOFFS[i,j]

	} # end of looping through j (column player) for this i
} # end of looping through i (row player) 

# Calculate the fractions of rounds that row plays dove and cooperate 
FractionRowPlaysDove <- AvgRoundsRowPlaysDove * (1-w)
FractionRowPlaysCooperate <- AvgRoundsRowPlaysCooperate * (1-w)

# Add baseline fitness
if(min(PAYOFFS) <= -BaselineFitness) {
	BaselineFitness <- -min(PAYOFFS) 
}
PAYOFFS <- PAYOFFS + BaselineFitness

# Done creating payoff matrix for this set of parameter values.

################################################

# Do one run for this set of parameter values

############################################
# Set starting frequencies -----------------
Frequencies <- rep(NA,dim(S)[1])

# We need a consistent conservative set of strategy frequencies - 100% hawkish defectors
antisocial <- which(S$DOVE==0 & S$COOPERATOR==0 & S$COMPLYTOCONDITIONALDOVE==0 & S$COMPLYTOLEVELLER==0 & S$COMPLYTOCONDITIONALCOOPERATOR==0)						
Frequencies[antisocial] <- startingfractionHawkDefect / length(antisocial)
Frequencies[-antisocial] <- (1-startingfractionHawkDefect) / (dim(S)[1]-length(antisocial))
Frequencies <- Frequencies / sum(Frequencies) # Renormalize in case they don't add to 1

# Model the replicator dynamics ---------------------

# Fitness holds the expected payoffs for each strategy
Fitness <- rep(NA,dim(S)[1])
						
for(iteration in 1:epoch) {
	# Record the frequencies for plotting
	HistoryOfFrequencies[iteration,] <- Frequencies
	
	# Calculate average payoffs for each strategy from the payoff matrix and frequencies
	for(i in 1:dim(S)[1]) {	Fitness[i] <- sum(Frequencies * PAYOFFS[i,])}
	
	# Calculate overall average payoff ----------
	MeanFitness <- sum(Frequencies * Fitness)
	
	# Calculate new post-selection frequencies
	NewFrequencies <-  Frequencies * Fitness / MeanFitness 
	
	# Allow mutations between strategy types
	transitions <- matrix(mutationprobability * Frequencies / dim(S)[1],nrow=dim(S)[1],ncol=dim(S)[1])
	Frequencies <- NewFrequencies + colSums(transitions) - rowSums(transitions)
	if(min(Frequencies)<0) {
		Frequencies[Frequences<0] <- 0
		Frequencies <- Frequencies / sum(Frequencies)
	}
	if(max(Frequencies)>1) {
		Frequencies[Frequences>1] <- 1
		Frequencies[Frequences<1] <- 0
	}
}	
		

################
################

# Now do a second run, with different parameter values

v <- vrange[2]
d <- drange[2]
b <- brange[2]
c <- crange[2]
w <- wrange[2]

# Compute payoff matrix------------------------------------------

# This is done once for each parameter combination, even if there are multiple runs.
# The payoff matrix PAYOFFS has one row per strategy, one column per strategy.
# It gives the payoff to the row strategy when facing the column strategy.
PAYOFFS <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])

# These record the sequence of Hawk/Dove and Cooperate/Defect moves 
# by the row strategy when facing the column strategy
RowPlaysDove <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])
RowPlaysCooperate <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])
ColumnPlaysDove <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])
ColumnPlaysCooperate <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])

# These record the average number of times row plays Dove and Cooperate
AvgRoundsRowPlaysDove <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])
AvgRoundsRowPlaysCooperate <- matrix(ncol=dim(S)[1],nrow=dim(S)[1])

# For each i row strategy and j column strategy in the strategy matrix
for(i in 1:dim(S)[1]) {
	for(j in 1:dim(S)[1]) {
		
		# How many rounds do we need to account for?  This number just needs to be high
		# enough for all transient behavioral interactions within pairs to settle down.
		maxrounds <- 10 
		
		# Create accounting variables to record 
		# Hawk-Dove & Cooperate-Defect behavior across rounds
		RowPlaysDoveInRound <- rep(NA, maxrounds)
		RowPlaysCooperateInRound <- rep(NA, maxrounds)		
		ColumnPlaysDoveInRound <- rep(NA, maxrounds)
		ColumnPlaysCooperateInRound <- rep(NA, maxrounds)
		
		# Create accounting variables to record
		# admonishment by punishing strategies
		RowConditionalDoveAdmonishes <- rep(0, maxrounds)
		RowLevellerAdmonishes <- rep(0, maxrounds)
		RowConditionalCooperatorAdmonishes <- rep(0, maxrounds)
		ColumnConditionalDoveAdmonishes <- rep(0, maxrounds)
		ColumnLevellerAdmonishes <- rep(0, maxrounds)
		ColumnConditionalCooperatorAdmonishes <- rep(0, maxrounds)
		
		# Create accounting variables to record payoffs
		RowHDPayoff <- rep(NA, maxrounds)
		RowPDPayoff <- rep(NA, maxrounds)
		ColumnHDPayoff <- rep(NA, maxrounds)
		ColumnPDPayoff <- rep(NA, maxrounds)
		
		# What is the behavior of row and column in the first and subsequent rounds?
		
		# 1st Round - Players have no info, they follow their 1st nature
		RowPlaysDoveInRound[1] <- S[i,]$DOVE 
		RowPlaysCooperateInRound[1] <- S[i,]$COOPERATOR 
		ColumnPlaysDoveInRound[1] <- S[j,]$DOVE 
		ColumnPlaysCooperateInRound[1] <- S[j,]$COOPERATOR 
		
		# Round 2 through maxrounds
		for (round in 2:maxrounds) {
			
			# Set the default behavior according to the basic propensities of the strategies
			RowPlaysDoveInRound[round] <- S[i,]$DOVE 
			RowPlaysCooperateInRound[round] <- S[i,]$COOPERATOR 
			ColumnPlaysDoveInRound[round] <- S[j,]$DOVE 
			ColumnPlaysCooperateInRound[round] <- S[j,]$COOPERATOR 
			
			# If row has been admonished by a conditional dove column and is compliant, row plays dove
			if (S[i,]$COMPLYTOCONDITIONALDOVE==1 & max(ColumnConditionalDoveAdmonishes[1:(round-1)])==1) {
				RowPlaysDoveInRound[round] <- 1
			}
			# If row has been admonished by a column leveller and is compliant, row plays dove
			if (S[i,]$COMPLYTOLEVELLER==1 & max(ColumnLevellerAdmonishes[1:(round-1)])==1) {
				RowPlaysDoveInRound[round] <- 1
			}
			
			# If row is a conditional dove and column played hawk last round, row maybe punishes with hawk
			if (S[i,]$CONDITIONALDOVE==1 & ColumnPlaysDoveInRound[round-1]==0) {
				# Column played hawk against a conditional dove row
				if (sum(RowConditionalDoveAdmonishes)==0) { 
					# Row hasn't admonished before
					# The conditional dove row plays hawk and admonishes
					RowPlaysDoveInRound[round] <- 0 
					RowConditionalDoveAdmonishes[round] <- 1
				} else {
					# Row has admonished as a conditional dove before
					if (RowConditionalDoveAdmonishes[round-1]==1 & sum(RowConditionalDoveAdmonishes)==1) { 
						# Row just admonished once, last round. Don't punish, as column
						# may come around this round based on last round's admonishment.
					} else { # Column has no excuse. Row punishes as a conditional dove.
						RowPlaysDoveInRound[round] <- 0
						RowConditionalDoveAdmonishes[round] <- 1
					}
				}
			}
			
			# Row may cooperate if is compliant to conditional cooperators
			if (S[i,]$COMPLYTOCONDITIONALCOOPERATOR==1 & max(ColumnConditionalCooperatorAdmonishes[1:(round-1)])==1) {
				# Row is compliant to conditional cooperators, and has been punished by column
				# in the past. Therefore row plays cooperate.
				RowPlaysCooperateInRound[round] <- 1
			}
			
			# If row is a conditional cooperator and column defected last round, row considers punishment
			if (S[i,]$CONDITIONALCOOPERATOR==1 & ColumnPlaysCooperateInRound[round-1]==0) {
				# If row is compliant to leveller and column admonished row as a leveller last round, don't treat it as a defection
				if (ColumnLevellerAdmonishes[round-1]==1 & S[i,]$COMPLYTOLEVELLER==1) {
					# Do nothing
				} else {										
					if (sum(RowConditionalCooperatorAdmonishes)==0) { 
						# Row hasn't punished column before. Row defects and admonishes.
						RowPlaysCooperateInRound[round] <- 0
						RowConditionalCooperatorAdmonishes[round] <- 1
					} else {
						if (RowConditionalCooperatorAdmonishes[round-1]==1 & sum(RowConditionalCooperatorAdmonishes)==1) { 
							# Row just admonished once, last round. Don't punish, as column
							# may come around this round based on last round's admonishment.
						} else { # Column has no excuse. Row punishes as a conditional cooperator.
							RowPlaysCooperateInRound[round] <- 0
							RowConditionalCooperatorAdmonishes[round] <- 1
						}
					}
				}
			}
			
			# If row is a leveller and column played dove last round, row considers punishment.
			if (S[i,]$LEVELLER==1 & ColumnPlaysDoveInRound[round-1]==0) {
				if (sum(RowLevellerAdmonishes)==0) { 
					# Row hasn't punished column before. Row defects and admonishes.
					RowPlaysCooperateInRound[round] <- 0
					RowLevellerAdmonishes[round] <- 1
				} else {
					if (RowLevellerAdmonishes[round-1]==1 & sum(RowLevellerAdmonishes)==1) { 
						# Row just admonished once, last round. Don't punish, as column
						# may come around this round based on last round's admonishment.
					} else { # Column has no excuse. Row punishes as a conditional cooperator.
						RowPlaysCooperateInRound[round] <- 0
						RowLevellerAdmonishes[round] <- 1
					}
				}
			}
			
			# If column complies to conditional dove and has been admonished, column plays dove
			if (S[j,]$COMPLYTOCONDITIONALDOVE==1 & max(RowConditionalDoveAdmonishes[1:(round-1)])==1) {
				ColumnPlaysDoveInRound[round] <- 1
			}
			# If column complies to leveller and has been admonished, column plays dove
			if (S[j,]$COMPLYTOLEVELLER==1 & max(RowLevellerAdmonishes[1:(round-1)])==1) {
				ColumnPlaysDoveInRound[round] <- 1
			}
			
			# If column is a conditional dove and row played hawk last round, column considers punishing
			if (S[j,]$CONDITIONALDOVE==1 & RowPlaysDoveInRound[round-1]==0) {
				if (sum(ColumnConditionalDoveAdmonishes)==0) {
					# Column hasn't punished row before. Column defects and admonishes.
					ColumnPlaysDoveInRound[round] <- 0
					ColumnConditionalDoveAdmonishes[round] <- 1
				} else {
					if (ColumnConditionalDoveAdmonishes[round-1]==1 & sum(ColumnConditionalDoveAdmonishes)==1) { 
						# Column just admonished once, last round. Don't punish, as row
						# may come around this round based on last round's admonishment.
					} else { # Row has no excuse. Row punishes as a conditional dove..
						ColumnPlaysDoveInRound[round] <- 0
						ColumnConditionalDoveAdmonishes[round] <- 1
					}
				}
			}
			
			# If column complies to conditional cooperators and has been admonished, column cooperates
			if (S[j,]$COMPLYTOCONDITIONALCOOPERATOR==1 & max(RowConditionalCooperatorAdmonishes[1:(round-1)])==1) {
				ColumnPlaysCooperateInRound[round] <- 1
			}
			
			# If column is a conditional cooperator and row defects, column considers punishment
			if (S[j,]$CONDITIONALCOOPERATOR==1 & RowPlaysCooperateInRound[round-1]==0) {
				# If column is compliant to leveller and row admonished column as a leveller last round, don't treat it as a defection
				if (RowLevellerAdmonishes[round-1]==1 & S[j,]$COMPLYTOLEVELLER==1) {
					# Do nothing
				} else {
					if (sum(ColumnConditionalCooperatorAdmonishes)==0) { 
						# Column hasn't punished row before. Column defects and admonishes.
						ColumnPlaysCooperateInRound[round] <- 0
						ColumnConditionalCooperatorAdmonishes[round] <- 1
					} else {
						if (ColumnConditionalCooperatorAdmonishes[round-1]==1 & sum(ColumnConditionalCooperatorAdmonishes)==1) { 
							# Column just admonished once, last round. Don't punish, as row
							# may come around this round based on last round's admonishment.
						} else { # Row has no excuse. Row punishes as a conditional coopreator.
							ColumnPlaysCooperateInRound[round] <- 0
							ColumnConditionalCooperatorAdmonishes[round] <- 1
						}
					}
				}
			}
			
			# If column is a leveller and row played hawk last round, column considers punishment
			if (S[j,]$LEVELLER==1 & RowPlaysDoveInRound[round-1]==0) {
				if (sum(ColumnLevellerAdmonishes)==0) { 
					# Column hasn't punished row before. Column defects and admonishes.
					ColumnPlaysCooperateInRound[round] <- 0
					ColumnLevellerAdmonishes[round] <- 1
				} else {
					if (ColumnLevellerAdmonishes[round-1]==1 & sum(ColumnLevellerAdmonishes)==1) { 														# Column just admonished once, last round. Don't punish, as row
						# may come around this round based on last round's admonishment.
					} else { # Row has no excuse. Row punishes as leveller.
						ColumnPlaysCooperateInRound[round] <- 0
						ColumnLevellerAdmonishes[round] <- 1
					}
				}
			}
		}
		
		# Add up payoffs -----------
		for (round in 1:maxrounds) {
			# Row payoff from hawk-dove
			if(RowPlaysDoveInRound[round]==1 & ColumnPlaysDoveInRound[round]==1) {RowHDPayoff[round] <- v/2}
			if(RowPlaysDoveInRound[round]==1 & ColumnPlaysDoveInRound[round]==0) {RowHDPayoff[round] <- 0}
			if(RowPlaysDoveInRound[round]==0 & ColumnPlaysDoveInRound[round]==1) {RowHDPayoff[round] <- v}
			if(RowPlaysDoveInRound[round]==0 & ColumnPlaysDoveInRound[round]==0) {RowHDPayoff[round] <- (v-d)/2}
			# weight the payoff by the probability of reaching that round
			RowHDPayoff[round] <- RowHDPayoff[round] * w ^ (round-1) 
			
			# Row payoff from prisoner's dilemma
			if(RowPlaysCooperateInRound[round]==1 & ColumnPlaysCooperateInRound[round]==1) {RowPDPayoff[round] <- b-c}
			if(RowPlaysCooperateInRound[round]==1 & ColumnPlaysCooperateInRound[round]==0) {RowPDPayoff[round] <- -c}
			if(RowPlaysCooperateInRound[round]==0 & ColumnPlaysCooperateInRound[round]==1) {RowPDPayoff[round] <- b}
			if(RowPlaysCooperateInRound[round]==0 & ColumnPlaysCooperateInRound[round]==0) {RowPDPayoff[round] <- 0}
			# weight the payoff by the probability of reaching that round
			RowPDPayoff[round] <- RowPDPayoff[round] * w ^ (round-1)
			
			# Column payoff from hawk-dove
			if(ColumnPlaysDoveInRound[round]==1 & RowPlaysDoveInRound[round]==1) {ColumnHDPayoff[round] <- v/2}
			if(ColumnPlaysDoveInRound[round]==1 & RowPlaysDoveInRound[round]==0) {ColumnHDPayoff[round] <- 0}
			if(ColumnPlaysDoveInRound[round]==0 & RowPlaysDoveInRound[round]==1) {ColumnHDPayoff[round] <- v}
			if(ColumnPlaysDoveInRound[round]==0 & RowPlaysDoveInRound[round]==0) {ColumnHDPayoff[round] <- (v-d)/2}
			# weight the payoff by the probability of reaching that round
			ColumnHDPayoff[round] <- ColumnHDPayoff[round] * w ^ (round-1)
			
			# Column payoff from prisoner's dilemma
			if(ColumnPlaysCooperateInRound[round]==1 & RowPlaysCooperateInRound[round]==1) {ColumnPDPayoff[round] <- b-c}
			if(ColumnPlaysCooperateInRound[round]==1 & RowPlaysCooperateInRound[round]==0) {ColumnPDPayoff[round] <- -c}
			if(ColumnPlaysCooperateInRound[round]==0 & RowPlaysCooperateInRound[round]==1) {ColumnPDPayoff[round] <- b}
			if(ColumnPlaysCooperateInRound[round]==0 & RowPlaysCooperateInRound[round]==0) {ColumnPDPayoff[round] <- 0}
			# weight the payoff by the probability of reaching that round
			ColumnPDPayoff[round] <- ColumnPDPayoff[round] * w ^ (round-1)	
		}
		# weight the last round payoff so that it covers all expected subsequent rounds
		RowHDPayoff[maxrounds] <- RowHDPayoff[maxrounds] / (1-w)
		RowPDPayoff[maxrounds] <- RowPDPayoff[maxrounds] / (1-w)
		
		# Save a record of the sequence of plays in each game as a string of 1s and 0s
		RowPlaysDove[i,j] <- paste(RowPlaysDoveInRound,sep="",collapse="")
		RowPlaysCooperate[i,j] <- paste(RowPlaysCooperateInRound,sep="",collapse="")
		ColumnPlaysDove[i,j] <- paste(ColumnPlaysDoveInRound,sep="",collapse="")
		ColumnPlaysCooperate[i,j] <- paste(ColumnPlaysCooperateInRound,sep="",collapse="")
		
		# Calculate the fraction of rounds that row plays dove and cooperate
		wgts <- 1:maxrounds - 1
		wgts <- w ^ wgts
		wgts[maxrounds] <- wgts[maxrounds] / (1-w)
		AvgRoundsRowPlaysDove[i,j] <- sum(RowPlaysDoveInRound * wgts)
		AvgRoundsRowPlaysCooperate[i,j] <- sum(RowPlaysCooperateInRound * wgts)
		
		# Save the payoffs in the payoff matrix
		PAYOFFS[i,j] <- sum(RowHDPayoff,RowPDPayoff)
		PAYOFFS[i,j]

	} # end of looping through j (column player) for this i
} # end of looping through i (row player) 

# Calculate the fractions of rounds that row plays dove and cooperate 
FractionRowPlaysDove <- AvgRoundsRowPlaysDove * (1-w)
FractionRowPlaysCooperate <- AvgRoundsRowPlaysCooperate * (1-w)

# Add baseline fitness
if(min(PAYOFFS) <= -BaselineFitness) {
	BaselineFitness <- -min(PAYOFFS) 
}
PAYOFFS <- PAYOFFS + BaselineFitness

# Done creating payoff matrix for this second set of parameter values.

################################################

# Do one run for this second set of parameter values

############################################
# Starting frequencies carrying over from the end of the previous run -----------------

# Model the replicator dynamics ---------------------

# Fitness holds the expected payoffs for each strategy
Fitness <- rep(NA,dim(S)[1])
						
for(iteration in 1:epoch) {
	# Record the frequencies for plotting
	HistoryOfFrequencies[iteration+epoch,] <- Frequencies
	
	# Calculate average payoffs for each strategy from the payoff matrix and frequencies
	for(i in 1:dim(S)[1]) {	Fitness[i] <- sum(Frequencies * PAYOFFS[i,])}
	
	# Calculate overall average payoff ----------
	MeanFitness <- sum(Frequencies * Fitness)
	
	# Calculate new post-selection frequencies
	NewFrequencies <-  Frequencies * Fitness / MeanFitness 
	
	# Allow mutations between strategy types
	transitions <- matrix(mutationprobability * Frequencies / dim(S)[1],nrow=dim(S)[1],ncol=dim(S)[1])
	Frequencies <- NewFrequencies + colSums(transitions) - rowSums(transitions)
	if(min(Frequencies)<0) {
		Frequencies[Frequences<0] <- 0
		Frequencies <- Frequencies / sum(Frequencies)
	}
	if(max(Frequencies)>1) {
		Frequencies[Frequences>1] <- 1
		Frequencies[Frequences<1] <- 0
	}
}

################
################
################
################

# Calculate fraction of different strategy types in the populations
FractionDove <- rowSums(HistoryOfFrequencies[,which(S$DOVE==1)])
FractionCooperator <- rowSums(HistoryOfFrequencies[,which(S$COOPERATOR==1)])
FractionLeveller <- rowSums(HistoryOfFrequencies[,which(S$LEVELLER==1)])
FractionConditionalCooperator <- rowSums(HistoryOfFrequencies[,which(S$CONDITIONALCOOPERATOR==1)])
FractionConditionalDove <- rowSums(HistoryOfFrequencies[,which(S$CONDITIONALDOVE==1)])
FractionComplyToLeveller <- rowSums(HistoryOfFrequencies[,which(S$COMPLYTOLEVELLER==1)])
FractionComplyToConditionalCooperator <- rowSums(HistoryOfFrequencies[,which(S$COMPLYTOCONDITIONALCOOPERATOR==1)])
FractionComplyToConditionalDove <- rowSums(HistoryOfFrequencies[,which(S$COMPLYTOCONDITIONALDOVE==1)])

################
################
################

# Plot the change in frequencies right around the parameter shift ###########

# Create a new plot space
dev.new(width=8.5, height=3.75)
par(mar=c(5,4,3,1))
par(oma=c(0,0,0,0))

# Plot the histories
plot(c(1,1),c(0,1),xlim=xminmax,col="white", xlab=expression(italic("t")), ylab="frequency", yaxt='n')
points(time, 1-FractionDove,type="l",col="blue",lwd=3)
points(time, FractionCooperator,type="l",col="darkorange",lwd=3)
points(time, FractionConditionalCooperator, type="l",col="darkorange",lty=1)
points(time, FractionComplyToConditionalCooperator, type="l",lty=3,lwd=1,col="darkorange")
points(time, FractionLeveller, type="l",col="green4",lwd=4,lty=1)
points(time, FractionComplyToLeveller, type="l",lty=3,lwd=1,col="green4")
axis(2,at=c(0,1))
abline(v=0)
arrows(xminmax[1]-10,1.125,-1,1.125,xpd=TRUE,code=3,length=0.1)
mtext("Low gains to cooperation",side=3,outer=TRUE,line=-0.9,at=0.25,cex=0.8,padj=2) 
arrows(1,1.125,xminmax[2]+10,1.125,xpd=TRUE,code=3,length=0.1)
mtext("High gains to cooperation",side=3,outer=TRUE,line=-0.9,at=0.7,cex=0.8,padj=2) 
legend(xminmax[1]-7.5,0.85,legend=c("Hawk","Cooperator","Conditional Cooperator","Conditional Defector","Leveller","Acquiescent Hawk"),col=c("blue","darkorange","darkorange","darkorange","green4","green4"),lwd=c(3,3,1,1,4,1),lty=c(1,1,1,3,1,3),bty="n",cex=0.75)

