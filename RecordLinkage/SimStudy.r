# Simulation using Australian demographic characteristics
# Simulated datafiles obtained using Peter Christen's code

# Bipartite record linkage with overlap of 10%

rm(list=ls())

source("Gibbs.R")

library(RecordLinkage)
library(clue)

# function computes agreement levels for binary comparisons, used inside for
AgrLevBinComp <- function(x){
	same <- (x[cellinds[,1]]==x[cellinds[,2]])
	AgrLev <- 1*same
	AgrLev[!same] <- 2
	AgrLev <- as.factor(AgrLev)
	return(AgrLev)
}

# function computes agreement levels for Levenshtein comparisons, used inside for
AgrLevLevenshtein <- function(x,breaks=c(-Inf,.001,.25,.5,Inf)){
	x <- as.character(x)
	LevenshteinSim <- 1 - levenshteinSim(x[cellinds[,1]], x[cellinds[,2]])
	AgrLev <- cut(LevenshteinSim,breaks=breaks,labels=seq_len(length(breaks)-1))
	return(AgrLev)
}

# This implementation uses pieces of code of Sadinle (2014, AOAS)
# which used sequential conditional probabilities as the model parameters.  
# The following lines correspond to specifying flat priors for the U and M parameters
# in the paper "Bayesian Estimation of Bipartite Matchings for Record Linkage"
# Fields: Gname, Fname, Age, Occup
plb1 <- list(rep(0,3),rep(0,3),rep(0,1),rep(0,1))
beta0 <- beta1 <- c(3:1,3:1,1,1)
alpha0 <- alpha1 <- c(rep(1,3),rep(1,3),rep(1,1),rep(1,1))

niter <- 1000
burnin <- niter*0.1

system.time(
for(n_errors in c(1:3)){
	for(dataset in 0:99){
	
		records <- read.csv(paste("percdups50_nerrors",n_errors,"_dataset",dataset,"_4fields_twofiles_BRLpaper.csv",sep=""))
		
		records$file <- rep(2:1, length.out = dim(records)[1])
		
		records <- records[records$file %in% c(1,2),] # pick original record and first dup
		
		#### pre processing
		
		# get entity ID
		records$rec.id <- gsub("^rec-", "", records$rec.id)
		records$rec.id <- as.numeric(gsub("-[[:alnum:]]+", "", records$rec.id))
		
		# identify which records will actually belong to each sample
		records$samp1 <- records$rec.id < 500
		records$samp2 <- records$rec.id < 50
		records$samp2[(records$rec.id >= 500)&(records$rec.id < 950)] <- TRUE
		# the above three lines can be modified to obtain other %s of overlap
		# in the paper we also use 50% and 100%
		
		# drop extra records
		records <- records[records$samp1 | records$file == 2,]
		records <- records[records$samp2 | records$file == 1,]
        
		# don't need these two columns any more
		records$samp1 <- NULL
		records$samp2 <- NULL

		records <- records[order(records$file),]

		n <- dim(records)[1]
		
		n1 <- sum(records$file==1)
		n2 <- sum(records$file==2)

		cellinds <- expand.grid(1:n1,(n1+1):(n1+n2))
		
		# computing agreement levels for binary comparisons
		
		AgrLevAge <- AgrLevBinComp(records$age)
		AgrLevOccup <- AgrLevBinComp(records$occup)
		
		# computing agreement levels for comparisons based on Levenshtein
		AgrLevGname <- AgrLevLevenshtein(records$gname)
		AgrLevFname <- AgrLevLevenshtein(records$fname)
		
		compdata <- data.frame(i=cellinds[,1],j=cellinds[,2],GName=AgrLevGname,FName=AgrLevFname,Age=AgrLevAge,Occup=AgrLevOccup)
		
		TrueMatches <- records$rec.id[cellinds[,1]]==records$rec.id[cellinds[,2]]

		possiblelinks <- compdata[,c("i","j")]
		
		chain <- BRLGibbs_BetaMatching_PaperSimulation(compdata, niter=niter, 
		 alpha1=alpha1,beta1=beta1,plb1=plb1[1:4],alpha0=alpha0,beta0=beta0,aBM=1,bBM=1,seed=50,burnin=burnin) 
				
		# Save chain of Z's
		write.table(chain$Z[,burnin:niter],paste("Zchain_overlap10_nerrors",n_errors,"_dataset",dataset,".txt",sep=""),row.names = FALSE,col.names=FALSE)

	}# end 'for' of dataset number
}# end 'for' of number of errors
)
