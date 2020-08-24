# Gibbs sampler #

# INPUT:
# compdata: data.frame, each column is a factor, the levels should be ordered from 1 to the number of levels
# The columns compdata$i, compdata$j have to organize the pairs in lexicographic order.
# niter: number of iterations of Gibbs sampler
# alpha1,beta1,alpha0,beta0: vectors with parameters for the beta priors.  If NULL, a flat prior is taken.
# plb1: prior lower bounds for binary comparisons
# aBM=1,bBM: hyperparameters of beta prior on bipartite matchings
# seed: if 0 (default) no seed is set for simulation, otherwise the number is taken as the seed
# burnin: number of iterations to burn-in

BRLGibbs_BetaMatching_PaperSimulation <- function(compdata,niter=500,alpha1=NULL,beta1=NULL,plb1=NULL,alpha0=NULL,beta0=NULL,aBM=1,bBM=1,seed=0,burnin=1){
	
	if(seed) set.seed(seed)
	
	compdata$i <- as.numeric(compdata$i)
	compdata$j <- as.numeric(compdata$j)
	
	npairs <- dim(compdata)[1]
	
	possiblepairs <- compdata[,c("i","j")]
	
	n1 <- max(possiblepairs$i)
	n2 <- max(possiblepairs$j) - n1
	
	compdata$i <- NULL
	compdata$j <- NULL
	
	nAgrLevels <- sapply(compdata,FUN=nlevels)
	F <- length(nAgrLevels)
	lstlevfield <- cumsum(nAgrLevels)
	fstlevfield <- c(1,lstlevfield[-F]+1)
	fieldInd <- rep(seq_len(F),nAgrLevels)
	fieldIndParam <- fieldInd[-lstlevfield]
	nBetaPriors <- sum(nAgrLevels) - F
	
	plb1 <- unlist(plb1)
	
	# next four lines compute the binary indicators from the agreement levels
	xfun <- function(f) paste("(compdata[,",f,"]==",seq_len(nAgrLevels[f]),")")
	expr1 <- paste(sapply(sapply(seq_len(F),xfun),FUN=paste,collapse=","),collapse=",")
	expr2 <- paste("cbind(",expr1,")")
	BinAgrLevels <- eval(parse(text=expr2))
	
	BinAgrLevels[is.na(BinAgrLevels)] <- FALSE # replacing NAs by FALSE or zeroes is justified by ignorability and CI assumptions (see paper)
	nBinAgrLevels <- dim(BinAgrLevels)[2]
	sumsBinAgrLevels <- colSums(BinAgrLevels)
	
	Z <- matrix(NA,n2,niter)
	m <- matrix(NA,nBetaPriors,niter)
	u <- matrix(NA,nBetaPriors,niter)
	
	# Z0 doesn't link any records
	Znew <- n1:(n2+n1-1)
	freelinks <- rep(1,n1)
	
	# random draw from beta truncated to the interval [a,1]
	dyn.load("rtbetaC.so") # has to be in working directory
	
	rtruncbeta <- function(alpha=2,beta=500,a=.5,iter=100){
		.C("rtbetaC", alpha = as.double(alpha), beta = as.double(beta),
			a = as.double(a), iter = as.integer(iter), x = as.double(0) )$x
	}
	
	dyn.load("GibbsInnerIter4.so")
	
	InnerIter <- function(Znew, freelinks, Lambda){
		out <- .C("InnerIterBRL_BetaMatching_SimPaper", Znew=as.integer(Znew), freelinks=as.integer(freelinks),
					n2=as.integer(length(Znew)), n1=as.integer(length(freelinks)),
					nfreelinks=as.integer(sum(freelinks)), Lambda=as.double(Lambda),
					aBM=as.double(aBM), bBM=as.double(bBM))
		return(list(Znew=out$Znew, freelinks=out$freelinks))
	}
	
	for (iter in seq_len(niter)){
		
		recinfile2withlinks <- which(Znew < n1)
		recinfile1withlinks <- Znew[Znew < n1] + 1
		linked <- recinfile1withlinks + n1*(recinfile2withlinks-1)
		
		A1 <- simplify2array( lapply(seq_len(nBinAgrLevels), FUN=function(fl) sum(BinAgrLevels[linked,fl]) ) )
		A0 <- sumsBinAgrLevels - A1 
		
		revcumsumA1 <- unlist( lapply(split(A1,fieldInd), FUN=function(x) rev(cumsum(rev(x)))) )
		revcumsumA0 <- unlist( lapply(split(A0,fieldInd), FUN=function(x) rev(cumsum(rev(x)))) )
		
		A1 <- A1[-lstlevfield]
		A0 <- A0[-lstlevfield]
		
		revcumsumA1 <- revcumsumA1[-fstlevfield]
		revcumsumA0 <- revcumsumA0[-fstlevfield]
		
		ma <- A1 + alpha1
		mb <- revcumsumA1 + beta1
		ua <- A0 + alpha0
		ub <- revcumsumA0 + beta0
		
		mnewArguments <- as.matrix(cbind(ma,mb,plb1))
		mnewArgumentsList <- split(mnewArguments,seq_len(nBetaPriors))
		mnew <- simplify2array( lapply(mnewArgumentsList,FUN=function(x) rtruncbeta(alpha=x[1],beta=x[2],a=x[3],iter = 100) ) )
		
		unew <- rbeta(n=nBetaPriors, shape1=ua, shape2=ub)
		
		mstarnew <- unlist( lapply( split(mnew,fieldIndParam), FUN = function(x){ c(log(x),0) + c(0,cumsum(log(1-x))) } ) )
		ustarnew <- unlist( lapply( split(unew,fieldIndParam), FUN = function(x){ c(log(x),0) + c(0,cumsum(log(1-x))) } ) )
		
		mminusu <- mstarnew - ustarnew
		
		Lambda <- BinAgrLevels %*% mminusu
		
		Iter_Z <- InnerIter(Znew, freelinks, Lambda)
		
		freelinks <- Iter_Z$freelinks
		Znew <- Iter_Z$Znew 
		
		Z[,iter] <- Znew 
		m[,iter] <- mnew 
		u[,iter] <- unew 
	}
	
	m <- m[,-seq_len(burnin)]
	u <- u[,-seq_len(burnin)]
	
	m_mean <- rowMeans(m)
	u_mean <- rowMeans(u)
	
	logmstar_mean <- unlist( lapply( split(m_mean,fieldIndParam), FUN = function(x){ c(log(x),0) + c(0,cumsum(log(1-x))) } ) )
	logustar_mean <- unlist( lapply( split(u_mean,fieldIndParam), FUN = function(x){ c(log(x),0) + c(0,cumsum(log(1-x))) } ) )
		
	logmminuslogu <- logmstar_mean - logustar_mean
		
	Lambda <- BinAgrLevels %*% logmminuslogu
	
	return(list(Z=Z+1,m=m,u=u,Lhat=Lambda))
}
