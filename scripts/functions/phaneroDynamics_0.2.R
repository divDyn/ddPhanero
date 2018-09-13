#' ---
#' title: "Functions to analyze Phanerozoic-scale time series of diversity dynamics"
#' author: "Ádám T. Kocsis"
#' date: "September 3rd, 2018"
#' ---

#' Summarizing the output of the analyzeMetrics() function
#' 
#' The purpose of this function is to loop through a bunch of object entries (lists), extract and organize the information within their elements.
#' 
#' @param entries (\code{list}): Names within the R global environment that are outputs of the analyzeMetrics function.
#' @param pos (\code{character}): Positive test results will be indicated with this character string.
#' @param neg (\code{character}): Negative test results will be indicated with this character string.
#' @param alpha (\code{numeric}): Used alpha-level of significance, defaults to 0.05.
summarize<-function(entries,pos="+", neg="-", alpha=0.05){
	listObj <- sapply(entries, function(x){
		list(get(x))
	})
	
	# first
	puOrCo<-matrix(neg, ncol=length(entries), nrow=2)
	colnames(puOrCo) <- entries
	rownames(puOrCo)  <- c("pulsed models used for extinctions", "pulsed models used for originations")
	
	for(i in 1:length(entries)){
		listElem <- listObj[[i]]
		temp<-listElem[["whichModel is indicated, pulsed or continuous"]]
		if(temp[1,1]) puOrCo[1,i]<- pos
		if(temp[1,2]) puOrCo[2,i]<-pos
	}
	
	
	# decline
	# potential duration influnce
	decline<-matrix(neg, ncol=length(entries), nrow=4)
	colnames(decline) <- entries
	rownames(decline)  <- c("extinction series decline", "extinction series decline (post-ord)",
		"origination series decline", "origination series decline (post-ord)")
	for(i in 1:length(entries)){
		listElem <- listObj[[i]]
		temp<-listElem[["declines"]]
		for(j in 1:4) if(temp[j,2]<=alpha) decline[j,i] <-pos
	}
	
	
	# highest mass extinction
	me <- NULL
	for(i in 1:length(listObj)){
		me<-c(me,listObj[[i]][["largest extinction"]])
	}
	mes<-matrix(NA, ncol=length(me), nrow=1)
	mes[1,] <- me
	colnames(mes) <- entries
	rownames(mes) <- "largest ext. value"
	
	# outliers
	# all outlier entries
	out <- NULL
	for(i in 1:length(listObj)){
		out<-unique(c(out,listObj[[i]][["extinction outliers (boxplot)"]]))
	}
	
	
	outliers<- matrix(FALSE, ncol=length(entries), nrow=length(out))
	colnames(outliers) <- entries
	rownames(outliers) <- paste(out)
	
	for(i in 1:length(listObj)){
		temp <- listObj[[i]][["extinction outliers (boxplot)"]]
		outliers[temp,i]  <- TRUE
	}
	
	rownames(outliers) <- paste(out, "detected as ME")
	
	outliers2<-outliers
	outliers2[outliers]<-pos
	outliers2[!outliers]<-neg
	
	
	
	# log-normality
	logNorm<-matrix(pos, ncol=length(entries), nrow=2)
	colnames(logNorm) <- entries
	rownames(logNorm)  <- c("lognormally distributed extinctions", "lognormally distributed originations")
	for(i in 1:length(entries)){
		listElem <- listObj[[i]]
		temp<-listElem[["log-normality (Shapiro-Wilk p-value)"]]
		for(j in 1:2) if(temp[j]<=alpha) logNorm[j,i] <-neg
	}
	pair<- c(
		"divPlus", "ori",
		"div", "extPlus",
		"ext", "oriPlus",
		"ext", "extPlus",
		"ori", "oriPlus"
	)
	
	pair <- matrix(pair, ncol=2, byrow=T)
	
	dyna <- matrix(neg, ncol=length(entries), nrow=nrow(pair))
	rownames(dyna) <- apply(pair, 1, paste, collapse=", ")
	colnames(dyna) <- entries
	
	for(i in 1:length(entries)){
		listElem <- listObj[[i]]
		temp<-listElem[["dyn"]]
		
		for(j in 1:nrow(dyna)){
			temp2 <-temp
			
			# count only the positive correlations in the first 3 cases
			if(j%in%1:3) temp2 <- temp2[temp2[, "estimate"]>=0,]
		
			for(g in 1:nrow(temp2)){
				if(sum(pair[j,]%in%as.character(temp2[g,1:2]))==2) dyna[j,i] <- pos
			}
		}
	
	}
	
	
	final<-rbind(puOrCo,decline,mes,outliers2,logNorm, dyna)
	
	return(final)
}

#' Analysis function of Phanerozoic-scale diversity dynamics
#' 
#' This function outputs results regarding the 'pulsed' vs. continuous models of turnover,
#' the decline of the rates, the presence of mass extinctions and implications of equilibrial dynamics.ű
#' 
#' The analyis for the decline is between 540 and 20 million years. Detrended series are used betwen 475 and 20 million years. The post-Ordovician part starts at 443 million years.
#' 
#' @param dat (\code{data.frame}): Data table of the time series/variables to be analyzed.
#' @param ext {\code{character}}: Column name of the extinction rate variable.
#' @param ori {\code{character}}: Column name of the origination rate variable.
#' @param div {\code{character}}: Column name of the richness variable.
#' @param age {\code{character}}: Column name of mid ages of the time bins.
#' @param name {\code{character}}: Column name of the names of the bins (from the stages or bins) objects.
#' @param dur {\code{character}}: Column name of the interval durations.
#' @param normalize {\code{logical}}: If set to \code{TRUE} then the rate values will be normalized with the bin durations.
#' 	if it is indicated by correlations between the rate values and the interval durations. If set to \code{FALSE}, 
#' 	the input rate values will be used regardless of the potential effects of bin durations.
#' @param plot {\code{logical}}: If set to \code{TRUE} then a four panel plot is produced, showing the input time series and the detrending.
#' @param feedback {\code{logical}}: If set to \code{FALSE}, all warnings and messages are disabled. Warnings might arise due to ties in correlation tests, or after logging 0s.
analyzeMetrics<-function(dat, ext, ori, div, age, name, dur, normalize=FALSE, plot=T, feedback=T){
	if(!feedback){
		options(warn=-1)
	}

	# should the pulsed or continuous model be used???
	pulsed <- matrix(TRUE, ncol=2, nrow=2)
	colnames(pulsed) <- c("extinction", "origination")
	rownames(pulsed) <- c("supplied (pulsed) used", "pulsed used because conflict ")
	pulsed[2,] <- FALSE
	
	if(normalize){
		extPuCo<-pulseCont(dat[, ext], dur=dat[,dur])$sig
		if(length(extPuCo)==1){
			if(names(extPuCo)=="not-normalized"){
				dat[, ext] <-dat[,ext]/dat[, dur] *mean(dat[,dur], na.rm=T)
				if(feedback) message("Normalization of extinctions is indicated, the values in the entry table changed!")
				pulsed[1,1]<-FALSE
			}
		}
		if(length(extPuCo)==2) pulsed[2,1]<-TRUE
		
		oriPuCo<-pulseCont(dat[, ori], dur=dat[,dur])$sig
		if(length(oriPuCo)==1){
			if(names(oriPuCo)=="not-normalized"){
				dat[, ori] <-dat[,ori]/dat[, dur] *mean(dat[,dur], na.rm=T)
				if(feedback) message("Normalization of originations was indicated, the values in the entry table changed!")
				pulsed[1,2]<-FALSE
			}
		}
		if(length(oriPuCo)==2) pulsed[2,2]<-TRUE
		
	}
	
	ext <- dat[,ext]
	ori <- dat[,ori]
	div <- dat[,div]
	age <- dat[,age]
	name <- dat[,name]
	dur <- dat[,dur]
	
	
	# boundaries for various problems
	maxAgeDecline <- 540
	minAgeDecline <- 20
	
	maxAgeDetrend <- 475
	minAgeDetrend <- 20
	
	postOrdov <- 443
	
	#logical to see what is necessary
	# Cambrian included 
	indNAlong<- !(age<=maxAgeDecline & age>=minAgeDecline)
	
	# detrending interval
	indNAshort<- !(age<=maxAgeDetrend & age>=minAgeDetrend)
	
	# post ordovician
	indNApostord <- !(age<=postOrdov & age>=minAgeDecline)
		
	extVarLong <- ext
	oriVarLong <- ori
	
	extVarLong[indNAlong]<-NA
	oriVarLong[indNAlong]<-NA

	
	# plot all three variables
	nMaxRate<-max(c(extVarLong, oriVarLong), na.rm=T)
	
	# plot the original turnover rates
	if(plot){
		par(mfrow=c(2,2))
		par(mar=c(1, 4.1,4.1,2.1))
		tsplot(stages,  shading="series", ylim=c(0, nMaxRate*1.05), ylab="Turover rates", xlim=c(530,0), plot.args=list(axes=F), xlab="")
		axis(2)
		lines(age, extVarLong, col="red")
		lines(age, oriVarLong, col="blue")
		legend("topright", legend=c("extinction", "origination"), col=c("red", "blue"), lwd=c(1,1), bg="white", inset=c(0.01, 0.01))
	}


	# declin in the extinction rate
	# create a table from the decline-information
	decMat <-matrix(NA, ncol=2, nrow=4)
	colnames(decMat)<- c("correlation","p-value")
	rownames(decMat)<- c("extinction", "post-Ordovician extinction", "origination", "post-Ordovician origination")
	
	# is the extinction rate declining
	extDecline <- cor.test(age, extVarLong, method="spearman")
	decMat[1,1]<- extDecline$estimate
	decMat[1,2]<- extDecline$p.value
	
	# is the origination rate declining
	oriDecline <- cor.test(age, oriVarLong, method="spearman")
	decMat[3,1]<- oriDecline$estimate
	decMat[3,2]<- oriDecline$p.value
	
	# are the rates declining in the post ordovician?
	# extinction
	extPostOrd <- extVarLong
	extPostOrd[indNApostord] <- NA
	extDeclinePostOrd <- cor.test(age, extPostOrd, method="spearman")
	
	decMat[2,1]<- extDeclinePostOrd$estimate
	decMat[2,2]<- extDeclinePostOrd$p.value
	
	# origination
	oriPostOrd <- oriVarLong
	oriPostOrd[indNApostord] <- NA
	oriDeclinePostOrd <- cor.test(age, oriPostOrd, method="spearman")
	
	decMat[4,1]<- oriDeclinePostOrd$estimate
	decMat[4,2]<- oriDeclinePostOrd$p.value
	
	# detrending the turnover rates
		logExt <- log(extVarLong)
		logOri <- log(oriVarLong)
		
		
		logExt[is.infinite(logExt)] <- NA
		logOri[is.infinite(logOri)] <- NA
		
		# model based on the logged data, can be some estimation problems but this doesn't influence short-term correlation patterns
		extMod<-lm(logExt ~ age)
		oriMod<-lm(logOri ~ age)
		
		# predicted values (exponentiated back)
		predExt <- exp(predict(extMod, newdata=data.frame(age=age)))
		predOri <- exp(predict(oriMod, newdata=data.frame(age=age)))
		
		if(plot){
			lines(age, predExt,col="red", lwd=2)
			lines(age, predOri,col="blue", lwd=2)
		}

		# residuals of the multiplicative model
		detExt <- extVarLong/predExt
		detOri <- oriVarLong/predOri
		
		# limit the data further to the ordovician
			detExtShort <- detExt
			detOriShort <- detOri
			detExtShort[indNAshort] <- NA
			detOriShort[indNAshort] <- NA
		
	# largest peak
	
	repVals<-list(
		"end-Permian"=c("Permian 4","Changhsingian"),
		"end-Triassic"=c("Triassic 4","Rhaetian"),
		"end-Cretaceous"=c("Cretaceous 8","Maastrichtian"),
		"end-Devonian"= c("Devonian 4", "Famennian"),
		default="other"
	)
		
		largest <- name[which(max(detExtShort,  na.rm=T)==detExtShort)]
		largest<-categorize(largest, repVals)
		
	# outliers
		boxp<- boxplot(detExtShort, plot=F)
		outliers <- name[as.numeric(names(boxp$out))]

		outliers <- categorize(outliers, repVals)
	
	# logged data are normal - extinction
		extVar <-log(detExtShort)
		extVar[is.infinite(extVar)] <- NA
		
		pShap<- shapiro.test(extVar)$p.value
		names(pShap)[1] <- "ext"
	
	# logged data are normal - origination
		oriVar <-log(detOriShort)
		oriVar[is.infinite(oriVar)] <- NA
		
		pShap<-c(pShap,shapiro.test(oriVar)$p.value)
		names(pShap)[2] <- "ori"

	# get a diversity series
		# select the same interval as for turnover rates
		divVarLong<-div
		divVarLong[indNAlong]<-NA
		
		#1. logging the data
		logDiv <- log(divVarLong)
		
		# quadratic regression 
		divModQuad <- lm(logDiv ~ poly(age,2))
		
		# taking the residuals
			logPredict<- predict(divModQuad, newdata=data.frame("age"=age))
			
			# the residuals
			logResid <- logDiv-logPredict
			
		# adding back the mean
			logResidMean<- mean(logDiv, na.rm=T)+logResid
		
		# exponentiating
			detDiv <- exp(logResidMean)

		# limit to the same part as the turnover
			detDivShort <- detDiv
			detDivShort[indNAshort] <- NA
	
		# plot diversity
		if(plot){
			#1. real dimension
			par(mar=c(1, 4.1,4.1,2.1))
			nMaxDiv<-max(divVarLong, na.rm=T)
			nMinDiv<-min(divVarLong, na.rm=T)
			tsplot(stages,  shading="series", ylim=c(nMinDiv*0.95, nMaxDiv*1.05), ylab="Richness", xlim=c(530,0), plot.args=list(axes=F), xlab="")
			lines(age, divVarLong, lty=1)
			lines(age, detDivShort, col="black", lty=2)
			axis(2)
			legend("topleft", inset=c(0.01, 0.01), legend=c("original", "detrended"), lty=c(1,2), bg="white")
			
		# the detrended rates
			par(mar=c(5.1,4.1,0.5,2.1))
			tsplot(stages, boxes="per", ylim=c(0,4), shading="series", xlim=c(530,0), ylab="Detrended rates", xlab="Age (Ma)")
			lines(age, detExtShort,col="red" )
			lines(age, detOriShort,col="blue" )
			abline(h=boxp$stats[5], lty=2)
	
		# 2. log dimension richness
			nMaxDiv<-log(nMaxDiv)
			nMinDiv<-log(nMinDiv)
			par(mar=c(5.1,4.1,0.5,2.1))
			tsplot(stages,  boxes="per", shading="series", ylim=c(nMinDiv*0.95, nMaxDiv*1.05), ylab="Logged richness", xlim=c(530,0), xlab="Age (Ma)")
			lines(age, logDiv,col="black" )
			lines(age,logPredict, lwd=2)
		}	
			
		
	# run the diversity dynamics matrix
		dyn<- dynamics(div=detDivShort, ext=detExtShort,ori=detOriShort, alpha=0.05)
	
	# the final results
		res<-list(
			"whichModel is indicated, pulsed or continuous"=pulsed, 
			"declines"=decMat, 
			"largest extinction"=largest,
			"extinction outliers (boxplot)"=outliers,  
			"log-normality (Shapiro-Wilk p-value)"=pShap, 
			"dyn"=dyn$sig)
	if(!feedback){
		options(warn=0)
	}

	return(res)
}


#' Testing the 'pulsed' and 'continuous' models of turnover
#' 
#' The function takes a rate time series and the durations of the intervals and outputs correlation tests
#' between them using both the unnormalized ('pulsed') and the normalized (rates/dur, 'continuous') rates series.
#' 
#' @param rates {\code{numeric}}: Time series of rates.	
#' @param dur {\code{numeric}}: Interval durations.
#' @param method {\code{character}}: The method of the correlation tests.
#' @param alpha (\code{numeric}): Used alpha-level of significance, defaults to 0.05.
pulseCont<-function(rates, dur, method="spearman", alpha=0.05){
	suppressWarnings(
		pulsed<-cor.test(dur, rates, method=method)
	)
	suppressWarnings(
		cont <- cor.test(dur, rates/dur, method=method)
	)
	
	est<-c(pulsed$estimate, cont$estimate)
	p<-c(pulsed$p.value, cont$p.value)
	
	nam<- c("not-normalized", "normalized")
	names(est)<-nam
	names(p) <- nam
	sig<-est[p<alpha]
	
	# return value
	list(est=est, p=p, sig=sig)
}	
	

#' Testing for the cross correlations at a given lag between extinction, origination and richness series	
#' 
#' The function requires detrended rates. 
#' 
#' @param ori {\code{numeric}}: Origination rate time series.
#' @param ext {\code{numeric}}: Extinction rate time series.
#' @param div {\code{numeric}}: Diversity time series.
#' @param method {\code{character}}: The method of the correlation tests.
#' @param L {\code{intefer}}: Lag to be tested
#' @param alpha (\code{numeric}): Used alpha-level of significance, defaults to 0.05.
dynamics<- function(ori, ext, div, method="spearman", l=1, alpha=0.05){
	#lagged series
	oriPlus<-ori[(l+1):(length(ori)+l)]
	extPlus<-ext[(l+1):(length(ext)+l)]
	divPlus<-div[(l+1):(length(div)+l)]

	# the names of the used variables
	varNames<-c("div","divPlus","ori","oriPlus","ext","extPlus")
	
	corMat <- matrix(NA, ncol=length(varNames), nrow=length(varNames))
	colnames(corMat)<-varNames
	rownames(corMat)<-varNames
	
	# copy
	pMat <- corMat
	
	for(i in 1:length(varNames)){
		for(j in 1:length(varNames)){
			var1 <- get(varNames[i])
			var2 <- get(varNames[j])
		
			temp<-cor.test(var1, var2, method=method)
			corMat[i,j] <- temp$estimate
			pMat[i,j] <- temp$p.value
		}
	}
	
	signMat <- corMat
	signMat[pMat>alpha] <- NA
	
	signMat<- as.data.frame(as.table(signMat), stringsAsFactors=F)
	signMat<-signMat[!is.na(signMat$Freq),]
	
	# add the correlation values too
	signMat$p<-rep(NA, nrow(signMat))
	
	for(i in 1:nrow(signMat)){
		signMat[i, 1:2]<-sort(signMat[i, 1:2])
		signMat$p[i]<-pMat[signMat[i,1], signMat[i,2]]
	}
	signMat<-unique(signMat)
	signMat<-signMat[signMat$Freq!=1, ]
	
	obj<-list(est=corMat, p=pMat, sig=signMat) 
	colnames(obj$sig)[colnames(obj$sig)=="Freq"] <- "estimate"

	# omit same column output from significant at least
	obj$sig <- obj$sig[obj$sig[,1]!=obj$sig[,2],]

	
	return(obj)

}
