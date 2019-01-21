#' ---
#' title: "Functions to analyze Phanerozoic-scale time series of diversity dynamics, version 0.3"
#' author: "Ádám T. Kocsis"
#' date: "September 18th, 2018"
#' ---

#' Extracting values form the output of the analyzeMetrics() function
#' 
#' The purpose of this function is to loop through a bunch of object entries (lists), extract and organize the information within their elements.
#'
#' @param entries (\code{list}): Names within the R global environment that are outputs of the analyzeMetrics function.
#' @param pvals (\code{logical}): Should only the p-values be extracted (except for the positive negative values. )
extractVals<-function(entries, pvals=TRUE){
	listObj <- sapply(entries, function(x){
		list(get(x))
	})
	
	res <- matrix(NA, ncol=length(entries), nrow=18)
	

	# pulsed or continous 4
	pcNames <- c("ext. rates with durations", "norm. ext. rates with durations", "orig. rates with durations",  "norm. orig. rates with  durations")
	for(i in 1:length(entries)){
		# all results of the case
		oneRes <- listObj[[i]]
		pc <- oneRes[["pulsed-continuous significance"]]
		
		if(pvals){
			res[1:4,i]<- pc[,"p"]
		}else{
			res[1:4,i]<- pc[,"est"]
		}

		# decline of rates 4
		decNames <-rownames(oneRes$declines)

		if(pvals){
			res[5:8,i] <- oneRes$declines[,"p-value"]
		}else{
			res[5:8,i] <- oneRes$declines[,"correlation"]
		}
	
		# mass extinction 5
		extNames  <- c("end-Permian ME", "end-Triassic ME", "end-Cretaceous ME", "end-Permian is highest","number of mass extinctions")
		if(pvals) {
			# the presences of the big 3
			signME<- c("end-Permian", "end-Triassic","end-Cretaceous") %in% oneRes[["extinction outliers (boxplot)"]]
	
			# where not present 0.02 (black values)
			temp <- rep(0.02,3)
			# where present: 0.002 (bold values)
			temp[signME] <- 0.002
		
			res[9:11,i]<-temp

			
			# is the end-permian the highest?
			if("end-Permian"==oneRes[["largest extinction"]]){
				res[12,i] <- 0.002
			}else{
				res[12,i] <- 0.02
			}
			
			# how many mass extinctions are there?
			res[13,i] <- 0.002

		}else{
			# the presences of the big 3
			res[9:11,i] <- c("end-Permian", "end-Triassic","end-Cretaceous") %in% oneRes[["extinction outliers (boxplot)"]]
	
			# is the end-perian the highest?
			res[12,i] <- "end-Permian"==oneRes[["largest extinction"]]

			# how many mass extinctions are there?
			res[13,i] <-length(oneRes[["extinction outliers (boxplot)"]])

			
		}

		# lognormal 2
		lnNames <- c("extinctions log-normal (p-values)", "originations log-normal (p-values)")
		res[14:15,i] <- oneRes[["log-normality (Shapiro-Wilk p-value)"]]
	
		# dynamics 3
		dynNames <- c("origination and lagged diversity", "diversity and lagged extinction", "diversity and lagged origination")
		dyn<-oneRes$dyn

		if(pvals){
			res[16,i] <- dyn$p["ori", "divPlus"]
			res[17,i] <- dyn$p["div", "extPlus"]
			res[18,i] <- dyn$p["ext", "oriPlus"]
		}else{
			res[16,i] <- dyn$est["ori", "divPlus"]
			res[17,i] <- dyn$est["div", "extPlus"]
			res[18,i] <- dyn$est["ext", "oriPlus"]
		}

	}
	rownames(res) <- c(pcNames, decNames, extNames, lnNames, dynNames)
	colnames(res) <- entries
	
	res <- t(res)

	return(res)
}

#' Analysis function of Phanerozoic-scale diversity dynamics
#' 
#' This function outputs results regarding the 'pulsed' vs. continuous models of turnover,
#' the decline of the rates, the presence of mass extinctions and implications of equilibrial dynamics.
#' 
#' The analyis for the decline is between 540 and 20 million years. Detrended series are used betwen 475 and 20 million years. The post-Ordovician part starts at 443 million years.
#' 
#' @param dat (\code{data.frame}): Data table of the time series/variables to be analyzed.
#' @param ext (\code{character}): Column name of the extinction rate variable.
#' @param ori (\code{character}): Column name of the origination rate variable.
#' @param div (\code{character}): Column name of the richness variable.
#' @param age (\code{character}): Column name of mid ages of the time bins.
#' @param name {\code{character}}: Column name of the names of the bins (from the stages or bins) objects.
#' @param dur {\code{character}}: Column name of the interval durations.
#' @param normalize {\code{logical}}: If set to \code{TRUE} then the rate values will be normalized with the bin durations.
#' 	if it is indicated by correlations between the rate values and the interval durations. If set to \code{FALSE}, 
#' 	the input rate values will be used regardless of the potential effects of bin durations.
#' @param plot {\code{logical}}: If set to \code{TRUE} then a four panel plot is produced, showing the input time series and the detrending.
#' @param feedback {\code{logical}}: If set to \code{FALSE}, all warnings and messages are disabled. Warnings might arise due to ties in correlation tests, or after logging 0s.
#' @param detrend{\code{character}}: The detrending type. Setting it "linear" will apply a linear model on the transformed values. 
#' Setting it to "arima" will call the \code{auto.arima} function from the \code{forecast} package and fit an ARIMA model to the transformed time series.
#' Setting this argument to the default "loess" will call the \code{as.loess} function from the \code{fANCOVA} package.
#' @param transform {\code{character}}: The type of transformation for the time series. Set to \code{"log"} for a logarithm transformation, \code{"sqrt"} for a square-root transformation and \code{FALSE} for no transformation.
#' @param additive (\code{logical}): Should additive decomposition be used? If set to \code{FALSE}, multiple decomposition will be used instead (default). 
#' If transformation are applied to the data, then the decomposition will be done in the transformed space!
analyzeMetrics<-function(dat, ext, ori, div, age, name, dur, normalize=FALSE, plot=T, feedback=T, detrend="loess", transform=FALSE, additive=FALSE){
	
#	# examples used to develop the function
#	dat <- cbind(ddStages, stages)
#	ext <- "extPC" # extinction rates
#	ori <- "oriPC" # origination rates
#	div <- "divCSIB" # diversity (richness) series
#	dur <- "dur" # durations of time intervals
#	age <- "mid" # time interal midpoints
#	name <- "name" # names of the time intervals

	if(!feedback){
		options(warn=-1)
	}

	# check if forecast is needed
	if(detrend=="arima"){
		if(!"forecast"%in%installed.packages()){
			message("The forecast package is necessary to run this function. It will be downloaded from CRAN.")
			install.packages("forecast")
		} 
	}

	# check for the fANCOVA pacakges
	if(detrend=="loess"){
		if(!"fANCOVA"%in%installed.packages()){
			message("The fANCOVA package is necessary to run this function. It will be downloaded from CRAN.")
			install.packages("fANCOVA")
		} 
	}

	# should the pulsed or continuous model be used???

	extPuCo<-pulseCont(dat[, ext], dur=dat[,dur])
	oriPuCo<-pulseCont(dat[, ori], dur=dat[,dur])
	
	if(normalize){
		pulsed <- matrix(TRUE, ncol=2, nrow=2)
		colnames(pulsed) <- c("extinction", "origination")
		rownames(pulsed) <- c("supplied (pulsed) used", "pulsed used because conflict ")
		pulsed[2,] <- FALSE
	
	
		if(length(extPuCo$sig)==1){
			if(names(extPuCo$sig)=="not-normalized"){
				dat[, ext] <-dat[,ext]/dat[, dur] *mean(dat[,dur], na.rm=T)
				if(feedback) message("Normalization of extinctions is indicated, the values in the entry table changed!")
				pulsed[1,1]<-FALSE
			}
		}
		if(length(extPuCo$sig)==2) pulsed[2,1]<-TRUE
		
		if(length(oriPuCo$sig)==1){
			if(names(oriPuCo$sig)=="not-normalized"){
				dat[, ori] <-dat[,ori]/dat[, dur] *mean(dat[,dur], na.rm=T)
				if(feedback) message("Normalization of originations was indicated, the values in the entry table changed!")
				pulsed[1,2]<-FALSE
		
			}
		}
		if(length(oriPuCo$sig)==2) pulsed[2,2]<-TRUE
	}else{
		pulsed <- TRUE

	}

	# significance of the tests
	puCoSign <- matrix(NA, ncol=2, nrow=4)
	rownames(puCoSign) <- c("extinction (not-normalized)", "extinction (normalized)","origination (not-normalized)", "origination (normalized)")
	colnames(puCoSign) <- c("est", "p")

	puCoSign[,"est"] <- c(extPuCo$est,oriPuCo$est)
	puCoSign[,"p"] <- c(extPuCo$p,oriPuCo$p)

	
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
	
	postDate <- 443
#	postDate <- 460 # post -mid Ordovician
	
	#logical to see what is necessary
	# Cambrian included 
	indNAlong<- !(age<=maxAgeDecline & age>=minAgeDecline)
	
	# detrending interval
	indNAshort<- !(age<=maxAgeDetrend & age>=minAgeDetrend)
	
	# post ordovician
	indNApostord <- !(age<=postDate & age>=minAgeDecline)
		
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
		tsplot(stages,  shading="per", ylim=c(0, nMaxRate*1.05), ylab="Turover rates", xlim=c(530,0), plot.args=list(axes=F), xlab="")
		axis(2)
		lines(age, extVarLong, col="red")
		lines(age, oriVarLong, col="blue")
		legend("topright", legend=c("extinction", "origination"), col=c("red", "blue"), lwd=c(1,1), bg="white", inset=c(0.01, 0.01))
	}


	# decline in the extinction rate
	# create a table from the decline-information
	decMat <-matrix(NA, ncol=2, nrow=4)
	colnames(decMat)<- c("correlation","p-value")
	rownames(decMat)<- c("extinctions", "post-Ordovician extinctions", "originations", "post-Ordovician originations")
	
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
	

	# transform the turnover rates
	# log-transform
	if(transform=="log"){
		transExt <- log(extVarLong)
		transOri <- log(oriVarLong)
			
		transExt[is.infinite(transExt)] <- NA
		transOri[is.infinite(transOri)] <- NA
	}
	if(transform=="sqrt"){
		transExt <- sqrt(extVarLong)
		transOri <- sqrt(oriVarLong)
			
		transExt[is.infinite(transExt)] <- NA
		transOri[is.infinite(transOri)] <- NA
	}

	# no transformation
	if(transform==FALSE){
		transExt <- extVarLong
		transOri <- oriVarLong
	}

	# DETRENDING
	# detrending using an ARIMA processes
	if(detrend=="arima"){

		# fit an automatic ARIMA model to the time series
		extModel<- forecast::auto.arima(transExt,stepwise=FALSE)
		oriModel<- forecast::auto.arima(transOri,stepwise=FALSE)
		
		# predicted values
		predExt <- as.numeric(extModel$fitted)
		predOri <- as.numeric(oriModel$fitted)

	}

	if(detrend=="loess"){
 		# omit freaking missing values
 		#extinctions
 		extMiss <- !is.na(transExt)
 		transExtNoNA <- transExt[extMiss]
 		ageExt <- age[extMiss]

		#originations
 		oriMiss <- !is.na(transOri)
 		transOriNoNA <- transOri[oriMiss]
 		ageOri <- age[oriMiss]
	
		# the models
		extModel <- fANCOVA::loess.as(ageExt, transExtNoNA, degree = 1, criterion = "aicc", user.span = NULL, plot = FALSE)
 		oriModel <- fANCOVA::loess.as(ageOri, transOriNoNA, degree = 1, criterion = "aicc", user.span = NULL, plot = FALSE)

 		# predicted
 		predExt <- predict(extModel, newdata=data.frame(x=age))
 		predOri <- predict(oriModel, newdata=data.frame(x=age))
	}

	# the detrending model: simple linear regression
	if(detrend=="linear"){

		# model based on the logged data, can be some estimation problems but this doesn't influence short-term correlation patterns
		extMod<-lm(transExt ~ age)
		oriMod<-lm(transOri ~ age)
		
		# predicted values (exponentiated back)
		predExt <- predict(extMod, newdata=data.frame(age=age))
		predOri <- predict(oriMod, newdata=data.frame(age=age))

	}
	
	# residuals (logging will mean multiplicative decompostion!!!)
	if(additive){
		detExt <- transExt-predExt
		detOri <- transOri-predOri
	}else{
		detExt <- transExt/predExt
		detOri <- transOri/predOri
	}
	# log transformation
	if(transform=="log"){
		# predicted values
		predExt <- exp(predExt)
		predOri <- exp(predOri)
		
		# the residuals
		detExt <- exp(detExt)
		detOri <- exp(detOri)

	}
	
	# log transformation
	if(transform=="sqrt"){
		# predicted values
		predExt <- predExt^2
		predOri <- predOri^2
		
		# the residuals
		detExt <- detExt^2
		detOri <- detOri^2

	}


	#plot the predictions
	if(plot){
		lines(age, predExt,col="red", lwd=2)
		lines(age, predOri,col="blue", lwd=2)
		
		# plot model for arima
		if(detrend=="arima"){
			dispArima(extModel, prefix="ext.", x=350, y=nMaxRate, cex=0.75)
			dispArima(extModel, prefix="ori.", x=350, y=nMaxRate*0.95, cex=0.75)
		}
		
	}

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
		# add names so values can be identified
		names(detExtShort) <- name
		
		boxp<- boxplot(detExtShort, plot=F)

		outliers <- names(boxp$out)

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
		if(transform=="log"){
			transDiv <- log(divVarLong)
		}
		
		if(transform=="sqrt"){
			transDiv <- sqrt(divVarLong)
		}
		if(transform==FALSE){
			transDiv <- divVarLong
		}
		
		# linear detrending
		if(detrend=="linear"){
			# quadratic regression 
			# divModQuad <- lm(transDiv ~ poly(age,2))
			divMod <- lm(transDiv ~ age)
		
			#calculate the prediction
			transPredict<- predict(divMod, newdata=data.frame("age"=age))

		}

		if(detrend=="arima"){
			# fit an automatic ARIMA model to the time series
			divModel<- forecast::auto.arima(transDiv,stepwise=FALSE)
			transPredict <- as.numeric(divModel$fitted)
			
			
		}

		if(detrend=="loess"){
 			divMiss <- !is.na(transDiv)
 			transDivNoNA <- transDiv[divMiss]
 			ageDiv <- age[divMiss]

 			divModel <- fANCOVA::loess.as(ageDiv, transDivNoNA, degree = 1, criterion = "aicc",  user.span = NULL, plot = FALSE)
 	
			# predicted
 			transPredict <- predict(divModel, newdata=data.frame(x=age))
 		
		}
		
		# the residuals
		if(additive){
			transResid <- transDiv-transPredict
		}else{
			transResid <- transDiv/transPredict
		}
					
		# adding back the mean for visualization
		if(additive){
			detDiv<- mean(transDiv, na.rm=T)+transResid
		}else{
			detDiv<- mean(transDiv, na.rm=T)*transResid
		}
			
	
		if(transform=="log"){	
			# exponentiating
			detDiv <- exp(detDiv)
			transPredict <- exp(transPredict)

		}
		
		if(transform=="sqrt"){	
			# exponentiating
			detDiv <- detDiv^2
			transPredict <- transPredict^2

		}


		# limit to the same part as the turnover rates
			detDivShort <- detDiv
			detDivShort[indNAshort] <- NA
	
		# plot diversity
		if(plot){
			#1. real dimension
			par(mar=c(1, 4.1,4.1,2.1))
			nMaxDiv<-max(divVarLong, na.rm=T)
			nMinDiv<-min(divVarLong, na.rm=T)
			tsplot(stages,  shading="per", ylim=c(nMinDiv*0.95, nMaxDiv*1.05), ylab="Richness", xlim=c(530,0), plot.args=list(axes=F), xlab="")
			lines(age, divVarLong, lty=1)
			lines(age, detDivShort, col="black", lty=2)
			axis(2)
			legend("top", inset=c(0.01, 0.01), legend=c("original", "detrended"), lty=c(1,2), bg="white")
			
		# the detrended rates
			detMax <- max(c(detExtShort,detExtShort), na.rm=T)
			detMin <- min(c(detExtShort,detExtShort), na.rm=T)
			if(detMin>0) detMin <-0
			par(mar=c(5.1,4.1,0.5,2.1))
			tsplot(stages, boxes="per", ylim=c(detMin*1.05,detMax*1.05), shading="per", xlim=c(530,0), ylab="Detrended rates", xlab="Age (Ma)")
			lines(age, detExtShort,col="red" )
			lines(age, detOriShort,col="blue" )
			abline(h=boxp$stats[5], lty=2)
	
		# 2. log dimension richness
			par(mar=c(5.1,4.1,0.5,2.1))
			
			if(transform=="log"){
				nMaxDiv<-log(nMaxDiv)
				nMinDiv<-log(nMinDiv)
				tsplot(stages,  boxes="per", shading="per", ylim=c(nMinDiv*0.95, nMaxDiv*1.05), ylab="Logged richness", xlim=c(530,0), xlab="Age (Ma)")
			}
			if(transform=="sqrt"){
				nMaxDiv<-sqrt(nMaxDiv)
				nMinDiv<-sqrt(nMinDiv)
				tsplot(stages,  boxes="per", shading="per", ylim=c(nMinDiv*0.95, nMaxDiv*1.05), ylab="Square root richness", xlim=c(530,0), xlab="Age (Ma)")
			}
			if(transform==FALSE){
				tsplot(stages,  boxes="per", shading="per", ylim=c(nMinDiv*0.95, nMaxDiv*1.05), ylab="Richness", xlim=c(530,0), xlab="Age (Ma)")
			}
			
			lines(age, transDiv,col="black" )
			lines(age,transPredict, lwd=2)
			if(detrend=="arima") dispArima(divModel, prefix="", x=350, y=nMaxDiv, cex=0.75)
			
		}	
			
		
	# run the diversity dynamics matrix
		dyn<- dynamics(div=detDivShort, ext=detExtShort,ori=detOriShort, alpha=0.05)
	
	# the final results
		res<-list(
			"whichModel is indicated, pulsed or continuous"=pulsed, 
			"pulsed-continuous significance"=puCoSign,
			"declines"=decMat, 
			"largest extinction"=largest,
			"extinction outliers (boxplot)"=outliers,  
			"log-normality (Shapiro-Wilk p-value)"=pShap, 
			"dyn"=dyn)
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


dispArima<-function(model,prefix, ...){
	pars<- model$model
	tex <- paste("ARIMA(", length(pars$phi), ",", length(pars$Delta), ",", length(pars$theta), ")", sep="")
	lab<- paste(prefix, tex)
	text(label=lab, ...)
}
