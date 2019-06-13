# Basic code for the Phanerozoic diversity handout
#
#   # 1. I used this link to download the data, saved it as 'all_2018-09-14.csv'
#	https://paleobiodb.org/data1.2/occs/list.csv?datainfo&rowcount&interval=Ediacaran,Holocene&show=class,classext,genus,subgenus,abund,coll,coords,loc,paleoloc,strat,stratext,lith,env,ref,crmod,timebins,timecompare
#	
#   # 2. Write binary file, so that it takes up less space:
#	dat <- read.csv("D:/all_2019-01-03.csv", header=T, 
#		stringsAsFactors=F, skip=17)
#
#		# make this dataset somewhat slimmer so that it can be hosted on github
#		need<-c(
#			"collection_no",
#			"collection_name",
#			"identified_rank",
#			"identified_name",
#			"accepted_name",
#			"accepted_rank",
#			"early_interval",
#			"late_interval",
#			"max_ma",
#			"min_ma",
#			"reference_no",
#			"phylum",
#			"class",
#			"order",
#			"family",
#			"genus",
#			"lng",
#			"lat",
#			"paleolng",
#			"paleolat",
#			"formation",
#			"lithology1",
#			"lithification1",
#			"environment",
#			"created",
#			"zone"
#		)
#
#		dat <- dat[,need]
#
#	save(dat, file="d:/2019-01-03_paleoDB.RData")
#	# then it was posted on GitHub
#
#	# extracting metadata
#	# read in occurrence file
#		fname <- "D:/all_2019-01-03.csv"
#		con<-file(fname)
#		a<-readLines(con, 16)
#		close(con)
#
#	# write metaData
#		meta <- "D:/metadata_2019-01-03.txt"
#		met <- file(meta)
#		writeLines(text=a, met)

################################################################################
# workflow starts from here, set your working diretory!!!
	workdir<-"D:/Dropbox/WorkSpace/2017-04-05_divDyn/ddPhanero/"
#	workdir <- "/media/kocsis/Data/Dropbox/WorkSpace/2017-04-05_divDyn/ddPhanero/"
	setwd(workdir)

	# load the package
	library(divDyn)

#	source("D:/Dropbox/WorkSpace/2017-04-05_divDyn/ddPhanero/scripts/0.4/phanDyn.R")
	source("https://github.com/divDyn/ddPhanero/raw/master/scripts/0.4/phanDyn.R")

	# subfolders used for output (make them!)
	redo<-"export/0.4"
	
################################################################################
# read the data table
	load(url("https://github.com/divDyn/ddPhanero/raw/master/data/PaleoDB/2019-01-03_paleoDB.RData"))
#	load("D:/Dropbox/WorkSpace/2017-04-05_divDyn/ddPhanero/data/PaleoDB/2019-01-03_paleoDB.RData")

# 1. taxonomic filtering
	# filter records not identified at least to genus
	dat <-dat[dat$accepted_rank %in% c("genus", "species"),]

	# omit non-informative genus entries
	dat <- dat[dat$genus!="", ]

	#A. phyla
	marineNoPlant <- c("",
		"Agmata",
		"Annelida",
		"Bilateralomorpha",
		"Brachiopoda",
		"Bryozoa",
		"Calcispongea",
		"Chaetognatha",
		"Cnidaria",
		"Ctenophora",
		"Echinodermata",
		"Entoprocta",
		"Foraminifera",
		"Hemichordata",
		"Hyolitha",
		"Mollusca",
		"Nematoda",
		"Nematomorpha",
		"Nemertina",
		"Onychophora",
		"Petalonamae",
		"Phoronida",
		"Platyhelminthes",
		"Porifera",
		"Problematica",
		"Rhizopodea",
		"Rotifera",
		"Sarcomastigophora",
		"Sipuncula",
		"Uncertain",
		"Vetulicolia",
		""
	)

	# which rows?
	bByPhyla <- dat$phylum%in% marineNoPlant

	# the other
		noNeed <- dat[!bByPhyla,]
		needPhylum <- dat[bByPhyla,]

	#B. classes
	#	levels(factor(noNeed$class))
		needClass <- c(
			"Acanthodii",
			"Actinopteri",
			"Actinopterygii",
			"Agnatha",
			"Cephalaspidomorphi",
			"Chondrichthyes",
			"Cladistia",
			"Coelacanthimorpha",
			"Conodonta",
			"Galeaspida",
			"Myxini",
			"Osteichthyes",
			"Petromyzontida",
			"Plagiostomi",
			"Pteraspidomorphi",
			# here come the Arthropods
			"Artiopoda",
			"Branchiopoda",
			"Cephalocarida",
			"Copepoda",
			"Malacostraca",
			"Maxillopoda",
			"Megacheira",
			"Merostomoidea",
			"Ostracoda",
			"Paratrilobita",
			"Pycnogonida",
			"Remipedia",
			"Thylacocephala",
			"Trilobita",
			"Xiphosura"
		)
		
		# which rows?
		bNeedClass <- dat$class %in% needClass

	#C.  mammals
	#	mammals <- dat[dat$class=="Mammalia",]
	#	levels(factor(mammals$order))

		needMammalOrd <- c("Cetacea", "Sirenia")

		# which rows?
		bMammalOrder <- dat$order %in% needMammalOrd

		# the carnivores
		carnivores <- dat[dat$order=="Carnivora",]
		levels(factor(carnivores$family))

		needFam <- c("Otariidae", "Phocidae", "Desmatophocidae")

		# which rows?
		bNeedMamFam<- dat$family%in%needFam

	# D. Reptiles
	#	reptiles <- dat[dat$class=="Reptilia",]
	#	levels(factor(reptiles$order))

		needReptOrd<-c(
			"Eosauropterygia",
			"Hupehsuchia",
			"Ichthyosauria",
			"Placodontia",
			"Sauropterygia",
			"Thalattosauria"
		)
		
		# which rows?
		bRept <- dat$order%in%needReptOrd

	# E. turtles 
	#	turtles <- dat[dat$order=="Testudines",]
	#	levels(factor(turtles$family))
	
	# Chelonioidea turtles
	needTurtleFam <- c(
		"Cheloniidae",
		"Protostegidae",
		"Dermochelyidae",
		"Dermochelyoidae",
		"Toxochelyidae",
		"Pancheloniidae"
	)

	# which rows?
	bTurtle <- dat$family%in%needTurtleFam

	# subset the data
	dat <- dat[
		bByPhyla |
		bNeedClass |
		bMammalOrder |
		bNeedMamFam |
		bRept | 
		bTurtle
		, ]


	# resolve the potential homonymy problem
	dat$clgen <- paste(dat$class, dat$genus)

################################################################################
# 2. filter by environment
	levels(factor((dat$environment)))

	omitEnv <- c(
		"\"floodplain\"",
		"alluvial fan",
		"cave",
		"\"channel\"",
		"channel lag" ,
		"coarse channel fill",
		"crater lake",
		"crevasse splay",
		"dry floodplain",
		"delta plain",
		"dune",
		"eolian indet.",
		"fine channel fill",
		"fissure fill",
		"fluvial indet.",
		"fluvial-lacustrine indet.",
		"fluvial-deltaic indet.",
		"glacial",
		"interdune",
		"karst indet.",
		"lacustrine - large",
		"lacustrine - small",
		"lacustrine delta front",
		"lacustrine delta plain",
		"lacustrine deltaic indet.",
		"lacustrine indet.",
		"lacustrine interdistributary bay",
		"lacustrine prodelta",
		"levee",
		"loess",
		"mire/swamp",
		"pond",
		"sinkhole",
		"spring",
		"tar",
		"terrestrial indet.",
		"wet floodplain"
	)

	dat<-dat[!dat$environment%in%omitEnv, ]


# finally omit unlithified sediments
	dat <- dat[dat$lithification1!="unlithified",]

	
# resolving remaining marine environmental variables (not for this, but for additional analyses) 
 	# lithology
	dat$lith<-categorize(dat$lithology1,keys$lith)

	# batyhmetry
	dat$bath <- categorize(dat$environment,keys$bath) 

	# grain size
	dat$gra <- categorize(dat$lithology1,keys$grain) 

	# reef or not?
	dat$reef <- categorize(dat$environment, keys$reef) 
	dat$reef[dat$lith=="clastic" & dat$environment=="marine indet."] <- "non-reef" # reef or not?/2


################################################################################
# 3. stratigraphic resolution
	# time scales
	data(stages)
	data(bins)
	
	# rename the entries
	colnames(bins)[colnames(bins)=="X10"]<-"name"
	colnames(stages)[colnames(stages)=="stage"]<-"name"

	# load lookup keys
	data(keys)

#-------------------------------------------------------------------------------
# A.  the bin entries

	# a. categorize interval names to bin numbers
		# categorize is the new function of the package
		binMin<-categorize(dat[,"early_interval"],keys$binInt)
		binMax<-categorize(dat[,"late_interval"],keys$binInt)

		# convert to numeric
		binMin<-as.numeric(binMin)
		binMax<-as.numeric(binMax)

	# b. resolve max-min interval uncertainty
	# final variable (empty)
		dat$bin <- rep(NA, nrow(dat))

	# use entries, where
		binCondition <- c(
		# the early and late interval fields indicate the same bin
			which(binMax==binMin),
		# or the late_interval field is empty
			which(binMax==-1))

	# in these entries, use the bin indicated by the early_interval
		dat$bin[binCondition] <- binMin[binCondition]

 # Sampling
	sampBin <- binstat(dat, bin="bin", tax="clgen", coll="collection_no", 
		duplicates=FALSE)  

	# sort the number of occs/bin
	binOccs <- sampBin$occs
	names(binOccs) <- rownames(sampBin)
	sort(binOccs)

	# plot them
	# time scale
	tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,35000), 
		ylab="number of entries", plot.args=list(cex.lab=1.5, cex.axis=1.8),
		labels.args=list(cex=1.5))

	# occurrences
	lines(bins$mid, sampBin$occs, lwd=2)

	# collections
	lines(bins$mid, sampBin$colls, lwd=2, col="blue")

	# legend
	legend("topright", bg="white", legend=c("occurrences", "collections"), 
		col=c("black", "blue"), lwd=2, inset=c(0.1,0.01), cex=1.3)
	
#####################################-------------------------------------------
# do the same for the stages
# B. the stg entries (lookup)
		stgMin<-categorize(dat[,"early_interval"],keys$stgInt)
		stgMax<-categorize(dat[,"late_interval"],keys$stgInt)

		# convert to numeric
		stgMin<-as.numeric(stgMin)
		stgMax<-as.numeric(stgMax)

	# empty container
		dat$stg <- rep(NA, nrow(dat))

	# select entries, where
		stgCondition <- c(
		# the early and late interval fields indicate the same stg
			which(stgMax==stgMin),
		# or the late_intervarl field is empty
			which(stgMax==-1))

	# in these entries, use the stg indicated by the early_interval
		dat$stg[stgCondition] <- stgMin[stgCondition]

	# terrible in the pre-Silurian
# using the online items
	# additional treatment required for Cambrian
		# load data
		load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/cambStrat.RData"))
		# correct it with this function
		source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2018-08-31/cambProcess.R")

	# and the Ordovician
		# load data
		load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"))
		# correct it with this function
		source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2018-08-31/ordProcess.R")

# using the offline items
#	# additional treatment required for Cambrian
#		# load data
#		load("D:/Dropbox/WorkSpace/2017-04-05_divDyn/ddPhanero/data/Stratigraphy/2018-08-31/cambStrat.RData")
#		# correct it with this function
#		source("D:/Dropbox/WorkSpace/2017-04-05_divDyn/ddPhanero/scripts/strat/2018-08-31/cambProcess.R")
#
#	# and the Ordovician
#		# load data
#		load("D:/Dropbox/WorkSpace/2017-04-05_divDyn/ddPhanero/data/Stratigraphy/2018-08-31/ordStrat.RData")
#		# correct it with this function
#		source("D:/Dropbox/WorkSpace/2017-04-05_divDyn/ddPhanero/scripts/strat/2018-08-31/ordProcess.R")


# Sampling
	sampStg<- binstat(dat, bin="stg", tax="clgen", coll="collection_no", 
		duplicates=FALSE)  

	# time scale
	tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,20000), 
		ylab="number of entries", plot.args=list(cex.lab=1.5, cex.axis=1.8),
		labels.args=list(cex=1.5))

	# occurrences
	lines(stages$mid, sampStg$occs, lwd=2)

	# collections
	lines(stages$mid, sampStg$colls, lwd=2, col="blue")

	# legend
	legend("top", bg="white", legend=c("occurrences", "collections"), 
		col=c("black", "blue"), lwd=2, inset=c(0.15,0.01), cex=1.3)
	
	# sort the occs/stage
	stgOccs <- sampStg$occs
	names(stgOccs) <- rownames(sampStg)
	sort(stgOccs)


################################################################################
# 3. Actual analyses start from here
###################################---------------------------------------------

# pre-calculate some variables for prototyping and display items
	# raw patterns (stages)
		ddStages<-divDyn(dat, bin="stg", tax="clgen")
		
	# SQS (stages)
		sqsStagesPlot<-subsample(dat, bin="stg", tax="clgen", 
			coll="collection_no", q=0.7, iter=100, 
			ref="reference_no",singleton="ref", type="sqs", duplicates=FALSE, 
			excludeDominant=TRUE, largestColl =TRUE, output="dist")


# panels of the first figure
	# set working directory to the trial directory
	setwd(paste(workdir, redo, sep=""))
	
	# (a) richness from raw data
	pdf("first.pdf", 12,10)
		# time scale
		tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,2800), 
			ylab="Richness", plot.args=list(cex.lab=1.5, cex.axis=1.8),
			labels.args=list(cex=1.5), xlab="Age (Ma)")

		# corrected SIB richness
		lines(stages$mid, ddStages$divCSIB, col="blue", lwd=4)

		# SIB richness
		lines(stages$mid, ddStages$divSIB, col="#00BB33", lwd=2)
		legend("bottomleft", inset=c(0.15,0.058), col=c("#00BB33", "blue"), 
			legend=c("raw SIB", "corrected SIB"),  lwd=c(2,4), bg="white", 
			cex=1.8) 

	dev.off()
	
	# (b) show one subsampling trial
	pdf("onetrial.pdf", 12,10)
		# time scale
		tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,2800), 
			ylab="Richness (corrected SIB)",  xlab="Age (Ma)", 
			plot.args=list(cex.lab=1.5, cex.axis=1.8),labels.args=list(cex=1.5))
		
		# corrected SIB from all data
		lines(stages$mid, ddStages$divCSIB, col="blue", lwd=4)

		# corrected SIB from SQS-subsampled data (one trial)
		lines(stages$mid, sqsStagesPlot$divCSIB[,51], col="#00000088", lwd=4)
	dev.off()
	
	# (c) show the averaging of trials
	pdf("many.pdf", 12, 10)
		# time scale
		tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,1000), 
			ylab="Subsampled richness (corrected SIB)", xlab="Age (Ma)",  
			plot.args=list(cex.lab=1.5, cex.axis=1.8),labels.args=list(cex=1.5))

		# plot every subsampling output curve (columns in the object)
		for(i in 1:ncol(sqsStagesPlot$divCSIB)){
			lines(stages$mid, sqsStagesPlot$divCSIB[,i], col="#00000088")
		}

		# average corrected SIB curve
		lines(stages$mid, apply(sqsStagesPlot$divCSIB, 1, mean, na.rm=T), 
			col="#CC3300", lwd=4)
	dev.off()
	

################################################################################
# 4. Iterated analyses start from here
###################################---------------------------------------------

# 10my scale
	# raw patterns
	ddBins<-divDyn(dat, bin="bin", tax="clgen")
	
	# CR
	crBins<-subsample(dat, bin="bin", tax="clgen", coll="collection_no", q=4800, 
		iter=500, duplicates=FALSE)

	# SQS
	sqsBins<-subsample(dat, bin="bin", tax="clgen", coll="collection_no", q=0.7, 
		iter=500, ref="reference_no",singleton="ref", type="sqs", 
		duplicates=FALSE, excludeDominant=TRUE, largestColl =TRUE)

# stages
	# raw patterns
	ddStg<-divDyn(dat, bin="stg", tax="clgen")

	# CR
	crStg<-subsample(dat, bin="stg", tax="clgen", coll="collection_no", q=1100, 
		iter=500, duplicates=FALSE)

	# SQS
	sqsStg<-subsample(dat, bin="stg", tax="clgen", coll="collection_no", q=0.7, 
		iter=500, ref="reference_no",singleton="ref", type="sqs", 
		duplicates=FALSE, excludeDominant=TRUE, largestColl =TRUE)


# Analysis of produced results

# combination table
	# types of rates
	comb <- matrix(
		c(
			"extPC", "oriPC", "divCSIB",
			"extC3t", "oriC3t", "divCSIB",
			"extGF", "oriGF", "divCSIB",
			"ext2f3", "ori2f3", "divCSIB"
		), 
		ncol=3, nrow=4, byrow=T)
	rownames(comb) <- c("PC", "C3t", "GF","2f3")
	comb <- comb[rep(1:4,6),]
	
	# the result matrices
	sourceVar<-rep(
		c("ddBins", "crBins", "sqsBins", "ddStg", "crStg", "sqsStg"),
		each=4)
	
	# timescale objects
	scale <- rep(c("bins", "stages"), each=12)
	
	comb<-cbind(sourceVar, scale, comb)
	colnames(comb) <- c("source", "timescale", "ext", "ori", "div")
	
	# the names of the results
	subtype <- rep(rep(c("raw", "cr", "sqs"), each=4),2)
	rownames(comb)<- paste(subtype, rownames(comb), sep="")
	rownames(comb) <- paste(
		rownames(comb), 
		rep(c("10my", "stages"), each=12), 
		sep="_")


# matrices to hold rates for later plotting
	extDatBin <- data.frame(bins=1:49)
	oriDatBin <- data.frame(bins=1:49)
	extDatStg <- data.frame(stg=1:95)
	oriDatStg <- data.frame(stg=1:95)


# set working directory to the export directory
setwd(paste(workdir, redo, sep=""))

# open pdf
pdf("detrending.pdf", 17,14)

# iterate through all cases
for(i in 1:nrow(comb)){
	case <- rownames(comb)[i]

	# the results matrices and the timescale object combined
	metrics <- cbind(get(comb[i, "timescale"]), get(comb[i, "source"]))

	# save rates for later, depending on the resolution
	# 10 my
	if(comb[i, "timescale"] == "bins"){
		oriDatBin[[case]] <- metrics[, comb[i, "ori"]]
		extDatBin[[case]] <- metrics[, comb[i, "ext"]]
	}

	# stages
	if(comb[i, "timescale"] == "stages"){
		oriDatStg[[case]] <- metrics[, comb[i, "ori"]]
		extDatStg[[case]] <- metrics[, comb[i, "ext"]]

	}

	# the analytical function
	res <- analyzeMetrics(metrics, ext=comb[i, "ext"], ori=comb[i, "ori"], 
		div=comb[i, "div"], age="mid", dur="dur", name="name",
		normalize=FALSE, plot=T, detrend="loess", transform=FALSE, 
		additive=FALSE)

	# save in global namespace with unique name
	assign(case, res)

	# add the name to the plot
	par(mfrow=c(1,1))
	mtext(side=3, text=case, line=-2, cex=3)

}

dev.off()

################################################################################
# Which hypotheses support which analyses?

# the shown values
values <- extractVals(rownames(comb), pvals=FALSE)

# the p values
pvals <- extractVals(rownames(comb), pvals=TRUE)
	
# formatting of this table
	# round
	valuesRounded <- round(values,2)
	valuesRounded[,c(
			"extinctions log-normal (p-values)",
			"originations log-normal (p-values)")]<-
		round(values[,c(
			"extinctions log-normal (p-values)",
			"originations log-normal (p-values)")],3)
	
	#transform to character
	#valuesRounded <- as.data.frame(valuesRounded)
	for(i in 9:13){
		valuesRounded[,i] <- as.character(valuesRounded[,i])
		valuesRounded[valuesRounded[,i]=="1",i] <- "yes"
		valuesRounded[valuesRounded[,i]=="0",i] <- "no"
	}
	
	# for better text (no 0 p-values!)
	valuesRounded[valuesRounded[,"extinctions log-normal (p-values)"]=="0",
		"extinctions log-normal (p-values)"] <- "<0.001"
	valuesRounded[valuesRounded[,"originations log-normal (p-values)"]=="0",
		"originations log-normal (p-values)"] <- "<0.001"
	
	# transpose everything
	tValues<- t(valuesRounded)
	tP <- t(pvals)

	# separate based on resolution
	valBin <- tValues[,1:12]
	pBin <- tP[,1:12]
	valStage <- tValues[,13:24]
	pStage <- tP[,13:24]
	
	write.table(valBin, file="valBin.csv", sep=";")
	write.table(pBin, file="pBin.csv", sep=";")
	write.table(valStage, file="valStage.csv", sep=";")
	write.table(pStage, file="pStage.csv", sep=";")


################################################################################
# Display item
	# remove the bin numbers from containers
	extDatBin$bins<-NULL
	extDatStg$stg<-NULL
	oriDatBin$bins<-NULL
	oriDatStg$stg<-NULL

# open .pdf
pdf("sumRates.pdf", 17,14)

	# make it 2 by 2
	par(mfrow=c(2,2))

	# extinctions
	# 1st panel
		# margin adjustment
		par(mar=c(2, 5.1,4.1,1)) 
		
		# plot
		tsplot(stages, boxes="sys", shading="sys", ylim=c(0, 3), xlim=4:95,
			ylab="Extinction rates", plot.args=list(axes=F, cex.lab=2), xlab="",
			labels.args=list(cex=1.5))
		axis(2, cex.axis=1.8)

		# background
		shades(bins$mid, as.matrix(extDatBin), col="red") 

		# extinction rates series - BIN
		for(i in 1:ncol(extDatBin)) lines(bins$mid, extDatBin[,i]) 
		
		# show resolution
		mtext(line=1, text="10 my bins", side=3, cex=3) 
		
	# 2nd panel
		# margin adjustment
		par(mar=c(2, 1,4.1,4.1)) 
		# plot
		tsplot(stages, boxes="sys", shading="sys", ylim=c(0, 2), ylab="", 
			xlim=4:95, plot.args=list(axes=F, cex.lab=2), xlab="",
			labels.args=list(cex=1.5))

		# background
		shades(stages$mid, as.matrix(extDatStg), col="red") 
		
		# extinction rates series - stages
		for(i in 1:ncol(extDatStg)) lines(stages$mid, extDatStg[,i]) 
		
		# show resolution
		mtext(line=1, text="Stages", side=3, cex=3)

	# originations
	# 3rd panel
		# margin adjustment
		par(mar=c(5.1,5.1,1,1)) 
		# plot
		tsplot(stages, boxes="sys", shading="sys", ylim=c(0, 3), xlim=4:95,
			ylab="Origination rates", plot.args=list(axes=F, cex.lab=2),
			labels.args=list(cex=1.5), xlab="Age (Ma)")
		axis(1, cex.axis=1.8)
		axis(2, cex.axis=1.8)

		# background
		shades(bins$mid, as.matrix(oriDatBin), col="darkgreen") 

		# origination rates series - BIN
		for(i in 1:ncol(oriDatBin)) lines(bins$mid, oriDatBin[,i]) 

		# 4th panel
		# margin adjustment
		par(mar=c(5.1,1,1,4.1))

		# plot
		tsplot(stages, boxes="sys", shading="sys", ylim=c(0, 2), ylab="", 
			xlim=4:95, plot.args=list(axes=F, cex.lab=2),
			labels.args=list(cex=1.5), xlab="Age (Ma)")
		axis(1, cex.axis=1.8)

		# background
		shades(stages$mid, as.matrix(oriDatStg), col="darkgreen") 

		# origination rates series - stages
		for(i in 1:ncol(oriDatStg)) lines(stages$mid, oriDatStg[,i]) 

dev.off()
