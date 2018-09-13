# Script to assign Cambrian collections to 'stg' stages based on a table called 'camb'
# last checked with data of 2018-08-31 - Adam Kocsis

###############################################################
# Cambrian stratigraphic assignments
	# the collections entered
	colls <- as.character(dat$collection_no)
	
	# which are cambrian?
	bool <- colls%in%names(camb)
	
	# which of these have already some identifiers
	table(dat$stg[bool])

#	# apparently present in both, but identified as Tremadocian
#	temp <- dat[bool,]
#
#	weirdColls<-as.character(unique(temp[!is.na(temp$stg),"collection_no"]))
#
#	# replace these with Tremadocian
#	camb[weirdColls] <- 14

	# the cambrian collections - as they are in the occurrence dataset
	subColls <- colls[bool]
	
	# order/assign the stg accordingly
	subStg<-camb[subColls]
	
	# copy original
	newStg <- dat$stg
	
	# replace the missing entries
	newStg[bool]  <- subStg
	
	#
	dat$stg <- newStg
