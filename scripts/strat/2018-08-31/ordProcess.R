# Script to assign Orodician collections to 'stg' stages based on tables format, max.int, and zones
# last checked with data of 2018-08-31 - Adam Kocsis

# Transform to collections
new <- unique(dat[is.na(dat$stg), c(
	"collection_no", 
	"early_interval", 
	"late_interval", 
	"zone", 
	"formation",
	"max_ma",
	"min_ma",
	"bin",
	"stg")])
	
	# check always
	
	# Looping formation (added condition that period is same)
    for (i in 1:nrow(format))  {
		ix <- which((as.character(new$formation) == as.character(format$formation[i])))
		new$stg[ix] <- format$stg[i]
    }
	
#	x <- table(new$stg) # control
#   sum(x)

    # Looping early_intervals 
    for (i in 1:nrow(max.int))  {
        ix <- which(as.character(new$early_interval) == as.character(max.int$Max.int[i]))
        new$stg[ix] <- max.int$stg.1[i]
    }

    #  Looping late intervals (to check if different)
	stg2 <- rep(NA, nrow(new))
    for (i in 1:nrow(max.int))  {
		ix <- which(as.character(new$late_interval) == as.character(max.int$Max.int[i]))
        stg2[ix] <- max.int$stg.1[i]
    }

	ix <- which(new$stg<stg2) # should ignore NAs in second column
    new$stg[ix] <- NA


 #   x <- table(new$stg) # control
 #   sum(x)

	
	# Looping zones
	for (i in 1:nrow(zones))  {
		ix <- which(as.character(new$zone) == as.character(zones$zone[i]))
		new$stg[ix] <- zones$stg[i]
	}

	
#	x <- table(new$stg) # control
#	sum(x)
#	View(new[new$min_ma>400,])
	
	# only that part, which has stg assignments now
	new2 <- new[!is.na(new$stg), ]

	# vector: stg numbers, names:collection numbers
	ord <- new2$stg
	names(ord) <- new2$collection_no

	# the collection identifiers of occurrences in the total dataset
	colls <- as.character(dat$collection_no)

	# which are present in the newly gathered data?
	bool <- colls%in%names(ord)

	# collection identifiers of the occurrences of only these collections
	subColls <- colls[bool]
	
	# order/assign the stg accordingly
	subStg<-ord[subColls]
	
	# copy original
	newStg <- dat$stg
	
	# replace the missing entries
	newStg[bool]  <- subStg
	
	# make sure things are OK
#	origTab <- table(dat$stg)
#	newTab <- table(newStg)
#	newStg-origTab # should be all positive!!!

	# add to the full table
	dat$stg <- newStg

