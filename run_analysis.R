# Load Required SQLDF Library
library(sqldf)



#Set Data Path and File Sepearator
datapath <- "specdata"
pathSeparator <- "\\"

#Set Output File
outputFile <-  paste(datapath, pathSeparator, "tidydata.txt", sep="")


# Get Column Widths
getColWidths <- function(){
	cols <- read.table(paste(datapath, pathSeparator, "features.txt", sep=""), stringsAsFactors=FALSE)[,2]
	ok <- c(grep("-mean()", cols, fixed = T), grep("-std()", cols, fixed = T))
	widths = numeric()
	for (r in 1:length(cols)) {
		widths <- c(widths, if (r %in% ok) 16 else -16)
	}
	widths
}

# Get Column Names
getColNames <- function(){
	cols <- read.table(paste(datapath, pathSeparator, "features.txt", sep=""), stringsAsFactors=FALSE)[,2]
	ok <- c(grep("-mean()", cols, fixed = T), grep("-std()", cols, fixed = T))
	names = character()
	for (r in 1:length(cols)) {
		if (r %in% ok) {
			s <- cols[r]
			s <- gsub("\\(", "", s)
			s <- gsub("\\)", "", s)
			s <- gsub("\\-", "_", s)
			names <- c(names, s)
		}
	}
	names
}

# Load Data
loadData <- function(dataFile, labelsFile, subjectFile){
	fileName <- paste(datapath, pathSeparator, dataFile, sep = "")
	result <- read.fwf(fileName, widths = getColWidths())
	colnames(result) <- getColNames()
	fileName <- paste(datapath, pathSeparator, labelsFile, sep = "")
	l <- read.fwf(fileName, widths=c(5))
	names(l) <- c("Activity")
	result$Activity <- l[, "Activity"]
	labels <- read.table(paste(datapath, pathSeparator, "activity_labels.txt", sep=""), sep=" ", stringsAsFactors=FALSE)
	names(labels) <- c("id", "name")
	for (i in 1:nrow(result)) {
		c <- labels[labels$id == result[i, "Activity"], "name"]
		result[i, "Activity"] <- c
	}
	fileName <- paste(datapath, pathSeparator, subjectFile, sep = "")
	l <- read.fwf(fileName, widths=c(5))
	names(l) <- c("N")
	result$N <- l[, "N"]
	result
}


# SQL

getSql <- function(dataset){
	sql <- "SELECT N, Activity"
	names <- getColNames()
	for(n in names){
		sql <- paste(sql, ", AVG(", n, ") AS ", n, sep = "")
	}
	paste(sql, "FROM", dataset, "GROUP BY N, Activity", sep = " ")
}


print("Loading train data set...")
d0 <- loadData("train\\X_train.txt", "train\\Y_train.txt", "train\\subject_train.txt")
print(paste("Loaded", nrow(d0), "rows", sep=" "))

print("Loading test data set...")
d1 <- loadData("test\\X_test.txt", "test\\Y_test.txt", "test\\subject_test.txt")
print(paste("Loaded", nrow(d1), "rows", sep=" "))

print("Calculating...")
d0 <- rbind(d1, d0)
d1 <- NULL
d0 <- sqldf(getSql("d0"))
	
print(paste("Writing file '", outputFile, "'...", sep = ""))
write.csv(d0, file=outputFile, quote=FALSE, row.names=FALSE)
print("Done")
