# This function runs c3 algoirthm on all datasets in subdirectories of current directory

runAll <- function(mainpath="." , n){

subdirs <- list.dirs()

# ("precision", "F-score", "recall", "TP", "FP", "FN")
result <- matrix(nrow=length(subdirs), ncol=6)
colnames(result) <- c("precision", "F-score", "recall", "TP", "FP", "FN")


for(i in 1:length(subdirs)){
	if(subdirs[i] != "."){
		dataFileName <- paste(subdirs[i], '/dataset.txt',sep="")
		trueNetFileName <- paste(subdirs[i], '/trueNet.txt',sep="")
		net <- NULL
		
		#start of procedure
		#data(expdata)
		expdata <- read.table(dataFileName)
		#fetching the example true network of the data set
		#data(truenet)
		truenet < read.table(trueNetFileName)
		
		#copula transforming the data set
		expdata <- copula(expdata)

		#set a significance threshold value
		alpha <- 0.01

		#set an iteration number for statistical analysis in sigtestMTC
		itnum <-5

		# eliminate nonsignificant MI valued edges by BH method. Another MTC method can be changed if desired.
		res <- sigtestMTC( expdata, alpha, itnum, methodsig="BH")

		#The Step 2 of C3NET algorithm is applied. 
		net <- c3(res$Inew, n) # regulatory network inferred (non zero elements stand for links of the predicted network)

		# the predicted network validation
		# ("precision", "F-score", "recall", "TP", "FP", "FN")
		score <- checknet(net,truenet)

		result[i,1] <- score[1]
		result[i,2] <- score[2]
		result[i,3] <- score[3]
		result[i,4] <- score[4]
		result[i,5] <- score[5]
		result[i,6] <- score[6]
	}#if		
	
}#for

result

}