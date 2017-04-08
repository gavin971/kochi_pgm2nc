regrid_data <- function(data,bujur,lintang,
						skip_bujur,skip_lintang)
{
	Sbujur <- toString(format(bujur, nsmall = 2));
	Sbujur <- strsplit(Sbujur,", ");Sbujur <- unlist(Sbujur)
	Slintang <- toString(format(lintang, nsmall = 2)); 
	Slintang <- strsplit(Slintang,", "); 
	Slintang <- unlist(Slintang)
	colnames(data) <- Slintang
	rownames(data) <- Sbujur
	## <<<<<< regrid m x n >>>>>> ##
	Rlintang <- lintang[seq(1,length(lintang),skip_lintang)]
	Rbujur <- bujur[seq(1,length(bujur),skip_bujur)]
	SRbujur <- toString(format(Rbujur, nsmall = 2));
	SRbujur <- strsplit(SRbujur,", ");SRbujur<-unlist(SRbujur)
	SRlintang <- toString(format(Rlintang, nsmall = 2)); 
	SRlintang <- strsplit(SRlintang,", "); 
	SRlintang <- unlist(SRlintang)
	hasil <- data[SRbujur,SRlintang]
	return(hasil)
}