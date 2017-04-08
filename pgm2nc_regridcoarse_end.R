rm(list = ls())
library(pixmap)
library(ncdf4)
setwd('~/pgm2nc_end/skrip/')
try(system("gunzip ../data_awal/*.gz ", intern = TRUE, ignore.stderr = TRUE))
try(system("rm ls_pgm.txt ", intern = TRUE, ignore.stderr = TRUE))
try(system("find ../data_awal/ -type f -not -name '*.pgm' -print0 | xargs -0 rm --", 
		   intern = TRUE, ignore.stderr = TRUE))
try(system("rm -R -- ../data_awal/*/", intern = TRUE, ignore.stderr = TRUE))
try(system("ls ../data_awal/ > ls_pgm.txt", intern = TRUE, ignore.stderr = TRUE))
nama_lama <- read.table("ls_pgm.txt",header=F)
nama_lama <- unlist(t(nama_lama))
tanggal <- substr(nama_lama,5,12)
nama_baru <- sprintf("merg_20%s_4km-pixel.nc",tanggal)
panjangjamdata <- length(nama_baru)
jam_running <- 1:length(nama_baru)
waktu_nc_wib <- seq(c(ISOdate(as.numeric(substr(nama_baru[1],6,9)),
							  as.numeric(substr(nama_baru[1],10,11)),
							  as.numeric(substr(nama_baru[1],12,13)), 
							  as.numeric(substr(nama_baru[1],14,15)) )), 
					by = "hour", length.out = length(jam_running))

# <<<<<<< PILIH REGRID DALAM  km, DEFAULT DATA 4 km >>>>>> #
m <- 4 ; n <- 4  # <<<< m = RESOLUSI BUJUR, n =  RESOLUSI LINTANG >>>> #
if( m < 4 ){      
	if( n < 4 ){
		n <- 4; m <- 4
	}
}
skiplon <- as.integer(m/4);
skiplat <- as.integer(n/4)

# <<<<<<<<< INPUT PGM >>>>>>>>> #
nama_input <- nama_lama

# <<<<<<<< INPUT WAKTU >>>>>>>> #
beda_waktu <- 7  # <<<< LOCAL TIME MINUS GMT >>>> #
waktu_nc_gmt <- sprintf("%s GMT",substr(waktu_nc_wib-(3600*beda_waktu),1,19))
YYYYMMDD <- substr(waktu_nc_gmt,1,10)
HH <- as.numeric(substr(waktu_nc_gmt,12,13))
waktu_nc_gmt <- sprintf("hours since %s %d",YYYYMMDD,HH) 

# <<<<<<<< OUTPUT NC4 >>>>>>>>> #
nama_output <- nama_baru

# <<<<<<<< DATA-DEPENDENSI >>>> #
source("nc_akhir_pgm.R")
source("regrid_bilinear.R")
load("../dependen_input/kal.RData") ## <<<< variable kalibrasi >>>> ##
load("../dependen_input/lat_lon.RData")

# <<<<<<< FUNGSI KALIBRASI ---> OOP OUTPUT NetCDF4 >>>>>> #  
pgm2nc <- function(waktu,xin1,xout1,skipbujur,skiplintang){
	ir <- read.pnm(file = sprintf("../data_awal/%s",xin1)) # var
	ir <- getChannels(ir)
	data <- ir[1101:1701,401:1601]*255
	data_ir <- data
	for(i in 0:(length(kal)-1)){
		data_ir[data_ir==i]=kal[i+1]
	}
	putar90 <- function(x){
		t(apply(x, 2, rev))
	}
	data_ir <- putar90(data_ir)
	data_ir <- regrid_data(data_ir,lon,lat,skipbujur,skiplintang)
	Rlon <- lon[seq(1,length(lon),by=skipbujur)]
	Rlat <- lat[seq(1,length(lat),by=skiplintang)]
	nc_akhir_pgm(waktu,data_ir,xout1,Rlon,Rlat) 
}

# <<<<<< ITERASI AKHIR >>>>>> #

for(i in 1:length(nama_input)){
	pgm2nc(waktu_nc_gmt[i],nama_input[i],nama_output[i],skiplon,skiplat)
}



