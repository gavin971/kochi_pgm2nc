rm(list = ls())
library(pixmap)
library(ncdf4)
# setwd('~/pgm2nc_end/skrip/')
saya <- try(system("whoami", intern = TRUE, ignore.stderr = TRUE))
setwd(sprintf('/home/%s/pgm2nc_end/skrip/',saya))
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

# <<<<<<<< BUG WAKTU >>>>>>> #
  
YEAR <- sprintf("20%s",substr(nama_input[1],5,6))
bulan  <- sprintf("%s",substr(nama_input[1],7,8))
start_date <- '01' #DD 
start_hour <- '00:00:00' #HH  
end_date <- sprintf("%s",substr(nama_input[length(nama_input)],9,10))
end_hour <-'23:00:00' #HH 
st_time <- sprintf('%s-%s-%s %s',YEAR,bulan,start_date,start_hour) 
end_time <- sprintf('%s-%s-%s %s',YEAR,bulan,end_date,end_hour) 
time_index <- seq(as.POSIXct(st_time), as.POSIXct(end_time), "hour") 
miss_index <- sprintf("MTS2%s%s%s%sIR1.pgm.gz",substr(time_index,3,4),substr(time_index,6,7),
        substr(time_index,9,10),substr(time_index,12,13))

missing_file <- read.csv2("../missing_file/Oct_2011.txt",header=F,sep = "\t")
missing_file <- as.vector(missing_file[2:dim(missing_file)[1],2])
integer_index <- 1:length(time_index); 

final_index_missing <- 1:length(missing_file)
for(i in 1:length(missing_file)){
  final_index_missing[i] <- integer_index[miss_index==missing_file[i]]
}

waktu_nc_wib <- time_index[! time_index %in% time_index[final_index_missing]]


# <<<<<<<< INPUT WAKTU >>>>>>>> #
# beda_waktu <- 7  # <<<< LOCAL TIME MINUS GMT >>>> #
# waktu_nc_gmt <- sprintf("%s GMT",substr(waktu_nc_wib-(3600*beda_waktu),1,19))
waktu_nc_gmt <- sprintf("%s GMT",substr(waktu_nc_wib,1,19))
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



