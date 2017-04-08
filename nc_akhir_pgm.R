library(ncdf4)

nc_akhir_pgm <- function(waktu,nama_var,nama_output,lon,lat){ ## akhir hasil replikasi input merg netcdf
	if(class(nama_output) != "character"){
		print("harus string !!! maka dari itu diganti string")
		nama_output<-as.character(nama_output)
	} else{
		print("lanjut!!")
		nama_output<-nama_output
	}
	if(class(nama_var) != "matrix"){
		print("kudu matrix!! maka dari itu bikin matrix dulu")
		nama_var<-as.matrix(nama_var)
	} else{
		print("lanjut !!")
		nama_var<-nama_var
	}
	dimtime <- ncdim_def('time', units=waktu, vals=1)
	dimlon <- ncdim_def(name='longitude', units='degrees_east', 
						longname='Longitude', vals=lon )
	dimlat <- ncdim_def(name='latitude', units='degrees_north', 
						longname='Latitude', vals=lat )
	varch4 <- ncvar_def(name='ch4', units='', dim=list(dimlon,dimlat, dimtime),
						longname='IR BT (add 75 to this value)',prec = 'float')
	outputfile <- sprintf('../output_nc4/%s',nama_output);
	con <- nc_create(outputfile, varch4)
	ncatt_put(con, varch4, 'comments', 'Unknown1 variable comment')
	ncatt_put(con, varch4, 'units', '')
	ncatt_put(con, varch4, 'grid_name', 'grid01')
	ncatt_put(con, varch4, 'grid_type', 'linear')
	ncatt_put(con, varch4, 'level_description', 'Earth surface')
	ncatt_put(con, varch4, 'time_statistic', 'instantaneous')
	ncatt_put(con, varch4, 'missing_value', 330)
	# <<<<<<<<<<<< GLOBAL ATRIBUT >>>>>>>>>>>> #
	ncatt_put(con,0,"Conventions","COARDS")
	ncatt_put(con,0,"calendar","standard")
	ncatt_put(con,0,"comments","File")
	ncatt_put(con,0,"model","geos/das")
	ncatt_put(con,0,"center","gsfc")
	# <<<<<<<<<<<<<< GABUNG DIMENSI-VARIABEL-ATRIBUT >>>>>>>>>>>>>#
	ncvar_put(con, varch4, nama_var)
	nc_close(con)
}
