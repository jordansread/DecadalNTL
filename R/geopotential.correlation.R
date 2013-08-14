source('get.delstage.R')
source('create.corrmap.R')
require(ncdf4)
library(Hmisc)

corr.stage	<-	TRUE
p.val  <-	0.05
corr.method  <-	'pearson'

# stopped here...

geopot.nc	<- 	nc_open('../data/hgt.mon.mean.nc')
Stage	<-	read.table('../data/year_stage.tsv',header = TRUE,
	sep = "\t")
if (corr.stage){
	correlate	<- Stage$Agg_S_anomaly[6:length(Stage$Agg_S_anomaly)] # starting in 1948...
	yyyy.use	<-	seq(48, 111, 1)
  plot.title<- 'Correlation between GPH and lake stage'
  plot.output<- 'stageCorrGPH'
} else {
	year.use  <-	seq(1948, 2010, 1)
	correlate	<-	get.delstage(year.use,c(4,9))
	yyyy.use	<-	seq(48, 110, 1)
	plot.title<- 'Correlation between GPH and change in lake stage'
	plot.output<- 'delStageCorrGPH'
}



level.use	<-	500

month.use	<-	c("Apr","May","Jun","Jul","Aug","Sep")


timeUnits	<- 	ncatt_get(geopot.nc,'time','units')$value
tiCheck <- regexpr('(hours since) (.*)' ,timeUnits, perl=TRUE)
hoursSince  <-  ncvar_get(geopot.nc, "time")
#make sure the unit string is as expected. I think 
# the timestep is always in hours
if(attr(tiCheck,'capture.start')[1] < 0 || attr(tiCheck,'capture.start')[2] < 0){
	stop('Unexpected time unit in NetCDF file')
}

# Get the epoch from the unit string
epoch <- as.POSIXct(substr(timeUnits, attr(tiCheck,'capture.start')[2], attr(tiCheck,'capture.start')[2] + attr(tiCheck,'capture.length')[2]))

time	<-	as.POSIXct(epoch+(hoursSince*86400/24))
month.i	<-	which(months(time,TRUE) %in% month.use)

level	<-	ncvar_get(geopot.nc,"level")
z.i	<-	which(level==level.use)

lat	<-	ncvar_get(geopot.nc,'lat')
lon	<-	ncvar_get(geopot.nc,'lon')

corr	<-	matrix(nrow=length(lon),ncol=length(lat))
pvals  <-	matrix(0,nrow=length(lon),ncol=length(lat))
for (x in 1:length(lon)){
	for (y in 1:length(lat)){
		# this is lat/lon loop --
		geo.height	<-	ncvar_get(geopot.nc, "hgt",start=c(x,y,z.i,1),count=c(1,1,1,-1)) # get all time
		#Order is X-Y-Z-T

		value	<-	vector(length=length(yyyy.use))

		geo.height	<-	geo.height[month.i]
		years	<-	as.double(as.POSIXlt(time[month.i])$year)

		for (i in 1:length(yyyy.use)){
			value[i]	<-	mean(geo.height[years==yyyy.use[i]])
			
		}
		corr.matrix	<-	matrix(data=c(value,correlate),ncol=2,nrow=length(value))
		corr.val	<-	rcorr(corr.matrix,type=corr.method)
		if (!is.na(corr.val$P[1,2])){
		  if (corr.val$P[1,2] < p.val){
		    pvals[x,y]	<-	1 # else it will remain 0
		  }
		  corr[x,y]	<-	corr.val$r[1,2]
		}
	}
}

#lon=lon-180
#lon[lon<(-180)]=lon[lon<(-180+360)]
dat_grid	<-	expand.grid(x=lon,y=lat)
dat_grid$value	<-	as.vector(corr)#as.vector(m)
p_grid	<-	expand.grid(x=lon,y=lat)
p_grid$value	<-	as.vector(pvals)

create.corrmap(plot.title,dat_grid,p_grid,output=plot.output)