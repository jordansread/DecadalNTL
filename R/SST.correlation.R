# have to read messed up file format:

#     ----- ----- -----
#    |     |     |     |
#    | DAY | MON | YR  |
#    |_____|_____|_____|____________________________
#90N |(1,1)                                         |
#    |                                              |
#    |                                              |
#    |                                              |
#    |                                              |
#    |(1,90)                                        |
#Equ |                                              |
#    |(1,91)                                        |
#    |                                              |
#    |                                              |
#    |                                              |
#    |                                              |
#90S |(1,180)______________________________(360,180)|
#     180W                 0                    180E

#Temperatures are stored as degrees C * 100
#100% sea-ice-covered gridboxes are flagged as -1000
#Land squares are set to -32768

# 1) read in files
# 2) IF month is in array, store value and find others for averaging
# 3) populate matrix with YEARLY values

source('get.delstage.R')
source('create.corrmap.R')
library(Hmisc)


corr.method	<-	'pearson'
p.val  <-	.05

month.use	<-	c(4,5,6,7,8,9) # FIXXXX

corr.stage	<-	TRUE
Stage	<-	read.table('../data/year_stage.tsv',header = TRUE,
	sep = "\t")
if (corr.stage){
	correlate	<- Stage$Agg_S_anomaly # starting in 1943...
	year.use	<-	seq(1943, 2011, 1)
	plot.title<-"Correlation between SST and lake stage"
	plot.output	<-	"stageCorrSST"
} else {
	year.use	<-	seq(1943, 2010, 1)
	correlate	<-	get.delstage(year.use,month.use)
	plot.title<-"Correlation between SST and change in lake stage"
	plot.output	<-	"delstageCorrSST"
}


MM.dim	<-	length(month.use)
time.dim	<-	length(year.use)
long.dim	<-	360
lat.dim	<-	180
SST	<-	array(NA, dim=c(long.dim,lat.dim,time.dim))
holding	<-	array(NA, dim=c(long.dim,lat.dim,MM.dim))

rootDir	<-	'../data/SST files/'
fileN	<-	c(
	'HadISST1_SST_1931-1960.txt',
	'HadISST1_SST_1961-1990.txt',
	'HadISST1_SST_1991-2003.txt',
	'HadISST1_SST_2004.txt',
	'HadISST1_SST_2005.txt',
	'HadISST1_SST_2006.txt',
	'HadISST1_SST_2007.txt',
	'HadISST1_SST_2008.txt',
	'HadISST1_SST_2009.txt',
	'HadISST1_SST_2010.txt',
	'HadISST1_SST_2011.txt')

for (f in 1:length(fileN)){
	line.num	<-	1

	# find number of lines...
	c <- file(paste(rootDir,fileN[f],sep=''),"r") #
	fileLines <- readLines(c)
	close(c)
	nRead <- length(fileLines)
	con <- file(paste(rootDir,fileN[f],sep=''), "rt") 

	while (line.num<nRead){
		head.line <-	readLines(con, 1) # first line is headers, which will give time...
		line.num	<-	line.num+1
		head.line	<-	strsplit(head.line,' ')
		mm	<- as.numeric(head.line[[1]][11])
		yyyy	<-	as.numeric(head.line[[1]][13])
		if (is.na(mm)){
			mm	<- as.numeric(head.line[[1]][10])
			yyyy	<-	as.numeric(head.line[[1]][12])
		}

		# -- now loop through file --

		data.lines	<-	readLines(con, 180)#?
		line.num	<-	line.num+180
		if (mm %in% month.use && yyyy %in% year.use){
			# do things w/ the data
			data.lines	<-	gsub("  "," ",data.lines)
			data.lines	<-	gsub("  "," ",data.lines)
			data.lines	<-	gsub("-32768"," -32768",data.lines)
			data.holding	<-	strsplit(data.lines,' ')
			for (i in 1:lat.dim){
				holding[,i,which(mm==month.use)] 	<-	data.holding[[i]][data.holding[[i]]!=""]
			}
			#data.holding	<-	data.holding[data.holding!=""]
		}

		if (mm==tail(month.use,1) && yyyy %in% year.use){ 
			for (i in 1:lat.dim){
				tHold	<-	apply(holding[,i,],c(1,2),as.numeric)*0.01 # longitude and time, convert to 
				tHold[tHold<0]	<-	NA
				SST[,i,which(yyyy==year.use)]	<-	apply(tHold,1,mean)
			}
		}

	}
	print(paste('done with ',fileN[f],sep=' '))
	close(con)
}

corr	<-	matrix(nrow=long.dim,ncol=lat.dim)
pvals	<-	matrix(0,nrow=long.dim,ncol=lat.dim)

for (x in 1:long.dim){
	for (y in 1:lat.dim){

		corr.matrix	<-	matrix(data=c(SST[x,y,],correlate),ncol=2,nrow=length(correlate))
		corr.val	<-	rcorr(corr.matrix,type=corr.method)
		if (!is.na(corr.val$P[1,2])){
			if (corr.val$P[1,2] < p.val){
				pvals[x,y]	<-	1 # else it will remain 0
			}
			corr[x,y]	<-	corr.val$r[1,2]
		}
	}
}

lon	<-	seq(-180,179,1)
lat	<-	seq(90,-89,-1)

lon[lon<0] = lon[lon<0]+360
dat_grid	<-	expand.grid(x=lon,y=lat)
dat_grid$value	<-	as.vector(corr)#as.vector(m)
p_grid	<-	expand.grid(x=lon,y=lat)
p_grid$value	<-	as.vector(pvals)

create.corrmap(plot.title,dat_grid,p_grid,output=plot.output,corr.lims=c(-0.4,0.4),map.layer='top')
