get.delstage	<- function(years,months,day=c(1,1)){
	
	# months is array of numeric months
	# years is an array of numeric years
	if (missing(months)){
		months	<-	c(4,9)
		day=c(1,1)
	}
	if (missing(years)){
		years	<-	seq(1943,2010,1)
	}
	# returns array of AVERAGE delstage during the time period for each year. NA if blank
	root.dir	<-	'../data/'
	
	files	<-	c('Buffalo_lvl.txt','Crystal_lvl.txt')
	
	del.array	<-	matrix(nrow=length(years),ncol=length(files))
	for (i in 1:length(files)){
		stage	<-	read.table(paste(root.dir,files[i],sep=''),header = TRUE,
			sep = "\t")
			date	<-	as.POSIXct(stage$date)
			elev	<-	as.numeric(stage$elevation)
			
			# for each year, find two points for delstage
			for (j in 1:length(years)){
				t_1	<-	as.POSIXct(paste(years[j],months[1],day[1],sep='-'))
				t_2	<-	as.POSIXct(paste(years[j],tail(months,1),tail(day,1),sep='-'))
				stage.vals	<-	approx(date,elev,c(t_1,t_2))
				del.array[j,i]	<-	stage.vals$y[2]-stage.vals$y[1]
			}
	}
	dof	<-	function(values){
		return(mean(values,na.rm=TRUE))
	}
	
	delstage	<-	apply(del.array,1,dof)
	return(delstage)
}
