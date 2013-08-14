
dataDir	<-	'/Users/jread/Desktop/Current Projects/Decadal Water Level/Data/'
files	<-	 c('Buffalo_lvl','Crystal_lvl','Region_GW','Crystal_GW')

plotGW	<-	FALSE


offsets	<-	c(0,-59,0,-23)
if (plotGW){
	colors	<-	c(
		rgb(180,180,180,255,maxColorValue=255),
		rgb(180,180,180,255,maxColorValue=255),
		rgb(0,0,255,255,maxColorValue=255),
		rgb(210,26,154,255,maxColorValue=255))

	bgClr	<- 	c(
		rgb(200,200,200,255,maxColorValue=255),
		rgb(200,200,200,255,maxColorValue=255),
		rgb(140,140,140,255,maxColorValue=255),
		rgb(230,230,230,255,maxColorValue=255))
		figN = "waterLevelGW"
		ltys	<-	c(0,0,1,1)	
		pchs	<-	c(21,21,23,23)
		lwds	<-	c(1.2,1.2,2.5,2.5)
		cexs	<-	c(1.4,1.4,.65,.65)
} else {
	colors	<-	c(
		rgb(0,0,255,255,maxColorValue=255),
		rgb(210,26,154,255,maxColorValue=255),
		NULL,NULL)
	bgClr	<- 	c(
		rgb(140,140,140,255,maxColorValue=255),
		rgb(230,230,230,255,maxColorValue=255),
		NULL,NULL)
		figN = "waterLevel"
		ltys	<-	c(1,1,1,1)	
		pchs	<-	c(21,21,23,23)
		lwds	<-	c(2.2,2.2,2.5,2.5)
		cexs	<-	c(1.4,1.4,.8,.8)
}

	


figW  <- 10
figH  <- 6.25
lM    <-1.35
bM    <-.75
rM    <-.15
tM    <-.15
fRes  <- 200
fontN <- 11
xL    <- c(as.POSIXct('1980-01-01'),as.POSIXct('1995-01-01'))# c(as.POSIXct('1940-01-01'),as.POSIXct('2013-01-01'))
yL1    <- c(-145,95)
yL2    <- c(-45,45)

output = paste("Figures/",figN,".png", sep = "")
png(output, width=figW, height=figH, units="in",res=fRes)
par(mai=c(bM,lM,rM,tM),cex=1.6)
ylabel <- 'Lake surface elevation (cm)'
plot(xL,yL1,type="n",xlab=NA,ylab=ylabel,
     font=fontN,font.lab=fontN,tcl=-.2,xaxs="i",cex.lab=1.3,cex=1.5)

lines(xL,c(0,0),col="grey24",lwd=1.8,pch=1,lty=2)
for (i in 1:length(files)){
	paste(paste(dataDir,files[i],'.txt'))
	data	<-	read.table(paste(dataDir,files[i],'.txt',sep=""),
		header=TRUE,sep='\t',na.strings=c('na','NA'))
	dates	<-	as.POSIXct(data[,1])
	level	<-	data[,2]+offsets[i]
	lines(dates,level,col=colors[i], lty=ltys[i], lwd=2.5)
	lines(dates,level,col=colors[i], 
		pch=pchs[i],type="b", lty=0, lwd=lwds[i],cex=cexs[i],
		bg=bgClr[i]) 
	
}



dev.off()