rootDir	<-	'../data/'
source('get.delstage.R')
data = read.table(paste(c(rootDir,'year_stage.tsv'),collapse=''),header=TRUE)

# create a centered stage, set as the yearly average
stageCentered	<-	approx(data$Year, data$Agg_S_anomaly,data$Year[1:(length(data$Year)-1)]+.5)$y
delStage	<-	diff(data$Agg_S_anomaly)

delStage	<-	get.delstage(years=seq(1943,2010,1),months=c(4,5,6,7,8,9),day=c(1,1))
Stage	<-	data$Agg_S_anomaly[1:length(data$Agg_S_anomaly)-1]
data$Year
Find_Max_CCF<- function(Stage,delStage)
{
 d <- ccf(Stage, delStage, plot = TRUE,ylab = "cross-correlation",xlim=c(-10,-1))
 cor = d$acf[,,1]
 lag = d$lag[,,1]
 res = data.frame(cor,lag)
 res_max = res[which.max(res$cor),]
 return(res_max)
}

res_max	<- Find_Max_CCF(stageCentered,delStage)
print(res_max)

res_max	<- Find_Max_CCF(delStage,Stage)
print(res_max)