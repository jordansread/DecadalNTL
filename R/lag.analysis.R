rootDir	<-	'/Users/jread/Desktop/Science Projects/Watras_decadal/'

data = read.table(paste(c(rootDir,'year_stage.tsv'),collapse=''),header=TRUE)

# create a centered stage, set as the yearly average
stageCentered	<-	approx(data$Year, data$Agg_S_anomaly,data$Year[1:(length(data$Year)-1)]+.5)$y
delStage	<-	diff(data$Agg_S_anomaly)

Find_Max_CCF<- function(Stage,delStage)
{
 d <- ccf(Stage, delStage, plot = TRUE,ylab = "cross-correlation")
 cor = d$acf[,,1]
 lag = d$lag[,,1]
 res = data.frame(cor,lag)
 res_max = res[which.max(res$cor),]
 return(res_max)
}

res_max	<- Find_Max_CCF(stageCentered,delStage)
print(res_max)