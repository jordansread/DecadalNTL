create.corrmap	<-	function(plot.title,dat_grid,p_grid,output='output',corr.lims=c(-0.4,0.4)){
	
	image.file	<-	paste('../Figures/',output,'.pdf',sep='')
	# map used is 0 to 360º
	dat_grid$value[dat_grid$value<corr.lims[1]] <- corr.lims[1]
	dat_grid$value[dat_grid$value>corr.lims[2]] <- corr.lims[2]
	
	figW  <- 7
	figH  <- 3.25
	xLim<- c(121,339)
	yLim<- c(16,74)
	
	require(ggplot2)
	require(maps)
	world<- map('world2',interior=FALSE,plot=FALSE,fill=TRUE)
	head(fortify(world))
	heat.plot<- (ggplot()+geom_raster(data=dat_grid,aes(x,y,fill=value))+
	  geom_contour(data=p_grid,aes(x,y,z=value),colour='white',size=1.,bins=1,linetype = 5)+
	  geom_path(aes(long,lat,group=group),data=world)+
	  scale_fill_gradient2(mid='grey80', high="red", low="blue",na.value='white',guide='legend',limits=corr.lims)+
	  labs(list(title = plot.title))+
	  theme_bw())

	image	<-	heat.plot+
	  scale_x_continuous(name='',labels = c('160ºW','160ºE','120ºE','80ºE','40ºE'),breaks=c(160,200,240,280,320),limits=xLim)+
	  scale_y_continuous(name='',labels = c('20ºN','40ºN','60ºN'),breaks=c(20,40,60),limits=yLim)+
	  coord_cartesian(xlim = xLim,ylim=yLim)+
	  theme(legend.position = c(.92, .33),
	        text = element_text(size = 16,family = 'serif'),
	        title = element_text(size=14,family='serif'),
	        legend.title=element_blank(),
	        legend.background=element_blank(),
	        plot.margin = unit(c(0,0.5,0,0), "cm"),panel.background=element_blank())
	
	ggsave(file=image.file, plot=image, width=figW, height=figH)
}