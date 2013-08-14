create.corrmap	<-	function(plot.title,dat_grid,p_grid,corr.lims=c(-0.4,0.4)){
	
	# map used is 0 to 360º
	dat_grid$value[dat_grid$value<corr.lims[1]] <- corr.lims[1]
	dat_grid$value[dat_grid$value>corr.lims[2]] <- corr.lims[2]
	
	xLim<- c(120,340)
	yLim<- c(15,75)
	
	require(ggplot2)
	require(maps)
	world<- map('world2',interior=FALSE,plot=FALSE,fill=TRUE)
	head(fortify(world))
	heat.plot<- (ggplot()+geom_raster(data=dat_grid,aes(x,y,fill=value))+
	  geom_contour(data=p_grid,aes(x,y,z=value),colour='white',size=2,bins=1)+
	  geom_path(aes(long,lat,group=group),data=world)+
	  scale_fill_gradient2(mid='grey90', high="red", low="blue",na.value='white',guide='legend',limits=corr.lims)+
	  labs(list(title = plot.title))+
	  theme_bw())

	heat.plot+
	  scale_x_continuous(name='',labels = c('160ºW','160ºE','120ºE','80ºE','40ºE'),breaks=c(160,200,240,280,320),limits=xLim)+
	  scale_y_continuous(name='',labels = c('20ºN','40ºN','60ºN'),breaks=c(20,40,60),limits=yLim)+
	  coord_cartesian(xlim = xLim,ylim=yLim)+
	  theme(legend.position = c(.92, .32),
	        text = element_text(size = 16,family = 'serif'),
	        title = element_text(size=14,family='serif'),
	        legend.title=element_blank(),
	        legend.background=element_blank(),
	        plot.margin = unit(c(0,0.5,0,0), "cm"),panel.background=element_blank())
	
}