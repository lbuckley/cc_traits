for(dat.k in 1:6){ 
  
  if(dat.k==1){
    #MAMMALS
    #to long format
    data.l<- melt(mammals, id.vars = c("Taxon", "High_change"))
    
    plot.d1= ggplot(data.l) + aes(x=value, y = High_change)+geom_point()+
      facet_wrap(.~variable, scales="free_x")
      ggtitle('Mammals')+
      theme_bw(base_size=14)
    
  }
  
  if(dat.k==2){
    #PLANTS
    #to long format
    data.l<- melt(plants, id.vars = c("Taxon", "migration_m"))
    
    plot.d2= ggplot(data.l) + aes(x=value, y = migration_m)+geom_point()+
      facet_wrap(.~variable, scales="free_x")
    ggtitle('Alpine plants')+
      theme_bw(base_size=14)
  
    #### UPDATE
  #to long format factor
  data.l<- melt(plants, id.vars = c("Taxon", "migration_m"))
  
  ggplot(data.l, aes(migration_m, color=variable))+geom_density()
  
  }
  
  if(dat.k==3){
    #FISH
    #set up data
    dat=na.omit(fish)
    dat$y= dat$Latitudinal.Difference
    #drop ID rows
    dat<- dat[,3:ncol(dat)]
    
    #set up ordered factors
    dat$habitat= factor(dat$habitat, ordered=TRUE)
    dat$WaterType= factor(dat$WaterType, ordered=TRUE)
  }
  
  if(dat.k==4){
    #EURO PLANTS
    #set up data
    dat=na.omit(eplants)
    dat$y= dat$LeadingEdge
    #drop ID rows
    dat<- dat[,3:ncol(dat)]
    
    #set up ordered factors
    # dat$TemperatureIndicator= factor(dat$TemperatureIndicator, ordered=TRUE)
    #  dat$NutrientIndicator= factor(dat$NutrientIndicator, ordered=TRUE)
    #  dat$LifeStrategy= factor(dat$LifeStrategy, ordered=TRUE)
  }
  
  if(dat.k==5){
    #LEPS
    #set up data
    dat=na.omit(lep)
    dat$y= dat$D_border_0.9
    #drop ID rows
    dat<- dat[,4:ncol(dat)]
    
    #set up ordered factors
    dat$wintering= factor(dat$wintering, ordered=TRUE)
    dat$num.gen= factor(dat$num.gen, ordered=TRUE)
  }
  
  if(dat.k==6){
    #BIRDS
    #set up data
    dat=na.omit(bird)
    dat$y= dat$D_border_0.9
    #drop ID rows
    dat<- dat[,4:ncol(dat)]
    
    #set up ordered factors
    dat$wintering= factor(dat$wintering, ordered=TRUE)
    dat$num.gen= factor(dat$num.gen, ordered=TRUE)
  }
  

  
  
  
  
} #end loop datasheets
#--------------
#PLOT PREDICTORS

# alpine plants: seed shed month earliest, number floristic zones
# European plants: temp indicator, seed release height
# Mammals: alt timit, longevity
# Fish: depth, benthopelagic, vulnerability

#----
# alpine plants: seed shed month earliest, number floristic zones

plot1a= ggplot(plants) + aes(x=earliest_seed_shed_mo, y = migration_m)+geom_point()+
  xlab("Earliest seed shed month")+ylab("Elevation shift (m)")+ 
  ggtitle('A. Alpine plants')+
  theme_bw(base_size=14)

plot1b= ggplot(plants) + aes(x=seed_shed_dur_mos, y = migration_m)+geom_point()+
  xlab("Seed shed duration (mo)")+ylab("Elevation shift (m)")+ 
  ggtitle('A. Alpine plants')+
  theme_bw(base_size=14)

#----
# European plants: temp indicator, seed release height

plot1c= ggplot(eplants) + aes(x=TemperatureIndicator, y = LeadingEdge)+geom_point()+
  xlab("Temperature indicator")+ylab("Elevation shift (m)")+ 
  ggtitle('B. European plants')+
  theme_bw(base_size=14)

plot1d= ggplot(eplants) + aes(x=Dispersal, y = LeadingEdge)+geom_point()+
  xlab("Dispersal")+ylab("Elevation shift (m)")+ 
  ggtitle('B. European plants')+
  theme_bw(base_size=14)

#check units

#----
# Mammals: alt timit, longevity

plot1e= ggplot(mammals) + aes(x=Orig_high_limit, y = High_change)+geom_point()+
  xlab("Original altitude (m)")+ylab("Elevation shift (m)")+ 
  ggtitle('C. Mammals')+
  theme_bw(base_size=14)

plot1f= ggplot(mammals) + aes(x=Longevity_yrs, y = High_change)+geom_point()+
  xlab("Longevity (years)")+ylab("Elevation shift (m)")+ 
  ggtitle('C. Mammals')+
  theme_bw(base_size=14)+
  theme(axis.title.x = element_text(margin = margin(t = -15)))

#----
# Fish: depth, benthopelagic, vulnerability

plot1g= ggplot(fish) + aes(x=DepthRangeDeep, y = Latitudinal.Difference)+geom_point()+
  xlab("Depth range (m)")+ylab("Latitudinal shift (°)")+ 
  ggtitle('D. Triennial marine survey')+
  theme_bw(base_size=14)+
  theme(axis.title.x = element_text(margin = margin(t = -15)))

fish$habitat= factor(fish$DemersPelag, 
                     levels=c("bathydemersal","demersal","benthopelagic","pelagic-oceanic","pelagic-neritic","reef-associated"))

plot1h= ggplot(fish) + aes(x=habitat, y = Latitudinal.Difference)+geom_point()+
  xlab("Habitat")+ylab("Latitudinal shift (°)")+ 
  ggtitle('D. Triennial marine survey')+
  theme_bw(base_size=14)+ 
  theme(axis.text.x = element_text(angle = 45))+
  theme(axis.title.x = element_text(margin = margin(t = -15)))

#----
#combine
#(plot1a | plot1b) / (plot1c | plot1d) / (plot1e | plot1f) / (plot1g | plot1h)

#setwd for figures
setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/figures/")

pdf("Fig0.pdf", height = 8, width = 10)
(plot1b | plot1d) / (plot1f | plot1h)
dev.off()

#supplementary plots
pdf("FigSm.pdf", height = 12, width = 12)
plot.m
dev.off()

pdf("FigStms.pdf", height = 12, width = 12)
plot.tms
dev.off()

pdf("FigSap.pdf", height = 12, width = 12)
plot.ap
dev.off()

pdf("FigSep.pdf", height = 12, width = 12)
plot.ep
dev.off()
