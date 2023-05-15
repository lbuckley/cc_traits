for(dat.k in 1:6){ 
  
  if(dat.k==1){
    #MAMMALS
    #to long format
    data.l<- melt(mammals, id.vars = c("Taxon", "High_change"))
    
    plot.d1= ggplot(data.l) + aes(x=value, y = High_change)+geom_point()+
      facet_wrap(.~variable, scales="free_x")+
      ggtitle('Mammals')+
      theme_bw(base_size=14)
    
  }
  
  if(dat.k==2){
    #PLANTS
    #to long format
    data.l<- melt(plants, id.vars = c("Taxon", "migration_m"))
    
    plot.d2= ggplot(data.l) + aes(x=value, y = migration_m)+geom_point()+
      facet_wrap(.~variable, scales="free_x")+
    ggtitle('Alpine plants')+
      theme_bw(base_size=14)
  
  # #to long format factor
  # data.l<- melt(plants, id.vars = c("Taxon", "migration_m"))
  # 
  # ggplot(data.l[data.l$variable==c("DispersalMode", "BreedSysCode","nichebreadth_amplit_ocean"),], 
  #        aes(migration_m, color=factor(value)))+geom_density()+
  #          facet_wrap(.~variable)
  
  }
  
  if(dat.k==3){
    #FISH
    #to long format
    data.l<- melt(fish, id.vars = c("Species", "Latitudinal.Difference"))
    
    plot.d3= ggplot(data.l) + aes(x=value, y = Latitudinal.Difference)+geom_point()+
      facet_wrap(.~variable, scales="free_x")+
    ggtitle('Fish')+
      theme_bw(base_size=14)
  }
  
  if(dat.k==4){
    #EURO PLANTS
    #to long format
    data.l<- melt(eplants, id.vars = c("speciesname", "LeadingEdge"))
    
    plot.d4= ggplot(data.l) + aes(x=value, y = LeadingEdge)+geom_point()+
      facet_wrap(.~variable, scales="free_x")+
      ggtitle('European plants')+
      theme_bw(base_size=14)
  }
  
  if(dat.k==5){
    #LEPS
    #to long format
    data.l<- melt(lep, id.vars = c("Species", "Taxonomic.group","D_border_0.9"))
    
    plot.d5= ggplot(data.l) + aes(x=value, y = D_border_0.9)+geom_point()+
      facet_wrap(.~variable, scales="free_x")+ 
      ggtitle('Lepidopterans')+
      theme_bw(base_size=14)
  }
  
  if(dat.k==6){
    #BIRDS
    #to long format
    data.l<- melt(bird, id.vars = c("Species", "Taxonomic.group","D_border_0.9"))
    
    plot.d6= ggplot(data.l) + aes(x=value, y = D_border_0.9)+geom_point()+
      facet_wrap(.~variable, scales="free_x")+ 
      ggtitle('Birds')+
      theme_bw(base_size=14)
  }
} #end loop datasheets

#setwd for figures
setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/figures/")

pdf("Fig0_alltraits.pdf", onefile = TRUE)
print(plot.d1)
print(plot.d2)
print(plot.d3)
print(plot.d4)
print(plot.d5)
print(plot.d6)
dev.off()

#--------------
#SELECT PANELS
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
