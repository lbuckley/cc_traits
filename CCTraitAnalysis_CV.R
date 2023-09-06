library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(sjPlot)
library(car)
library(patchwork)
library(ggcorrplot)
library(tidymodels)
library(vip)
library(ISLR)
library(e1071) #SVM models
library(iml) #Interpretable Machine Learning

#----
#read data
mammals= read.csv("./data/mammals_Angertetal2011.csv")
plants= read.csv("./data/plants_Angertetal2011.csv")
lepbird= read.csv("./data/MothsBirds_Hallfors2023.csv")

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/data")
eplants= read.csv("eplants_Rumpfetal2018.csv")
fish= read.csv("fish_Pinskyetal2013.csv")

datasets= c("mammals", "plants","fish", "eplants", "lep", "birds")
dat.titles= c("small mammals", "alpine plants","fish", "plants", "moths", "birds")

dat.labs<- c("F. Small mammals", "A. Alpine plants","D. Fish", "B. Plants", "C. Moths", "E. Birds")
names(dat.labs)<- c("mammals", "plants","fish", "eplants", "lep", "birds")

met.labs<- c("RMSE", "R-squared")
names(met.labs)<- c("rmse", "rsq")

#----
#Process data

#mammals
# Mammals: alt timit, longevity
# https://onlinelibrary.wiley.com/doi/full/10.1111/j.1461-0248.2011.01620.x

#update traits to correspond to Angert et al.
mammals$DietBreadth=0
mammals$DietBreadth[mammals$Food=="omni"]=1
mammals$AnnualRhythm=0
mammals$AnnualRhythm[mammals$Annual_rhythm=="fachib"]=1
mammals$DailyRhythm=0
mammals$DailyRhythm[mammals$Daily_rhythm=="both"]=1

#restrict traits
mammals=mammals[,c("Taxon","High_change","Orig_high_limit","Longevity_yrs","Litters_per_yr","Litter_size","Rangesize_km2","Mass_g","DietBreadth","Bio1_mean","Bio1_std")]
#drop daily and anual rhythm due to spread "DailyRhythm","AnnualRhythm",

#log transform range size and mass
mammals$Mass_g= log(mammals$Mass_g)
mammals$Rangesize_km2= log(mammals$Rangesize_km2)

#check correlations
r <- cor(mammals[,c(3:10)], use="complete.obs")
ggcorrplot(r)

#to long
mammals.l<- mammals %>%
  gather("trait", "value", 3:ncol(mammals))

## Facet labels
#trait.labs <- c("Altitudinal limit (m)","Longevity (yrs)","Litters per yr","Litter size","log Range size (km2)","log Mass (g)","Daily rhythm","Annual rhythm","Diet breadth","Temp mean","Temp breadth")
#names(trait.labs) <- c("Orig_high_limit","Longevity_yrs","Litters_per_yr","Litter_size","Rangesize_km2","Mass_g","DailyRhythm","AnnualRhythm","DietBreadth","Bio1_mean","Bio1_std")
trait.labs.m<- c("Altitudinal limit (m)","Longevity (yrs)","Litters per yr","Litter size","log Range size (km2)","log Mass (g)","Diet breadth","T mean (°C)","T breadth (°C)")
names(trait.labs.m)<- c("Orig_high_limit","Longevity_yrs","Litters_per_yr","Litter_size","Rangesize_km2","Mass_g","DietBreadth","Bio1_mean","Bio1_std")
trait.cat<- c("c","l","l","l","e","l","e","c","c")
#store
trait.labs.all<- cbind(trait.labs.m, names(trait.labs.m), trait.cat)

#plot
plot.mammal= ggplot(mammals.l) + aes(x=value, y = High_change)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs.m)) + 
  ggtitle('F. Small mammals')+
  theme_bw()+ylab("Elevation shift (m)") #+stat_smooth(method='lm', formula= y~poly(x,2))
#+geom_smooth(se=FALSE) #+scale_x_log10()

#scale and center
mammals <- mammals %>%
  mutate(Orig_high_limit = scale(Orig_high_limit),
         Mass_g = scale(Mass_g),
         Bio1_mean = scale(Bio1_mean),
         Bio1_std = scale(Bio1_std),
         Rangesize_km2 = scale(Rangesize_km2))

#make factors
mammals$DietBreadth= factor(mammals$DietBreadth, ordered=TRUE)

#----
#alpine plants

#update traits to correspond to Angert et al.
plants$DispersalMode=NA
plants$DispersalMode[plants$dispersal_mode=="gravity"]=0
plants$DispersalMode[plants$dispersal_mode=="wind" | plants$dispersal_mode=="animal" | plants$dispersal_mode=="water" ]=1

#restrict traits
#plants=plants[,c("Taxon","migration_m","earliest_seed_shed_mo","seed_shed_dur_mos",
#                 "nichebreadth_num_flor_zones", "BreedSysCode",
#                 "Ave_seed_shed_ht_m","flwr_dur_mos","DispersalMode",
#                 "diaspore_mass_mg","nichebreadth_amplit_ocean","Nbound_lat_GBIF_nosyn")]

#restrict variables to increase datasets
plants=plants[,c("Taxon","migration_m","earliest_seed_shed_mo","seed_shed_dur_mos",
                 "Nbound_lat_GBIF_nosyn", "Bio1_mean_nosyn", "Bio1_std_nosyn")]

#check correlations
r <- cor(plants[,c(3:12)], use="complete.obs")
ggcorrplot(r)

#to long
plants.l<- plants %>%
  gather("trait", "value", 3:ncol(plants))
plants.l$value= as.numeric(plants.l$value)

## Facet labels
trait.labs.ap <- c("Earliest seed shed (mo)","Seed shed duration (mo)","N latitude (°)", "T mean (°C)", "T breadth (°C)")
names(trait.labs.ap) <- c("earliest_seed_shed_mo","seed_shed_dur_mos",
                       "Nbound_lat_GBIF_nosyn", "Bio1_mean_nosyn", "Bio1_std_nosyn")
trait.cat<- c("d","d","c", "c", "c")

trait.labs.t<- cbind(trait.labs.ap, names(trait.labs.ap),trait.cat)
trait.labs.all<- rbind(trait.labs.all, trait.labs.t)

#plot  
plot.aplants= ggplot(plants.l) + aes(x=value, y = migration_m)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs.ap)) +ggtitle('A. Alpine plants') +
  theme_bw()+ylab("Elevation shift (m)") #+ stat_smooth(method='lm', formula= y~poly(x,2))
#+scale_x_log10()

#scale and center
plants <- plants %>%
  mutate(earliest_seed_shed_mo = scale(earliest_seed_shed_mo),
         seed_shed_dur_mos = scale(seed_shed_dur_mos),
         Nbound_lat_GBIF_nosyn = scale(Nbound_lat_GBIF_nosyn),
         Bio1_mean_nosyn = scale(Bio1_mean_nosyn),
         Bio1_std_nosyn = scale(Bio1_std_nosyn)
         )

#----
#fish
#https://www.science.org/doi/abs/10.1126/science.1239352

#Make salt / fresh variable
fish$WaterType=0
fish$WaterType[fish$Brack== -1]=1
fish$WaterType[fish$Fresh== -1]=2

#log transform
fish$DepthRangeDeep= log(fish$DepthRangeDeep)
fish$Length= log(fish$Length)

#compress habitats
fish$habitat= as.numeric(factor(fish$DemersPelag, 
                                levels=c("bathydemersal","demersal","benthopelagic","pelagic-oceanic","pelagic-neritic","reef-associated")))
fish$habitat[fish$habitat>3]<-3 #group pelagic habitats

#restrict traits
fish=fish[,c("Species","Latitudinal.Difference","habitat","DepthRangeDeep","Length","WaterType","Vulnerability")]
#include vulnerability

#to long
fish.l<- fish %>%
  gather("trait", "value", 3:ncol(fish))

# Facet labels
trait.labs.f <- c("Habitat","log Depth range (m)","log Length (cm)","Vulnerability index","Water Type")
names(trait.labs.f) <- c("habitat","DepthRangeDeep","Length","Vulnerability","WaterType")
trait.cat<- c("e","e","l","e","e")

trait.labs.t<- cbind(trait.labs.f, names(trait.labs.f), trait.cat)
trait.labs.all<- rbind(trait.labs.all, trait.labs.t)

#plot
plot.fish= ggplot(fish.l) + aes(x=value, y = Latitudinal.Difference)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs.f)) +ggtitle('D. Fish') +
  theme_bw()+ylab("Latitudinal shift (°)") #+stat_smooth(method='lm', formula= y~poly(x,2))

#scale and center
fish <- fish %>%
  mutate(DepthRangeDeep = scale(DepthRangeDeep),
         Length = scale(Length),
         Vulnerability = scale(Vulnerability))

#make factors
fish$habitat= factor(fish$habitat, ordered=TRUE)
fish$WaterType= factor(fish$WaterType, ordered=TRUE)

#----
#European plants
#https://www.pnas.org/doi/abs/10.1073/pnas.1713936115

#to numeric
#Check codes: (1) C-competitors, (2) S-stress tolerators, and (3) R-ruderals.
#ls.codes=c("ccc","ccs","css","crs","sss","rss","rrs") 
#eplants$LifeStrategy= match(eplants$LifeStrategy, ls.codes)

#log transform
#eplants$LifeSpan= log(eplants$LifeSpan)
#eplants$SeedReleaseHeight= log(eplants$SeedReleaseHeight)

#restrict traits
#eplants=eplants[,c("speciesname","LeadingEdge","TemperatureIndicator","NutrientIndicator","SeedReleaseHeight","LifeStrategy","LifeSpan","NoOfVegOffspings","Dispersal","Persistence")]
#Look into adding Historic position, Historical optimum
eplants=eplants[,c("speciesname","LeadingEdge","TemperatureIndicator","NutrientIndicator","Dispersal","Persistence")]

#check correlations
r <- cor(eplants[,c(3:12)], use="complete.obs")
ggcorrplot(r)

#to long
eplants.l<- eplants %>%
  gather("trait", "value", 3:ncol(eplants))

# Facet labels
trait.labs.p <- c("Temperature indicator","Nutrient indicator","Dispersal index","Persistence index")
names(trait.labs.p) <- c("TemperatureIndicator","NutrientIndicator","Dispersal","Persistence")
trait.cat<- c("c","e","d","l")

trait.labs.t<- cbind(trait.labs.p, names(trait.labs.p), trait.cat)
trait.labs.all<- rbind(trait.labs.all, trait.labs.t)

#plot
plot.eplants= ggplot(eplants.l) + aes(x=value, y = LeadingEdge)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs.p)) +ggtitle('B. Plants') +
  theme_bw()+ylab("Elevational shift (m)") #+stat_smooth(method='lm', formula= y~poly(x,2))
#check LeadingEdge, Optimum, RearEdge

#scale and center
eplants <- eplants %>%
  mutate(Dispersal = scale(Dispersal),
        Persistence = scale(Persistence) )

#make factors
#eplants$LifeStrategy= factor(eplants$LifeStrategy, ordered=TRUE)
eplants$NutrientIndicator= factor(eplants$NutrientIndicator, ordered=TRUE)
eplants$TemperatureIndicator= factor(eplants$TemperatureIndicator, ordered=TRUE)

#----
#Lepidoptera and birds
# https://academic.oup.com/evlett/advance-article/doi/10.1093/evlett/qrad004/7076361
# Moth: move further N with bigger temp mean and and narrower breath
# Bird: move further N with narrowed temp breadth and resident wintering
# Butterfly: no strong predictors

#restrict traits
lepbird= lepbird[,c("Species","Taxonomic.group","D_border_0.9",
                    "temp.mean","temp.sd","precip.mean","precip.sd","wintering","body.size","num.gen","range.size")] 

#split moths and birds
#moth= lepbird[lepbird$Taxonomic.group %in% c("Moth"),]
#butterfly= lepbird[lepbird$Taxonomic.group %in% c("Butterfly"),]
lep= lepbird[lepbird$Taxonomic.group %in% c("Moth"),] #just moths for now: , "Butterfly"
bird=  lepbird[lepbird$Taxonomic.group == "Bird",]

#Code overwintering stage
#Lep
wint.stage= c("egg","larva","pupa","adult")
lep$wintering= match(lep$wintering, wint.stage)         
#bird, resident; short-distance migrant, and long-distance migrant
bird.stage= c("R","S","L")
bird$wintering= match(bird$wintering, bird.stage)  

#check correlations
r <- cor(lep[,c(4:6,8)], use="complete.obs")
r <- cor(bird[,c(4:6,8)], use="complete.obs")
ggcorrplot(r)

#to long
lep.l<- lep %>%
  gather("trait", "value", 4:ncol(lep))
bird.l<- bird %>%
  gather("trait", "value", 4:ncol(bird))

# Facet labels
trait.labs.l <- c("Range size (grid cells)","Overwintering mode","Body size (mm or g)","Number generations","T mean (°C)","T breadth (°C)","P mean (mm)","P breadth (mm)") 
names(trait.labs.l) <- c("range.size","wintering","body.size","num.gen","temp.mean","temp.sd","precip.mean","precip.sd")  
trait.cat<- c("e","l","l","l","c","c","c","c")

trait.labs.t<- cbind(trait.labs.l, names(trait.labs.l), trait.cat)
trait.labs.all<- rbind(trait.labs.all, trait.labs.t)

#plot
plot.lep= ggplot(lep.l) + aes(x=value, y = D_border_0.9)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs.l)) +ggtitle('C. Moths') +
  theme_bw()+ylab("Latitudinal shift (km)") 

plot.bird= ggplot(bird.l) + aes(x=value, y = D_border_0.9)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs.l)) +ggtitle('E. Birds') +
  theme_bw()+ylab("Latitudinal shift (km)") 

#set up ordered factors
bird$num.gen= factor(bird$num.gen, ordered=TRUE)
lep$num.gen= factor(lep$num.gen, ordered=TRUE)
bird$wintering= factor(bird$wintering, ordered=TRUE)
lep$wintering= factor(lep$wintering, ordered=TRUE)

#scale and center
lep <- lep %>%
  mutate(temp.mean = scale(temp.mean),
         temp.sd = scale(temp.sd),
         precip.mean = scale(precip.mean),
         precip.sd = scale(precip.sd),
         body.size = scale(body.size),
         range.size = scale(range.size))

#===================================
#Models
# OLS: linear relationships and no variable interaction
# Ridge: “ridge”-regularized linear model
# KernalRidge 
# SVR: support vector machine (SVM).
# RF: Random forest

# OLS-P: add polynomials
# Ridge-P: add polynomials

#Tidymodels
#https://rstudio-pubs-static.s3.amazonaws.com/802290_cb734d1218864fd093bdcf69208bd21a.html
#Example: https://smltar.com/mlregression.html

# eval: k-fold cross-validation scheme (Hastie et al. 2009, Section 7.10) combined with a mean squared error metric
# https://rsample.tidymodels.org/reference/vfold_cv.html

#LOOP THROUGH DATA
for(dat.k in 1:6){ 

if(dat.k==1){
  #MAMMALS
  #set up data
  dat=mammals
  dat$y= dat$High_change
  #drop ID rows
  dat<- dat[,3:ncol(dat)]
  trait.labs.svm= names(trait.labs.m)
}

if(dat.k==2){
  #PLANTS
  #set up data
  dat=plants
  dat$y= dat$migration_m
  #drop ID rows
  dat<- dat[,3:ncol(dat)]
  trait.labs.svm= names(trait.labs.ap)
}

if(dat.k==3){
  #FISH
  #set up data
  dat=fish
  dat$y= dat$Latitudinal.Difference
  #drop ID rows
  dat<- dat[,3:ncol(dat)]
  trait.labs.svm= names(trait.labs.f)
}

if(dat.k==4){
  #EURO PLANTS
  #set up data
  dat=eplants
  dat$y= dat$LeadingEdge
  #drop ID rows
  dat<- dat[,3:ncol(dat)]
  trait.labs.svm= names(trait.labs.p)
}

if(dat.k==5){
  #LEPS
  #set up data
  dat=lep
  dat$y= dat$D_border_0.9
  #drop ID rows
  dat<- dat[,4:ncol(dat)]
  trait.labs.svm= names(trait.labs.l)
 }

if(dat.k==6){
  #BIRDS
  #set up data
  dat=bird
  dat$y= dat$D_border_0.9
  #drop ID rows
  dat<- dat[,4:ncol(dat)]
  trait.labs.svm= names(trait.labs.l)
}

## USE ABSOLUTE VALUE OF SHIFT
#dat$y= abs(dat$y)
  
#-------------------
#drop NA
dat<- na.omit(dat)

#split the data
# Split data into 70% for training and 30% for testing
set.seed(2056)
dat_split <- dat %>% 
  initial_split(prop = 0.70)

# Extract the data in each split
train <- training(dat_split)
test <- testing(dat_split)

#-----
#OLS
lm_spec <- linear_reg() %>%
  set_engine(engine = "lm")

lm_wf <- 
  workflow() %>%
  add_model(lm_spec) %>%
  add_formula(y ~ .)

lm_fit <-
  fit(lm_wf,
      data = train
  )

#tidy(lm_fit)

#OLS Poly

if(dat.k==1){
  rec_poly <- recipe(y ~ ., data = train) %>%
    step_mutate(Orig_high_limit= poly(Orig_high_limit, degree=2)) %>%
    step_mutate(Rangesize_km2= poly(Rangesize_km2, degree=2)) %>%
    step_mutate(Mass_g= poly(Mass_g, degree=2)) %>%
    step_mutate(Bio1_mean= poly(Bio1_mean, degree=2)) %>%
    step_mutate(Bio1_std= poly(Bio1_std, degree=2)) %>%
   # step_poly(Orig_high_limit, degree = 2) %>% #Orig_high_limit,Rangesize_km2, Mass_g,
    step_ordinalscore(DietBreadth)
  }

if(dat.k==2){
  rec_poly <- recipe(y ~ earliest_seed_shed_mo + seed_shed_dur_mos + Nbound_lat_GBIF_nosyn + Bio1_mean_nosyn + Bio1_std_nosyn, data = train) %>%
    step_mutate(earliest_seed_shed_mo= poly(earliest_seed_shed_mo, degree=2)) %>%
    step_mutate(seed_shed_dur_mos= poly(seed_shed_dur_mos, degree=2)) %>%
    step_mutate(Nbound_lat_GBIF_nosyn= poly(Nbound_lat_GBIF_nosyn, degree=2)) %>%
    step_mutate(Bio1_mean_nosyn= poly(Bio1_mean_nosyn, degree=2)) %>%
    step_mutate(Bio1_std_nosyn= poly(Bio1_std_nosyn, degree=2)) 
    #step_poly(earliest_seed_shed_mo, seed_shed_dur_mos,  Nbound_lat_GBIF_nosyn) #%>%
    #step_ordinalscore(BreedSysCode, DispersalMode)
}

if(dat.k==3){
  rec_poly <- recipe(y ~ ., data = train) %>%
    step_mutate(DepthRangeDeep= poly(DepthRangeDeep, degree=2)) %>%
    step_mutate(Length= poly(Length, degree=2)) %>%
    step_mutate(Vulnerability= poly(Vulnerability, degree=2)) %>%
  #  step_poly(DepthRangeDeep, degree = 2) %>% #DepthRangeDeep, Length, Vulnerability, 
    step_ordinalscore(habitat,WaterType)
}

if(dat.k==4){
  rec_poly <- recipe(y ~ ., data = train) %>%
    #step_mutate(SeedReleaseHeight= poly(SeedReleaseHeight, degree=2)) %>%
    #step_mutate(LifeSpan= poly(LifeSpan, degree=2)) %>%
    step_mutate(Dispersal= poly(Dispersal, degree=2)) %>%
    step_mutate(Persistence= poly(Persistence, degree=2)) %>%
    #step_poly(Persistence, degree=1) %>% #SeedReleaseHeight,LifeSpan,Dispersal,Persistence
    step_ordinalscore(TemperatureIndicator,NutrientIndicator)
}

if(dat.k %in% c(5,6)){
  rec_poly <- recipe(y ~ ., data = train) %>%
    step_mutate(temp.mean= poly(temp.mean, degree=2)) %>%
    step_mutate(temp.sd= poly(temp.sd, degree=2)) %>%
    step_mutate(precip.mean= poly(precip.mean, degree=2)) %>%
    step_mutate(precip.sd= poly(precip.sd, degree=2)) %>%
    step_mutate(body.size= poly(body.size, degree=2)) %>%
    step_mutate(range.size= poly(range.size, degree=2)) %>%
    step_ordinalscore(wintering,num.gen)
}

#https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/
#train_poly <- juice(prep(rec_poly, retain=TRUE))

lm_spec_poly <- linear_reg() %>%
     set_mode("regression") %>%
     set_engine(engine = "lm")
 
lm_wf_poly <- 
  workflow() %>%
  add_model(lm_spec_poly) %>%
  add_recipe(rec_poly)

lm_fit_poly <- lm_wf_poly %>%
  fit(data=train)

#----
# Ridge: “ridge”-regularized linear model
#https://www.tidymodels.org/learn/models/parsnip-ranger-glmnet/
#use scaled data

glmn_spec <- linear_reg(penalty = 0.001, mixture = 0.5) %>%
  set_engine(engine = "glmnet")

glmn_fit <- glmn_spec %>%
  fit(y ~ .,
      data = train
  )

glmn_wf <- 
  workflow() %>%
  add_model(glmn_spec) %>%
  add_formula(y ~ .)

#Ridge poly
glmn_spec_poly <- linear_reg(penalty = 0.001, mixture = 0.5) %>%
  set_engine(engine = "glmnet")

glmn_wf_poly <- 
  workflow() %>%
  add_model(glmn_spec_poly) %>%
  add_recipe(rec_poly)

glmn_fit_poly <- glmn_wf_poly %>%
  fit(data=train)

#----
#Kernel regression
#Not well implemented in R, try RBF SVM instead

svm_rbf_spec <- svm_rbf(rbf_sigma = 0.2) %>%
  set_mode("regression") %>%
  set_engine("kernlab", scaled = FALSE)

svm_rbf_fit <- svm_rbf_spec %>% 
  set_args(cost = 10) %>%
  fit(y ~ ., data = train)

svm_rbf_wf <- 
  workflow() %>%
  add_model(svm_rbf_spec) %>%
  add_formula(y ~ .)

#https://osm.netlify.app/post/kernel-error/

#----
# SVR: support vector machine (SVM)

svm_linear_spec <- svm_poly(degree = 1) %>%
  set_mode("regression") %>%
  set_engine("kernlab", scaled = FALSE)

svm_linear_fit <- svm_linear_spec %>% 
  set_args(cost = 10) %>%
  fit(y ~ ., data = train)

svm_linear_wf <- 
  workflow() %>%
  add_model(svm_linear_spec) %>%
  add_formula(y ~ .)

#-------
#RF
#rf_spec <- rand_forest(mode = "regression", trees = 100) %>%
#  set_engine("ranger")
rf_spec <- rand_forest(mode = "regression", trees = 100) %>%
  set_engine("randomForest")
#randomForest(Creditability~.,data=mydata, mtry=best.m, importance=TRUE,ntree=500)

rf_fit <- rf_spec %>%
  fit(y ~ ., data = train
  )

rf_wf <- 
  workflow() %>%
  add_model(rf_spec) %>%
  add_formula(y ~ .)

#=====================
#Evaluation

#cross validation 
set.seed(1234)
if(dat.k==1) folds <- vfold_cv(na.omit(dat), v=5) #USe K=5 for small mammals dataset
if(dat.k>1) folds <- vfold_cv(na.omit(dat), v=10) 

#lm
set.seed(456)
lm_res <- fit_resamples(
  lm_wf,
  folds,
  control = control_resamples(save_pred = TRUE)
)

cv.all= lm_res %>%
  collect_metrics()
cv.all$Model="LM"

lm_res %>%
  unnest(.predictions) %>%
  ggplot(aes(y, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", linewidth = 1.5) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Truth",
    y = "Predicted shift",
    color = NULL
  )

#---------------------
# lm poly

set.seed(456)
lm_res <- fit_resamples(
  lm_wf_poly,
  folds,
  control = control_resamples(save_pred = TRUE)
)

cv.metric= lm_res %>%
  collect_metrics()
cv.metric$Model="LM poly"
cv.all=rbind(cv.all, cv.metric)

#---------------------
#ridge regression

set.seed(456)
glmn_res <- fit_resamples(
  glmn_wf,
  folds,
  control = control_resamples(save_pred = TRUE)
)

cv.metric= glmn_res %>%
  collect_metrics()
cv.metric$Model="RR"
cv.all=rbind(cv.all, cv.metric)

#---------------------
#ridge regression poly

set.seed(456)
glmn_res_poly <- fit_resamples(
  glmn_wf_poly,
  folds,
  control = control_resamples(save_pred = TRUE)
)

cv.metric= glmn_res_poly %>%
  collect_metrics()
cv.metric$Model="RR poly"
cv.all=rbind(cv.all, cv.metric)

#---------------------
#svm_rbf
set.seed(456)
svm_rbf_res <- fit_resamples(
  svm_rbf_wf,
  folds,
  control = control_resamples(save_pred = TRUE)
)

cv.metric= svm_rbf_res %>%
  collect_metrics()
cv.metric$Model="SVM rbf"
cv.all=rbind(cv.all, cv.metric)

svm_rbf_res %>%
  unnest(.predictions) %>%
  ggplot(aes(y, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", linewidth = 1.5) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Truth",
    y = "Predicted shift",
    color = NULL
  )
#---------------------
#svm_linear
set.seed(456)
svm_linear_res <- fit_resamples(
  svm_linear_wf,
  folds,
  control = control_resamples(save_pred = TRUE)
)

cv.metric= svm_linear_res %>%
  collect_metrics()
cv.metric$Model="SVM linear"
cv.all=rbind(cv.all, cv.metric)

svm_linear_res %>%
  unnest(.predictions) %>%
  ggplot(aes(y, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", linewidth = 1.5) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Truth",
    y = "Predicted shift",
    color = NULL
  )

#---------------------
#rf
set.seed(456)
rf_res <- fit_resamples(
  rf_wf,
  folds,
  control = control_resamples(save_pred = TRUE)
)

cv.metric= rf_res %>%
  collect_metrics()
cv.metric$Model="RF"
cv.all=rbind(cv.all, cv.metric)

rf_res %>%
  unnest(.predictions) %>%
  ggplot(aes(y, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", linewidth = 1.5) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Truth",
    y = "Predicted shift",
    color = NULL
  )

#==============
#VIP
#VI for cross validation
#https://stackoverflow.com/questions/72538064/tidymodels-how-to-extract-importance-from-training-data/72680901#72680901

get_rf_imp <- function(x) {
  x %>% 
    extract_fit_parsnip() %>% 
    vip::vi()
}

ctrl_imp <- control_grid(extract = get_rf_imp)

#OLS
lm_res <-
  workflow(y ~ ., lm_spec) %>%
  fit_resamples(folds, control = ctrl_imp)

lm_poly_res <-
  workflow() %>%
  add_model(lm_spec_poly) %>%
  add_recipe(rec_poly) %>%
  fit_resamples(folds, control = ctrl_imp)

#RR
glmn_res <-
  workflow(y ~ ., glmn_spec) %>%
  fit_resamples(folds, control = ctrl_imp)

glmn_poly_res <-
  workflow() %>%
  add_model(glmn_spec_poly) %>%
  add_recipe(rec_poly) %>%
  fit_resamples(folds, control = ctrl_imp)

#SVM linear
#https://stackoverflow.com/questions/62772397/integration-of-variable-importance-plots-within-the-tidy-modelling-framework
#permutation based

svm_linear_fit <- workflow() %>%
  add_model(svm_linear_spec) %>%
  add_formula(y ~ .) %>%
  fit(train)

 svm_linear_vi<-
   svm_linear_fit %>%
   pull_workflow_fit() %>%
   vi(method = "permute", 
      target = "y", metric = "rsquared",
      pred_wrapper = kernlab::predict, train = train, nsim=7)

# #MODIFY TO KEEP SIGN
#  svm_linear_vi<-
#       svm_linear_fit %>%
#       pull_workflow_fit() %>%
#       vi(method = "shap", 
#         feature_names= trait.labs.svm,  
#         pred_wrapper = kernlab::predict, train = train, nsim=7)

#SVM RBF
svm_rbf_fit <- workflow() %>%
  add_model(svm_rbf_spec) %>%
  add_formula(y ~ .) %>%
  fit(train)

 svm_rbf_vi<-
   svm_rbf_fit %>%
   pull_workflow_fit() %>%
   vi(method = "permute", 
      target = "y", metric = "rsquared",
      pred_wrapper = kernlab::predict, train = train, nsim=7)

#RF
rf_res <-
  workflow(y ~ ., rf_spec) %>%
  fit_resamples(folds, control = ctrl_imp)

#estimate Shapley values for SVM
#https://bgreenwell.github.io/fastshap/articles/fastshap.html
#subject to numeric 
train2= train[ , unlist(lapply(train, is.numeric))]  
 
svm_model_lin <- svm(y ~ ., train2, kernel = "linear", scale=FALSE, cost=10)
svm_model_rbf <- svm(y ~ . , train2, kernel = "radial", scale=FALSE, cost=10)

set.seed(101) # for reproducibility
shap_svm_lin <- fastshap::explain(svm_model_lin, X = train2[,-ncol(train2)], nsim = 10,
                 pred_wrapper = predict)
shap_svm_rbf <- fastshap::explain(svm_model_rbf, X = train2[,-ncol(train2)], nsim = 10,
                         pred_wrapper = predict)


shap_svm_lin_mean= apply(shap_svm_lin, MARGIN=2, FUN=mean)
shap_svm_lin_stdev= apply(shap_svm_lin, MARGIN=2, FUN=sd)

shap_svm_rbf_mean= apply(shap_svm_rbf, MARGIN=2, FUN=mean)
shap_svm_rbf_stdev= apply(shap_svm_rbf, MARGIN=2, FUN=sd)

#Gather VI
mods= c("LM","LM poly","RR","RR poly","RF")

for(mod.k in 1:5){

if(mod.k==1) mod_res<- lm_res
if(mod.k==2) mod_res<- lm_poly_res
if(mod.k==3) mod_res<- glmn_res
if(mod.k==4) mod_res<- glmn_poly_res
if(mod.k==5) mod_res<- rf_res

mod.vi= mod_res %>%
  dplyr::select(id, .extracts) %>%
  unnest(.extracts) %>%
  unnest(.extracts) 

mod.vi$ImpSign<- mod.vi$Importance
if("Sign" %in% colnames(mod.vi) ) mod.vi$ImpSign[which(mod.vi$Sign== "NEG")]= mod.vi$ImpSign[which(mod.vi$Sign== "NEG")] * (-1)
if(("Sign" %in% colnames(mod.vi))==FALSE) mod.vi$ImpSign<- NA

mod.vi= mod.vi %>%
group_by(Variable) %>%
 summarise(Mean = mean(Importance),
          Variance = sd(Importance),
          Mean.sign = mean(ImpSign),
          Variance.sign = sd(ImpSign),
          ) 

mod.vi$Model<- mods[mod.k]

if(mod.k==1) vi.mods= mod.vi 
if(mod.k>1) vi.mods= rbind(vi.mods, mod.vi)

}  #end loop models

#svm doesn't include signs; add shapely to include signs
#add svm
vi.svm1= as.data.frame(matrix(NA, nrow= nrow(svm_linear_vi),ncol=ncol(vi.mods)))
names(vi.svm1)= names(vi.mods)
vi.svm1$Variable= svm_linear_vi$Variable
vi.svm1$Mean= abs(svm_linear_vi$Importance)
vi.svm1$Variance= svm_linear_vi$StDev
vi.svm1$Model<- "SVM linear"
#add shapley
match1= match(names(shap_svm_lin_mean), vi.svm1$Variable)
vi.svm1$Mean.sign[na.omit(match1)]= shap_svm_lin_mean[!is.na(match1)]
#vi.svm1$Variance.sign[na.omit(match1)]= shap_svm_lin_stdev[!is.na(match1)]

vi.mods= rbind(vi.mods, vi.svm1)

#RBF
vi.svm1= as.data.frame(matrix(NA, nrow= nrow(svm_rbf_vi),ncol=ncol(vi.mods)))
names(vi.svm1)= names(vi.mods)
vi.svm1$Variable= svm_rbf_vi$Variable
vi.svm1$Mean= abs(svm_rbf_vi$Importance)
vi.svm1$Variance= svm_rbf_vi$StDev
vi.svm1$Model<- "SVM rbf"
#add shapley
match1= match(names(shap_svm_rbf_mean), vi.svm1$Variable)
vi.svm1$Mean.sign[na.omit(match1)]= shap_svm_rbf_mean[!is.na(match1)]
#vi.svm1$Variance.sign[na.omit(match1)]= shap_svm_rbf_stdev[!is.na(match1)]

vi.mods= rbind(vi.mods, vi.svm1)

#------------
#Combine across datasets
vi.mods$dataset= datasets[dat.k]
cv.all$dataset= datasets[dat.k]

if(dat.k==1){
 vi.dat= vi.mods
 cv.dat= cv.all
}
if(dat.k>1){
  vi.dat= rbind(vi.dat, vi.mods)
  cv.dat= rbind(cv.dat, cv.all)
}

#==========================
#predictions across models

# https://juliasilge.com/blog/intro-tidymodels/

results_train <- lm_fit %>%
  predict(new_data = train) %>%
  mutate(
    truth = train$y,
    model = "LM"
  ) %>%
  bind_rows(lm_fit_poly %>%
              predict(new_data = train) %>%
              mutate(
                truth = train$y,
                model = "LM poly"
              ))%>%
  bind_rows(glmn_fit %>%
              predict(new_data = train) %>%
              mutate(
                truth = train$y,
                model = "RR"
              ))%>%
  bind_rows(glmn_fit_poly %>%
              predict(new_data = train) %>%
              mutate(
                truth = train$y,
                model = "RR poly"
              ))%>%
  bind_rows(svm_rbf_fit %>%
              predict(new_data = train) %>%
              mutate(
                truth = train$y,
                model = "SVM rbf"
              ))%>%
  bind_rows(svm_linear_fit %>%
              predict(new_data = train) %>%
              mutate(
                truth = train$y,
                model = "SVM linear"
              ))%>%
  bind_rows(rf_fit %>%
              predict(new_data = train) %>%
              mutate(
                truth = train$y,
                model = "RF"
              ))

results_test <- lm_fit %>%
  predict(new_data = test) %>%
  mutate(
    truth = test$y,
    model = "LM"
  ) %>%
  bind_rows(lm_fit_poly %>%
              predict(new_data = test) %>%
              mutate(
                truth = test$y,
                model = "LM poly"
              ))%>%
  bind_rows(glmn_fit %>%
              predict(new_data = test) %>%
              mutate(
                truth = test$y,
                model = "RR"
              ))%>%
  bind_rows(glmn_fit_poly %>%
              predict(new_data = test) %>%
              mutate(
                truth = test$y,
                model = "RR poly"
              ))%>%
  bind_rows(svm_rbf_fit %>%
              predict(new_data = test) %>%
              mutate(
                truth = test$y,
                model = "SVM rbf"
              ))%>%
  bind_rows(svm_linear_fit %>%
              predict(new_data = test) %>%
              mutate(
                truth = test$y,
                model = "SVM linear"
              ))%>%
  bind_rows(rf_fit %>%
              predict(new_data = test) %>%
              mutate(
                truth = test$y,
                model = "RF"
              ))

#Combine across datasets
results_train$dataset= datasets[dat.k]
results_test$dataset= datasets[dat.k]

if(dat.k==1){
  pred.test= results_test
  pred.train= results_train
}
if(dat.k>1){
  pred.test= rbind(pred.test, results_test)
  pred.train= rbind(pred.train, results_train)
}

} #end loop datasets

#rmse
results_train %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)

results_test %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)

#=============================
#PLOTS

#Trait plots
# alpine plants: seed shed month earliest
plot1a= ggplot(plants) + aes(x=earliest_seed_shed_mo, y = migration_m)+geom_point()+
  xlab("Earliest seed shed (mo)")+ylab("Elevation shift (m)")+ 
  ggtitle('A. Alpine plants')+
  theme_bw(base_size=14)

# European plants: seed release height
plot1b= ggplot(eplants) + aes(x=TemperatureIndicator, y = LeadingEdge)+geom_point()+
  xlab("Temperature indicator")+ylab("Elevation shift (m)")+ 
  ggtitle('B. Plants')+
  theme_bw(base_size=14)

# Lep: temp breadth
plot1c= ggplot(lep) + aes(x=temp.sd, y = D_border_0.9)+geom_point()+
  xlab("T breadth (°C)")+ylab("Latitudinal shift (km)")+ 
  ggtitle('C. Moths')+
  theme_bw(base_size=14)

# Fish: depth range
plot1d= ggplot(fish) + aes(x=DepthRangeDeep, y = Latitudinal.Difference)+geom_point()+
  xlab("Depth range (m)")+ylab("Latitudinal shift (°)")+ 
  ggtitle('D. Fish')+
  theme_bw(base_size=14) #+
  #theme(axis.title.x = element_text(margin = margin(t = -15)))

# Bird: temp breadth
plot1e= ggplot(bird) + aes(x=temp.sd, y = D_border_0.9)+geom_point()+
  xlab("T breadth (°C)")+ylab("Latitudinal shift (km)")+ 
  ggtitle('E. Birds')+
  theme_bw(base_size=14)

# Mammals: alt timit
plot1f= ggplot(mammals) + aes(x=Orig_high_limit, y = High_change)+geom_point()+
  xlab("Altitudinal limit (m)")+ylab("Elevation shift (m)")+ 
  ggtitle('F. Small mammals')+
  theme_bw(base_size=14)

#----
#combine
#(plot1a | plot1b) / (plot1c | plot1d) / (plot1e | plot1f) / (plot1g | plot1h)

#setwd for figures
setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/figures/")

pdf("Fig1_traits.pdf", height = 8, width = 10)
(plot1a | plot1b | plot1c) / (plot1d | plot1e | plot1f)
dev.off()

#------------------------
#All traits
pdf("FigS1_alltraits.pdf", onefile = TRUE)
print(plot.aplants)
print(plot.eplants)
print(plot.lep)
print(plot.fish)
print(plot.bird)
print(plot.mammal)
dev.off()

#-----------------------------
#RMSE plot

#scale to max value
cv.dat= as.data.frame(cv.dat)

cv.max= cv.dat %>%
  group_by(dataset, .metric) %>%
  summarize(max.mean=max(mean))
cv.max= as.data.frame(cv.max)

cv.dat$dat_mod= paste(cv.dat$dataset, cv.dat$.metric,sep="_")
cv.max$dat_mod= paste(cv.max$dataset, cv.max$.metric,sep="_")

match1= match(cv.dat$dat_mod, cv.max$dat_mod)
cv.dat$MeanScale= cv.dat$mean/cv.max$max.mean[match1]
cv.dat$SterrScale= cv.dat$std_err/cv.max$max.mean[match1]

#scale 
cv.dat$MeanScale[which(cv.dat$.metric=="rsq")]= cv.dat$mean[which(cv.dat$.metric=="rsq")]
cv.dat$SterrScale[which(cv.dat$.metric=="rsq")]= cv.dat$std_err[which(cv.dat$.metric=="rsq")]

#order models
cv.dat$Model= factor(cv.dat$Model, levels=c("LM","LM poly", "RR", "RR poly", "SVM linear", "SVM rbf", "RF"), ordered=TRUE)
#order datasets
cv.dat$dataset= factor(cv.dat$dataset, levels=c("plants","eplants", "lep", "fish", "birds", "mammals"), ordered=TRUE)

#plot
cv.plot1= ggplot(cv.dat[cv.dat$dataset %in% c("plants", "eplants","lep"),]) + aes(y=MeanScale, x = Model)+geom_point(size=2)+ #geom_line()+
  facet_grid(.metric~dataset, scales="free_y", labeller = labeller(dataset = dat.labs, .metric=met.labs, nrow=4), switch="y")+
  theme_bw()
cv.plot1= cv.plot1 + 
  geom_errorbar(data=cv.dat[cv.dat$dataset %in% c("plants", "eplants","lep"),], aes(x=Model, y=MeanScale, ymin=MeanScale-SterrScale, ymax=MeanScale+SterrScale), width=0, col="black")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+ ylab("")+xlab("")

cv.plot2= ggplot(cv.dat[cv.dat$dataset %in% c("fish","birds","mammals"),]) + aes(y=MeanScale, x = Model)+geom_point(size=2)+ #geom_line()+
  facet_grid(.metric~dataset, scales="free_y", labeller = labeller(dataset = dat.labs, .metric=met.labs), switch="y")+
  theme_bw()
cv.plot2= cv.plot2 + 
  geom_errorbar(data=cv.dat[cv.dat$dataset %in% c("fish","birds","mammals"),], aes(x=Model, y=MeanScale, ymin=MeanScale-SterrScale, ymax=MeanScale+SterrScale), width=0, col="black")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("")

#-----
#plot average CV
cv.dat.rmse= cv.dat[cv.dat$Model=="LM" & cv.dat$.metric=="rmse",]
cv.dat.rsq= cv.dat[cv.dat$Model=="LM" & cv.dat$.metric=="rsq",]

cv.rmse= cv.dat[cv.dat$.metric=="rmse",]
cv.rsq= cv.dat[cv.dat$.metric=="rsq",]

cv.rmse$mean.dif= cv.rmse$MeanScale - cv.dat.rmse[match(cv.rmse$dataset, cv.dat.rmse$dataset),"MeanScale"]
cv.rsq$mean.dif= cv.rsq$MeanScale - cv.dat.rsq[match(cv.rsq$dataset, cv.dat.rsq$dataset),"MeanScale"]

cv.dat2= rbind(cv.rmse, cv.rsq)
cv.dat2= cv.dat2[-which(cv.dat2$Model=="LM"),]
cv.dat2$dat= dat.titles[match(cv.dat2$dataset, datasets)]
cv.dat2$dat= factor(cv.dat2$dat, levels=c("alpine plants","plants","moths","fish","birds","small mammals"))

datasets= c("mammals", "plants","fish", "eplants", "lep", "birds")
dat.titles= c("small mammals", "alpine plants","fish", "plants", "moths", "birds")

#mean across models
cv.dat.agg= aggregate(cv.dat2, by=list(cv.dat2$Model, cv.dat2$.metric), FUN="mean")
colnames(cv.dat.agg)[1:2]<- c("Model",".metric")
cv.dat.agg<- cv.dat.agg[,c(1,2,14)]

#order models
cv.dat.agg$Model= factor(cv.dat.agg$Model, levels=c("LM","LM poly", "RR", "RR poly", "SVM linear", "SVM rbf", "RF"), ordered=TRUE)

#plot
cv.mean.plot=ggplot(cv.dat2) + aes(y=mean.dif, x = Model, color=dat)+
  geom_hline(yintercept=0, color = "gray80", linewidth = 1)+
  geom_point(size=2)+geom_line(aes(group=dat))+
  facet_wrap(~.metric, ncol=1, labeller = labeller(dataset = dat.labs, .metric=met.labs), scales="free_y")+
  theme_bw() +ylab("difference in metric from LM")

cv.mean.plot= cv.mean.plot + 
  geom_point(data = cv.dat.agg, 
             mapping = aes(x = Model, y = mean.dif), color="black", size=4, shape=15)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ theme(legend.position="bottom")+
  scale_color_discrete(name="")

layout <- "
AAAAABB
AAAAABB
AAAAABB
AAAAABB
CCCCCBB
CCCCC##"

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/figures/")
pdf("Fig2_CvPlot.pdf",height = 8, width = 12)
cv.plot1 + cv.mean.plot + cv.plot2 + plot_layout(design = layout) + plot_annotation(tag_levels = 'A')
dev.off()

#------------------
#VI plots with cross validation

#scale to max value
vi.dat2= as.data.frame(vi.dat)

vi.max= vi.dat2 %>%
group_by(dataset, Model) %>%
summarize(max.mean=max(Mean),
          max.mean.sign=max(abs(Mean.sign), na.rm=TRUE))
vi.max= as.data.frame(vi.max)

vi.dat2$dat_mod= paste(vi.dat$dataset, vi.dat$Model,sep="_")
vi.max$dat_mod= paste(vi.max$dataset, vi.max$Model,sep="_")

match1= match(vi.dat2$dat_mod, vi.max$dat_mod)
vi.dat2$MeanScale= vi.dat2$Mean/vi.max$max.mean[match1]
vi.dat2$StdevScale= vi.dat2$Variance/vi.max$max.mean[match1]
vi.dat2$MeanScale.sign= vi.dat2$Mean.sign/vi.max$max.mean.sign[match1]
vi.dat2$StdevScale.sign= vi.dat2$Variance.sign/vi.max$max.mean.sign[match1]

#condense to maximum value for factors
vi.dat2$Variable= gsub("`", "", vi.dat2$Variable)

vi.dat2$Variable= gsub(".L", "", vi.dat2$Variable)
vi.dat2$Variable= gsub(".Q", "", vi.dat2$Variable)
vi.dat2$Variable= gsub(".C", "", vi.dat2$Variable)
vi.dat2$Variable= gsub("^4", "", vi.dat2$Variable)
vi.dat2$Variable= gsub("^5", "", vi.dat2$Variable)
vi.dat2$Variable= gsub("^6", "", vi.dat2$Variable)

vi.dat2$Variable= gsub("_poly_1", "", vi.dat2$Variable)
vi.dat2$Variable= gsub("_poly_2", "", vi.dat2$Variable)
vi.dat2$Variable= gsub(".1", "", vi.dat2$Variable, fixed = TRUE)
vi.dat2$Variable= gsub(".2", "", vi.dat2$Variable, fixed = TRUE)
vi.dat2$Variable= gsub("1", "", vi.dat2$Variable)
vi.dat2$Variable= gsub("2", "", vi.dat2$Variable)

vi.dat2$Variable[grep("TemperatureIndicator", vi.dat2$Variable, value = FALSE)]<- "TemperatureIndicator"
vi.dat2$Variable[grep("LifeStrategy", vi.dat2$Variable, value = FALSE)]<- "LifeStrategy"

#reduce to max
vi.dat2$dat_mod_var= paste(vi.dat2$dat_mod, vi.dat2$Variable, sep="_")

vi.dat.max= aggregate(vi.dat2, by=list(vi.dat2$dat_mod_var), FUN=max, na.rm=TRUE, na.action=NULL)
vi.dat.max[sapply(vi.dat.max, is.infinite)] <- NA

#average importance across models
vi.mean= vi.dat2 %>%
  group_by(dataset, Variable) %>%
  summarize(MeanImp= mean(MeanScale),
            MeanImp.sign= mean(MeanScale.sign))
vi.mean= as.data.frame(vi.mean)
vi.mean$Model= "mean"

#append mean data
vi.mean.add= vi.dat2[1:nrow(vi.mean),]
vi.mean.add[]= NA
vi.mean.add[,c("dataset","Variable","MeanScale","MeanScale.sign","Model")]= vi.mean[,c("dataset","Variable","MeanImp","MeanImp.sign","Model")]
vi.dat2= rbind(vi.dat2, vi.mean.add)
#code mean
vi.dat2$IsMeanImp="N"
vi.dat2$IsMeanImp[which(vi.dat2$Model=="mean")]="Y"

#average importance across models for max
vi.mean= vi.dat.max %>%
  group_by(dataset, Variable) %>%
  summarize(MeanImp= mean(MeanScale),
            MeanImp.sign= mean(MeanScale.sign))
vi.mean= as.data.frame(vi.mean)
vi.mean$Model= "mean"

#append mean data
vi.mean.add= vi.dat.max[1:nrow(vi.mean),]
vi.mean.add[]= NA
vi.mean.add[,c("dataset","Variable","MeanScale","MeanScale.sign","Model")]= vi.mean[,c("dataset","Variable","MeanImp","MeanImp.sign","Model")]
vi.dat.max= rbind(vi.dat.max, vi.mean.add)
#code mean
vi.dat.max$IsMeanImp="N"
vi.dat.max$IsMeanImp[which(vi.dat.max$Model=="mean")]="Y"
#vi.dat.max$IsMeanImp= factor(vi.dat.max$IsMeanImp, levels=c("Y","N"), ordered=TRUE)

#order by mean trait importance
#vi.dat2$dataset= factor(vi.dat2$dataset, levels=c("plants","eplants", "lep", "fish", "birds", "mammals"), ordered=TRUE)
vi.dat.max<- vi.dat.max[order(vi.dat.max$IsMeanImp, vi.dat.max$MeanScale, vi.dat.max$dataset, decreasing = TRUE), ]

vi.plot= ggplot(vi.dat2) + aes(y=MeanScale, x = reorder(Variable, MeanScale), color=Model, group=Model)+geom_point()+geom_line()+
  facet_grid(.~dataset, scales="free_y") + theme(axis.text.x = element_text(angle=90))

#order models by complexity
vi.dat.max$Model= factor(vi.dat.max$Model, levels=c("LM","LM poly", "RR", "RR poly", "SVM linear", "SVM rbf", "RF", "mean"), ordered=TRUE)

#update labels
trait.labs.all= as.data.frame(trait.labs.all)
names(trait.labs.all)= c("trait.labs","trait","category")

#update names
vi.dat.max$Variable[grep("Bio_mean", vi.dat.max$Variable)]="Bio1_mean"
vi.dat.max$Variable[grep("Bio_std", vi.dat.max$Variable)]="Bio1_std"
vi.dat.max$Variable[grep("Rangesize_km", vi.dat.max$Variable)]="Rangesize_km2"

match1= match(vi.dat.max$Variable,  trait.labs.all$trait)
vi.dat.max$trait.lab<- NA
vi.dat.max$trait.lab[which(!is.na(match1))]<- trait.labs.all$trait.labs[match1[which(!is.na(match1))]]
vi.dat.max$trait.cat[which(!is.na(match1))]<- trait.labs.all$category[match1[which(!is.na(match1))]]

#set color
vi.dat.max$trait.color= c("cornflowerblue","darkorange","forestgreen","purple")[match(vi.dat.max$trait.cat, c("c","d","e","l") )]

#---------------------
#split by dataset and split
library(patchwork)
library(viridis)
vi.plots <- vector('list', length(datasets))
vi.plots.sign <- vector('list', length(datasets))

pd <- position_dodge(0.4)

#set up colors
cols.mod= viridis(n=8,option="turbo")

for(dat.k in 1:6){
  #reorder
  vi.dat.sub= vi.dat.max[vi.dat.max$dataset==datasets[dat.k],]
  vi.dat.sub = vi.dat.sub[order(vi.dat.sub$MeanScale), ]
  # lock in factor level order
  vi.dat.sub$trait.lab <- factor(vi.dat.sub$trait.lab, levels = vi.dat.sub[which(vi.dat.sub$Model=="mean"),"trait.lab"])
  cols= vi.dat.sub$trait.color[vi.dat.sub$Model=="mean"]
  
  vi.plot= ggplot(vi.dat.sub) + aes(y=MeanScale, x = trait.lab, color=Model, group=Model, lty=IsMeanImp, alpha=IsMeanImp,linewidth=IsMeanImp)+
    geom_pointrange(aes(ymin = MeanScale-StdevScale, ymax = MeanScale+StdevScale), position=pd)+
    #geom_point(position=position_dodge(width=0.5))+
      geom_line(position=pd) +  #geom_jitter(width = 0.1, height = 0)
    #facet_grid(.~RF, scales="free")+
    ggtitle(dat.labs[dat.k])+theme_bw()+ylim(0,1)+
    xlab("Predictor")+ylab("Importance")+
    scale_color_viridis_d(option="turbo") + 
    scale_alpha_discrete(range = c(0.5, 1))+
    scale_linewidth_manual(values = c(N = 0.5, Y=1))+
   # geom_errorbar(data=vi.dat2[vi.dat2$dataset==datasets[dat.k],], aes(y=MeanScale, x = Variable, ymin=MeanScale-StdevScale, ymax=MeanScale+StdevScale), width=0, position=position_dodge(width=0.5))+
    guides(lty="none", alpha="none", linewidth="none")
  
  if(dat.k>1) vi.plot=vi.plot + theme(legend.position = "none")
  vi.plots[[dat.k]]= vi.plot+ coord_flip()+
    theme(axis.text.y = element_text(colour= cols))
  
  #account for sign
  #drop polys for sign
  vi.dat.sub= vi.dat.sub[which(vi.dat.sub$Model %in% c("LM","RR", "SVM linear", "SVM rbf")),]
  
  vi.plot= ggplot(vi.dat.sub) + aes(y=MeanScale.sign, x = trait.lab, color=Model, group=Model, lty=IsMeanImp)+
    geom_pointrange(aes(ymin = MeanScale.sign-StdevScale.sign, ymax = MeanScale.sign+StdevScale.sign), position=pd)+
    geom_line(position=pd) +  #geom_jitter(width = 0.1, height = 0)
    ggtitle(dat.labs[dat.k])+theme_bw()+
    xlab("Predictor")+ylab("Effect")+
    scale_color_discrete(type=cols.mod[c(1,3,5,6)]) + 
    guides(lty="none")+
    geom_hline(yintercept = 0, color="gray",lwd=1)
  
  if(dat.k>1) vi.plot=vi.plot + theme(legend.position = "none")
  vi.plots.sign[[dat.k]]= vi.plot+ coord_flip()+
    theme(axis.text.y = element_text(colour= cols))+ylim(-1.2,1.2)
}

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/figures/")
pdf("Fig3_ViPlotsCV.pdf",height = 8, width = 14)
(vi.plots[[2]] | vi.plots[[4]] | vi.plots[[5]])/
  (vi.plots[[3]] | vi.plots[[6]] | vi.plots[[1]])
dev.off()

pdf("Fig4_ViPlotsCV_sign.pdf",height = 8, width = 14)
(vi.plots.sign[[2]] | vi.plots.sign[[4]] | vi.plots.sign[[5]])/
  (vi.plots.sign[[3]] | vi.plots.sign[[6]] | vi.plots.sign[[1]])
dev.off()

#===============================
#prediction plots

pred.test$datasub= "test"
pred.train$datasub= "train"
pred.all= rbind(pred.test, pred.train)
pred.all$datasub= factor(pred.all$datasub, levels=c("train","test"), ordered=TRUE)

#subset to plants and lep
pred.all= pred.all[which(pred.all$dataset %in% c("eplants","lep")),]

#order models by complexity
pred.all$model= factor(pred.all$model, levels=c("LM","LM poly", "RR", "RR poly", "SVM linear", "SVM rbf", "RF"), ordered=TRUE)

dat.labs2<- c("Plants", "Moths")
names(dat.labs2)<- c("eplants", "lep")

pred.plot= ggplot(pred.all) + aes(y=.pred, x = truth, color=model)+
  geom_abline(lty = 2, color = "gray80", linewidth = 1.5) +
  geom_point(alpha = 0.7) +
  facet_grid(datasub~dataset, labeller = labeller(dataset = dat.labs2))+
  scale_color_discrete(type=cols.mod[-length(cols.mod)])+
  labs(
    x = "Observed shift",
    y = "Predicted shift",
    color = "Model"
  )+ theme_bw()

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/figures/")
pdf("Fig5_ PredPlot.pdf",height = 8, width = 10)
pred.plot
dev.off()

#===============================

