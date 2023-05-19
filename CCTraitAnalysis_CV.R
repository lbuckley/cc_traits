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

#----
#read data

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/data")

mammals= read.csv("mammals01.csv")
plants= read.csv("plants5.csv")
fish= read.csv("west-coast-triennial _species_generaltraits.csv")
eplants= read.csv("rumpf_ShiftsTraitsBuckley_20180418.csv")
lepbird= read.csv("Data_Shifts_NicheMetrics_Traits.csv")

datasets= c("plants","eplants", "lep", "fish", "birds", "mammals")

#----
#Process data

#mammals
# Mammals: alt timit, longevity

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
trait.labs <- c("Altitudinal limit (m)","Longevity (yrs)","Litters per yr","Litter size","log Range size (km2)","log Mass (g)","Daily rhythm","Annual rhythm","Diet breadth","Temp mean","Temp breadth")
names(trait.labs) <- c("Orig_high_limit","Longevity_yrs","Litters_per_yr","Litter_size","Rangesize_km2","Mass_g","DailyRhythm","AnnualRhythm","DietBreadth","Bio1_mean","Bio1_std")

#plot
plot.mammal= ggplot(mammals.l) + aes(x=value, y = High_change)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) + 
  ggtitle('Mammals')+
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
plants=plants[,c("Taxon","migration_m","earliest_seed_shed_mo","seed_shed_dur_mos",
                 "nichebreadth_num_flor_zones", "BreedSysCode",
                 "Ave_seed_shed_ht_m","flwr_dur_mos","DispersalMode",
                 "diaspore_mass_mg","nichebreadth_amplit_ocean","Nbound_lat_GBIF_nosyn")]

#restrict variables to increase datasets
plants=plants[,c("Taxon","migration_m","earliest_seed_shed_mo","seed_shed_dur_mos",
                 "Nbound_lat_GBIF_nosyn")]

#check correlations
r <- cor(plants[,c(3:12)], use="complete.obs")
ggcorrplot(r)

#to long
plants.l<- plants %>%
  gather("trait", "value", 3:ncol(plants))
plants.l$value= as.numeric(plants.l$value)

## Facet labels
trait.labs <- c("Earliest seed shed (mo)","Seed shed duration (mo)","Number floristic zones", "Breeding System Code",
                "Seed shed height (m)","Flower duration (mo)","Dispersal mode",
                "Diaspore mass (mg)","Number oceanic zones","Northern Latitude (degrees)")
names(trait.labs) <- c("earliest_seed_shed_mo","seed_shed_dur_mos",
                       "nichebreadth_num_flor_zones", "BreedSysCode",
                       "Ave_seed_shed_ht_m","flwr_dur_mos","DispersalMode",
                       "diaspore_mass_mg","nichebreadth_amplit_ocean","Nbound_lat_GBIF_nosyn")

#plot  
plot.aplants= ggplot(plants.l) + aes(x=value, y = migration_m)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) +ggtitle('Alpine plants') +
  theme_bw()+ylab("Elevation shift (m)") #+ stat_smooth(method='lm', formula= y~poly(x,2))
#+scale_x_log10()

#scale and center
plants <- plants %>%
  mutate(earliest_seed_shed_mo = scale(earliest_seed_shed_mo),
         seed_shed_dur_mos = scale(seed_shed_dur_mos),
         Nbound_lat_GBIF_nosyn = scale(Nbound_lat_GBIF_nosyn))

#----
#fish

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
trait.labs <- c("Habitat","log Depth Range (?)","log Length (?)","Vulnerability","Water Type")
names(trait.labs) <- c("habitat","DepthRangeDeep","Length","Vulnerability","WaterType")

#plot
plot.fish= ggplot(fish.l) + aes(x=value, y = Latitudinal.Difference)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) +ggtitle('Triennial marine survey') +
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
ls.codes=c("ccc","ccs","css","crs","sss","rss","rrs") 
eplants$LifeStrategy= match(eplants$LifeStrategy, ls.codes)

#log transform
eplants$LifeSpan= log(eplants$LifeSpan)
eplants$SeedReleaseHeight= log(eplants$SeedReleaseHeight)

#restrict traits
#eplants=eplants[,c("speciesname","LeadingEdge","TemperatureIndicator","NutrientIndicator","Dispersal","Persistence","RetInFurSheep","GutSurvival")]
eplants=eplants[,c("speciesname","LeadingEdge","TemperatureIndicator","NutrientIndicator","SeedReleaseHeight","LifeStrategy","LifeSpan","NoOfVegOffspings","Dispersal","Persistence")]
#Look into adding Historic position, Historical optimum

#check correlations
r <- cor(eplants[,c(3:12)], use="complete.obs")
ggcorrplot(r)

#to long
eplants.l<- eplants %>%
  gather("trait", "value", 3:ncol(eplants))

# Facet labels
trait.labs <- c("Temperature Indicator","Nutrient Indicator","Sheep Fur Retention","Gut Survival","Seed Release Height","Life Strategy","LifeSpan","N Veg Offsping","Dispersal","Persistence")
names(trait.labs) <- c("TemperatureIndicator","NutrientIndicator","RetInFurSheep","GutSurvival","SeedReleaseHeight","LifeStrategy","LifeSpan","NoOfVegOffspings","Dispersal","Persistence")

#plot
plot.eplants= ggplot(eplants.l) + aes(x=value, y = LeadingEdge)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) +ggtitle('European plants') +
  theme_bw()+ylab("Elevational shift (m)") #+stat_smooth(method='lm', formula= y~poly(x,2))
#check LeadingEdge, Optimum, RearEdge

#scale and center
eplants <- eplants %>%
  mutate(Dispersal = scale(Dispersal),
         LifeSpan = scale(LifeSpan),
         NoOfVegOffspings = scale(NoOfVegOffspings),
         Persistence = scale(Persistence),
         SeedReleaseHeight = scale(SeedReleaseHeight) )

#make factors
eplants$LifeStrategy= factor(eplants$LifeStrategy, ordered=TRUE)
eplants$NutrientIndicator= factor(eplants$NutrientIndicator, ordered=TRUE)
eplants$TemperatureIndicator= factor(eplants$TemperatureIndicator, ordered=TRUE)

#----
#Add lepidoptera and birds
# https://academic.oup.com/evlett/advance-article/doi/10.1093/evlett/qrad004/7076361
# Moth: move further N with bigger temp mean and and narrower breath
# Bird: move further N with narrowed temp breadth and resident wintering
# Butterfly: no strong predictors

#restrict traits
lepbird= lepbird[,c("Species","Taxonomic.group","D_border_0.9",
                    "temp.mean","temp.sd","precip.mean","wintering","body.size","num.gen")] #"precip.sd",

#split moths and birds
#moth= lepbird[lepbird$Taxonomic.group %in% c("Moth"),]
#butterfly= lepbird[lepbird$Taxonomic.group %in% c("Butterfly"),]
lep= lepbird[lepbird$Taxonomic.group %in% c("Moth", "Butterfly"),]
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
trait.labs <- c("Range Size","Wintering","Body size","Number Generations","Temp Mean","Temp Breadth","Moisture Mean") 
names(trait.labs) <- c("range.size","wintering","body.size","num.gen","temp.mean","temp.sd","precip.mean")  

#plot
plot.lep= ggplot(lep.l) + aes(x=value, y = D_border_0.9)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) +ggtitle('Lepidopterans') +
  theme_bw()+ylab(" shift (degree)") 

plot.bird= ggplot(bird.l) + aes(x=value, y = D_border_0.9)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) +ggtitle('Birds') +
  theme_bw()+ylab(" shift (degree)") 

#set up ordered factors
bird$num.gen= factor(bird$num.gen, ordered=TRUE)
lep$num.gen= factor(lep$num.gen, ordered=TRUE)
bird$wintering= factor(bird$wintering, ordered=TRUE)
lep$wintering= factor(lep$wintering, ordered=TRUE)

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

# install.packages(c('tidyverse', 'tidymodels', 'ranger', 'vip', 'palmerpenguins', 'skimr', 'paletteer', 'nnet', 'here'))
library(tidymodels)
library(ISLR)

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
}

if(dat.k==2){
  #PLANTS
  #set up data
  dat=plants
  dat$y= dat$migration_m
  #drop ID rows
  dat<- dat[,3:ncol(dat)]
}

if(dat.k==3){
  #FISH
  #set up data
  dat=fish
  dat$y= dat$Latitudinal.Difference
  #drop ID rows
  dat<- dat[,3:ncol(dat)]
}

if(dat.k==4){
  #EURO PLANTS
  #set up data
  dat=eplants
  dat$y= dat$LeadingEdge
  #drop ID rows
  dat<- dat[,3:ncol(dat)]
}

if(dat.k==5){
  #LEPS
  #set up data
  dat=lep
  dat$y= dat$D_border_0.9
  #drop ID rows
  dat<- dat[,4:ncol(dat)]
 }

if(dat.k==6){
  #BIRDS
  #set up data
  dat=bird
  dat$y= dat$D_border_0.9
  #drop ID rows
  dat<- dat[,4:ncol(dat)]
}

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

#train<- na.omit(train)

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
#rec_poly <- recipe(y ~ ., data = train) %>%
#  step_center(all_numeric_predictors())%>%
#  step_normalize(all_numeric_predictors())%>%
#   step_poly(mget(preds.poly), degree = 2)
#######need to generalize

if(dat.k==1){
  rec_poly <- recipe(y ~ ., data = train) %>%
    #step_poly(Orig_high_limit, degree = 2) %>% #Orig_high_limit,Rangesize_km2, Mass_g,
    step_ordinalscore(DietBreadth)
  }

if(dat.k==2){
  rec_poly <- recipe(y ~ ., data = train) #%>%
    #step_poly(earliest_seed_shed_mo, degree = 2) #%>%
    #step_poly(earliest_seed_shed_mo,seed_shed_dur_mos,nichebreadth_num_flor_zones,
    #          Ave_seed_shed_ht_m,diaspore_mass_mg,Nbound_lat_GBIF_nosyn, degree = 2)
    #step_ordinalscore(BreedSysCode, DispersalMode)
}

if(dat.k==3){
  rec_poly <- recipe(y ~ ., data = train) #%>%
  #  step_poly(DepthRangeDeep, degree = 2) %>% #DepthRangeDeep, Length, Vulnerability, 
  #  step_ordinalscore(habitat,WaterType)
}

if(dat.k==4){
  rec_poly <- recipe(y ~ ., data = train) %>%
#    step_poly(SeedReleaseHeight, degree = 2) %>% #SeedReleaseHeight,LifeSpan,Dispersal,Persistence
    step_ordinalscore(TemperatureIndicator,NutrientIndicator,LifeStrategy)
}

if(dat.k %in% c(5,6)){
  rec_poly <- recipe(y ~ ., data = train) %>%
    step_poly(body.size, degree = 2) %>%
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

#Fit
#lm_fit_poly <- fit(lm_spec_poly, y ~ ., train_poly)

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
if(dat.k==1){
  rec_glmn_poly <- recipe(y ~ ., data = train) %>%
   # step_poly(Rangesize_km2, degree = 2)%>% #Orig_high_limit, Rangesize_km2, Mass_g,
    #step_ordinalscore(all_factor())
    step_ordinalscore(DietBreadth)
}

if(dat.k==2){
  rec_glmn_poly <- recipe(y ~ ., data = train) #%>%
    #step_poly(earliest_seed_shed_mo,seed_shed_dur_mos,nichebreadth_num_flor_zones,
    #          Ave_seed_shed_ht_m,diaspore_mass_mg,Nbound_lat_GBIF_nosyn, degree = 2)%>%
    #step_poly(Nbound_lat_GBIF_nosyn, degree = 2) #%>%
    #step_ordinalscore(BreedSysCode, DispersalMode)
}

if(dat.k==3){
  rec_glmn_poly <- recipe(y ~ ., data = train) %>%
    #step_poly(DepthRangeDeep, Length, Vulnerability, degree = 2)%>%
    step_ordinalscore(habitat,WaterType)
}

if(dat.k==4){
  rec_glmn_poly <- recipe(y ~ ., data = train) %>%
    #step_poly(SeedReleaseHeight,LifeSpan,Dispersal,Persistence, degree = 2) #%>%
    step_ordinalscore(TemperatureIndicator, NutrientIndicator, LifeStrategy)
}

if(dat.k %in% c(5,6)){
  rec_glmn_poly <- recipe(y ~ ., data = train) %>%
    step_poly(body.size, degree = 2)%>%
    #step_ordinalscore(all_factor())
    step_ordinalscore(wintering, num.gen)
}

glmn_spec_poly <- linear_reg(penalty = 0.001, mixture = 0.5) %>%
  set_engine(engine = "glmnet")

glmn_wf_poly <- 
  workflow() %>%
  add_model(glmn_spec_poly) %>%
  add_recipe(rec_glmn_poly)

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
folds <- vfold_cv(na.omit(dat), v=7) #Or K=5 in N<20?

#lm
set.seed(456)
lm_res <- fit_resamples(
  lm_wf,
  folds,
  control = control_resamples(save_pred = TRUE)
)

cv.all= lm_res %>%
  collect_metrics()
cv.all$Model="lm"

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
cv.metric$Model="lm poly"
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
cv.metric$Model="rr"
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
cv.metric$Model="rr poly"
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
cv.metric$Model="svm rbf"
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
cv.metric$Model="svm linear"
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
cv.metric$Model="rf"
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
  workflow(y ~ ., lm_spec_poly) %>%
  fit_resamples(folds, control = ctrl_imp)

#RR
glmn_res <-
  workflow(y ~ ., glmn_spec) %>%
  fit_resamples(folds, control = ctrl_imp)

glmn_poly_res <-
  workflow(y ~ ., glmn_spec_poly) %>%
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

#Gather VI
mods= c("lm","lm poly","rr","rr poly","rf")

for(mod.k in 1:5){

if(mod.k==1) mod_res<- lm_res
if(mod.k==2) mod_res<- lm_poly_res
if(mod.k==3) mod_res<- glmn_res
if(mod.k==4) mod_res<- glmn_poly_res
if(mod.k==5) mod_res<- rf_res

mod.vi= mod_res %>%
  select(id, .extracts) %>%
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

#svm doesn't include signs 
#add svm
vi.svm1= as.data.frame(matrix(NA, nrow= nrow(svm_linear_vi),ncol=6))
names(vi.svm1)= names(vi.mods)
vi.svm1$Variable= svm_linear_vi$Variable
vi.svm1$Mean= abs(svm_linear_vi$Importance)
vi.svm1$Variance= svm_linear_vi$StDev
vi.svm1$Mean.sign= svm_linear_vi$Importance
vi.svm1$Variance.sign= svm_linear_vi$StDev
vi.svm1$Model<- "svm linear"
vi.mods= rbind(vi.mods, vi.svm1)

vi.svm1= as.data.frame(matrix(NA, nrow= nrow(svm_rbf_vi),ncol=6))
names(vi.svm1)= names(vi.mods)
vi.svm1$Variable= svm_rbf_vi$Variable
vi.svm1$Mean= abs(svm_linear_vi$Importance)
vi.svm1$Variance= svm_linear_vi$StDev
vi.svm1$Mean.sign= svm_linear_vi$Importance
vi.svm1$Variance.sign= svm_linear_vi$StDev
vi.svm1$Model<- "svm rbf"
vi.mods= rbind(vi.mods, vi.svm1)
}  #end loop models

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

} #end loop datasets

#=============================
#PLOTS
#setwd for figures
setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/figures/")

#Trait plots
pdf("Fig0_alltraits.pdf", onefile = TRUE)
print(plot.aplants)
print(plot.eplants)
print(plot.lep)
print(plot.fish)
print(plot.bird)
print(plot.mammal)
dev.off()

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
cv.dat$Model= factor(cv.dat$Model, levels=c("lm","lm poly", "rr", "rr poly", "svm linear", "svm rbf", "rf"), ordered=TRUE)
#order datasets
cv.dat$dataset= factor(cv.dat$dataset, levels=c("plants","eplants", "lep", "fish", "birds", "mammals"), ordered=TRUE)

#plot
cv.plot= ggplot(cv.dat) + aes(y=MeanScale, x = Model)+geom_point(size=2)+ #geom_line()+
  facet_grid(.metric~dataset, scales="free_y")+theme_bw()

cv.plot= cv.plot + 
  geom_errorbar(data=cv.dat, aes(x=Model, y=MeanScale, ymin=MeanScale-SterrScale, ymax=MeanScale+SterrScale), width=0, col="black")

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/figures/")
pdf("CvPlot.pdf",height = 6, width = 15)
cv.plot
dev.off()
  
#------------------
#VI plots with cross validation

#scale to max value
vi.dat2= as.data.frame(vi.dat)

vi.max= vi.dat2 %>%
group_by(dataset, Model) %>%
summarize(max.mean=max(Mean),
          max.mean.sign=max(Mean.sign))
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

vi.dat2$Variable[grep("TemperatureIndicator", vi.dat2$Variable, value = FALSE)]<- "TemperatureIndicator"
vi.dat2$Variable[grep("LifeStrategy", vi.dat2$Variable, value = FALSE)]<- "LifeStrategy"

#reduce to max
vi.dat2$dat_mod_var= paste(vi.dat2$dat_mod, vi.dat2$Variable, sep="_")

vi.dat.max= aggregate(vi.dat2, by=list(vi.dat2$dat_mod_var), FUN="max")

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

#order datasets
vi.dat2$dataset= factor(vi.dat2$dataset, levels=c("plants","eplants", "lep", "fish", "birds", "mammals"), ordered=TRUE)

vi.plot= ggplot(vi.dat2) + aes(y=MeanScale, x = Variable, color=Model, group=Model)+geom_point()+geom_line()+
  facet_grid(.~dataset, scales="free_y") + theme(axis.text.x = element_text(angle=90))

#---------------------
#split by dataset and split
library(patchwork)
vi.plots <- vector('list', length(datasets))
vi.plots.sign <- vector('list', length(datasets))

for(dat.k in 1:6){
  vi.plot= ggplot(vi.dat.max[vi.dat.max$dataset==datasets[dat.k],]) + aes(y=MeanScale, x = Variable, color=Model, group=Model, lty=IsMeanImp)+geom_point(size=2)+geom_line()+
    #facet_grid(.~RF, scales="free")+
    ggtitle(datasets[dat.k])+theme_bw()+ylim(0,1)
  
  #vi.plot= vi.plot + 
  #  geom_errorbar(data=vi.dat2[vi.dat2$dataset==datasets[dat.k],], aes(y=MeanScale, x = Variable, ymin=MeanScale-StdevScale, ymax=MeanScale+StdevScale), width=0, col="black")
  
  if(dat.k<6) vi.plot=vi.plot + theme(legend.position = "none")
  vi.plots[[dat.k]]= vi.plot+ coord_flip()
  
  #account for sign
  vi.plot= ggplot(vi.dat2[vi.dat2$dataset==datasets[dat.k],]) + aes(y=MeanScale.sign, x = Variable, color=Model, group=Model, lty=IsMeanImp)+geom_point(size=2)+geom_line()+
    #facet_grid(.~RF, scales="free")+
    ggtitle(datasets[dat.k])+theme_bw()+
    geom_hline(yintercept = 0, color="gray",lwd=1)
  
  #vi.plot= vi.plot + 
  #  geom_errorbar(data=vi.dat2[vi.dat2$dataset==datasets[dat.k],], aes(y=MeanScale, x = Variable, ymin=MeanScale-StdevScale, ymax=MeanScale+StdevScale), width=0, col="black")
  
  if(dat.k<6) vi.plot=vi.plot + theme(legend.position = "none")
  vi.plots.sign[[dat.k]]= vi.plot+ coord_flip()
}

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/figures/")
pdf("ViPlotsCV.pdf",height = 8, width = 14)
(vi.plots[[1]] | vi.plots[[2]] | vi.plots[[3]])/
  (vi.plots[[4]] | vi.plots[[5]] | vi.plots[[6]])
dev.off()

pdf("ViPlotsCV_sign.pdf",height = 8, width = 14)
(vi.plots.sign[[1]] | vi.plots.sign[[2]] | vi.plots.sign[[3]])/
  (vi.plots.sign[[4]] | vi.plots.sign[[5]] | vi.plots.sign[[6]])
dev.off()

#===============================
