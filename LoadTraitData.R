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

#----
#Make plot of trait predictors for main text and supplement

#Vars to plot?
# alpine plants: seed shed month earliest, number floristic zones
# European plants: temp indicator, seed release height
# Mammals: alt timit, longevity
# Fish: depth, benthopelagic, vulnerability

#----
#read data

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/StudentsPostdocs/Cannistra/Traits/data")

mammals= read.csv("mammals01.csv")
plants= read.csv("plants5.csv")
fish= read.csv("west-coast-triennial _species_generaltraits.csv")
eplants= read.csv("rumpf_ShiftsTraitsBuckley_20180418.csv")
lepbird= read.csv("Data_Shifts_NicheMetrics_Traits.csv")

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
mammals=mammals[,c("Taxon","High_change","Orig_high_limit","Longevity_yrs","Litters_per_yr","Litter_size","Rangesize_km2","Mass_g","DietBreadth","DailyRhythm","AnnualRhythm")]

#check correlations
r <- cor(mammals[,c(3:10)], use="complete.obs")
ggcorrplot(r)

#to long
mammals.l<- mammals %>%
  gather("trait", "value", 3:ncol(mammals))

## Facet labels
trait.labs <- c("Altitudinal limit (m)","Longevity (yrs)","Litters per yr","Litter size","Range size (km2)","Mass (g)","Daily rhythm","Annual rhythm","Diet breadth")
names(trait.labs) <- c("Orig_high_limit","Longevity_yrs","Litters_per_yr","Litter_size","Rangesize_km2","Mass_g","DailyRhythm","AnnualRhythm","DietBreadth")

#plot
plot.m= ggplot(mammals.l) + aes(x=value, y = High_change)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) + 
  ggtitle('Mammals')+
  theme_bw()+ylab("Elevation shift (m)") #+stat_smooth(method='lm', formula= y~poly(x,2))
#+geom_smooth(se=FALSE) #+scale_x_log10()

#scale
mammals$Orig_high_limit= scale(mammals$Orig_high_limit)
mammals$Rangesize_km2= scale(mammals$Rangesize_km2)
mammals$Mass_g= scale(mammals$Mass_g)
mammals$Young_per_yr= scale(mammals$Young_per_yr)

#mod
mod1= lm(High_change~Orig_high_limit+Longevity_yrs+Bio1_std+Litters_per_yr+Young_per_yr+Rangesize_km2+Mass_g+Daily_rhythm_code+Annual_rhythm_code, data=mammals)
#mod1= lm(High_change~poly(Orig_high_limit)+poly(Longevity_yrs)+poly(Bio1_std)+poly(Litters_per_yr)+poly(Young_per_yr)+poly(Rangesize_km2)+poly(Mass_g)+poly(Daily_rhythm_code)+poly(Annual_rhythm_code), data=mammals)
mod1= lm(High_change~poly(Orig_high_limit)*poly(Longevity_yrs), data=mammals)

#plot model
plot_model(mod1, type="pred", terms=c("Orig_high_limit"), show.data=TRUE)

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
plot.ap= ggplot(plants.l) + aes(x=value, y = migration_m)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) +ggtitle('Alpine plants') +
  theme_bw()+ylab("Elevation shift (m)") #+ stat_smooth(method='lm', formula= y~poly(x,2))
#+scale_x_log10()

#mod
#scale
plants$diaspore_ave_len_mm= scale(plants$diaspore_ave_len_mm)
plants$diaspore_min_len_mm = scale(plants$diaspore_min_len_mm)
plants$diaspore_mass_mg = scale(plants$diaspore_mass_mg)
plants$MaxAlt = scale(plants$MaxAlt)
plants$seed_mass_mg = scale(plants$seed_mass_mg)

mod1= lm(migration_m~earliest_seed_shed_mo+latest_seed_shed_mo+seed_shed_dur_mos+nichebreadth_num_flor_zones+             
           diaspore_min_len_mm+BreedSysCode+diaspore_ave_len_mm+MaxAlt+seed_mass_mg+oceanity+Min_seed_shed_ht_m+
           flwr_mo_end+dispmode01+flwr_mo_start+diaspore_mass_mg+nichebreadth_amplit_ocean+  
           StorageOrgan+Max_seed_shed_ht_m, data=plants)

#plot model
plot_model(mod1, type="pred", terms=c("seed_mass_mg"), show.data=TRUE)
plot_model(mod1, type="pred", terms=c("diaspore_mass_mg"), show.data=TRUE)

#----
#fish

#Make salt / fresh variable
fish$WaterType=0
fish$WaterType[fish$Brack== -1]=1
fish$WaterType[fish$Fresh== -1]=2

#compress habitats
#finalize how to code
fish$habitat= as.numeric(factor(fish$DemersPelag, 
                                levels=c("bathydemersal","demersal","benthopelagic","pelagic-oceanic","pelagic-neritic","reef-associated")))

#restrict traits
fish=fish[,c("Species","Latitudinal.Difference","habitat","DepthRangeDeep","Length","WaterType","Vulnerability")]
#include vulnerability

#to long
fish.l<- fish %>%
  gather("trait", "value", 3:ncol(fish))

# Facet labels
trait.labs <- c("Habitat","Depth Range (?)","Length (?)","Vulnerability","Water Type")
names(trait.labs) <- c("habitat","DepthRangeDeep","Length","Vulnerability","WaterType")

#plot
plot.tms= ggplot(fish.l) + aes(x=value, y = Latitudinal.Difference)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) +ggtitle('Triennial marine survey') +
  theme_bw()+ylab("Latitudinal shift (°)") #+stat_smooth(method='lm', formula= y~poly(x,2))

#----
#European plants

#to numeric
#Check codes: (1) C-competitors, (2) S-stress tolerators, and (3) R-ruderals.
ls.codes=c("ccc","ccs","css","crs","sss","rss","rrs") 
eplants$LifeStrategy= match(eplants$LifeStrategy, ls.codes)

#restrict traits
#eplants=eplants[,c("speciesname","LeadingEdge","TemperatureIndicator","NutrientIndicator","Dispersal","Persistence")]
eplants=eplants[,c("speciesname","LeadingEdge","TemperatureIndicator","NutrientIndicator","RetInFurSheep","GutSurvival","SeedReleaseHeight","LifeStrategy","LifeSpan","NoOfVegOffspings","Dispersal","Persistence")]
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
plot.ep= ggplot(eplants.l) + aes(x=value, y = LeadingEdge)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) +ggtitle('European plants') +
  theme_bw()+ylab("Elevational shift (m)") #+stat_smooth(method='lm', formula= y~poly(x,2))
#check LeadingEdge, Optimum, RearEdge

mod1= lm(LeadingEdge~RelativeAbundance+TemperatureIndicator+NutrientIndicator+TerminalVelocity+
           RetInFurCattle+RetInFurSheep+GutSurvival+SeedReleaseHeight+Dispersal+
           LifeStrategy+LifeSpan+Dominance+NoOfVegOffspings+Persistence, data=eplants)

Anova(mod1, type=3)

#----
#Add lepidoptera and birds

#restrict traits
lepbird= lepbird[,c("Species","Taxonomic.group","D_border_0.9",
                    "range.size","wintering","body.size","num.gen")]

#split moths and birds
lep= lepbird[lepbird$Taxonomic.group %in% c("Moth","Butterfly"),]
bird=  lepbird[lepbird$Taxonomic.group == "Bird",]

#Code overwintering stage
#Lep
wint.stage= c("egg","larva","pupa","adult")
lep$wintering= match(lep$wintering, wint.stage)         
#bird, resident; short-distance migrant, and long-distance migrant
bird.stage= c("R","S","L")
bird$wintering= match(bird$wintering, bird.stage)  
           
#check correlations
r <- cor(lep[,c(3:7)], use="complete.obs")
r <- cor(bird[,c(3:7)], use="complete.obs")
ggcorrplot(r)

#to long
lep.l<- lep %>%
  gather("trait", "value", 4:ncol(lep))
bird.l<- bird %>%
  gather("trait", "value", 4:ncol(bird))

# Facet labels
trait.labs <- c("Range Size","Wintering","Body size","Number Generations") 
names(trait.labs) <- c("range.size","wintering","body.size","num.gen")  

#plot
plot.lep= ggplot(lep.l) + aes(x=value, y = D_border_0.9)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) +ggtitle('Lep') +
  theme_bw()+ylab(" shift (X)") 

plot.bird= ggplot(bird.l) + aes(x=value, y = D_border_0.9)+geom_point()+
  facet_wrap(~trait, scales="free", labeller = labeller(trait = trait.labs)) +ggtitle('Birds') +
  theme_bw()+ylab(" shift (X)") 

#----
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

# install.packages(c('tidyverse', 'tidymodels', 'ranger', 'vip', 'palmerpenguins', 'skimr', 'paletteer', 'nnet', 'here'))
library(tidymodels)

# k-fold cross-validation scheme (Hastie et al. 2009, Section 7.10) combined with a mean squared error metric
# https://rsample.tidymodels.org/reference/vfold_cv.html

#set up data
dat=na.omit(plants)
dat$y= dat$migration_m

preds <-  c("earliest_seed_shed_mo","seed_shed_dur_mos","nichebreadth_num_flor_zones","BreedSysCode","Ave_seed_shed_ht_m",
            "flwr_dur_mos","DispersalMode","diaspore_mass_mg","nichebreadth_amplit_ocean","Nbound_lat_GBIF_nosyn")

#split the data
# Split data into 70% for training and 30% for testing
set.seed(2056)
dat_split <- dat %>% 
  initial_split(prop = 0.70)

# Extract the data in each split
train <- training(dat_split)
test <- testing(dat_split)

#scale
dat$diaspore_mass_mg = scale(dat$diaspore_mass_mg)

#-----
#OLS


lr = lm(migration_m~earliest_seed_shed_mo+seed_shed_dur_mos+nichebreadth_num_flor_zones+BreedSysCode+
Ave_seed_shed_ht_m+flwr_dur_mos+DispersalMode+diaspore_mass_mg+nichebreadth_amplit_ocean, data = train)
  
#Step 1 - create the evaluation metrics function

eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  print(adj_r2) #Adjusted R-squared
  print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}

# Step 2 - predicting and evaluating the model on train data
predictions = predict(lr, newdata = train)
eval_metrics(lr, train, predictions, target = 'migration_m')

# Step 3 - predicting and evaluating the model on test data
predictions = predict(lr, newdata = test)
eval_metrics(lr, test, predictions, target = 'migration_m')

#----
# Ridge: “ridge”-regularized linear model
#https://www.tidymodels.org/learn/models/parsnip-ranger-glmnet/
#use scaled data

glmn_fit <- 
  linear_reg(penalty = 0.001, mixture = 0.5) %>% 
  set_engine("glmnet") %>%
  fit(y ~ ., data = train)

#----
# KernalRidge 
#radial basis function (or squared exponential) kernel applied to the training data
#https://adrian.ng/R/imp-krr/

#Not well implemented in R, try RBF SVM insteam

svm_rbf_spec <- svm_rbf(rbf_sigma = 0.2) %>%
  set_mode("regression") %>%
  set_engine("kernlab", scaled = FALSE)

svm_rbf_fit <- svm_rbf_spec %>% 
  set_args(cost = 10) %>%
  fit(y ~ ., data = dat)

#----
# SVR: support vector machine (SVM)

svm_linear_spec <- svm_poly(degree = 1) %>%
  set_mode("regression") %>%
  set_engine("kernlab", scaled = FALSE)

svm_linear_fit <- svm_linear_spec %>% 
  set_args(cost = 10) %>%
  fit(y ~ ., data = dat)

#----
# RF: Random forest
# https://www.tidymodels.org/learn/models/parsnip-ranger-glmnet/

rf_defaults <- rand_forest(mode = "regression")


rf_xy_fit <- 
  rf_defaults %>%
  set_engine("ranger") %>%
  fit_xy(
    x = dat[, preds],
    y = dat[,"migration_m"]
  )

#---------------
#Evaluation and plot

vip(rf_xy_fit)



# Predict rentals for the test set and bind it to the test_set
results <- test %>% 
  bind_cols(svm_linear_fit %>% 
              # Predict rentals
              predict(new_data = test) %>% 
              rename(predictions = .pred))

# Compare predictions
results %>% 
  select(c(rentals, predictions)) %>% 
  slice_head(n = 10)


