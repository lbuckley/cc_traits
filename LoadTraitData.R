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

# eval: k-fold cross-validation scheme (Hastie et al. 2009, Section 7.10) combined with a mean squared error metric
# https://rsample.tidymodels.org/reference/vfold_cv.html

#set up data
dat=na.omit(plants)
dat$y= dat$migration_m
#drop ID rows
dat<- dat[,3:ncol(dat)]

preds <-  c("earliest_seed_shed_mo","seed_shed_dur_mos","nichebreadth_num_flor_zones","BreedSysCode","Ave_seed_shed_ht_m",
            "flwr_dur_mos","DispersalMode","diaspore_mass_mg","nichebreadth_amplit_ocean","Nbound_lat_GBIF_nosyn")
#predictors to consider as polynomials
preds.poly <-  c("earliest_seed_shed_mo","seed_shed_dur_mos","nichebreadth_num_flor_zones","Ave_seed_shed_ht_m",
            "flwr_dur_mos","diaspore_mass_mg","Nbound_lat_GBIF_nosyn")

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

#OLS Poly
rec_poly <- recipe(y ~ ., data = train) %>%
  step_center(all_numeric_predictors())%>%
  step_poly(., degree = 2)
#Fix step poly

lm_wf_poly <- 
  workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(rec_poly)

lm_fit_poly <-
  fit(lm_wf_poly,
      data = train
  )

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
rf_spec <- rand_forest(mode = "regression", trees = 100) %>%
  set_engine("ranger")

rf_fit <- rf_spec %>%
  fit(y ~ ., data = train
  )

rf_wf <- 
  workflow() %>%
  add_model(rf_spec) %>%
  add_formula(y ~ .)

#=====================
#Evaluation
# https://juliasilge.com/blog/intro-tidymodels/

results_train <- lm_fit %>%
  predict(new_data = train) %>%
  mutate(
    truth = train$y,
    model = "lm"
  ) %>%
  bind_rows(lm_fit_poly %>%
              predict(new_data = train) %>%
              mutate(
                truth = train$y,
                model = "lm_poly"
              ))%>%
  bind_rows(glmn_fit %>%
              predict(new_data = train) %>%
              mutate(
                truth = train$y,
                model = "ridgereg"
              ))%>%
  bind_rows(svm_rbf_fit %>%
              predict(new_data = train) %>%
              mutate(
                truth = train$y,
                model = "svm_rbf"
              ))%>%
  bind_rows(svm_linear_fit %>%
              predict(new_data = train) %>%
              mutate(
                truth = train$y,
                model = "svm_linear"
              ))%>%
  bind_rows(rf_fit %>%
              predict(new_data = train) %>%
              mutate(
                truth = train$y,
                model = "rf"
              ))

results_test <- lm_fit %>%
  predict(new_data = test) %>%
  mutate(
    truth = test$y,
    model = "lm"
  ) %>%
  bind_rows(lm_fit_poly %>%
              predict(new_data = test) %>%
              mutate(
                truth = test$y,
                model = "lm_poly"
              ))%>%
  bind_rows(glmn_fit %>%
              predict(new_data = test) %>%
              mutate(
                truth = test$y,
                model = "ridgereg"
              ))%>%
  bind_rows(svm_rbf_fit %>%
              predict(new_data = test) %>%
              mutate(
                truth = test$y,
                model = "svm_rbf"
              ))%>%
  bind_rows(svm_linear_fit %>%
              predict(new_data = test) %>%
              mutate(
                truth = test$y,
                model = "svm_linear"
              ))%>%
  bind_rows(rf_fit %>%
              predict(new_data = test) %>%
              mutate(
                truth = test$y,
                model = "rf"
              ))

#rmse
results_train %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)

results_test %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)

#visualize
results_test %>%
  mutate(train = "testing") %>%
  bind_rows(results_train %>%
              mutate(train = "training")) %>%
  ggplot(aes(truth, .pred, color = model)) +
  geom_abline(lty = 2, color = "gray80", linewidth = 1.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(~train) +
  labs(
    x = "Truth",
    y = "Predicted shift",
    color = "Type of model"
  )

#===============================
#cross validation 
set.seed(1234)
folds <- vfold_cv(train, v=10) #Or K=5 in N<20?

#lm
set.seed(456)
lm_res <- fit_resamples(
  lm_wf,
  folds,
  control = control_resamples(save_pred = TRUE)
)

lm_res %>%
  collect_metrics()

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
#glmn
set.seed(456)
glmn_res <- fit_resamples(
  glmn_wf,
  folds,
  control = control_resamples(save_pred = TRUE)
)

glmn_res %>%
  collect_metrics()

glmn_res %>%
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
#svm_rbf
set.seed(456)
svm_rbf_res <- fit_resamples(
  svm_rbf_wf,
  folds,
  control = control_resamples(save_pred = TRUE)
)

svm_rbf_res %>%
  collect_metrics()

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

svm_linear_res %>%
  collect_metrics()

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

rf_res %>%
  collect_metrics()

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

# OLS
set.seed(345)
last_lm_fit <- 
  lm_wf %>% 
  last_fit(dat_split)

last_lm_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20, geom="point")

# GLM
set.seed(345)
last_glmn_fit <- 
  glmn_wf %>% 
  last_fit(dat_split)

last_glmn_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20, geom="point")

#No VIP for SVM or RF as currently configured, WORKING ON

#SVM
#https://stackoverflow.com/questions/62772397/integration-of-variable-importance-plots-within-the-tidy-modelling-framework
#permutation based
svm_fit <- workflow() %>%
  add_model(svm_linear_spec) %>%
  add_formula(y ~ .) %>%
  fit(train)

svm_fit %>%
  pull_workflow_fit() %>%
  vip(method = "permute", 
      target = "y", metric = "rsquared",
      pred_wrapper = kernlab::predict, train = train)

#Python implementation uses Shapley-based scores for Kernel and SVM
svm_fit %>%
  pull_workflow_fit() %>%
  vip(method = "shap", 
      #target = "y", metric = "rsquared",
      pred_wrapper = kernlab::predict, train = train, feature_names= names(train)[1:ncol(train)-1])

#https://stackoverflow.com/questions/67833723/r-tidymodels-vip-variable-importance-determination
#method = c("model", "firm", "permute", "shap")
#  "model" (the default), for model-specific VI scores (see vi_model() for details).
#  "firm", for variance-based VI scores (see vi_firm() for details).
#  "permute", for permutation-based VI scores (see vi_permute for details).
#  "shap", for Shapley-based VI scores.

#Phython implementation uses Gini scores for RF, but they appear to be for classification

#https://cran.r-project.org/web/packages/vip/vip.pdf
#rank=TRUE for ranks

#Model metrics
#vip::metric_mse

#Polynomial expansions
#https://emilhvitfeldt.github.io/ISLR-tidymodels-labs/07-moving-beyond-linearity.html

#rec_poly <- recipe(wage ~ age, data = Wage) %>%
#  step_poly(age, degree = 4)
# poly_wf <- workflow() %>%
#  add_model(lm_spec) %>%
#  add_recipe(rec_poly)
#add orthogonal polynomials 
#https://recipes.tidymodels.org/reference/step_poly.html

