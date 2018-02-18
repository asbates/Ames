setwd('Desktop/Projects/Ames')

library(tidyverse)
library(caret)
library(Metrics)


train <- read_csv('train.csv')
test <- read_csv('test.csv')

# combine train and test for cleaning
full <- bind_rows(train, test, .id = 'id') 

# there are a lot of variables that have missing values
# since we'll be looking at these quite a bit, lets put this into a function
na_prop <- function(data){
  n <- dim(data)[1]
  prop <- apply(data, MARGIN = 2, FUN = function(x) sum(is.na(x)) / n)
  return(sort(prop, decreasing = TRUE))
}

na_prop(full)



# PoolQC, MiscFeature, Alley, Fence all have over 80% NA's
# Id isn't relevant
# let's just get rid of them
full <- select(full, -one_of(c('PoolQC', 'MiscFeature', 'Alley', 'Fence',
                               'Id')))

na_prop(full)

# FireplaceQu is almost half NA. most of the other values are 'Gd'
# we're just going to consider number of fireplaces and ignore quality
qplot(x = FireplaceQu, data = full)
full <- select(full, -FireplaceQu)

na_prop(full)

# next up is LotFrontage
# LotFrontage itself is a bit skewed
qplot(LotFrontage, data = full, binwidth = 10)

# the NA's might correspond to 0 i.e. apartments
# but most of the properties are 'RL' or residential low density 
# so that's probably not the case
qplot(MSZoning, data = full)

# for LotFrontage, we wil replace the NA's with the median value
# most of MSZoning is 'RL' and there are only a few NA's
# we'll replace these with 'RL'
med_frontage <- median(full$LotFrontage, na.rm = TRUE)
full <- replace_na(full, replace = list(LotFrontage = med_frontage, MSZoning = 'RL'))

na_prop(full)

# there are seven variables pertaining to garages
# GargeYrBlt (year garage built) is likely not as important as the year 
# the house was built so we will drop it
full <- select(full, -GarageYrBlt)

# GarageQual and GarageCond both measure the quality so they are redundant
# NA's here mean no garage so we will replace this with 'none'
# they both have essentially the same proportion of each category as well
qplot(GarageQual, data = full)
qplot(GarageCond, data = full)

# we will just consider GarageQual
full <- select(full, -GarageCond)

# NA in GarageType and GarageFinish means no garage so we will replace these as well
full <- replace_na(full, replace = list(GarageQual = 'none', GarageType = 'none',
                                        GarageFinish = 'none'))

# GarageCars and GarageArea both measure size so we will only use one of these
# typically people don't think of garages in terms of area but in terms of number
# of cars we will use GarageCars and assume there is no garage
full <- select(full, -GarageArea)
full <- replace_na(full, replace = list(GarageCars = 0))

na_prop(full)


# up next: the 11! variables concerning basements
# whoever cooked up that many ways to assess a basement must be a spinal tap fan
# for the categorical basement variables, we replace NA with 'none'
bsmt_replace_cat <- list(BsmtQual = 'none', BsmtCond = 'none', BsmtExposure = 'none',
                         BsmtFinType1 = 'none', BsmtFinType2 = 'none')
full <- replace_na(full, replace = bsmt_replace_cat)

# for the numerical variables, several measure square footage
# we keep only the total sf and replace any NA's with the median
full <- select(full, -one_of(c('BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF')))
med_bsmt_sf <- median(full$TotalBsmtSF, na.rm = TRUE)
full <- replace_na(full, replace = list(TotalBsmtSF = med_bsmt_sf))

# the last two basement variables are number of bathrooms
# NA's here probably mean no basement 
# in any case 0 is the most common and there are only 2 missing
# so we replace NA's with 0
qplot(BsmtFullBath, data = full)
qplot(BsmtHalfBath, data =full)

full <- replace_na(full, replace = list(BsmtFullBath = 0, BsmtHalfBath = 0))

na_prop(full)

# for masonry veneer, most have 'none' and 0 so this is what we replace with
qplot(MasVnrType, data = full)
qplot(MasVnrArea, data = full)

full <- replace_na(full, replace = list(MasVnrType = 'none', MasVnrArea = 0))

na_prop(full)

# Utilities is overwhelmingly 'AllPub'
# we probably won't get anything useful out of this so we drop it
qplot(Utilities, data = full)
full <- select(full, -Utilities)

# similarly for functional
qplot(Functional, data = full)
full <- select(full, -Functional)

# for Exterior1st and Exterior2nd, we replace with the most common values
qplot(Exterior1st, data = full)
qplot(Exterior2nd, data = full)
full <- replace_na(full, replace = list(Exterior1st = 'VinylSd',
                                        Exterior2nd = 'VinylSd'))

# we may not get much out of Electrical because most have the same value
# but we replace NA's with the mode and let our models decide
qplot(Electrical, data = full)
full <- replace_na(full, replace = list(Electrical = 'SBrkr'))

# KitchenQual has only one NA which we replace with the mode
qplot(KitchenQual, data = full)
full <- replace_na(full, replace = list(KitchenQual = 'TA'))

na_prop(full)

# and finally we have SaleType
# again we replace NA's with the most common value
qplot(SaleType, data = full)
full <- replace_na(full, replace = list(SaleType = 'WD'))

na_prop(full)

# that's it for missing values! (well, except for SalePrice, but that's our response)

# now we look at the rest of the variables
# Street is essentially all paved so we remove this
qplot(Street, data = full)
full <- select(full, -Street)

# Lotshape and LandContour describe the shape of the property
# both have several categories
# we convert these to 'Regular' and 'Irregular' or 'Level' and 'Not Level'
qplot(LotShape, data = full)
qplot(LandContour, data = full)
full <- full %>%
  mutate(LotShape = if_else(LotShape %in% c('IR1', 'IR2', 'IR3'),
                            'Irregular',LotShape))
full <- full %>%
  mutate(LandContour = if_else(LandContour %in% c('Bnk', 'HLS', 'Low'),
         'Not Level', LandContour))


# as far as transformations go, this is where we stop
# there may be more we can try, but this is what we consider sufficient

# we are left with 66 varibles
# we split our data back into training and testing 
# and remove the temporarily created id variable

train_clean <- filter(full, id == 1)
test_clean <- filter(full, id == 2)
train_clean <- select(train_clean, -id)
test_clean <- select(test_clean, -id)


# now we can split our test data and fit some models
# ideally, we would use the performance metric used by Kaggle:
# root mean square logarithmic error
# but I was having trouble doing this with Caret
train_index <- createDataPartition(train_clean$SalePrice, p = 0.8, list = FALSE)
training <- train_clean[train_index,]
testing <- train_clean[-train_index,]

# we'll use 10 fold CV with 5 repeats
control <- trainControl(method = 'repeatedcv', number = 10, repeats = 5)

# first we try an elastic net
enet_fit <- train(SalePrice ~., data = training, method = 'glmnet',
                  trControl = control, tuneLength = 10)

enet_pred <- predict(enet_fit, testing)
rmsle(testing$SalePrice, enet_pred)  # 0.1293949


# now let's try a random forrest
rf_fit <- train(SalePrice~., data = training, method = 'rf',
                trControl = control, tuneLength = 10)

rf_pred <- predict(rf_fit, testing)
rmsle(testing$SalePrice, rf_pred)  # 0.1244603


# and finally boosting
# this produces warnings about distribution = 'bernoulli' i think from the
# gbm package. i couldn't figure out what was going on
# but looking at various attributes of the model, it seems fine
# ignoring warnings without knowing exactly what they mean and that they are o.k.
# is not a good idea but i'm lost on this
gbm_fit <- train(SalePrice ~., data = training, method = 'gbm', verbose = FALSE,
                 trControl = control, tuneLength = 10)

gbm_pred <- predict(gbm_fit, testing)
rmsle(testing$SalePrice, gbm_pred)  # 0.1125172 (simple tune)


# based on the testing, it seems like gbm is the way to go
# now we fit to the full training set and make our submission
# we'll also submit the random forest to see how the scores compare

rf_fit$bestTune
rf_final <- train(SalePrice ~., data = train_clean, method = 'rf',
                  tuneGrid = data.frame(mtry = 74),
                  trControl = trainControl(method = 'none'))
rf_final_pred <- predict(rf_final, test_clean)

# read in sample submission file and overwrite with predicitons
rf_submission <- read_csv('sample_submission.csv')
rf_submission$SalePrice <- rf_final_pred
write_csv(rf_submission, 'rf_submission.csv')

gbm_fit$bestTune
gbm_grid <- data.frame(n.trees = 450, interaction.depth = 4, shrinkage = 0.1,
                       n.minobsinnode = 10)
gbm_final <- train(SalePrice ~., data = train_clean, method = 'gbm', verbose = FALSE,
                   tuneGrid = gbm_grid, trControl = trainControl(method = 'none'))

gbm_final_pred <- predict(gbm_final, test_clean)
gbm_submission <- read_csv('sample_submission.csv')
gbm_submission$SalePrice <- gbm_final_pred
write_csv(gbm_submission, 'gbm_submission.csv')
