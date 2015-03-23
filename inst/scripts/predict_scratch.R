## load global.R
library(caret)
library(AppliedPredictiveModeling)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  classProbs = TRUE,
  allowParallel = TRUE,
  ## repeated ten times
  repeats = 10,
  summaryFunction = twoClassSummary
  )

enetGrid <- expand.grid(alpha = seq(0, 1, by = 0.1),
                        lambda = c(seq(100, 1, -10), 1))
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = c(0.1))

## source global.R
source('global.R');
## first, benchmark with just glm with period diffs
pd <- nba_wl_games[,p3_vars ]
wins <- factor(ifelse(nba_wl_games$WIN ==1 , "WIN", "LOSS"))
rm(nba_wl_games)
## all variables have variance
nzv <- nearZeroVar(pd, saveMetrics = TRUE, foreach = TRUE)
pd_corr_m <- cor(pd)
summary(pd_corr_m[upper.tri(pd_corr_m)])
# touch time is very correlated
# but that's it, we'll keep them in
pd_corr <- findCorrelation(pd_corr_m, cutoff = 0.8)

# find linear combinations
linearC <- findLinearCombos(pd)
# none
# get one index with many samples
p_index <- createDataPartition(wins, p = 0.8, times = 10,
                                list = FALSE)
# period data, centered and scaled


# p1 glm
pdCenterScaler <- preProcess(pd)
ptrain <- predict(pdCenterScaler, pd[p_index[, 1], ])
ptest <- predict(pdCenterScaler, pd[-p_index[, 1], ])
ptrain$wins <- wins[p_index[, 1]]

train_by_period <- function(df, period = 1){

  form <- paste0('wins ~ ',
                 paste0(sapply(seq(1, period), function(p) {
                   sprintf('P%d_diff', p)}),
                   collapse = '+'))
  p_glm <- train(formula(form),
                  df,
                  method = "glm",
                  trControl = fitControl,
                  family = binomial,
                  metric = 'ROC')
  p_glm_pred <- predict(p_glm, ptest, type = 'prob')

  # choosing models with built in feature selection
  # p1 glmnet

  p_glmnet <- train(wins ~ ., df,
                     method = 'glmnet',
                     trControl = fitControl,
                     family = 'binomial',
                     metric = 'ROC',
                     tuneGrid = enetGrid
  )
  p_glmnet_pred <- predict(p_glmnet, ptest, type='prob')

  p_gbm <- train(wins ~ ., df,
                  method = 'gbm',
                  trControl = fitControl,
                  distribution = 'bernoulli',
                  metric = 'ROC',
                  tuneGrid = gbmGrid
  )

  p_gbm_pred <- predict(p_gbm, ptest, type='prob')

  ## rf
  rfGrid <- data.frame(mtry = seq(3, floor(sqrt(length(ptrain) - 1))))
  p_rf <- train(wins ~ ., df,
                method = 'rf',
                trControl = fitControl,
                metric = 'ROC',
                tuneGrid = rfGrid)

  p_rf_pred <- predict(p_rf, ptest, type='prob')
  resamps <- resamples(list(p_glm = p_glm,
                               p_glmnet = p_glmnet,
                               p_gbm = p_gbm,
                            p_rf = p_rf))
  return list(
    'glm' = list(model = p_glm, preds = p_glm_pred),
    'glmnet', = list(model = p_glmnet, preds = p_glmnet_pred),
    'gbm' = list(model = p_gbm, preds = p_gbm_pred),
    'rf' = list(model = p_rf, preds = p_rf_pred),
    'resamples'= resamps
    )
}

## loop through periods 1:3, not done here


trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))

## maybe for later
#
rocGlm <- roc(nba_wl_games[-modelOutput$index, 'WIN'],
              predict(modelOutput$period_1$glm$model,
                      pd[-modelOutput$index, ], type='prob')[,2],
              levels = c('1', '0'),
              percent=TRUE,
              col='blue')
rocGlmnet <- roc(nba_wl_games[-modelOutput$index, 'WIN'],
              predict(modelOutput$period_1$glmnet$model,
                      pd[-modelOutput$index, ], type='prob')[,2],
              levels = c('1', '0'),
              percent=TRUE,
              col = 'orange')
rocGbm <- roc(nba_wl_games[-modelOutput$index, 'WIN'],
              predict(modelOutput$period_1$gbm$model,
                      pd[-modelOutput$index, ], type='prob')[,2],
              levels = c('1', '0'),
              percent=TRUE)
rocRf <- roc(nba_wl_games[-modelOutput$index, 'WIN'],
              predict(modelOutput$period_1$rf$model,
                      pd[-modelOutput$index, ], type='prob')[,2],
              levels = c('1', '0'),
              percent=TRUE)

rocobj1 <- plot(rocObj, main="Statistical comparison", col="#1c61b6")
plot(rocGlmnet, add=TRUE, col='green')
plot(rocGbm, add=TRUE, col='orange')
plot(rocRf, add=TRUE, col='darkgreen')
rocobj2 <- lines.roc(aSAH$outcome, aSAH$ndka,
                     percent=TRUE, col="#008600")
testobj <- roc.test(rocobj1, rocobj2)
text(50, 50,
     labels=paste("p-value =", format.pval(testobj$p.value)),
     adj=c(0, .5))
legend("bottomright", legend=c("S100B", "NDKA"),
       col=c("#1c61b6", "#008600"), lwd=2)
