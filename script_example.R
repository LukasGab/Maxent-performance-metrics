# Author: Lukas Gabor, 2019

# Library packages
library (raster)
library(dismo)

#------ Load Data ------
# Species data
sp <- shapefile("virtualis")

# Environmental data
twi <- raster("twi.tif")
dem <- raster("dem.tif")
preds <- stack(twi, dem)

plot(preds)

bg <- randomPoints(preds, 10000, p = sp) # create background points

# Divide species data for testing and training the model
fold <- kfold(sp, k=5) # divide species occurrences to 5 folds (20 % for testing the model)

test <- sp[fold == 1, ]
train <- sp[fold != 1, ]

maxent.model <- maxent (preds, train, quadratic=T, hinge = T, linear = F, product = F, threshold = F,
                          args=c("maximumbackground=10000", "betamultiplier=10", "defaultprevalence=0.5"))
  
# Model Evaluation
model.evaluation <- evaluate(maxent.model, p=test, a=bg, preds)

# Load metrics function
performance.metrics <- function (model.evaluation) {
  AUC <- model.evaluation@auc
  TSS <- mean(model.evaluation@TPR) + mean(model.evaluation@TNR) - 1
  Sensitivity <- mean(model.evaluation@TPR)
  Specificity <- mean(model.evaluation@TNR)
  C.kappa <- mean(model.evaluation@kappa)
  Sorensen <- 2*mean(model.evaluation@TPR)/((mean(model.evaluation@FNR) + 2*mean(model.evaluation@TPR) + mean(model.evaluation@FPR)))
  Jaccard <- mean(model.evaluation@TPR)/((mean(model.evaluation@FNR) + mean(model.evaluation@TPR) + mean(model.evaluation@FPR)))
  F.measure <- 2 * Jaccard
  Over.pred <- mean(model.evaluation@FPR)/((mean(model.evaluation@TPR)+mean(model.evaluation@FPR)))
  Under.pred <- mean(model.evaluation@FNR)/((mean(model.evaluation@TPR)+mean(model.evaluation@FNR)))
  data.frame(AUC = AUC, TSS = TSS, Sensitivity = Sensitivity, Specificity = Specificity, C.kappa = C.kappa,
             Sorensen = Sorensen, Jaccard = Jaccard, F.measure = F.measure, Over.pred = Over.pred, Under.pred = Under.pred)
}

results <- performance.metrics(model.evaluation)
results

# Model prediction
model.prediction <- predict(maxent.model, preds)
plot(model.prediction, main='Maxent - Predicted Suitability')
