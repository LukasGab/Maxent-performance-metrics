library (raster)
library(dismo)

#------ Loading Data ------
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

model.prediction <- list() # create empty list for model prediction
data.performance <- numeric(0) # create empty dataframe for evaluation the model

  test <- sp[fold == 1, ]
  train <- sp[fold != 1, ]
  
  # before modeling be sure that you have R and Java in same version (32 or 64 bit) otherwise you will get an error
  # I recommend to uninstall both and install both Java and R only in 64 bit version
  # After first run only, you will get an error indicates that you missing file maxent.jar or dismo.jar, just copy it to the file that
  #the error message show you. You can find both files in the file I gave you
  maxent.model <- maxent (preds, train, quadratic=T, hinge = T, linear = F, product = F, threshold = F,
                          args=c("maximumbackground=10000", "betamultiplier=10", "defaultprevalence=0.5")) # modelovani
  # all possible arguments (you can put them to args=c()) -> https://groups.google.com/forum/#!topic/maxent/yRBlvZ1_9rQ
  
  # maxent.model # check the results via website
  # plot(maxent.model) # plot variable controbution graph
  # response(maxent.model) # plot species responses to environmental variables
  
  # Evaluation the model
  model.evaluation <- evaluate(maxent.model, p=test, a=bg, preds) # evaluation the model
  
  # Usual discrimination metrics
  auc <- as.data.frame(model.evaluation@auc)
  tss <- data.frame(mean(model.evaluation@TPR) + mean(model.evaluation@TNR) - 1)
  sensitivity <- data.frame(mean(model.evaluation@TPR))
  specificity <- data.frame(mean(model.evaluation@TNR))
  

  # Prediction species distribution
  model.prediction [[i]] <- predict(maxent.model, preds)