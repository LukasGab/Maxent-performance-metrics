---
# title: "R function computing SDMs performance metrics (including new metrics suggested by Leroy et al. 2018)"
# Paper title from Leroy et al.: Without quality presence–absence data, discrimination metrics such as TSS can be misleading measures of model performance
# author: "Lukas Gabor"
# date: "13/111/2019"
# metrics: AUC, TSS, Sensitivity, Specificity, Jaccard's similarity index, Sørensen's similarity index, 
         F‐measure, Overprediction rate, Underprediction rate
# input (model.evaluation): output of evaluate function (dismo package)
# output: dataframe
---


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
