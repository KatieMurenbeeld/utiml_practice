library(utiml)

new.toyml <- remove_attributes(toyml, c("iatt8", "iatt9", "ratt10"))
pre.process <- function (mdata) {
  aux <- remove_skewness_labels(mdata, 5) # Remove infrequent labels (less than 5)
  aux <- remove_unlabeled_instances(aux) # Remove instances without labels
  aux <- remove_unique_attributes(aux) # Remove constant attributes
  return(mdata)
}

set.seed(123)
ds <- create_holdout_partition(new.toyml, c(train=0.7, test=0.3))
model <- br(ds$train, "RF")
predictions <- predict(model, ds$test)

results <- multilabel_evaluate(ds$test, predictions, c("example-based", "macro-F1"))
round(results, 4)

base.preds <- predict(baseline(ds$train, "general"), ds$test)
base.res <- multilabel_evaluate(ds$test, base.preds, c("hamming-loss", "F1"))

round(base.res, 4)

head(predictions)

head(as.bipartition(predictions))

head(as.ranking(predictions))

head(mcut_threshold(predictions))
