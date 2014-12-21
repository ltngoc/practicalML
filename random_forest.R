myPreProcess <- function(train) {
  train <- train[ ,-(1:7)]
  nzv.info <- nearZeroVar(train, saveMetrics = T)
  nz.vars <- which(nzv.info$nzv == T)
  train <- train[ , -nz.vars]
  
  propNA <- function(col, ds) {
    prop.na <- sum(is.na(ds[ ,col]))/nrow(ds)
    data.frame("predictor" = names(ds)[col], "prop.na" = prop.na)
  }
  na.summary <- ldply(1: ncol(train), propNA, ds = train)
  too.many.nas <- which(na.summary$prop.na > 0.90)
  train <- train[ ,-too.many.nas]
  ## return processed train 
  train
}

getBadFeats <- function(ds) {
  bad.feats <- 1:7
  nzv.info <- nearZeroVar(ds, saveMetrics = T)
  nz.vars <- which(nzv.info$nzv == T)
  bad.feats <- c(bad.feats, nz.vars)
  
  propNA <- function(col, ds) {
    prop.na <- sum(is.na(ds[ ,col]))/nrow(ds)
    data.frame("predictor" = names(ds)[col], "prop.na" = prop.na)
  }
  na.summary <- ldply(1: ncol(ds), propNA, ds = ds)
  too.many.nas <- which(na.summary$prop.na > 0.90)
  ds <- ds[ ,-too.many.nas]
  ## return processed ds 
  ds
}

runRF <- function(i) {
  valid <- ds[partition[[i]], ]
  train <- ds[-partition[[i]], ] 
  pair <- myPreProcess(train = train, valid = valid)
  train <- pair$train
  valid <- pair$valid
  cat("Start running random forest \n")
  rf.fit <- randomForest(y = train$classe, x = train[ ,-ncol(train)], ntree = 100)
  rf.pred <- predict(rf.fit, valid[ ,-ncol(valid)])
  conf.mat <- confusionMatrix(rf.pred, reference = valid$classe)
  data.frame("fold" = i, "accuracy" = conf.mat$overall["Accuracy"])
}
runRF(2)
res <- ldply(1:10, runRF, .progress = "time")
res

test.set <- read.csv("pml-testing.csv", header = T)

bad.feats <- c(1:7, nz.vars, too.many.nas)
test.set <- test.set[ , -(1:7)]
test.set <- test.set[ , -nz.vars]
test.set <- test.set[ , -too.many.nas]


answers <- predict(rf.fit, test.set[ ,-53])
save(rf.fit, file = "rfModel.rda")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)



