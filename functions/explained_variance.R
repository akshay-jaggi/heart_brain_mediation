project = function(data.matrix, latent.matrix) {
  latent.unit = latent.matrix / norm(latent.matrix, 'f')
  proj.scores = t(data.matrix) %*% latent.unit
  data.projected = data.frame(outer(latent.unit,t(proj.scores)))
  return(data.projected)
}

evr_ = function(data.matrix, latent.matrix) {
  data.projected = project(data.matrix, latent.matrix)
  data.orth = data.matrix - data.projected
  evr = 1 - sum(apply(data.orth,2,var)) / sum(apply(data.matrix,2,var))
  return(evr)
}

evr = function(dataset, latent) {
  data.matrix = as.matrix(dataset)
  latent.matrix = as.matrix(latent)
  return(evr_(data.matrix, latent.matrix))
}

evr_orth = function(dataset, latents) {
  data.matrix = as.matrix(dataset)
  evrs = apply(latents, 2, function(x) evr(data.matrix, as.matrix(x)))
  return(list(evrs, sum(evrs)))
}

evr_nonorth = function(dataset, latents) {
  data.matrix = as.matrix(dataset)
  data.orth = matrix(data.matrix)
  for(latent in latents) {
    data.projected = project(data.matrix, as.matrix(latent))
    data.orth = data.orth - data.projected
  }
  evr = 1 - sum(apply(data.orth,2,var)) / sum(apply(data.matrix,2,var))
  return(evr)
}

pca.cross.val = function(dataset, k=10) {
  flds <- caret::createFolds(seq(nrow(dataset)), k = 10, list = TRUE, returnTrain = TRUE)
  evrs = matrix(0,10,10)
  i = 1
  for(fld in flds) {
    train = dataset[fld,]
    test = dataset[-fld,]
    train.mean = apply(train, 2, mean)
    train.sd = apply(train, 2, sd)
    train.zscore = sweep(sweep(train, 2, FUN='-',train.mean),2,FUN='/',train.sd)
    test.zscore = sweep(sweep(test, 2, FUN='-',train.mean),2,FUN='/',train.sd)
    train.pcs = prcomp(train.zscore, center = FALSE, scale = FALSE)
    evr_out = evr_orth(test.zscore, 
                       data.frame(as.matrix(test.zscore) %*% as.matrix(train.pcs$rotation[,1:10])))
    evrs[i,] = evr_out[[1]]
    i = i + 1 
  }
  return(evrs)
}