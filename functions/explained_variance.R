project = function(data.matrix, latent.matrix) {
  latent.unit = latent.matrix / norm(latent.matrix, 'f')
  proj.scores = t(data.matrix) %*% latent.unit
  data.projected = data.frame(outer(latent.unit,t(proj.scores)))
  return(data.projected)
}

ev_ = function(data.matrix, latent.matrix) {
  data.projected = project(data.matrix, latent.matrix)
  data.orth = data.matrix - data.projected
  ev = sum(apply(data.matrix,2,var)) - sum(apply(data.orth,2,var))
  return(ev)
}

ev = function(dataset, latent) {
  data.matrix = as.matrix(dataset)
  latent.matrix = as.matrix(latent)
  return(ev_(data.matrix, latent.matrix))
}

ev_orth = function(dataset, latents) {
  data.matrix = as.matrix(dataset)
  evs = apply(latents, 2, function(x) ev(data.matrix, as.matrix(x)))
  return(list(evs, sum(evs)))
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