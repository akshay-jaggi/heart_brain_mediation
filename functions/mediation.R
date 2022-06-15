library(lavaan)
library(dplyr)

run.single.mediation = function(dataframe, independent, dependent, mediators, covariates) {
  results.list = list()
  effects = 
  "de := c
  ie := a*b
  te := c + a*b
  pm := (a*b) / (c+a*b)"
  covariates = paste(covariates,collapse=" + ")
  for(mediator in mediators) {
    med.model = paste(c(mediator," ~ a * ", independent, " + ", covariates, "\n",
                        dependent, " ~ c * ", independent, " + b * ",mediator," + ", covariates),
                      collapse = "")
    med.model = paste(c(med.model, effects), collapse = '\n')
    med.sem = sem(med.model,dataframe)
    out = standardizedsolution(med.sem)
    results.list[[mediator]] = out[out$op=="~"|out$op==":=",]
  }
  results.frame = bind_rows(results.list, .id = "mediator")
  return(results.frame)
}

process.mediation.results = function(results.frame) {
  results.frame.final = dplyr::select(results.frame, -c('se','z'))
  results.frame.final = results.frame.final[results.frame.final$label != '',]
  results.frame.final$pvalue_adj = results.frame.final$pvalue 
  for(test in unique(results.frame.final$label)) {
    results.frame.final$pvalue_adj[results.frame.final$label == test] =
      p.adjust(results.frame.final$pvalue[results.frame.final$label == test],'BH')
  }
  results.frame.final$significant = 
    (results.frame.final$pvalue_adj < 0.05) & (!is.na(results.frame.final$pvalue_adj))
  return(results.frame.final)
}