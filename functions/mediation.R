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

multiple.mediation.model = function(independent, dependent, mediators, covariates) {
  covariates = paste(covariates,collapse=" + ")
  mediator.models = c()
  main.model = paste(dependent, " ~ c * ", independent, " + ", covariates,sep="")
  mediator.model = paste(" * ", independent, " + ", covariates,sep="")
  total.effect = 'te := c'
  indirect.effects = c()
  percent.meds = c()
  covariances = c()
  counter = 1
  for(mediator in mediators) {
    coefa = paste('a',counter,sep="")
    coefb = paste('b',counter,sep="")
    specific.mediator.model = paste(mediator, " ~ ",coefa,mediator.model, sep="")
    mediator.models = c(mediator.models, specific.mediator.model)
    main.model = paste(main.model, paste(' + ', coefb,' * ', mediator, sep=""), sep="")
    product.term = paste(coefa,' * ',coefb,sep="")
    effect = paste('ie',counter, ' := ', product.term, sep="")
    percent.med = paste('pm',counter, ' := (', product.term,') / ',sep="")
    indirect.effects = c(indirect.effects, effect)
    total.effect = paste(total.effect, ' + ',product.term,sep="")
    percent.meds = c(percent.meds, percent.med)
    if (counter < length(mediators)) {
      for (med in mediators[(counter+1):length(mediators)]) {
        covariances = c(covariances,paste(mediator, '~~',med))
      }
    }
    counter = counter + 1
  }
  percent.meds = paste(percent.meds, "(", stringr::str_split(total.effect,"te := ")[[1]][2], ")", sep = "")
  full.model.statement = paste(c(main.model,
                                 mediator.models,
                                 covariances,
                                 indirect.effects,
                                 percent.meds,
                                 total.effect),collapse="\n")
  return(full.model.statement)
}

run.multiple.mediation = function(dataframe, independent, dependent, mediators, covariates) {
  full.model.statement = multiple.mediation.model(independent, dependent, mediators, covariates)
  multiple.mediation.sem = sem(full.model.statement,dataframe)
  mm.out = standardizedsolution(multiple.mediation.sem)
  return(mm.out)
}

mediation.ind.dep.pairs = function(dataframe, independents, dependents, mediators, covariates) {
  mediation.individual.single.pairs = list()
  iter = 1
  for (independent in independents) {
    mediation.individual.single.pairs[[independent]] = list()
    covariate.string = paste(covariates,collapse=" + ")
    for (dependent in dependents) {
      lin.model = paste(c(dependent," ~ ", independent, " + ", covariate.string, "\n"), collapse = "")
      lin.model.sem = sem(lin.model,dataframe)
      out = standardizedsolution(lin.model.sem)
      if(out$pvalue[1] < 0.05) {
        mediation.individual.single.pair = run.single.mediation(dataframe,
                                                                independent,
                                                                dependent,
                                                                mediators, covariates)
        mediation.individual.single.pair.final = process.mediation.results(mediation.individual.single.pair)
        mediation.individual.single.pair.final$independent = rep(independent, nrow(mediation.individual.single.pair.final))
        mediation.individual.single.pair.final$dependent = rep(dependent, nrow(mediation.individual.single.pair.final))
        mediation.individual.single.pairs[[iter]] = mediation.individual.single.pair.final
        iter = iter + 1
      }
    }
  }
  mediation.pairs.final = dplyr::bind_rows(mediation.individual.single.pairs)
  return(mediation.pairs.final)
}