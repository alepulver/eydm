library(plyr)
library(ggplot2)
library(gridExtra)
library(doParallel)
library(foreach)

# XXX: this does not work because of buggy imports
#nodes <- detectCores()
#cl <- makeCluster(nodes)
#registerDoParallel(cl)

df.var.fun = function(f, df) {
  function(var) {
    f(df[[var]])
  }
}

df.summary = function(df, name) {
  vars.numeric <- Filter(df.var.fun(is.numeric,df), names(df))
  vars.factor <- Filter(df.var.fun(is.factor,df), names(df))
  vars.string <- Filter(df.var.fun(is.character,df), names(df))
  
  data.frame(
       "Conjunto de datos" = name,
       "Observaciones" = dim(df)[1],
       "Variables" = dim(df)[2],
       "Numéricas" = length(vars.numeric),
       "Categóricas" = length(vars.factor),
       "Caracteres" = length(vars.string)
  )
}

datasets.summary = function(datasets) {
  ldply(names(datasets), function(name) {
    df.summary(datasets[[name]], name)
  })
}

my.multiplot <- function(df, plotFunc, varSelectFunc = NULL) {
  variables <- names(df)
  if (!is.null(varSelectFunc)) {
    variables <- Filter(function(x) varSelectFunc(df[[x]]), variables)
  }

  result <- foreach(var=variables) %dopar% {
    plotFunc(df,var)
  }
  l <- length(result)
  return (do.call(grid.arrange, c(result, ncol=ceiling(sqrt(l)))))  
}

my.boxplot = function(df, var) {
  ggplot(df, aes_string(x="factor(0)", y=var)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(size=1) +
    xlab(var) + ylab("")
}

my.histogram = function(df, var) {
  ggplot(df, aes_string(x=var)) +
    geom_histogram(aes(y = ..density..), color="black", fill=NA) +
    geom_density(color = "blue")
}

my.barplot = function(df, var) {
  ggplot(df, aes_string(x=var)) + geom_bar() +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_text(hjust = 0))
}

my.missing_values = function(df) {
  total = dim(df)[1]
  ldply(names(df), function(x) {
    missing = sum(is.na(df[[x]]))
    if (missing > 0) {
      data.frame(Variable = x, Faltantes = missing, Porcentaje = 100*missing/total)
    } else {
      NULL
    }
  })
}

my.sample_clusters = function(df, groups, n) {
  ldply(levels(groups), function(x) {
    subdf = df[groups == x,]
    total = dim(subdf)[1]
    indices = sample(1:total, min(n, total))
    
    result = subdf[indices,]
    result[["Group"]] = x
    result
  })
}

my.stratified_sample = function(groups, prop) {
  indices = c()
  for (x in levels(groups)) {
    total = table(groups)[x]
    if (total > 1) {
      current = sample(which(groups == x), max(1, round(prop*total)))
      indices = c(indices, current)
    }
  }
  indices
}