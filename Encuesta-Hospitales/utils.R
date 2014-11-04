library(ggplot2)
library(gridExtra)
library(lattice)
library(reshape2)

my.multiplot <- function(df, plotFunc, rejectFunc = NULL, varNames = NULL) {
  result <- list()
  for (var in names(df)) {
    if (!is.null(varNames) && !(var %in% varNames)) {
      next
    }
    if (!is.null(rejectFunc) && rejectFunc(df,var)) {
      next
    }
    
    p <- plotFunc(df,var)
    if (!is.null(p)) {
      result <- append(result, list(p))
    }
  }
  l <- length(result)
  return (do.call(grid.arrange, c(result, ncol=min(3,ceiling(sqrt(l))))))
}

my.boxplots <- function(df) {
  my.multiplot(df, function(df,var) {
    ggplot(df, aes_string(x="factor(0)", y=var)) +
      geom_boxplot(outlier.colour = "red") +
      geom_jitter() +
      xlab("")
  })
}

my.density <- function(df) {
  my.multiplot(df, function(df,var) {
    ggplot(df, aes_string(x=var)) +
      geom_histogram(aes(y = ..density..), color="black", fill=NA) +
      geom_density(color = "blue")
  })
}

my.quantiles <- function(df) {
  my.multiplot(df, function(df,var) {
    my.df <- data.frame(first = sort(df[[var]]), observation = 1:(dim(df)[1]))
    names(my.df)[1] <- var
    
    p <- ggplot(my.df, aes_string(y=var, x="observation")) +
      geom_point() +
      geom_line()
  }, rejectFunc = function(df,var) { is.factor(df[[var]]) })
}

my.outliers <- function(df) {
  my.multiplot(df, function(df,var) {
    dotplot(sort(df[[var]]), xlab = var)
  }, rejectFunc = function(df,var) { is.factor(df[[var]]) })
}

my.explvar <- function(x) { x^2/sum(x^2) }

my.loadings2mat <- function(x) {
  mat <- matrix(x, nrow=attr(x, "dim")[1])
  colnames(mat) <- attr(x, "dimnames")[[2]]
  rownames(mat) <- attr(x, "dimnames")[[1]]
  return (mat)
}

my.normvec <- function(x) { sqrt(sum(x^2)) }

my.contingency_matrix <- function(df) {
  tmp <- dcast(df, as.formula(sprintf("%s ~ %s", names(df)[1], names(df)[2])))
  rownames(tmp) <- tmp[,1]
  tmp <- tmp[,-1]
  as.matrix(tmp)
}

my.contingency_heatmap <- function(df) {
  aesthetics <- aes_string(y = names(df)[1], x = names(df)[2], fill = 'value')
  ggplot(df, aesthetics) + geom_tile()
}

my.contingency_analysis <- function(df) {
  total.row <- daply(df, 1, function(df) sum(df$value))
  total.col <- daply(df, 2, function(df) sum(df$value))
  total.sum <- sum(df$value)
  expected <- (total.row %o% total.col) / total.sum
  df.mat <- my.contingency_matrix(df)
  chisqr <- (df.mat - expected)^2 / expected
  categories <- c(length(total.row), length(total.col))
  
  list(totalRows = total.row, totalCols = total.col, total = total.sum,
       matrix = df.mat, expected = expected, chisqrMatrix = chisqr,
       chisqr = sum(chisqr), categories = categories,
       chisqrDf = (categories[1]-1) * (categories[2]-1))
}

my.factor_table <- function(column) {
  result <- as.data.frame(table(column))
  names(result) <- c("Caso", "Cantidad")
  result[["Porcentaje"]] <- sprintf("%.1f%%", 100 * result$Cantidad / length(column))
  result
}