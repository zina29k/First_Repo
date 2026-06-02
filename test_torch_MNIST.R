library(data.table)
library(mlr3)

MNIST_dt <- fread("data_Classif/MNIST.csv")
subset_dt <- fread("mnist-unbalanced.csv")

task_dt <- data.table(subset_dt, MNIST_dt)

task_dt[, odd := as.factor(y %% 2)]

tache <- TaskClassif$new(
  id = " ",
  backend = task_dt,
  target = "odd"
)
