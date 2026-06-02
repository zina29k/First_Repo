#Load MNIST data csv

library(data.table)
MNIST_dt <- fread("data_Classif/MNIST.csv")
data.table(
  name=names(MNIST_dt),
  first_row=unlist(MNIST_dt[1]),
  last_row=unlist(MNIST_dt[.N]))

#Class distribution (count and proportion for each class)

(multi_counts <- MNIST_dt[, .(
  count=.N,
  prop=.N/nrow(MNIST_dt)
), by=y][order(-prop)]) #Balanced from 9% to 11%


#Ordering experiment (Random vs Proportional) samples
# For (1% , 10%, 100%) of the MNIST data
# (Random is less stable especially in 1% : class prop from 6% to 12%)

my.seed <- 1
set.seed(my.seed)
rand_ord <- MNIST_dt[, sample(.N)]
prop_ord <- data.table(y=MNIST_dt$y[rand_ord])[
  , prop_y := seq(0,1,l=.N), by=y
][, order(prop_y)]
ord_list <- list(
  random=rand_ord,
  proportional=rand_ord[prop_ord])
ord_prop_dt_list <- list()
for(ord_name in names(ord_list)){
  ord_vec <- ord_list[[ord_name]]
  y_ord <- MNIST_dt$y[ord_vec]
  for(prop_data in c(0.01, 0.1, 1)){
    N <- nrow(MNIST_dt)*prop_data
    N_props <- data.table(y=y_ord[1:N])[, .(
      count=.N,
      prop_y=.N/N
    ), by=y][order(-prop_y)]
    ord_prop_dt_list[[paste(ord_name, prop_data)]] <- data.table(
      ord_name, prop_data, N_props)
  }
}
ord_prop_dt <- rbindlist(ord_prop_dt_list)
dcast(ord_prop_dt, ord_name + prop_data ~ y, value.var="prop_y")

#To Binary Classification (by parity) -> odd 

(binary_counts <- MNIST_dt[
  , odd := y %% 2
][, .(
  count=.N,
  prop=.N/nrow(MNIST_dt)
), by=odd][order(-prop)])

#Create Unbalanced proportion

larger_N <- binary_counts$count[1]/2
target_prop <- c(0.5, 0.1, 0.05, 0.01, 0.005, 0.001)
(smaller_dt <- data.table(
  target_prop,
  count=as.integer(target_prop*larger_N/(1-target_prop))
)[
  , prop := count/(count+larger_N)
][])

#Small Unbalanced subsets from 0.1 to 0.001 prop
#One row for each Imbalanced variant based on the original MNIST data

(unb_small_dt <- data.table(
subset="unbalanced",
binary_counts[2,.(odd)],
smaller_dt[-1]))

#unbalanced/balanced dataset

subset_mat <- matrix(
  NA, nrow(MNIST_dt), nrow(unb_small_dt),
  dimnames=list(
    NULL,
    target_prop=paste0(
      "seed",
      my.seed,
      "_prop",
      unb_small_dt$target_prop)))
emp_y_list <- list()
emp_props_list <- list()
MNIST_ord <- MNIST_dt[, .(odd, .I)][ord_list$proportional]
for(unb_i in 1:nrow(unb_small_dt)){
  unb_row <- unb_small_dt[unb_i]
  unb_count_dt <- rbind(
    data.table(subset="balanced", binary_counts[,.(odd)], smaller_dt[1]),
    data.table(subset="unbalanced", binary_counts[1,.(odd)], smaller_dt[1]),
    unb_row)
  MNIST_ord[, subset := NA_character_]
  for(o in c(1,0)){
    o_dt <- unb_count_dt[odd==o]
    sub_vals <- o_dt[, rep(subset, count)]
    o_idx <- which(MNIST_ord$odd==o)
    some_idx <- o_idx[1:length(sub_vals)]
    MNIST_ord[some_idx, subset := sub_vals]
  }
  subset_mat[MNIST_ord$I, unb_i] <- MNIST_ord$subset
  ## Check to make unbalanced is a subset of the previous larger one.
  if(unb_i>1)stopifnot(all(which(
    subset_mat[,unb_i]=="unbalanced"
  ) %in% which(
    subset_mat[,unb_i-1]=="unbalanced"
  )))
  ## Check to make sure balanced is the same as previous.
  if(unb_i>1)stopifnot(identical(
    which(subset_mat[,unb_i]=="balanced"),
    which(subset_mat[,unb_i-1]=="balanced")
  ))
  (unb_MNIST <- data.table(
    target_prop=unb_row$target_prop,
    subset=subset_mat[,unb_i],
    odd=MNIST_dt$odd,
    y=MNIST_dt$y)[, idx := .I][!is.na(subset)])
  emp_y_list[[unb_i]] <- unb_MNIST[, .(
    count=.N
  ), by=.(target_prop,subset,y)]
  emp_props_list[[unb_i]] <- unb_MNIST[, .(
    count=.N,
    first=idx[1], 
    last=idx[.N]
  ), by=.(target_prop,subset,odd)
  ][
    , prop_in_subset := count/sum(count)
    , by=subset
  ][]
}
emp_y <- rbindlist(emp_y_list)
(emp_props <- rbindlist(emp_props_list))

#Verify multi-class proportion

dcast(emp_y, subset + target_prop ~ y, value.var="count")

#Write in csv file
fwrite(subset_mat, "mnist-unbalanced.csv")
