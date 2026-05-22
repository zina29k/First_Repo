pkgs <- c(
  "quarto", "tidyverse", "ggplot2", "knitr", "rmarkdown",
  "data.table", "directlabels", "atime"
)

ins.mat <- installed.packages()
missing.pkgs <- setdiff(pkgs, rownames(ins.mat))

if (length(missing.pkgs) > 0) {
  install.packages(missing.pkgs, repos = "https://cloud.r-project.org")
}

quarto::quarto_render()