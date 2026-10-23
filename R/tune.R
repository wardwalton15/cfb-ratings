# Coordinate-descent tuning on the training seasons, then a report on the
# held-out test seasons. Run: Rscript R/tune.R

source("R/backtest.R")

ss <- load_all()
p <- default_params
ev <- function(p) score_backtest(backtest(p, ss = ss))$train$mae
grid <- list(
  pts_lambda = c(2, 3, 4, 6),
  lambda = c(150, 300, 600),
  decay = c(0.9, 0.94, 0.97, 1),
  fcs_carry = c(0.5, 0.75, 1)
)
for (nm in names(grid)) {
  res <- sapply(grid[[nm]], function(v) ev(modifyList(p, setNames(list(v), nm))))
  p[[nm]] <- grid[[nm]][which.min(res)]
  cat(nm, paste(sprintf("%s:%.3f", grid[[nm]], res), collapse = "  "), "->", p[[nm]], "\n")
}

bt <- backtest(p, ss = ss)
sc <- score_backtest(bt)
cat("\nTuned params:\n"); print(unlist(p))
cat("\nTrain:\n"); print(sc$train)
cat("\nTest (held out):\n"); print(sc$test)
print(sc$by_season); print(sc$by_phase)
cat("\nBlend weights on all seasons (use these in default_params):\n")
print(blend_weights(bt, train = c(TRAIN_SEASONS, TEST_SEASONS)))
