# USEFUL FUNCTIONS


# tables for RE models
# ~~~~~~~~~~~~~~~~~~~~

make_RE_restable <- function(model, model_name) {
  
  aux <- if(str_detect(model_name, ".+\\([1,2].+")){
    aux <-  exp(model$modelStruct$corStruct[1:2]) 
  } else {
    aux <- NA
  }

  phis <- (aux-1)/(aux+1)
  
  coeffs <- as.data.frame(coef(summary(model)) |> 
            round(4)) |> 
            rownames_to_column("Coeff")
  
  phi1   <- as_tibble_row(list("Coeff" = "phi_1", 
                               "Value" = phis[1], 
                               "Std.Error" = NA, 
                               "DF" = NA, 
                               "t-value" = NA, 
                               "p-value" = NA))
  
  phi2   <- as_tibble_row(list("Coeff" = "phi_2", 
                               "Value" = phis[2], 
                               "Std.Error" = NA, 
                               "DF" = NA, 
                               "t-value" = NA, 
                               "p-value" = NA))  
  
  loglik <- as_tibble_row(list("Coeff" = "LogLikelihood", 
                               "Value" = model$logLik, 
                               "Std.Error" = NA, 
                               "DF" = NA, 
                               "t-value" = NA, 
                               "p-value" = NA))
  
  aic <- as_tibble_row(list("Coeff" = "AIC", 
                            "Value" = AIC(model), 
                            "Std.Error" = NA, 
                            "DF" = NA, 
                            "t-value" = NA, 
                            "p-value" = NA))
  
  N   <- as_tibble_row(list("Coeff" = "N", 
                            "Value" = model$dims$N, 
                            "Std.Error" = NA, 
                            "DF" = NA, 
                            "t-value" = NA, 
                            "p-value" = NA))
  
  t   <- as_tibble_row(list("Coeff" = "t", 
                            "Value" = model$dims$N / model$dims$ngrps[[1]], 
                            "Std.Error" = NA, 
                            "DF" = NA, 
                            "t-value" = NA, 
                            "p-value" = NA))    
  
  n   <- as_tibble_row(list("Coeff" = "n", 
                            "Value" = model$dims$ngrps[[1]], 
                            "Std.Error" = NA, 
                            "DF" = NA, 
                            "t-value" = NA, 
                            "p-value" = NA))    
  results <- coeffs |> 
    bind_rows(phi1, phi2, loglik,aic, n, t, N) |> 
    dplyr::select(Coeff, Value, Std.Error) |> 
    pivot_longer(c(Value, Std.Error), names_to = "Type", values_to = "Values") |>
    mutate(Value = case_when(Type == "Std.Error" ~ paste0("(", round(Values,4), ")"),
                             TRUE ~ as.character(round(Values, 4)))) |> 
    filter(Value != "(NA)") |>
    dplyr::select(Coeff, Type, Value) %>% 
    `colnames<-`(c("Coeff", "type", model_name))
  results
}


# tables for FE models
# ~~~~~~~~~~~~~~~~~~~~

make_FE_restable <- function(model, model_name) {
  
  its <- model$model |> rownames_to_column("it") |> dplyr::select(it)
  
  coeffs <- as.data.frame(coeftest(model, 
                                   vocv. = vcovHC(model, method = "arellano"))[] |> 
            round(4)) |> 
            rownames_to_column("Coeff") |>
            `colnames<-`(c("Coeff","Estimate","Std.Error","t-value","p-value"))
  
  phi_1 <- as_tibble_row(list("Coeff" = "phi BG test (pval)",
                              "Estimate" = ifelse(
                                str_detect(model_name, ".+ARIMA.1....."), 
                                round(pbgtest(model)$p.value, 4), 
                                NA
                              ),
                              "Std.Error" = NA, 
                              "DF" = NA, "t-value" = NA, "p-value" = NA))
  
  r2_adj <- as_tibble_row(list("Coeff" = "r2_adj", 
                               "Estimate" = r.squared(model, dfcor = TRUE), 
                               "Std.Error" = NA, 
                               "DF" = NA, 
                               "t-value" = NA, 
                               "p-value" = NA))
  
  n   <- as_tibble_row(list("Coeff" = "n", 
                            "Estimate" = str_sub(its$it,1,-5) |> unique() |> length(), 
                            "Std.Error" = NA, 
                            "DF" = NA, 
                            "t-value" = NA, 
                            "p-value" = NA))
  
  t   <- as_tibble_row(list("Coeff" = "t", 
                            "Estimate" = str_sub(its$it,-4,-1) |> unique() |> length(), 
                            "Std.Error" = NA, 
                            "DF" = NA, 
                            "t-value" = NA, 
                            "p-value" = NA))  
  
  N   <- as_tibble_row(list("Coeff" = "N", 
                            "Estimate" = nrow(its), 
                            "Std.Error" = NA, 
                            "DF" = NA, 
                            "t-value" = NA, 
                            "p-value" = NA))
  
  results <- coeffs |> 
    bind_rows(phi_1,r2_adj, n, t, N) |> 
    dplyr::select(Coeff, Estimate, Std.Error) |> 
    pivot_longer(c(Estimate, Std.Error), 
                 names_to = "Type", 
                 values_to = "Values") |>
    mutate(Value = case_when(Type == "Std.Error" ~ paste0("(", round(Values,4), ")"),
                             TRUE ~ as.character(round(Values, 4)))) |> 
    filter(Value != "(NA)") |>
    dplyr::select(Coeff, Type, Value) %>% 
    `colnames<-`(c("Coeff", "type", model_name))
  results
}

ts_cv <- function(
  ts, # ts object
  order, xcovars, include_mean, # ARIMA model setup
  # hyper-parameters
  min_window = 5, # minimum length of training set K; default is 5
  forward = 1, # number of forward periods P; default is 1
  sliding_trainset = TRUE # whether the sampling scheme of training set is sliding window; default is TRUE; FALSE means expanding window scheme
) {
  # input: `ts` object
  if (!any(class(ts) == "ts")) ts <- ts(death) # ts object
  ts_start <- tsp(ts)[1] # the starting point of ts t
  ts_end <- tsp(ts)[2] # the end point of ts
  ts_freq <- tsp(ts)[3] # the frequency of ts
  ts_length <- length(ts) # The total length of time series T
  # input: model setup
  #order <- c(1, 0, 0) # order of ARMA model
  #xcovars <- law # covariates for prediction
  #include_mean <- TRUE # whether include intercept
  # CV procedure
  n_iters <- ts_length - min_window
  mae <- matrix(NA, nrow = n_iters, ncol = forward) # pre-allocate a matrix to store performance metrics
  for (i in 1:n_iters) {
    # compute the starting and end points of training and test sets
    if (sliding_trainset == TRUE) {
      train_start <- ts_start + (i - 1) / ts_freq
    } else {
      train_start <- ts_start
    }
    train_end <- ts_start + (i - 1 + min_window - 1) / ts_freq
    test_start <- ts_start + (min_window + i - 1) / ts_freq
    test_end <- min(ts_start + (min_window + i - 1 + forward - 1) / ts_freq, ts_end)
    # construct training and test sets
    train_set <- window(ts, start = train_start, end = train_end)
    test_set <- window(ts, start = test_start, end = test_end)
    # also truncate covariates time series to the same length
    x_train <- window(xcovars, start = train_start, end = train_end)
    x_test <- window(xcovars, start = test_start, end = test_end)
    
    # run model on training set
    train_fit <- forecast::Arima(train_set, order = order, xreg = x_train, include.mean = include_mean)
    
    
    # compute performance metrics on test set
    pred <- forecast::forecast(train_fit, h = forward, xreg = x_test)
    mae[i, 1:length(test_set)] <- abs(pred$mean - test_set)
  }
  # average performance metrics across iterations
  mae <- colMeans(mae, na.rm = TRUE)
  return(mae)
}












