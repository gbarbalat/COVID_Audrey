#add on your own algo
SL.hal9001<- function (Y, X, newX = NULL, family = "binomial",
                       obsWeights = rep(1, length(Y)), id = NULL, max_degree = ifelse(ncol(X) >= 20,2, 3),
                       smoothness_orders = 1, num_knots = ifelse(smoothness_orders >= 1, 25, 50),
                       reduce_basis = 1/sqrt(length(Y)), lambda = NULL,
                       ...)
{
  if (!is.matrix(X))
    X <- as.matrix(X)
  if (!is.null(newX) & !is.matrix(newX))
    newX <- as.matrix(newX)
  hal_fit <- fit_hal(X = X, Y = Y, family = family$family,
                     fit_control = list(weights = obsWeights), id = id, max_degree = max_degree,
                     smoothness_orders = smoothness_orders, num_knots = num_knots,
                     reduce_basis = reduce_basis, lambda = lambda)
  if (!is.null(newX)) {
    pred <- stats::predict(hal_fit, new_data = newX)
  }
  else {
    pred <- stats::predict(hal_fit, new_data = X)
  }
  fit <- list(object = hal_fit)
  #class(fit) <- "SL.hal9001"
  out <- list(pred = pred, fit = fit)
  return(out)
}

SL.step_IA_glmnet <- function (Y, X, newX, family, direction = "backward", trace = 0,
                        k = 2, ...)
{
   whichVarialbes <- screen.glmnet(Y,X, family=binomial())#returns logical vector
  #whichVarialbes <- screen.corP(Y,X, family=binomial(), method = "spearman",minPvalue = 0.3)
  #whichVarialbes <- screen.corRank(Y,X, family=binomial(), method = "spearman", rank=10)

  fit.glm <- glm(Y ~ ., data = X[,whichVarialbes], family = binomial())
  fit.stepMain <- step(fit.glm, direction = direction)

  fit.step <- step(fit.stepMain, scope = Y ~ .^2, direction = direction,
                   trace = trace, k = k)
  pred <- predict(fit.step, newdata = newX, type = "response")
  fit <- list(object = fit.step)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.step")
  return(out)
}

SL.step_IA_randomForest <- function (Y, X, newX, family, direction = "backward", trace = 0,
                               k = 2, ...)
{
  whichVarialbes <- screen.randomForest(Y,X, family=binomial(),nVar = 10)#returns logical vector

  fit.glm <- glm(Y ~ ., data = X[,whichVarialbes], family = binomial())
  fit.stepMain <- step(fit.glm, direction = direction)

  fit.step <- step(fit.stepMain, scope = Y ~ .^2, direction = direction,
                   trace = trace, k = k)
  pred <- predict(fit.step, newdata = newX, type = "response")
  fit <- list(object = fit.step)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.step")
  return(out)
}

SL.glmnet_ridge = function(...) {
  SL.glmnet(..., alpha=0)
}

SL.glmnet_0.5 = function(...) {
  SL.glmnet(..., alpha=0.5)
}

SL.rf_100 = function(...) {
  SL.randomForest(..., num.trees = 100)
}



(mtry_seq = floor(sqrt(75) * c(0.5, 1, 2)))
(rf_learners = create.Learner("SL.randomForest", tune = list(mtry = mtry_seq)))


#xgb learners
grid_params <- list(
  max_depth = c(1,2, 4, 6),
  eta = c(0.001, 0.01,0.1, 0.3),
  nrounds = c(100,500,1000)
)
xgb_learners = create.Learner("SL.xgboost",
                              tune = grid_params, detailed_names = TRUE, name_prefix = "xgb")


# Prediction wrapper functions for SL VIM
imp_fun <- function(object, newdata) {
  N_fold <- length(object$AllSL)
  all_preds=vector(length=nrow(newdata))

  #for each fold, call predict function
  for (i in 1:N_fold) {
    SL_model=object$AllSL[[i]]
    which_row=as.integer(rownames(object$AllSL[[i]]$SL.predict))
    all_preds[which_row]=predict.SuperLearner(object=SL_model,
                                              newdata = newdata[which_row,])$pred
  }

  return(all_preds)
}


#change functions for SHAP anal
predict_model.SuperLearner <- function(object, newdata) {
  predict(object, newdata = newdata)$pred
}


get_model_specs.SuperLearner_decipherPEC <- function(x){
  feature_list = list()
  
  feature_list$labels <- covars#x$varNames
  m <- length(feature_list$labels)
  
  feature_list$classes <-rep("numeric",m)# attr(x$Terms,"dataClasses")[-1]
  
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[feature_list$classes=="factor"] <- NA
  
  return(feature_list)
}



get_model_specs.SuperLearner <- function(x){
  
  data=data.frame(y_train=as.numeric(y_train),
                  x_train)

  x <- gbm::gbm(
    y_train ~ .,
    data = data,
    distribution = "gaussian"
  )
  
  
  feature_list = list()
  feature_list$labels <- labels(x$Terms)
  m <- length(feature_list$labels)
  
  feature_list$classes <- attr(x$Terms,"dataClasses")[-1]
  feature_list$factor_levels <- setNames(sapply(x_train, levels), feature_list$labels)
  
  # feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  # feature_list$factor_levels[feature_list$classes=="factor"] <- NA # the model object doesn't contain factor levels info
  
  
  return(feature_list)
}


# head(imp_fun(object=sl_20_allAlgo[[1]],newdata=SL_data_20_MHC[,-(1:5)]))
# summary(sl_20_allAlgo[[1]])


# # Prepare the data for explanation
# shapr<-function (x, model, n_combinations = NULL)
# {
# 
#   if (!is.matrix(x) & !is.data.frame(x)) {
#     stop("x should be a matrix or a dataframe.")
#   }
#   explainer <- as.list(environment())
#   explainer$exact <- ifelse(is.null(n_combinations), TRUE,
#                             FALSE)
# 
#   feature_list_model <- get_model_specs(model)
# 
#   processed_list <- preprocess_data(x = x, feature_list = feature_list_model)
#   x_train <- processed_list$x_dt
#   updated_feature_list <- processed_list$updated_feature_list
#   explainer$n_features <- ncol(x_train)
# 
#   tmp <- predict_model(model, head(x_train, 2))
#   if (!(all(is.numeric(tmp)) & length(tmp) == 2)) {
#     stop(paste0("The predict_model function of class ",
#                 class(model), " is invalid.\n", "See the 'Advanced usage' section of the vignette:\n",
#                 "vignette('understanding_shapr', package = 'shapr')\n",
#                 "for more information on running shapr with custom models.\n"))
#   }
#   ##########
# 
#   dt_combinations <- feature_combinations(m = explainer$n_features,
#                                           exact = explainer$exact, n_combinations = n_combinations,
#                                           weight_zero_m = 10^6)
# 
#   weight_matrix <- function(X, normalize_W_weights = TRUE) {
# 
#     # Fetch weights
#     w <- X[["shapley_weight"]]
# 
#     if (normalize_W_weights) {
#       w[-c(1, length(w))] <- w[-c(1, length(w))] / sum(w[-c(1, length(w))])
#     }
# 
#     W <- weight_matrix_cpp(
#       features = X[["features"]],
#       m = X[.N][["n_features"]],
#       n = X[, .N],
#       w = w
#     )
# 
#     return(W)
#   }
# 
# 
#   weighted_mat <- weight_matrix(X = dt_combinations, normalize_W_weights = TRUE)
#   feature_matrix <- feature_matrix_cpp(features = dt_combinations[["features"]],
#                                        m = explainer$n_features)
#   if (!explainer$exact && n_combinations > (2^explainer$n_features -
#                                             2)) {
#     explainer$exact <- TRUE
#   }
#   explainer$S <- feature_matrix
#   explainer$W <- weighted_mat
#   explainer$X <- dt_combinations
#   explainer$x_train <- x_train
#   explainer$x <- NULL
#   explainer$feature_list <- updated_feature_list
#   attr(explainer, "class") <- c("explainer", "list")
#   return(explainer)
# }