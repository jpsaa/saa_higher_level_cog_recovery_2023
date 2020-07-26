predict_lqmm_modified <- function (object, newdata, level = 0, ...) {
  
  tau <- object$tau
  nq <- length(tau)
  group <- object$group
  M <- object$ngroups
  q <- object$dim_theta[2]
  
  group.new <- as.numeric(newdata$id)
  M.new <- length(unique(newdata$id))
  
  ### encoding the new data
  newdata$id <- as.numeric(newdata$id)
  enc <- dataPreparation::build_encoding(dataSet = newdata, 
                                         cols = names(newdata))
  newdata.enc <- dataPreparation::one_hot_encoder(dataSet = newdata, 
                                                  encoding = enc, 
                                                  drop = TRUE)
  
  # enc <- dataPreparation::build_encoding(dataSet = train.data, cols = names(train.data))
  # train.data.enc <- dataPreparation::one_hot_encoder(dataSet = train.data, encoding = enc, drop = TRUE)
  
  ### matching the column names
  names(newdata.enc) <- gsub("\\.| |-|_", "", names(newdata.enc))
  colnames(object$mmf) <- gsub("\\.| |-|_", "", colnames(object$mmf))
  # names(train.data.enc) <- gsub("\\.| |-","",names(train.data.enc))
  colnames(object$mmr) <- gsub("\\.| |-|_", "", colnames(object$mmr))
  
  ### gettting only columns used in model
  matched <- match(colnames(object$mmf), names(newdata.enc))
  newdata.mod <- newdata.enc %>% 
    select(matched) %>% 
    as.matrix
  # train.data.model <- train.data.enc %>% select(matched) %>% as.matrix
  
  if (nq == 1) {
    FXD <- object$mmf %*% matrix(object$theta_x)
    # prediction of new data with fixed effects only, when predicting only one quartile (length of tau ==1)
    FXD.new <- newdata.mod %*% matrix(object$theta_x)
  }
  else {
    # predictions as above for more than one quartile (tau > 1)
    rownames(object$theta_x) <- gsub("\\.| |-|_", "", rownames(object$theta_x))
    FXD <- object$mmf %*% object$theta_x
    FXD.new <- newdata.mod %*% object$theta_x
  }
  
  if (level == 1) {
    # predicting the random effects
    RE <- ranef(object)
    mmr.l <- split(object$mmr, group)
    # creating a diag matrix containing the new groups
    mmr.l.new <- split(cbind(newdata.enc , "(Intercept)" = 1) %>% 
                         select(colnames(object$mmr)), group.new)
    
    if (nq == 1) {
      # Predcting random effects from the train data for tau ==1
      RE.l <- split(RE, unique(group))
      RND <- NULL
      # container for new data
      RND.new <- NULL
      
      for (i in 1:M) {
        RND <- rbind(RND, matrix(as.numeric(mmr.l[[i]]), 
                                 ncol = q) %*% matrix(as.numeric(RE.l[[i]]), 
                                                      nrow = q))
      }
      
      # this loop returns the random effects for new data based on train data (for tau == 1)
      for (k in 1:M.new) {
        
        RND.new <- rbind(RND.new, 
                         matrix(as.numeric(unlist(mmr.l.new[[k]])),
                                ncol = q) %*% matrix(
                                  ## the function below extracts the random effects from the train data
                                  list(VarCorr.lqmm(object) ###function adapted from nlme package 
                                  )[[k]]))
      }
    }
    else {
      # random effects from train data
      RND <- matrix(NA, length(object$y), nq)
      # container for new data
      RND.new <- matrix(NA, nrow(newdata), nq)
      
      for (j in 1:nq) {
        RE.l <- split(RE[[j]], unique(group))
        tmp <- NULL
        for (i in 1:M) {
          tmp <- rbind(tmp, matrix(as.numeric(mmr.l[[i]]), 
                                   ncol = q) %*% matrix(as.numeric(RE.l[[i]]), 
                                                        nrow = q))
        }
        RND[, j] <- tmp
      }
      # this loop creates the random effects for the new data based on the random effects from the train data for tau > 1
      for (m in 1:nq) {
        RE.l.new <- split(VarCorr.lqmm(object)[[m]], unique(group.new))
        tmp <- NULL
        for (p in 1:M.new) {
          tmp <- rbind(tmp, matrix(as.numeric(unlist(mmr.l.new[[p]])),
                                   ncol = q ) %*% matrix(
                                     VarCorr.lqmm(object)[[p]]
                                   )
          )
        }
        RND.new[, m] <- tmp
      }  
      
    }
  }
  if (level == 0) {
    colnames(FXD) <- format(tau, digits = 4)
    colnames(FXD.new) <- format(tau, digits = 4)
    
    ans <- FXD[object$revOrder, ]
    ans.newdata <- round(FXD.new, 3)
    ans.newdata[newdata$time,]
    
  }
  if (level == 1) {
    ans <- FXD + RND
    ### analyzing how predictions are adjusted by random effects
    # cbind(cbind(FXD[1:10]) , cbind(RND[1:10]), cbind(FXD[1:10]) + cbind(RND[1:10]) )
    
    # adding the fixed and random effects for new data
    ans.newdata <- FXD.new + RND.new
    
    colnames(ans) <- format(tau, digits = 4)
    ans <- ans[object$revOrder, ]
    
    ans.newdata[newdata$time,]
    
  }
  # return only the new data
  return(ans.newdata)
}