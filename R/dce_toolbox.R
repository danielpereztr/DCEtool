requireNamespace("fastDummies")
requireNamespace("tibble")
requireNamespace("tidyverse")
requireNamespace("stringi")
requireNamespace("dplyr")
requireNamespace("matrixStats")
requireNamespace("idefix")

# Generate the Optimal or Bayesian efficient design.
#' @export
dce_toolbox <- function(attributes, csets, alts, nochoice, priors, alg){
  #checks
  if (nochoice == TRUE){
    nc <- 1
  } else {
    nc <- 0
  }
  if ((sum(attributes)-length(attributes)+nc) != length(priors) & alg == "fedorov"){
    stop("The number of priors does not correspond to the number of parameters in the model.")
  }
  if (sum(attributes-1)>csets){
    stop("The number of sets is not enough.", " Try with ", sum(attributes-1), " sets or more.")
  }
  
  # Create all alternatives
  toexp <- list()
  for (i in attributes){
    te <- seq(1,i)
    toexp[[length(toexp) + 1]] <- te
  }
  profiles <- expand.grid(toexp)
  
  
  # Create the full factorial design
  expcs <- list()
  for (j in 1:alts){
    expcs[[length(expcs) + 1]] <- seq(1,nrow(profiles))
  }
  sets <- expand.grid(expcs)
  sets <- sets[sets$Var1 > sets$Var2,]
  allsets <- data.frame()
  for (row in 1:nrow(sets)){
    for (alt in 1:alts){
      pr <- as.numeric(sets[row,][alt])
      p <- profiles[pr,]
      p <- cbind(row, p)
      allsets <- rbind(allsets, p)
    }
    if (nochoice == TRUE){
      n <- rep(1, (length(p))-1)
      n <- unlist(append(p[1], n))
      allsets <- rbind(allsets, n)
    }
  }
  
  # Select a random design with the number of sets inputed
  random.sets <- sample.int(max(allsets$row),csets)
  randomdce <- allsets[allsets$row %in% random.sets,]
  
  # Dummify function
  dummify <- function(randomdce, alts){
    temp_mat <- data.frame(NA)
    for (col in 1:(ncol(randomdce[,2:ncol(randomdce)]))){
      temp_dum <- fastDummies::dummy_cols(randomdce[,col+1])
      temp_dum <- temp_dum[,2:ncol(temp_dum)]
      temp_dum <- as.data.frame(temp_dum)
      for (x in 1:ncol(temp_dum)){
        colnames(temp_dum)[x] <- paste0("Var",col,x)
      }
      temp_mat <- cbind(temp_mat, temp_dum)
    }
    temp_mat[1] <- randomdce[1]    
    colnames(temp_mat)[1] <- "task"
    temp_mat <- tibble::add_column(temp_mat, rep(1:alts, length.out = nrow(temp_mat)) ,.after = "task")
    colnames(temp_mat)[2] <- "alt"
    temp_mat <- temp_mat[, -which(stringi::stri_sub(colnames(temp_mat),-1) == 1)]
    temp_mat[,1] <- rep(1:(nrow(temp_mat)/2), each = alts)[1:nrow(temp_mat)]
    return(temp_mat)
  }
  
  # Dp-error function for the "optimal" DCE. The DBerr function from 'idefix' will be used for bayesian designs. 
  D.error <- function(des, priors){
    #compute exp(Vi)
    des <- cbind(des, "expv" = t(exp(priors %*% t(des[,3:ncol(des)]))))
    #calculate probabilities
    des <- merge(des, stats::aggregate(expv ~ task, FUN = "sum", data = des), by = "task")
    des <- cbind(des[1:(length(des)-2)],  "prob" = des$expv.x/des$expv.y)
    #information matrix
    m <- matrix(data = 0, ncol = length(priors), nrow = length(priors)) #accumulation matrix
    for (i in 1:max(des$task)){
      n <- t(des[des$task == i,][3:(ncol(des)-1)]) %*% 
        (diag(des[des$task == i,ncol(des)]) - des[des$task == i,ncol(des)] %*% 
           t(des[des$task == i,ncol(des)])) %*% 
        as.matrix(des[des$task == i,][3:(ncol(des)-1)])
      m <- m + n 
    }
    #D-error
    Derr <- det(m)^(-1/length(priors))
    return(Derr)
  }
  
  # Add an alternative if there is a null option
  if (nochoice == TRUE){
    alt <- alts + 1 
  } else {
    alt <- alts
  }
  
  # Dummify the temp_mat
  temp_mat <- dummify(randomdce, alts = alt)
  
  # Add the opt-out option
  if (nochoice == TRUE){
    optout <- rep(0, alts)
    optout <- append(optout, 1)
    optout <- rep(optout, max(temp_mat$task))
    temp_mat <- cbind(temp_mat[,1:2], optout, temp_mat[,3:ncol(temp_mat)])
  }
  
  # Modified Fedorov algorithm
  fedorov <- function(allsets, alt, profiles, csets, priors, nochoice, temp_mat){
    allsets_d <- dummify(allsets, alt)
    acumderr <- c()
    min <- c()
    allalts <- cbind(allsets[1:nrow(profiles),1], profiles)
    colnames(allalts)[1] <- "row"
    allalts <- dummify(allalts, alts = alts)
    it_d <- D.error(des = temp_mat, priors = priors)
    if (nochoice == TRUE){
      msp <- 4
    } else {
      msp <- 3
    }
    aa <- allalts[,3:ncol(allalts)]
    for (it in 1:10){ ### Iterations
      for (a in 1:nrow(temp_mat)){ ### Alternatives in the design
        if (temp_mat$alt[a] == (alts+1)){
          next
        } else{
          for (p in 1:(nrow(aa))){ ### Try all possible alternatives
            temp_mat[a,msp:ncol(temp_mat)] <- aa[p,]
            min <- append(min, D.error(des = temp_mat, priors = priors))
          }
          temp_mat[a,msp:ncol(temp_mat)] <- aa[which(min == min(min, na.rm = TRUE))[1],]
          min <- c()
        }
      }
      it_d <- append(it_d, D.error(des = temp_mat, priors = priors))
      change <- (it_d[(length(it_d))] - it_d[(length(it_d)-1)])/it_d[(length(it_d)-1)]
      change <- change*100
      if (it == 1){cat("D-error of the random starting design: ", it_d[1])} else {cat("\n")}
      if (change >= -0.1 && it >= 2){
        break
      } else {
        cat("\nIteration ", it, ": D-error ", D.error(des = temp_mat, priors = priors))
        cat("\nChange: ", change, "% \n")
      }
    }
    
    cat(D.error(des = temp_mat, priors = priors))
    Derror <- D.error(des = temp_mat, priors = priors)
    results <- list("design" = temp_mat, "D-error" = Derror, "details" = "Fedorov modified algorithm used to generate an optimal design")
    return(results)
    return(cat("D-error of the current design: ", D.error(des = temp_mat, priors = priors)))
  }
  
  cea <- function(randomdce, attributes, alts, csets, priors, nochoice){
    min <- c()
    it_d <- c()
    for (it in 1:10){
      for (row in 1:nrow(randomdce)){ #Loop for rows
        if (nochoice == FALSE | (nochoice == TRUE && row %% alt)){
          for (at in 1:(ncol(randomdce)-1)){ #attribute
            for (lev in 1:attributes[at]){
              randomdce[row, at+1] <- lev
              temp_mat <- dummify(randomdce, alts)
              if (nochoice == TRUE){
                temp_mat <- tibble::add_column(temp_mat, "optout" = rep(append(rep(0, alts), 1), csets), .after = 2)
              }
              min <- append(min, idefix::DBerr(priors, as.matrix(temp_mat[,-c(1,2)]), alts))
            }
            randomdce[row, at+1] <- which(min == min(min, na.rm = TRUE))[1]
            min <- c()
          }
        }
      }
      temp_mat <- dummify(randomdce, alts)
      if (nochoice == TRUE){
        temp_mat <- tibble::add_column(temp_mat, "optout" = rep(append(rep(0, alts), 1), csets), .after = 2)
      }
      it_d <- append(it_d, idefix::DBerr(priors, as.matrix(temp_mat[,-c(1,2)]), alts))
      change <- (it_d[(length(it_d))] - it_d[(length(it_d)-1)])/it_d[(length(it_d)-1)]
      change <- change*100
      if (it == 1){cat("DB-error of the random starting design: ", it_d[1])} else {cat("\n")}
      if (change >= -0.1 && it >= 2){
        break
      } else {
        cat("\nIteration ", it, ": DB-error ", idefix::DBerr(priors, as.matrix(temp_mat[,-c(1,2)]), alts))
        cat("\nChange: ", change, "% \n")
      }
    }
    Derror <- idefix::DBerr(priors, as.matrix(temp_mat[,-c(1,2)]), alts)
    
    if (nochoice == TRUE){  
      temp_mat$task <- rep(1:csets, each = alts+1)
      temp_mat$alt <- rep(1:(alts+1), times = csets)
    }
    
    results <- list("design" = temp_mat, "DB-error" = Derror, "details" = "Coordinate exchange algorithm used to generate an efficient Bayesian design")
    
    return(results)
  }
  
  
  if (alg == "cea" && nochoice == TRUE){
    nrandomdce <- data.frame()
    for (i in seq(from = 1, to = nrow(randomdce), by = (alt))){
      temporal <- randomdce[i:(alt),]
      nrandomdce <- rbind(nrandomdce, temporal, rep(1, each = length(attributes)))
    }
  }
  
  if (alg != "fedorov" && alg != "cea"){
    stop("The 'alg' argument must be either 'fedorov' or 'cea'.")
  } else if (alg == "fedorov"){
    return(fedorov(allsets, alt, profiles, csets, priors, nochoice, temp_mat))
  } else if (alg == "cea"){
    return(cea(randomdce, attributes, alts, csets, priors, nochoice))
  }
}
