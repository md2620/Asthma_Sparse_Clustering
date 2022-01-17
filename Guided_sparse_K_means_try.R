#Reading the files
df = read.csv("/rds/general/user/md2620/home/asthma/Data/expression_matrix_serum.csv")


#Guided Sparse K-Means 
GuidedSparseKmeans.KLam <- function(x, z, pre.K = NULL, s.one, model, nstart = 20, maxiter = 15, silence = F) {
  
  if (is.null(s.one)) 
    stop("Must provide s.one.")
  if (s.one <= 1) 
    stop("s.one should be greater than 1.")
  if (is.null(model) || model %in% c("linear", "logit", "exp", "polr", "cox") != TRUE) 
    stop("Must select one from 'linear', 'logit', 'exp', 'polr','cox'.")
  
  R2.per <- getR2(x, z, model)
  
  if (is.null(pre.K)) {
    o.cor <- order(R2.per, decreasing = T)
    sim.expr.top <- x[, o.cor][, 1:400]
    gsP.Z <- clusGap(sim.expr.top, FUN = kmeans, K.max = 8, B = 50)
    K.select <- which.max(gsP.Z$Tab[, "gap"])
  } else K.select <- pre.K
  
  lam.pool <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5)
  
  output <- list()
  for (i in 1:length(lam.pool)) {
    if (silence == F) 
      print(paste0("lam=", lam.pool[i]))
    output[[i]] <- GuidedSparseKmeans.R2out(x, R2.per, K = K.select, s = s.one, lam = lam.pool[i], nstart = nstart, 
                                            maxiter = maxiter, silence = silence)
  }
  ARI.Cs <- c(adjustedRandIndex(output[[1]][[1]]$clusters, output[[2]][[1]]$clusters), adjustedRandIndex(output[[2]][[1]]$clusters, 
                                                                                                         output[[3]][[1]]$clusters), adjustedRandIndex(output[[3]][[1]]$clusters, output[[4]][[1]]$clusters), adjustedRandIndex(output[[4]][[1]]$clusters, 
                                                                                                                                                                                                                                output[[5]][[1]]$clusters), adjustedRandIndex(output[[5]][[1]]$clusters, output[[6]][[1]]$clusters), adjustedRandIndex(output[[6]][[1]]$clusters, 
                                                                                                                                                                                                                                                                                                                                                       output[[7]][[1]]$clusters), adjustedRandIndex(output[[7]][[1]]$clusters, output[[8]][[1]]$clusters), adjustedRandIndex(output[[8]][[1]]$clusters, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              output[[9]][[1]]$clusters), adjustedRandIndex(output[[9]][[1]]$clusters, output[[10]][[1]]$clusters))
  jaccard.gene <- c(jaccard(output[[1]][[1]]$weights != 0, output[[2]][[1]]$weights != 0), jaccard(output[[2]][[1]]$weights != 
                                                                                                     0, output[[3]][[1]]$weights != 0), jaccard(output[[3]][[1]]$weights != 0, output[[4]][[1]]$weights != 
                                                                                                                                                  0), jaccard(output[[4]][[1]]$weights != 0, output[[5]][[1]]$weights != 0), jaccard(output[[5]][[1]]$weights != 
                                                                                                                                                                                                                                       0, output[[6]][[1]]$weights != 0), jaccard(output[[6]][[1]]$weights != 0, output[[7]][[1]]$weights != 
                                                                                                                                                                                                                                                                                    0), jaccard(output[[7]][[1]]$weights != 0, output[[8]][[1]]$weights != 0), jaccard(output[[8]][[1]]$weights != 
                                                                                                                                                                                                                                                                                                                                                                         0, output[[9]][[1]]$weights != 0), jaccard(output[[9]][[1]]$weights != 0, output[[10]][[1]]$weights != 
                                                                                                                                                                                                                                                                                                                                                                                                                      0))
  
  len1 <- len2 <- length(lam.pool) - 1
  lam.select1 <- lam.select2 <- 0.25
  
  for (i in 1:(len1 - 2)) {
    mu1 <- mean(ARI.Cs[(len1 - i):len1])
    sd1 <- sd(ARI.Cs[(len1 - i):len1])
    if (ARI.Cs[len1 - i - 1] < mu1 - 2 * max(sd1, 0.05) | ARI.Cs[len1 - i - 1] > mu1 + 2 * max(sd1, 0.05)) {
      lam.select1 <- lam.pool[len1 - i]
      break
    }
  }
  
  for (i in 1:(len2 - 2)) {
    mu2 <- mean(jaccard.gene[(len2 - i):len2])
    sd2 <- sd(jaccard.gene[(len2 - i):len2])
    if (jaccard.gene[len2 - i - 1] < mu2 - 2 * max(sd2, 0.05) | jaccard.gene[len2 - i - 1] > mu2 + 2 * max(sd2, 
                                                                                                           0.05)) {
      lam.select2 <- lam.pool[len2 - i]
      break
    }
  }
  lam.select <- max(lam.select1, lam.select2)
  
  list(K = K.select, lam = lam.select, R2.per = R2.per, ARI.Cs = ARI.Cs, Jaccard.gene = jaccard.gene)
}


## 2nd function 
GuidedSparseKmeans <- function(x, z, K, s, lam, model, nstart = 20, maxiter = 15, silence = F) {
  
  if (is.null(K)) 
    stop("Must provide K.")
  if (is.null(s)) 
    stop("Must provide s.")
  if (min(s) <= 1) 
    stop("s should be greater than 1.")
  if (is.null(lam)) 
    stop("Must provide lam.")
  if (is.null(model) || model %in% c("linear", "logit", "exp", "polr", "cox") != TRUE) 
    stop("Must select one from 'linear', 'logit', 'exp', 'polr','cox'.")
  
  R2.per <- getR2(x, z, model)
  ind.init.zero <- order(R2.per, decreasing = T)[401:(ncol(x))]
  R2.per.new <- R2.per
  R2.per.new[ind.init.zero] <- 0
  R2.per.prop <- abs(R2.per.new/sum(R2.per.new))
  out <- list()
  for (i in 1:length(s)) {
    if (silence == F) 
      print(paste0("s=", s[i]))
    w <- R2.per.prop * s[i]
    w.old <- rep(1, ncol(x))
    obj.w <- NULL
    niter <- 0
    while (l1n(w - w.old)/l1n(w.old) > 1e-04 && niter < maxiter) {
      niter <- niter + 1
      if (silence == F) 
        print(niter)
      w.old <- w
      y <- sweep(x[, w != 0], 2, sqrt(w[w != 0]), "*")
      c <- kmeans(y, K, nstart = nstart)$cluster
      update <- update.w(x, c, s[i], lam, R2.per)
      w <- update$w
      obj.w <- c(obj.w, update$bc.t.prop.w + lam * update$R2.w)
    }
    out[[i]] <- list(weights = w, clusters = c, object = obj.w, bound = s[i], R2.per = R2.per)
  }
  return(out)
}



