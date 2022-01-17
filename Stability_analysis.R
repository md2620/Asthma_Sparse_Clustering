### LOAD ALL FUNCTIONS
### DON'T LOOK TOO MUCH INTO KprotoClustering function, i think it's wrong, i am trying to use KprotoClustering2 that I define at the end of those functions loading

KprotoClustering <- function(xdata, nc = NULL, Lambda, scale = TRUE, rows = TRUE, ...) {
  # Checking clustMixType package is installed
  if (!requireNamespace("clustMixType")) {
    stop("This function requires the 'clustMixType' package.")
  }
  
  # Storing extra arguments
  extra_args <- list(...)
  
  # Transposing for clustering of columns
  #if (!rows) {
 #   xdata <- t(xdata)
 # }
  
  # Scaling the data
 # if (scale) {
   # xdata <- scale(xdata)
 # }
  
  # Re-formatting Lambda
  if (is.vector(Lambda)) {
    Lambda <- cbind(Lambda)
  }
  
  # Re-formatting nc
  if (!is.null(nc)) {
    if (is.vector(nc)) {
      nc <- cbind(nc)
    }
  } else {
    nc <- cbind(seq(1, nrow(xdata)))
  }
  
  # Extracting relevant extra arguments (kproto)
  ids <- which(names(extra_args) %in% names(formals(clustMixType::kproto)))
  ids <- ids[!ids %in% c("x","k","keep.data", "na.rm", "lambda")]
  
  # Initialisation of array storing co-membership matrices
  adjacency <- array(NA, dim = c(nrow(xdata), nrow(xdata), nrow(nc) * nrow(Lambda)))
  weight <- matrix(NA, nrow = nrow(nc) * nrow(Lambda), ncol = ncol(xdata))
  
  # Iterating over the pair of parameters
  id <- 0
  for (i in 1:nrow(Lambda)) {
    # Running sparse hierarchical clustering
    myclust <- do.call(clustMixType::kproto, args = c(
      list(x = xdata,k = nc, keep.data = TRUE, na.rm = FALSE, lambda = Lambda[i, 1]),
      extra_args[ids]
    ))
    
    # Defining clusters
    mygroups <- do.call(clustMixType::kproto, args = list(tree = myclust$cluster, k = nc))
    if (is.null(dim(mygroups))) {
      mygroups <- cbind(mygroups)
    }
    for (j in 1:nrow(nc)) {
      adjacency[, , id + j] <- CoMembership(groups = mygroups[, j])
      weight[id + j, ] <- myclust$ws[, 1]
    }
    id <- id + nrow(nc)
  }
  
  # Setting row and column names
  rownames(weight) <- paste0("s", seq(0, nrow(weight) - 1))
  colnames(weight) <- colnames(xdata)
  
  return(list(comembership = adjacency, weight = weight))
}



############################### Co-membership function #################
#' Pairwise co-membership
#'
#' Generates a symmetric and binary matrix indicating if a pair of features
#' belongs to the same cluster.
#'
#' @param groups vector of group membership.
#'
#' @return A symmetric and binary matrix.
#'
#' @export
CoMembership <- function(groups) {
  if (length(unique(groups)) > 1) {
    # Building binary cluster membership for each feature
    V <- stats::model.matrix(~ as.factor(groups) - 1)
    
    # Building cluster co-membership
    comembership <- V %*% t(V)
  } else {
    comembership <- matrix(1, nrow = length(groups), ncol = length(groups))
  }
  
  # Re-formatting co-membership matrix
  diag(comembership) <- 0
  rownames(comembership) <- colnames(comembership) <- names(groups)
  
  return(comembership)
}


##### Consensus clustering #####
#' Runs (sparse) consensus clustering models with different combinations of
#' parameters controlling the number of clusters in the underlying algorithm
#' (\code{nc}), penalty for the selection of variables contributing to the
#' clustering (\code{Lambda}) and thresholds co-membership proportions. These
#' three parameters are jointly calibrated by maximising the stability score of
#' the model. The threshold in selection proportions to identify the variables
#' driving the clustering is also chosen by maximising the stability score in
#' the calibrated clustering model. This function can be used to identify stable
#' clusters of observations sharing similar profiles, as well as variables
#' driving the clustering if a sparse clustering algorithm is used.
#'
#' @inheritParams VariableSelection
#' @param xdata data matrix with observations as rows and variables as columns.
#' @param Lambda vector of penalty parameters.
#' @param nc matrix of parameters controlling the number of clusters in the
#'   underlying algorithm specified in \code{implementation}. If \code{nc} is
#'   not provided, it is set to \code{seq(1, nrow(xdata))}.
#' @param implementation function to use for clustering. Possible functions
#'   include \code{\link{HierarchicalClustering}} (hierarchical clustering),
#'   \code{\link{SparseHierarchicalClustering}} (sparse hierarchical
#'   clustering), \code{\link{KMeansClustering}} (k-means),
#'   \code{\link{GMMClustering}} (Gaussian Mixture Models),
#'   \code{\link{PAMClustering}} (Partioning Around Medoids),
#'   \code{\link{CLARAClustering}} (Clustering Large Applications) and
#'   \code{\link{DBSCANClustering}} (Density-Based Spatial Clustering of
#'   Applications with Noise). Alternatively, a user-defined function taking
#'   \code{xdata} and \code{Lambda} as arguments and returning a binary and
#'   symmetric matrix for which diagonal elements are equal to zero can be used.
#' @param scale logical indicating if the data should be scaled to ensure that
#'   all variables contribute equally to the clustering of the observations.
#'
#' @details To ensure reproducibility of the results, the state of the random
#'   number generator is fixed to \code{seed}. For parallelisation of the code,
#'   consensus clustering results produced with different \code{seed}s and all
#'   other parameters equal can be combined (more details in
#'   \code{\link{Combine}}).
#'
#' @return A list with: \item{S}{a matrix of the best (block-specific) stability
#'   scores for different parameters controlling the number of clusters in the
#'   underlying algorithm. } \item{Lambda}{a matrix of parameters controlling
#'   the number of clusters. } \item{Q}{a matrix of average numbers of
#'   co-members for different parameters controlling the number of clusters.}
#'   \item{Q_s}{a matrix of calibrated numbers of stable co-members for
#'   different parameters controlling the number of clusters in the underlying
#'   algorithm. } \item{P}{a matrix of calibrated thresholds in co-membership
#'   proportions for different parameters controlling the number of clusters in
#'   the underlying algorithm. } \item{PFER}{a matrix of the upper-bounds in
#'   PFER of calibrated consensus clustering models with different (sets of)
#'   parameters controlling the number of clusters in the underlying algorithm.}
#'   \item{FDP}{a matrix of the upper-bounds in FDP of calibrated consensus
#'   clustering models with different parameters controlling the number of
#'   clusters in the underlying algorithm.} \item{S_2d}{an array of stability
#'   scores obtained with different combinations of parameters. Columns
#'   correspond to different thresholds in co-membership proportions. }
#'   \item{PFER_2d}{an array of computed upper-bounds of PFER obtained with
#'   different combinations of parameters. Columns correspond to different
#'   thresholds in co-membership proportions. } \item{FDP_2d}{an array of
#'   computed upper-bounds of FDP obtained with different combinations of
#'   parameters. Columns correspond to different thresholds in co-membership
#'   proportions. } \item{selprop}{an array of co-membership proportions. Rows
#'   and columns correspond to features being clustered (rows of \code{xdata}).
#'   Indices along the third dimension correspond to different parameters
#'   controlling the number of clusters in the underlying algorithm.}
#'   \item{methods}{a list with \code{type="clustering"}, \code{implementation},
#'   \code{resampling} and \code{PFER_method} values used for the run.}
#'   \item{param}{a list with values of other objects used for the run.} For all
#'   objects except \code{selprop} and those stored in \code{methods} or
#'   \code{params}, rows correspond to parameter values stored in the output
#'   \code{Lambda}.
#'
#' @family stability selection functions
#' @seealso \code{\link{Resample}}, \code{\link{StabilityScore}},
#'   \code{\link{SparseHierarchicalClustering}}, \code{\link{KMeansClustering}},
#'   \code{\link{GMMClustering}}, \code{\link{PAMClustering}}
#'
#' @references \insertRef{ourstabilityselection}{focus}
#'
#'   \insertRef{stabilityselectionMB}{focus}
#'
#'   \insertRef{stabilityselectionSS}{focus}
#'
#' @examples
#' \dontshow{
#' set.seed(1)
#' simul <- SimulateClustering(n = c(5, 5, 5), pk = 10)
#' stab <- Clustering(
#'   xdata = simul$data,
#'   nc = c(2, 3),
#'   Lambda = c(1.1, 1.2),
#'   K = 5,
#'   verbose = FALSE
#' )
#' mymembership <- Clusters(stab)
#' }
#' \dontrun{
#' # Data simulation
#' set.seed(2)
#' simul <- SimulateClustering(
#'   n = c(30, 30, 30), pk = 15,
#'   theta_xc = c(rep(1, 5), rep(0, 10)),
#'   ev_xc = c(rep(0.95, 5), rep(0, 10)),
#' )
#' par(mar = c(5, 5, 5, 5))
#' Heatmap(
#'   mat = as.matrix(dist(simul$data)),
#'   colours = c("navy", "white", "red")
#' )
#'
#' # Sparse consensus clustering
#' stab <- Clustering(
#'   xdata = simul$data,
#'   Lambda = cbind(seq(1.1, 2, by = 0.1))
#' )
#'
#' # Stably contributing variables
#' CalibrationPlot(stab)
#' Argmax(stab, clustering = FALSE)
#' ArgmaxId(stab, clustering = FALSE)
#' SelectionProportions(stab)
#' SelectedVariables(stab)
#'
#' # Stable clusters
#' CalibrationPlot(stab, clustering = TRUE)
#' Argmax(stab, clustering = TRUE)
#' ArgmaxId(stab, clustering = TRUE)
#' plot(Graph(Adjacency(stab), node_colour = simul$theta, satellites = TRUE))
#' ClusteringPerformance(theta = Clusters(stab), theta_star = simul$theta)
#'
#' # Consensus clustering based on hierarchical clustering
#' stab <- Clustering(
#'   xdata = simul$data,
#'   implementation = HierarchicalClustering
#' )
#' CalibrationPlot(stab, xlab = expression(italic(k)), clustering = TRUE)
#' plot(Graph(Adjacency(stab), node_colour = simul$theta, satellites = TRUE))
#' ClusteringPerformance(theta = Clusters(stab), theta_star = simul$theta)
#'
#' # Consensus clustering based on k-means clustering
#' stab <- Clustering(
#'   xdata = simul$data,
#'   implementation = KMeansClustering
#' )
#' CalibrationPlot(stab, xlab = expression(italic(k)), clustering = TRUE)
#' plot(Graph(Adjacency(stab), node_colour = simul$theta, satellites = TRUE))
#' ClusteringPerformance(theta = Clusters(stab), theta_star = simul$theta)
#'
#' # Consensus clustering based on PAM clustering
#' stab <- Clustering(
#'   xdata = simul$data,
#'   implementation = PAMClustering
#' )
#' CalibrationPlot(stab, xlab = expression(italic(k)), clustering = TRUE)
#' plot(Graph(Adjacency(stab), node_colour = simul$theta, satellites = TRUE))
#' ClusteringPerformance(theta = Clusters(stab), theta_star = simul$theta)
#' }
#' @export
Clustering <- function(xdata, Lambda = NULL, nc = NULL,
                       pi_list = seq(0.6, 0.9, by = 0.01), K = 100, tau = 0.5, seed = 1, n_cat = 3,
                       implementation = SparseHierarchicalClustering, scale = TRUE,
                       n_cores = 1, output_data = FALSE, verbose = TRUE, ...) {
  # Visiting all possible numbers of clusters
  if (is.null(nc)) {
    nc <- cbind(seq(1, nrow(xdata) * tau))
  } else {
    if (any(nc > (nrow(xdata) * tau))) {
      nc <- nc[nc <= (nrow(xdata) * tau)]
      if (length(nc) > 0) {
        warning(paste0(
          "Invalid input for argument 'nc'. The number of clusters can not exceed the subsample size: ",
          nrow(xdata) * tau, ". Invalid values have been removed."
        ))
        nc <- cbind(nc)
      } else {
        stop(paste0(
          "Invalid input for argument 'nc'. The number of clusters can not exceed the subsample size: ",
          nrow(xdata) * tau, "."
        ))
      }
    }
  }
  
  # Setting fixed Lambda if not provided
  if (is.null(Lambda)) {
    Lambda <- 1.1
  }
  
  # Error and warning messages
  resampling <- "subsampling" # only subsampling is available as bootstrap would give distance of zero
  PFER_method <- "MB"
  PFER_thr <- Inf # different interpretation in clustering
  FDP_thr <- Inf
  pk <- NULL
  lambda_other_blocks <- 0.1
  start <- "warm"
  Lambda_cardinal <- 50
  lambda_max <- NULL
  lambda_path_factor <- 0.001
  max_density <- 0.5
  bigblocks <- bigblocks_vect <- blocks <- N_blocks <- nblocks <- PFER_thr_blocks <- FDP_thr_blocks <- NULL
  CheckInputGraphical(
    xdata = xdata, pk = pk, Lambda = Lambda, lambda_other_blocks = lambda_other_blocks,
    pi_list = pi_list, K = K, tau = tau, seed = seed, n_cat = n_cat,
    implementation = implementation, start = start, scale = scale,
    resampling = resampling, PFER_method = PFER_method, PFER_thr = PFER_thr, FDP_thr = FDP_thr,
    Lambda_cardinal = Lambda_cardinal,
    lambda_max = lambda_max, lambda_path_factor = lambda_path_factor, max_density = max_density,
    verbose = verbose
  )
  
  # Check if parallelisation is possible (forking)
  if (.Platform$OS.type != "unix") {
    if (n_cores > 1) {
      warning("Invalid input for argument 'n_cores'. Parallelisation relies on forking, it is only available on Unix systems.")
    }
    n_cores <- 1
  }
  
  # Stability selection and score
  mypar <- parallel::mclapply(X = 1:n_cores, FUN = function(k) {
    return(SerialClustering(
      xdata = xdata, Lambda = cbind(Lambda), nc = cbind(nc),
      pi_list = pi_list, K = ceiling(K / n_cores), tau = tau, seed = as.numeric(paste0(seed, k)), n_cat = n_cat,
      implementation = implementation,
      output_data = output_data, verbose = verbose, ...
    ))
  }) # keep pk for correct number of blocks etc
  
  # Combining the outputs from parallel iterations
  out <- mypar[[1]]
  if (n_cores > 1) {
    for (i in 2:length(mypar)) {
      out <- do.call(Combine, list(stability1 = out, stability2 = mypar[[2]], graph = TRUE))
    }
  }
  
  # Re-set the function names
  if ("methods" %in% names(out)) {
    myimplementation <- as.character(substitute(implementation))
    if (is.function(resampling)) {
      myresampling <- as.character(substitute(resampling))
    } else {
      myresampling <- resampling
    }
    out$methods$implementation <- myimplementation
    out$methods$resampling <- myresampling
    out$methods$type <- "clustering"
  }
  
  # Defining the class
  class(out) <- "clustering"
  
  return(out)
}


#' Consensus clustering (internal)
#'
#' Runs stability selection regression models with different combinations of
#' parameters controlling the sparsity of the underlying selection algorithm
#' (e.g. penalty parameter for regularised models) and thresholds in selection
#' proportions. These two parameters are jointly calibrated by maximising the
#' stability score of the model (possibly under a constraint on the expected
#' number of falsely stably selected features). This function uses a serial
#' implementation and requires the grid of parameters controlling the underlying
#' algorithm as input (for internal use only).
#'
#' @inheritParams Clustering
#' @param Lambda matrix of parameters controlling the level of sparsity in the
#'   underlying feature selection algorithm specified in \code{implementation}.
#'   With \code{implementation="glmnet"}, \code{Lambda} contains penalty
#'   parameters.
#'
#' @return A list with: \item{S}{a matrix of the best stability scores for
#'   different parameters controlling the level of sparsity in the underlying
#'   algorithm.} \item{Lambda}{a matrix of parameters controlling the level of
#'   sparsity in the underlying algorithm.} \item{Q}{a matrix of the average
#'   number of selected features by underlying algorithm with different
#'   parameters controlling the level of sparsity.} \item{Q_s}{a matrix of the
#'   calibrated number of stably selected features with different parameters
#'   controlling the level of sparsity.} \item{P}{a matrix of calibrated
#'   thresholds in selection proportions for different parameters controlling
#'   the level of sparsity in the underlying algorithm.} \item{PFER}{a matrix of
#'   the upper-bounds in PFER of calibrated stability selection models with
#'   different parameters controlling the level of sparsity.} \item{FDP}{a
#'   matrix of the upper-bounds in FDP of calibrated stability selection models
#'   with different parameters controlling the level of sparsity.} \item{S_2d}{a
#'   matrix of stability scores obtained with different combinations of
#'   parameters. Columns correspond to different tresholds in selection
#'   proportions.} \item{selprop}{a matrix of selection proportions. Columns
#'   correspond to predictors from \code{xdata}.} \item{Beta}{an array of model
#'   coefficients. Columns correspond to predictors from \code{xdata}. Indices
#'   along the third dimension correspond to different resampling iterations.
#'   With multivariate outcomes, indices along the fourth dimension correspond
#'   to outcome-specific coefficients.} \item{method}{a list of
#'   \code{implementation}, \code{family}, \code{resampling} and
#'   \code{PFER_method} values used for the run.} \item{param}{a list of
#'   \code{K}, \code{pi_list}, \code{tau}, \code{n_cat}, \code{pk}, \code{n}
#'   (number of observations), \code{PFER_thr}, \code{FDP_thr} and \code{seed}
#'   values used for the run. The datasets \code{xdata} and \code{ydata} are
#'   also included if \code{output_data=TRUE}.} For all objects except those
#'   stored in \code{methods} or \code{params}, rows correspond to parameter
#'   values stored in the output \code{Lambda}.
#'
#' @keywords internal
SerialClustering <- function(xdata, Lambda, nc, pi_list = seq(0.6, 0.9, by = 0.01),
                             K = 100, tau = 0.5, seed = 1, n_cat = 3,
                             implementation = SparseHierarchicalClustering,
                             output_data = FALSE, verbose = TRUE, ...) {
  
  # Defining resampling method (only subsampling is available as bootstrap would give distance of zero)
  resampling <- "subsampling"
  PFER_method <- "MB"
  PFER_thr <- Inf
  FDP_thr <- Inf
  
  # # Defining K if using complementary pairs (SS)
  # if (PFER_method == "SS") {
  #   K <- ceiling(K / 2) * 2
  #   tau <- 0.5
  # }
  
  # Initialising objects to be filled
  N <- N_block <- ncol(xdata)
  
  # Initialising array of co-membership proportions
  bigstab_obs <- array(0,
                       dim = c(nrow(xdata), nrow(xdata), nrow(Lambda) * nrow(nc)),
                       dimnames = list(rownames(xdata), rownames(xdata), NULL)
  )
  sampled_pairs <- matrix(0, nrow = nrow(xdata), ncol = nrow(xdata))
  rownames(sampled_pairs) <- colnames(sampled_pairs) <- rownames(xdata)
  
  # Initialising the array for contributing variables
  Beta <- array(0, dim = c(nrow(Lambda) * nrow(nc), ncol(xdata), K))
  rownames(Beta) <- paste0("s", seq(0, nrow(Beta) - 1))
  colnames(Beta) <- colnames(xdata)
  
  # Setting seed for reproducibility
  withr::local_seed(seed)
  
  # Computation of the selection proportions over Lambda
  if (verbose) {
    pb <- utils::txtProgressBar(style = 3)
  }
  if (PFER_method == "MB") {
    for (k in 1:K) {
      # Subsampling observations
      s <- Resample(
        data = xdata, family = NULL, tau = tau, resampling = resampling, ...
      )
      Xsub <- xdata[s, ]
      
      # Applying clustering algorithm
      mybeta <- ClusteringAlgo(
        xdata = Xsub,
        Lambda = Lambda, nc = nc,
        implementation = implementation, ...
      )
      
      # Storing weights, used to define set of selected variables
      Beta[rownames(mybeta$weight), colnames(mybeta$weight), k] <- mybeta$weight
      
      # Storing co-membership status
      for (i in 1:dim(mybeta$comembership)[3]) {
        bigstab_obs[s, s, i] <- bigstab_obs[s, s, i] + mybeta$comembership[, , i]
      }
      
      # Storing sampled pairs
      sampled_pairs[s, s] <- sampled_pairs[s, s] + 1
      
      if (verbose) {
        utils::setTxtProgressBar(pb, k / K)
      }
    }
    
    # Computing the selection proportions
    bigstab_var <- matrix(NA, nrow = nrow(Beta), ncol = ncol(Beta))
    colnames(bigstab_var) <- colnames(Beta)
    rownames(bigstab_var) <- rownames(Beta)
    for (i in 1:nrow(Beta)) {
      for (j in 1:ncol(Beta)) {
        bigstab_var[i, j] <- sum(Beta[i, j, ] != 0) / K
      }
    }
    
    # Computing the co-membership proportions
    for (i in 1:dim(bigstab_obs)[3]) {
      bigstab_obs[, , i] <- bigstab_obs[, , i] / sampled_pairs
    }
    bigstab_obs[is.nan(bigstab_obs)] <- NA
  }
  
  # if (PFER_method == "SS") {
  #   for (k in 1:ceiling(K / 2)) {
  #     # Subsampling observations
  #     s <- Resample(
  #       data = xdata, family = NULL, tau = tau, resampling = resampling, ...
  #     )
  #     Xsub <- xdata[s, ]
  #
  #     # Applying clustering algorithm
  #     mybeta1 <- ClusteringAlgo(
  #       xdata = Xsub,
  #       Lambda = Lambda, nc = nc,
  #       implementation = implementation, ...
  #     )
  #
  #     # Complementary subset
  #     ns <- seq(1, nrow(xdata))[!seq(1, nrow(xdata)) %in% s]
  #     Xsub <- xdata[ns, ]
  #     mybeta2 <- ClusteringAlgo(
  #       xdata = Xsub,
  #       Lambda = Lambda, nc = nc,
  #       implementation = implementation, ...
  #     )
  #
  #     # Storing weights, used to define set of selected variables from first set
  #     Beta[rownames(mybeta1$weight), colnames(mybeta1$weight), k] <- mybeta1$weight
  #
  #     # Storing weights, used to define set of selected variables from complementary set
  #     Beta[rownames(mybeta2$weight), colnames(mybeta2$weight), ceiling(K / 2) + k] <- mybeta2$weight
  #
  #     # Storing co-membership status
  #     for (i in 1:dim(mybeta$comembership)[3]) {
  #       bigstab_obs[s, s, i] <- bigstab_obs[s, s, i] + mybeta1$comembership[, , i]
  #       bigstab_obs[ns, ns, i] <- bigstab_obs[ns, ns, i] + mybeta2$comembership[, , i]
  #     }
  #
  #     # Storing sampled pairs
  #     sampled_pairs[s, s] <- sampled_pairs[s, s] + 1
  #     sampled_pairs[ns, ns] <- sampled_pairs[ns, ns] + 1
  #
  #     if (verbose) {
  #       utils::setTxtProgressBar(pb, 2 * k / K)
  #     }
  #   }
  #
  #   # Computing the simultaneous selection proportions
  #   bigstab_var <- matrix(0, nrow = nrow(Beta), ncol = ncol(Beta))
  #   colnames(bigstab_var) <- colnames(Beta)
  #   rownames(bigstab_var) <- rownames(Beta)
  #   for (k in 1:ceiling(K / 2)) {
  #     A1 <- ifelse(Beta[, , k] != 0, yes = 1, no = 0)
  #     A2 <- ifelse(Beta[, , ceiling(K / 2) + k] != 0, yes = 1, no = 0)
  #     A <- A1 + A2
  #     A <- ifelse(A == 2, yes = 1, no = 0)
  #     bigstab_var <- bigstab_var + A
  #   }
  #   bigstab_var <- bigstab_var / ceiling(K / 2)
  #
  #   # Computing the co-membership proportions (cannot do simultaneous as different observations)
  #   for (i in 1:dim(bigstab_obs)[3]) {
  #     bigstab_obs[, , i] <- bigstab_obs[, , i] / sampled_pairs
  #   }
  #   bigstab_obs[is.nan(bigstab_obs)] <- NA
  # }
  
  if (verbose) {
    cat("\n")
  }
  
  # Step 1: calibration for contributing variables
  metrics1 <- StabilityMetrics(
    selprop = bigstab_var, pk = NULL, pi_list = pi_list, K = K, n_cat = n_cat,
    Sequential_template = NULL, graph = FALSE,
    PFER_method = PFER_method, PFER_thr_blocks = PFER_thr, FDP_thr_blocks = FDP_thr
  )
  
  # Step 2: calibration for co-membership
  metrics2 <- StabilityMetrics(
    selprop = bigstab_obs, pk = NULL, pi_list = pi_list, K = K, n_cat = n_cat,
    Sequential_template = NULL, graph = TRUE,
    PFER_method = PFER_method, PFER_thr_blocks = PFER_thr, FDP_thr_blocks = FDP_thr
  )
  
  # # Computation of the stability score over Lambda and pi_list
  # metrics <- StabilityMetrics(
  #   selprop = bigstab_obs, pk = NULL, pi_list = pi_list, K = K, n_cat = n_cat,
  #   Sequential_template = NULL, graph = TRUE,
  #   PFER_method = PFER_method, PFER_thr_blocks = PFER_thr, FDP_thr_blocks = FDP_thr
  # )
  if (verbose) {
    utils::setTxtProgressBar(pb, 1)
    cat("\n")
  }
  
  # Preparing outputs
  myimplementation <- as.character(substitute(implementation, env = parent.frame(n = 2)))
  if (is.function(resampling)) {
    myresampling <- as.character(substitute(resampling))
  } else {
    myresampling <- resampling
  }
  out <- list(
    S = metrics1$S,
    Lambda = cbind(rep(Lambda[, 1], each = nrow(nc))),
    Q = metrics1$Q, Q_s = metrics1$Q_s, P = metrics1$P,
    PFER = metrics1$PFER, FDP = metrics1$FDP,
    S_2d = metrics1$S_2d, PFER_2d = metrics1$PFER_2d, FDP_2d = metrics1$FDP_2d,
    selprop = bigstab_var,
    Beta = Beta,
    Sc = metrics2$S,
    nc = cbind(rep(nc, nrow(Lambda))),
    Sc_2d = metrics2$S_2d,
    coprop = bigstab_obs,
    methods = list(
      type = "clustering", implementation = myimplementation,
      resampling = myresampling, PFER_method = PFER_method
    ),
    params = list(
      K = K, pi_list = pi_list, tau = tau, n_cat = n_cat,
      pk = ncol(xdata), n = nrow(xdata),
      PFER_thr = PFER_thr, FDP_thr = FDP_thr,
      seed = seed
    )
  )
  
  if (output_data) {
    out$params <- c(out$params, list(xdata = xdata))
  }
  
  return(out)
}


CheckInputGraphical <- function(xdata, pk = NULL, Lambda = NULL, lambda_other_blocks = 0.1,
                                pi_list = seq(0.6, 0.9, by = 0.01), K = 100, tau = 0.5, seed = 1, n_cat = 3,
                                implementation = PenalisedGraphical, start = "cold", scale = TRUE,
                                resampling = "subsampling", PFER_method = "MB", PFER_thr = Inf, FDP_thr = Inf,
                                Lambda_cardinal = 50, lambda_max = NULL, lambda_path_factor = 0.0001, max_density = 0.3,
                                verbose = TRUE) {
  # List of arguments
  myargs <- c(
    "xdata", "pk", "Lambda", "lambda_other_blocks",
    "pi_list", "K", "tau", "seed", "n_cat",
    "start", "scale",
    "PFER_method", "PFER_thr", "FDP_thr",
    "Lambda_cardinal",
    "lambda_path_factor", "max_density",
    "verbose"
  )
  
  # Checking the inputs (xdata)
  xdata <- as.matrix(xdata)
  if (sum(is.na(xdata)) > 0) {
    stop("Invalid input for argument 'xdata'. Missing values are not allowed in 'xdata'.")
  }
  if ((nrow(xdata) < 10) | (ncol(xdata) <= 1)) {
    stop("Invalid input for argument 'xdata'. Not enough xdata.")
  }
  
  # Checking the inputs (pk)
  if (!is.null(pk)) {
    pk <- as.numeric(pk)
    if (sum(pk) != ncol(xdata)) {
      stop("Invalid input for argument 'pk'. The number of variables per group 'pk' must sum to the number of columns in 'xdata'.")
    }
  } else {
    pk <- ncol(xdata)
  }
  
  # Checking the inputs (pi_list)
  pi_list <- sort(pi_list)
  if (n_cat == 3) {
    if (any(pi_list > 0.5) & any(pi_list < 1)) {
      if ((min(pi_list) < 0.5) | (max(pi_list) > 1)) {
        warning("The values in 'pi_list' must be between 0.5 and 1. All other values were discarded.")
        pi_list <- pi_list[which((pi_list > 0.5) & (pi_list < 1))]
      }
    } else {
      stop("Invalid input for argument 'pi_list'. The values in the vector must be greater than 0.5 and lower than 1. To consider thresholds below 0.5, argument 'n_cat' must be set to 2.")
    }
  } else {
    if (any(pi_list > 0) & any(pi_list < 1)) {
      if ((min(pi_list) < 0) | (max(pi_list) > 1)) {
        warning("The values in 'pi_list' must be between 0 and 1. All other values were discarded.")
        pi_list <- pi_list[which((pi_list > 0) & (pi_list < 1))]
      }
    } else {
      stop("Invalid input for argument 'pi_list'. The values in the vector must be greater than 0 and lower than 1.")
    }
  }
  
  # Checking the inputs (K)
  K <- as.numeric(K)
  if ((length(K) != 1) | is.na(K)) {
    warning("Invalid input for argument 'K'. The number of resampling iterations 'K' must be a single number.")
    K <- 100
  }
  
  # Checking the inputs (tau)
  tau <- as.numeric(tau)
  if ((length(tau) != 1) | is.na(tau) | (tau >= 1) | (tau <= 0)) {
    warning("Invalid input for argument 'tau'. The subsample size 'tau' must be a number between 0 and 1. The default value (0.5) was used.")
    tau <- 0.5
  }
  
  # Checking the inputs (seed)
  seed <- as.numeric(seed)
  if ((length(seed) != 1) | is.na(seed)) {
    warning("Invalid input for argument 'seed'. The argument 'seed' must be a single number. The default value (1) was used.")
    seed <- 1
  }
  
  # Checking the inputs (n_cat)
  n_cat <- as.numeric(n_cat)
  if ((length(n_cat) != 1) | is.na(n_cat)) {
    warning("Invalid input for argument 'n_cat'. The argument 'seed' must be set to 2 or 3. The default value (3) was used.")
    n_cat <- 3
  }
  
  # Checking the inputs (implementation)
  if (!is.function(implementation)) {
    stop("Invalid input for argument 'implementation'. This argument must be a function to use for graphical modelling.")
  }
  
  # Checking the inputs (start)
  start <- as.character(start)
  if ((length(start) != 1) | is.na(start) | (!start %in% c("cold", "warm"))) {
    warning("Invalid input for argument 'start'. The argument must be 'cold' or 'warm'. The default value (cold) was used.")
  }
  
  # Checking the inputs (scale)
  scale <- as.logical(scale)
  if ((length(scale) != 1) | is.na(scale)) {
    stop("Invalid input for argument 'scale'. The argument 'scale' must be logical (TRUE or FALSE).")
  }
  
  # Checking the inputs (resampling)
  if ((!is.function(resampling)) & (!is.character(resampling))) {
    stop("Invalid input for argument 'resampling'. The argument 'resampling' must be a character string. Possible values are: 'subsampling', 'bootstrap' or the name of a function.")
  }
  
  # Checking the inputs (PFER_method)
  PFER_method <- as.character(PFER_method)
  if ((length(PFER_method) != 1) | (!PFER_method %in% c("MB", "SS"))) {
    stop("Invalid input for argument 'PFER_method'. Possible values are: 'MB' or 'SS'.")
  }
  
  # Checking the inputs (PFER_method and resampling)
  if (is.character(resampling)) {
    if ((PFER_method == "SS") & (resampling == "bootstrap")) {
      warning("Arguments 'resampling' and 'PFER_method' are not compatible. With 'PFER_method' set to 'SS', the resampling is done with complementary pairs of subsamples.")
      resampling <- "subsampling"
    }
  }
  
  # Checking the inputs (lambda_max)
  if (!is.null(lambda_max)) {
    lambda_max <- as.numeric(lambda_max)
    if ((length(lambda_max) != 1) | is.na(lambda_max) | (lambda_max <= 0)) {
      warning("Invalid input for argument 'lambda_max'. The argument 'lambda_max' must be a single positive number. The default value (NULL) was used.")
      lambda_max <- NULL
    }
  }
  
  # Checking the inputs (lambda_path_factor)
  lambda_path_factor <- as.numeric(lambda_path_factor)
  if ((length(lambda_path_factor) != 1) | is.na(lambda_path_factor) | (lambda_path_factor <= 0) | (lambda_path_factor >= 1)) {
    warning("Invalid input for argument 'lambda_path_factor'. The argument 'lambda_path_factor' must be a single number between 0 and 1. The default value (0.0001) was used.")
    lambda_path_factor <- 0.0001
  }
  
  # Checking the inputs (max_density)
  max_density <- as.numeric(max_density)
  if ((length(max_density) != 1) | is.na(max_density) | (max_density <= 0) | (max_density > 1)) {
    warning("Invalid input for argument 'max_density'. The argument 'max_density' must be a single number between 0 and 1. The default value (0.3) was used.")
    max_density <- 0.3
  }
  
  # Checking the inputs (Lambda_cardinal)
  Lambda_cardinal <- as.numeric(Lambda_cardinal)
  if (is.null(Lambda)) {
    if ((length(Lambda_cardinal) != 1) | is.na(Lambda_cardinal) | (Lambda_cardinal < 2)) {
      warning("Invalid input for argument 'Lambda_cardinal'. The argument 'Lambda_cardinal' must be a single positive number. A value of 10 was used.")
      Lambda_cardinal <- 10
    }
  }
  
  # Create matrix with block indices
  bigblocks <- BlockMatrix(pk)
  bigblocks_vect <- bigblocks[upper.tri(bigblocks)]
  N_blocks <- unname(table(bigblocks_vect))
  blocks <- unique(as.vector(bigblocks_vect))
  names(N_blocks) <- blocks
  nblocks <- max(blocks)
  
  # Checking the inputs (lambda_other_blocks in single-block analyses)
  if (!is.null(lambda_other_blocks)) {
    if ((length(pk) == 1)) {
      lambda_other_blocks <- NULL
    } else {
      if (length(lambda_other_blocks) == 1) {
        lambda_other_blocks <- rep(lambda_other_blocks, nblocks)
      } else {
        if (length(lambda_other_blocks) != nblocks) {
          stop(paste0(
            "Invalid input for argument 'lambda_other_blocks'. This argument must be a vector with as many entries as there are blocks in the data (i.e. ",
            nblocks, " entries in this case)."
          ))
        }
      }
    }
  }
  
  # Checking the inputs (verbose)
  verbose <- as.logical(verbose)
  if ((length(verbose) != 1) | is.na(verbose)) {
    warning("Invalid input for argument 'verbose'. The argument 'verbose' must be logical (TRUE or FALSE). The default value (TRUE) was used.")
    verbose <- TRUE
  }
  
  # Checking the inputs (Lambda)
  if (!is.null(Lambda)) {
    if (is.matrix(Lambda)) {
      if ((ncol(Lambda) != nblocks) & (ncol(Lambda) != 1)) {
        stop(paste0("Invalid input for argument 'Lambda'. The argument 'Lambda' must be a matrix as many columns as blocks (N=", nblocks, ")."))
      }
      if (ncol(Lambda) == 1) {
        Lambda <- as.numeric(as.vector(Lambda))
      } else {
        Lambda_copy <- Lambda
        Lambda <- NULL
        for (k in 1:ncol(Lambda_copy)) {
          Lambda <- cbind(Lambda, as.numeric(Lambda_copy[, k]))
        }
      }
    } else {
      Lambda <- as.numeric(Lambda)
    }
    if (any(is.na(Lambda))) {
      if (all(is.na(Lambda))) {
        stop("Invalid input for argument 'Lambda'. The input only contains missing values.")
      } else {
        Lambda <- as.matrix(stats::na.exclude(Lambda))
        warning("Invalid input for argument 'Lambda'. The input contains missing values. These have been excluded.")
      }
    }
  }
  
  # Checking the inputs (PFER_thr)
  PFER_thr <- as.numeric(PFER_thr)
  if ((!length(PFER_thr) %in% c(1, nblocks)) | is.na(PFER_thr) | (PFER_thr <= 0)) {
    warning("Invalid input for argument 'PFER_thr'. The threshold in the upper-bound of the expected number of False Positives 'PFER_thr' must be a vector with positive numbers (or Inf). The default value (Inf) was used.")
    PFER_thr <- Inf
  }
  
  # Checking the inputs (FDP_thr)
  FDP_thr <- as.numeric(FDP_thr)
  if (length(pk) == 1) {
    if ((!length(PFER_thr) %in% c(1, nblocks)) | is.na(FDP_thr) | ((!is.infinite(FDP_thr)) & (FDP_thr <= 0)) | ((!is.infinite(FDP_thr)) & (FDP_thr > 1))) {
      warning("Invalid input for argument 'FDP_thr'. The threshold in the upper-bound of the False Discovery Proportion 'FDP_thr' must be a vector with numbers between 0 and 1 (or Inf to deactivate). The default value (Inf) was used.")
      FDP_thr <- Inf
    }
  }
  
  # Prepare the PFER and FDP thresholds
  if (length(PFER_thr) == 1) {
    PFER_thr_blocks <- ceiling(prop.table(N_blocks) * PFER_thr)
  } else {
    if (length(PFER_thr) == nblocks) {
      PFER_thr_blocks <- PFER_thr
    }
  }
  if (length(FDP_thr) == 1) {
    FDP_thr_blocks <- rep(FDP_thr, nblocks)
  } else {
    if (length(FDP_thr) == nblocks) {
      FDP_thr_blocks <- FDP_thr
    }
  }
  
  # Assigning checked values to the parent function
  for (i in 1:length(myargs)) {
    assign(myargs[i], get(myargs[i]), envir = parent.frame(n = 1))
  }
  
  # Assigning extra objects to the parent function
  myextra <- c("bigblocks", "bigblocks_vect", "blocks", "N_blocks", "nblocks", "PFER_thr_blocks", "FDP_thr_blocks")
  for (i in 1:length(myextra)) {
    assign(myextra[i], get(myextra[i]), envir = parent.frame(n = 1))
  }
}


BlockMatrix <- function(pk) {
  nblocks <- sum(upper.tri(matrix(NA, ncol = length(pk), nrow = length(pk)), diag = TRUE))
  blocks <- matrix(NA, nrow = length(pk), ncol = length(pk))
  blocks[upper.tri(blocks, diag = TRUE)] <- 1:nblocks
  
  mybreaks <- c(0, cumsum(pk))
  bigblocks <- matrix(ncol = sum(pk), nrow = sum(pk))
  row_id_start <- matrix(mybreaks[row(blocks)], ncol = length(pk)) + 1
  row_id_end <- matrix(mybreaks[row(blocks) + 1], ncol = length(pk))
  col_id_start <- matrix(mybreaks[col(blocks)], ncol = length(pk)) + 1
  col_id_end <- matrix(mybreaks[col(blocks) + 1], ncol = length(pk))
  
  row_id_start <- row_id_start[upper.tri(row_id_start, diag = TRUE)]
  row_id_end <- row_id_end[upper.tri(row_id_end, diag = TRUE)]
  col_id_start <- col_id_start[upper.tri(col_id_start, diag = TRUE)]
  col_id_end <- col_id_end[upper.tri(col_id_end, diag = TRUE)]
  
  for (block_id in blocks[upper.tri(blocks, diag = TRUE)]) {
    ids <- rbind(
      expand.grid(
        row_id_start[block_id]:row_id_end[block_id],
        col_id_start[block_id]:col_id_end[block_id]
      ),
      expand.grid(
        col_id_start[block_id]:col_id_end[block_id],
        row_id_start[block_id]:row_id_end[block_id]
      )
    )
    bigblocks[as.matrix(ids)] <- block_id
  }
  
  return(bigblocks)
}


#' Block diagonal matrix
#'
#' Generates a binary block diagonal matrix.
#'
#' @param pk vector encoding the grouping structure.
#'
#' @return A binary block diagonal matrix.
#'
#' @examples
#' # Small example
#' mat <- BlockDiagonal(pk = c(2, 3))
#' @export
BlockDiagonal <- function(pk) {
  bigblocks <- BlockMatrix(pk)
  bigblocks[!bigblocks %in% diag(bigblocks)] <- 0
  bigblocks[bigblocks %in% diag(bigblocks)] <- 1
  return(bigblocks)
}


#' Block structure
#'
#' Generates a symmetric matrix encoding the block structure from the numbers of
#' variables in each group. This function can be used to visualise block IDs.
#'
#' @inheritParams BlockMatrix
#'
#' @return A symmetric matrix of size \code{length(pk))}.
#'
#' @family multi-block functions
#' @seealso \code{\link{GraphicalModel}}
#'
#' @examples
#' # Example with 2 groups
#' mat <- BlockStructure(pk = rep(10, 2))
#'
#' # Example with 5 groups
#' mat <- BlockStructure(pk = rep(10, 5))
#' @export
BlockStructure <- function(pk) {
  nblocks <- sum(upper.tri(matrix(NA, ncol = length(pk), nrow = length(pk)), diag = TRUE))
  blocks <- matrix(NA, nrow = length(pk), ncol = length(pk))
  blocks[upper.tri(blocks, diag = TRUE)] <- 1:nblocks
  blocks[lower.tri(blocks, diag = TRUE)] <- 1:nblocks
  
  return(blocks)
}


#' Multi-block grid
#'
#' Generates a matrix of parameters controlling the sparsity of the underlying
#' selection algorithm for multi-block calibration.
#'
#' @param Lambda vector or matrix of penalty parameters.
#' @param lambda_other_blocks optional vector of penalty parameters to use for
#'   other blocks in the iterative multi-block procedure.
#'
#' @family multi-block functions
#' @seealso \code{\link{GraphicalModel}}
#'
#' @return A list with: \item{Lambda}{a matrix of (block-specific) penalty
#'   parameters. In multi-block stability selection, rows correspond to sets of
#'   penalty parameters and columns correspond to different blocks.}
#'   \item{Sequential_template}{logical matrix encoding the type of procedure
#'   for data with multiple blocks in stability selection graphical modelling.
#'   For multi-block estimation, the procedure is separately calibrating each
#'   block while the others are weakly penalised (\code{TRUE} only for the block
#'   currently being calibrated and \code{FALSE} for other blocks). Other
#'   approaches with joint calibration of the blocks are allowed (all entries
#'   are set to \code{TRUE}).}
#'
#' @examples
#' \dontrun{
#'
#' # Multi-block grid
#' Lambda <- matrix(c(
#'   0.8, 0.6, 0.3,
#'   0.5, 0.4, 0.2,
#'   0.7, 0.5, 0.1
#' ),
#' ncol = 3, byrow = TRUE
#' )
#' mygrid <- BlockLambdaGrid(Lambda, lambda_other_blocks = 0.1)
#'
#' # Multi-parameter grid (not recommended)
#' Lambda <- matrix(c(
#'   0.8, 0.6, 0.3,
#'   0.5, 0.4, 0.2,
#'   0.7, 0.5, 0.1
#' ),
#' ncol = 3, byrow = TRUE
#' )
#' mygrid <- BlockLambdaGrid(Lambda, lambda_other_blocks = NULL)
#' }
#'
#' @export
BlockLambdaGrid <- function(Lambda, lambda_other_blocks = NULL) {
  if ((length(lambda_other_blocks) == 1) & (!is.vector(Lambda))) {
    lambda_other_blocks <- rep(lambda_other_blocks, ncol(Lambda))
  }
  if ((is.null(lambda_other_blocks)) & (!is.vector(Lambda))) {
    Lambda_blocks <- Lambda
    Sequential_template <- matrix(TRUE, ncol = ncol(Lambda), nrow = nrow(Lambda))
  } else {
    # Create Lambda grid matrix with nblocks columns
    if (!is.null(lambda_other_blocks)) {
      nblocks <- length(lambda_other_blocks)
    } else {
      lambda_other_blocks <- 1
      nblocks <- 1
    }
    Lambda_blocks <- NULL
    if (is.vector(Lambda)) {
      Sequential_template <- matrix(FALSE, nrow = nblocks * length(Lambda), ncol = nblocks)
    } else {
      Sequential_template <- matrix(FALSE, nrow = nblocks * nrow(Lambda), ncol = nblocks)
    }
    for (block_id in 1:nblocks) {
      if (!is.vector(Lambda)) {
        tmpLambda <- Lambda[, block_id]
      } else {
        tmpLambda <- Lambda
      }
      Lambda_blocks <- cbind(Lambda_blocks, rep(lambda_other_blocks[block_id], nblocks * length(tmpLambda)))
      Lambda_blocks[(length(tmpLambda) * (block_id - 1) + 1):(length(tmpLambda) * (block_id)), block_id] <- tmpLambda
      Sequential_template[(length(tmpLambda) * (block_id - 1) + 1):(length(tmpLambda) * (block_id)), block_id] <- TRUE
    }
  }
  
  return(list(Lambda = Lambda_blocks, Sequential_template = Sequential_template))
}



Resample <- function(data, family = NULL, tau = 0.5, resampling = "subsampling", ...) {
  # Preparing the data
  if (is.vector(data)) {
    data <- matrix(data, ncol = 1)
  }
  
  # if (!resampling %in% c("subsampling", "bootstrap")) {
  if (is.function(resampling)) {
    # s <- do.call(get(resampling), args = list(data = data, tau = tau, ...))
    s <- do.call(resampling, args = list(data = data, tau = tau, ...))
  } else {
    if (!resampling %in% c("subsampling", "bootstrap")) {
      stop("Invalid input for argument 'resampling'. It must be a function or a character string: 'subsampling' or 'bootstrap'.")
    } else {
      # Using or not replacement in resampling
      replacement <- ifelse(resampling == "subsampling", yes = FALSE, no = TRUE)
      
      # Definition of the size of sub/bootstrap sample
      if (replacement) {
        tau <- 1
      }
      
      # Resampling procedure
      if (!is.null(family)) {
        # Resampling for regression models
        if (family %in% c("gaussian", "poisson", "mgaussian")) {
          s <- sample(nrow(data), size = tau * nrow(data), replace = replacement)
        }
        if (family == "binomial") {
          if (ncol(data) > 1) {
            data <- cbind(apply(data, 1, sum)) # to ensure balanced classes for PLS-DA
          }
          s <- NULL
          for (mycat in levels(factor(data))) {
            scat <- sample(which(data == mycat), size = tau * sum(data == mycat), replace = replacement)
            s <- c(s, scat)
          }
        }
        if (family == "multinomial") {
          s <- NULL
          for (mycat in levels(factor(data))) {
            scat <- sample(which(data == mycat), size = tau * sum(data == mycat), replace = replacement)
            s <- c(s, scat)
          }
        }
        if (family == "cox") {
          s0 <- sample(which(data[, 2] == "0"), size = tau * sum(data[, 2] == "0"), replace = replacement)
          s1 <- sample(which(data[, 2] == "1"), size = tau * sum(data[, 2] == "1"), replace = replacement)
          s <- c(s0, s1)
        }
      } else {
        # Resampling for network models
        s <- sample(1:nrow(data), size = tau * nrow(data), replace = replacement)
      }
    }
  }
  return(s)
}


ClusteringAlgo <- function(xdata,
                           Lambda = NULL, nc,
                           implementation = HierarchicalClustering, ...) {
  # Making sure none of the variables has a null standard deviation
  mysd <- rep(NA, ncol(xdata))
  for (j in 1:ncol(xdata)) {
    mysd[j] <- stats::sd(xdata[, j])
  }
  if (any(mysd == 0)) {
    for (k in which(mysd == 0)) {
      xdata[, k] <- xdata[, k] + stats::rnorm(n = nrow(xdata), sd = min(mysd[mysd != 0]) / 100)
    }
  }
  
  # Applying user-defined function for variable selection
  out <- do.call(implementation, args = list(xdata = xdata, nc = nc, Lambda = Lambda, ...))
  
  if ("weight" %in% names(out)) {
    beta_full <- out$weight
    
    # Setting the beta coefficient to zero for predictors with always the same value (null standard deviation)
    if (any(mysd == 0)) {
      selected[, which(mysd == 0)] <- 0
      if (length(dim(beta_full)) == 2) {
        beta_full[, which(mysd == 0)] <- 0
      }
      if (length(dim(beta_full)) == 3) {
        beta_full[, which(mysd == 0), ] <- 0
      }
    }
  } else {
    beta_full <- matrix(1, nrow = length(nc), ncol = ncol(xdata))
    rownames(beta_full) <- paste0("s", seq(0, nrow(beta_full) - 1))
    colnames(beta_full) <- colnames(xdata)
  }
  
  return(list(comembership = out$comembership, weight = beta_full))
}

# Consensus clustering based on k-proto clustering
stab <- Clustering(
  xdata = stab_df_final,
  implementation = KprotoClustering)
#' CalibrationPlot(stab, xlab = expression(italic(k)), clustering = TRUE)
#' plot(Graph(Adjacency(stab), node_colour = simul$theta, satellites = TRUE))
#' ClusteringPerformance(theta = Clusters(stab), theta_star = simul$theta)

final_df_un_2 <- drop_na(final_df_un)


#Load dataset
stab_df = read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df_all_processed.csv")

#Drop 1st row
stab_df = subset(stab_df, select = -c(X))

#Set Subject.ID as index
stab_df2 <- stab_df[-1]
row.names(stab_df2) <- stab_df$Unnamed..0

#Create an index of your original categorical variables
index_cat <- c(181:361)
stab_df2[,index_cat] <- lapply(stab_df2[ ,index_cat], as.factor)

num_cat <- c(1:180)
stab_df2[,num_cat] <- lapply(stab_df2[ ,num_cat], as.numeric)

#Select numerical variables
num_stab_df2 <- stab_df2 %>%
  select_if(Negate(is.factor))

#Select categorical variables
cat_stab_df2<- stab_df2 %>%
  select_if(Negate(is.numeric))

#Combine cat and num dataframes before running kproto
stab_df_final <- cbind(num_stab_df2, cat_stab_df2)
# we drop all variables that have 0 variance when data is scaled
stab_df_final = subset(stab_df_final, select = -c(Omics.Furoylglycine,Omics.Glutamic.acid,Omics.N.Acetylglutamic.acid,
                                                      Omics.Phenylalanine,Omics.Allantoin,Omics.Isoleucine,Omics.N.Acetylputrescine,
                                                      Omics.Sarcosine,Omics.Cytosine,Omics.Lysine,Omics.N.Methyl.D.aspartic.acid,
                                                      Omics.Xanthosine,Omics.Glucosamine,Omics.Maltose,Omics.O.Acetylserine,
                                                      Omics.Xylose))

stab_df_final = subset(stab_df_final, select = -c(Healthy,cohort,cluster_K_2,cluster_K_3,cluster_K_4,cluster_K_5,cluster_K_6,cluster_K_7,cluster_K_8,
                                      cluster_K_9,cluster_K_10,cluster_K_11,cluster_K_12,cluster_K_13,cluster_K_14,cluster_K_15,
                                      MildModerate,Severe,Severe_asthma,Severe_Smoker))

View(sapply(stab_df_final,class))


#Running k-proto algo
myclust <- ClusteringAlgo(
xdata = stab_df_final,
 nc = 2:5, Lambda = c(1.5, 2), na.rm = FALSE,
implementation = KprotoClustering
 )









KMeansClustering <- function(xdata, nc = NULL, scale = TRUE, rows = TRUE, ...) {
  # Storing extra arguments
  extra_args <- list(...)
  
  # Transposing for clustering of columns
  if (!rows) {
    xdata <- t(xdata)
  }
  
  # Scaling the data
  if (scale) {
    xdata <- scale(xdata)
  }
  
  # Re-formatting nc
  if (!is.null(nc)) {
    if (is.vector(nc)) {
      nc <- cbind(nc)
    }
  } else {
    nc <- cbind(seq(1, nrow(xdata)))
  }
  
  # Initialisation of array storing co-membership matrices
  adjacency <- array(0, dim = c(nrow(xdata), nrow(xdata), nrow(nc)))
  
  # Extracting relevant extra arguments (kmeans)
  ids <- which(names(extra_args) %in% names(formals(stats::kmeans)))
  ids <- ids[!ids %in% c("x", "centers")]
  
  # Running k-means clustering
  for (k in 1:nrow(nc)) {
    if (nc[k, 1] == 1) {
      adjacency[, , k] <- CoMembership(groups = rep(1, nrow(xdata)))
    } else {
      if (nc[k, 1] < nrow(xdata)) {
        myclust <- do.call(stats::kmeans, args = c(list(x = xdata, centers = nc[k, 1]), extra_args[ids]))
        mygroups <- myclust$cluster
        adjacency[, , k] <- CoMembership(groups = mygroups)
      }
    }
  }
  
  return(list(comembership = adjacency))
}


#' Simulation of data with underlying graph structure
#'
#' Simulates (i) a graph, and (ii) multivariate Normal data for which the graph
#' structure is encoded in the nonzero entries of the true partial correlation
#' matrix. This procedure ensures that the conditional independence structure
#' between the variables is encoded in the simulated graph. The outputs of this
#' function can be used to evaluate the ability of a graphical model to identify
#' edges of a conditional independence graph.
#'
#' @param n number of observations in the simulated data.
#' @param pk vector of the number of variables per group in the simulated data.
#'   The number of nodes in the simulated graph is \code{sum(pk)}. With multiple
#'   groups, the simulated (partial) correlation matrix has a block structure,
#'   where blocks arise from the integration of the \code{length(pk)} groups.
#'   This argument is only used if \code{sum(pk)} is equal to the number of
#'   rows/columns in \code{theta} is not provided.
#' @param theta optional binary and symmetric adjacency matrix encoding the
#'   conditional independence structure.
#' @param implementation function for simulation of the graph. By default,
#'   algorithms implemented in \code{\link[huge]{huge.generator}} are used.
#'   Alternatively, a user-defined function can be used. It must take \code{pk},
#'   \code{topology} and \code{nu} as arguments and return a
#'   \code{(sum(pk)*(sum(pk)))} binary and symmetric matrix for which diagonal
#'   entries are all equal to zero. This function is only applied if
#'   \code{theta} is not provided.
#' @param topology topology of the simulated graph. If using
#'   \code{implementation=HugeAdjacency}, possible values are listed for the
#'   argument \code{graph} of \code{\link[huge]{huge.generator}}. These are:
#'   "random", "hub", "cluster", "band" and "scale-free".
#' @param nu_within expected density (number of edges over the number of node
#'   pairs) of within-group blocks in the graph. If \code{length(pk)=1}, this is
#'   the expected density of the graph. If \code{implementation=HugeAdjacency},
#'   this argument is only used for \code{topology="random"} or
#'   \code{topology="cluster"} (see argument \code{prob} in
#'   \code{\link[huge]{huge.generator}}).
#' @param nu_between expected density (number of edges over the number of node
#'   pairs) of between-group blocks in the graph. Similar to \code{nu_within}.
#'   By default, the same density is used for within and between blocks
#'   (\code{nu_within}=\code{nu_between}). Only used if \code{length(pk)>1}.
#' @param output_matrices logical indicating if the true precision and (partial)
#'   correlation matrices should be included in the output.
#' @param v_within vector defining the (range of) nonzero entries in the
#'   diagonal blocks of the precision matrix. These values must be between -1
#'   and 1 if \code{pd_strategy="min_eigenvalue"}. If \code{continuous=FALSE},
#'   \code{v_within} is the set of possible precision values. If
#'   \code{continuous=TRUE}, \code{v_within} is the range of possible precision
#'   values.
#' @param v_between vector defining the (range of) nonzero entries in the
#'   off-diagonal blocks of the precision matrix. This argument is the same as
#'   \code{v_within} but for off-diagonal blocks. It is only used if
#'   \code{length(pk)>1}.
#' @param v_sign vector of possible signs for precision matrix entries. Possible
#'   inputs are: \code{-1} for positive partial correlations, \code{1} for
#'   negative partial correlations, or \code{c(-1, 1)} for both positive and
#'   negative partial correlations.
#' @param continuous logical indicating whether to sample precision values from
#'   a uniform distribution between the minimum and maximum values in
#'   \code{v_within} (diagonal blocks) or \code{v_between} (off-diagonal blocks)
#'   (\code{continuous=TRUE}) or from proposed values in \code{v_within}
#'   (diagonal blocks) or \code{v_between} (off-diagonal blocks)
#'   (\code{continuous=FALSE}).
#' @param pd_strategy method to ensure that the generated precision matrix is
#'   positive definite (and hence can be a covariance matrix). With
#'   \code{pd_strategy="diagonally_dominant"}, the precision matrix is made
#'   diagonally dominant by setting the diagonal entries to the sum of absolute
#'   values on the corresponding row and a constant u. With
#'   \code{pd_strategy="min_eigenvalue"}, diagonal entries are set to the sum of
#'   the absolute value of the smallest eigenvalue of the precision matrix with
#'   zeros on the diagonal and a constant u.
#' @param ev expected proportion of explained variance by the first Principal
#'   Component (PC1) of a Principal Component Analysis. This is the largest
#'   eigenvalue of the correlation (if \code{scale=TRUE}) or covariance (if
#'   \code{scale=FALSE}) matrix divided by the sum of eigenvalues. If
#'   \code{ev=NULL} (the default), the constant u is chosen by maximising the
#'   contrast of the correlation matrix.
#' @param scale logical indicating if the proportion of explained variance by
#'   PC1 should be computed from the correlation (\code{scale=TRUE}) or
#'   covariance (\code{scale=FALSE}) matrix. If \code{scale=TRUE}, the
#'   correlation matrix is used as parameter of the multivariate normal
#'   distribution.
#' @param u_list vector with two numeric values defining the range of values to
#'   explore for constant u.
#' @param tol accuracy for the search of parameter u as defined in
#'   \code{\link[stats]{optimise}}.
#' @param ... additional arguments passed to the graph simulation function
#'   provided in \code{implementation}.
#'
#' @seealso \code{\link{SimulatePrecision}}, \code{\link{MakePositiveDefinite}},
#'   \code{\link{Contrast}}, \code{\link{GraphicalModel}}
#' @family simulation functions
#'
#' @return A list with: \item{data}{simulated data with \code{n} observation and
#'   \code{sum(pk)} variables.} \item{theta}{adjacency matrix of the simulated
#'   graph} \item{omega}{simulated (true) precision matrix. Only returned if
#'   \code{output_matrices=TRUE}.} \item{phi}{simulated (true) partial
#'   correlation matrix. Only returned if \code{output_matrices=TRUE}.}
#'   \item{sigma}{ simulated (true) covariance matrix. Only returned if
#'   \code{output_matrices=TRUE}.} \item{u}{value of
#'   the constant u used for the simulation of \code{omega}. Only returned if
#'   \code{output_matrices=TRUE}.}
#'
#' @examples
#' \dontrun{
#'
#' # Simulation of random graph with 50 nodes
#' set.seed(1)
#' simul <- SimulateGraphical(n = 100, pk = 50, topology = "random", nu_within = 0.05)
#' print(simul)
#' plot(simul)
#'
#' # Simulation of scale-free graph with 20 nodes
#' set.seed(1)
#' simul <- SimulateGraphical(n = 100, pk = 20, topology = "scale-free")
#' plot(simul)
#'
#' # Extracting true precision/correlation matrices
#' set.seed(1)
#' simul <- SimulateGraphical(
#'   n = 100, pk = 20,
#'   topology = "scale-free", output_matrices = TRUE
#' )
#' str(simul)
#'
#' # Simulation of multi-block data
#' set.seed(1)
#' pk <- c(20, 30)
#' simul <- SimulateGraphical(
#'   n = 100, pk = pk,
#'   nu_within = 0.05, nu_between = 0.05,
#'   pd_strategy = "min_eigenvalue"
#' )
#' mycor <- cor(simul$data)
#' Heatmap(mycor,
#'   colours = c("darkblue", "white", "firebrick3"),
#'   legend_range = c(-1, 1), legend_length = 50,
#'   legend = FALSE, axes = FALSE
#' )
#' for (i in 1:2) {
#'   axis(side = i, at = c(0.5, pk[1] - 0.5), labels = NA)
#'   axis(
#'     side = i, at = mean(c(0.5, pk[1] - 0.5)),
#'     labels = ifelse(i == 1, yes = "Group 1", no = "Group 2"),
#'     tick = FALSE, cex.axis = 1.5
#'   )
#'   axis(side = i, at = c(pk[1] + 0.5, sum(pk) - 0.5), labels = NA)
#'   axis(
#'     side = i, at = mean(c(pk[1] + 0.5, sum(pk) - 0.5)),
#'     labels = ifelse(i == 1, yes = "Group 2", no = "Group 1"),
#'     tick = FALSE, cex.axis = 1.5
#'   )
#' }
#'
#' # User-defined function for graph simulation
#' CentralNode <- function(pk, hub = 1) {
#'   theta <- matrix(0, nrow = sum(pk), ncol = sum(pk))
#'   theta[hub, ] <- 1
#'   theta[, hub] <- 1
#'   diag(theta) <- 0
#'   return(theta)
#' }
#' simul <- SimulateGraphical(n = 100, pk = 10, implementation = CentralNode)
#' plot(simul) # star
#' simul <- SimulateGraphical(n = 100, pk = 10, implementation = CentralNode, hub = 2)
#' plot(simul) # variable 2 is the central node
#'
#' # User-defined adjacency matrix
#' mytheta <- matrix(c(
#'   0, 1, 1, 0,
#'   1, 0, 0, 0,
#'   1, 0, 0, 1,
#'   0, 0, 1, 0
#' ), ncol = 4, byrow = TRUE)
#' simul <- SimulateGraphical(n = 100, theta = mytheta)
#' plot(simul)
#'
#' # User-defined adjacency and block structure
#' simul <- SimulateGraphical(n = 100, theta = mytheta, pk = c(2, 2))
#' mycor <- cor(simul$data)
#' Heatmap(mycor,
#'   colours = c("darkblue", "white", "firebrick3"),
#'   legend_range = c(-1, 1), legend_length = 50, legend = FALSE
#' )
#' }
#' @export
SimulateGraphical <- function(n = 100, pk = 10, theta = NULL,
                              implementation = HugeAdjacency, topology = "random",
                              nu_within = 0.1, nu_between = NULL,
                              v_within = c(0.5, 1), v_between = c(0, 0.1),
                              v_sign = c(-1, 1), continuous = TRUE,
                              pd_strategy = "diagonally_dominant", ev = NULL, scale = TRUE,
                              u_list = c(1e-10, 1), tol = .Machine$double.eps^0.25,
                              output_matrices = FALSE, ...) {
  # Defining number of nodes
  p <- sum(pk)
  if (!is.null(theta)) {
    if (ncol(theta) != p) {
      p <- pk <- ncol(theta)
    }
  }
  
  # Defining the between-block density
  if (is.null(nu_between)) {
    nu_between <- nu_within
  }
  
  # Building adjacency matrix
  if (is.null(theta)) {
    theta <- SimulateAdjacency(
      pk = pk,
      implementation = implementation, topology = topology,
      nu_within = nu_within, nu_between = nu_between, ...
    )
  }
  
  # Simulation of a precision matrix
  out <- SimulatePrecision(
    pk = pk, theta = theta,
    v_within = v_within, v_between = v_between,
    v_sign = v_sign, continuous = continuous,
    pd_strategy = pd_strategy, ev = ev, scale = scale,
    u_list = u_list, tol = tol
  )
  omega <- out$omega
  
  # Computing the covariance matrix
  if (scale) {
    sigma <- stats::cov2cor(solve(omega))
  } else {
    sigma <- solve(omega)
  }
  
  # Computing the partial correlation matrix
  if (output_matrices) {
    phi <- -stats::cov2cor(omega) + 2 * diag(ncol(omega))
  }
  
  # Simulating data from multivariate normal distribution
  x <- MASS::mvrnorm(n, rep(0, p), sigma)
  colnames(x) <- paste0("var", 1:ncol(x))
  rownames(x) <- paste0("obs", 1:nrow(x))
  
  if (output_matrices) {
    out <- list(
      data = x, theta = theta,
      omega = omega, phi = phi, sigma = sigma,
      u = out$u
    )
  } else {
    out <- list(data = x, theta = theta)
  }
  
  # Defining the class
  class(out) <- "simulation_graphical_model"
  
  return(out)
}


#' Simulation of data with underlying clusters
#'
#' Simulates mixture multivariate Normal data with clusters of observations
#' (rows) sharing similar profiles along (a subset of) variables (columns). The
#' conditional independence structure between the variables can be simulated or
#' provided in argument \code{adjacency}. The same covariance is used across all
#' clusters. Independent variables are simulated by default
#' (\code{nu_within=0}).
#'
#' @inheritParams SimulateGraphical
#' @param n vector of the number of observations per cluster in the simulated
#'   data. The number of observations in the simulated data is \code{sum(n)}.
#' @param pk vector of the number of variables in the simulated data.
#' @param adjacency optional binary and symmetric adjacency matrix encoding the
#'   conditional independence structure between variables.
#' @param theta_xc optional binary vector encoding which variables (columns)
#'   contribute to the clustering structure between observations (rows).
#' @param nu_xc expected proportion of variables contributing to the clustering
#'   over the total number of variables. This argument is only used if
#'   \code{theta_xc} is not provided.
#' @param ev_xc vector of marginal expected proportion of explained for each
#'   variable contributing to the clustering. This parameter is only used for
#'   variables with a nonzero entry in \code{theta_xc}.
#' @param ev_xx expected proportion of explained variance by the first Principal
#'   Component (PC1) of a Principal Component Analysis applied on the
#'   predictors. This is the largest eigenvalue of the correlation (if
#'   \code{scale=TRUE}) or covariance (if \code{scale=FALSE}) matrix divided by
#'   the sum of eigenvalues. If \code{ev=NULL} (the default), the constant u is
#'   chosen by maximising the contrast of the correlation matrix.
#'
#' @seealso \code{\link{MakePositiveDefinite}}, \code{\link{GraphicalModel}}
#' @family simulation functions
#'
#' @return A list with: \item{data}{simulated data with \code{sum(n)}
#'   observation and \code{sum(pk)} variables} \item{theta}{simulated (true)
#'   cluster membership.} \item{theta}{adjacency matrix of the graph encoding
#'   the conditional independence structure between variables.}
#'   \item{theta_xc}{binary vector encoding variables contributing to the
#'   clustering structure.} \item{ev}{vector of marginal expected proportions of
#'   explained variance for each variable.}
#'
#' @examples
#' \dontrun{
#' ## Example with 3 clusters
#'
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateClustering(
#'   n = c(10, 30, 15), ev_xc = 0.95
#' )
#' print(simul)
#' plot(simul)
#'
#'
#' ## Example with 2 variables contributing to clustering
#'
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateClustering(
#'   n = c(200, 100, 150), pk = 10,
#'   theta_xc = c(1, 1, rep(0, 8))
#' )
#' print(simul)
#'
#' # Visualisation of the data
#' par(mar = c(5, 5, 5, 5))
#' Heatmap(
#'   mat = simul$data,
#'   colours = c("navy", "white", "red")
#' )
#' simul$ev # marginal proportions of explained variance
#'
#' # Visualisation along contributing variables
#' plot(simul$data[, 1:2], col = simul$theta)
#'
#'
#' ## Example with more distinct clusters
#'
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateClustering(
#'   n = c(200, 100, 150), pk = 10,
#'   theta_xc = c(1, 1, rep(0, 8)),
#'   ev_xc = c(0.9, 0.8, rep(0, 8))
#' )
#'
#' # Visualisation along contributing variables
#' plot(simul$data[, 1:2], col = simul$theta)
#'
#'
#' ## Example with correlated contributors
#'
#' # Data simulation
#' pk <- 10
#' adjacency <- matrix(0, pk, pk)
#' adjacency[1, 2] <- adjacency[2, 1] <- 1
#' set.seed(1)
#' simul <- SimulateClustering(
#'   n = c(200, 100, 150), pk = pk,
#'   theta_xc = c(1, 1, rep(0, 8)),
#'   ev_xc = c(0.9, 0.8, rep(0, 8)),
#'   adjacency = adjacency,
#'   pd_strategy = "min_eigenvalue",
#'   v_within = 0.6, v_sign = -1
#' )
#'
#' # Visualisation along contributing variables
#' plot(simul$data[, 1:2], col = simul$theta)
#'
#' # Checking marginal proportions of explained variance
#' mymodel <- lm(simul$data[, 1] ~ as.factor(simul$theta))
#' summary(mymodel)$r.squared
#' mymodel <- lm(simul$data[, 2] ~ as.factor(simul$theta))
#' summary(mymodel)$r.squared
#' }
#' @export
SimulateClustering <- function(n = c(10, 10), pk = 10, adjacency = NULL,
                               theta_xc = NULL, nu_xc = 0.1, ev_xc = NULL,
                               implementation = HugeAdjacency, topology = "random",
                               nu_within = 0, nu_between = NULL,
                               v_within = c(0.5, 1), v_between = c(0, 0.1),
                               v_sign = c(-1, 1), continuous = TRUE,
                               pd_strategy = "diagonally_dominant", ev_xx = NULL, scale = TRUE,
                               u_list = c(1e-10, 1), tol = .Machine$double.eps^0.25,
                               output_matrices = FALSE) {
  # Using multi-block simulator with unconnected blocks
  out <- SimulateGraphical(
    n = sum(n), pk = pk, theta = adjacency,
    implementation = implementation,
    topology = topology,
    nu_within = nu_within,
    nu_between = nu_between,
    output_matrices = output_matrices,
    v_within = v_within,
    v_between = v_between,
    continuous = continuous,
    pd_strategy = pd_strategy, ev = ev_xx, scale = scale,
    u_list = u_list, tol = tol
  )
  
  # Defining number of clusters
  nc <- length(n)
  
  # Defining variables contributing to the clustering
  if (is.null(theta_xc)) {
    theta_xc <- SamplePredictors(pk = sum(pk), q = 1, nu = nu_xc, orthogonal = TRUE)[, 1]
  }
  
  # Simulating marginal proportions of explained variance
  if (is.null(ev_xc)) {
    ev_xc <- stats::runif(n = sum(pk))
  } else {
    if (length(ev_xc) == 1) {
      ev_xc <- rep(ev_xc, sum(pk))
    }
  }
  
  # Re-naming the outputs
  out$adjacency <- out$theta
  
  # Definition of membership
  theta <- NULL
  for (i in 1:length(n)) {
    theta <- c(theta, rep(i, each = n[i]))
  }
  names(theta) <- rownames(out$data)
  out$theta <- theta
  
  # Building binary cluster membership for each feature
  V <- stats::model.matrix(~ as.factor(theta) - 1)
  
  # Simulating the cluster-specific means
  mu_mat <- matrix(NA, nrow = sum(n), ncol = sum(pk))
  for (k in 1:ncol(mu_mat)) {
    # Defining variance to reach expected proportion of e.v.
    var_mu <- ev_xc[k] * 1 / (1 - ev_xc[k])
    
    # Sampling initial values for cluster-specific means
    mu <- stats::rnorm(n = nc, mean = 0, sd = 1)
    for (i in 1:nrow(mu_mat)) {
      mu_mat[i, k] <- mu[theta[i]]
    }
    
    # Scaling to ensure mean of zero and defined variance
    mu_mat[, k] <- scale(mu_mat[, k])
    mu_mat[, k] <- mu_mat[, k] * sqrt(var_mu)
  }
  
  # Using cluster-specific mean for contributing variables
  # TODO: check impact of using non-scaled data here?
  for (k in 1:ncol(mu_mat)) {
    if (theta_xc[k] == 1) {
      out$data[, k] <- out$data[, k] + mu_mat[, k]
    }
  }
  
  # Definition of contributing variables
  names(theta_xc) <- colnames(out$data)
  out$theta_xc <- theta_xc
  out$ev <- ev_xc * theta_xc
  
  # Defining the class
  class(out) <- "simulation_clustering"
  
  return(out)
}


#' Simulation of sparse orthogonal components
#'
#' Simulates variables following a multivariate Normal distribution that could
#' be obtained from a sparse linear combination of orthogonal latent variables.
#' This generates blocks of mutually independent variables, where all variables
#' from a block can be obtained from a linear combination of the same latent
#' variables. The latent variables would correspond to Principal Components from
#' a sparse Principal Component Analysis. The loadings coefficients, their
#' support, and the proportions of explained variance by each of the latent
#' variables are returned. This function can be used to evaluate the performance
#' of sparse Principal Component Analysis algorithms.
#'
#' @inheritParams SimulateGraphical
#' @param adjacency optional binary and symmetric adjacency matrix encoding the
#'   conditional graph structure between observations. The clusters encoded in
#'   this argument must be in line with those indicated in \code{pk}. Edges in
#'   off-diagonal blocks are not allowed to ensure that the simulated orthogonal
#'   components are sparse. Corresponding entries in the precision matrix will
#'   be set to zero.
#'
#' @details The data is simulated from a centered multivariate Normal
#'   distribution with a block-diagonal covariance matrix. Independence between
#'   variables from the different blocks ensures that sparse orthogonal
#'   components can be generated. The block-diagonal (partial) correlation
#'   matrix is obtained using a graph structure encoding the conditional
#'   independence between variables. The orthogonal latent variables are
#'   obtained from eigendecomposition of the true correlation matrix. The sparse
#'   eigenvectors contain the weights of the linear combination of variables to
#'   construct the latent variable (loadings coefficients). The proportion of
#'   explained variance by each of the latent variable is computed from
#'   eigenvalues. As latent variables are defined from the true correlation
#'   matrix, the number of sparse orthogonal components is not limited by the
#'   number of observations and is equal to \code{sum(pk)}.
#'
#' @return A list with: \item{data}{simulated data with \code{n} observation and
#'   \code{sum(pk)} variables.} \item{loadings}{loadings coefficients of the
#'   orthogonal latent variables (principal components).} \item{theta}{support
#'   of the loadings coefficients.} \item{ev}{proportion of explained variance
#'   by each of the orthogonal latent variables.} \item{adjacency}{adjacency
#'   matrix of the simulated graph.} \item{omega}{simulated (true) precision
#'   matrix. Only returned if \code{output_matrices=TRUE}.} \item{phi}{simulated
#'   (true) partial correlation matrix. Only returned if
#'   \code{output_matrices=TRUE}.} \item{C}{ simulated (true) correlation
#'   matrix. Only returned if \code{output_matrices=TRUE}.}
#'
#' @seealso \code{\link{MakePositiveDefinite}}, \code{\link{GraphicalModel}}
#' @family simulation functions
#'
#' @examples
#' \dontrun{
#' # Simulation of 3 components with high e.v.
#' set.seed(1)
#' simul <- SimulateComponents(pk = c(5, 3, 4), ev = 0.4)
#' print(simul)
#' plot(simul)
#' plot(cumsum(simul$ev), ylim = c(0, 1), las = 1)
#'
#' # Simulation of 3 components with moderate e.v.
#' set.seed(1)
#' simul <- SimulateComponents(pk = c(5, 3, 4), ev = 0.25)
#' print(simul)
#' plot(simul)
#' plot(cumsum(simul$ev), ylim = c(0, 1), las = 1)
#'
#' # Simulation of multiple components with low e.v.
#' pk <- sample(3:10, size = 5, replace = TRUE)
#' simul <- SimulateComponents(
#'   pk = pk,
#'   nu_within = 0.3, v_within = c(0.8, 0.5), v_sign = -1, ev = 0.1
#' )
#' plot(simul)
#' plot(cumsum(simul$ev), ylim = c(0, 1), las = 1)
#' }
#' @export
SimulateComponents <- function(n = 100, pk = c(10, 10),
                               adjacency = NULL, nu_within = 1,
                               v_within = c(0.5, 1), v_sign = -1, continuous = TRUE,
                               pd_strategy = "min_eigenvalue", ev = 0.1, scale = TRUE,
                               u_list = c(1e-10, 1), tol = .Machine$double.eps^0.25,
                               output_matrices = FALSE) {
  # Using multi-block simulator with unconnected blocks
  out <- SimulateGraphical(
    n = n, pk = pk, theta = adjacency,
    implementation = HugeAdjacency,
    topology = "random",
    nu_within = nu_within, # fully connected components by default
    nu_between = 0, # need unconnected blocks
    v_within = v_within,
    v_between = 0,
    v_sign = v_sign,
    continuous = continuous,
    pd_strategy = pd_strategy, ev = ev, scale = scale,
    u_list = u_list, tol = tol,
    output_matrices = TRUE
  )
  
  # Eigendecomposition of the covariance
  eig <- eigen(out$sigma)
  
  # Definition of membership
  membership <- NULL
  for (i in 1:length(pk)) {
    membership <- c(membership, rep(i, each = pk[i]))
  }
  names(membership) <- colnames(out$data)
  out$membership <- membership
  
  # Re-naming the outputs
  out$adjacency <- out$theta
  
  # Definition of sparse principal components
  out$loadings <- round(eig$vectors, digits = 10)
  out$theta <- ifelse(out$loadings != 0, yes = 1, no = 0)
  rownames(out$theta) <- rownames(out$loadings) <- colnames(out$adjacency)
  colnames(out$theta) <- colnames(out$loadings) <- paste0("PC", 1:ncol(out$theta))
  
  # Definition of proportion of explained variance
  ev <- eig$values / sum(eig$values)
  names(ev) <- colnames(out$theta)
  out$ev <- ev
  
  # Re-arranging the output
  out <- out[c("data", "loadings", "theta", "ev", "membership", "omega", "phi", "C", "u")]
  if (!output_matrices) {
    out <- out[c("data", "loadings", "theta", "ev", "membership")]
  }
  
  # Defining the class
  class(out) <- "simulation_components"
  
  return(out)
}


#' Simulation of predictors and associated outcome
#'
#' Simulates (i) a matrix \code{xdata} of \code{n} observations for
#' \code{sum(pk)} normally distributed predictor variables, (ii) a matrix
#' \code{zdata} of \code{length(pk)} orthogonal latent variables, and (iii) a
#' matrix \code{ydata} of \code{length(pk)} outcome variables. The conditional
#' independence structure between the predictors and latent variables is encoded
#' in a precision matrix, where the diagonal entries corresponding to latent
#' variables are tuned to reach a user-defined expected proportion of explained
#' variance. To ensure that latent variables are orthogonal (these can be
#' interpreted as the Principal Components of a Partial Least Squares model),
#' the predictors contributing to their definition are taken from independent
#' blocks of variables. The outcome variables are then obtained from a linear
#' combination of the latent variables. The outputs of this function can be used
#' to evaluate the ability of variable selection algorithms to identify, among
#' the variables in \code{xdata}, relevant predictors of the outcome variables
#' in \code{ydata}.
#'
#' @inheritParams SimulateGraphical
#' @param pk vector with the number of predictors in each independent block of
#'   variables in \code{xdata}. The number of independent blocks, which
#'   determines the maximum number of orthogonal latent variables that can be
#'   simulated, is given by \code{length(pk)}.
#' @param family type of outcome. If \code{family="gaussian"}, normally
#'   distributed outcomes are simulated. If \code{family="binomial"} or
#'   \code{family="multinomial"}, binary outcome(s) are simulated from a
#'   multinomial distribution where the probability is defined from a linear
#'   combination of normally distributed outcomes.
#' @param N number of classes of the categorical outcome. Only used if
#'   \code{family="multinomial"}.
#' @param ev_xz vector of the expected proportions of explained variances for each
#'   of the orthogonal latent variables. It must contain values in ]0,1[, and
#'   must be a vector of length \code{length(pk)} or a single value to generate
#'   latent variables with the same expected proportion of explained variance.
#' @param adjacency_x optional matrix encoding the conditional independence
#'   structure between predictor variables in \code{xdata}. This argument must
#'   be a binary symmetric matrix of size \code{sum(pk)} with zeros on the
#'   diagonal.
#' @param nu_within expected density (number of edges over the number of node
#'   pairs) of the conditional independence graph in the within-group blocks for
#'   predictors. For independent predictors, use \code{nu_within=0}. This
#'   argument is only used if \code{adjancency_x} is not provided.
#' @param theta_xz optional binary matrix encoding the predictor variables from
#'   \code{xdata} (columns) contributing to the definition of the orthogonal
#'   latent outcomes from \code{zdata} (rows).
#' @param nu_xz expected proportion of relevant predictors over the total number
#'   of predictors to be used for the simulation of the orthogonal latent
#'   outcomes. This argument is only used if \code{theta_xz} is not provided.
#' @param theta_zy optional binary matrix encoding the latent variables from
#'   \code{zdata} (columns) contributing to the definition of the observed
#'   outcomes from \code{ydata} (rows). This argument must be a square matrix of
#'   size \code{length(pk)}. If \code{theta_zy} is a diagonal matrix, each
#'   latent variable contributes to the definition of one observed outcome so
#'   that there is a one-to-one relationship between latent and observed
#'   outcomes (i.e. they are colinear). Nonzero off-diagonal elements in
#'   \code{theta_zy} introduce some correlation between the observed outcomes by
#'   construction from linear combinations implicating common latent outcomes.
#'   This argument is only used if \code{eta} is not provided.
#' @param nu_zy probability for each of the off-diagonal elements in
#'   \code{theta_zy} to be a 1. If \code{nu_zy=0}, \code{theta_zy} is a diagonal
#'   matrix. This argument is only used if \code{theta_zy} is not provided.
#' @param eta optional matrix of coefficients used in the linear combination of
#'   latent outcomes to generate observed outcomes.
#' @param eta_set vector defining the range of values from which \code{eta} is
#'   sampled. This argument is only used if \code{eta} is not provided.
#' @param ev_xx expected proportion of explained variance by the first Principal
#'   Component (PC1) of a Principal Component Analysis. This is the largest
#'   eigenvalue of the correlation (if \code{scale=TRUE}) or covariance (if
#'   \code{scale=FALSE}) matrix divided by the sum of eigenvalues. If
#'   \code{ev=NULL} (the default), the constant u is chosen by maximising the
#'   contrast of the correlation matrix.
#'
#' @return A list with: \item{xdata}{simulated predictor data.}
#'   \item{ydata}{simulated outcome data.} \item{proba}{simulated probability of
#'   belonging to each outcome class. Only used for \code{family="binomial"} or
#'   \code{family="multinomial"}.} \item{logit_proba}{logit of the simulated
#'   probability of belonging to each outcome class. Only used for
#'   \code{family="binomial"} or \code{family="multinomial"}.}
#'   \item{zdata}{simulated data for orthogonal latent outcomes.}
#'   \item{beta}{matrix of true beta coefficients used to generate outcomes in
#'   \code{ydata} from predictors in \code{xdata}.} \item{theta}{binary matrix
#'   indicating the predictors from \code{xdata} contributing to the definition
#'   of each of the outcome variables in \code{ydata}.} \item{eta}{matrix of
#'   coefficients used in the linear combination of latent variables from
#'   \code{zdata} to define observed outcomes in \code{ydata}.}
#'   \item{theta_zy}{binary matrix indicating the latent variables from
#'   \code{zdata} used in the definition of observed outcomes in \code{ydata}.}
#'   \item{xi}{matrix of true beta coefficients used to generate orthogonal
#'   latent outcomes in \code{zdata} from predictors in \code{xdata}.}
#'   \item{theta_xz}{binary matrix indicating the predictors from \code{xdata}
#'   contributing to the definition of each of the latent outcome variables in
#'   \code{zdata}.} \item{omega_xz}{precision matrix for variables in
#'   \code{xdata} and \code{zdata}.} \item{adjacency}{binary matrix encoding the
#'   conditional independence structure between variables from \code{xdata}
#'   (var), \code{zdata} (latent) and \code{ydata} (outcome).}
#'
#' @family simulation functions
#'
#' @seealso \code{\link{VariableSelection}}
#'
#' @examples
#' \dontrun{
#'
#' ## Continuous outcomes
#'
#' # Univariate outcome
#' set.seed(1)
#' simul <- SimulateRegression(pk = c(5, 7, 3))
#' print(simul)
#' plot(simul)
#'
#' # Multivariate outcome
#' set.seed(1)
#' simul <- SimulateRegression(pk = c(5, 7, 3))
#' print(simul)
#' plot(simul)
#'
#' # Independent predictors
#' set.seed(1)
#' simul <- SimulateRegression(pk = c(5, 3), nu_within = 0)
#' print(simul)
#' plot(simul)
#'
#' # Blocks of strongly inter-connected predictors
#' set.seed(1)
#' simul <- SimulateRegression(
#'   pk = c(5, 5), nu_within = 0.5,
#'   v_within = c(0.5, 1), v_sign = -1, continuous = TRUE, pd_strategy = "min_eigenvalue"
#' )
#' print(simul)
#' par(mar = c(5, 5, 5, 5))
#' Heatmap(
#'   mat = cor(simul$xdata),
#'   colours = c("navy", "white", "red"),
#'   legend_range = c(-1, 1)
#' )
#' plot(simul)
#'
#'
#' ## Categorical outcomes
#'
#' # Binary outcome
#' set.seed(1)
#' simul <- SimulateRegression(pk = 20, family = "binomial")
#' print(simul)
#' table(simul$ydata[, 1])
#'
#' # Categorical outcome
#' set.seed(1)
#' simul <- SimulateRegression(pk = 20, family = "multinomial")
#' print(simul)
#' apply(simul$ydata, 2, sum)
#' }
#' @export
SimulateRegression <- function(n = 100, pk = 10, N = 3,
                               family = "gaussian", ev_xz = 0.8,
                               adjacency_x = NULL, nu_within = 0.1,
                               theta_xz = NULL, nu_xz = 0.2,
                               theta_zy = NULL, nu_zy = 0.5,
                               eta = NULL, eta_set = c(-1, 1),
                               v_within = c(0.5, 1), v_sign = c(-1, 1), continuous = TRUE,
                               pd_strategy = "diagonally_dominant", ev_xx = NULL, scale = TRUE,
                               u_list = c(1e-10, 1), tol = .Machine$double.eps^0.25) {
  # Checking the inputs
  if ((length(pk) > 1) & (family == "multinomial")) {
    stop("The simulation of multiple categorical outcomes is not possible with the current implementation.")
  }
  
  # Definition of the number of latent outcome variables
  q <- length(pk)
  p <- sum(pk)
  if (length(nu_xz) != q) {
    nu_xz <- rep(nu_xz[1], q)
  }
  if (length(ev_xz) != q) {
    ev_xz <- rep(ev_xz[1], q)
  }
  
  # Checking the values of ev_xz
  if (any(ev_xz <= 0) | any(ev_xz >= 1)) {
    stop("Invalid input for argument 'ev_xz'. Please provide values strictly between 0 and 1.")
  }
  
  # Simulation of the conditional independence structure with independent blocks
  if (is.null(adjacency_x)) {
    adjacency_x <- SimulateAdjacency(
      pk = pk, nu_between = 0, nu_within = nu_within,
      implementation = HugeAdjacency,
      topology = "random"
    )
  }
  
  # Simulation of the binary contribution status of predictors for latent outcome variables
  if (is.null(theta_xz)) {
    theta_xz <- SamplePredictors(pk = pk, q = q, nu = nu_xz, orthogonal = TRUE)
  }
  
  # Using the same support for all categories (multinomial only)
  if (family == "multinomial") {
    theta_xz <- matrix(rep(theta_xz, N), ncol = N)
    q <- N
    ev_xz <- rep(ev_xz, N)
  }
  
  # Setting row and column names
  colnames(theta_xz) <- paste0("latent", 1:ncol(theta_xz))
  rownames(theta_xz) <- paste0("var", 1:nrow(theta_xz))
  
  # Simulation of precision matrix for both predictors and latent outcomes
  big_theta <- cbind(
    rbind(matrix(0, nrow = q, ncol = q), theta_xz),
    rbind(t(theta_xz), adjacency_x)
  )
  rownames(big_theta) <- colnames(big_theta)
  out <- SimulatePrecision(
    theta = big_theta, v_within = v_within,
    v_sign = v_sign, continuous = continuous,
    pd_strategy = pd_strategy, ev = ev_xx, scale = scale, u_list = u_list, tol = tol
  )
  omega <- out$omega
  
  # Setting diagonal precision for latent outcomes to reach expected proportion of explained variance
  for (j in 1:q) {
    pred_ids <- seq(q + 1, q + p)
    omega[j, j] <- omega[j, pred_ids, drop = FALSE] %*% solve(omega[pred_ids, pred_ids]) %*% t(omega[j, pred_ids, drop = FALSE]) * 1 / ev_xz[j]
  }
  
  # Checking positive definiteness (mostly for multinomial?)
  eig <- eigen(omega)$values
  if (any(eig <= 0)) {
    message("The requested proportion of explained variance could not be achieved.")
    ev_xz <- stats::optimise(TuneExplainedVarianceReg, interval = c(0, min(ev_xz)), omega = omega, q = q, p = p)$minimum
    ev_xz <- rep(ev_xz, q)
    for (j in 1:q) {
      pred_ids <- seq(q + 1, q + p)
      omega[j, j] <- omega[j, pred_ids, drop = FALSE] %*% solve(omega[pred_ids, pred_ids]) %*% t(omega[j, pred_ids, drop = FALSE]) * 1 / ev_xz[j]
    }
  }
  
  # Computing the covariance matrix
  sigma <- solve(omega)
  
  # Computing the regression coefficients from X to Z
  xi <- solve(sigma[grep("var", colnames(omega)), grep("var", colnames(omega))]) %*% sigma[grep("var", colnames(sigma)), grep("latent", colnames(sigma))]
  colnames(xi) <- colnames(theta_xz)
  
  # Simulation of data from multivariate normal distribution
  x <- MASS::mvrnorm(n, rep(0, p + q), sigma)
  rownames(x) <- paste0("obs", 1:nrow(x))
  
  # Separating predictors from latent outcome variables
  xdata <- x[, grep("var", colnames(x)), drop = FALSE]
  zdata <- x[, grep("latent", colnames(x)), drop = FALSE]
  
  # Simulation of eta coefficients to get observed outcomes from latent outcomes
  if (is.null(eta)) {
    if (family == "multinomial") {
      # Variables in Z and Y are the same for multinomial as restricted to one categorical outcome
      theta_zy <- eta <- diag(N)
    } else {
      if (is.null(theta_zy)) {
        theta_zy <- SamplePredictors(pk = q, q = q, nu = nu_zy, orthogonal = FALSE)
      }
      eta <- matrix(stats::runif(q * q, min = min(eta_set), max = max(eta_set)),
                    ncol = q, nrow = q
      )
      eta <- eta * theta_zy
    }
  } else {
    theta_zy <- ifelse(eta != 0, yes = 1, no = 0)
  }
  rownames(eta) <- rownames(theta_zy) <- paste0("latent", 1:q)
  colnames(eta) <- colnames(theta_zy) <- paste0("outcome", 1:q)
  ydata <- zdata %*% eta
  
  # Computing the xy coefficients and binary contribution status
  beta <- xi %*% eta
  beta <- base::zapsmall(beta)
  theta <- ifelse(beta != 0, yes = 1, no = 0)
  
  # Compute binary outcome for logistic regression
  if (family == "binomial") {
    # Variability is coming from binomial distribution for logistic
    ydata <- xdata %*% beta
    
    # Sampling from (series of) binomial distributions
    proba <- matrix(NA, nrow = n, ncol = q)
    for (j in 1:q) {
      proba[, j] <- 1 / (1 + exp(-ydata[, j])) # inverse logit
    }
    
    ydata_cat <- matrix(0, nrow = n, ncol = q)
    for (j in 1:q) {
      for (i in 1:n) {
        ydata_cat[i, j] <- stats::rbinom(n = 1, size = 1, prob = proba[i, j])
      }
    }
    
    # Setting row and column names
    rownames(ydata_cat) <- rownames(proba) <- rownames(ydata)
    colnames(ydata_cat) <- colnames(proba) <- colnames(ydata)
  }
  
  if (family == "multinomial") {
    # Variability is coming from multinomial distribution for logistic
    ydata <- xdata %*% beta
    
    proba <- matrix(NA, nrow = n, ncol = q)
    for (j in 1:q) {
      proba[, j] <- 1 / (1 + exp(-ydata[, j])) # inverse logit
    }
    
    # Sampling from multinomial distribution
    ydata_cat <- matrix(0, nrow = n, ncol = q)
    for (i in 1:n) {
      ydata_cat[i, ] <- stats::rmultinom(n = 1, size = 1, prob = proba[i, ])[, 1]
    }
    
    # Setting row and column names
    rownames(ydata_cat) <- rownames(proba) <- rownames(ydata)
    colnames(ydata_cat) <- colnames(proba) <- colnames(ydata)
  }
  
  # Extracting the conditional independence structure between x, z and y
  adjacency <- rbind(
    cbind(matrix(0, nrow = q, ncol = q), t(theta_zy), matrix(0, nrow = q, ncol = p)),
    cbind(rbind(theta_zy, matrix(0, nrow = p, ncol = q)), big_theta)
  )
  rownames(adjacency) <- colnames(adjacency) <- c(colnames(theta_zy), rownames(big_theta))
  
  # Return the simulated X and Y
  if (family %in% c("binomial", "multinomial")) {
    if (family == "binomial") {
      out <- list(
        xdata = xdata, ydata = ydata_cat,
        proba = proba, logit_proba = ydata,
        zdata = zdata,
        beta = beta, theta = theta,
        eta = eta, theta_zy = theta_zy,
        xi = xi, theta_xz = theta_xz,
        ev_xz = ev_xz,
        omega_xz = omega,
        adjacency = adjacency
      )
    }
    if (family == "multinomial") {
      out <- list(
        xdata = xdata, ydata = ydata_cat,
        proba = proba, logit_proba = ydata,
        zdata = zdata,
        beta = beta, theta = theta,
        theta_zy = theta_zy,
        xi = xi, theta_xz = theta_xz,
        ev_xz = ev_xz,
        omega_xz = omega,
        adjacency = adjacency
      )
    }
  } else {
    out <- list(
      xdata = xdata, ydata = ydata, zdata = zdata,
      beta = beta, theta = theta,
      eta = eta, theta_zy = theta_zy,
      xi = xi, theta_xz = theta_xz,
      ev_xz = ev_xz,
      omega_xz = omega,
      adjacency = adjacency
    )
  }
  
  # Defining the class
  class(out) <- "simulation_regression"
  
  return(out)
}


#' Simulation of an undirected graph
#'
#' Simulates the adjacency matrix encoding an unweighted, undirected graph with
#' no self-loops.
#'
#' @inheritParams SimulateGraphical
#' @param pk number of nodes.
#' @param nu expected density (number of edges over the number of node pairs) of
#'   the graph. This argument is only used for \code{topology="random"} or
#'   \code{topology="cluster"} (see argument \code{prob} in
#'   \code{\link[huge]{huge.generator}}).
#' @param ... additional arguments to be passed to
#'   \code{\link[huge]{huge.generator}}.
#'
#' @return A symmetric adjacency matrix encoding an unweighted, undirected graph
#'   with no self-loops.
#'
#' @family simulation functions
#'
#' @examples
#' \dontrun{
#'
#' # Simulation of a scale-free graph with 20 nodes
#' adjacency <- HugeAdjacency(pk = 20, topology = "scale-free")
#' plot(Graph(adjacency))
#' }
#' @export
HugeAdjacency <- function(pk = 10, topology = "random", nu = 0.1, ...) {
  # Storing extra arguments
  extra_args <- list(...)
  
  # Extracting relevant extra arguments
  ids <- which(names(extra_args) %in% names(formals(huge::huge.generator)))
  ids <- ids[!ids %in% c("n", "d", "prob", "graph", "verbose")]
  
  # Running simulation model
  mymodel <- do.call(huge::huge.generator, args = c(
    list(
      n = 2, d = sum(pk), prob = nu,
      graph = topology, verbose = FALSE
    ),
    extra_args[ids]
  ))
  theta <- as.matrix(mymodel$theta)
  
  # Re-organising the variables to avoid having centrality related to variable ID (e.g. for scale-free models)
  ids <- sample(ncol(theta))
  theta <- theta[ids, ids]
  
  return(theta)
}


#' Simulation of an undirected graph with block structure
#'
#' Simulates the adjacency matrix of an unweighted, undirected graph with no
#' self-loops, and with different densities in diagonal compared to off-diagonal
#' blocks.
#'
#' @inheritParams SimulateGraphical
#'
#' @return A symmetric adjacency matrix encoding an unweighted, undirected graph
#'   with no self-loops, and with different densities in diagonal compared to off-diagonal
#' blocks.
#'
#' @family simulation functions
#'
#' @examples
#' \dontrun{
#'
#' # Simulation of a scale-free graph with 20 nodes
#' adjacency <- SimulateAdjacency(pk = 20, topology = "scale-free")
#' plot(Graph(adjacency))
#'
#' # Simulation of a random graph with block structure
#' adjacency <- SimulateAdjacency(
#'   pk = rep(10, 3),
#'   nu_within = 0.7, nu_between = 0.03
#' )
#' plot(Graph(adjacency))
#'
#' # User-defined function for graph simulation
#' CentralNode <- function(pk, hub = 1) {
#'   theta <- matrix(0, nrow = sum(pk), ncol = sum(pk))
#'   theta[hub, ] <- 1
#'   theta[, hub] <- 1
#'   diag(theta) <- 0
#'   return(theta)
#' }
#' simul <- SimulateAdjacency(pk = 10, implementation = CentralNode)
#' plot(Graph(simul)) # star
#' simul <- SimulateAdjacency(pk = 10, implementation = CentralNode, hub = 2)
#' plot(Graph(simul)) # variable 2 is the central node
#' }
#'
#' @export
SimulateAdjacency <- function(pk = 10,
                              implementation = HugeAdjacency, topology = "random",
                              nu_within = 0.1, nu_between = 0, ...) {
  # Storing all arguments
  args <- c(mget(ls()), list(...))
  
  # Checking the inputs
  if (topology != "random") {
    if (length(pk) > 1) {
      pk <- sum(pk)
      warning(paste0("Multi-block simulations are only allowed with topology='random'. Argument 'pk' has been set to ", pk, "."))
    }
  }
  
  # Creating matrix with block indices
  bigblocks <- BlockMatrix(pk)
  bigblocks_vect <- bigblocks[upper.tri(bigblocks)]
  
  # Making as factor to allow for groups with 1 variable (for clustering)
  bigblocks_vect <- factor(bigblocks_vect, levels = seq(1, max(bigblocks)))
  block_ids <- unique(as.vector(bigblocks))
  
  # Identifying relevant arguments
  if (!"..." %in% names(formals(implementation))) {
    ids <- which(names(args) %in% names(formals(implementation)))
    args <- args[ids]
  }
  
  # Simulation of the adjacency matrix
  if ("nu" %in% names(formals(implementation))) {
    if (length(pk) > 1) {
      # Initialising theta
      theta <- matrix(0, nrow = sum(pk), ncol = sum(pk))
      theta_vect <- theta[upper.tri(theta)]
      
      # Allowing for different densities in within and between blocks
      theta_w <- do.call(implementation, args = c(args, list(nu = nu_within)))
      theta_w_vect <- theta_w[upper.tri(theta_w)]
      theta_b <- do.call(implementation, args = c(args, list(nu = nu_between)))
      theta_b_vect <- theta_b[upper.tri(theta_b)]
      
      # Filling within and between blocks
      for (k in block_ids) {
        if (k %in% unique(diag(bigblocks))) {
          theta_vect[bigblocks_vect == k] <- theta_w_vect[bigblocks_vect == k]
        } else {
          theta_vect[bigblocks_vect == k] <- theta_b_vect[bigblocks_vect == k]
        }
      }
      theta[upper.tri(theta)] <- theta_vect
      theta <- theta + t(theta)
    } else {
      theta <- do.call(implementation, args = c(args, list(nu = nu_within)))
    }
  } else {
    theta <- do.call(implementation, args = c(args))
  }
  
  # Ensuring the adjacency matrix is symmetric (undirected graph) with no self-loops
  theta <- ifelse(theta + t(theta) != 0, yes = 1, no = 0)
  diag(theta) <- 0
  
  # Setting variable names
  colnames(theta) <- rownames(theta) <- paste0("var", 1:ncol(theta))
  
  return(theta)
}


#' Simulation of symmetric matrix with block structure
#'
#' Simulates a symmetric matrix with block structure. Matrix entries are
#' sampled from (i) a discrete uniform distribution taking values in
#' \code{v_within} (for entries in the diagonal block) or \code{v_between} (for
#' entries in off-diagonal blocks) if \code{continuous=FALSE}, or (ii) a
#' continuous uniform distribution taking values in the range given by
#' \code{v_within} or \code{v_between} if \code{continuous=TRUE}.
#'
#' @param pk vector of the number of variables per group, defining the block
#'   structure.
#' @param v_within vector defining the (range of) nonzero entries in the
#'   diagonal blocks. If \code{continuous=FALSE}, \code{v_within} is the set of
#'   possible values. If \code{continuous=FALSE}, \code{v_within} is the range
#'   of possible values.
#' @param v_between vector defining the (range of) nonzero entries in the
#'   off-diagonal blocks. If \code{continuous=FALSE}, \code{v_between} is the
#'   set of possible precision values. If \code{continuous=FALSE},
#'   \code{v_between} is the range of possible precision values. This argument
#'   is only used if \code{length(pk)>1}.
#' @param v_sign vector of possible signs for matrix entries. Possible
#'   inputs are: \code{-1} for negative entries only, \code{1} for
#'   positive entries only, or \code{c(-1, 1)} for both positive and
#'   negative entries.
#' @param continuous logical indicating whether to sample precision values from
#'   a uniform distribution between the minimum and maximum values in
#'   \code{v_within} (diagonal blocks) or \code{v_between} (off-diagonal blocks)
#'   (\code{continuous=TRUE}) or from proposed values in \code{v_within}
#'   (diagonal blocks) or \code{v_between} (off-diagonal blocks)
#'   (\code{continuous=FALSE}).
#'
#' @return A symmetric matrix with uniformly distributed entries sampled from
#'   different distributions for diagonal and off-diagonal blocks.
#'
#' @keywords internal
SimulateSymmetricMatrix <- function(pk = 10,
                                    v_within = c(0.5, 1), v_between = c(0, 0.1),
                                    v_sign = c(-1, 1), continuous = FALSE) {
  # Creating matrix with block indices
  bigblocks <- BlockMatrix(pk)
  bigblocks_vect <- bigblocks[upper.tri(bigblocks)]
  
  # Making as factor to allow for groups with 1 variable (for clustering)
  bigblocks_vect <- factor(bigblocks_vect, levels = seq(1, max(bigblocks)))
  block_ids <- unique(as.vector(bigblocks))
  
  # Building absolute v matrix
  v <- bigblocks
  v_vect <- v[upper.tri(v)]
  for (k in block_ids) {
    if (k %in% v_vect) {
      if (k %in% unique(diag(bigblocks))) {
        if (continuous) {
          v_vect[bigblocks_vect == k] <- stats::runif(sum(bigblocks_vect == k), min = min(v_within), max = max(v_within))
        } else {
          v_vect[bigblocks_vect == k] <- base::sample(v_within, size = sum(bigblocks_vect == k), replace = TRUE)
        }
      } else {
        if (continuous) {
          v_vect[bigblocks_vect == k] <- stats::runif(sum(bigblocks_vect == k), min = min(v_between), max = max(v_between))
        } else {
          v_vect[bigblocks_vect == k] <- base::sample(v_between, size = sum(bigblocks_vect == k), replace = TRUE)
        }
      }
    }
  }
  
  # Sampling the sign of precision entries
  v_vect <- v_vect * base::sample(sort(unique(v_sign)), size = length(v_vect), replace = TRUE)
  
  # Building v matrix
  diag(v) <- 0
  v[upper.tri(v)] <- v_vect
  v[lower.tri(v)] <- 0
  v <- v + t(v)
  
  return(v)
}


#' Simulation of a precision matrix
#'
#' Simulates a sparse precision matrix from an adjacency matrix \code{theta}
#' encoding a conditional independence graph. Zero entries in the precision
#' matrix indicate pairwise conditional independence. Diagonal entries can be
#' tuned to (i) maximise the contrast of the correlation matrix, or (ii) reach a
#' user-defined proportion of explained variance by the first Principal
#' Component (see \code{\link{MakePositiveDefinite}}).
#'
#' @inheritParams SimulateGraphical
#' @param theta binary and symmetric adjacency matrix encoding the conditional
#'   independence structure.
#' @param scale logical indicating if the proportion of explained variance by
#'   PC1 should be computed from the correlation (\code{scale=TRUE}) or
#'   covariance (\code{scale=FALSE}) matrix.
#'
#' @return A list with: \item{omega}{true simulated precision matrix.}
#'   \item{u}{value of the constant u used to ensure that \code{omega} is
#'   positive definite.}
#'
#' @examples
#' \dontrun{
#'
#' # Simulation of an adjacency matrix
#' theta <- SimulateAdjacency(pk = c(5, 5), nu_within = 0.7)
#' print(theta)
#'
#' # Simulation of a precision matrix maximising the contrast
#' simul <- SimulatePrecision(theta = theta)
#' print(simul$omega)
#'
#' # Simulation of a precision matrix with specific ev by PC1
#' simul <- SimulatePrecision(
#'   theta = theta,
#'   pd_strategy = "min_eigenvalue",
#'   ev = 0.3, scale = TRUE
#' )
#' print(simul$omega)
#' }
#' @export
SimulatePrecision <- function(pk = NULL, theta,
                              v_within = c(0.5, 1), v_between = c(0, 0.1),
                              v_sign = c(-1, 1), continuous = TRUE,
                              pd_strategy = "diagonally_dominant", ev = NULL, scale = TRUE,
                              u_list = c(1e-10, 1), tol = .Machine$double.eps^0.25) {
  # Checking inputs and defining pk
  if (is.null(pk)) {
    pk <- ncol(theta)
  } else {
    if (sum(pk) != ncol(theta)) {
      stop("Arguments 'pk' and 'theta' are not consistent. The sum of 'pk' entries must be equal to the number of rows and columns in 'theta'.")
    }
  }
  
  # Checking the choice of pd_strategy
  if (!pd_strategy %in% c("diagonally_dominant", "min_eigenvalue")) {
    stop("Invalid input for argument 'pd_strategy'. Possible values are: 'diagonally_dominant' or 'min_eigenvalue'.")
  }
  
  # Checking other input values
  if (any((v_within < 0) | (v_within > 1))) {
    stop("Invalid input for argument 'v_within'. Values must be between 0 and 1.")
  }
  if (any((v_between < 0) | (v_between > 1))) {
    stop("Invalid input for argument 'v_between'. Values must be between 0 and 1.")
  }
  if (any(!v_sign %in% c(-1, 1))) {
    stop("Invalid input for argument 'v_sign'. Possible values are -1 and 1.")
  }
  
  # Ensuring that v values are lower than or equal to 1
  if (any(abs(v_within) > 1)) {
    v_within <- v_within / max(abs(v_within))
    message("The values provided in 'v_within' have been re-scaled to be lower than or equal to 1 in absolute value.")
  }
  
  # Ensuring that diagonal entries of theta are zero
  diag(theta) <- 0
  
  # Building v matrix
  v <- SimulateSymmetricMatrix(
    pk = pk, v_within = v_within, v_between = v_between,
    v_sign = v_sign, continuous = continuous
  )
  
  # Filling off-diagonal entries of the precision matrix
  omega_tilde <- theta * v
  
  # Ensuring positive definiteness
  omega_pd <- MakePositiveDefinite(
    omega = omega_tilde, pd_strategy = pd_strategy,
    ev = ev, scale = scale, u_list = u_list, tol = tol
  )
  
  # # Preparing realistic diagonally dominant precision matrix
  # if (pd_strategy == "diagonally_dominant") {
  #   # Defining grid of u values if not provided
  #   if (is.null(u)) {
  #     u <- 10^-(seq(0, 5, by = 0.1))
  #     refining_u_grid <- TRUE
  #     niter_max <- 5
  #     tolerance <- 10
  #   } else {
  #     refining_u_grid <- FALSE
  #   }
  #
  #   # Filling off-diagonal entries of the precision matrix
  #   omega <- theta * v
  #
  #   # Calibrate u based on contrasts of the correlation matrix
  #   contrast <- NULL
  #   for (u_value in u) {
  #     omega_tmp <- MakePositiveDefinite(omega = omega, u_value = u_value, pd_strategy = pd_strategy)
  #     C <- stats::cov2cor(solve(omega_tmp))
  #     contrast <- c(contrast, Contrast(C))
  #   }
  #
  #   # Avoiding extreme values in u grid if not provided by the user
  #   if (refining_u_grid) {
  #     stop <- 0
  #     niter <- 1
  #     while (stop == 0) {
  #       niter <- niter + 1
  #       if (niter == niter_max_u_grid) {
  #         stop <- 1
  #       }
  #       # Satisfied with calibrated u if the argmax is not too close to the boundaries (as defined from tolerance_u_grid)
  #       if (any(which(contrast == max(contrast)) %in% seq(tolerance_u_grid, length(u) - tolerance_u_grid) == TRUE)) {
  #         stop <- 1
  #       } else {
  #         # Adding smaller values of u
  #         if (any(which(contrast == max(contrast)) %in% seq(1, tolerance_u_grid) == TRUE)) {
  #           u <- c(u, 10^-seq(min(-log10(u)) - u_delta, min(-log10(u)), by = 0.1))
  #         }
  #
  #         # Adding larger values of u
  #         if (any(which(contrast == max(contrast)) %in% seq(length(u) - tolerance_u_grid, length(u)) == TRUE)) {
  #           u <- c(u, 10^-seq(max(-log10(u)), max(-log10(u) + u_delta), by = 0.1))
  #         }
  #
  #         # Sorting values in u
  #         u <- sort(u, decreasing = TRUE)
  #
  #         # Computing the contrast for all visited values of u
  #         contrast <- NULL
  #         for (u_value in u) {
  #           omega_tmp <- MakePositiveDefinite(omega = omega, u_value = u_value, pd_strategy = pd_strategy)
  #           C <- stats::cov2cor(solve(omega_tmp))
  #           contrast <- c(contrast, Contrast(C))
  #         }
  #       }
  #     }
  #   }
  #
  #   # Computing calibrated precision matrix
  #   if (length(u) > 1) {
  #     u_value <- u[length(contrast) - which.max(rev(contrast)) + 1] # adding smallest possible u value to the diagonal
  #     omega <- MakePositiveDefinite(omega = omega, u_value = u_value, pd_strategy = pd_strategy)
  #   } else {
  #     omega <- omega_tmp
  #   }
  # }
  #
  # # Allowing for higher correlations using smallest eigenvalue
  # if (pd_strategy == "min_eigenvalue") {
  #   # Defining a small constant
  #   if (is.null(u)) {
  #     u <- 1e-5
  #   }
  #
  #   # Initialisation of full precision matrix
  #   omega <- matrix(0, ncol = sum(pk), nrow = sum(pk))
  #
  #   # Creating matrix with block indices
  #   bigblocks <- BlockMatrix(pk)
  #
  #   # Making positive definite diagonal blocks (Schur complement shows it is p.d.)
  #   for (i in unique(diag(bigblocks))) {
  #     # Filling off-diagonal entries of the precision matrix (for corresponding diagonal block)
  #     omega_block <- matrix((theta * v)[which(bigblocks == i)], ncol = sum(diag(bigblocks) == i))
  #
  #     # # Computing the signed adjacency
  #     # signed_theta <- sign(omega_block)
  #
  #     # Defining a positive definite precision matrix
  #     omega_tmp <- MakePositiveDefinite(omega = omega_block, u_value = u, pd_strategy = pd_strategy)
  #
  #     # # Using sampled entries (need to be lower than or equal to 1 in absolute value)
  #     # diag(omega_block) <- diag(omega_tmp)
  #
  #     # Filling full precision matrix
  #     omega[which(bigblocks == i)] <- omega_tmp
  #   }
  #
  #   # Accounting for off-diagonal blocks (sum of p.d. is p.d.)
  #   if (length(pk) > 1) {
  #     # Filling off-diagonal entries of the precision matrix (for off-diagonal blocks)
  #     omega_block <- theta * v
  #     omega_block[which(bigblocks %in% unique(diag(bigblocks)))] <- 0
  #
  #     # # Computing the signed adjacency
  #     # signed_theta <- sign(omega_block)
  #
  #     # Defining a positive definite precision matrix
  #     omega_tmp <- MakePositiveDefinite(omega = omega_block, u_value = u, pd_strategy = pd_strategy)
  #
  #     # # Using sampled entries (need to be lower than or equal to 1 in absolute value)
  #     # diag(omega_block) <- diag(omega_tmp)
  #
  #     # Computing sum of the p.d. matrices
  #     omega <- omega + omega_tmp
  #   }
  #
  #   # Setting row and column names
  #   rownames(omega) <- colnames(omega) <- colnames(theta)
  # }
  
  # Returning the output
  return(omega_pd)
}


#' Simulation of binary contribution status
#'
#' Simulates the binary contribution status of potential predictor variables
#' from different blocks to outcome variables. For each outcome, the set of true
#' predictors is sampled from one block of potential predictors. If the blocks
#' of variables are independent, the outcomes will be independent too.
#'
#' @inheritParams SimulateSymmetricMatrix
#' @param q number of outcome variables. By default, one block of predictor is
#'   linked to one outcome, i.e. \code{q=sum(pk)}.
#' @param nu vector of probabilities. Each entry corresponds to one block of
#'   predictors and defines the probability for each predictor within the block
#'   to be chosen as true predictor of the corresponding outcome variable.
#' @param orthogonal logical indicating if the outcomes have to be defined from
#'   independent blocks of predictors as encoded in \code{pk}.
#'
#' @return A binary matrix encoding the contribution status of each predictor
#'   variable (columns) to each outcome variable (rows).
#'
#' @keywords internal
SamplePredictors <- function(pk, q = NULL, nu = 0.1, orthogonal = TRUE) {
  # Definition of the number of outcome variables
  if (is.null(q)) {
    q <- length(pk)
  }
  if (length(nu) != q) {
    nu <- rep(nu[1], q)
  }
  
  # Simulation of the binary status for true predictors
  theta <- matrix(0, nrow = q, ncol = sum(pk))
  for (k in 1:q) {
    if (orthogonal) {
      if (k > 1) {
        ids <- seq(cumsum(pk)[k - 1] + 1, cumsum(pk)[k])
      } else {
        ids <- seq(1, cumsum(pk)[k])
      }
      theta[k, ids] <- stats::rbinom(pk[k], size = 1, prob = nu[k])
      
      # Introducing at least one true predictor
      if (sum(theta[k, ids]) == 0) {
        theta[k, sample(ids, size = 1)] <- 1
      }
    } else {
      theta[k, ] <- stats::rbinom(sum(pk), size = 1, prob = nu[k])
      theta[k, k] <- 1
    }
  }
  
  return(t(theta))
}


#' Making positive definite
#'
#' Determines the diagonal entries of a symmetric matrix to ensure it is
#' positive definite. For this, diagonal entries of the matrix are defined to be
#' higher than (i) the sum of entries on the corresponding rows, which ensure it
#' is diagonally dominant, or (ii) the absolute value of the smallest eigenvalue
#' of the same matrix with a diagonal of zeros. The magnitude of (standardised)
#' values in the inverse matrix is tuned by adding a constant u to the diagonal
#' entries. Considering the matrix to make positive definite is a precision
#' matrix, the constant u is chosen to (i) maximise the contrast of the
#' corresponding correlation matrix, or (ii) tune the proportion of explained
#' variance by the first Principal Component (i.e. largest eigenvalue of the
#' covariance/correlation matrix divided by the sum of eigenvalues).
#'
#' @inheritParams SimulateGraphical
#' @param omega input matrix.
#' @param scale logical indicating if the proportion of explained variance by
#'   PC1 should be computed from the correlation (\code{scale=TRUE}) or
#'   covariance (\code{scale=FALSE}) matrix.
#'
#' @return A list with: \item{omega}{positive definite matrix.} \item{u}{value
#'   of the constant u.}
#'
#' @examples
#' \dontrun{
#'
#' # Simulation of a symmetric matrix
#' p <- 5
#' set.seed(1)
#' omega <- matrix(rnorm(p * p), ncol = p)
#' omega <- omega + t(omega)
#' diag(omega) <- 0
#'
#' # Diagonal dominance maximising contrast
#' omega_pd <- MakePositiveDefinite(omega,
#'   pd_strategy = "diagonally_dominant"
#' )
#' eigen(omega_pd$omega)$values # positive eigenvalues
#'
#' # Diagonal dominance with specific proportion of explained variance by PC1
#' omega_pd <- MakePositiveDefinite(omega,
#'   pd_strategy = "diagonally_dominant",
#'   ev = 0.55
#' )
#' lambda_inv <- eigen(cov2cor(solve(omega_pd$omega)))$values
#' max(lambda_inv) / sum(lambda_inv) # expected ev
#'
#' # Version not scaled (using eigenvalues from the covariance)
#' omega_pd <- MakePositiveDefinite(omega,
#'   pd_strategy = "diagonally_dominant",
#'   ev = 0.55, scale = FALSE
#' )
#' lambda_inv <- 1 / eigen(omega_pd$omega)$values
#' max(lambda_inv) / sum(lambda_inv) # expected ev
#'
#' # Non-negative eigenvalues maximising contrast
#' omega_pd <- MakePositiveDefinite(omega,
#'   pd_strategy = "min_eigenvalue"
#' )
#' eigen(omega_pd$omega)$values # positive eigenvalues
#'
#' # Non-negative eigenvalues with specific proportion of explained variance by PC1
#' omega_pd <- MakePositiveDefinite(omega,
#'   pd_strategy = "min_eigenvalue",
#'   ev = 0.7
#' )
#' lambda_inv <- eigen(cov2cor(solve(omega_pd$omega)))$values
#' max(lambda_inv) / sum(lambda_inv)
#'
#' # Version not scaled (using eigenvalues from the covariance)
#' omega_pd <- MakePositiveDefinite(omega,
#'   pd_strategy = "min_eigenvalue",
#'   ev = 0.7, scale = FALSE
#' )
#' lambda_inv <- 1 / eigen(omega_pd$omega)$values
#' max(lambda_inv) / sum(lambda_inv)
#' }
#' @export
MakePositiveDefinite <- function(omega, pd_strategy = "diagonally_dominant",
                                 ev = NULL, scale = TRUE, u_list = c(1e-10, 1),
                                 tol = .Machine$double.eps^0.25) {
  
  # Making positive definite using diagonally dominance
  if (pd_strategy == "diagonally_dominant") {
    # Constructing the diagonal as the sum of entries
    diag(omega) <- apply(abs(omega), 1, sum)
    lambda <- eigen(omega)$values
  }
  # Making positive definite using eigendecomposition
  if (pd_strategy == "min_eigenvalue") {
    # Extracting smallest eigenvalue of omega_tilde
    lambda <- eigen(omega)$values
    lambda0 <- abs(min(lambda))
    
    # Making the precision matrix positive semidefinite
    lambda <- lambda + lambda0
    diag(omega) <- lambda0
  }
  
  if (is.null(ev)) {
    # Finding u that maximises the contrast
    if (min(u_list) != max(u_list)) {
      argmax_u <- stats::optimise(MaxContrast,
                                  omega = omega, maximum = TRUE, tol = 1,
                                  lower = min(u_list), upper = max(u_list)
      )
      u <- argmax_u$maximum
    } else {
      u <- min(u_list)
    }
  } else {
    # Finding extreme values
    if (scale) {
      max_ev <- TuneExplainedVarianceCor(u = min(u_list), omega = omega)
      min_ev <- TuneExplainedVarianceCor(u = max(u_list), omega = omega)
    } else {
      max_ev <- TuneExplainedVarianceCov(u = min(u_list), lambda = lambda)
      min_ev <- TuneExplainedVarianceCov(u = max(u_list), lambda = lambda)
    }
    
    # Finding u corresponding to the required proportion of explained variance
    if ((ev <= min_ev) | (ev >= max_ev)) {
      if (ev <= min_ev) {
        u <- max(u_list)
        if (ev < min_ev) {
          message(paste0("The smallest proportion of explained variance by PC1 that can be obtained is ", round(min_ev, digits = 2), "."))
        }
      } else {
        u <- min(u_list)
        if (ev > max_ev) {
          message(paste0("The largest proportion of explained variance by PC1 that can be obtained is ", round(max_ev, digits = 2), "."))
        }
      }
    } else {
      if (scale) {
        # Minimising the difference between requested and possible ev
        if (min(u_list) != max(u_list)) {
          argmin_u <- stats::optimise(TuneExplainedVarianceCor,
                                      omega = omega, ev = ev,
                                      lower = min(u_list), upper = max(u_list), tol = tol
          )
          u <- argmin_u$minimum
        } else {
          u <- min(u_list)
        }
      } else {
        # Minimising the difference between requested and possible ev
        if (min(u_list) != max(u_list)) {
          argmin_u <- stats::optimise(TuneExplainedVarianceCov,
                                      lambda = lambda, ev = ev,
                                      lower = min(u_list), upper = max(u_list), tol = tol
          )
          u <- argmin_u$minimum
        } else {
          u <- min(u_list)
        }
      }
    }
  }
  
  # Constructing the diagonal
  diag(omega) <- diag(omega) + u
  
  return(list(omega = omega, u = u))
}


#' Maximising matrix contrast
#'
#' Computes the contrast of the correlation matrix obtained by adding u to the
#' diagonal of the precision matrix. This function is used to find the value of
#' u that maximises the contrast when constructing a diagonally dominant
#' precision matrix.
#'
#' @param u constant u added to the diagonal of the precision matrix.
#' @param omega positive semi-definite precision matrix.
#' @param digits number of digits to use in the definition of the contrast.
#'
#' @return A single number, the contrast of the generated precision matrix.
MaxContrast <- function(u, omega, digits = 3) {
  diag(omega) <- diag(omega) + u
  return(Contrast(stats::cov2cor(omega), digits = digits))
}


#' Matrix contrast
#'
#' Computes the matrix contrast, defined as the number of unique truncated
#' entries with a specified number of digits.
#'
#' @param mat input matrix.
#' @param digits number of digits to use.
#'
#' @return A single number, the contrast of the input matrix.
#'
#' @export
Contrast <- function(mat, digits = 3) {
  return(length(unique(round(as.vector(abs(mat)), digits = digits))))
}


#' Tuning function (covariance)
#'
#' Computes the difference in absolute value between the desired and observed
#' proportion of explained variance from the first Principal Component of a
#' Principal Component Analysis applied on the covariance matrix. The precision
#' matrix is obtained by adding u to the diagonal of a positive semidefinite
#' matrix. This function is used to find the value of the constant u
#' that generates a covariance matrix with desired proportion of explained
#' variance.
#'
#' @inheritParams MaxContrast
#' @param ev desired proportion of explained variance. If \code{ev=NULL}, the
#'   obtained proportion of explained variance is returned.
#' @param lambda eigenvalues of the positive semidefinite precision matrix.
#'
#' @return The difference in proportion of explained variance in absolute values
#'   or observed proportion of explained variance (if \code{ev=NULL}).
#'
#' @export
TuneExplainedVarianceCov <- function(u, ev = NULL, lambda) {
  lambda <- lambda + u
  lambda_inv <- 1 / lambda
  tmp_ev <- max(lambda_inv) / sum(lambda_inv)
  if (is.null(ev)) {
    out <- tmp_ev
  } else {
    out <- abs(tmp_ev - ev)
  }
  return(out)
}


#' Tuning function (correlation)
#'
#' Computes the difference in absolute value between the desired and observed
#' proportion of explained variance from the first Principal Component of a
#' Principal Component Analysis applied on the correlation matrix. The precision
#' matrix is obtained by adding u to the diagonal of a positive semidefinite
#' matrix. This function is used to find the value of the constant u
#' that generates a correlation matrix with desired proportion of explained
#' variance.
#'
#' @inheritParams TuneExplainedVarianceCov
#' @param omega positive semidefinite precision matrix.
#'
#' @return The difference in proportion of explained variance in absolute values
#'   or observed proportion of explained variance (if \code{ev=NULL}).
#'
#' @export
TuneExplainedVarianceCor <- function(u, ev = NULL, omega) {
  diag(omega) <- diag(omega) + u
  mycor <- stats::cov2cor(solve(omega))
  tmp_ev <- norm(mycor, type = "2") / ncol(mycor)
  if (is.null(ev)) {
    out <- tmp_ev
  } else {
    out <- abs(tmp_ev - ev)
  }
  return(out)
}


#' Tuning function (regression)
#'
#' Computes the absolute difference between the smallest eigenvalue and
#' requested one (parameter \code{tol}) for a precision matrix with predictors
#' and outcomes.
#'
#' @param ev_xz proportion of explained variance.
#' @param omega precision matrix.
#' @param tol requested smallest eigenvalue after transformation of the input
#'   precision matrix.
#' @param q number of outcome variables.
#' @param p number of predictor variables.
#'
#' @return The absolute difference between the smallest eigenvalue of the
#'   transformed precision matrix and requested value \code{tol}.
#'
#' @export
TuneExplainedVarianceReg <- function(ev_xz, omega, tol = 0.1, q, p) {
  ev_xz <- rep(ev_xz, q)
  for (j in 1:q) {
    pred_ids <- seq(q + 1, q + p)
    omega[j, j] <- omega[j, pred_ids, drop = FALSE] %*% solve(omega[pred_ids, pred_ids]) %*% t(omega[j, pred_ids, drop = FALSE]) * 1 / ev_xz[j]
  }
  return(abs(min(eigen(omega)$values) - tol))
}



StabilityMetrics <- function(selprop, pk = NULL, pi_list = seq(0.6, 0.9, by = 0.01),
                             K = 100, n_cat = 3,
                             PFER_method = "MB", PFER_thr_blocks = Inf, FDP_thr_blocks = Inf,
                             Sequential_template = NULL, graph = TRUE, group = NULL) {
  if (graph) {
    nlambda <- dim(selprop)[3]
  } else {
    nlambda <- nrow(selprop)
  }
  
  # Extracting pk
  if (is.null(pk)) {
    pk <- ncol(selprop)
  }
  
  if (is.null(Sequential_template)) {
    Sequential_template <- matrix(TRUE, nrow = nlambda, ncol = 1)
  }
  
  # Create matrix with block indices
  nblocks <- 1
  if (graph) { # to avoid memory issues in high dimensional variable selection
    bigblocks <- BlockMatrix(pk)
    bigblocks_vect <- bigblocks[upper.tri(bigblocks)]
    N_blocks <- unname(table(bigblocks_vect))
    blocks <- unique(as.vector(bigblocks_vect))
    names(N_blocks) <- blocks
    nblocks <- max(blocks)
  }
  
  # Initialising objects to be filled
  Q <- Q_s <- P <- matrix(NA, nrow = nlambda, ncol = nblocks)
  best_loglik <- best_PFER <- best_FDP <- matrix(NA, nrow = nlambda, ncol = nblocks)
  if (nblocks == 1) {
    loglik <- PFER <- FDP <- matrix(NA, ncol = length(pi_list), nrow = nlambda)
  } else {
    loglik <- array(NA, dim = c(nlambda, length(pi_list), nblocks))
  }
  
  # Computing the metrics for each value of lambda
  for (k in 1:nlambda) {
    # Extracting corresponding selection proportions
    if (graph) {
      stab_iter <- selprop[, , k]
    } else {
      stab_iter <- selprop[k, ]
    }
    
    # Computing stability score with block-specific pi
    for (block_id in 1:nblocks) {
      if (Sequential_template[k, block_id]) {
        if (graph) {
          stab_iter_block <- stab_iter[(bigblocks == block_id) & (upper.tri(bigblocks))] # selection proportions in the block
        } else {
          stab_iter_block <- stab_iter
        }
        
        # Using group penalisation (extracting one per group)
        if (!is.null(group)) {
          stab_iter_block <- stab_iter_block[cumsum(group)]
        }
        
        q_block <- round(sum(stab_iter_block, na.rm = TRUE)) # average number of edges selected by the original procedure in the block
        Q[k, block_id] <- q_block
        N_block <- length(stab_iter_block) # maximum number of edges in the block
        tmp_loglik <- tmp_PFERs <- tmp_FDPs <- rep(NA, length(pi_list))
        
        # Computing error rates and stability score for different values of pi
        for (j in 1:length(pi_list)) {
          pi <- pi_list[j]
          tmp_PFERs[j] <- PFER(q = q_block, pi = pi, N = N_block, K = K, PFER_method = PFER_method)
          tmp_FDPs[j] <- FDP(selprop = stab_iter_block, PFER = tmp_PFERs[j], pi = pi)
          if ((tmp_PFERs[j] <= PFER_thr_blocks[block_id]) & (tmp_FDPs[j] <= FDP_thr_blocks[block_id])) {
            # Computing stability score (group penalisation is accounted for above so no need here)
            tmp_loglik[j] <- StabilityScore(selprop = stab_iter_block, pi_list = pi, K = K, n_cat = n_cat, group = NULL)
          }
        }
        
        # Storing stability score in a matrix if only one block
        if (nblocks == 1) {
          loglik[k, ] <- tmp_loglik
          PFER[k, ] <- tmp_PFERs
          FDP[k, ] <- tmp_FDPs
        } else {
          loglik[k, , block_id] <- tmp_loglik
        }
        
        # Keeping best stability score and other parameters at the max
        if (any(!is.na(tmp_loglik))) {
          tmp_loglik[is.na(tmp_loglik)] <- 0
          myid <- which.max(tmp_loglik)
          tmp_loglik[which(tmp_loglik == 0)] <- NA
          best_loglik[k, block_id] <- tmp_loglik[myid]
          P[k, block_id] <- pi_list[myid]
          Q_s[k, block_id] <- sum(stab_iter_block >= pi_list[myid], na.rm = TRUE)
          best_PFER[k, block_id] <- tmp_PFERs[myid]
          best_FDP[k, block_id] <- tmp_FDPs[myid]
        }
      }
    }
  }
  best_loglik_blocks <- best_loglik
  best_loglik <- matrix(apply(best_loglik, 1, sum), ncol = 1)
  
  if (nblocks == 1) {
    return(list(
      S = best_loglik_blocks,
      Q = Q, Q_s = Q_s, P = P,
      PFER = best_PFER, FDP = best_FDP,
      S_2d = loglik, PFER_2d = PFER, FDP_2d = FDP
    ))
  } else {
    return(list(
      S = best_loglik_blocks,
      Q = Q, Q_s = Q_s, P = P,
      PFER = best_PFER, FDP = best_FDP,
      S_2d = loglik
    ))
  }
}


PFER <- function(q, pi, N, K, PFER_method = "MB") {
  # Checking the inputs (PFER_method)
  PFER_method <- as.character(PFER_method)
  if ((length(PFER_method) != 1) | (!PFER_method %in% c("MB", "SS"))) {
    stop("Invalid input for argument 'PFER_method'. Possible values are: 'MB' or 'SS'.")
  }
  
  if (pi > 0.5) {
    # Computing upper-bound of the PFER using approach proposed by MB
    if (PFER_method == "MB") {
      upperbound <- 1 / (2 * pi - 1) * q^2 / N
    }
    
    # Computing upper-bound of the PFER using approach proposed by SS
    if (PFER_method == "SS") {
      cutoff <- pi
      B <- ceiling(K / 2)
      theta <- q / N
      if (cutoff <= 3 / 4) {
        tmp <- 2 * (2 * cutoff - 1 - 1 / (2 * B))
      } else {
        tmp <- (1 + 1 / B) / (4 * (1 - cutoff + 1 / (2 * B)))
      }
      upperbound <- q^2 / N / tmp
      
      # Setting to Inf if "out of bounds"
      if ((cutoff < 1 / 2 + min(theta^2, 1 / (2 * B) + 3 / 4 * theta^2)) | (cutoff > 1)) {
        upperbound <- Inf
      }
    }
  } else {
    upperbound <- Inf
  }
  
  # Re-formatting the upperbound
  if (is.na(upperbound)) {
    upperbound <- Inf
  }
  
  return(upperbound)
}


#' False Discovery Proportion
#'
#' Computes the False Discovery Proportion (upper-bound) as a ratio of the PFER
#' (upper-bound) over the number of stably selected features. In stability
#' selection, the FDP corresponds to the expected proportion of stably selected
#' features that are not relevant to the outcome (i.e. proportion of False
#' Positives among stably selected features).
#'
#' @param selprop matrix or vector of selection proportions.
#' @param PFER Per Family Error Rate.
#' @param pi threshold in selection proportions.
#'
#' @return The FDP (upper-bound).
#'
#' @family stability metric functions
#'
#' @examples
#' # Simulating set of selection proportions
#' selprop <- round(runif(n = 20), digits = 2)
#'
#' # Computing the FDP with a threshold of 0.8
#' fdp <- FDP(PFER = 3, selprop = selprop, pi = 0.8)
#' @export
FDP <- function(selprop, PFER, pi) {
  # Preparing objects
  if (is.matrix(selprop)) {
    selprop <- selprop[upper.tri(selprop)]
  }
  
  # Computing the number of stable edges
  S <- sum(selprop >= pi, na.rm = TRUE)
  
  # Computing the proportion of false discoveries among discoveries (False Discovery Proportion)
  if (S != 0) {
    FDP <- PFER / S
  } else {
    FDP <- 0
  }
  
  return(FDP)
}


StabilityScore <- function(selprop, pi_list = seq(0.6, 0.9, by = 0.01), K, n_cat = 3, group = NULL) {
  # Preparing objects
  if (is.matrix(selprop)) {
    selprop <- selprop[upper.tri(selprop)]
  }
  
  # Using group penalisation (extracting one per group)
  if (!is.null(group)) {
    selprop <- selprop[cumsum(group)]
  }
  
  # Computing the number of features (edges/variables)
  N <- sum(!is.na(selprop))
  
  # Computing the average number of selected features
  q <- round(sum(selprop, na.rm = TRUE))
  
  # Loop over the values of pi
  score <- rep(NA, length(pi_list))
  for (i in 1:length(pi_list)) {
    pi <- pi_list[i]
    
    # Computing the probabilities of being stable-in, stable-out or unstable under the null (uniform selection)
    p_vect <- BinomialProbabilities(q, N, pi, K, n_cat = n_cat)
    
    # Computing the log-likelihood
    if (any(is.na(p_vect))) {
      # Returning NA if not possible to compute (e.g. negative number of unstable features, as with pi<=0.5)
      l <- NA
    } else {
      if (n_cat == 2) {
        S_0 <- sum(selprop < pi, na.rm = TRUE) # Number of not stably selected features
        S_1 <- sum(selprop >= pi, na.rm = TRUE) # Number of stably selected features
        
        # Checking consistency
        if (S_0 + S_1 != N) {
          stop(paste0("Inconsistency in number of edges \n S_0+S_1=", S_0 + S_1, " instead of ", N))
        }
        
        # Log-likelihood
        l <- S_0 * p_vect$p_0 + S_1 * p_vect$p_1
      }
      
      if (n_cat == 3) {
        S_0 <- sum(selprop <= (1 - pi), na.rm = TRUE) # Number of stable-out features
        S_1 <- sum(selprop >= pi, na.rm = TRUE) # Number of stable-in features
        U <- sum((selprop < pi) & (selprop > (1 - pi)), na.rm = TRUE) # Number of unstable features
        
        # Checking consistency
        if (S_0 + S_1 + U != N) {
          stop(paste0("Inconsistency in number of edges \n S_0+S_1+U=", S_0 + S_1 + U, " instead of ", N))
        }
        
        # Log-likelihood
        l <- S_0 * p_vect$p_1 + U * p_vect$p_2 + S_1 * p_vect$p_3
      }
      
      # Re-formatting if infinite
      if (is.infinite(l)) {
        l <- NA
      }
    }
    
    # Getting the stability score
    score[i] <- -l
  }
  
  return(score)
}


#' Binomial probabilities for stability score
#'
#' Computes the probabilities of observing each category of selection
#' proportions under the assumption of a uniform selection procedure.
#'
#' @inheritParams StabilityScore
#' @param q average number of features selected by the underlying algorithm.
#' @param N total number of features.
#' @param pi threshold in selection proportions. If n_cat=3, these values must
#'   be >0.5 and <1. If n_cat=2, these values must be >0 and <1.
#'
#' @return A list of probabilities for each of the 2 or 3 categories of
#'   selection proportions.
#'
#' @export
BinomialProbabilities <- function(q, N, pi, K, n_cat = 3) {
  if (n_cat == 2) {
    # Definition of the threshold in selection counts
    thr <- round(K * pi) # Threshold above (>=) which the feature is stably selected
    
    # Probability of observing a selection count below thr_down under the null (uniform selection)
    p_0 <- stats::pbinom(thr - 1, size = K, prob = q / N, log.p = TRUE) # proportion < pi
    
    # Probability of observing a selection count above thr_up under the null
    p_1 <- stats::pbinom(thr - 1, size = K, prob = q / N, lower.tail = FALSE, log.p = TRUE) # proportion >= pi
    
    # Checking consistency between the three computed probabilities (should sum to 1)
    if (abs(exp(p_0) + exp(p_1) - 1) > 1e-3) {
      message(paste("N:", N))
      message(paste("q:", q))
      message(paste("K:", K))
      message(paste("pi:", pi))
      stop(paste0("Probabilities do not sum to 1 (Binomial distribution) \n p_0+p_1=", exp(p_0) + exp(p_1)))
    }
    
    # Output the two probabilities under the assumption of uniform selection procedure
    return(list(p_0 = p_0, p_1 = p_1))
  }
  
  if (n_cat == 3) {
    # Definition of the two thresholds in selection counts
    thr_down <- round(K * (1 - pi)) # Threshold below (<=) which the feature is stable-out
    thr_up <- round(K * pi) # Threshold above (>=) which the feature is stable-in
    
    # Probability of observing a selection count below thr_down under the null (uniform selection)
    p_1 <- stats::pbinom(thr_down, size = K, prob = q / N, log.p = TRUE) # proportion <= (1-pi)
    
    # Probability of observing a selection count between thr_down and thr_up under the null
    if ((thr_down) >= (thr_up - 1)) {
      # Not possible to compute (i.e. negative number of unstable features)
      p_2 <- NA
    } else {
      # Using cumulative probabilities
      p_2 <- log(stats::pbinom(thr_up - 1, size = K, prob = q / N) - stats::pbinom(thr_down, size = K, prob = q / N)) # 1-pi < proportion < pi
      
      # Using sum of probabilities (should not be necessary)
      if (is.infinite(p_2) | is.na(p_2)) {
        p_2 <- 0
        for (i in seq(thr_down + 1, thr_up - 1)) {
          p_2 <- p_2 + stats::dbinom(i, size = K, prob = q / N)
        }
        p_2 <- log(p_2)
      }
    }
    
    # Probability of observing a selection count above thr_up under the null
    p_3 <- stats::pbinom(thr_up - 1, size = K, prob = q / N, lower.tail = FALSE, log.p = TRUE) # proportion >= pi
    
    # Checking consistency between the three computed probabilities (should sum to 1)
    if (!is.na(p_2)) {
      if (abs(exp(p_1) + exp(p_2) + exp(p_3) - 1) > 1e-3) {
        message(paste("N:", N))
        message(paste("q:", q))
        message(paste("K:", K))
        message(paste("pi:", pi))
        stop(paste0("Probabilities do not sum to 1 (Binomial distribution) \n p_1+p_2+p_3=", exp(p_1) + exp(p_2) + exp(p_3)))
      }
    }
    
    # Output the three probabilities under the assumption of uniform selection procedure
    return(list(p_1 = p_1, p_2 = p_2, p_3 = p_3))
  }
}

CalibrationPlot <- function(stability, clustering = FALSE, metric = "both", block_id = NULL,
                            lines = FALSE, colours = c("ivory", "navajowhite", "tomato", "darkred"),
                            legend = TRUE, legend_length = 15, legend_range = NULL,
                            xlab = expression(lambda), ylab = expression(pi), zlab = expression(italic(q)),
                            filename = NULL, fileformat = "pdf", res = 500,
                            width = 7, height = 7, units = "in", ...) {
  # Extracting the number of blocks/components
  if ((stability$methods$type == "graphical_model") & (is.null(block_id))) {
    bigblocks <- BlockMatrix(stability$params$pk)
    bigblocks_vect <- bigblocks[upper.tri(bigblocks)]
    N_blocks <- unname(table(bigblocks_vect))
    blocks <- unique(as.vector(bigblocks_vect))
    names(N_blocks) <- blocks
    nblocks <- max(blocks)
    block_id <- 1:nblocks
  } else {
    block_id <- 1
  }
  nblocks <- length(block_id)
  
  # Saving as PDF
  if (!is.null(filename)) {
    if (fileformat == "pdf") {
      grDevices::pdf(filename, width = width, height = height)
    } else {
      grDevices::png(filename, width = width, height = height, res = res, units = units)
    }
  }
  
  if (metric == "both") {
    for (b in block_id) {
      # Extracting the stability scores
      if (clustering) {
        mat <- stability$Sc_2d
        if (length(unique(stability$Lambda)) > 1) {
          # Identifying best number of contributing variables
          lambda_hat <- stability$Lambda[which.max(stability$S), 1]
          ids <- which(as.character(stability$Lambda) == lambda_hat)
        } else {
          ids <- 1:nrow(stability$Sc)
        }
        mat <- mat[ids, ]
      } else {
        if (length(stability$params$pk) == 1) {
          mat <- stability$S_2d
          ids <- which(apply(mat, 1, FUN = function(x) {
            any(!is.na(x))
          }))
          mat <- mat[ids, , drop = FALSE]
        } else {
          mat <- stability$S_2d[, , b]
          ids <- which(apply(mat, 1, FUN = function(x) {
            any(!is.na(x))
          }))
          mat <- mat[ids, , drop = FALSE]
        }
      }
      mat <- mat[, , drop = FALSE]
      colnames(mat) <- stability$params$pi_list
      if (stability$methods$type == "clustering") {
        if (length(unique(stability$Lambda[, b])) > 1) {
          rownames(mat) <- paste0(stability$nc[, b], " - ", stability$Lambda[, b])[ids]
        } else {
          rownames(mat) <- (stability$nc[, b])[ids]
        }
      } else {
        if (grepl("penalised", tolower(stability$methods$implementation))) {
          rownames(mat) <- formatC(stability$Lambda[, b], format = "e", digits = 2)[ids]
        } else {
          rownames(mat) <- (stability$Lambda[, b])[ids]
        }
      }
      
      # Extracting corresponding numbers of selected variables (q)
      Q <- stability$Q[, b]
      Q <- Q[ids]
      
      # Heatmap representation
      Heatmap(t(mat[nrow(mat):1, ncol(mat):1]),
              colours = colours, axes = FALSE,
              legend = legend, legend_length = legend_length, legend_range = legend_range
      )
      
      # Identifying best pair of parameters
      withr::local_par(list(xpd = FALSE))
      if (stability$methods$type == "clustering") {
        if (clustering) {
          graphics::abline(v = nrow(mat) - which(stability$nc[ids, b] == Argmax(stability, clustering = clustering)[b, 1]) + 0.5, lty = 3)
        } else {
          tmp <- paste0(stability$nc[, b], " - ", stability$Lambda[, b])[ArgmaxId(stability, clustering = clustering)[1, 1]]
          graphics::abline(v = nrow(mat) - which(rownames(mat) == tmp) + 0.5, lty = 3)
        }
      } else {
        graphics::abline(v = nrow(mat) - which(stability$Lambda[ids, b] == Argmax(stability, clustering = clustering)[b, 1]) + 0.5, lty = 3)
      }
      graphics::abline(h = which.min(abs(as.numeric(colnames(mat)) - Argmax(stability, clustering = clustering)[b, 2])) - 0.5, lty = 3)
      
      # Including axes
      graphics::axis(
        side = 2, at = (1:ncol(mat)) - 0.5, las = 2,
        labels = formatC(as.numeric(colnames(mat)), format = "f", digits = 2), ...
      )
      if (grepl("penalised", tolower(stability$methods$implementation))) {
        graphics::axis(
          side = 3, at = (1:nrow(mat)) - 0.5, las = 2,
          labels = rev(formatC(Q, format = "f", big.mark = ",", digits = 0)), ...
        )
        graphics::axis(side = 1, at = (1:nrow(mat)) - 0.5, las = 2, labels = rev(rownames(mat)), ...)
      } else {
        graphics::axis(side = 1, at = (1:nrow(mat)) - 0.5, las = 2, labels = rev(rownames(mat)), ...)
      }
      
      # Including axis labels
      graphics::mtext(text = ylab, side = 2, line = 3.5, cex = 1.5)
      if (grepl("penalised", tolower(stability$methods$implementation))) {
        graphics::mtext(text = xlab, side = 1, line = 5.2, cex = 1.5)
        graphics::mtext(text = zlab, side = 3, line = 3.5, cex = 1.5)
      } else {
        graphics::mtext(text = xlab, side = 1, line = 3.5, cex = 1.5)
      }
    }
  } else {
    if (metric == "lambda") {
      for (b in block_id) {
        # Extracting the stability scores
        if (length(stability$params$pk) == 1) {
          mat <- stability$S_2d
          ids <- which(apply(mat, 1, FUN = function(x) {
            any(!is.na(x))
          }))
          mat <- mat[ids, , drop = FALSE]
        } else {
          mat <- stability$S_2d[, , b]
          ids <- which(apply(mat, 1, FUN = function(x) {
            any(!is.na(x))
          }))
          mat <- mat[ids, , drop = FALSE]
        }
        
        # Extracting the best stability score (with optimal pi) for each lambda value
        vect <- apply(mat, 1, max, na.rm = TRUE)
        
        # Extracting corresponding numbers of selected variables (q)
        Q <- stability$Q[, b, drop = FALSE]
        Q <- Q[ids]
        
        # Extracting corresponding lambda values
        Lambda <- stability$Lambda[ids, b, drop = FALSE]
        
        # Re-ordering by decreasing lambda
        ids <- sort.list(Lambda, decreasing = TRUE)
        Lambda <- Lambda[ids]
        Q <- Q[ids]
        vect <- vect[ids]
        
        # Using input ylab if not as default
        ylab <- ifelse(as.character(ylab) == as.character(expression(pi)), yes = "Stability Score", no = ylab)
        
        # Making plot
        cex_points <- 0.7
        plot(Lambda, vect,
             pch = 19, col = "navy", cex = cex_points,
             xlab = "", ylab = ylab, cex.lab = 1.5, xaxt = "n"
        )
        graphics::abline(h = graphics::axTicks(side = 2), lty = 3, col = "grey")
        graphics::abline(h = max(vect), lty = 2, col = "red")
        graphics::abline(v = which.max(vect), lty = 2, col = "red")
        graphics::points(Lambda, vect, pch = 19, col = "navy", cex = cex_points)
        if (lines) {
          graphics::lines(Lambda, vect, col = "navy")
        }
        
        # Adding x-axis and z-axis and their labels
        xseq <- seq(1, length(Lambda), length.out = 5)
        graphics::abline(v = Lambda[xseq], lty = 3, col = "grey")
        graphics::axis(side = 1, at = Lambda[xseq], labels = formatC(Lambda[xseq], format = "e", digits = 2), las = 2)
        graphics::axis(
          side = 3, at = Lambda[xseq], las = 2,
          labels = formatC(Q[xseq], format = "f", big.mark = ",", digits = 0)
        )
        graphics::mtext(text = xlab, side = 1, line = 5.2, cex = 1.5)
        graphics::mtext(text = zlab, side = 3, line = 3.5, cex = 1.5)
      }
    }
    
    if (metric == "pi") {
      for (b in block_id) {
        # Extracting the stability scores
        if (length(stability$params$pk) == 1) {
          mat <- stability$S_2d
          ids <- which(apply(mat, 1, FUN = function(x) {
            any(!is.na(x))
          }))
          mat <- mat[ids, , drop = FALSE]
        } else {
          mat <- stability$S_2d[, , b]
          ids <- which(apply(mat, 1, FUN = function(x) {
            any(!is.na(x))
          }))
          mat <- mat[ids, , drop = FALSE]
        }
        
        # Extracting the best stability score (with optimal lambda) for each pi value
        vect <- apply(mat, 2, max, na.rm = TRUE)
        
        # Using input ylab if not as default
        ylab <- ifelse(as.character(ylab) == as.character(expression(pi)), yes = "Stability Score", no = ylab)
        
        # Using input xlab if not as default
        xlab <- ifelse(as.character(xlab) == as.character(expression(lambda)), yes = expression(pi), no = ylab)
        
        # Making plot
        cex_points <- 0.7
        plot(1:length(vect), vect,
             pch = 19, col = "navy", cex = cex_points,
             xlab = "", ylab = ylab, cex.lab = 1.5, xaxt = "n"
        )
        xticks <- graphics::axTicks(side = 1)
        if (min(xticks) == 0) {
          xticks <- xticks + 1
        }
        graphics::abline(v = xticks, lty = 3, col = "grey")
        graphics::abline(h = graphics::axTicks(side = 2), lty = 3, col = "grey")
        graphics::abline(h = max(vect), lty = 2, col = "red")
        graphics::abline(v = which.max(vect), lty = 2, col = "red")
        graphics::points(1:length(vect), vect, pch = 19, col = "navy", cex = cex_points)
        if (lines) {
          graphics::lines(1:length(vect), vect, col = "navy")
        }
        
        # Adding x-axis and its labels
        graphics::axis(side = 1, at = xticks, labels = formatC(stability$params$pi_list[xticks], digits = 2), las = 2)
        graphics::mtext(text = xlab, side = 1, line = 5.2, cex = 1.5)
      }
    }
  }
  
  if (!is.null(filename)) {
    grDevices::dev.off()
  }
}


#' Heatmap visualisation
#'
#' Produces a heatmap for visualisation of matrix entries.
#'
#' @param mat data matrix.
#' @param colours vector of colours used for the heatmap. By default a gradient
#'   of colours ranging from ivory to dark red is used.
#' @param resolution number of different colours to use.
#' @param axes logical indicating if the row and column names of \code{mat}
#'   should be displayed.
#' @param legend logical indicating if the colour bar should be included.
#' @param legend_length length of the colour bar.
#' @param legend_range range of the colour bar.
#'
#' @return A heatmap.
#'
#' @seealso \code{\link{CalibrationPlot}}
#'
#' @examples
#' \dontrun{
#'
#' # Data simulation
#' set.seed(1)
#' mat <- matrix(rnorm(200), ncol = 20)
#'
#' # Generating heatmaps
#' Heatmap(mat = mat)
#' Heatmap(mat = mat, colours = c("lightgrey", "blue", "black"), legend = FALSE)
#' }
#'
#' @export
Heatmap <- function(mat, colours = c("ivory", "navajowhite", "tomato", "darkred"),
                    resolution = 10000, axes = TRUE,
                    legend = TRUE, legend_length = NULL, legend_range = NULL) {
  # Transposing the input matrix so that rows are rows
  mat <- t(mat)
  
  # Defining the legend length
  if (is.null(legend_length)) {
    legend_length <- ncol(mat)
  }
  
  # Preparing colours
  colours <- grDevices::colorRampPalette(colours)(resolution)
  names(colours) <- 1:resolution
  
  # Re-formatting matrix
  mat <- mat[, ncol(mat):1]
  vect <- as.vector(mat)
  
  # Defining extreme values
  if (is.null(legend_range)) {
    # myrange <- c(min(vect, na.rm = TRUE), max(vect, na.rm = TRUE))
    myrange <- range(vect, na.rm = TRUE)
    myrange <- c(floor(myrange[1]), ceiling(myrange[2]))
  } else {
    myrange <- legend_range
  }
  
  # Getting corresponding colours
  mycol <- as.character(cut(vect, breaks = seq(myrange[1], myrange[2], length.out = resolution + 1), labels = 1:resolution, include.lowest = TRUE))
  mycol_mat <- matrix(mycol, ncol = ncol(mat))
  
  # Making heatmap
  plot(NA,
       xlim = c(0, nrow(mycol_mat)), ylim = c(0, ncol(mycol_mat)),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
  )
  for (i in 0:(nrow(mycol_mat) - 1)) {
    for (j in 0:(ncol(mycol_mat) - 1)) {
      graphics::polygon(
        x = c(i, i + 1, i + 1, i), y = c(j, j, j + 1, j + 1),
        col = colours[mycol_mat[i + 1, j + 1]],
        border = colours[mycol_mat[i + 1, j + 1]]
      )
    }
  }
  if (axes) {
    if (!is.null(rownames(mat))) {
      graphics::axis(side = 1, at = 1:nrow(mat) - 0.5, labels = rownames(mat), las = 2)
    } else {
      graphics::axis(side = 1, at = c(1, nrow(mat)) - 0.5, labels = NA, las = 2)
    }
    if (!is.null(colnames(mat))) {
      graphics::axis(side = 2, at = 1:ncol(mat) - 0.5, labels = colnames(mat), las = 2)
    } else {
      graphics::axis(side = 2, at = c(1, ncol(mat)) - 0.5, labels = NA, las = 2)
    }
  }
  
  
  # Adding colour bar (legend)
  if (legend) {
    withr::local_par(list(xpd = TRUE))
    legend_width_factor <- 1.05
    mylegend_values <- grDevices::axisTicks(c(myrange[1], myrange[2]), log = FALSE)
    mylegend_ids <- as.numeric(as.character(cut(mylegend_values,
                                                breaks = seq(myrange[1], myrange[2], length.out = resolution + 1),
                                                labels = 1:resolution, include.lowest = TRUE
    )))
    ypos <- ncol(mat)
    xpos <- nrow(mat) * 1.05
    for (l in 1:length(colours)) {
      graphics::polygon(
        x = c(xpos, xpos * legend_width_factor, xpos * legend_width_factor, xpos),
        y = c(
          ypos - legend_length + legend_length * l / length(colours),
          ypos - legend_length + legend_length * l / length(colours),
          ypos - legend_length + legend_length * (l + 1) / length(colours),
          ypos - legend_length + legend_length * (l + 1) / length(colours)
        ),
        col = colours[l], border = colours[l]
      )
      if (l %in% mylegend_ids) {
        graphics::text(
          x = xpos * legend_width_factor, y = ypos - legend_length + legend_length * (l + 0.5) / length(colours),
          labels = paste0("- ", mylegend_values[which(mylegend_ids == l)]), adj = c(0, 0.5)
        )
      }
    }
    withr::local_par(list(xpd = FALSE)) # for legend
  }
}


ArgmaxId <- function(stability = NULL, S = NULL, clustering = FALSE) {
  if ((is.null(stability)) & (is.null(S))) {
    stop("Invalid input. One of the two arguments has to be specified: 'stability' or 'S'.")
  }
  if (clustering) {
    if (!is.null(S)) {
      stop("Invalid input. Argument 'stability' needs to be supplied with clustering = TRUE.")
    }
  }
  if (is.null(S)) {
    if (clustering) {
      if (length(unique(stability$Lambda)) > 1) {
        # Identifying best number of contributing variables
        lambda_hat <- stability$Lambda[which.max(stability$S), 1]
        ids <- which(as.character(stability$Lambda) == lambda_hat)
      } else {
        ids <- 1:nrow(stability$Sc)
      }
      Sc <- stability$Sc[ids, 1]
      Sc_2d <- stability$Sc_2d[ids, , drop = FALSE]
      
      # Identifying best number of clusters
      argmax_id <- matrix(NA, nrow = 1, ncol = 2)
      id <- which.max(Sc)
      argmax_id[, 1] <- ids[id]
      tmpSc <- Sc_2d[id, ]
      argmax_id[, 2] <- which.max(tmpSc)
    } else {
      argmax_id <- matrix(NA, nrow = ncol(stability$Lambda), ncol = 2)
      if (is.null(stability$params$lambda_other_blocks) & (length(stability$params$pk) > 1)) {
        id <- which.max(apply(stability$S, 1, sum, na.rm = TRUE))
        argmax_id[, 1] <- rep(id, nrow(argmax_id))
        for (block_id in 1:ncol(stability$Lambda)) {
          if (!is.na(stability$P[id, block_id])) {
            argmax_id[block_id, 2] <- which(stability$params$pi_list == stability$P[id, block_id])
          }
        }
      } else {
        for (block_id in 1:ncol(stability$Lambda)) {
          if (ncol(stability$Lambda) == 1) {
            myS <- stability$S
          } else {
            myS <- stability$S[, block_id, drop = FALSE]
          }
          myS[is.na(myS)] <- 0
          myid <- which.max(myS[, 1])
          argmax_id[block_id, ] <- c(myid, which(stability$params$pi_list == stability$P[myid, block_id]))
        }
      }
    }
  } else {
    argmax_id <- matrix(NA, nrow = 1, ncol = 2)
    myS <- apply(S, 1, max, na.rm = TRUE)
    myS[is.na(myS)] <- 0
    myid <- which.max(myS)
    argmax_id[1, ] <- c(myid, max(which(S[myid, ] == myS[myid])))
  }
  colnames(argmax_id) <- c("lambda_id", "pi_id")
  return(argmax_id)
}


#' Calibrated parameters
#'
#' Extracts calibrated parameter values in stability selection.
#'
#' @param stability output of \code{\link{VariableSelection}} or
#'   \code{\link{GraphicalModel}}.
#' @inheritParams ArgmaxId
#'
#' @return A matrix of parameter values. The first column (\code{lambda}])
#'   denotes the calibrated hyper-parameter of the underlying algorithm. The
#'   second column (\code{pi}) is the calibrated threshold in
#'   selection/co-membership proportions. In multi-block graphical modelling,
#'   rows correspond to different blocks.
#'
#' @family calibration functions
#' @seealso \code{\link{VariableSelection}}, \code{\link{GraphicalModel}}
#'
#' @examples
#' \dontrun{
#'
#' ## Graphical modelling
#'
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateGraphical(pk = 20)
#'
#' # Stability selection
#' stab <- GraphicalModel(xdata = simul$data)
#'
#' # Extracting calibrated parameters
#' args <- Argmax(stab)
#'
#'
#' ## Clustering
#'
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateClustering(
#'   n = c(10, 30, 15)
#' )
#'
#' # Consensus clustering
#' stab <- Clustering(xdata = simul$data, Lambda = c(1.1, 1.5, 2))
#'
#' # Extracting calibrated parameters for variable selection
#' Argmax(stab, clustering = FALSE)
#'
#' # Extracting calibrated parameters for co-membership
#' Argmax(stab, clustering = TRUE)
#' }
#'
#' @export
Argmax <- function(stability, clustering = FALSE) {
  argmax <- matrix(NA, nrow = ncol(stability$Lambda), ncol = 2)
  if (clustering) {
    id <- ArgmaxId(stability = stability, clustering = clustering)
    argmax[, 1] <- stability$nc[id[1], 1]
    argmax[, 2] <- stability$params$pi_list[id[2]]
  } else {
    if (is.null(stability$params$lambda_other_blocks) & (length(stability$params$pk) > 1)) {
      id <- which.max(apply(stability$S, 1, sum, na.rm = TRUE))
      argmax[, 1] <- stability$Lambda[id, ]
      argmax[, 2] <- stability$P[id, ]
    } else {
      for (block_id in 1:ncol(stability$Lambda)) {
        if (ncol(stability$Lambda) == 1) {
          myS <- stability$S
        } else {
          myS <- stability$S[, block_id, drop = FALSE]
        }
        myS[is.na(myS)] <- 0
        myid <- which.max(myS[, 1])
        argmax[block_id, ] <- c(stability$Lambda[myid, block_id], stability$P[myid, block_id])
      }
    }
  }
  colnames(argmax) <- c("lambda", "pi")
  return(argmax)
}


#' Calibrated adjacency matrix
#'
#' Builds the adjacency matrix of the (calibrated) stability selection graphical
#' model.
#'
#' @param stability output of \code{\link{GraphicalModel}}.
#' @param argmax_id optional matrix of parameter IDs. If \code{argmax_id=NULL},
#'   the calibrated adjacency matrix is returned.
#'
#' @return A binary and symmetric adjacency matrix encoding an undirected graph
#'   with no self-loops.
#'
#' @family calibration functions
#' @seealso \code{\link{GraphicalModel}}
#'
#' @examples
#' \dontrun{
#'
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateGraphical(pk = 20)
#'
#' # Stability selection
#' stab <- GraphicalModel(xdata = simul$data)
#'
#' # Calibrated adjacency matrix
#' A <- Adjacency(stab)
#'
#' # User-defined parameters
#' myids <- matrix(c(20, 10), nrow = 1)
#' stab$Lambda[myids[1], 1] # corresponding penalty
#' stab$params$pi_list[myids[2]] # corresponding threshold
#' A <- Adjacency(stab, argmax_id = myids)
#' }
#'
#' @export
Adjacency <- function(stability, argmax_id = NULL) {
  if (class(stability) == "bi_selection") {
    if ("selectedY" %in% names(stability)) {
      A <- Square(cbind(stability$selectedX, stability$selectedY))
    } else {
      A <- Square(stability$selectedX)
    }
  } else {
    if (stability$methods$type == "graphical_model") {
      A <- matrix(0, ncol = ncol(stability$selprop), nrow = nrow(stability$selprop))
    } else {
      A <- matrix(0, ncol = ncol(stability$coprop), nrow = nrow(stability$coprop))
    }
    bigblocks <- BlockMatrix(stability$params$pk)
    if (is.null(argmax_id)) {
      if (stability$methods$type == "graphical_model") {
        argmax_id <- ArgmaxId(stability)
        argmax <- Argmax(stability)
      } else {
        argmax_id <- ArgmaxId(stability, clustering = TRUE)
        argmax <- Argmax(stability, clustering = TRUE)
      }
    } else {
      argmax <- NULL
      for (block_id in 1:ncol(stability$Lambda)) {
        argmax <- rbind(argmax, c(
          stability$Lambda[argmax_id[block_id, 1], block_id],
          stability$params$pi_list[argmax_id[block_id, 2]]
        ))
      }
    }
    for (block_id in 1:ncol(stability$Lambda)) {
      if (stability$methods$type == "graphical_model") {
        A_block <- ifelse(stability$selprop[, , argmax_id[block_id, 1]] >= argmax[block_id, 2], 1, 0)
      } else {
        A_block <- ifelse(stability$coprop[, , argmax_id[block_id, 1]] >= argmax[block_id, 2], 1, 0)
      }
      A_block[lower.tri(A_block)] <- 0
      A_block <- A_block + t(A_block) # for symmetry
      if (length(stability$params$pk) > 1) {
        A_block[bigblocks != block_id] <- 0
      }
      A <- A + A_block
    }
  }
  A[is.na(A)] <- 0
  return(A)
}


#' Set of stably selected variables
#'
#' Builds the (calibrated) set of stably selected variables.
#'
#' @inheritParams Adjacency
#' @param stability output of \code{\link{VariableSelection}},
#'   or \code{\link{BiSelection}}.
#'
#' @return A binary vector encoding the selection status of the variables
#'   (\code{1} if selected, \code{0} otherwise).
#'
#' @family calibration functions
#' @seealso \code{\link{VariableSelection}}, \code{\link{BiSelection}}
#'
#' @examples
#' \dontrun{
#'
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateRegression(pk = 50)
#'
#' # Stability selection
#' stab <- VariableSelection(xdata = simul$xdata, ydata = simul$ydata)
#'
#' # Calibrated set
#' selected <- SelectedVariables(stab)
#'
#' # User-defined parameters
#' myids <- matrix(c(80, 10), nrow = 1)
#' stab$Lambda[myids[1], 1] # corresponding penalty
#' stab$params$pi_list[myids[2]] # corresponding threshold
#' selected <- SelectedVariables(stab, argmax_id = myids)
#' }
#' @export
SelectedVariables <- function(stability, argmax_id = NULL) {
  if (class(stability) == "clustering") {
    selprop <- SelectionProportions(stability, argmax_id = argmax_id)
    if (any(selprop != 1)) {
      score <- StabilityScore(
        selprop = selprop,
        K = stability$params$K,
        pi_list = stability$params$pi_list,
        n_cat = stability$params$n_cat
      )
      stability_selected <- ifelse(selprop >= stability$params$pi_list[which.max(score)],
                                   yes = 1, no = 0
      )
    } else {
      stability_selected <- selprop
    }
  }
  
  if (class(stability) %in% c("graphical_model", "variable_selection")) {
    if (is.null(argmax_id)) {
      argmax_id <- ArgmaxId(stability)
    }
    stability_selected <- ifelse(stability$selprop[argmax_id[1], ] >= stability$params$pi_list[argmax_id[2]],
                                 yes = 1, no = 0
    )
  }
  
  if (class(stability) == "bi_selection") {
    stability_selected <- stability$selectedX
  }
  
  return(stability_selected)
}


#' Stable cluster membership
#'
#' Builds the (calibrated) stable clusters as connected components of the graph
#' defined from stable co-membership.
#'
#' @inheritParams Adjacency
#' @param adjacency adjacency matrix or output of \code{\link{GraphicalModel}}
#'   or \code{\link{Clustering}}.
#'
#' @return A vector encoding the cluster membership.
#'
#' @family calibration functions
#' @seealso \code{\link{Clustering}}
#'
#' @examples
#' \dontrun{
#'
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateGraphical(pk = 50, n = 10)
#'
#' # Consensus clustering
#' stab <- Clustering(xdata = simul$data, implementation = HierarchicalClustering)
#'
#' # Stable cluster membership
#' mymembership <- Clusters(stab)
#' }
#' @export
Clusters <- function(adjacency = NULL, argmax_id = NULL) {
  if (!is.matrix(adjacency)) {
    # Computing stable co-membership matrix
    adjacency <- Adjacency(stability = adjacency, argmax_id = argmax_id)
  }
  
  # Extracting stable connected components
  mymembership <- igraph::components(Graph(adjacency, satellites = TRUE))$membership
  
  return(mymembership)
}


#' Selection proportions
#'
#' Extracts the selection proportions of the (calibrated) stability selection
#' model or co-membership proportions of the (calibrated) consensus clustering
#' model.
#'
#' @inheritParams Adjacency
#' @param stability output of \code{\link{VariableSelection}},
#'   \code{\link{GraphicalModel}}, or \code{\link{BiSelection}}.
#'
#' @return A symmetric matrix (graphical model) or vector (variable selection)
#'   of selection proportions.
#'
#' @family calibration functions
#' @seealso \code{\link{VariableSelection}}, \code{\link{GraphicalModel}}
#'
#' @examples
#' \dontrun{
#'
#' ## Variable selection
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateRegression(pk = 50)
#'
#' # Stability selection
#' stab <- VariableSelection(xdata = simul$xdata, ydata = simul$ydata)
#'
#' # Calibrated selection proportions
#' prop <- SelectionProportions(stab)
#'
#' # User-defined parameters
#' myids <- matrix(c(80, 10), nrow = 1)
#' stab$Lambda[myids[1], 1] # corresponding penalty
#' stab$params$pi_list[myids[2]] # corresponding threshold
#' prop <- SelectionProportions(stab, argmax_id = myids)
#'
#' ## Graphical model
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateGraphical(pk = 20)
#'
#' # Stability selection
#' stab <- GraphicalModel(xdata = simul$data)
#'
#' # Calibrated adjacency matrix
#' prop <- SelectionProportions(stab)
#'
#' # User-defined parameters
#' myids <- matrix(c(20, 10), nrow = 1)
#' stab$Lambda[myids[1], 1] # corresponding penalty
#' stab$params$pi_list[myids[2]] # corresponding threshold
#' prop <- SelectionProportions(stab, argmax_id = myids)
#' }
#'
#' @export
SelectionProportions <- function(stability, argmax_id = NULL) {
  out <- NULL
  
  if (stability$methods$type == "graphical_model") {
    out <- SelectionProportionsGraphical(stability = stability, argmax_id = argmax_id)
  }
  if (stability$methods$type == "variable_selection") {
    out <- SelectionProportionsRegression(stability = stability, argmax_id = argmax_id)
  }
  if (stability$methods$type == "clustering") {
    argmax_id <- ArgmaxId(stability)
    out <- SelectionProportionsRegression(stability = stability, argmax_id = argmax_id)
  }
  if (stability$methods$type == "bi_selection") {
    out <- stability$selpropX
  }
  
  return(out)
}


#' Selection proportions (graphical model)
#'
#' Extracts the selection proportions of the (calibrated) stability selection
#' model.
#'
#' @inheritParams Adjacency
#' @param stability output of \code{\link{GraphicalModel}}.
#'
#' @return A symmetric matrix.
#'
#' @keywords internal
SelectionProportionsGraphical <- function(stability, argmax_id = NULL) {
  A <- matrix(0, ncol = ncol(stability$selprop), nrow = nrow(stability$selprop))
  bigblocks <- BlockMatrix(stability$params$pk)
  if (is.null(argmax_id)) {
    argmax_id <- ArgmaxId(stability)
    argmax <- Argmax(stability)
  } else {
    argmax <- NULL
    for (block_id in 1:ncol(stability$Lambda)) {
      argmax <- rbind(argmax, c(
        stability$Lambda[argmax_id[block_id, 1], ],
        stability$params$pi_list[argmax_id[block_id, 2]]
      ))
    }
  }
  for (block_id in 1:ncol(stability$Lambda)) {
    A_block <- stability$selprop[, , argmax_id[block_id, 1]]
    A_block[lower.tri(A_block)] <- 0
    A_block <- A_block + t(A_block) # for symmetry
    if (length(stability$params$pk) > 1) {
      A_block[bigblocks != block_id] <- 0
    }
    A <- A + A_block
  }
  return(A)
}


#' Selection proportions (variable selection)
#'
#' Extracts the selection proportions of the (calibrated) stability selection
#' model.
#'
#' @inheritParams Adjacency
#' @param stability output of \code{\link{VariableSelection}}.
#'
#' @return A vector of selection proportions.
#'
#' @keywords internal
SelectionProportionsRegression <- function(stability, argmax_id = NULL) {
  if (is.null(argmax_id)) {
    argmax_id <- ArgmaxId(stability)
  }
  m <- stability$selprop[argmax_id[1], ]
  return(m)
}


#' Model coefficients
#'
#' Extracts the coefficients of visited models at different resampling
#' iterations of a stability selection run. This function can be applied to the
#' output of \code{\link{VariableSelection}}.
#'
#' @param stability output of \code{\link{VariableSelection}}.
#' @param side character string indicating if coefficients of the predictor
#'   (\code{side="X"}) or outcome (\code{side="Y"}) coefficients should be
#'   returned. Option \code{side="Y"} is only applicable to PLS models.
#' @param comp component ID. Only applicable to PLS models.
#' @param iterations vector of iteration IDs. If \code{iterations=NULL}, the
#'   coefficients of all visited models are returned. This number must be
#'   smaller than the number of iterations \code{K} used for the stability
#'   selection run.
#'
#' @seealso \code{\link{VariableSelection}}
#'
#' @examples
#' \dontrun{
#'
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateRegression(n = 100, pk = 50, family = "gaussian")
#'
#' # Stability selection
#' stab <- VariableSelection(xdata = simul$xdata, ydata = simul$ydata, family = "gaussian")
#'
#' # Coefficients of visited models
#' coefs <- Coefficients(stab)
#' dim(coefs)
#'
#' # Coefficients of the first fitted model
#' coefs <- Coefficients(stab, iterations = 1)
#' dim(coefs)
#'
#' # Stability selection
#' stab <- VariableSelection(
#'   xdata = simul$xdata, ydata = simul$ydata,
#'   implementation = SparsePLS, family = "gaussian"
#' )
#'
#' # Coefficients of visited models
#' coefs <- Coefficients(stab, side = "Y", )
#' dim(coefs)
#' }
#' @export
Coefficients <- function(stability, side = "X", comp = 1, iterations = NULL) {
  if (is.null(iterations)) {
    iterations <- seq(1, dim(stability$Beta)[3])
  } else {
    iterations <- iterations[iterations <= dim(stability$Beta)[3]]
  }
  if (length(iterations) == 0) {
    stop("Invalid input for argument 'iterations'. This argument must be a number smaller than the number of iterations used for the stability selection run.")
  }
  if (ncol(stability$Beta) == stability$params$pk) {
    return(stability$Beta[, , iterations, drop = FALSE])
  } else {
    if (!side %in% c("X", "Y")) {
      warning("Invalid input for argument 'side'. The default value ('X') was used.")
      side <- "X"
    }
    side_id <- grepl(paste0(side, "_"), colnames(stability$Beta))
    comp_id <- grepl(paste0("_PC", comp), colnames(stability$Beta))
    return(stability$Beta[, side_id & comp_id, iterations, drop = FALSE])
  }
}


Graph <- function(adjacency, node_label = NULL, node_colour = NULL, node_shape = NULL,
                  edge_colour = "grey60", label_colour = "grey20",
                  mode = "undirected", weighted = FALSE, satellites = FALSE) {
  # Checking input values (weighted)
  if (!is.null(weighted)) {
    if (!weighted %in% c(TRUE, FALSE)) {
      stop("Invalid input for argument 'weighted'. Possible values are: NULL, TRUE or FALSE.")
    }
  }
  
  # Extracting the adjacency matrix from the output of GraphicalModel()
  if (!is.matrix(adjacency)) {
    stability <- adjacency
    adjacency <- Adjacency(stability = adjacency)
  }
  
  # Setting row and column names if none
  if (is.null(rownames(adjacency)) & (is.null(colnames(adjacency)))) {
    rownames(adjacency) <- colnames(adjacency) <- paste0("var", 1:ncol(adjacency))
  } else {
    if (is.null(rownames(adjacency))) {
      rownames(adjacency) <- colnames(adjacency)
    }
    if (is.null(colnames(adjacency))) {
      colnames(adjacency) <- rownames(adjacency)
    }
  }
  
  # Checking input values (node label)
  if (!is.null(node_label)) {
    if (length(node_label) != ncol(adjacency)) {
      stop(paste0(
        "Invalid input for argument 'node_label'. It must be a vector of length ",
        ncol(adjacency), " (the same as the number of nodes in the adjacency matrix)."
      ))
    }
  }
  
  # Checking input values (node colour)
  if (!is.null(node_colour)) {
    if (length(node_colour) == 1) {
      node_colour <- rep(node_colour, ncol(adjacency))
    } else {
      if (length(node_colour) != ncol(adjacency)) {
        stop(paste0(
          "Invalid input for argument 'node_colour'. It must be a vector of length ",
          ncol(adjacency), " (the same as the number of nodes in the adjacency matrix)."
        ))
      }
    }
  }
  
  # Checking input values (node shape)
  if (!is.null(node_shape)) {
    if (length(node_shape) == 1) {
      node_shape <- rep(node_shape, ncol(adjacency))
    } else {
      if (length(node_shape) != ncol(adjacency)) {
        stop(paste0(
          "Invalid input for argument 'node_shape'. It must be a vector of length ",
          ncol(adjacency), " (the same as the number of nodes in the adjacency matrix)."
        ))
      }
    }
    
    if (!any(node_shape %in% c("circle", "square", "triangle", "star"))) {
      stop(paste0("Invalid input for argument 'node_shape'. Possible values for the entries of the vector are: circle, square, triangle or star."))
    }
  }
  
  # Adding shapes if required
  if (!is.null(node_shape)) {
    if (any(node_shape == "star")) {
      igraph::add_shape("star",
                        clip = igraph::shape_noclip,
                        plot = mystar, parameters = list(vertex.norays = 5)
      )
    }
    
    if (any(node_shape == "triangle")) {
      igraph::add_shape("triangle",
                        clip = igraph::shape_noclip,
                        plot = mytriangle
      )
    }
  }
  
  # Default node colours
  if (is.null(node_colour)) {
    node_colour <- rep("skyblue", ncol(adjacency))
    if (exists("stability")) {
      if (class(stability) == "bi_selection") {
        node_colour <- ifelse(grepl("comp", colnames(adjacency)),
                              yes = "darkmagenta", no = "skyblue"
        )
        node_colour[colnames(adjacency) %in% colnames(stability$selectedY)] <- "tomato"
      }
    }
  }
  
  # Default node shapes
  if (is.null(node_shape)) {
    node_shape <- rep("circle", ncol(adjacency))
  }
  
  # Default node labels
  if (is.null(node_label)) {
    node_label <- colnames(adjacency)
  }
  
  # Formatting node characteristics
  names(node_colour) <- colnames(adjacency)
  names(node_label) <- colnames(adjacency)
  names(node_shape) <- colnames(adjacency)
  
  # Formatting adjacency matrix
  if (!is.null(weighted)) {
    if (!weighted) {
      adjacency <- ifelse(adjacency != 0, yes = 1, no = 0)
      weighted <- NULL
    }
  } else {
    adjacency <- round(adjacency)
  }
  
  # Estimating igraph object
  mygraph <- igraph::graph_from_adjacency_matrix(adjacency, mode = mode, weighted = weighted)
  mydegrees <- igraph::degree(mygraph)
  
  # Changing arrow size for directed graphs
  if (mode == "directed") {
    igraph::E(mygraph)$arrow.size <- 0.2
  }
  
  # Including/excluding satellites (nodes with no edges)
  if (!satellites) {
    mygraph <- igraph::delete.vertices(mygraph, v = names(mydegrees)[mydegrees == 0])
  }
  
  # Formatting vertices
  mydegrees <- igraph::degree(mygraph)
  igraph::V(mygraph)$size <- as.numeric(as.character(cut(mydegrees, breaks = 4, labels = c(3, 4, 5, 6))))
  igraph::V(mygraph)$label <- node_label[igraph::V(mygraph)$name]
  igraph::V(mygraph)$color <- node_colour[igraph::V(mygraph)$name]
  igraph::V(mygraph)$shape <- node_shape[igraph::V(mygraph)$name]
  igraph::V(mygraph)$frame.color <- igraph::V(mygraph)$color
  igraph::V(mygraph)$label.family <- "sans"
  igraph::V(mygraph)$label.cex <- as.numeric(as.character(cut(mydegrees, breaks = 4, labels = c(0.4, 0.45, 0.5, 0.55))))
  igraph::V(mygraph)$label.color <- label_colour
  
  # Formatting edges
  igraph::E(mygraph)$color <- edge_colour
  if (is.null(weighted)) {
    igraph::E(mygraph)$width <- 0.5
  } else {
    igraph::E(mygraph)$width <- igraph::E(mygraph)$weight
  }
  
  return(mygraph)
}


#' Star-shaped nodes
#'
#' Produces star-shaped nodes in an igraph object.
#'
#' @param coords a matrix of coordinates
#' (see \code{\link[igraph]{add_shape}}).
#' @param v a vector of node IDs
#' (see \code{\link[igraph]{add_shape}}).
#' @param params node graphical parameters
#' (see \code{\link[igraph]{add_shape}}).
#'
#' @seealso \code{\link[igraph]{add_shape}}
#'
#' @keywords internal
mystar <- function(coords, v = NULL, params) {
  vertex.color <- params("vertex", "color")
  if ((length(vertex.color) != 1) & !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1 / 200 * params("vertex", "size")
  if ((length(vertex.size) != 1) & !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  norays <- params("vertex", "norays")
  if ((length(norays) != 1) & !is.null(v)) {
    norays <- norays[v]
  }
  
  mapply(coords[, 1], coords[, 2], vertex.color, vertex.size, norays,
         FUN = function(x, y, bg, size, nor) {
           graphics::symbols(
             x = x, y = y, fg = bg, bg = bg,
             stars = matrix(c(size, size / 2), nrow = 1, ncol = nor * 2),
             add = TRUE, inches = FALSE
           )
         }
  )
}


#' Triangular nodes
#'
#' Produces triangular nodes in an igraph object.
#'
#' @param coords a matrix of coordinates
#' (see \code{\link[igraph]{add_shape}}).
#' @param v a vector of node IDs
#' (see \code{\link[igraph]{add_shape}}).
#' @param params node graphical parameters
#' (see \code{\link[igraph]{add_shape}}).
#'
#' @seealso \code{\link[igraph]{add_shape}}
#'
#' @keywords internal
mytriangle <- function(coords, v = NULL, params) {
  vertex.color <- params("vertex", "color")
  if ((length(vertex.color) != 1) & !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1 / 200 * params("vertex", "size")
  if ((length(vertex.size) != 1) & !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  norays <- 3
  
  mapply(coords[, 1], coords[, 2], vertex.color, vertex.size, norays,
         FUN = function(x, y, bg, size, nor) {
           graphics::symbols(
             x = x, y = y, fg = bg, bg = bg,
             stars = matrix(c(size, size / 2), nrow = 1, ncol = nor * 2),
             add = TRUE, inches = FALSE
           )
         }
  )
}


ClusteringPerformance <- function(theta, theta_star, pk = NULL) {
  # Initialising unused parameters
  cor <- NULL
  thr <- 0.5
  
  # Computing co-membership matrices
  theta <- CoMembership(theta)
  theta_star <- CoMembership(theta_star)
  
  # Storing similarities/differences between estimated and true sets
  Asum <- theta + 2 * theta_star
  
  # Extracting block-specific performances
  if (is.null(pk)) {
    tmp <- SelectionPerformanceSingle(Asum, cor = cor, thr = thr)
    rand <- (tmp$TP + tmp$TN) / (tmp$TP + tmp$FP + tmp$TN + tmp$FN)
    tmp <- cbind(tmp, rand = rand)
    return(tmp)
  } else {
    Asum_vect <- Asum[upper.tri(Asum)]
    bigblocks <- BlockMatrix(pk)
    bigblocks_vect <- bigblocks[upper.tri(bigblocks)]
    if (!is.null(cor)) {
      cor_vect <- cor[upper.tri(cor)]
    } else {
      cor_vect <- NULL
    }
    
    tmp <- SelectionPerformanceSingle(Asum, cor = cor, thr = thr)
    rand <- (tmp$TP + tmp$TN) / (tmp$TP + tmp$FP + tmp$TN + tmp$FN)
    tmp <- cbind(tmp, rand = rand)
    out <- tmp
    for (k in sort(unique(bigblocks_vect))) {
      tmp <- SelectionPerformanceSingle(Asum_vect[bigblocks_vect == k],
                                        cor = cor_vect[bigblocks_vect == k], thr = thr
      )
      rand <- (tmp$TP + tmp$TN) / (tmp$TP + tmp$FP + tmp$TN + tmp$FN)
      tmp <- cbind(tmp, rand = rand)
      out <- rbind(out, tmp)
    }
    
    return(out)
  }
}


SelectionPerformanceSingle <- function(Asum, cor = NULL, thr = 0.5) {
  # Asum is an adjacency matrix with 3 for TP, 2 for FN, 1 for FP, and 0 for TN
  
  # Preparing objects
  if (is.matrix(Asum)) {
    p <- ncol(Asum)
    N <- p * (p - 1) / 2
    Asum <- Asum[upper.tri(Asum)]
  } else {
    N <- length(Asum)
  }
  
  # Computing the numbers of True/False Positives/Negatives
  TP <- sum(Asum == 3)
  FN <- sum(Asum == 2)
  FP <- sum(Asum == 1)
  TN <- sum(Asum == 0)
  
  # Separation between correlated and independent features based on a threshold in correlation
  if (!is.null(cor)) {
    if (is.matrix(cor)) {
      cor_vect <- cor[upper.tri(cor)]
    } else {
      cor_vect <- cor
    }
    FP_c <- sum((Asum == 1) & (abs(cor_vect) >= thr))
    FP_i <- sum((Asum == 1) & (abs(cor_vect) < thr))
  }
  
  # Computing performances in selection
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  accuracy <- (TP + TN) / N
  if (TP + FP > 0) {
    precision <- TP / (TP + FP)
  } else {
    precision <- 0
  }
  if ((TP + FN) > 0) {
    recall <- TP / (TP + FN)
  } else {
    recall <- 1
  }
  if ((precision > 0) | (recall > 0)) {
    F1_score <- 2 * precision * recall / (precision + recall)
  } else {
    F1_score <- 0
  }
  
  if (is.null(cor)) {
    return(data.frame(
      TP = TP, FN = FN, FP = FP, TN = TN,
      sensitivity = sensitivity, specificity = specificity,
      accuracy = accuracy, precision = precision, recall = recall, F1_score = F1_score
    ))
  } else {
    return(data.frame(
      TP = TP, FN = FN, FP = FP, TN = TN, FP_c = FP_c, FP_i = FP_i,
      sensitivity = sensitivity, specificity = specificity,
      accuracy = accuracy, precision = precision, recall = recall, F1_score = F1_score
    ))
  }
}


library(huge)

SparseHierarchicalClustering <- function(xdata, nc = NULL, Lambda, scale = TRUE, rows = TRUE, ...) {
  # Checking sparcl package is installed
  if (!requireNamespace("sparcl")) {
    stop("This function requires the 'sparcl' package.")
  }
  
  # Storing extra arguments
  extra_args <- list(...)
  
  # Transposing for clustering of columns
  if (!rows) {
    xdata <- t(xdata)
  }
  
  # Scaling the data
  if (scale) {
    xdata <- scale(xdata)
  }
  
  # Re-formatting Lambda
  if (is.vector(Lambda)) {
    Lambda <- cbind(Lambda)
  }
  
  # Re-formatting nc
  if (!is.null(nc)) {
    if (is.vector(nc)) {
      nc <- cbind(nc)
    }
  } else {
    nc <- cbind(seq(1, nrow(xdata)))
  }
  
  # Extracting relevant extra arguments (hclust)
  ids <- which(names(extra_args) %in% names(formals(stats::hclust)))
  ids <- ids[!ids %in% c("x", "wbound", "silent")]
  
  # Initialisation of array storing co-membership matrices
  adjacency <- array(NA, dim = c(nrow(xdata), nrow(xdata), nrow(nc) * nrow(Lambda)))
  weight <- matrix(NA, nrow = nrow(nc) * nrow(Lambda), ncol = ncol(xdata))
  
  # Iterating over the pair of parameters
  id <- 0
  for (i in 1:nrow(Lambda)) {
    # Running sparse hierarchical clustering
    myclust <- do.call(sparcl::HierarchicalSparseCluster, args = c(
      list(x = xdata, wbound = Lambda[i, 1], silent = TRUE),
      extra_args[ids]
    ))
    
    # Defining clusters
    mygroups <- do.call(stats::cutree, args = list(tree = myclust$hc, k = nc))
    if (is.null(dim(mygroups))) {
      mygroups <- cbind(mygroups)
    }
    for (j in 1:nrow(nc)) {
      adjacency[, , id + j] <- CoMembership(groups = mygroups[, j])
      weight[id + j, ] <- myclust$ws[, 1]
    }
    id <- id + nrow(nc)
  }
  
  # Setting row and column names
  rownames(weight) <- paste0("s", seq(0, nrow(weight) - 1))
  colnames(weight) <- colnames(xdata)
  
  return(list(comembership = adjacency, weight = weight))
}


VariableSelection <- function(xdata, ydata = NULL, Lambda = NULL, pi_list = seq(0.6, 0.9, by = 0.01),
                              K = 100, tau = 0.5, seed = 1, n_cat = 3,
                              family = "gaussian", implementation = PenalisedRegression,
                              resampling = "subsampling", PFER_method = "MB", PFER_thr = Inf, FDP_thr = Inf,
                              Lambda_cardinal = 100, group_x = NULL, group_penalisation = FALSE,
                              n_cores = 1, output_data = FALSE, verbose = TRUE, ...) {
  # Defining Lambda if used with sparse PCA or PLS
  if (is.null(Lambda)) {
    if (as.character(substitute(implementation)) %in% c("SparseGroupPLS", "GroupPLS")) {
      Lambda <- seq(1, length(group_x) - 1)
    }
    if (as.character(substitute(implementation)) %in% c("SparsePLS", "SparsePCA")) {
      Lambda <- seq(1, ncol(xdata) - 1)
    }
  }
  
  # Object preparation, error and warning messages
  CheckInputRegression(
    xdata = xdata, ydata = ydata, Lambda = Lambda, pi_list = pi_list,
    K = K, tau = tau, seed = seed, n_cat = n_cat,
    family = family, implementation = implementation,
    resampling = resampling, PFER_method = PFER_method,
    PFER_thr = PFER_thr, FDP_thr = FDP_thr,
    Lambda_cardinal = Lambda_cardinal,
    verbose = verbose
  )
  
  # Checking that group_x is provided for group penalisation
  if (group_penalisation) {
    if (is.null(group_x)) {
      stop("Please provide argument 'group_x' for group penalisation. Argument 'group_x' should be a vector with the number of variables in each group.")
    }
  }
  
  if (is.null(Lambda)) {
    # Defining grid of lambda values (using glmnet implementation)
    Lambda <- LambdaGridRegression(
      xdata = xdata, ydata = ydata, tau = tau, seed = seed,
      family = family,
      resampling = resampling,
      Lambda_cardinal = Lambda_cardinal, check_input = FALSE, ...
    )
  }
  
  # Check if parallelisation is possible (forking)
  if (.Platform$OS.type != "unix") {
    if (n_cores > 1) {
      warning("Invalid input for argument 'n_cores'. Parallelisation relies on forking, it is only available on Unix systems.")
    }
    n_cores <- 1
  }
  
  # Stability selection and score
  mypar <- parallel::mclapply(X = 1:n_cores, FUN = function(k) {
    return(SerialRegression(
      xdata = xdata, ydata = ydata, Lambda = Lambda, pi_list = pi_list,
      K = ceiling(K / n_cores), tau = tau, seed = as.numeric(paste0(seed, k)), n_cat = n_cat,
      family = family, implementation = implementation, resampling = resampling,
      PFER_method = PFER_method, PFER_thr = PFER_thr, FDP_thr = FDP_thr,
      group_x = group_x, group_penalisation = group_penalisation,
      output_data = output_data, verbose = verbose, ...
    ))
  })
  
  # Combining the outputs from parallel iterations
  out <- mypar[[1]]
  if (n_cores > 1) {
    for (i in 2:length(mypar)) {
      out <- do.call(Combine, list(stability1 = out, stability2 = mypar[[2]]))
    }
  }
  
  # Re-set the function names
  if ("methods" %in% names(out)) {
    myimplementation <- as.character(substitute(implementation))
    if (is.function(resampling)) {
      myresampling <- as.character(substitute(resampling))
    } else {
      myresampling <- resampling
    }
    out$methods$implementation <- myimplementation
    out$methods$resampling <- myresampling
  }
  
  # Defining the class
  class(out) <- "variable_selection"
  
  return(out)
}


#' Stability selection in regression (internal)
#'
#' Runs stability selection regression models with different combinations of
#' parameters controlling the sparsity of the underlying selection algorithm
#' (e.g. penalty parameter for regularised models) and thresholds in selection
#' proportions. These two parameters are jointly calibrated by maximising the
#' stability score of the model (possibly under a constraint on the expected
#' number of falsely stably selected features). This function uses a serial
#' implementation and requires the grid of parameters controlling the underlying
#' algorithm as input (for internal use only).
#'
#' @inheritParams VariableSelection
#' @param Lambda matrix of parameters controlling the level of sparsity in the
#'   underlying feature selection algorithm specified in \code{implementation}.
#'   With \code{implementation="glmnet"}, \code{Lambda} contains penalty
#'   parameters.
#'
#' @return A list with: \item{S}{a matrix of the best stability scores for
#'   different parameters controlling the level of sparsity in the underlying
#'   algorithm.} \item{Lambda}{a matrix of parameters controlling the level of
#'   sparsity in the underlying algorithm.} \item{Q}{a matrix of the average
#'   number of selected features by underlying algorithm with different
#'   parameters controlling the level of sparsity.} \item{Q_s}{a matrix of the
#'   calibrated number of stably selected features with different parameters
#'   controlling the level of sparsity.} \item{P}{a matrix of calibrated
#'   thresholds in selection proportions for different parameters controlling
#'   the level of sparsity in the underlying algorithm.} \item{PFER}{a matrix of
#'   the upper-bounds in PFER of calibrated stability selection models with
#'   different parameters controlling the level of sparsity.} \item{FDP}{a
#'   matrix of the upper-bounds in FDP of calibrated stability selection models
#'   with different parameters controlling the level of sparsity.} \item{S_2d}{a
#'   matrix of stability scores obtained with different combinations of
#'   parameters. Columns correspond to different tresholds in selection
#'   proportions.} \item{selprop}{a matrix of selection proportions. Columns
#'   correspond to predictors from \code{xdata}.} \item{Beta}{an array of model
#'   coefficients. Columns correspond to predictors from \code{xdata}. Indices
#'   along the third dimension correspond to different resampling iterations.
#'   With multivariate outcomes, indices along the fourth dimension correspond
#'   to outcome-specific coefficients.} \item{method}{a list of
#'   \code{implementation}, \code{family}, \code{resampling} and
#'   \code{PFER_method} values used for the run.} \item{param}{a list of
#'   \code{K}, \code{pi_list}, \code{tau}, \code{n_cat}, \code{pk}, \code{n}
#'   (number of observations), \code{PFER_thr}, \code{FDP_thr} and \code{seed}
#'   values used for the run. The datasets \code{xdata} and \code{ydata} are
#'   also included if \code{output_data=TRUE}.} For all objects except those
#'   stored in \code{methods} or \code{params}, rows correspond to parameter
#'   values stored in the output \code{Lambda}.
#'
#' @keywords internal
SerialRegression <- function(xdata, ydata = NULL, Lambda, pi_list = seq(0.6, 0.9, by = 0.01),
                             K = 100, tau = 0.5, seed = 1, n_cat = 3,
                             family = "gaussian", implementation = PenalisedRegression,
                             resampling = "subsampling", PFER_method = "MB", PFER_thr = Inf, FDP_thr = Inf,
                             group_x = NULL, group_penalisation = FALSE,
                             output_data = FALSE, verbose = TRUE, ...) {
  # Defining K if using complementary pairs (SS)
  if (PFER_method == "SS") {
    K <- ceiling(K / 2) * 2
    tau <- 0.5
  }
  
  # Initialising objects to be filled
  N <- N_block <- ncol(xdata)
  
  # Initialising the arrays
  s <- Resample(data = ydata, family = family, tau = tau, resampling = resampling, ...)
  Xsub <- xdata[s, ]
  Ysub <- ydata[s, ]
  mybeta <- SelectionAlgo(
    xdata = Xsub, ydata = Ysub,
    Lambda = Lambda[, 1], group_x = group_x,
    family = family, implementation = implementation, ...
  )
  Beta <- array(0, dim = c(nrow(mybeta$selected), ncol(mybeta$selected), K))
  rownames(Beta) <- rownames(mybeta$selected)
  colnames(Beta) <- colnames(mybeta$selected)
  if (length(dim(mybeta$beta_full)) == 2) {
    Beta_full <- array(0,
                       dim = c(nrow(Lambda), dim(mybeta$beta_full)[2], K),
                       dimnames = list(rownames(mybeta$beta_full), dimnames(mybeta$beta_full)[[2]], NULL)
    )
  } else {
    if (length(dim(mybeta$beta_full)) == 3) {
      Beta_full <- array(0,
                         dim = c(nrow(Lambda), dim(mybeta$beta_full)[2], K, dim(mybeta$beta_full)[3]),
                         dimnames = list(rownames(mybeta$beta_full), dimnames(mybeta$beta_full)[[2]], NULL, dimnames(mybeta$beta_full)[[3]])
      )
    } else {
      stop(paste0("Invalid output from the variable selection function: ", implementation, "(). The output 'beta_full' must be an array with 2 or 3 dimensions."))
    }
  }
  
  # Setting seed for reproducibility
  withr::local_seed(seed)
  
  # Computation of the selection proportions over Lambda
  if (verbose) {
    pb <- utils::txtProgressBar(style = 3)
  }
  if (PFER_method == "MB") {
    for (k in 1:K) {
      s <- Resample(data = ydata, family = family, tau = tau, resampling = resampling, ...)
      Xsub <- xdata[s, ]
      Ysub <- ydata[s, ]
      mybeta <- SelectionAlgo(
        xdata = Xsub, ydata = Ysub,
        Lambda = Lambda[, 1], group_x = group_x,
        family = family, implementation = implementation, ...
      )
      
      # Resampling if model failed to converge
      while (is.infinite(mybeta$selected[1])) {
        s <- Resample(data = ydata, family = family, tau = tau, resampling = resampling, ...)
        Xsub <- xdata[s, ]
        Ysub <- ydata[s, ]
        mybeta <- SelectionAlgo(
          xdata = Xsub, ydata = Ysub,
          Lambda = Lambda[, 1], group_x = group_x,
          family = family, implementation = implementation, ...
        )
      }
      
      # Storing (one set of) beta coefficients, used to define set of selected variables
      Beta[rownames(mybeta$selected), colnames(mybeta$selected), k] <- mybeta$selected
      
      # Storing all beta coefficients
      if (length(dim(Beta_full)) == 3) {
        Beta_full[rownames(mybeta$beta_full), colnames(mybeta$beta_full), k] <- mybeta$beta_full
      } else {
        Beta_full[rownames(mybeta$beta_full), colnames(mybeta$beta_full), k, ] <- mybeta$beta_full
      }
      
      if (verbose) {
        utils::setTxtProgressBar(pb, k / K)
      }
    }
    
    # Computing the selection proportions
    bigstab <- matrix(NA, nrow = nrow(Beta), ncol = ncol(Beta))
    colnames(bigstab) <- colnames(Beta)
    rownames(bigstab) <- rownames(Beta)
    for (i in 1:nrow(Beta)) {
      for (j in 1:ncol(Beta)) {
        bigstab[i, j] <- sum(Beta[i, j, ] != 0) / K
      }
    }
  }
  
  if (PFER_method == "SS") {
    for (k in 1:ceiling(K / 2)) {
      s <- Resample(data = ydata, family = family, tau = tau, resampling = resampling, ...)
      
      # First subset
      Xsub <- xdata[s, ]
      Ysub <- ydata[s, ]
      mybeta1 <- SelectionAlgo(
        xdata = Xsub, ydata = Ysub,
        Lambda = Lambda[, 1], group_x = group_x,
        family = family, implementation = implementation, ...
      )
      
      # Complementary subset
      Xsub <- xdata[seq(1, nrow(xdata))[!seq(1, nrow(xdata)) %in% s], ]
      Ysub <- ydata[seq(1, nrow(xdata))[!seq(1, nrow(xdata)) %in% s], ]
      mybeta2 <- SelectionAlgo(
        xdata = Xsub, ydata = Ysub,
        Lambda = Lambda[, 1], group_x = group_x,
        family = family, implementation = implementation, ...
      )
      
      # Resampling if model failed to converge
      while (is.infinite(mybeta1$selected[1]) | is.infinite(mybeta2$selected[1])) {
        s <- Resample(data = ydata, family = family, tau = tau, resampling = resampling, ...)
        
        # First subset
        Xsub <- xdata[s, ]
        Ysub <- ydata[s, ]
        mybeta <- SelectionAlgo(
          xdata = Xsub, ydata = Ysub,
          Lambda = Lambda[, 1], group_x = group_x,
          family = family, implementation = implementation, ...
        )
        
        # Complementary subset
        Xsub <- xdata[seq(1, nrow(xdata))[!seq(1, nrow(xdata)) %in% s], ]
        Ysub <- ydata[seq(1, nrow(xdata))[!seq(1, nrow(xdata)) %in% s], ]
        mybeta <- SelectionAlgo(
          xdata = Xsub, ydata = Ysub,
          Lambda = Lambda[, 1], group_x = group_x,
          family = family, implementation = implementation, ...
        )
      }
      
      # Storing beta coefficients from first set
      Beta[rownames(mybeta1$selected), colnames(mybeta1$selected), k] <- mybeta1$selected
      
      # Storing all beta coefficients from first set
      if (length(dim(Beta_full)) == 3) {
        Beta_full[rownames(mybeta1$beta_full), colnames(mybeta1$beta_full), k] <- mybeta1$beta_full
      } else {
        Beta_full[rownames(mybeta1$beta_full), colnames(mybeta1$beta_full), k, ] <- mybeta1$beta_full
      }
      
      # Storing beta coefficients from complementary set
      Beta[rownames(mybeta2$selected), colnames(mybeta2$selected), ceiling(K / 2) + k] <- mybeta2$selected
      
      # Storing all beta coefficients from complementary set
      if (length(dim(Beta_full)) == 3) {
        Beta_full[rownames(mybeta2$beta_full), colnames(mybeta2$beta_full), ceiling(K / 2) + k] <- mybeta2$beta_full
      } else {
        Beta_full[rownames(mybeta2$beta_full), colnames(mybeta2$beta_full), ceiling(K / 2) + k, ] <- mybeta2$beta_full
      }
      
      if (verbose) {
        utils::setTxtProgressBar(pb, 2 * k / K)
      }
    }
    
    # Computing the simultaneous selection proportions
    bigstab <- matrix(0, nrow = nrow(Beta), ncol = ncol(Beta))
    colnames(bigstab) <- colnames(Beta)
    rownames(bigstab) <- rownames(Beta)
    for (k in 1:ceiling(K / 2)) {
      A1 <- ifelse(Beta[, , k] != 0, yes = 1, no = 0)
      A2 <- ifelse(Beta[, , ceiling(K / 2) + k] != 0, yes = 1, no = 0)
      A <- A1 + A2
      A <- ifelse(A == 2, yes = 1, no = 0)
      bigstab <- bigstab + A
    }
    bigstab <- bigstab / ceiling(K / 2)
  }
  
  if (verbose) {
    cat("\n")
  }
  
  # Computation of the stability score over Lambda and pi_list
  if (group_penalisation) {
    metrics <- StabilityMetrics(
      selprop = bigstab, pk = NULL, pi_list = pi_list, K = K, n_cat = n_cat,
      Sequential_template = NULL, graph = FALSE, group = group_x,
      PFER_method = PFER_method, PFER_thr_blocks = PFER_thr, FDP_thr_blocks = FDP_thr
    )
  } else {
    metrics <- StabilityMetrics(
      selprop = bigstab, pk = NULL, pi_list = pi_list, K = K, n_cat = n_cat,
      Sequential_template = NULL, graph = FALSE,
      PFER_method = PFER_method, PFER_thr_blocks = PFER_thr, FDP_thr_blocks = FDP_thr
    )
  }
  if (verbose) {
    utils::setTxtProgressBar(pb, 1)
    cat("\n")
  }
  Beta <- Beta_full
  
  # Preparing outputs
  myimplementation <- as.character(substitute(implementation, env = parent.frame(n = 2)))
  if (is.function(resampling)) {
    myresampling <- as.character(substitute(resampling))
  } else {
    myresampling <- resampling
  }
  out <- list(
    S = metrics$S, Lambda = Lambda,
    Q = metrics$Q, Q_s = metrics$Q_s, P = metrics$P,
    PFER = metrics$PFER, FDP = metrics$FDP,
    S_2d = metrics$S_2d, PFER_2d = metrics$PFER_2d, FDP_2d = metrics$FDP_2d,
    selprop = bigstab, Beta = Beta,
    methods = list(
      type = "variable_selection", implementation = myimplementation, family = family,
      resampling = myresampling, PFER_method = PFER_method
    ),
    params = list(
      K = K, pi_list = pi_list, tau = tau, n_cat = n_cat,
      pk = ncol(xdata), n = nrow(xdata),
      PFER_thr = PFER_thr, FDP_thr = FDP_thr,
      seed = seed
    )
  )
  
  if (output_data) {
    out$params <- c(out$params, list(xdata = xdata, ydata = ydata))
  }
  
  return(out)
}

CheckInputRegression <- function(xdata, ydata = NULL, Lambda = NULL, pi_list = seq(0.6, 0.9, by = 0.01),
                                 K = 100, tau = 0.5, seed = 1, n_cat = 3,
                                 family = "gaussian", implementation = PenalisedRegression,
                                 resampling = "subsampling", PFER_method = "MB", PFER_thr = Inf, FDP_thr = Inf,
                                 Lambda_cardinal = 100,
                                 verbose = TRUE) {
  # List of arguments
  myargs <- c(
    "xdata", "ydata", "Lambda", "pi_list", "K", "tau", "seed", "n_cat",
    "family",
    "PFER_method", "PFER_thr", "FDP_thr",
    "Lambda_cardinal", "verbose"
  )
  
  # Checking the inputs (xdata and ydata)
  xdata <- as.matrix(xdata)
  if (!is.null(ydata)) {
    if (sum(is.na(xdata)) > 0) {
      stop("Invalid input for argument 'xdata'. Missing values are not allowed in 'xdata'.")
    }
    if (sum(is.na(ydata)) > 0) {
      stop("Invalid input for argument 'ydata'. Missing values are not allowed in 'ydata'.")
    }
    if ((nrow(xdata) < 10) | (ncol(xdata) <= 1)) {
      stop("Invalid input for argument 'xdata'. Not enough data.")
    }
  }
  
  # Preparing xdata
  if (is.null(colnames(xdata))) {
    colnames(xdata) <- paste0("var", 1:ncol(xdata))
  }
  
  # Preparing ydata
  if (!is.null(ydata)) {
    if (is.vector(ydata) | is.factor(ydata)) {
      ydata <- matrix(ydata, ncol = 1)
    }
  }
  
  # Checking the inputs (xdata and ydata)
  if (!is.null(ydata)) {
    if (nrow(xdata) != nrow(ydata)) {
      stop("Arguments 'xdata' and 'ydata' are not compatible. They have different numbers of observations.")
    }
  }
  
  # Creating dummy ydata (for resampling in unsupervised models)
  if (is.null(ydata)) {
    ydata <- cbind(rep(0, nrow(xdata)))
  }
  
  # Naming rows of xdata and ydata
  if (is.null(rownames(xdata)) & is.null(rownames(ydata))) {
    rownames(xdata) <- paste0("obs", 1:nrow(xdata))
    rownames(ydata) <- rownames(xdata)
  } else {
    if ((is.null(rownames(xdata))) & (!is.null(rownames(ydata)))) {
      rownames(xdata) <- rownames(ydata)
    }
    if ((!is.null(rownames(xdata))) & (is.null(rownames(ydata)))) {
      rownames(ydata) <- rownames(xdata)
    }
  }
  
  # Re-ordering the datasets to ensure that subsamples will be the same regardless of the order of observations in the input
  ids <- sort.list(rownames(xdata))
  xdata <- xdata[ids, , drop = FALSE]
  ydata <- ydata[ids, , drop = FALSE]
  
  # Further checking/preparing ydata
  if ((family == "cox")) {
    if ((ncol(ydata) != 2) | (length(unique(ydata[, 2])) != 2)) {
      stop("Invalid input for argument 'ydata'. For Cox regression using glmnet, the argument 'ydata' needs to be a matrix or data frame with two columns: the time to event and binary status.")
    }
    colnames(ydata) <- c("time", "status")
    tmp <- as.factor(ydata[, 2])
    if (verbose) {
      message(paste0("Reference category: ", levels(tmp)[1]))
      message(paste0("Other category: ", levels(tmp)[2]))
    }
    ydata[, 2] <- as.numeric(tmp) - 1
    ydata <- as.matrix(ydata)
  }
  if ((family %in% c("binomial", "multinomial"))) {
    if (ncol(ydata) > 1) {
      ydata_original <- ydata
      ydata <- matrix(0, nrow = nrow(ydata_original), ncol = ncol(ydata_original))
      for (j in 1:ncol(ydata)) {
        tmp <- as.factor(ydata_original[, j])
        if (verbose) {
          message(paste0("Reference category for column ", j, ": ", levels(tmp)[1]))
          message(paste0("Other category for column ", j, ": ", levels(tmp)[2]))
        }
        ydata[, j] <- (as.numeric(tmp) - 1) * j
      }
      ydata <- apply(ydata, 1, sum)
    } else {
      ydata <- as.factor(ydata)
      if (verbose) {
        message(paste0("Reference category: ", levels(ydata)[1]))
        message(paste0("Other categorie(s): ", paste(levels(ydata)[-1], collapse = ", ")))
      }
      ydata <- as.numeric(ydata) - 1
    }
    ydata <- matrix(ydata, ncol = 1)
    ytmp <- as.numeric(table(ydata))
    if (any(ytmp == 1)) {
      stop("At least one category in 'ydata' with only one observation.")
    }
  }
  
  # Checking the inputs (Lambda)
  if (!is.null(Lambda)) {
    if (is.matrix(Lambda)) {
      Lambda_copy <- Lambda
      Lambda <- NULL
      for (k in 1:ncol(Lambda_copy)) {
        Lambda <- cbind(Lambda, as.numeric(Lambda_copy[, k]))
      }
    } else {
      Lambda <- as.numeric(Lambda)
      Lambda <- cbind(Lambda)
    }
    if (any(is.na(Lambda))) {
      if (all(is.na(Lambda))) {
        stop("Invalid input for argument 'Lambda'. The input only contains missing values.")
      } else {
        Lambda <- as.matrix(stats::na.exclude(Lambda))
        warning("Invalid input for argument 'Lambda'. The input contains missing values. These have been excluded.")
      }
    }
    rownames(Lambda) <- paste0("s", seq(0, nrow(Lambda) - 1))
  }
  
  # Checking the inputs (pi_list)
  pi_list <- sort(pi_list)
  if (n_cat == 3) {
    if (any(pi_list > 0.5) & any(pi_list < 1)) {
      if ((min(pi_list) < 0.5) | (max(pi_list) > 1)) {
        warning("The values in 'pi_list' must be between 0.5 and 1. All other values were discarded.")
        pi_list <- pi_list[which((pi_list > 0.5) & (pi_list < 1))]
      }
    } else {
      stop("Invalid input for argument 'pi_list'. The values in the vector must be greater than 0.5 and lower than 1. To consider thresholds below 0.5, argument 'n_cat' must be set to 2.")
    }
  } else {
    if (any(pi_list > 0) & any(pi_list < 1)) {
      if ((min(pi_list) < 0) | (max(pi_list) > 1)) {
        warning("The values in 'pi_list' must be between 0 and 1. All other values were discarded.")
        pi_list <- pi_list[which((pi_list > 0) & (pi_list < 1))]
      }
    } else {
      stop("Invalid input for argument 'pi_list'. The values in the vector must be greater than 0 and lower than 1.")
    }
  }
  
  # Checking the inputs (K)
  K <- as.numeric(K)
  if ((length(K) != 1) | is.na(K)) {
    warning("Invalid input for argument 'K'. The number of resampling iterations 'K' must be a single number.")
    K <- 100
  }
  
  # Checking the inputs (tau)
  tau <- as.numeric(tau)
  if ((length(tau) != 1) | is.na(tau) | (tau >= 1) | (tau <= 0)) {
    warning("Invalid input for argument 'tau'. The subsample size 'tau' must be a number between 0 and 1. The default value (0.5) was used.")
    tau <- 0.5
  }
  
  # Checking the inputs (seed)
  seed <- as.numeric(seed)
  if ((length(seed) != 1) | is.na(seed)) {
    warning("Invalid input for argument 'seed'. The argument 'seed' must be a single number. The default value (1) was used.")
    seed <- 1
  }
  
  # Checking the inputs (n_cat)
  n_cat <- as.numeric(n_cat)
  if ((length(n_cat) != 1) | is.na(n_cat)) {
    warning("Invalid input for argument 'n_cat'. The argument 'seed' must be set to 2 or 3. The default value (3) was used.")
    n_cat <- 3
  }
  
  # Checking the inputs (family)
  family <- as.character(family)
  if ((length(family) != 1) | is.na(family)) {
    stop("Invalid input for argument 'family'. The argument 'family' must be a character string.")
  }
  
  # Checking the inputs (implementation)
  if (!is.function(implementation)) {
    stop("Invalid input for argument 'implementation'. This argument must be a function to use for variable selection.")
  }
  
  # Checking the inputs (resampling)
  if ((!is.function(resampling)) & (!is.character(resampling))) {
    stop("Invalid input for argument 'resampling'. The argument 'resampling' must be a character string. Possible values are: 'subsampling', 'bootstrap' or the name of a function.")
  }
  
  # Checking the inputs (PFER_method)
  PFER_method <- as.character(PFER_method)
  if ((length(PFER_method) != 1) | (!PFER_method %in% c("MB", "SS"))) {
    stop("Invalid input for argument 'PFER_method'. Possible values are: 'MB' or 'SS'.")
  }
  
  # Checking the inputs (PFER_method and resampling)
  if (is.character(resampling)) {
    if ((PFER_method == "SS") & (resampling == "bootstrap")) {
      warning("Arguments 'resampling' and 'PFER_method' are not compatible. With 'PFER_method' set to 'SS', the resampling is done with complementary pairs of subsamples.")
      resampling <- "subsampling"
    }
  }
  
  # Checking the inputs (PFER_thr)
  PFER_thr <- as.numeric(PFER_thr)
  if ((length(PFER_thr) != 1) | is.na(PFER_thr) | (PFER_thr <= 0)) {
    warning("Invalid input for argument 'PFER_thr'. The threshold in the upper-bound of the expected number of False Positives 'PFER_thr' must be a single positive number (or Inf). The default value (Inf) was used.")
    PFER_thr <- Inf
  }
  
  # Checking the inputs (FDP_thr)
  FDP_thr <- as.numeric(FDP_thr)
  if ((length(FDP_thr) != 1) | is.na(FDP_thr) | ((!is.infinite(FDP_thr)) & (FDP_thr <= 0)) | ((!is.infinite(FDP_thr)) & (FDP_thr > 1))) {
    warning("Invalid input for argument 'FDP_thr'. The threshold in the upper-bound of the False Discovery Proportion 'FDP_thr' must be a single number between 0 and 1 (or Inf to deactivate). The default value (Inf) was used.")
    FDP_thr <- Inf
  }
  
  # Checking the inputs (PFER_thr and FDP_thr)
  if ((!is.infinite(PFER_thr)) & (!is.infinite(FDP_thr))) {
    warning("Arguments 'PFER_thr' and 'FDP_thr' are not compatible. Only one of these two arguments can be used (i.e. not set to Inf). Argument 'PFER_thr' was used.")
    FDP_thr <- Inf
  }
  
  # Checking the inputs (Lambda_cardinal)
  Lambda_cardinal <- as.numeric(Lambda_cardinal)
  if (is.null(Lambda)) {
    if ((length(Lambda_cardinal) != 1) | is.na(Lambda_cardinal) | (Lambda_cardinal < 1)) {
      warning("Invalid input for argument 'Lambda_cardinal'. The argument 'Lambda_cardinal' must be a single positive number. A value of 10 was used.")
      Lambda_cardinal <- 10
    }
  }
  
  # Checking the inputs (verbose)
  verbose <- as.logical(verbose)
  if ((length(verbose) != 1) | is.na(verbose)) {
    warning("Invalid input for argument 'verbose'. The argument 'verbose' must be logical (TRUE or FALSE). The default value (TRUE) was used.")
    verbose <- TRUE
  }
  
  # Assigning checked values to the parent function
  for (i in 1:length(myargs)) {
    if (!is.null(get(myargs[i]))) {
      assign(myargs[i], get(myargs[i]), envir = parent.frame(n = 1))
    }
  }
}

HierarchicalClustering <- function(xdata, nc = NULL, scale = TRUE, rows = TRUE, ...) {
  # Storing extra arguments
  extra_args <- list(...)
  
  # Transposing for clustering of columns
  if (!rows) {
    xdata <- t(xdata)
  }
  
  # Scaling the data
  if (scale) {
    xdata <- scale(xdata)
  }
  
  # Re-formatting nc
  if (!is.null(nc)) {
    if (is.vector(nc)) {
      nc <- cbind(nc)
    }
  } else {
    nc <- cbind(seq(1, nrow(xdata)))
  }
  
  # Extracting relevant extra arguments (distance)
  ids <- which(names(extra_args) %in% names(formals(stats::dist)))
  ids <- ids[!ids %in% c("x")]
  
  # Computing pairwise distances
  mydistance <- do.call(stats::dist, args = c(list(x = xdata), extra_args[ids]))
  
  # Extracting relevant extra arguments (hclust)
  ids <- which(names(extra_args) %in% names(formals(stats::hclust)))
  ids <- ids[!ids %in% c("d")]
  
  # Running hierarchical clustering
  myclust <- do.call(stats::hclust, args = c(list(d = mydistance), extra_args[ids]))
  
  # Initialisation of array storing co-membership matrices
  adjacency <- array(NA, dim = c(nrow(xdata), nrow(xdata), nrow(nc)))
  
  # Defining clusters
  mygroups <- do.call(stats::cutree, args = list(tree = myclust, k = nc))
  if (is.null(dim(mygroups))) {
    mygroups <- cbind(mygroups)
  }
  for (i in 1:nrow(nc)) {
    adjacency[, , i] <- CoMembership(groups = mygroups[, i])
  }
  
  return(list(comembership = adjacency))
}



KprotoClustering2 <- function(xdata, nc = NULL, Lambda, scale = TRUE, rows = TRUE, ...) {
  # Checking clustMixType package is installed
  if (!requireNamespace("clustMixType")) {
    stop("This function requires the 'clustMixType' package.")
  }
  
  # Storing extra arguments
  extra_args <- list(...)
  
  # Transposing for clustering of columns
  #if (!rows) {
  #   xdata <- t(xdata)
  # }
  
  # Scaling the data
  # if (scale) {
  # xdata <- scale(xdata)
  # }
  
  # Re-formatting Lambda
  if (is.vector(Lambda)) {
    Lambda <- cbind(Lambda)
  }
  
  # Re-formatting nc
  if (!is.null(nc)) {
    if (is.vector(nc)) {
      nc <- cbind(nc)
    }
  } else {
    nc <- cbind(seq(1, nrow(xdata)))
  }
  
  # Extracting relevant extra arguments (kproto)
  ids <- which(names(extra_args) %in% names(formals(clustMixType::kproto)))
  ids <- ids[!ids %in% c("x","k","keep.data", "na.rm", "lambda")]
  
  # Initialisation of array storing co-membership matrices
  adjacency <- array(NA, dim = c(nrow(xdata), nrow(xdata), nrow(nc) * nrow(Lambda)))
  weight <- matrix(NA, nrow = nrow(nc) * nrow(Lambda), ncol = ncol(xdata))
  
  # Iterating over the pair of parameters
  id <- 0
  for (i in 1:nrow(Lambda)) {
    for (j in 1:nrow(nc)) {
      # Running sparse hierarchical clustering
      myclust <- do.call(clustMixType::kproto, args = c(
        list(x = as.data.frame(xdata), k = nc[j, 1], keep.data = TRUE, na.rm = FALSE, lambda = Lambda[i, 1]),
        extra_args[ids]
      ))
      
      # Defining clusters
      mygroups <- myclust$cluster
      adjacency[, , j] <- CoMembership(groups = mygroups)
    }
  }
  return(list(comembership = adjacency))
}


#Clear environment
#rm(list=ls())

#Read unsupervised dataset (num variables yeo-johnson scaled and categorical as original)
df_for_KMeans_un <- read.csv("/rds/general/user/md2620/home/asthma/Malo/Dataframes/Final/unsup_df.csv")

#Reproducibility 
set.seed(1222)

#Packages loading
library(sparcl)
library(survival)
library(compHclust)
library(cluster)

#Check classes of variables
View(sapply(df_for_KMeans_un, class))

#Set Subject.ID as index
df_un_KMeans <- df_for_KMeans_un[-1]
row.names(df_un_KMeans) <- df_for_KMeans_un$X
View(df_un_KMeans)

#Create an index of your original categorical variables
index_categorical <- c(181:361)
df_un_KMeans[,index_categorical] <- lapply(df_un_KMeans[ ,index_categorical], as.factor)

#Remove few variables that you don't use in kmeans and preweighted clustering
df_un_KMeans = subset(df_un_KMeans, select = -c(cohort,cluster_K_2,cluster_K_3,cluster_K_4,
                                                cluster_K_5,cluster_K_6,cluster_K_7,
                                                cluster_K_8,cluster_K_9,cluster_K_10,
                                                cluster_K_11,cluster_K_12,cluster_K_13,
                                                cluster_K_14,cluster_K_15,Healthy,Severe,Severe_asthma,
                                                MildModerate,Severe_Smoker))

#Check classes of variables
View(sapply(df_un_KMeans, class))

#Seperate numerical and categorical variables into 2 different dataframes
#Numerical dataframe
num_df_un_KMeans <- df_un_KMeans %>%
  select_if(Negate(is.factor))

#Categorical dataframe
cat_df_un_KMeans <- df_un_KMeans %>%
  select_if(is.factor)

#Check classes of variables
View(sapply(num_df_un_KMeans, class))

#Change integer variables to numeric for preweighted algorithm to work
num_df_un_KMeans$Omics.Xylose <- as.numeric(num_df_un_KMeans$Omics.Xylose)
num_df_un_KMeans$Omics.Xanthosine <- as.numeric(num_df_un_KMeans$Omics.Xanthosine)
num_df_un_KMeans$Omics.Furoylglycine <- as.numeric(num_df_un_KMeans$Omics.Furoylglycine)
num_df_un_KMeans$Omics.Allantoin <- as.numeric(num_df_un_KMeans$Omics.Allantoin)
num_df_un_KMeans$Omics.Cytosine <- as.numeric(num_df_un_KMeans$Omics.Cytosine)
num_df_un_KMeans$Omics.Glucosamine <- as.numeric(num_df_un_KMeans$Omics.Glucosamine)
num_df_un_KMeans$Omics.Glutamic.acid <- as.numeric(num_df_un_KMeans$Omics.Glutamic.acid)
num_df_un_KMeans$Omics.Isoleucine <- as.numeric(num_df_un_KMeans$Omics.Isoleucine)
num_df_un_KMeans$Omics.Lysine <- as.numeric(num_df_un_KMeans$Omics.Lysine)
num_df_un_KMeans$Omics.Maltose <- as.numeric(num_df_un_KMeans$Omics.Maltose)
num_df_un_KMeans$Omics.N.Acetylglutamic.acid <- as.numeric(num_df_un_KMeans$Omics.N.Acetylglutamic.acid)
num_df_un_KMeans$Omics.N.Methyl.D.aspartic.acid <- as.numeric(num_df_un_KMeans$Omics.N.Methyl.D.aspartic.acid)
num_df_un_KMeans$Omics.O.Acetylserine <- as.numeric(num_df_un_KMeans$Omics.O.Acetylserine)
num_df_un_KMeans$Omics.Phenylalanine <- as.numeric(num_df_un_KMeans$Omics.Phenylalanine)
num_df_un_KMeans$Omics.Sarcosine <- as.numeric(num_df_un_KMeans$Omics.Sarcosine)
num_df_un_KMeans$Omics.N.Acetylputrescine <- as.numeric(num_df_un_KMeans$Omics.N.Acetylputrescine)

# For visualization purposes, we drop all variables that have 0 variance
#Supply names of columns that have 0 variance
names(num_df_un_KMeans[, sapply(num_df_un_KMeans, function(v) var(v, na.rm=TRUE)==0)])

#Drop columns that have 0 variance from dataframe
num_df_un_KMeans <- num_df_un_KMeans[,apply(num_df_un_KMeans, 2, var, na.rm=TRUE) != 0]

#plot(Graph(Adjacency(stab), node_colour = simul$theta, satellites = TRUE))
#ClusteringPerformance(theta = Clusters(stab), theta_star = simul$theta)

#### Data simulation
#set.seed(1)
#simul <- SimulateClustering(n = c(10, 10), pk = 50)
#mykmeansl <- KMeansClustering(xdata = simul$data, nc = 1:10)

# Consensus clustering based on k-means clustering
#stab <- Clustering(
# xdata = simul$data,
# implementation = KMeansClustering)

#simul$theta

#par(mar = c(5, 6, 3, 12))
#CalibrationPlot(stab, xlab = expression(italic(k)), clustering = TRUE)
#plot(Graph(Adjacency(stab), node_colour = simul$theta, satellites = TRUE))
#ClusteringPerformance(theta = Clusters(stab), theta_star = simul$theta)
#### End simulation ####


#Run stability analysis using KMeans on K-proto dataset
stab_df = subset(stab_df, select = -c(cohort,cluster_K_2,cluster_K_3,cluster_K_4,
                                                cluster_K_5,cluster_K_6,cluster_K_7,
                                                cluster_K_8,cluster_K_9,cluster_K_10,
                                                cluster_K_11,cluster_K_12,cluster_K_13,
                                                cluster_K_14,cluster_K_15,Healthy,Severe,Severe_asthma,
                                                MildModerate,Severe_Smoker))

stab_df = subset(stab_df, select = -c(Omics.Furoylglycine,Omics.Glutamic.acid,Omics.N.Acetylglutamic.acid,
                                    Omics.Phenylalanine,Omics.Allantoin,Omics.Isoleucine,Omics.N.Acetylputrescine,
                                    Omics.Sarcosine,Omics.Cytosine,Omics.Lysine,Omics.N.Methyl.D.aspartic.acid,
                                    Omics.Xanthosine,Omics.Glucosamine,Omics.Maltose,Omics.O.Acetylserine,
                                    Omics.Xylose))

#Set Subject.ID as index
stab_df_protomeans <- stab_df[-1]
row.names(stab_df_protomeans) <- stab_df$Unnamed..0
View(stab_df_protomeans)

#Run stability analysis on kproto dataset ( F = 345)
stab_kmeans_protodf <- Clustering(
  xdata = stab_df_protomeans,
  nc = 2:15,
  implementation = KMeansClustering)

#Identification of stable cluster
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Stability/stable_calibration_kproto.pdf",   # The directory you want to save the file in
    width = 14, # The width of the plot in inches
    height = 8)
par(mar = c(5, 6, 3, 12))
CalibrationPlot(stab_kmeans_protodf,xlab = expression(italic(k)),clustering=TRUE)+
  title(main = "Identification of stable clusters via maximization of the stability score across models- Kproto covariates (F = 345)")
dev.off()

# Selection proportions
mat <- SelectionProportions(stab_kmeans_protodf)
print(mat)

myadjacency <- Adjacency(stab_kmeans_protodf)

pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Stability/adjacency_matrix_graph_kproto.pdf",   # The directory you want to save the file in
    width = 14, # The width of the plot in inches
    height = 8)
mygraph <- Graph(myadjacency, node_colour = membership)
set.seed(1)
plot(mygraph)
title(main = "Adjencacy matrix of the calibrated stability selection graphical model, (F=345)", sub = "Visualizing observations grouped together in more than 90% of the subsamples")
dev.off()

#Stable membership of observations into the 3 clusters
membership <- Clusters(stab_kmeans_protodf)
print(membership)

as.factor(membership)
levels(membership)

#Append clusters membership to stab_df_protomeans
stab_df_protomeans$stable_cluster <- factor(membership)
#Append cohort
stab_df_protomeans$cohort <- factor(df_for_KMeans_un$cohort)

pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Stability/cohort_stable_clust_kproto.pdf",   # The directory you want to save the file in
    width = 14, # The width of the plot in inches
    height = 8)
library(ggplot2)
g <- ggplot(stab_df_protomeans, aes(stable_cluster))
g + geom_bar(aes(fill=cohort), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Stable cluster membership of the 610 participants assessed at baseline, (F=345)", 
       subtitle="Asthma severity levels of participants partioned in 3 main clusters",
       x = "K", y = "Participants count",fill = "Asthma severity levels") 
dev.off()


#### RE-RUN STABILITY ANALYSIS ON KMEANS
#Run stability analysis on kmeans dataset ( F = 164)

##### Run K-Means stability analysis on num_df_un_KMeans #####
#mykmeans_real <- KMeansClustering(xdata = num_df_un_KMeans, nc = 1:15)
stab_real <- Clustering(
  xdata = num_df_un_KMeans,
  nc = 2:15,
  implementation = KMeansClustering)


#Identification of stable cluster
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Stability/stable_calibration_kmeans_164.pdf",   # The directory you want to save the file in
    width = 14, # The width of the plot in inches
    height = 8)
par(mar = c(5, 6, 3, 12))
CalibrationPlot(stab_real,xlab = expression(italic(k)),clustering=TRUE)+
  title(main = "Identification of stable clusters via maximization of the stability score across models - Kmeans covariates (F = 164)")
dev.off()

# Selection proportions - all are used (no sparsity coefficient lambda defined)
matrix <- SelectionProportions(stab_real)
print(matrix)

myadjacency_kmeans <- Adjacency(stab_real)

#Stable membership of observations into the 3 clusters
membership_kmeans <- Clusters(stab_real)
print(membership_kmeans)

pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Stability/adjacency_matrix_graph_kmeans_164.pdf",   # The directory you want to save the file in
    width = 14, # The width of the plot in inches
    height = 8)
mygraph_kmeans <- Graph(myadjacency_kmeans, node_colour = membership_kmeans)
set.seed(1)
plot(mygraph_kmeans)
title(main = "Adjencacy matrix of the calibrated stability selection graphical model, (F=164)", sub = "Visualizing observations grouped together in more than 90% of the subsamples")
dev.off()


as.factor(membership_kmeans)
levels(membership_kmeans)

#Append clusters membership to stab_df_protomeans
num_df_un_KMeans$stable_cluster <- factor(membership_kmeans)
#Append cohort
num_df_un_KMeans$cohort <- factor(df_for_KMeans_un$cohort)

pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Stability/cohort_stable_clust_kmeans_164.pdf",   # The directory you want to save the file in
    width = 14, # The width of the plot in inches
    height = 8)
library(ggplot2)
g <- ggplot(num_df_un_KMeans, aes(stable_cluster))
g + geom_bar(aes(fill=cohort), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Stable cluster membership of the 610 participants assessed at baseline, (F=164)", 
       subtitle="Asthma severity levels of participants partioned in 2 main clusters",
       x = "K", y = "Participants count",fill = "Asthma severity levels") 
dev.off()

#################### STUCK THERE #########################################
#################### PART REQUIRING BEST HELP / THOMAS ################### 
#### KPROTO
#Try running kproto - it works fine
mykproto <- KprotoClustering2(x = stab_df_final, nc = 2:4 , keep.data = TRUE, na.rm=FALSE,Lambda = 0.6:0.8)
kproto_test <- as.data.frame(mykproto)
kproto_mat <- as.matrix(kproto_test)
# Transposing for clustering of columns
kproto_mat <- t(kproto_mat)

#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("BiocManager")

#BiocManager::install("Ringo")
library(Ringo)
plotBM(kproto_mat, boxCol = "darkblue", reorder = TRUE, frame = TRUE)
#boxcol = color to use for boxes of 1s

#Try running stability analysis on kproto - it does not work as xdata can't handle both numeric and categorical variables as for now
stab_proto <- Clustering(
  xdata = stab_df_final,
  Lambda = cbind(seq(1.1, 1.3, by = 0.1)),
  implementation = KprotoClustering2)

### ENN KPROTO ###


### TRY RUNNING CLASSIC SPARSE HIERARCHICAL CLUSTERING ####
### Work well with simulated dataset but not with my dataset weirdly as well (Matrix definition problem feels like)
### Stability selection
#Set Subject.ID as index
stab_df_hierarchical <- stab_df[-1]
row.names(stab_df_hierarchical) <- stab_df$Unnamed..0
View(stab_df_hierarchical)

library(sparcl)


#' # Running sparse hierarchical clustering
 myclust <- ClusteringAlgo(
 xdata = stab_df_hierarchical,
Lambda = c(1.5, 2), nc = 2:5,
implementation = SparseHierarchicalClustering
)

stab_df_hierarchical <- as.matrix(stab_df_hierarchical)
stab_df_hierarchical <- mapply(stab_df_hierarchical, FUN=as.numeric)
#Run stability for sparse hierarchical clustering
stab_hier <- Clustering(
  xdata = stab_df_hierarchical$data,
  Lambda = seq(1.1, 1.3, by = 0.1),
  nc = 2:4)
#> Loading required namespace: sparcl
Lambda = cbind(seq(1.1, 1.3, by = 0.1))

test_simul <- simul$data

stab_df_hierarchical$data <- stab_df_hierarchical[1:610,1:345]
test <- stab_df_hierarchical$data
