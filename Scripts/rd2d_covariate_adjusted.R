# =============================================================================
# rd2d_covariate_adjusted.R
#
# Covariate-adjusted boundary RD estimator using the partialling-out
# (Frisch-Waugh-Lovell) approach of Calonico et al. (2019), adapted to the
# 2D boundary design of Cattaneo, Titiunik, and Yu (2025), Section 7.2.
#
# Key design decisions:
#   - Separate-by-side local polynomial fits (matching rd2d architecture)
#   - Partialling-out for covariate adjustment (gamma per evaluation point)
#   - Delta-method variance via adjusted residuals
#   - Half-vector approach for full J x J covariance matrix (AATE inference)
#
# Requires: rd2d package
# =============================================================================

# --- Import internal functions from rd2d ---
W.fun      <- rd2d:::W.fun
get_basis   <- rd2d:::get_basis
get_H       <- rd2d:::get_H
get_invH    <- rd2d:::get_invH
qrXXinv     <- rd2d:::qrXXinv
infl        <- rd2d:::infl
rd2d_vce    <- rd2d:::rd2d_vce


# =============================================================================
# rdbw2d_cov_full: Calonico (2019) full procedure adapted to 2D boundary RD
# =============================================================================
#
# Calonico, Cattaneo, Farrell & Titiunik (2019, RESTAT) recompute BOTH the
# leading bias and the leading variance constants on the partialled-out outcome
# Y_tilde = Y - gamma'Z when selecting MSE-optimal bandwidths in 1D RDD. The
# theory for the 2D boundary design is flagged as future work in Cattaneo,
# Titiunik & Yu (2025, Conclusion). This function implements the obvious 2D
# analogue at each evaluation point along the boundary:
#
#   1. Pilot bandwidths come from rdbw2d called on Y (unadjusted).
#   2. At each evaluation point j, pilot gamma_j is the local FWL estimate at
#      the pilot bandwidth (same machinery as rd2d_cov_core).
#   3. Form Y_tilde_j = Y - gamma_j'Z (globally; one Y_tilde per eval point).
#   4. Call rdbw2d again with Y_tilde_j as outcome, b = b[j, ], to obtain the
#      bias and variance constants of the partialled-out estimator at point j,
#      computed self-consistently inside rdbw2d's own machinery.
#   5. Return the per-evaluation-point h_j produced by rdbw2d on Y_tilde_j.

rdbw2d_cov_full <- function(Y, X, t, Z, b,
                            p = 1,
                            kernel = "tri",
                            kernel_type = "prod",
                            vce = "hc1",
                            bwselect = "mserd",
                            bwcheck = NULL,
                            stdvars = TRUE,
                            masspoints = "check",
                            C = NULL,
                            scaleregul = 3,
                            scalebiascrct = 1,
                            method = "dpi") {

  if (is.null(bwcheck)) bwcheck <- 50 + p + 1
  if (is.null(Z)) {
    return(rdbw2d(Y = Y, X = X, t = t, b = b, p = p,
                  kernel = kernel, kernel_type = kernel_type,
                  vce = vce, bwselect = bwselect,
                  bwcheck = bwcheck, stdvars = stdvars,
                  masspoints = masspoints, C = C,
                  scaleregul = scaleregul,
                  scalebiascrct = scalebiascrct,
                  method = method))
  }

  # ---- Step 1: pilot bandwidths from unadjusted rdbw2d ----
  bw_pilot <- rdbw2d(Y = Y, X = X, t = t, b = b, p = p,
                     kernel = kernel, kernel_type = kernel_type,
                     vce = vce, bwselect = bwselect,
                     bwcheck = bwcheck, stdvars = stdvars,
                     masspoints = masspoints, C = C,
                     scaleregul = scaleregul,
                     scalebiascrct = scalebiascrct,
                     method = method)

  # ---- Prepare covariates: standardize, drop collinear ----
  Z <- as.matrix(Z)
  if (ncol(Z) > 1) {
    qr_Z <- qr(Z)
    keep  <- qr_Z$pivot[seq_len(qr_Z$rank)]
    Z     <- Z[, keep, drop = FALSE]
  }
  Z <- scale(Z)
  Z[is.na(Z)] <- 0

  X <- as.matrix(X)
  b <- as.matrix(b)
  dat <- data.frame(x.1 = X[, 1], x.2 = X[, 2], y = Y, d = t)
  is_ctrl <- (dat$d == 0)
  is_trt  <- (dat$d == 1)
  neval <- nrow(b)

  # Storage: per-eval-point adjusted h and gamma
  h_adj <- matrix(NA_real_, neval, 4)
  colnames(h_adj) <- c("h01", "h02", "h11", "h12")
  gamma_list <- vector("list", neval)

  for (j in seq_len(neval)) {

    ev_x1 <- b[j, 1]
    ev_x2 <- b[j, 2]

    # Pilot bandwidths from rdbw2d (original-scale units)
    h.0 <- c(bw_pilot$bws$h01[j], bw_pilot$bws$h02[j])
    h.1 <- c(bw_pilot$bws$h11[j], bw_pilot$bws$h12[j])

    # ---- Step 2a: pilot gamma_j via local FWL at this eval point ----
    dat_c <- dat[, c("x.1", "x.2", "y", "d")]
    dat_c$x.1 <- dat_c$x.1 - ev_x1
    dat_c$x.2 <- dat_c$x.2 - ev_x2

    dat_0 <- dat_c[is_ctrl, ]
    dat_1 <- dat_c[is_trt, ]
    Z_0 <- Z[is_ctrl, , drop = FALSE]
    Z_1 <- Z[is_trt, , drop = FALSE]
    C_0 <- if (!is.null(C)) C[is_ctrl] else NULL
    C_1 <- if (!is.null(C)) C[is_trt] else NULL

    fit_0 <- rd2d_cov_side_fit(dat_0, Z_0, h.0, p,
                               kernel, kernel_type, vce, C_0)
    fit_1 <- rd2d_cov_side_fit(dat_1, Z_1, h.1, p,
                               kernel, kernel_type, vce, C_1)
    if (!fit_0$success || !fit_1$success) next

    dZ <- fit_0$dZ
    if (dZ > 0) {
      ZWZ <- fit_0$partout$ZWZ + fit_1$partout$ZWZ
      ZWY <- fit_0$partout$ZWY + fit_1$partout$ZWY
      gamma <- tryCatch(
        as.vector(chol2inv(chol(ZWZ)) %*% ZWY),
        error = function(e) as.vector(MASS::ginv(ZWZ) %*% ZWY)
      )
    } else {
      gamma <- numeric(0)
    }
    gamma_list[[j]] <- gamma

    # ---- Step 2b: rdbw2d on partialled-out outcome at this eval point ----
    Y_tilde <- Y - as.vector(Z %*% gamma)

    bw_pt <- tryCatch(
      rdbw2d(Y = Y_tilde, X = X, t = t,
             b = b[j, , drop = FALSE],
             p = p, kernel = kernel, kernel_type = kernel_type,
             vce = vce, bwselect = bwselect,
             bwcheck = bwcheck, stdvars = stdvars,
             masspoints = masspoints, C = C,
             scaleregul = scaleregul,
             scalebiascrct = scalebiascrct, method = method),
      error = function(e) NULL
    )

    if (!is.null(bw_pt)) {
      h_adj[j, "h01"] <- bw_pt$bws$h01[1]
      h_adj[j, "h02"] <- bw_pt$bws$h02[1]
      h_adj[j, "h11"] <- bw_pt$bws$h11[1]
      h_adj[j, "h12"] <- bw_pt$bws$h12[1]
    }
  }

  # ---- Step 3: assemble output ----
  # Replace bw_pilot bandwidths with adjusted ones (fall back to pilot if rdbw2d
  # failed at a point, so downstream code never sees NA).
  bw_adj <- bw_pilot
  bw_adj$bws$h01 <- ifelse(is.na(h_adj[, "h01"]), bw_pilot$bws$h01, h_adj[, "h01"])
  bw_adj$bws$h02 <- ifelse(is.na(h_adj[, "h02"]), bw_pilot$bws$h02, h_adj[, "h02"])
  bw_adj$bws$h11 <- ifelse(is.na(h_adj[, "h11"]), bw_pilot$bws$h11, h_adj[, "h11"])
  bw_adj$bws$h12 <- ifelse(is.na(h_adj[, "h12"]), bw_pilot$bws$h12, h_adj[, "h12"])
  bw_adj$pilot_gamma <- gamma_list
  bw_adj
}


# =============================================================================
# 1. Side-level WLS fitting for multiple outcomes
# =============================================================================
#
# Fits kernel-weighted polynomial regressions on ONE side (control or treated)
# at one evaluation point, for Y and each covariate Z_k separately. Returns
# all intermediate quantities needed for partialling-out and variance.

rd2d_cov_side_fit <- function(dat_side, Z_side, h, poly_order,
                              kernel, kernel_type, vce, C_side) {
  # Args:
  #   dat_side   : data.frame with x.1, x.2, y (already centered at eval pt)
  #   Z_side     : n_side x dZ matrix of covariates, or NULL
  #   h          : bandwidth, length 1 (radial) or 2 (product)
  #   poly_order : polynomial order
  #   kernel     : kernel name (e.g. "tri")
  #   kernel_type: "prod" or "rad"
  #   vce        : "hc0", "hc1", "hc2", or "hc3"
  #   C_side     : cluster IDs (vector) or NULL
  
  h <- as.vector(as.matrix(h))
  if (kernel_type == "prod") {
    if (length(h) == 1) h <- c(h, h)
    h.x <- h[1]; h.y <- h[2]
    w <- W.fun(dat_side$x.1 / h.x, kernel) *
      W.fun(dat_side$x.2 / h.y, kernel) / (h.x * h.y)
  } else {
    if (length(h) == 2) h <- sqrt(h[1]^2 + h[2]^2)
    h.x <- h; h.y <- h
    w <- W.fun(sqrt(dat_side$x.1^2 + dat_side$x.2^2) / h, kernel) / h^2
  }
  
  ind <- (w > 0)
  eN  <- sum(ind)
  count_p <- (poly_order + 1) * (poly_order + 2) / 2
  dZ <- if (!is.null(Z_side)) ncol(as.matrix(Z_side)) else 0L
  
  if (eN < count_p + 2)
    return(list(success = FALSE, eN = 0L))
  
  ew  <- w[ind]
  eY  <- dat_side$y[ind]
  eC  <- if (!is.null(C_side)) C_side[ind] else NULL
  eZ  <- if (dZ > 0) as.matrix(Z_side)[ind, , drop = FALSE] else NULL
  
  # Normalized coordinates
  eu <- data.frame(x.1 = dat_side$x.1[ind] / h.x,
                   x.2 = dat_side$x.2[ind] / h.y)
  
  eR      <- as.matrix(get_basis(eu, poly_order))   # n x count_p
  sqrtw_R <- sqrt(ew) * eR
  w_R     <- ew * eR
  
  invG <- qrXXinv(sqrtw_R)
  invH <- get_invH(c(h.x, h.y), poly_order)
  Hmat <- get_H(c(h.x, h.y), poly_order)
  
  # --- Fit each outcome: beta (original scale) and raw residuals ---
  fit_one <- function(a) {
    sqrtw_a   <- sqrt(ew) * a
    beta_norm <- invG %*% crossprod(sqrtw_R, matrix(sqrtw_a, ncol = 1))
    beta      <- invH %*% beta_norm          # count_p x 1, original scale
    fitted    <- as.vector(eR %*% beta_norm) # in normalized coords = original Y scale
    list(beta = beta, beta_norm = beta_norm, res_raw = a - fitted)
  }
  
  out_Y <- fit_one(eY)
  out_Z <- if (dZ > 0) lapply(1:dZ, function(k) fit_one(eZ[, k])) else list()
  
  # --- HC leverage adjustment (observation-level) ---
  if (vce == "hc0") {
    w_vce <- rep(1, eN)
  } else if (vce == "hc1") {
    w_vce <- rep(sqrt(eN / max(eN - count_p, 1)), eN)
  } else if (vce %in% c("hc2", "hc3")) {
    hii <- apply(sqrtw_R, 1, function(x) infl(x, invG))
    hii <- pmin(hii, 1 - 1e-6)
    w_vce <- if (vce == "hc2") sqrt(1 / (1 - hii)) else 1 / (1 - hii)
  } else {
    w_vce <- rep(1, eN)
  }
  
  # --- Partialling-out cross-products (for gamma estimation) ---
  partout <- NULL
  if (dZ > 0) {
    D   <- cbind(eY, eZ)                                        # n x (1+dZ)
    U   <- crossprod(w_R, D)                                     # count_p x (1+dZ)
    ZWD <- crossprod(eZ * ew, D)                                 # dZ x (1+dZ)
    U_Z <- matrix(U[, 2:(1 + dZ), drop = FALSE], nrow = count_p)  # count_p x dZ
    UiGU <- crossprod(U_Z, invG %*% U)                           # dZ x (1+dZ)
    ZWZ <- ZWD[, 2:(1 + dZ), drop = FALSE] - UiGU[, 2:(1 + dZ), drop = FALSE]
    ZWY <- ZWD[, 1, drop = FALSE]           - UiGU[, 1, drop = FALSE]
    partout <- list(ZWZ = ZWZ, ZWY = ZWY)
  }
  
  list(success = TRUE, eN = eN, ind = ind,
       invG = invG, invH = invH, Hmat = Hmat,
       h.x = h.x, h.y = h.y,
       ew = ew, eR = eR, sqrtw_R = sqrtw_R, w_R = w_R,
       eC = eC, count_p = count_p, dZ = dZ,
       out_Y = out_Y, out_Z = out_Z,
       w_vce = w_vce, partout = partout)
}


# =============================================================================
# 2. Core loop: fit all evaluation points at one polynomial order
# =============================================================================

rd2d_cov_core <- function(dat, Z_mat, eval_pts, e_deriv, poly_order,
                          hgrid.0, hgrid.1,
                          kernel, kernel_type, vce, C,
                          bwcheck, masspoints, unique_data) {
  
  neval   <- nrow(eval_pts)
  N       <- nrow(dat)
  sd.x1   <- sd(dat$x.1)
  sd.x2   <- sd(dat$x.2)
  
  if (is.null(hgrid.1)) hgrid.1 <- hgrid.0
  hgrid.0 <- as.matrix(hgrid.0)
  hgrid.1 <- as.matrix(hgrid.1)
  
  is_ctrl <- (dat$d == 0)
  is_trt  <- (dat$d == 1)
  
  # Storage
  tau_vec  <- rep(NA_real_, neval)
  se_vec   <- rep(NA_real_, neval)
  mu0_vec  <- rep(NA_real_, neval)
  mu1_vec  <- rep(NA_real_, neval)
  se0_vec  <- rep(NA_real_, neval)
  se1_vec  <- rep(NA_real_, neval)
  eN0_vec  <- rep(0L, neval)
  eN1_vec  <- rep(0L, neval)
  h_used_0 <- matrix(NA_real_, neval, 2)
  h_used_1 <- matrix(NA_real_, neval, 2)
  gamma_list <- vector("list", neval)
  
  # For cross-point covariance (half-vectors per obs, per eval point)
  # Store original-index half vectors of length N
  halves_0 <- vector("list", neval)
  halves_1 <- vector("list", neval)
  
  for (j in seq_len(neval)) {
    
    ev  <- eval_pts[j, ]
    vec <- e_deriv[j, ]
    
    # --- Center data ---
    dat_c <- dat[, c("x.1", "x.2", "y", "d")]
    dat_c$x.1 <- dat_c$x.1 - ev$x.1
    dat_c$x.2 <- dat_c$x.2 - ev$x.2
    
    # --- Bandwidth adjustment (bwcheck) ---
    h.0 <- as.numeric(hgrid.0[j, ])
    h.1 <- as.numeric(hgrid.1[j, ])
    
    if (!is.null(bwcheck)) {
      if (kernel_type == "prod") {
        dist_c <- pmax(abs(dat_c$x.1 / sd.x1), abs(dat_c$x.2 / sd.x2))
        mult <- c(sd.x1, sd.x2)
      } else {
        dist_c <- sqrt(dat_c$x.1^2 + dat_c$x.2^2)
        mult <- c(1, 1)
      }
      
      if (masspoints == "adjust" && !is.null(unique_data)) {
        u_c <- unique_data
        u_c$x.1 <- u_c$x.1 - ev$x.1
        u_c$x.2 <- u_c$x.2 - ev$x.2
        if (kernel_type == "prod") {
          dist_u <- pmax(abs(u_c$x.1 / sd.x1), abs(u_c$x.2 / sd.x2))
        } else {
          dist_u <- sqrt(u_c$x.1^2 + u_c$x.2^2)
        }
        sorted_0 <- sort(dist_u[u_c$d == 0])
        sorted_1 <- sort(dist_u[u_c$d == 1])
      } else {
        sorted_0 <- sort(dist_c[is_ctrl])
        sorted_1 <- sort(dist_c[is_trt])
      }
      
      N0_s <- length(sorted_0); N1_s <- length(sorted_1)
      bw_min_0 <- sorted_0[min(bwcheck, N0_s)] * mult
      bw_min_1 <- sorted_1[min(bwcheck, N1_s)] * mult
      bw_max_0 <- sorted_0[N0_s] * mult
      bw_max_1 <- sorted_1[N1_s] * mult
      
      same_bw <- all(hgrid.0[j, ] == hgrid.1[j, ])
      if (same_bw) {
        h.0 <- pmax(h.0, bw_min_0, bw_min_1)
        h.0 <- pmin(h.0, pmax(bw_max_0, bw_max_1))
        h.1 <- h.0
      } else {
        h.0 <- pmax(h.0, bw_min_0); h.0 <- pmin(h.0, bw_max_0)
        h.1 <- pmax(h.1, bw_min_1); h.1 <- pmin(h.1, bw_max_1)
      }
    }
    
    h_used_0[j, ] <- h.0
    h_used_1[j, ] <- h.1
    
    # --- Split sides ---
    dat_0 <- dat_c[is_ctrl, ]
    dat_1 <- dat_c[is_trt, ]
    Z_0 <- if (!is.null(Z_mat)) Z_mat[is_ctrl, , drop = FALSE] else NULL
    Z_1 <- if (!is.null(Z_mat)) Z_mat[is_trt, , drop = FALSE] else NULL
    C_0 <- if (!is.null(C)) C[is_ctrl] else NULL
    C_1 <- if (!is.null(C)) C[is_trt] else NULL
    
    # --- Fit both sides ---
    fit_0 <- rd2d_cov_side_fit(dat_0, Z_0, h.0, poly_order,
                               kernel, kernel_type, vce, C_0)
    fit_1 <- rd2d_cov_side_fit(dat_1, Z_1, h.1, poly_order,
                               kernel, kernel_type, vce, C_1)
    
    if (!fit_0$success || !fit_1$success) next
    
    eN0_vec[j] <- fit_0$eN
    eN1_vec[j] <- fit_1$eN
    dZ <- fit_0$dZ
    
    # --- Partialling out: pool across sides, estimate gamma ---
    if (dZ > 0) {
      ZWZ <- fit_0$partout$ZWZ + fit_1$partout$ZWZ
      ZWY <- fit_0$partout$ZWY + fit_1$partout$ZWY
      gamma <- tryCatch(
        as.vector(chol2inv(chol(ZWZ)) %*% ZWY),
        error = function(e) as.vector(MASS::ginv(ZWZ) %*% ZWY)
      )
    } else {
      gamma <- numeric(0)
    }
    gamma_list[[j]] <- gamma
    s_Y <- c(1, -gamma)   # adjustment vector: length 1 + dZ
    
    # --- Treatment effect (adjusted) ---
    # beta on each side: count_p x 1 for Y, count_p x 1 for each Z_k
    # Collect intercept-row values into a (1+dZ) vector per side
    mu_vec_0 <- c(as.numeric(vec %*% fit_0$out_Y$beta),
                  sapply(fit_0$out_Z, function(o) as.numeric(vec %*% o$beta)))
    mu_vec_1 <- c(as.numeric(vec %*% fit_1$out_Y$beta),
                  sapply(fit_1$out_Z, function(o) as.numeric(vec %*% o$beta)))
    
    tau_adj <- sum(s_Y * (mu_vec_1 - mu_vec_0))
    
    mu0_vec[j] <- mu_vec_0[1]  # Y conditional mean, control
    mu1_vec[j] <- mu_vec_1[1]  # Y conditional mean, treated
    tau_vec[j] <- tau_adj
    
    # --- Adjusted residuals (signed, raw) ---
    # adj_res_raw = res_Y - sum_k gamma_k * res_Zk
    adj_raw_0 <- fit_0$out_Y$res_raw
    adj_raw_1 <- fit_1$out_Y$res_raw
    if (dZ > 0) {
      for (k in 1:dZ) {
        adj_raw_0 <- adj_raw_0 - gamma[k] * fit_0$out_Z[[k]]$res_raw
        adj_raw_1 <- adj_raw_1 - gamma[k] * fit_1$out_Z[[k]]$res_raw
      }
    }
    
    # HC-adjusted signed residuals (for pointwise variance)
    adj_signed_0 <- adj_raw_0 * fit_0$w_vce
    adj_signed_1 <- adj_raw_1 * fit_1$w_vce
    
    # --- Pointwise variance via rd2d_vce ---
    sigma_0 <- rd2d_vce(fit_0$w_R, adj_signed_0, fit_0$eC,
                        c(fit_0$h.x, fit_0$h.y))
    sigma_1 <- rd2d_vce(fit_1$w_R, adj_signed_1, fit_1$eC,
                        c(fit_1$h.x, fit_1$h.y))
    
    cc_0 <- t(fit_0$invG) %*% sigma_0 %*% fit_0$invG
    cc_1 <- t(fit_1$invG) %*% sigma_1 %*% fit_1$invG
    
    v0 <- as.numeric(
      matrix(vec, nrow = 1) %*% fit_0$invH %*% cc_0 %*%
        fit_0$invH %*% matrix(vec, ncol = 1)
    ) / (fit_0$h.x * fit_0$h.y)
    
    v1 <- as.numeric(
      matrix(vec, nrow = 1) %*% fit_1$invH %*% cc_1 %*%
        fit_1$invH %*% matrix(vec, ncol = 1)
    ) / (fit_1$h.x * fit_1$h.y)
    
    se_vec[j] <- sqrt(max(v0 + v1, 0))
    
    # Side-specific SEs (for diagnostics)
    se0_vec[j] <- sqrt(max(v0, 0))
    se1_vec[j] <- sqrt(max(v1, 0))
    
    # --- Half-vectors for cross-point covariance ---
    # Following get_cov_half_v2 structure but with adjusted residuals.
    # half_t[i,:] = |adj_res_i| * w_vce_i * sqrt(h.x*h.y) * (ew_i * eR_i) %*% invG
    #
    # Then cov(j1,j2) on side t =
    #   vec' invH_j1 (half_j1[overlap,:]' half_j2[overlap,:]) invH_j2 vec
    #   / sqrt(h_j1.x * h_j1.y * h_j2.x * h_j2.y)
    
    build_half <- function(fit_t, adj_signed_t, side_idx) {
      # adj_signed_t already has HC adjustment applied (signed)
      # For half vectors we need |adj_signed_t|
      adj_abs <- abs(adj_signed_t)
      sqrth   <- sqrt(fit_t$h.x * fit_t$h.y)
      # n_eff x count_p matrix
      half_mat <- (adj_abs * sqrth) * fit_t$w_R   # broadcast: n_eff x count_p
      half_mat <- half_mat %*% fit_t$invG          # n_eff x count_p
      
      # Map back to original N-length indices for this side
      # side_idx: boolean mask into full data for this side (ctrl or trt)
      # fit_t$ind: boolean mask into side_idx for kernel window
      orig_idx <- which(side_idx)[fit_t$ind]
      list(mat = half_mat, orig_idx = orig_idx, invH = fit_t$invH,
           h.x = fit_t$h.x, h.y = fit_t$h.y)
    }
    
    halves_0[[j]] <- build_half(fit_0, adj_signed_0, is_ctrl)
    halves_1[[j]] <- build_half(fit_1, adj_signed_1, is_trt)
  }
  
  # === Full J x J covariance matrix ===
  cov_mat <- matrix(NA_real_, neval, neval)
  
  for (j1 in seq_len(neval)) {
    if (is.na(tau_vec[j1])) next
    for (j2 in j1:neval) {
      if (is.na(tau_vec[j2])) next
      
      cov_val <- 0
      
      # Accumulate over both sides (control and treated)
      for (side in list(list(h = halves_0), list(h = halves_1))) {
        hv_j1 <- side$h[[j1]]
        hv_j2 <- side$h[[j2]]
        if (is.null(hv_j1) || is.null(hv_j2)) next
        
        # Find overlapping observations (by original index)
        overlap <- intersect(hv_j1$orig_idx, hv_j2$orig_idx)
        if (length(overlap) == 0) next
        
        # Row indices within each half matrix
        rows_j1 <- match(overlap, hv_j1$orig_idx)
        rows_j2 <- match(overlap, hv_j2$orig_idx)
        
        vec_j1 <- e_deriv[j1, ]
        vec_j2 <- e_deriv[j2, ]
        
        # half_j1: n_overlap x count_p, half_j2: n_overlap x count_p
        # cov contribution = vec' invH_j1 (half_j1' half_j2) invH_j2 vec
        #                    / sqrt(h_j1 * h_j2)
        cross <- crossprod(hv_j1$mat[rows_j1, , drop = FALSE],
                           hv_j2$mat[rows_j2, , drop = FALSE])
        
        contrib <- as.numeric(
          matrix(vec_j1, nrow = 1) %*% hv_j1$invH %*% cross %*%
            hv_j2$invH %*% matrix(vec_j2, ncol = 1)
        ) / sqrt(hv_j1$h.x * hv_j1$h.y * hv_j2$h.x * hv_j2$h.y)
        
        cov_val <- cov_val + contrib
      }
      
      cov_mat[j1, j2] <- cov_val
      cov_mat[j2, j1] <- cov_val
    }
  }
  
  list(tau = tau_vec, se = se_vec, mu0 = mu0_vec, mu1 = mu1_vec,
       se0 = se0_vec, se1 = se1_vec,
       eN0 = eN0_vec, eN1 = eN1_vec,
       h_used_0 = h_used_0, h_used_1 = h_used_1,
       gamma = gamma_list, cov_mat = cov_mat)
}


# =============================================================================
# 3. Main estimation function: rd2d_cov
# =============================================================================
#
# Mirrors the rd2d() interface so that extract_aate_cov / run_rd2d_cov work.

rd2d_cov <- function(Y, X, t, Z = NULL,
                     b,
                     h = NULL,
                     p = 1, q = NULL,
                     kernel = "tri",
                     kernel_type = "prod",
                     vce = "hc1",
                     bwselect = "mserd",
                     bwcheck = NULL,
                     stdvars = TRUE,
                     level = 95,
                     bw_inflate = 1.0,
                     masspoints = "check",
                     C = NULL,
                     deriv = c(0, 0),
                     all.p = FALSE) {

  
  if (is.null(q)) q <- p + 1
  if (is.null(bwcheck)) bwcheck <- 50 + p + 1
  
  N <- length(Y)
  J <- nrow(b)
  dZ <- if (is.null(Z)) 0L else ncol(as.matrix(Z))
  
  cat(sprintf("  rd2d_cov: N=%d, J=%d, dZ=%d, p=%d, q=%d\n", N, J, dZ, p, q))
  
  # ---- Prepare data frame (matches rd2d internal format) ----
  X <- as.matrix(X)
  b <- as.matrix(b)
  dat <- data.frame(x.1 = X[, 1], x.2 = X[, 2], y = Y, d = t)
  
  if (!is.null(Z)) {
    Z <- as.matrix(Z)
    # Drop collinear covariates
    if (ncol(Z) > 1) {
      qr_Z <- qr(Z)
      keep <- qr_Z$pivot[seq_len(qr_Z$rank)]
      if (length(keep) < ncol(Z)) {
        cat(sprintf("  Dropping %d collinear covariate(s)\n", ncol(Z) - length(keep)))
        Z <- Z[, keep, drop = FALSE]
        dZ <- ncol(Z)
      }
    }
  }
  
  # ---- Unique data (for masspoints adjustment) ----
  unique_data <- NULL
  if (masspoints != "off") {
    unique_data <- unique(dat[, c("x.1", "x.2", "d")])
  }
  
  # ---- Evaluation points ----
  eval_pts <- data.frame(x.1 = b[, 1], x.2 = b[, 2])
  
  # ---- Derivative extraction vector ----
  count_p_p <- (p + 1) * (p + 2) / 2
  count_p_q <- (q + 1) * (q + 2) / 2
  
  # For deriv = (0,0), extract intercept (first element)
  # For general deriv, would need factorial scaling — for now support (0,0)
  e_deriv_p <- matrix(0, nrow = J, ncol = count_p_p)
  e_deriv_p[, 1] <- 1
  e_deriv_q <- matrix(0, nrow = J, ncol = count_p_q)
  e_deriv_q[, 1] <- 1
  
  # ---- Bandwidth selection via rdbw2d ----
  if (is.null(h)) {
    bw_fn <- rdbw2d_cov_full
    cat("  Selecting bandwidths via rdbw2d_cov_full ...\n")
    bw_res <- bw_fn(Y = Y, X = X, t = t, Z = Z, b = b,
                    p = p, kernel = kernel, kernel_type = kernel_type,
                    vce = vce, bwselect = bwselect,
                    bwcheck = bwcheck, stdvars = stdvars,
                    masspoints = masspoints, C = C)
    # mserd: columns 3,4 = h01, h02
    hgrid.0 <- cbind(bw_res$bws[, 3], bw_res$bws[, 4]) * bw_inflate
    if (bwselect == "msetwo") {
      hgrid.1 <- cbind(bw_res$bws[, 5], bw_res$bws[, 6]) * bw_inflate
    } else {
      hgrid.1 <- hgrid.0
    }
  }
  
  # ---- Fit at polynomial order p (point estimation) ----
  cat(sprintf("  Fitting p=%d (point estimation) ...\n", p))
  fit_p <- rd2d_cov_core(dat, Z, eval_pts, e_deriv_p, poly_order = p,
                         hgrid.0, hgrid.1,
                         kernel, kernel_type, vce, C,
                         bwcheck, masspoints, unique_data)
  
  # ---- Fit at polynomial order q (bias correction / inference) ----
  cat(sprintf("  Fitting q=%d (bias correction) ...\n", q))
  fit_q <- rd2d_cov_core(dat, Z, eval_pts, e_deriv_q, poly_order = q,
                         hgrid.0, hgrid.1,
                         kernel, kernel_type, vce, C,
                         bwcheck, masspoints, unique_data)
  
  # ---- Assemble results data.frame ----
  zvalues  <- fit_q$tau / fit_q$se
  pvalues  <- 2 * pnorm(abs(zvalues), lower.tail = FALSE)
  zval_ci  <- qnorm((level + 100) / 200)
  CI.lower <- fit_q$tau - zval_ci * fit_q$se
  CI.upper <- fit_q$tau + zval_ci * fit_q$se
  
  results_df <- data.frame(
    b1       = b[, 1],
    b2       = b[, 2],
    Est.p    = fit_p$tau,
    Se.p     = fit_p$se,
    Est.q    = fit_q$tau,
    Se.q     = fit_q$se,
    z        = zvalues,
    "P>|z|"  = pvalues,
    CI.lower = CI.lower,
    CI.upper = CI.upper,
    CB.lower = rep(NA_real_, J),
    CB.upper = rep(NA_real_, J),
    h01      = fit_q$h_used_0[, 1],
    h02      = fit_q$h_used_0[, 2],
    h11      = fit_q$h_used_1[, 1],
    h12      = fit_q$h_used_1[, 2],
    Nh0      = fit_q$eN0,
    Nh1      = fit_q$eN1,
    check.names = FALSE
  )
  
  # Side-specific results (for diagnostics)
  results_A0 <- data.frame(
    b1 = b[, 1], b2 = b[, 2],
    mu = fit_q$mu0, se = fit_q$se0,
    Nh = fit_q$eN0
  )
  results_A1 <- data.frame(
    b1 = b[, 1], b2 = b[, 2],
    mu = fit_q$mu1, se = fit_q$se1,
    Nh = fit_q$eN1
  )
  
  ok_p <- sum(!is.na(fit_p$tau))
  ok_q <- sum(!is.na(fit_q$tau))
  cat(sprintf("  Successful fits: %d/%d (p=%d), %d/%d (q=%d)\n",
              ok_p, J, p, ok_q, J, q))
  
  out <- list(
    results    = results_df,
    results.A0 = results_A0,
    results.A1 = results_A1,
    cov.p      = fit_p$cov_mat,
    cov.q      = fit_q$cov_mat,
    opt        = list(
      b = b, p = p, q = q, kernel = kernel, kernel_type = kernel_type,
      vce = vce, N = N, dZ = dZ, level = level,
      bwselect = if (is.null(h)) bwselect else "user",
      bw_inflate = bw_inflate,
      neval = J,
      h01 = results_df$h01, h02 = results_df$h02,
      h11 = results_df$h11, h12 = results_df$h12,
      Nh0 = fit_q$eN0, Nh1 = fit_q$eN1,
      gamma = fit_q$gamma
    ),
    rdmodel = "rd2d_cov"
  )
  class(out) <- "rd2d"
  out
}


# =============================================================================
# 4. Robust AATE extractor (handles NAs from failed evaluation points)
# =============================================================================

extract_aate_cov <- function(res, n_pts = NULL, include = NULL) {
  
  est     <- res$results$Est.q
  cov_mat <- res$cov.q
  if (is.null(n_pts)) n_pts <- length(est)
  
  # Valid points: non-NA estimates AND user-specified inclusion
  
  ok <- !is.na(est)
  if (!is.null(include)) {
    stopifnot(length(include) == n_pts)
    ok <- ok & include
  }
  
  n_ok <- sum(ok)
  if (n_ok == 0) {
    return(list(coef = NA, se = NA, z = NA, p = NA,
                ci_lower = NA, ci_upper = NA, n_pts = 0))
  }
  
  # Equal weights over valid points
  w_sub <- rep(1 / n_ok, n_ok)
  
  # AATE point estimate
  aate <- sum(w_sub * est[ok])
  
  # SE from subsetted covariance matrix
  cov_sub <- cov_mat[ok, ok, drop = FALSE]
  
  if (any(is.na(cov_sub))) {
    se_vec <- res$results$Se.q[ok]
    se <- sqrt(sum(w_sub^2 * se_vec^2, na.rm = TRUE))
    warning("Some covariance entries are NA; using diagonal approximation.")
  } else {
    se <- sqrt(max(as.numeric(t(w_sub) %*% cov_sub %*% w_sub), 0))
  }
  
  z <- aate / se
  p_val <- 2 * pnorm(abs(z), lower.tail = FALSE)
  ci <- aate + c(-1, 1) * qnorm(0.975) * se
  
  list(coef = aate, se = se, z = z, p = p_val,
       ci_lower = ci[1], ci_upper = ci[2], n_pts = n_ok)
}


# =============================================================================
# 5. Wrapper: drop-in replacement for run_rd2d() with covariates
# =============================================================================

run_rd2d_cov <- function(sf_data, outcome_col, Z_cols,
                         border_coords, n_pts, label,
                         p = 1, q = 2, bw_inflate = 1.0,
                         kernel = "tri", kernel_type = "prod",
                         vce = "hc1", bwselect = "mserd") {

  
  d <- sf_data
  ok <- !is.na(d[[outcome_col]]) & is.finite(d[[outcome_col]])
  for (zc in Z_cols) {
    ok <- ok & !is.na(d[[zc]]) & is.finite(d[[zc]])
  }
  d <- d[ok, ]
  
  Y <- d[[outcome_col]]
  X <- sf::st_coordinates(d)
  t_vec <- d$treatment
  
  # Prepare covariate matrix
  if (length(Z_cols) > 0) {
    Z_raw <- sf::st_drop_geometry(d)[, Z_cols, drop = FALSE]
    Z <- as.matrix(scale(Z_raw))
    Z[is.na(Z)] <- 0  # zero-variance columns
  } else {
    Z <- NULL
  }
  
  cat(sprintf("  %s (%d obs, %d covariates)...\n",
              label, length(Y), length(Z_cols)))
  
  res <- rd2d_cov(Y = Y, X = X, t = t_vec, Z = Z,
                  b = as.matrix(border_coords),
                  p = p, q = q,
                  kernel = kernel, kernel_type = kernel_type,
                  vce = vce, bwselect = bwselect,
                  bw_inflate = bw_inflate)
  
  aate <- extract_aate_cov(res, n_pts)
  
  cat(sprintf("    AATE = %.3f (SE = %.3f, p = %.4f)\n",
              aate$coef, aate$se, aate$p))
  
  list(res = res, aate = aate, label = label, n_obs = length(Y))
}


# =============================================================================
# 6. Print summary
# =============================================================================

print_rd2d_cov_summary <- function(res, label = "") {
  r <- res$results
  ok <- !is.na(r$Est.q)
  cat(sprintf("\n--- rd2d_cov summary: %s ---\n", label))
  cat(sprintf("  Polynomial order: p=%d, q=%d\n", res$opt$p, res$opt$q))
  cat(sprintf("  Covariates: %d\n", res$opt$dZ))
  cat(sprintf("  Eval points: %d (successful: %d)\n", res$opt$neval, sum(ok)))
  if (sum(ok) > 0) {
    cat(sprintf("  Median bandwidth: h1=%.1f, h2=%.1f\n",
                median(r$h01[ok]), median(r$h02[ok])))
    cat(sprintf("  Median eff. N: ctrl=%d, treat=%d\n",
                median(r$Nh0[ok]), median(r$Nh1[ok])))
    cat(sprintf("  Median |tau_q|: %.4f\n", median(abs(r$Est.q[ok]))))
    
    # Report gamma summaries if available
    gammas <- res$opt$gamma
    if (!is.null(gammas)) {
      g_ok <- gammas[ok]
      g_ok <- g_ok[!sapply(g_ok, is.null)]
      if (length(g_ok) > 0) {
        g_mat <- do.call(rbind, g_ok)
        cat(sprintf("  Gamma (median across eval pts): %s\n",
                    paste(round(apply(g_mat, 2, median), 4), collapse = ", ")))
      }
    }
  }
}