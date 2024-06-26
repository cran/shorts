#' Optimal profile functions
#'
#' Family of functions that serve a purpose of finding optimal sprint or force-velocity profile
#'
#' @param MSS,MAC Numeric vectors. Model parameters
#' @param F0,V0 Numeric vectors. FV profile parameters
#' @param bodymass Body mass in kg
#' @param inertia External inertia in kg (for example a weight vest, or a sled).
#'         Not included in the air resistance calculation
#' @param resistance External horizontal resistance in Newtons (for example tether device or a sled friction resistance)
#' @param distance Numeric vector
#' @inheritDotParams get_air_resistance
#' @references
#' Samozino P, Peyrot N, Edouard P, Nagahara R, Jimenez‐Reyes P, Vanwanseele B, Morin J. 2022.
#'      Optimal mechanical force-velocity profile for sprint acceleration performance.
#'       Scandinavian Journal of Medicine & Science in Sports 32:559–575. DOI: 10.1111/sms.14097.
#' @examples
#' MSS <- 10
#' MAC <- 8
#' bodymass <- 75
#'
#' fv <- create_FVP(MSS, MAC, bodymass)
#'
#' dist <- seq(5, 40, by = 5)
#'
#' opt_MSS_MAC_profile <- optimal_MSS_MAC(
#'   distance = dist,
#'   MSS,
#'   MAC
#' )[["profile_imb"]]
#'
#' opt_FV_profile <- optimal_FV(
#'   distance = dist,
#'   fv$F0,
#'   fv$V0,
#'   fv$bodymass
#' )[["profile_imb"]]
#'
#' opt_FV_profile_peak <- optimal_FV(
#'   distance = dist,
#'   fv$F0,
#'   fv$V0,
#'   fv$bodymass,
#'   method = "peak"
#' )[["profile_imb"]]
#'
#' plot(x = dist, y = opt_MSS_MAC_profile, type = "l", ylab = "Profile imbalance")
#' lines(x = dist, y = opt_FV_profile, type = "l", col = "blue")
#' lines(x = dist, y = opt_FV_profile_peak, type = "l", col = "red")
#' abline(h = 100, col = "gray", lty = 2)
#' @name optimal_functions
NULL

# ---------------------------------
# Functions to find optimal FV profile

# Scalar functions

# Function to find optimal profile while keeping the arithmetic Max Power the same
find_optimal_FV_scalar <- function(distance, F0, V0, bodymass = 75, inertia = 0, resistance = 0, ...) {
  opt_func <- function(par) {
    new_F0 <- F0 / par[1]
    new_V0 <- V0 * par[1]

    predict_time_at_distance_FV(
      distance = distance,
      F0 = new_F0,
      V0 = new_V0,
      bodymass = bodymass,
      inertia = inertia,
      resistance = resistance,
      ...
    )
  }

  get_optim_model <- function() {
    tryCatch(
      {
        stats::optim(
          par = 1,
          fn = opt_func,
          method = "Brent",
          lower = 1 / min(100, F0 / ifelse(resistance < 0, 0, resistance)),
          upper = min(100, F0 / ifelse(resistance < 0, 0, resistance))
        )
      },
      error = function(cond) {
        return(list(par = NA, value = NA))
      },
      warning = function(cond) {
        return(list(par = NA, value = NA))
      }
    )
  }

  results <- get_optim_model()

  F0_optim <- F0 / results$par
  V0_optim <- V0 * results$par

  FV_slope <- -(F0 / bodymass) / V0
  FV_slope_optim <- -(F0_optim / bodymass) / V0_optim

  profile_imb <- (FV_slope / FV_slope_optim) * 100

  t_orig <- predict_time_at_distance_FV(
    distance = distance,
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  ##############################################
  # Peak Power
  # Original profile
  converted <- convert_FVP(
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  MSS_conv <- converted$MSS
  MAC_conv <- converted$MAC

  Ppeak_dist <- find_peak_power_distance(
    MSS = MSS_conv,
    MAC = MAC_conv,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  Ppeak_time <- find_peak_power_time(
    MSS = MSS_conv,
    MAC = MAC_conv,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  # Optimal profile
  converted_optim <- convert_FVP(
    F0 = F0_optim,
    V0 = V0_optim,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  MSS_conv_optim <- converted_optim$MSS
  MAC_conv_optim <- converted_optim$MAC

  Ppeak_dist_optim <- find_peak_power_distance(
    MSS = MSS_conv_optim,
    MAC = MAC_conv_optim,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  Ppeak_time_optim <- find_peak_power_time(
    MSS = MSS_conv_optim,
    MAC = MAC_conv_optim,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  # Return the results
  list(
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    Pmax = F0 * V0 / 4,
    Pmax_rel = (F0 * V0 / 4) / bodymass,
    slope = FV_slope,
    distance = distance,
    time = t_orig,
    Ppeak = Ppeak_dist$peak_power,
    Ppeak_rel = Ppeak_dist$peak_power / bodymass,
    Ppeak_dist = Ppeak_dist$distance,
    Ppeak_time = Ppeak_time$time,
    F0_optim = F0_optim,
    F0_coef = 1 / results$par,
    V0_optim = V0_optim,
    V0_coef = results$par,
    Pmax_optim = F0_optim * V0_optim / 4,
    Pmax_rel_optim = (F0_optim * V0_optim / 4) / bodymass,
    slope_optim = FV_slope_optim,
    profile_imb = profile_imb,
    time_optim = results$value,
    time_gain = results$value - t_orig,
    Ppeak_optim = Ppeak_dist_optim$peak_power,
    Ppeak_rel_optim = Ppeak_dist_optim$peak_power / bodymass,
    Ppeak_dist_optim = Ppeak_dist_optim$distance,
    Ppeak_time_optim = Ppeak_time_optim$time
  )
}

# Assistant function for find_optimal_FV_peak_scalar()
find_FV_peak_power <- function(F0, V0, bodymass = 75, inertia = 0, resistance = 0, ...) {
  converted <- convert_FVP(
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  MSS_conv <- converted$MSS
  MAC_conv <- converted$MAC

  find_peak_power_distance(
    MSS = MSS_conv,
    MAC = MAC_conv,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )$peak_power
}

# Assistant function for find_optimal_FV_peak_scalar()
find_V0 <- function(F0, Ppeak, bodymass = 75, inertia = 0, resistance = 0, ...) {
  opt_func <- function(V0) {
    (Ppeak - find_FV_peak_power(
      F0 = F0,
      V0 = V0,
      bodymass = bodymass,
      inertia = inertia,
      resistance = resistance,
      ...
    ))
  }

  V0 <- (4 * Ppeak) / F0

  get_optim_model <- function() {
    tryCatch(
      {
        stats::uniroot(f = opt_func, interval = c(1 / 2, 2) * V0)$root
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  }
  get_optim_model()
}


# Function to find optimal profile while keeping the manifested Peak Power the same
find_optimal_FV_peak_scalar <- function(distance, F0, V0, bodymass = 75, inertia = 0, resistance = 0, ...) {
  Ppeak_orig <- find_FV_peak_power(
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  opt_func <- function(par) {
    new_F0 <- par
    new_V0 <- find_V0(
      F0 = new_F0,
      Ppeak = Ppeak_orig,
      bodymass = bodymass,
      inertia = inertia,
      resistance = resistance,
      ...
    )

    predict_time_at_distance_FV(
      distance = distance,
      F0 = new_F0,
      V0 = new_V0,
      bodymass = bodymass,
      inertia = inertia,
      resistance = resistance,
      ...
    )
  }

  F0_optim <- find_optimal_FV_scalar(
    distance = distance,
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )$F0_optim

  get_optim_model <- function() {
    tryCatch(
      {
        stats::optim(
          par = F0_optim,
          fn = opt_func,
          method = "Brent",
          lower = F0_optim * 0.8,
          upper = F0_optim * 1.2
        )
      },
      error = function(cond) {
        return(list(par = NA, value = NA))
      },
      warning = function(cond) {
        return(list(par = NA, value = NA))
      }
    )
  }

  results <- get_optim_model()

  F0_optim <- results$par
  V0_optim <- find_V0(
    F0 = F0_optim,
    Ppeak = Ppeak_orig,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  FV_slope <- -(F0 / bodymass) / V0
  FV_slope_optim <- -(F0_optim / bodymass) / V0_optim

  profile_imb <- (FV_slope / FV_slope_optim) * 100

  # Probe
  t_orig <- predict_time_at_distance_FV(
    distance = distance,
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  ##############################################
  # Peak Power
  # Original profile
  converted <- convert_FVP(
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  MSS_conv <- converted$MSS
  MAC_conv <- converted$MAC

  Ppeak_dist <- find_peak_power_distance(
    MSS = MSS_conv,
    MAC = MAC_conv,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  Ppeak_time <- find_peak_power_time(
    MSS = MSS_conv,
    MAC = MAC_conv,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  # Optimal profile
  converted_optim <- convert_FVP(
    F0 = F0_optim,
    V0 = V0_optim,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  MSS_conv_optim <- converted_optim$MSS
  MAC_conv_optim <- converted_optim$MAC

  Ppeak_dist_optim <- find_peak_power_distance(
    MSS = MSS_conv_optim,
    MAC = MAC_conv_optim,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  Ppeak_time_optim <- find_peak_power_time(
    MSS = MSS_conv_optim,
    MAC = MAC_conv_optim,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  # Return the results
  list(
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    Pmax = F0 * V0 / 4,
    Pmax_rel = (F0 * V0 / 4) / bodymass,
    slope = FV_slope,
    distance = distance,
    time = t_orig,
    Ppeak = Ppeak_dist$peak_power,
    Ppeak_rel = Ppeak_dist$peak_power / bodymass,
    Ppeak_dist = Ppeak_dist$distance,
    Ppeak_time = Ppeak_time$time,
    F0_optim = F0_optim,
    F0_coef = F0_optim / F0,
    V0_optim = V0_optim,
    V0_coef = V0_optim / V0,
    Pmax_optim = F0_optim * V0_optim / 4,
    Pmax_rel_optim = (F0_optim * V0_optim / 4) / bodymass,
    slope_optim = FV_slope_optim,
    profile_imb = profile_imb,
    time_optim = results$value,
    time_gain = results$value - t_orig,
    Ppeak_optim = Ppeak_dist_optim$peak_power,
    Ppeak_rel_optim = Ppeak_dist_optim$peak_power / bodymass,
    Ppeak_dist_optim = Ppeak_dist_optim$distance,
    Ppeak_time_optim = Ppeak_time_optim$time
  )
}

# Vector function

#' @rdname optimal_functions
#' @description \code{optimal_FV} finds "optimal" \code{F0} and \code{V0} where time at distance is
#'     minimized, while keeping the power the same
#' @param method Method to be utilized. Options are "peak" and "max" (default)
#' @return \code{optimal_FV} returns s data frame with the following columns
#'     \describe{
#'         \item{F0}{Original F0}
#'         \item{V0}{Original F0}
#'         \item{bodymass}{Bodymass}
#'         \item{inertia}{Inertia}
#'         \item{resistance}{Resistance}
#'         \item{Pmax}{Maximal power estimated using F0 * V0 / 4}
#'         \item{Pmax_rel}{Relative maximal power}
#'         \item{slope}{FV profile slope}
#'         \item{distance}{Distance}
#'         \item{time}{Time to cover distance}
#'         \item{Ppeak}{Peak power estimated quantitatively}
#'         \item{Ppeak_rel}{Relative peak power}
#'         \item{Ppeak_dist}{Distance at which peak power is manifested}
#'         \item{Ppeak_time}{Time at which peak power is manifested}
#'         \item{F0_optim}{Optimal F0}
#'         \item{F0_coef}{Ratio between F0_optim an F0}
#'         \item{V0_optim}{Optimal V0}
#'         \item{V0_coef}{Ratio between V0_optim an V0}
#'         \item{Pmax_optim}{Optimal maximal power estimated F0_optim * V0_optim / 4}
#'         \item{Pmax_rel_optim}{Optimal relative maximal power}
#'         \item{slope_optim}{Optimal FV profile slope}
#'         \item{profile_imb}{Percent ratio between slope and optimal slope}
#'         \item{time_optim}{Time to cover distance when profile is optimal}
#'         \item{time_gain}{Difference in time to cover distance between time_optimal and time}
#'         \item{Ppeak_optim}{Optimal peak power estimated quantitatively}
#'         \item{Ppeak_rel_optim}{Optimal relative peak power}
#'         \item{Ppeak_dist_optim}{Distance at which optimal peak power is manifested}
#'         \item{Ppeak_time_optim}{Time at which optimal peak power is manifested}
#'    }
#' @export
optimal_FV <- function(distance, F0, V0, bodymass = 75, inertia = 0, resistance = 0, method = "max", ...) {
  df <- data.frame(
    distance = distance,
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    method = method,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  df$id <- sprintf(paste0("%0", floor(log10(nrow(df))) + 1, "d"), seq(1, nrow(df)))

  df_list <- split(df, df$id)

  purrr::map_df(df_list, function(.x) {
    if (!(.x$method %in% c("peak", "max"))) {
      stop("Unknown method of the profile optimization. Please use 'max' or 'peak'")
    }

    if (.x$method == "max") {
      optim_func <- find_optimal_FV_scalar
    } else if (.x$method == "peak") {
      optim_func <- find_optimal_FV_peak_scalar
    }

    .x$method <- NULL
    .x$id <- NULL

    res <- do.call(optim_func, as.list(.x))

    data.frame(res)
  })
}

# ------------------------------------------------
# Functions for optimal sprint profile

find_optimal_MSS_MAC_scalar <- function(distance, MSS, MAC) {
  opt_func <- function(par) {
    new_MSS <- MSS / par[1]
    new_MAC <- MAC * par[1]

    predict_time_at_distance(
      distance = distance,
      MSS = new_MSS,
      MAC = new_MAC
    )
  }

  get_optim_model <- function() {
    tryCatch(
      {
        stats::optim(
          par = 1,
          fn = opt_func,
          method = "Brent",
          lower = 0,
          upper = 10
        )
      },
      error = function(cond) {
        return(list(par = NA, value = NA))
      },
      warning = function(cond) {
        return(list(par = NA, value = NA))
      }
    )
  }

  results <- get_optim_model()

  MSS_optim <- MSS / results$par
  MAC_optim <- MAC * results$par

  slope <- -MAC / MSS
  slope_optim <- -MAC_optim / MSS_optim

  profile_imb <- (slope / slope_optim) * 100

  # Probe
  t_orig <- predict_time_at_distance(
    distance = distance,
    MSS = MSS,
    MAC = MAC
  )

  # Return the results
  list(
    MSS = MSS,
    MAC = MAC,
    Pmax_rel = MSS * MAC / 4,
    slope = slope,
    distance = distance,
    time = t_orig,
    MSS_optim = MSS_optim,
    MSS_coef = 1 / results$par,
    MAC_optim = MAC_optim,
    MAC_coef = results$par,
    Pmax_rel_optim = MSS * MAC / 4,
    slope_optim = slope_optim,
    profile_imb = profile_imb,
    time_optim = results$value,
    time_gain = results$value - t_orig
  )
}


#' @rdname optimal_functions
#' @description \code{optimal_MSS_MAC} finds "optimal" \code{MSS} and \code{MAS} where time at distance is
#'     minimized, while keeping the \code{Pmax} the same
#' @return \code{optimal_MSS_MAC} returns a data frame with the following columns
#'     \describe{
#'         \item{MSS}{Original MSS}
#'         \item{MAC}{Original MAC}
#'         \item{Pmax_rel}{Relative maximal power estimated using MSS * MAC / 4}
#'         \item{slope}{Sprint profile slope}
#'         \item{distance}{Distance}
#'         \item{time}{Time to cover distance}
#'         \item{MSS_optim}{Optimal MSS}
#'         \item{MSS_coef}{Ratio between MSS_optim an MSS}
#'         \item{MAC_optim}{Optimal MAC}
#'         \item{MAC_coef}{Ratio between MAC_optim an MAC}
#'         \item{Pmax_rel_optim}{Optimal relative maximal power estimated using MSS_optim * MAC_optim / 4}
#'         \item{slope_optim}{Optimal sprint profile slope}
#'         \item{profile_imb}{Percent ratio between slope and optimal slope}
#'         \item{time_optim}{Time to cover distance when profile is optimal}
#'         \item{time_gain}{Difference in time to cover distance between time_optimal and time}
#'    }
#' @export
optimal_MSS_MAC <- function(distance, MSS, MAC) {
  df <- data.frame(
    distance = distance,
    MSS = MSS,
    MAC = MAC
  )

  df$id <- sprintf(paste0("%0", floor(log10(nrow(df))) + 1, "d"), seq(1, nrow(df)))

  df_list <- split(df, df$id)

  purrr::map_df(df_list, function(.x) {
    data.frame(find_optimal_MSS_MAC_scalar(
      distance = .x$distance,
      MSS = .x$MSS,
      MAC = .x$MAC
    ))
  })
}
