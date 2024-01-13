# Internal prepare data function
prepare_in_situ_data <- function(df,
                                 velocity_threshold,
                                 velocity_step,
                                 n_observations,
                                 na.rm) {

  # Remove acceleration over threshold
  df <- df[df$acceleration >= 0, ]

  # Remove velocity over threshold
  df <- df[df$velocity >= velocity_threshold, ]

  # Create groups
  df$group <- cut(
    df$velocity,
    breaks = seq(velocity_threshold, max(df$velocity, na.rm = TRUE) + 1, velocity_step),
    labels = FALSE,
    include.lowest = TRUE
  )

  # Extract observations per group
  df_list <- split(df, df$group)

  purrr::map_dfr(df_list, function(x) {
    best_acc <- x[order(x$acceleration, decreasing = TRUE), ]
    best_acc[seq(1, n_observations), ]
  })
}

#' @rdname model_functions
#' @description \code{model_in_situ} estimates short sprint parameters using velocity-acceleration trace,
#'     provided by the monitoring systems like GPS or LPS. See references for the information
#'
#' @param velocity_threshold Default is 3 m/s
#' @param velocity_step Velocity increment size for finding max acceleration. Default is 0.2 m/s
#' @param n_observations Number of top acceleration observations to keep in velocity bracket.
#'      Default is 2
#'
#' @references
#'     Clavel, P., Leduc, C., Morin, J.-B., Buchheit, M., & Lacome, M. (2023).
#'     Reliability of individual acceleration-speed profile in-situ in elite youth
#'     soccer players. Journal of Biomechanics, 153, 111602.
#'     https://doi.org/10.1016/j.jbiomech.2023.111602
#'
#'     Morin, J.-B. (2021). The “in-situ” acceleration-speed profile for team
#'     sports: testing players without testing them. JB Morin, PhD – Sport Science website.
#'     Accessed 31. Dec. 2023.
#'     https://jbmorin.net/2021/07/29/the-in-situ-sprint-profile-for-team-sports-testing-players-without-testing-them/
#'
#' @export
#' @examples
#'
#' # Model In-Situ (Embedded profiling)
#' data("LPS_session")
#' m1 <- model_in_situ(LPS_session$velocity, LPS_session$acceleration)
#' m1
#' plot(m1)
#'
model_in_situ <- function(velocity,
                          acceleration,
                          weights = 1,
                          velocity_threshold = 3,
                          velocity_step = 0.2,
                          n_observations = 2,
                          CV = NULL,
                          na.rm = FALSE,
                          ...) {

  # Estimation function
  model_func <- function(train, test, ...) {

    # Filter data
    train <- prepare_in_situ_data(
      train,
      velocity_threshold = velocity_threshold,
      velocity_step = velocity_step,
      n_observations = n_observations,
      na.rm = na.rm
    )

    test <- prepare_in_situ_data(
      test,
      velocity_threshold = velocity_threshold,
      velocity_step = velocity_step,
      n_observations = n_observations,
      na.rm = na.rm
    )

    # Linear model
    model <- stats::lm(
      acceleration ~ velocity,
      data = train,
      weights = train$weight
    )

    # Get parameters
    slope <- stats::coef(model)[[2]]
    MAC <- stats::coef(model)[[1]]

    MSS <- -MAC / slope
    TAU <- MSS / MAC

    PMAX <- MSS * MAC / 4

    # Model fit
    pred_acceleration <- stats::predict(model, newdata = data.frame(velocity = test$velocity))
    resid_acceleration <- test$acceleration - pred_acceleration

    return(list(
      data = train,
      model_info = list(
        predictor = "velocity",
        target = "acceleration"
      ),
      model = model,
      parameters = list(
        MSS = MSS,
        MAC = MAC,
        TAU = TAU,
        PMAX = PMAX
      ),
      corrections = NULL,
      predictions = list(
        .predictor = test$velocity,
        .observed = test$acceleration,
        .predicted = pred_acceleration,
        .residual = resid_acceleration
      )
    ))
  }

  model_sprint(
    df = data.frame(
      velocity = velocity,
      acceleration = acceleration,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}