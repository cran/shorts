## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 6,
  fig.height = 4,
  dpi = 80,
  out.width = "90%",
  auto_pdf = TRUE,
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(shorts)
library(tidyverse)

set.seed(1667)

john_MSS <- 8.5
john_MAC <- 7.5
john_TAU <- john_MSS / john_MAC

john_kinematics <- tibble(
  name = "John",
  distance = seq(0, 40, length.out = 10^4),
  time = shorts::predict_time_at_distance(distance, john_MSS, john_TAU),
  velocity = shorts::predict_velocity_at_distance(distance, john_MSS, john_TAU),
  acceleration = shorts::predict_acceleration_at_distance(distance, john_MSS, john_TAU),
  power = velocity * acceleration
)

## -----------------------------------------------------------------------------
john_kinematics_per_distance <- john_kinematics %>%
  gather("metric", "value", -name, -distance)

ggplot(john_kinematics_per_distance, aes(x = distance, y = value, color = name)) +
  theme_minimal() +
  geom_line(alpha = 0.8) +
  facet_wrap(~metric, scales = "free_y") +
  ylab(NULL)

## -----------------------------------------------------------------------------
john_kinematics_per_time <- john_kinematics %>%
  gather("metric", "value", -name, -time)

ggplot(john_kinematics_per_time, aes(x = time, y = value, color = name)) +
  theme_minimal() +
  geom_line(alpha = 0.8) +
  facet_wrap(~metric, scales = "free_y") +
  ylab(NULL)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics(path = "john-and-jack.png")

## -----------------------------------------------------------------------------
jack_kinematics <- john_kinematics

jack_kinematics <- jack_kinematics %>%
  # Remove those first 0.5m
  filter(distance >= 0.5) %>%

  # Now deduct time and distance
  mutate(
    distance = distance - min(distance),
    time = time - min(time),

    # And rename the athlete
    name = "Jack"
  )

## -----------------------------------------------------------------------------
sprint_kinematics <- rbind(
  john_kinematics,
  jack_kinematics
)

sprint_kinematics$name <- factor(
  sprint_kinematics$name,
  levels = c("John", "Jack")
)

## -----------------------------------------------------------------------------
kinematics_per_distance <- sprint_kinematics %>%
  gather("metric", "value", -name, -distance)

ggplot(kinematics_per_distance, aes(x = distance, y = value, color = name)) +
  theme_minimal() +
  geom_line(alpha = 0.8) +
  facet_wrap(~metric, scales = "free_y") +
  ylab(NULL)

## -----------------------------------------------------------------------------
kinematics_per_time <- sprint_kinematics %>%
  gather("metric", "value", -name, -time)

ggplot(kinematics_per_time, aes(x = time, y = value, color = name)) +
  theme_minimal() +
  geom_line(alpha = 0.8) +
  facet_wrap(~metric, scales = "free_y") +
  ylab(NULL)

## -----------------------------------------------------------------------------
sprint_distance <- c(5, 10, 20, 30, 40)

john_split_kinematics <- tibble(
  name = "John",
  distance = sprint_distance,
  time = shorts::predict_time_at_distance(distance, john_MSS, john_TAU)
)

john_split_kinematics

jack_split_kinematics <- tibble(
  name = "Jack",
  distance = sprint_distance,
  true_distance = distance + 0.5,
  true_time = shorts::predict_time_at_distance(true_distance, john_MSS, john_TAU),
  `time_05m` = shorts::predict_time_at_distance(0.5, john_MSS, john_TAU),
  time = true_time - `time_05m`
)

jack_split_kinematics

## -----------------------------------------------------------------------------
# Since this is a perfect simulation and stats::nls will complain
# we need to add very small noise, or measurement error
john_split_kinematics$time <- john_split_kinematics$time + rnorm(length(sprint_distance), 0, 10^-5)
jack_split_kinematics$time <- jack_split_kinematics$time + rnorm(length(sprint_distance), 0, 10^-5)

john_split_params <- with(
  john_split_kinematics,
  shorts::model_using_splits(distance, time)
)

jack_split_params <- with(
  jack_split_kinematics,
  shorts::model_using_splits(distance, time)
)

split_parameters <- rbind(
  unlist(john_split_params$parameters),
  unlist(jack_split_params$parameters)
)

rownames(split_parameters) <- c("John", "Jack")

round(split_parameters, 2)

## -----------------------------------------------------------------------------
split_model_fit <- rbind(
  unlist(john_split_params$model_fit),
  unlist(jack_split_params$model_fit)
)

rownames(split_model_fit) <- c("John", "Jack")

round(split_model_fit, 3)

## -----------------------------------------------------------------------------
john_split_kinematics$predicted_time <- shorts::predict_time_at_distance(
  sprint_distance,
  john_split_params$parameters$MSS,
  john_split_params$parameters$TAU
)

jack_split_kinematics$predicted_time <- shorts::predict_time_at_distance(
  sprint_distance,
  jack_split_params$parameters$MSS,
  jack_split_params$parameters$TAU
)

split_kinematics <- rbind(
  john_split_kinematics,
  select(jack_split_kinematics, name, distance, time, predicted_time)
)

split_kinematics$name <- factor(
  split_kinematics$name,
  levels = c("John", "Jack")
)

split_kinematics$difference <- with(
  split_kinematics,
  predicted_time - time
)

ggplot(split_kinematics, aes(x = distance, y = difference, color = name)) +
  theme_minimal() +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.8) +
  ylab("Predicted - observed")

## -----------------------------------------------------------------------------
jack_params_with_correction <- with(
  jack_split_kinematics,
  shorts::model_using_splits(distance, time, time_correction = 0.3)
)

unlist(jack_params_with_correction$parameters)

## -----------------------------------------------------------------------------
unlist(jack_params_with_correction$model_fit)

## -----------------------------------------------------------------------------
jack_split_kinematics_corrected <- select(jack_split_kinematics, name, distance, time)

jack_split_kinematics_corrected$predicted_time <- shorts::predict_time_at_distance(
  sprint_distance,
  jack_params_with_correction$parameters$MSS,
  jack_params_with_correction$parameters$TAU,
  time_correction = 0.3
)

jack_split_kinematics_corrected$difference <- with(
  jack_split_kinematics_corrected,
  predicted_time - time
)

jack_split_kinematics_corrected$name <- "Jack w/0.3s correction"

split_kinematics <- rbind(
  split_kinematics,
  jack_split_kinematics_corrected
)

ggplot(split_kinematics, aes(x = distance, y = difference, color = name)) +
  theme_minimal() +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.8) +
  ylab("Predicted - observed")

## -----------------------------------------------------------------------------
jack_param_with_estimated_time_correction <- with(
  jack_split_kinematics,
  shorts::model_using_splits_with_time_correction(distance, time)
)

unlist(jack_param_with_estimated_time_correction$parameters)

## -----------------------------------------------------------------------------
unlist(jack_param_with_estimated_time_correction$model_fit)

## -----------------------------------------------------------------------------
jack_split_kinematics_corrected_est <- select(jack_split_kinematics, name, distance, time)

jack_split_kinematics_corrected_est$predicted_time <- shorts::predict_time_at_distance(
  sprint_distance,
  jack_param_with_estimated_time_correction$parameters$MSS,
  jack_param_with_estimated_time_correction$parameters$TAU,
  jack_param_with_estimated_time_correction$parameters$time_correction
)

jack_split_kinematics_corrected_est$difference <- with(
  jack_split_kinematics_corrected_est,
  predicted_time - time
)

jack_split_kinematics_corrected_est$name <- "Jack w/est. correction"

split_kinematics <- rbind(
  split_kinematics,
  jack_split_kinematics_corrected_est
)

ggplot(split_kinematics, aes(x = distance, y = difference, color = name)) +
  theme_minimal() +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.8) +
  ylab("Predicted - observed")

## -----------------------------------------------------------------------------
jack_param_with_estimated_distance_correction <- with(
  jack_split_kinematics,
  shorts::model_using_splits_with_corrections(distance, time)
)

unlist(jack_param_with_estimated_distance_correction$parameters)

## -----------------------------------------------------------------------------
unlist(jack_param_with_estimated_distance_correction$model_fit)

## -----------------------------------------------------------------------------
jack_split_kinematics_corrected_est_dist <- select(jack_split_kinematics, name, distance, time)

jack_split_kinematics_corrected_est_dist$predicted_time <- shorts::predict_time_at_distance(
  sprint_distance,
  jack_param_with_estimated_distance_correction$parameters$MSS,
  jack_param_with_estimated_distance_correction$parameters$TAU,
  jack_param_with_estimated_distance_correction$parameters$time_correction,
  jack_param_with_estimated_distance_correction$parameters$distance_correction
)

jack_split_kinematics_corrected_est_dist$difference <- with(
  jack_split_kinematics_corrected_est_dist,
  predicted_time - time
)

jack_split_kinematics_corrected_est_dist$name <- "Jack w/est. distance correction"

split_kinematics <- rbind(
  split_kinematics,
  jack_split_kinematics_corrected_est_dist
)

ggplot(split_kinematics, aes(x = distance, y = difference, color = name)) +
  theme_minimal() +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.8) +
  ylab("Predicted - observed")

## -----------------------------------------------------------------------------
# Add small noise to the data so the model with distance and time corrected can be fitted
jack_kinematics$time <- jack_kinematics$time + rnorm(nrow(jack_kinematics), 0, 10^-5)

simple_model <- with(
  jack_kinematics,
  shorts::model_using_splits(distance, time)
)

simple_model_kinematics <- jack_kinematics %>%
  mutate(
    name = "simple model",
    time = shorts::predict_time_at_distance(
      distance,
      simple_model$parameters$MSS,
      simple_model$parameters$TAU
    ),
    velocity = shorts::predict_velocity_at_distance(
      distance,
      simple_model$parameters$MSS,
      simple_model$parameters$TAU
    ),
    acceleration = shorts::predict_acceleration_at_distance(
      distance,
      simple_model$parameters$MSS,
      simple_model$parameters$TAU
    ),
    power = velocity * acceleration
  )

# -----------------------------------
simple_model_corrected <- with(
  jack_kinematics,
  shorts::model_using_splits(distance, time, time_correction = 0.3)
)

simple_model_corrected_kinematics <- jack_kinematics %>%
  mutate(
    name = "simple model w/0.3s correction",
    time = shorts::predict_time_at_distance(
      distance,
      simple_model_corrected$parameters$MSS,
      simple_model_corrected$parameters$TAU,
      time_correction = 0.3
    ),
    velocity = shorts::predict_velocity_at_distance(
      distance,
      simple_model_corrected$parameters$MSS,
      simple_model_corrected$parameters$TAU,
      time_correction = 0.3
    ),
    acceleration = shorts::predict_acceleration_at_distance(
      distance,
      simple_model_corrected$parameters$MSS,
      simple_model_corrected$parameters$TAU,
      time_correction = 0.3
    ),
    power = velocity * acceleration
  )

# -----------------------------------
estimate_time_correction <- with(
  jack_kinematics,
  shorts::model_using_splits_with_time_correction(distance, time)
)

estimate_time_correction_kinematics <- jack_kinematics %>%
  mutate(
    name = "Model w/time correction",
    time = shorts::predict_time_at_distance(
      distance,
      estimate_time_correction$parameters$MSS,
      estimate_time_correction$parameters$TAU,
      estimate_time_correction$parameters$time_correction
    ),
    velocity = shorts::predict_velocity_at_distance(
      distance,
      estimate_time_correction$parameters$MSS,
      estimate_time_correction$parameters$TAU,
      estimate_time_correction$parameters$time_correction
    ),
    acceleration = shorts::predict_acceleration_at_distance(
      distance,
      estimate_time_correction$parameters$MSS,
      estimate_time_correction$parameters$TAU,
      estimate_time_correction$parameters$time_correction
    ),
    power = velocity * acceleration
  )

# -----------------------------------
# Remove zero from the data, otherwise model will complain
jack_kinematics_filtered <- jack_kinematics %>%
  filter(distance > 0.1)

estimate_corrections <- with(
  jack_kinematics_filtered,
  shorts::model_using_splits_with_corrections(distance, time)
)

estimate_corrections_kinematics <- jack_kinematics_filtered %>%
  mutate(
    name = "Model w/time & distance correction",
    time = shorts::predict_time_at_distance(
      distance,
      estimate_corrections$parameters$MSS,
      estimate_corrections$parameters$TAU,
      estimate_corrections$parameters$time_correction,
      estimate_corrections$parameters$distance_correction
    ),
    velocity = shorts::predict_velocity_at_distance(
      distance,
      estimate_corrections$parameters$MSS,
      estimate_corrections$parameters$TAU,
      estimate_corrections$parameters$time_correction,
      estimate_corrections$parameters$distance_correction
    ),
    acceleration = shorts::predict_acceleration_at_distance(
      distance,
      estimate_corrections$parameters$MSS,
      estimate_corrections$parameters$TAU,
      estimate_corrections$parameters$time_correction,
      estimate_corrections$parameters$distance_correction
    ),
    power = velocity * acceleration
  )

# -----------------------------------------------------
# Plot
sprint_kinematics <- rbind(
  jack_kinematics,
  simple_model_kinematics,
  simple_model_corrected_kinematics,
  estimate_time_correction_kinematics,
  estimate_corrections_kinematics
)

kinematics_per_distance <- sprint_kinematics %>%
  gather("metric", "value", -name, -distance)

ggplot(kinematics_per_distance, aes(x = distance, y = value, color = name)) +
  theme_minimal() +
  geom_line(alpha = 0.8) +
  facet_wrap(~metric, scales = "free_y") +
  ylab(NULL) +
  theme(legend.text = element_text(size = 6))

kinematics_per_time <- sprint_kinematics %>%
  gather("metric", "value", -name, -time)

ggplot(kinematics_per_time, aes(x = time, y = value, color = name)) +
  theme_minimal() +
  geom_line(alpha = 0.8) +
  facet_wrap(~metric, scales = "free_y") +
  ylab(NULL) +
  theme(legend.text = element_text(size = 6))

## -----------------------------------------------------------------------------
time_correction <- 0.3

# Wrapper functions
ind_model_wrapper <- function(data, ...) {
  sprint_mod <- shorts::model_using_splits(
    distance = data$distance,
    time = data$time,
    ...
  )
  
  data.frame(
    sprint_mod$parameters
  )
}

ind_model_wrapper_with_time_correction <- function(data, ...) {
  sprint_mod <- shorts::model_using_splits(
    distance = data$distance,
    time = data$time,
    time_correction = time_correction,
    ...
  )
  
  data.frame(
    sprint_mod$parameters
  )
}

ind_model_wrapper_with_time_correction_estimation <- function(data, ...) {
  sprint_mod <- shorts::model_using_splits_with_time_correction(
    distance = data$distance,
    time = data$time,
    ...
  )
  
  data.frame(
    sprint_mod$parameters
  )
}

ind_model_wrapper_with_correction_estimations <- function(data, ...) {
  sprint_mod <- shorts::model_using_splits_with_corrections(
    distance = data$distance,
    time = data$time,
    ...
  )
  
  data.frame(
    sprint_mod$parameters
  )
}

# -----------------------------------------------
set.seed(1667)


sim_df <- expand_grid(
  simulation = 1:1,
  true_MSS = c(7, 8, 9),
  true_TAU = c(0.6, 0.75, 0.9),
  cheating_distance = seq(0, 1, length.out = 10),
  distance = c(5, 10, 15, 20, 30, 40)
) %>%
  mutate(
    true_MAC = true_MSS / true_TAU,
    true_PMAX = (true_MSS * true_MAC) / 4,
    true_distance = distance + cheating_distance,
    cheating_time = predict_time_at_distance(cheating_distance, true_MSS, true_TAU),
    true_time = predict_time_at_distance(true_distance, true_MSS, true_TAU),
    time = true_time - cheating_time,
    # Add small noise to allow fitting
    time = time + rnorm(n(), 0, 10^-5)
  ) 

ind_model <- sim_df %>%
  group_by(simulation, cheating_distance, true_MSS, true_TAU) %>%
  do(ind_model_wrapper(.)) %>%
  mutate(time_correction = 0)

ind_model_with_time_correction <- sim_df %>%
  group_by(simulation, cheating_distance, true_MSS, true_TAU) %>%
  do(ind_model_wrapper_with_time_correction(.)) %>%
  mutate(time_correction = time_correction)

ind_model_with_time_correction_estimation <- sim_df %>%
  group_by(simulation, cheating_distance, true_MSS, true_TAU) %>%
  do(ind_model_wrapper_with_time_correction_estimation(., control = nls.control(tol = 0.1)))

ind_model_with_time_and_distance_correction_estimation <- sim_df %>%
  group_by(simulation, cheating_distance, true_MSS, true_TAU) %>%
  do(ind_model_wrapper_with_correction_estimations(., control = nls.control(tol = 0.1)))

combined_parameters <- rbind(
  data.frame(
    model = "norm",
    ind_model
  ),
  data.frame(
    model = "norm+time correction",
    ind_model_with_time_correction
  ),
  data.frame(
    model = "norm+time estimation",
    ind_model_with_time_correction_estimation
  ),
  data.frame(
    model = "norm+time+dist estimation",
    ind_model_with_time_and_distance_correction_estimation))

# Combine
sim_df <- left_join(
  combined_parameters,
  sim_df,
  by = c("simulation", "cheating_distance", "true_MSS", "true_TAU")
) %>%
  mutate(
    pred_time = predict_time_at_distance(distance, MSS, TAU, time_correction),
    sim_id = paste0(simulation, "-", cheating_distance)
  )

head(sim_df)

## -----------------------------------------------------------------------------
sim_df <- sim_df %>%
  mutate(
    true_MSS_text = factor(paste0("MSS = ", true_MSS)),
    true_TAU_text = factor(paste0("TAU = ", true_TAU))
  )

ggplot(
  sim_df,
  aes(x = cheating_distance, y = MSS, color = model)
) +
  theme_minimal(8) +
  geom_hline(aes(yintercept = true_MSS), linetype = "dashed", color = "grey") +
  geom_line() +
  facet_grid(true_MSS_text~true_TAU_text, scales = "free_y") +
  theme(legend.text = element_text(size = 6)) +
  ylab("Estimated MSS") +
  xlab("Cheating distance (m)")

## -----------------------------------------------------------------------------
ggplot(
  sim_df,
  aes(x = cheating_distance, y = TAU, color = model)
) +
  theme_minimal(8) +
  geom_hline(aes(yintercept = true_TAU), linetype = "dashed", color = "grey") +
  geom_line() +
  facet_grid(true_MSS_text~true_TAU_text, scales = "free") +
  theme(legend.text = element_text(size = 6)) +
  ylab("Estimated TAU") +
  xlab("Cheating distance (m)")

## -----------------------------------------------------------------------------
ggplot(
  sim_df,
  aes(x = cheating_distance, y = MAC, color = model)
) +
  theme_minimal(8) +
  geom_hline(aes(yintercept = true_MAC), linetype = "dashed", color = "grey") +
  geom_line() +
  facet_grid(true_MSS_text~true_TAU_text, scales = "free") +
  theme(legend.text = element_text(size = 6)) +
  ylab("Estimated MAC") +
  xlab("Cheating distance (m)")

## -----------------------------------------------------------------------------
ggplot(
  sim_df,
  aes(x = cheating_distance, y = PMAX, color = model)
) +
  theme_minimal(8) +
  geom_hline(aes(yintercept = true_PMAX), linetype = "dashed", color = "grey") +
  geom_line() +
  facet_grid(true_MSS_text~true_TAU_text, scales = "free") +
  theme(legend.text = element_text(size = 6)) +
  ylab("Estimated PMAX") +
  xlab("Cheating distance (m)")

