# Function for linear interpolation between times
# Use on nested df, nested by plot's groups

# Written to work specifically with colony data,
# could be tweaked with to allow for more general use via tidyselect

# k frames of interpolation
# p additional frames on observed times
tween_group <- function(df, k, p) {
  df <- arrange(df, time)
  n <- nrow(df)
  
  # Additional time steps between observed times 
  step_size <- (df$time[2] - df$time[1]) / k
  step_vec <- seq(0, .25, by = step_size)
  step_vec <- c(step_vec, rep(.25, p-1))
  time_new <- rep(df$time, each = (k + p)) + rep(step_vec, times = n)
  
  # Initialize vector values for all (obs. + add.) time steps 
  val_new <- rep(NA, n * (k + p))
  
  # Set values for observed time steps
  vals <- rep(df$colony_lost_pct[-1], each = p + 1)
  vals <- c(df$colony_lost_pct[1], vals)
  val_new[time_new %in% df$time] <- vals
  
  # Adjust times s.t. they are all unique 
  # Want to repeat observed time step (non-interpolated frame) in animation,
  # but time still needs to be unique
  step_vec <- seq(0, .25, by = step_size)
  step_vec <- c(step_vec, rep(.25, p-1) + seq(0, step_size / 2, length.out = p-1))
  time_new <- rep(df$time, each = (k + p)) + rep(step_vec, times = n)
  
  
  # Perform interpolation
  # If all values are NA, return NA
  # Need to return inside a list for pmap
  if (all(is.na(val_new))) {
    df <- tibble(
        time = time_new,
        colony_lost_pct = val_new
      ) |>
      slice(1:((n-1) * (k+p))) |> 
      list()
  } else {
    df <- tibble(
        time = time_new,
        colony_lost_pct = val_new
      ) |>
      slice(1:((n-1) * (k+p))) |>
      mutate(colony_lost_pct = na.approx(colony_lost_pct)) |>
      list()
  }
  
  df
}
