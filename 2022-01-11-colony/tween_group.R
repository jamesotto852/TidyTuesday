# Function for linear interpolation between times
# Use on nested df, nested by plot's groups

# k frames of interpolation
# p additional frames on observed times
tween_group <- function(df, k, p) {
  df <- arrange(df, time)
  n <- nrow(df)
  
  step_size <- (df$time[2] - df$time[1]) / k
  step_vec <- seq(0, .25, by = step_size)
  step_vec <- c(step_vec, rep(.25, p-1))
  time_new <- rep(df$time, each = (k + p)) + rep(step_vec, times = n)
  
  val_new <- rep(NA, n * (k + p))
  
  vals <- rep(df$colony_lost_pct[-1], each = p + 1)
  vals <- c(df$colony_lost_pct[1], vals)
  
  val_new[time_new %in% df$time] <- vals
  
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
      mutate(colony_lost_pct = zoo::na.approx(colony_lost_pct)) |>
      list()
  }
  
  df
}
