#' Identify work streaks from Git history
#' 
#' @param dir path to Git repo
#' @param threshold number of minutes. Commits further apart than `threshold` belong to different streaks
#' 
#' @details For every commit the function estimates the "commit time" which is
#'   the amount of time spent working towards that commit. Current approach is rather naive:
#'   1. Break the data into streaks using `threshold`
#'   2. For every streak:
#'       - All but first commit get commit time equal to the time difference from the previous commit.
#'       - For the first commit the commit time is the mean of all commits in the streak.
#'   3. For streaks of length one the commit time is the mean of all commit times for the repo.
#' 
#' @return Tibble with columns:
#' - `streak` - Streak ID
#' - `from` - Date-time, start of the work streak
#' - `to` - Date-time, end of the work streak
#' 
#' @export
git_timesheet <- function(dir=".", threshold=75) {
  dane <- git_log(dir, format_log="%H\t%ai", delim = "\t", col_names = c("md5", "date")) %>%
    mutate(
      date = lubridate::ymd_hms(date)
    ) %>%
    arrange(date) %>%
    mutate(
      d = as.numeric(date - lag(date)) / 60, # minutes
      t60m = replace_na(d >= threshold, TRUE),
      streak = cumsum(replace(t60m, 1, TRUE))
    )

  streak_freq <- dane %>%
    count(streak)
  
  # Average commit "duration" within a streak
  commit_avg <- dane %>%
    anti_join(
      filter(streak_freq, n == 1),
      by = "streak"
    ) %>%
    group_by(streak) %>%
    summarise(
      mu = mean(d[-1], na.rm=TRUE)
    )
  
  commit_times <- dane %>%
    left_join(
      mutate(commit_avg, t60m = TRUE),
      by = c("streak", "t60m")
    ) %>%
    mutate(
      commit_time = case_when(
        !t60m ~ d,
        t60m ~ mu
      ),
      commit_time = replace_na(commit_time, mean(commit_time, na.rm=TRUE))
    )
  
  commit_times %>%
    group_by(streak) %>%
    summarise(
      from = min(date) - lubridate::dseconds(commit_time[1] * 60),
      to = max(date)
    )
}


