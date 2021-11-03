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
    dplyr::mutate(
      date = lubridate::ymd_hms(date)
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      d = as.numeric(date - dplyr::lag(date)) / 60, # minutes
      t60m = tidyr::replace_na(d >= threshold, TRUE),
      streak = cumsum(replace(t60m, 1, TRUE))
    )

  streak_freq <- dane %>%
    dplyr::count(streak)
  
  # Average commit "duration" within a streak
  commit_avg <- dane %>%
    dplyr::anti_join(
      dplyr::filter(streak_freq, n == 1),
      by = "streak"
    ) %>%
    dplyr::group_by(streak) %>%
    dplyr::summarise(
      mu = mean(d[-1], na.rm=TRUE)
    )
  
  commit_times <- dane %>%
    dplyr::left_join(
      dplyr::mutate(commit_avg, t60m = TRUE),
      by = c("streak", "t60m")
    ) %>%
    dplyr::mutate(
      commit_time = dplyr::case_when(
        !t60m ~ d,
        t60m ~ mu
      ),
      commit_time = tidyr::replace_na(commit_time, mean(commit_time, na.rm=TRUE))
    )
  
  commit_times %>%
    dplyr::group_by(streak) %>%
    dplyr::summarise(
      from = min(date) - lubridate::dseconds(commit_time[1] * 60),
      to = max(date)
    )
}

globalVariables(c("d", "t60m", "streak", "n", "commit_time"))

# Export to Toggl 
# library(lubridate)
# 
# z %>%
#   mutate(
#     dur = lubridate::as.period(lubridate::as.duration(lubridate::interval(from,to)))
#   ) %>%
#   transmute(
#     Email = "michal2992@gmail.com",
#     Project = "Package `statustraj`",
#     `Start date` = as.Date(from),
#     `Start time` = sprintf('%02d:%02d:%02d', round(hour(from)), round(minute(from)), round(second(from))),
#     Duration = sprintf('%02d:%02d:%02d', round(hour(dur)), round(minute(dur)), round(second(dur)))
#   ) %>%
#   write_csv(file = "~/Desktop/statustraj.csv")
# 
# library(lubridate)
# td <- 
#   