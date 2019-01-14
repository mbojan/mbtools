library(tidyverse)
library(tidytext)

op <- options(
  tibble.print_min = 30
)

dane <- tibble(
  text = readLines("~/Desktop/dhgwdeg0.1gwesp1-10k-cd.out"),
) %>%
  rowid_to_column("line")


readcoef <- function(d, ...) {
  con <- textConnection(d)
  read.table(con, colClasses = "character", as.is=TRUE, header=TRUE)
}

d <- dane %>%
  mutate(
    has_iteration = grepl("Iteration [0-9]+ of at most [0-9]+", text),
    has_starting = grepl("Starting unconstrained MCMC", text),
    iteration = cumsum(has_iteration),
    x = cumprod( 1 - 2*(has_iteration | has_starting) )
  ) %>%
  filter(x == -1 & !has_iteration) %>%
  group_by(iteration) %>%
  mutate(
    block = cumsum(i = seq(1, n()) %% 2 != 0)
  ) %>%
  group_by(block) %>%
  mutate(
    wblock = seq(1, n())
  ) %>%
  group_by(iteration, wblock) %>%
  summarise(
    z = paste(text, collapse = "   ")
  ) %>%
  group_by(iteration) %>%
  summarise(
    z = map(paste(z, collapse="\n"), readcoef)
  ) %>%
  unnest() %>%
  gather(coef, value, -iteration) %>%
  mutate(value = as.numeric(value))



ggplot(d, aes(x=iteration, y=value)) +
  geom_point() +
  facet_wrap(~ coef, scales = "free")
