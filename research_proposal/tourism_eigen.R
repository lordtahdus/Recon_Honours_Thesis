library(tidyverse)
library(fpp3)

tourism_full <- tourism |>
  aggregate_key((State/Region), Trips = sum(Trips))
  # aggregate_key((State/Region) * Purpose, Trips = sum(Trips))

fit <- tourism_full |>
  filter(year(Quarter) <= 2015) |>
  model(base = ETS(Trips))

augment_fit <- fit %>%
  augment() %>%
  filter(.model == "base")

region <- augment_fit |>
  filter(!is_aggregated(Region)) |>
  as_tibble() |>
  select(Quarter, Region, .resid) |>
  pivot_wider(
    names_from = Region,
    values_from = .resid
  )

state <- augment_fit |>
  filter(is_aggregated(Region), !is_aggregated(State)) |>
  as_tibble() |>
  select(Quarter, State, .resid) |>
  pivot_wider(
    names_from = State,
    values_from = .resid
  )

total <- augment_fit |>
  filter(is_aggregated(State)) |>
  as_tibble() |>
  select(Quarter, State, .resid) |>
  pivot_wider(
    names_from = State,
    values_from = .resid
  )

full <- bind_cols(
  total,
  state |> select(-Quarter),
  region |> select(-Quarter)
)

full |>
  select(-Quarter) |>
  cov() |> princomp()

# prcomp(temp) |> screeplot(npcs=24, type="lines")

eigen(temp)$value[1:20] |>
  plot(
    ylab = "Log of Eigenvalue",
    xlab = " ",
    log = "y",
  ); abline(v=4.5, lty=2, col="purple")

eigen(temp)$value[1:20] |>
  plot(
    ylab = "Eigenvalue",
    xlab = "Component",
    ylim = c(0, 1e5),
    type = "b"
    # log = "y",
  ); abline(v=5, lty=2, lwd=1.5, col="purple")
