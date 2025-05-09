library(tidyr)
library(ggplot2)
library(tsibble)
library(dplyr)
library(purrr)
library(kableExtra)
library(knitr)
library(webshot2)


sim1 <- readRDS("D:/Github/Recon_Honours_Thesis/research_proposal/sim_results/S36-6-2-1_T100_M500_par.rds")
sim2 <- readRDS("D:/Github/Recon_Honours_Thesis/research_proposal/sim_results/S36-6-2-1_T300_M500_par.rds")

transfrom_sim_MSE <- function(MSE, to_ts = T) {
  df <- imap_dfr(MSE, function(mat, model_name) {
    as_tibble(mat) %>%
      mutate(h = row_number()) %>%
      pivot_longer(cols = -h, names_to = "series", values_to = "MSE") %>%
      mutate(
        .model = model_name,
      ) %>%
      select(.model, series, h, MSE)
  })
  if (to_ts) {
    return(
      as_tsibble(key = c(.model, series), index = h)
    )
  }
  return(df)
}


MSE1 <- transfrom_sim_MSE(sim1$MSE, F)
MSE2 <- transfrom_sim_MSE(sim2$MSE, F)

hier_level <- MSE1$series |>
  substring(1, 1) |>
  unique() |> sort(decreasing = TRUE)

MSE_full <- bind_rows(MSE1, MSE2, .id = "id") |>
  mutate(
    level = match(substring(series,1,1), hier_level)
  )

MSE_full |>
  group_by(id, series, h) |>
  mutate(
    base_MSE = MSE[.model == "base"],
    pct_change = ifelse(.model == "base",
                        base_MSE,
                        (MSE - base_MSE) / base_MSE * 100)
  ) |>
  ungroup()

group_defs <- tibble(
  h_group = c("h=1",  "h=1-8",   "h=1-16"),
  h_max   = c(1,      8,         16)
)

MSE_tidy <- MSE_full %>%
  # filter(h<=16) %>%
  crossing(group_defs) %>%    # gives every row all 3 candidate groups
  filter(h <= h_max) %>%      # keep only the overlapping ones
  # filter(h>15)
  group_by(id, .model, level, h_group) %>%
  summarise(mean_MSE = mean(MSE), .groups = "drop")

MSE_tidy <- MSE_tidy %>%
  group_by(level, id, h_group) %>%
  mutate(
    base_MSE = mean_MSE[.model == "base"],
    change = if_else(.model == "base",
                     formatC(mean_MSE, digits = 1, format = "f"),
                     formatC(100 * (mean_MSE - base_MSE) / base_MSE, digits = 1, format = "f"))
  ) %>%
  ungroup()

MSE_tidy <- MSE_tidy %>%
  pivot_wider(
    id_cols = c(level, .model),
    names_from = c(id, h_group),
    values_from = change
  ) %>%
  mutate(
    .model = recode(.model,
                    "mint_sample" = "MinT(Sample)",
                    "mint_shr"    = "MinT(Shrink)",
                    "mint_n"      = "MinT(N)",
                    "base"        = "Base"
    ),
    .model = factor(.model, levels = c("MinT(Sample)", "MinT(Shrink)", "MinT(N)", "Base"))
  ) %>%
  mutate(
    level = recode(level,
                   `1` = "Top level",
                   `2` = "Level 1",
                   `3` = "Level 2",
                   `4` = "Bottom level"
    ),
    level = factor(level, levels = c("Top level", "Level 1", "Level 2", "Bottom level"))
  ) %>%
  arrange(level, .model)

row_levels <- MSE_tidy$level
MSE_display <- MSE_tidy %>% select(-level)


my_kable <- MSE_display %>%
  mutate(across(where(is.numeric), ~formatC(.x, digits = 1, format = "f"))) %>%
  kable(
    booktabs = TRUE,
    longtable = TRUE,
    align = c("l", rep("r", 6)),  # now only 7 cols
    caption = "Mean Squared Errors by Model, Level, and Forecast Horizon",
    col.names = c(" ", rep(c("h=1", "h=1-8", "h=1-16"), 2)),
    escape = FALSE
  ) %>%
  add_header_above(c(" " = 1, "T = 100" = 3, "T = 300" = 3)) %>%
  {
    tbl <- .
    start <- 1
    for (lvl in unique(row_levels)) {
      end <- start + sum(row_levels == lvl) - 1
      tbl <- tbl %>% pack_rows(lvl, start, end)
      start <- end + 1
    }
    tbl
  }

my_kable

save_kable(
  my_kable,
  file = "D:/Github/Recon_Honours_Thesis/research_proposal/my_kable.png",
  keep_tex = TRUE
)
