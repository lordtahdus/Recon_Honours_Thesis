#' Plot radar chart comparing methods across multiple metrics
#' (FOR THESIS USE ONLY)
#' 
#' @param tourism_uniscores Data frame of univariate scores
#' @param tourism_energy Data frame of energy scores
#' 
#' @returns radar chart for tourism data
plot_radar <- function(tourism_uniscores, tourism_energy) {

  radar_df <- tourism_uniscores |> 
    filter(.model %in% c("base", "mint_shr", "mint_n", "mint_shr_pc_K1", "mint_n_pc_K1")) |>
    # average across series per iteration & compute % improv per iter
    group_by(model_label, rule, iter) |>
    summarise(
      score = mean(score),
      base_score = mean(base_score),
      pct_change = (base_score - score) / base_score * 100
    ) |>
    ungroup() |>
    # average across iter
    group_by(model_label, rule) |>
    summarise(
      pct_change_avg = mean(pct_change)
    ) |>
    ungroup() |>
    # filter(model_label != "Base") |>
    pivot_wider(names_from = rule, values_from = pct_change_avg) |>
    column_to_rownames("model_label")

  radar_df <- cbind(
    radar_df,
    Energy = tourism_energy |> 
      filter(.model %in% c("base", "mint_shr", "mint_n", "mint_shr_pc_K1", "mint_n_pc_K1")) |> 
      group_by(model_label) |> 
      summarise(
        mean_score = mean(score),
        pct_change_avg = mean(pct_change_per_series)
      ) |> 
      pull(pct_change_avg)
  )

  # rename the cols (scores)
  colnames(radar_df) <- c("CRPS", "Winkler\n80%", "Winkler\n95%", "Energy")


  {# --- 2) Add required max/min rows for fmsb::radarchart ---
  maxs <- apply(radar_df, 2, max, na.rm = TRUE)
  mins <- apply(radar_df, 2, min, na.rm = TRUE)
  mins <- pmin(mins, 0) # ensure min is at most 0

  # pad the scale a bit so polygons don’t touch edges
  pad  <- 0.01 * (maxs - mins)
  scale_top <- maxs + pad
  scale_bot <- mins - pad

  plot_df <- rbind(scale_top, scale_bot, radar_df)

  # --- 3) Colors (one per method, in row order) ---
  cols <- c("#7b7b7bff","#1B9E77", "#6A51A3", "#66A61E", "#E6AB02")[seq_len(nrow(radar_df))]

  # --- 4) Draw radar chart ---
  radarchart(
    plot_df,
    axistype = 1,
    pcol  = cols,
    pfcol = scales::alpha(cols, 0.20),
    plwd  = 2,
    plty  = 1,
    cglcol = "grey80", cglty = 1, cglwd = 0.8,
    axislabcol = "grey30",
    vlcex = 0.9
  )

  legend("topright", legend = rownames(radar_df),
        col = cols, lty = 1, lwd = 2, bty = "n", cex = 0.9)
  }
  title(" ")

  recordPlot()
}
