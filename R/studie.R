make_cb_totals <- function(data, ...) {
  data %>%
    group_by(vykaz_date, ..., source) %>%
    summarise(spending = sum(spending), .groups = "drop_last") |>
    ungroup() |>
    group_by(vykaz_date, ...)
}

make_cb_totals_perc <- function(data_summed, ...) {
  data_summed %>%
    group_by(vykaz_date, ..., .add = TRUE) %>%
    mutate(perc = spending/sum(spending)) %>%
    filter(source == "EU") |>
    ungroup()
}

palette_studie <- c(
  "ESIF: Běžné výdaje" = colorspace::darken("#56b9e4", 0),
  "mimo ESIF: Běžné výdaje" = colorspace::lighten("#56b9e4", .4),
  "ESIF: Kapitálové výdaje" = colorspace::darken("#d55e00", 0),
  "mimo ESIF: Kapitálové výdaje" = colorspace::lighten("#d55e00", .4),
  "ESIF" = "grey30",
  "mimo ESIF" = "grey50"
)

plot_esif <- function(data, perc = FALSE, by_trida = TRUE,
                        facet_var = NULL,
                        title = NULL, subtitle = NULL, caption = NULL,
                        breaks_y_limit = NA_real_, breaks_y_step = NULL,
                        legend_labels = names(palette_studie),
                        keep_legend = FALSE, divider = 1e9, ylabel_precision = 1) {

  is_multi <- !missing(facet_var)

  geom_fn <- if(perc) geom_col else geom_line

  data_plot <- data |>
    ungroup() |>
    mutate(source = recode(source, EU = "ESIF", vlastní = "mimo ESIF"),
           val = if(perc) perc else spending/divider,
           var = source) |>
    drop_na({{facet_var}})

  if(by_trida) data_plot <- data_plot |>
    mutate(var = paste0(source, ": ", trida) |>
             fct_relevel(!!!names(palette_studie)))

  p <- ggplot(data_plot, aes(vykaz_date, val)) +
    scale_x_date(labels = yr_lbls, breaks = yr_brks) +
    theme_studie(multiplot = is_multi) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)

  if(is_multi) {
    plt <- p + facet_wrap(facets = vars({{facet_var}}))
    } else {
    plt <- p
    }

  if(perc) {
    plt <- plt +
      geom_line(size = 2, aes(colour = var)) +
      geom_point(size = 4, colour = "white") +
      geom_point(size = 3, aes(colour = var)) +
      scale_y_continuous(expand = ptrr::flush_axis,
                         breaks = scales::breaks_pretty(n = 6),
                         limits = c(0, breaks_y_limit),
                         labels = ptrr::label_percent_cz(ylabel_precision)) +
      guides(colour = if(keep_legend) guide_legend(title = NULL) else "none") +
      scale_colour_manual(values = palette_studie)

  } else {
    plt <- plt +
      geom_col(aes(fill = var)) +
      scale_fill_manual(values = palette_studie, labels = legend_labels,
                        name = NULL) +
      scale_y_continuous(expand = ptrr::flush_axis,
                         limits = c(NA, breaks_y_limit),
                         # breaks = seq(from = 0, to = 2000, by = 500),
                         labels = ptrr::label_number_cz(accuracy = ylabel_precision))
  }
  return(plt)
}
