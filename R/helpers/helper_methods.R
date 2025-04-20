# R/helpers/helper_methods.R

#' @import ggplot2

library(ggplot2)


html_bullets <- function(chr, na.rm = TRUE)
{
  if (na.rm) {
    chr <- chr[!is.na(chr)]
  }
  else {
    chr[is.na(chr)] <- ""
  }
  paste0("<ul>", paste0(paste0("<li>", chr, "</li>"), collapse = ""),
         "</ul>")
}

calculate_dots <- function(body_weight, weight_lifted, is_female) {
  # Define coefficients for male and female
  male_coeff <- c(-307.75076, 24.0900756, -0.1918759221, 0.0007391293, -0.000001093)
  female_coeff <- c(-57.96288, 13.6175032, -0.1126655495, 0.0005158568, -0.0000010706)

  # Select coefficients based on gender
  coeff <- if (is_female) female_coeff else male_coeff
  max_bw <- if (is_female) 150 else 210

  # Clamp body weight within the valid range
  bw <- max(min(body_weight, max_bw), 40)

  # Compute denominator
  denominator <- coeff[1]
  for (i in 2:length(coeff)) {
    denominator <- denominator + coeff[i] * (bw^(i - 1))
  }

  # Compute DOTS score
  score <- (500 / denominator) * weight_lifted

  # Return rounded score
  return(round(score, 2))
}


create_lift_plot <- function(data, lift_name) {
  # Check if data exists and has valid values
  if (is.null(data) || nrow(data) == 0 || all(is.na(data[[lift_name]]))) {
    # Create an empty plot with a message
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Enter competition data to see your progress",
                 size = 5) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(20, 20, 20, 20)
        ) +
        xlim(0, 1) +
        ylim(0, 1)
    )
  }


  # Filter out NA values
  df_filtered <- data[!is.na(data[[lift_name]]), c("Comp","Date", lift_name)]
  names(df_filtered) <- c("Comp", "Date","Weight")

  # If after filtering we have less than 2 points, show message
  if (nrow(df_filtered) < 2) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Enter at least two competition results\nto see progress",
                 size = 5) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(20, 20, 20, 20)
        ) +
        xlim(0, 1) +
        ylim(0, 1)
    )
  }

  color_map <- c(
    "Best3SquatKg" = "#FC8D62",
    "Best3BenchKg" = "#8DA0CB",
    "Best3DeadliftKg" = "#66C2A5" ,
    "BodyweightKg" = "grey50" ,
    "Dots" = "purple",
    "TotalKg" = "black"
  )

  annot_map <- c(
    "Best3SquatKg" = "kg",
    "Best3BenchKg" = "kg",
    "Best3DeadliftKg" = "kg",
    "BodyweightKg" = "kg",
    "Dots" = "DOTS Score",
    "TotalKg" = "Kg"
  )


  df_filtered$Date <- as.Date(df_filtered$Date)

  # Ensure df_filtered has columns "Date" and "Comp"
  df_filtered <- df_filtered %>%
    arrange(Date) %>%
    mutate(Date = factor(Date, levels = unique(Date)))




  ggplot(df_filtered, aes(x = Date, y = Weight, group = 1)) +
    geom_rect(
      aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
      fill = "grey97",
      alpha = 0.5
    ) +
    geom_hline(
      yintercept = seq(0, max(df_filtered$Weight, na.rm = TRUE), length.out = 5),
      color = "white",
      linewidth = 0.8
    ) +
    geom_line(
      linewidth = 1.2,
      color = color_map[lift_name],
      alpha = 0.8
    ) +
    geom_point(
      size = 4,
      color = color_map[lift_name],
      fill = "white",
      shape = 21,
      stroke = 2
    ) +
    labs(
      x = "Competition",
      y = annot_map[lift_name]
      #title = paste("Lift Progress"),
      #subtitle = "Track your strength gains over time"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = 12,
        color = "grey30",
        margin = margin(b = 20)
      ),
      axis.title = element_text(
        size = 12,
        color = "grey30"
      ),
      axis.text = element_text(
        size = 10,
        color = "grey30"
      ),
      axis.text.x = element_text(
        angle = 60,
        hjust = 1,
        vjust = 1,
        size = 8
      ),
      panel.grid.major = element_line(
        color = "grey90",
        linewidth = 0.5
      ),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    scale_x_discrete(
      labels = df_filtered$Comp
    ) +
    scale_y_continuous(
      #labels = scales::label_number(suffix = annot_map[lift_name]),
      limits = c(min(df_filtered$Weight, na.rm = TRUE) * 0.9, max(df_filtered$Weight, na.rm = TRUE) * 1.1),
      expand = expansion(mult = c(0.1, 0.1))
    )

}







