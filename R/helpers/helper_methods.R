# R/helpers/helper_methods.R

#' @import ggplot2

library(ggplot2)

calculate_pct <- function(openpl,column_name,best_result) {



  df <- openpl


  # Don't allow negative values
  df <- df[df[[column_name]] > 0,]

  # Calculate the percentile
  percentile <- ecdf(df[[column_name]])(best_result) * 100

  # Ensure percentile is within bounds
  percentile <- max(0, min(percentile, 100))

  # Create a data frame for plotting
  data <- data.frame(x = c(0, percentile), y = 1)

  return(data)

}


get_ipf_age_class <- function(age) {
  sapply(age, function(a) {
    if (is.na(a)) {
      return(NA)
    } else if (a >= 14 && a <= 18) {
      return("subjunior")  # eligible for both, but prioritize open unless specifically sub-junior
    } else if (a >= 19 && a <= 23) {
      return("junior")
    } else if (a >= 40) {
      return("masters")
    } else if (a > 23 && a < 40) {
      return("open")
    } else {
      return(NA)  # under 14 or invalid
    }
  })
}

create_progress_bar <- function(percentages, column_name) {

  color_map <- c(
    "Best3SquatKg" = "#FC8D62",
    "Best3BenchKg" = "#8DA0CB",
    "Best3DeadliftKg" = "#66C2A5" ,
    "BodyweightKg" = "grey50" ,
    "Dots" = "purple",
    "TotalKg" = "black"
  )

  annot_map <- c(
    "Best3SquatKg" = "squat",
    "Best3BenchKg" = "bench",
    "Best3DeadliftKg" = "deadlift" ,
    "BodyweightKg" = "body weight" ,
    "Dots" = "DOTS score",
    "TotalKg" = "total"
  )


  data <- percentages[[column_name]]

  percentile <- data$x[2]

  # Plot using ggplot2
  progress_bar <- ggplot() +
    # Background bar
    geom_rect(aes(xmin = 0, xmax = 100, ymin = 0.4, ymax = 1.6),
              fill = "lightgray", color = "black", alpha = 0.5, linewidth = 1) +
    # Filled progress bar
    geom_rect(data = data, aes(xmin = 0, xmax = percentile, ymin = 0.4, ymax = 1.6),
              fill = color_map[column_name], color = "black", alpha = 0.9, linewidth = 1) +
    # Add percentage text
    annotate("text", x = 50, y = 1, label = paste0(round(percentile, 2), "%"),
             color = "white", size = 10, fontface = "bold") +
    # Add contextual information
    annotate("text", x = 50, y = 1.8, label = paste("Your",annot_map[column_name] ," is \n better than that of", round(percentile, 2), "% \n of other lifters in your category."),
             color = "black", size = 4, fontface = "bold") +

    theme_void() +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 2)) +
    theme(plot.margin = margin(10, 10, 10, 10))

  return(progress_bar)
}




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
        size = 7
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







