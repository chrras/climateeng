psychrometric_chart <- function(temp.db = NULL, hum.ratio = NULL,
                                temp.min = -15, temp.max = 30,
                                humidity.max = 0.020, alt = 0,
                                mollier = FALSE, alpha = 0.25,
                                disable.warnings = TRUE) {
  #' Psychrometric chart
  #'
  #' Plot psychrometric chart with or with data.
  #' @param temp.db Vector of dry-bulb temperatures [degC]. Defaults to NULL.
  #' @param hum.ratio Vector of humidity ratios [kg/kg]. Defaults to NULL.
  #' @param temp.min Minimum value of temperature axis [degC]. Defaluts to
  #'  -15 degC.
  #' @param temp.max Maximum value of temperature axis [degC]. Defaluts to
  #'  30 degC.
  #' @param humidity.max Maximum value of humidity axis [kg/kg]. Defaults to
  #'  0.020 kg/kg.
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @param mollier Boolean operator. Plot Mollier chart (tx-chart) or tx-chart.
  #'  Defaults to FALSE.
  #' @param alpha Transparancy of the data points. Defaults to 0.25.
  #' @param disable.warnings Some ignorable warnings appear when plotting the
  #'  chart. To see these and eventually others disable.warnings should be
  #'  FALSE. Defaults to TRUE.
  #' @return Plots the psychrometric chart. If the error
  #' "TEXT_SHOW_BACKTRACE environmental variable.
  #' Error in grid.Call(L_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
  #' polygon edge not found"
  #' shows up. Try run psychrometric_chart() again.
  #' @export
  #' @examples
  #' psychrometric_chart()
  #'
  #' temp.db <- c(20, 23, 18, 20, 10, 27)
  #' hum.ratio <- c(0.005, 0.009, 0.004, 0.011, 0.004, 0.01)
  #' psychrometric_chart(temp.db, hum.ratio, alpha = 1)
  #'
  #' psychrometric_chart(temp.db, hum.ratio, temp.min=5, mollier = TRUE,
  #'  alpha = 1)
  #' @author Christoffer Rasmussen

  # Check for package -------------------------------------------------------

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Please install it.",
         call. = FALSE)
  } else if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop("cowplot is needed for this function to work. Please install it.",
         call. = FALSE)
  } else if (!requireNamespace("weights", quietly = TRUE)) {
    stop("weights is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Plot parameters ---------------------------------------------------------

  # Constants
  FONT.SCALE <- 2.834646  # Translate annotation font size to theme font size.
  LINE.MULTIPLIER <- 3  # Line size of boarders compares to rest of lines.
  N <- 2000  # Number og line segments for stat_function lines.

  # Round temperature and humidity limits
  temp.min <- my_round(temp.min, 5, "floor")
  temp.max <- my_round(temp.max, 5, "ceiling")
  humidity.max <- my_round(humidity.max, 0.005, "ceiling")

  # Temperature which RH annotation should be centered between
  temp.1 <- 20
  temp.2 <- 25

  # Aspect ratio
  if (mollier == F) {
    asp <- (humidity.max / 0.005) / ((temp.max - temp.min) / 5)
    deg <- 0
    k <- 1
  } else {
    asp <- ((temp.max - temp.min) / 5) / (humidity.max / 0.005)
    deg <- 90
    k <- -1
  }
  # Axis to filp. Depends on chart type.
  axis.to.flip <- "y"

  # Plot parameters
  theme <- theme_parms()


  # Create base plot --------------------------------------------------------


  p <- ggplot2::ggplot(data.frame(x = c(temp.min, temp.max)), ggplot2::aes(x)) +

    # Add theme
    theme_climateeng_psy(asp) +

    # Axis breaks
    ggplot2::scale_x_continuous(
      breaks = seq(temp.min, temp.max, 5)) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, humidity.max, 0.005),
      labels = weights::rd(seq(0.0, humidity.max, 0.005), digits = 3, add = F)) +

    # Axis titles
    ggplot2::ylab(expression("Humidity ratio ("*kg[m]*"/"*kg[da]*")")) +
    ggplot2::xlab(expression("Dry-bulb temperature ("*degree*C*")"))


  # Trim chart --------------------------------------------------------------


  x_add <- (temp.max - temp.min) * 0.003
  y_add <- (humidity.max) * 0.003

  p <- p + ggplot2::coord_cartesian(
             xlim = c(temp.min - x_add, temp.max + x_add),
             ylim = c(-y_add, humidity.max + y_add),
             expand = F)


  # Converts tx-chart to xt-chart -------------------------------------------


  if (mollier == T) {
    p <- p + ggplot2::coord_flip(
               ylim = c(-y_add, humidity.max + y_add),
               xlim = c(temp.min - x_add, temp.max + x_add),
               expand = F)
    axis.to.flip <- "x"
  }


  # Relative humidity lines -------------------------------------------------


  for (i in seq(0, 100, 10)) {
    if (i %in% c(0, 100)) {
      # Border curve
      p <- p + ggplot2::stat_function(
                 fun = hum_ratio_rel_hum,
                 args = list(alt = alt,
                             rel.hum = i,
                             hum.ratio.max = humidity.max),
                 n = N,
                 size = theme$size.line * LINE.MULTIPLIER,
                 geom = "line",
                 col = theme$color.grid.major)
    } else {
      # Other RH curves
      p <- p + ggplot2::stat_function(
                 fun = hum_ratio_rel_hum,
                 args = list(alt = alt,
                             rel.hum = i,
                             hum.ratio.max = humidity.max),
                 n = N,
                 size = theme$size.line,
                 geom = "line",
                 col = theme$color.grid.major)
    }
  }


  # Absolute humidity lines -------------------------------------------------


  for (i in seq(0.005, humidity.max, 0.005)) {
    if (i == humidity.max) {
      # Border curve
      p <- p + ggplot2::geom_segment(
                 x = temp.max,
                 xend = dewpoint(hum.ratio = i, alt = alt),
                 y = i,
                 yend = i,
                 col = theme$color.grid.major,
                 size = theme$size.line * LINE.MULTIPLIER)
    } else {
      # Other RH curves
      p <- p + ggplot2::geom_segment(
                 x = temp.max,
                 xend = dewpoint(hum.ratio = i, alt = alt),
                 y = i,
                 yend = i,
                 size = theme$size.line,
                 col = theme$color.grid.major)
    }
  }


  # Dry bulb temperature lines ----------------------------------------------


  for (i in seq(temp.min, temp.max, 5)) {
    if (i %in% c(temp.min, temp.max)) {
      # Border curve
      p <- p + ggplot2::geom_segment(
                x = i,
                xend = i,
                y = 0,
                yend = min(sat_hum_ratio(i, alt), humidity.max),
                col = theme$color.grid.major,
                size = theme$size.line * LINE.MULTIPLIER)
    } else {
      # Other RH curves
      p <- p + ggplot2::geom_segment(
                x = i,
                xend = i,
                y = 0,
                yend = min(sat_hum_ratio(i, alt), humidity.max),
                size = theme$size.line,
                col = theme$color.grid.major)
    }
  }


  # Wet bulb temperature lines ----------------------------------------------


  # temp_wb_min = temp.min
  # while (wetbulb_intersect(temp_wb_min, alt) > temp.min) {
  #   temp_wb_min <- temp_wb_min - 5
  # }
  #
  # for (i in seq(temp_wb_min, temp.max - 5, 5)) {
  #   p <- p + geom_segment(x = i,
  #                         xend = wetbulb_intersect(i, alt),
  #                         y = sat_hum_ratio(i, alt),
  #                         yend = 0,
  #                         size = theme$size.line,
  #                         lty = "dashed")
  # }


  # Enthalpy lines ----------------------------------------------------------


  start <- my_round(enthalpy(temp.min, 0), 10, "ceiling")
  end <- my_round(enthalpy(temp.max, humidity.max), 10, "floor")

  # Add lines
  for (i in seq(start, end, 10)) {
    p <- p + ggplot2::stat_function(
               fun = hum_ratio_enthalpy,
               args = list(enthalpy = i,
                           alt = alt),
               n = N,
               size = theme$size.line,
               geom = "line",
               col = theme$color.grid.major)
  }


  # Data points -------------------------------------------------------------


  # Add data to plot if present
  if (!is.null(temp.db) | !is.null(hum.ratio)) {

    # Create data frame
    df.points <- data.frame(
      temp = temp.db,
      hum = hum.ratio)

    # Add data points
    p <- p +
      ggplot2::scale_color_gradient(low = "#3F5151", high = "#9B110E") +
      ggplot2::geom_point(
        data = df.points,
        ggplot2::aes(x = temp, y = hum),
        size = 1.25,
        alpha = alpha)
  }


# Annotation of relative humidity lines -----------------------------------


  temp <- mean(c(temp.1, temp.2))

  if (mollier == F) {
    seq <- seq(0, 80, 20)
  } else if (mollier == T) {
    seq <- seq(20, 100, 20)
  }


  for (i in seq) {

    # Define label
    if (i == 0) {
      label <- "Relative humidity: 0 %\n"
    } else if (i == 100) {
      label <- "Relative humidity: 100 %\n"
    } else {
      label <- paste0(i, " %\n")
    }

    # Add label
    p <- p +
      ggplot2::annotate("text",
        x = temp,
        y = hum_ratio(i, temp),
        size = theme$text.size.axis / FONT.SCALE,
        label =  label,
        angle = deg + k * slope_rel_hum(temp.1, temp.2, i),
        col = theme$color.axis.text)
  }


# Annotation of enthalpy lines --------------------------------------------


  start <- my_round(enthalpy(temp.min, 0), 10, "ceiling")
  end <- my_round(enthalpy(temp.max, humidity.max), 10, "floor")

  for (i in seq(start, end, 10)) {

    # Define label
    if (enthalpy_intersect(i, alt) < temp.min + 2 |
        enthalpy_intersect(i, alt) > dewpoint(humidity.max, alt) - 0.5) {
      label <- ""
    } else if (mollier == F) {
      if (i == 40) {
        label <- paste0("Enthalpy: ", i, " kJ/kg\n\n")
      } else {
        label <- paste0(i, "\n\n")
      }
    } else if (mollier == T) {
      if (i == 40) {
        label <- paste0("\n\nEnthalpy: ", i, " kJ/kg")
      } else {
        label <- paste0("\n\n", i)
      }
    }

    intersect <- enthalpy_intersect(i, alt)

    # Add label
    p <- p +
      ggplot2::annotate("text",
        x = intersect,
        y = sat_hum_ratio(intersect, alt),
        size = theme$text.size.axis / FONT.SCALE,
        label =  label,
        angle = deg + k * slope_rel_hum(intersect - 0.5,
                                        intersect + 0.5,
                                        100),
        col = theme$color.axis.text)
  }

  # Plot --------------------------------------------------------------------


  # Plot p with fliped axis
  if (disable.warnings == TRUE) {
    suppressWarnings(
      cowplot::ggdraw(cowplot::switch_axis_position(p, axis = axis.to.flip)))
  } else {
    cowplot::ggdraw(cowplot::switch_axis_position(p, axis = axis.to.flip))
  }
}
