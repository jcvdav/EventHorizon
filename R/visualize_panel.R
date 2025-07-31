#' Visualize a panel
#'
#' @param data An object of class data.frame or tibble containing the panel data
#' @param id A column name containing the unique identifier of the observational unit in the panel (the i dimension)
#' @param time A column name containing the time variable in the panel (i.e. the t dimension)
#' @param treatment A column name containing the name of the dummy variable indicating whether unit i is treated at time t
#' @param prepped A boolean specifying if the panel has already been prepared through this package (i.e. the treatment_ids have been created and modified). If so, the points will be colored based on treatment id.
#'
#' @returns a ggplot object
#' @export
#'
#' @examples
#' # Load packages
#' library(tidyr)
#' library(dplyr)
#' library(EventHorizon)
#'
# Load a panel
#' panel <- simulate_panel(n_units = 10, n_periods = 10)
#' visualize_panel(panel)
#'
#' event_horizon_panel <- panel |>
#' mutate(treatment_id = treatment_ids(id = id,
#'                                     time = time,
#'                                     treatment = treatment,
#'                                     window = 3),
#'        relative_time = calculate_relative_time(id = id,
#'                                                time = time,
#'                                                treatment_id = treatment_id,
#'                                                window = 3),
#'        treatment_id = propagate_treatment_id(id = id,
#'                                              time = time,
#'                                              treatment_id = treatment_id,
#'                                              window = 3)) |>
#' drop_na(treatment_id)
#'
#' visualize_panel(event_horizon_panel)
#' visualize_panel(event_horizon_panel, prepped = TRUE)
#
visualize_panel <- function(data, id = id, time = time, treatment = treatment, prepped = F) {
  p <- ggplot2::ggplot(data = data,
         mapping = ggplot2::aes(x = {{time}}, y = {{id}}, group = {{id}})) +
    ggplot2::geom_line(linewidth = 0.1) +
    ggplot2::scale_size_manual(values = c(0.1, 1)) +
    ggplot2::labs(x = "Time",
         y = "Unit id",
         size = "Treated")

  if(prepped){
    p <- p +
      ggplot2::geom_point(ggplot2::aes(size = treatment == 1, color = treatment_id)) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::guides(color="none")
  } else {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(size = treatment == 1))
  }



  return(p)
}
