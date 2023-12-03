#' Plot 3DF Accelerometry Data
#'
#' @param x a tibble with columns time, X, Y, Z.
#' @param x_var the x-axis variable.
#' @returns A ggplot object, faceted by channel, showing the acceleration
#' or energy (in the Fourier domain) of the data.
#' @importFrom ggplot2 ggplot aes geom_line facet_grid
#' @importFrom tidyr pivot_longer
#' @export
plot_accel = function(x, x_var = "time") {
  x |>
    pivot_longer(-!!x_var, values_to = "Acceleration", names_to = "Channel") |>
#    ggplot(aes_string(x = x_var, y = "Acceleration")) +
    ggplot(aes(x = !!as.symbol(x_var), y = Acceleration)) +
    geom_line() +
      facet_grid(Channel ~ .)
}

#' Histogram of study phases
#'
#' Filters the `studies` data based on multiple queries (i.e., title keywords,
#' date range, sponsor type) and returns returns a histogram showing the distribution
#' of study phases
#' @param studies A tibble. The data table to be filtered
#' @param sponsortype A string. The sponsortype entered by the users
#' @param kw A string. The keyword string entered by the users
#' @param dates A list of two date values (start and end date)
#' @param color A string. The color string chosen by the users
#'
#' @return A plot with filtered entries
#' @importFrom ggplot2 ggplot aes geom_col scale_x_discrete ylab
#' @importFrom dplyr left_join filter count collect
#'
create_phase_hist_plot = function(studies, sponsors, kw, dates, color, sponsortype) {
  # sponsors = ctrialsgov::ctgov_query(sponsor_type = sponsortype)
  d = data_query_search(studies, kw, dates)|> head(1000)

  labels = c("Early Phase 1", "Phase 1", "Phase 1/Phase 2", "Phase 2",
             "Phase 2/Phase 3", "Phase 3", "Phase 4", "NA", "Not Applicable")

  by_sponsor = d |>
    collect() |>
    left_join(as_tibble(sponsors), by = "nct_id") |>
    count(phase)

  ggplot(by_sponsor, aes(x = phase, y = n)) +
    geom_col(fill = color) +
    scale_x_discrete(name = "Phase", limits = labels) +
    ylab("Count")
}


#' Histogram of study types
#'
#' Function that filters the studies data based on multiple queries,
#' including keyword matching, date range filter, sponsor type, and
#' histogram color choice, and returns returns a histogram showing the
#' distribution of different study types
#' @param studies A tibble. The data table to be filtered
#' @param sponsortype A string. The sponsortype entered by the users
#' @param kw A string. The keyword string entered by the users
#' @param dates A list of two date values (start and end date)
#' @param color A string. The color string chosen by the users
#'
#' @return A histogram plot
#' @importFrom ggplot2 ggplot geom_col scale_x_discrete labs
#' @importFrom dplyr select collect left_join filter group_by summarize
create_studytype_histogram = function(studies, sponsors, sponsortype, kw, dates, color){
  # sponsors = ctrialsgov::ctgov_query(sponsor_type = sponsortype)
  d = data_query_search(studies, kw, dates)|> head(1000)

  by_studytype = d |>
    collect() |>
    left_join(as_tibble(sponsors), by = "nct_id") |>
    count(study_type)

  ggplot(by_studytype, aes(x = study_type, y = n)) +
    geom_col(fill = color) +
    labs(x = "Study Type", y = "Count",title = paste("Study Type Distribution"))
}


#' Pie chart of study purposes
#'
#' Function that filters the studies data based on multiple queries,
#' including keyword matching, date range filter, and sponsor type, and returns
#' a pie chart showing distribution of different primary purposes.
#' @param studies A tibble. The data table to be filtered
#' @param sponsortype A string. The sponsor type entered by the users
#' @param kw A string. The keyword string entered by the users
#' @param dates A list of two date values (start and end date)
#'
#' @return A pie chart plot
#' @importFrom ggplot2 ggplot geom_bar coord_polar labs them_void them geom_text
#' @importFrom dplyr select collect left_join filter count group_by summarize
create_purpose_pie = function(studies, sponsortype, kw, dates){
  sponsors = ctrialsgov::ctgov_query(sponsor_type = sponsortype)
  d = data_query_search(studies, kw, dates)|> head(1000)

  by_purpose = d |>
    select(nct_id) |>
    collect() |>
    left_join(sponsors, by = "nct_id") |>
    select(primary_purpose) |>
    group_by(primary_purpose) |>
    filter(!is.na(primary_purpose)) |>
    summarize(n = n())

  ggplot(by_purpose, aes(x = "", y = n, fill = primary_purpose)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    labs(fill = "Primary Purpose",title = paste("Primary Purpose Distribution")) +
    theme(legend.position = "right") +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5))
}


#' Histogram of conditions
#'
#' Function that creates histogram showing the conditions that trials in a query are examining,
#' filtered by dates and sponsors information. Apply color feature in the histogram.
#' @param studies A tibble. The data table to be filtered
#' @param conditions A tibble. The data table to be filtered
#' @param kw A string. The keyword string entered by the users
#' @param sponsortype A string. The sponsortype entered by the users
#' @param dates A list of two date values (start and end date)
#' @param color A string. The color string chosen by the users
#'
#' @return A plot with filtered entries
#' @importFrom ggplot2 ggplot geom_col coord_flip labs them_bw
#' @importFrom dplyr select collect inner_join filter count arrange group_by
#'
create_condition_histogram <- function(studies, sponsors, conditions, kw, sponsortype, dates, color) {
  # sponsors = ctrialsgov::ctgov_query(sponsor_type = sponsortype)
  # Join studies with conditions based on nct_id and filter by title keywords
  d = data_query_search(studies, kw, dates)|> head(1000)

  d <- d |>
    select(nct_id) |>
    left_join(as_tibble(sponsors), by = "nct_id", relationship = 'many-to-many') |>
    filter(agency_class == sponsortype) |>
    inner_join(as_tibble(conditions), by = "nct_id", relationship = 'many-to-many') |>
    count(condition_name) |>
    arrange(desc(n)) |>
    head(10) # Limit to top 10 conditions for the plot

  # Plot the data
  ggplot(d, aes(x = reorder(condition_name, n), y = n)) +
    geom_col(fill = color) +
    labs(x = "Condition", y = "Number of Trials",title = paste("Condition Distribution")) +
    theme_bw() +
    coord_flip()
}


#' Pie chart of intervention types
#'
#' Function that creates a pie chart showing the distribution of intervention type that trials in a query are examining,
#' filtered by dates and sponsors information.
#' @param studies A tibble. The data table to be filtered
#' @param intervention A tibble. The data table to be filtered
#' @param kw A string. The keyword string entered by the users
#' @param sponsortype A string. The sponsor type entered by the users
#' @param dates A list of two date values (start and end date)
#'
#' @return A pie chart with filtered entries
#' @importFrom ggplot2 ggplot geom_bar coord_polar labs them_void them geom_text
#' @importFrom dplyr select left_join inner_join filter count group_by arrange
create_intervention_pie_data <- function(studies, sponsors, interventions, kw, sponsortype, dates) {
  # sponsors = ctrialsgov::ctgov_query(sponsor_type = sponsortype)
  d = data_query_search(studies, kw, dates)|> head(1000)

  pie_data <- d |>
    select(nct_id) |>
    left_join(as_tibble(sponsors), by = "nct_id") |>
    filter(agency_class == sponsortype) |>
    inner_join(as_tibble(interventions), by = "nct_id") |>
    count(intervention_type) |>
    arrange(desc(n)) |>
    head(7)

  total <- sum(pie_data$n)
  pie_data$percentage <- pie_data$n / total * 100

  ggplot(pie_data, aes(x = "", y = n, fill = intervention_type)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5)) +
    theme_void() +
    theme(legend.title = element_blank()) +
    labs(fill = "Intervention Type", title = paste("Proportion of Intervention Types"))+
    scale_fill_hue(c=45, l=75)
}


#' Histogram of intervention types
#'
#' Function that creates a histogram showing the specific intervention that trials in a query are examining,
#' filtered by intervention type, dates and sponsors information. Apply color feature in the histogram.
#' @param studies A tibble. The data table to be filtered
#' @param intervention A tibble. The data table to be filtered
#' @param kw A string. The keyword string entered by the users
#' @param interventionType A string. The intervention type entered by the users
#' @param sponsortype A string. The sponsor type entered by the users
#' @param dates A list of two date values (start and end date)
#' @param color A string. The color string chosen by the users
#'
#' @return A plot with filtered entries
#' @importFrom ggplot2 ggplot geom_col  labs them_bw coord_flip
#' @importFrom dplyr select left_join inner_join filter count group_by arrange
create_intervention_histogram <- function(studies, sponsors, interventions, kw, interventionType, sponsortype, dates, color) {
  # sponsors = ctrialsgov::ctgov_query(sponsor_type = sponsortype)
  d = data_query_search(studies, kw, dates)|> head(1000)

  d <- d |>
    select(nct_id) |>
    left_join(as_tibble(sponsors), by = "nct_id") |>
    filter(agency_class == sponsortype) |>
    inner_join(as_tibble(interventions), by = "nct_id")

  specific_intervention <- d |>
    filter(intervention_type == interventionType) |>
    count(intervention_name) |>
    arrange(desc(n))|>
    head(10)

  ggplot(specific_intervention, aes(x = reorder(intervention_name, n), y = n)) +
    geom_col(fill = color) +
    labs(x = "Intervention", y = "Number of Trials",title = paste("Histogram of", interventionType, "Interventions")) +
    theme_bw() +
    coord_flip()
}

