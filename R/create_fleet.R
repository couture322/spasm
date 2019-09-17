#' create_fleet
#'
#' @param eq_f equilibrium fishing rate
#' @param length_50_sel set gear selectivity, relative to spp lengths
#' @param delta cm above length50 at 95 selectivity
#' @param mpa_reaction leave', 'concentrate'; def?
#' @param fish fish life history data from create_fish
#' @param cost
#' @param beta effort exponent in cost calcs
#' @param theta calculated in sim_fishery for cost tuning
#' @param q quanity for economic estimates
#' @param fleet_model "open access", "supplied-catch", "constant-effort", "constant-catch"
#' @param effort_allocation how are fishers deciding where to fish? 'gravity', 'profit-gravity', 'simple'
#' @param initial_effort initial effort, before MPA established, relative to r0
#' @param cost_function
#' @param cost_slope
#' @param tech_rate
#' @param target_catch
#' @param catches data for 'supplied-catch'
#' @param sigma_effort
#' @param profit_lags
#' @param theta_tuner
#' @param q_cv
#' @param q_ac
#' @param cost_cv # cost coeff of var
#' @param cost_ac #cost autocorrelation
#' @param max_perc_change_f
#' @param max_cr_ratio
#' @param q_slope
#' @param oa_ratio
#' @param mey_buffer
#' @param effort_ac
#'
#' @return a fleet object
#' @export
#'
#' @examples
#' \dontrun{
#' create_fleet(eq_f = 2,length_50_sel = 25, length_95_sel = 27, fish = bluefish)
#' }
create_fleet <- function(eq_f = NA,
                         length_50_sel = 1,
                         delta = 2,
                         fish,
                         mpa_reaction = 'concentrate',
                         cost = .1,
                         beta = 1,
                         theta = 1e-1,
                         max_perc_change_f = 2,
                         max_cr_ratio = 0.75,
                         b_ref_oa = 0.25,
                         q = 1e-3,
                         q_cv = 0,
                         q_ac = 0,
                         q_slope = 0,
                         cost_cv = 0,
                         cost_ac = 0,
                         cost_slope = 0,
                         fleet_model = 'constant-effort',
                         effort_allocation = 'gravity',
                         cost_function = 'constant',
                         tech_rate = 0,
                         initial_effort = 100,
                         target_catch = 0,
                         catches = NA,
                         sigma_effort = 0,
                         profit_lags = 1,
                         mey_buffer = 2,
                         effort_ac = 0) {


  length_bins <- as.numeric(colnames(fish$length_at_age_key))

  sel_at_bin <- ((1 / (1 + exp(-log(
    19
  ) * ((length_bins - length_50_sel) / (delta)
  )))))

  p_sel_at_age <- (as.matrix(fish$length_at_age_key) %*% sel_at_bin)

  length_95_sel <- (length_50_sel + delta)

  sel_at_age <- p_sel_at_age

  mey_buffer <- mey_buffer

  rm(fish)

  fleet <- list(mget(ls()))

  fleet <- fleet[[1]]

  return(fleet)
}
