#' create_fleet
#'
#' @param eq_f
#' @param length_50_sel
#' @param delta cm above length50 at 95 selectivity
#' @param mpa_reaction
#' @param fish
#' @param cost
#' @param beta
#' @param theta
#' @param q
#' @param fleet_model
#' @param effort_allocation
#' @param initial_effort
#' @param cost_function
#' @param cost_slope
#' @param tech_rate
#' @param target_catch
#' @param catches
#' @param sigma_effort
#' @param profit_lags
#' @param theta_tuner
#' @param q_cv
#' @param q_ac
#' @param cost_cv
#' @param cost_ac
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
#' @examples create_fleet(eq_f = 2,length_50_sel = 25, length_95_sel = 27, fish = bluefish)
create_fleet <- function(eq_f = NA, # ???
                         length_50_sel = 1, #  set gear selectivity, relative to spp lengths
                         delta = 2, # difference between length_50_sel and length_95_sel
                         fish, # fish life history data
                         mpa_reaction = 'concentrate', # 'leave', 'concentrate'; def?
                         cost = .1, #fishing costs
                         beta = 1, # effort exponent in cost calcs
                         theta = 1e-1, # calculated in sim_fishery for cost tuning
                         max_perc_change_f = 2, # used to calculate theta in sim_fishery
                         max_cr_ratio = 0.75,
                         b_ref_oa = 0.25, # used to calculate tuned cost ratio?
                         q = 1e-3, # quanity for economic estimates
                         q_cv = 0,
                         q_ac = 0,
                         q_slope = 0,
                         cost_cv = 0,
                         cost_ac = 0,
                         cost_slope = 0,
                         fleet_model = 'constant-effort', # "open access", "supplied-catch", "constant-effort"(?)
                         effort_allocation = 'gravity', # how are fishers deciding where to fish? options? 'gravity', 'profit-gravity', 'simple'
                         cost_function = 'constant',
                         tech_rate = 0,
                         initial_effort = 100,
                         target_catch = 0,
                         catches = NA, #dont see where this is used
                         sigma_effort = 0, # variation around effort
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
