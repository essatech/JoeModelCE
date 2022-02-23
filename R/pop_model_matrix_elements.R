#'Population Model Matrix Elements
#'
#' @description Build population model matrix elements.
#'
#' @details TODO add details.
#'
#' @param pop_mod_setup List. Object returned from population_model_setup().
#'
#' @returns a list object of symbolic objects.
#'
#' @export
#'
pop_model_matrix_elements <- function(pop_mod_setup = NA) {


  life_histories <- pop_mod_setup$life_histories
  life_stages_symbolic <- pop_mod_setup$life_stages_symbolic
  life_pars <- pop_mod_setup$life_pars
  Nstage <- pop_mod_setup$Nstage
  density_stage_symbolic <- pop_mod_setup$density_stage_symbolic


  Nyears <- 10 * sum(life_histories$nYrs)

  # Build the projection matrix
  pmx.det <-
    pmx_eval(
      life_stages_symbolic,
      c(
        life_histories,
        life_histories$S,
        life_histories$nYrs,
        life_histories$mat
      )
    )

  # Initialize populations and find carrying capacity for each life stage
  # Carrying capacity is defined at the adult stage and
  # estimated for all other life-stages using the stable-stage distribution
  # (i.e., if the carrying capacity of the adult stage was set to 100 the
  # carrying capacity for the hatchling stage is the amount of hatchlings
  # necessary to result in 100 adults at equilibrium conditions)

  s0_optim_min <-
    stats::optimize(
      s0_optim.f,
      interval = c(0, 1),
      tol = 1e-16,
      mx = life_stages_symbolic,
      dat = life_histories,
      target.lambda = 1
    )$minimum


  life_histories$s0.1.det <- s0_optim_min

  # Add YOY survival to survival rate vector
  life_histories$S["s0"] <- life_histories$s0.1.det

  M.1.pmx <- {
    pmx <-
      pmx_eval(
        life_stages_symbolic,
        c(
          life_histories,
          life_histories$S,
          life_histories$nYrs,
          life_histories$mat
        )
      )
  }

  life_histories$gen.time <- popbio::generation.time(M.1.pmx)
  names(life_histories$gen.time) <- "gen.time"

  # variation in eggs per spawner
  life_histories$eps_sd <- life_pars["eps_sd", "Value"]
  names(life_histories$sR) <- "eps_sd"

  # temporal correlation in eggs per spawner
  life_histories$egg_rho <- life_pars["egg_rho", "Value"]
  names(life_histories$egg_rho) <- "egg_rho"

  # variation in natural mortality
  life_histories$M.cv <- life_pars["M.cv", "Value"]
  names(life_histories$M.cv) <- "M.cv"

  # temporal correlation in natural mortality
  life_histories$M.rho <- life_pars["M.rho", "Value"]
  names(life_histories$M.rho) <- "M.rho"

  life_histories$cr <- life_pars[grep("cr", life_pars$Name), "Value"]
  names(life_histories$cr) <- paste("cr", c("E", 0:Nstage), sep = "")





  #--------------------------------------------
  # Build return object
  #--------------------------------------------
  ret_obj <- list()

  # Density independent projection matrix
  ret_obj$projection_matrix <- pmx.det

  ret_obj$life_histories <- life_histories
  ret_obj$life_stages_symbolic <- life_stages_symbolic
  ret_obj$density_stage_symbolic <- density_stage_symbolic

  return(ret_obj)



}
