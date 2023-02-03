#' Project matrix with Density Dependence
#'
#' @description Project the matrix model forward in time with density dependence.
#'
#' @details Builds deterministic projection matrix using `popbio::stable.stage` with initial parameters based on arguments provided. Applies CE stressors to appropriate targets based on `CE_df`. All population modeling components are contained within this function. Users define a projection matrix, density-dependence matrix, harm projection matrix, life history parameters, years to simulate, carrying capacity, catastrophic event probabilities and a cumulative effects data frame (CE_df). When run this function will project the population forward in time. See the vignette tutorial Population Model Overview for details and instructions.
#'
#' @param M.mx A projection matrix expression
#' @param D.mx A matrix of density-dependence effect
#' @param H.mx A harm projection matrix
#' @param dat Life history data
#' @param Nyears Years to run simulation
#' @param K The population carrying Capacity
#' @param p.cat Probability of catastrophic event.
#' @param CE_df Cumulative effect dataframe. Data frame identifying cumulative effects stressors targets (system capacity or population parameter, or both, and target life stages.
#' @param K_adj Boolean. Should K_adj be run. Defaults to false.
#' @param stage_k_override Vector of K values for egg (E), fry (0), stage_1, stage_2 values etc. defaults to NULL. If set values will override adult K value for alternative DD mechanism.
#' @param bh_dd_stages Character vector of life stages c("stage_E", "stage_0", "stage_1", "stage_2", ...) to apply classical Beverton-Holt density-dependence. Will override compensation ratios if set. Use "stage_e" for egg-to-fry k, "stage_0" for fry to stage_1 k and "stage_1" for stage_1 to stage_2 k and so on. Densities are the capped value for the transition stage.
#' @importFrom rlang .data
#'
#' @returns A list object with projected years, population size, lambda, fecundity, survival, catastrophic events.
#'
#' @export
Projection_DD <- function(M.mx = NA,
                          D.mx = NULL,
                          H.mx = NULL,
                          dat = NA,
                          Nyears = 100,
                          K = NA,
                          p.cat = NA,
                          CE_df = NULL,
                          K_adj = FALSE,
                          stage_k_override = NULL,
                          bh_dd_stages = NULL
                          ) {


  # Define variables in function as null
  # stable.stage <- Nstage <- E_est <- NULL


  # MJB: Should simple stage-specific K values be used instead of stable-stage
  # instead of compensation ratios
  if(!(is.null(stage_k_override))) {
    # Reset fry survivorship
    if(length(stage_k_override) != (dat$Nstage + 2)) {
      stop("length of stage_k_override must include egg, fry and a value for each adult stage...")
    }
    dat$s0.1.det <- dat$Surv_annual[2]
    dat$S[2] <- dat$Surv_annual[2]
  }




  # Adjust K to give correct mean after stochasticity
  if (is.null(dat$K.adj)) {
    dat$K.adj <- 1
  }
  Ka <- K * dat$K.adj


  # Make CC Adjustments run with K_adj (optional) but slower
  if(K_adj) {
    kadj <- pop_model_K_adj(replicates = 100,
                    dat = dat,
                    mx = life_stages_symbolic,
                    dx = density_stage_symbolic,
                    Nyears = Nyears)
    dat$K.adj <- kadj$K.adj
    Ka <- K * dat$K.adj
  }




  # Set D = 1 if no density_depenence
  if (is.null(D.mx)) {
    D <- 1
  }

  # if no harm set H to 1
  if (is.null(H.mx)) {
    H <- rep(1, Nyears)
  } else if (is.matrix(H.mx)) {
    H <- replicate(Nyears, H.mx, simplify = F)
  } else {
    H <- H.mx
  }

  # Catastrophes
  Catastrophe <- sample(
    c(1, 0),
    Nyears,
    replace = TRUE,
    prob = c(p.cat / dat$gen.time, 1 - p.cat / dat$gen.time)
  )

  # effect on catastrophe on pop size (percent reduction) - scaled Beta dist'n fit from Reed et al. (2003)
  e.cat <- sapply(Catastrophe, function(x) {
    ifelse(x == 0, NA, stats::rbeta(1, shape1 = 0.762, shape2 = 1.5) * (1 - .5) + .5)
  })


  # ----------------------------------
  # Initialize parameters
  # ----------------------------------

  # egg carrying capacity

  # Deterministic projection matrix
  pmx.det <- pmx_eval(M.mx, c(dat, dat$S, dat$nYrs, dat$mat))

  SS <- popbio::stable.stage(pmx_eval(M.mx, c(dat, dat$S, dat$nYrs, dat$mat)))

  # evaluate whether we are quantifying the initial carrying capacities correctly
  k_stage <- SS / SS[dat$Nstage] * Ka

  # MJB added line - Nstage was floating in global memory from KW code
  Nstage <- dat$Nstage

  names(k_stage) <- paste("K", 1:Nstage, sep = "")
  dat$K <- k_stage
  dat$Ke <- E_est(N = dat$K[-1], dat = c(dat, dat$mat, dat$S))
  dat$K0 <- dat$Ke * dat$S["sE"]


  # ------------------------------------------------------
  # Override SS stage-specific carrying capacity K-values
  # with custom values
  # ------------------------------------------------------

  # MJB: Should simple stage-specific K values be used instead of stable-stage
  # instead of compensation ratios
  if(!(is.null(stage_k_override))) {

    dat$Ke <- ifelse(is.na(stage_k_override[1]), dat$Ke, stage_k_override[1]) # EGG
    dat$K0 <- ifelse(is.na(stage_k_override[2]), dat$K0, stage_k_override[2]) # FRY

    # older stages - update adults
    k_stage_mod <- stage_k_override[3:(Nstage + 2)]
    k_stage <- ifelse(is.na(k_stage_mod), k_stage, k_stage_mod)
    names(k_stage) <- paste("K", 1:Nstage, sep = "")
    dat$K <- k_stage

    # Override K and Ka if set
    last_stage <- stage_k_override[(Nstage + 2)]
    Ka <- ifelse(is.na(last_stage), Ka, last_stage)
    K <- ifelse(is.na(last_stage), K, last_stage)
  }


  # ----------------------------------
  # Apply CE stressors
  # ----------------------------------

  # Apply the harm matrix to K or S (if needed)

  # Define nicknames for stages
  alevin_stage <- 2
  all_juv <- 3:5
  fry_stages <- 3
  fry_parr_stages <- 3:4
  # 3 is used as eggs, yoy, and age-0 are not ever mature
  parr_stages <- 4
  # 3 is used as eggs, yoy, and age-0 are not ever mature
  juv_stages <- 1:3 #1:(3 + max(which(dat$mat == 0)))
  adult_stages <- (3 + max(which(dat$mat > 0)))
  subadult_stages <- adult_stages - 1


  # Note from Matt: Kyle has mentioned that a future improvement to the code
  # will be to fix this massive ifelse chain to make the framework more
  # flexible to other life history types ...

  # MJB: Apply CE stressors to population parameters
  # if not null
  if (!(is.null(CE_df))) {
    dat <- pop_model_ce_apply(
      CE_df = CE_df,
      dat = dat,
      alevin_stage = alevin_stage,
      all_juv = all_juv,
      fry_stages = fry_stages,
      fry_parr_stages = fry_parr_stages,
      parr_stages = parr_stages,
      subadult_stages = subadult_stages,
      adult_stages = adult_stages)
  }


  # YOY carrying capacity

  # Fecundity
  ft <- lapply(1:(Nyears + 1), function(x) {
    f_temp <- f_rand(dat$eps, dat$eps_sd, rho = dat$egg_rho)
    f_temp <- ifelse(is.na(f_temp), dat$eps, f_temp)
    names(f_temp) <- "eps"
    return(f_temp)
  })

  # Survival
  suppressWarnings({
    st <- lapply(1:(Nyears + 1), function(x) {
      s_temp <- s_rand(dat$S, dat$M.cv, rho = dat$M.rho)
      s_temp <- ifelse(is.na(s_temp), 0, s_temp)
      s_temp
    })
  })

  new_dat <- dat
  new_dat[["eps"]] <- NULL

  # Population matrix
  M.list <- lapply(1:(Nyears + 1), function(x) {
    pmx_eval(M.mx, c(new_dat, st[[x]], ft[[x]], new_dat$nYrs, new_dat$mat))
  })

  # Initial population structure
  N <- sum(new_dat$K) * popbio::stable.stage(M.list[[1]])

  # number of Egg produced
  Na <- Nb_est(N[-1], new_dat$mat)
  E <-
    E_est(
      N = N[-1],
      dat = c(new_dat, ft[[1]], st[[1]], new_dat$nYrs, new_dat$mat)
    )

  # initialize output vectors
  Nvec <- rep(NA, Nyears + 1)

  # Adult pop vector
  Nvec[1] <- Na

  # age-specific annual population size
  Ns <- list(N)

  # population growth rate
  lambdas <- rep(NA, Nyears)

  # loop through years
  for (t in 1:Nyears) {

    # Density-Dependent Survival
    if (is.null(D.mx) == FALSE) {

      # create vector of density dependence effects
      # using compensation ratios
      d.vec <-
        d.vec.f(
          df = new_dat,
          N = c(E, E * st[[t + 1]]["sE"], N),
          Ks = c(new_dat$Ke, new_dat$K0, new_dat$K)
        )

      # check if any survival rates > 1
      s.test <- d.vec * st[[t + 1]] # survival rate after DD effects

      # MJB added: to deal with frequent NA issue here...
      s.test <- ifelse(is.na(s.test), 0, s.test)

      if (any(s.test > 1)) {
          # any s.test > 1?
          s.err <- which(s.test > 1) # ID which is > 1
          # set density dependence effect of 1/survival - give s = 1 after DD effects
          d.vec[s.err] <- 1 / st[[t + 1]][s.err]
      }

      # create density dependence effects matrix
      D <- pmx_eval(D.mx, as.list(d.vec))
    }


    # MJB: create vector of density dependence effects using the
    # classical the Beverton-Holt function
    if(is.null(bh_dd_stages) == FALSE) {

      d.vec <- d.vec.bh(
        df = new_dat,
        N = c(E, E * st[[t + 1]]["sE"], N),
        Ks = c(new_dat$Ke, new_dat$K0, new_dat$K),
        bh_dd_stages = bh_dd_stages
      )

      # create density dependence effects matrix
      D <- pmx_eval(D.mx, as.list(d.vec))
    }


    # Population Projection matrix
    A <- M.list[[t + 1]] * D * H[[t]]


    # project the population 1 year ahead.
    if (Catastrophe[t] == 1) {
      N <- N * (1 - e.cat[t])
    } else {
      N <- as.vector(A %*% N)
    }




    # Number of adults
    Na <- Nb_est(N[-1], new_dat$mat)

    # Number of Juveniles
    Nj <- sum(N * c(1, 1 - new_dat$mat))

    # number of Egg produced
    E <- E_est(N = N[-1], c(new_dat, st[[t + 1]], ft[[t + 1]], new_dat$mat))

    # Number of mature fish in pop
    Nvec[t + 1] <- Na
    Ns[t + 1] <- list(N)

    # pop growth rate
    lambdas[t] <- Nvec[t + 1] / Nvec[t]
  }

  # Build return object from function
  ret_obj <- list(
    "pop" = as.data.frame(list(year = 0:Nyears, N = Nvec)),
    "N" = do.call(rbind, Ns),
    "lambdas" = lambdas,
    "vars" = list("ft" = do.call(rbind, ft), "st" = do.call(rbind, st)),
    "Cat." = as.data.frame(list("Cat." = Catastrophe, "e.cat" = e.cat))
  )

  return(ret_obj)
}
