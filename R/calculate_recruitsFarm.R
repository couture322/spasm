#' \code{calcualte_recruitsFarm} calculates recruits
#' as a function of spawning stock biomass under
#' various forms of density dependence
#'
#' @param farmYrs
#' @param pop
#' @param fish
#' @param num_patches
#' @param patch_habitat
#' @param phase
#'
#' @return recruits of age min_age in each patch
#' @export
#'
#' @examples
#' \dontrun{
#' calculate_recruits(pop, fish, num_patches = 10, phase = 'grow')
#' }

calculate_recruitsFarm <-
  function(farmYrs,
           pop,
           fish,
           num_patches,
           patch_habitat = 1,
           fallowHab,
           phase = 'burn',
           move_matrix,
           rec_devs = 0) {

     if (length(patch_habitat) == 1) {
      patch_habitat <- rep(patch_habitat, num_patches)

    }
    prop_patch_habitat <- ((patch_habitat / sum(patch_habitat)))

    prop_fallow_habitat <- ((fallowHab / sum(fallowHab)))

    r0s <-
      rep(fish$r0, num_patches) * prop_patch_habitat

    # ssb0s <- rep(fish$ssb0, num_patches) * ((patch_habitat / sum(patch_habitat)))

    ssb0s <- fish$ssb0


    if (phase == 'burn') {
      recruits <-
        rep(fish$r0, num_patches)
      if (fish$density_dependence_form != 2){

        recruits <- crossprod(recruits, move_matrix)

      }

    } else {
      if (fish$density_dependence_form == 1) {
        # Recruitment is independent in each area, but a fraction of the recruits in each area drift to the adjacent areas before settling
        recruits <- pop %>%
          dplyr::group_by(patch) %>%
          dplyr::summarise(ssb = sum(ssb)) %>%
          dplyr::mutate(recruits = (0.8 * r0s * fish$steepness * ssb) / (
            0.2 * ssb0s * (1 - fish$steepness) +
              (fish$steepness - 0.2) * ssb
          )) %>% {
            .$recruits
          }

        recruits <- crossprod(recruits, move_matrix)

      }
      if (fish$density_dependence_form == 2) {
        # Density dependence occurs at the regional level and then recruits move

        recruits <- pop %>%
          dplyr::summarise(ssb = sum(ssb)) %>%
          dplyr::mutate(recruits = (0.8 * sum(r0s) * fish$steepness * ssb) / (
            0.2 * sum(fish$ssb0) * (1 - fish$steepness) +
              (fish$steepness - 0.2) * ssb
          )) %>% {
            .$recruits
          }

        # browser()
        recruits <- if(is.na(farmYrs) | farmYrs[pop$year[1]]==0) {recruits} else {
                        if(farmYrs[pop$year[1]]==1) {
                          recruits * prop_patch_habitat} else {
                            recruits * prop_fallow_habitat}

        # recruits <- crossprod(recruits, move_matrix)

      }
  }
      if (fish$density_dependence_form == 3) {
        #larvae are distributed to each area, and then density dependence occurs based on the number of spawners in each area

        ssb <- pop %>%
          dplyr::group_by(patch) %>%
          dplyr::summarise(ssb = sum(ssb))

        larvae <- crossprod(ssb$ssb, move_matrix)

        recruits <- (0.8 * r0s * fish$steepness * larvae) / (
          0.2 * ssb0s * (1 - fish$steepness) +
            (fish$steepness - 0.2) * larvae)

      }

    }
    recruits <- recruits * exp(rec_devs - fish$sigma_r^2/2);

    return(recruits)

  }
